#' @title Convert yaml to json schema
#'
#' @description Convert yaml version of schema to json version.
#'
#' @noRd
#' @inheritParams yaml_to_json_file
#' @return The schema in a list
yaml_to_list <- function(yaml_path, name = NA) {
  tmp_path <- tempfile(fileext = ".json")
  json_path <- suppressMessages(
    ymlthis::write_as_json(path = yaml_path, out = tmp_path)
  )
  json <- jsonlite::read_json(json_path)
  if (!is.na(name) & name %in% names(json)) {
    return(json[[name]])
  } else {
    return(json)
  }
}

#' @title Write JSON schema to file
#'
#' @description Write JSON schema to a file.
#'
#' @noRd
#' @param schema List with schema to write as JSON
#' @inheritParams yaml_to_json_file
write_json_schema <- function(schema, json_path) {
  json <- jsonlite::prettify(rjson::toJSON(schema))
  write(json, file = json_path)
}

#' @title Convert yaml to JSON schema file
#'
#' @description Convert a yaml schema to JSON schema and write to file.
#'
#' @export
#' @param yaml_path Path to the yaml file.
#' @param json_path JSON output file path. If `NA` (default), creates temporary
#' file to write to.
#' @param name Name of the schema to return. This is needed if there are
#' multiple sections of the yaml file and only a single section that has the
#' schema needs to be returned.
#' @return `json_path` for the written file.
yaml_to_json_file <- function(yaml_path, json_path = NA, name = NA) {
  if (all(is.na(json_path))) {
    json_path <- tempfile(fileext = ".json")
  }
  json <- yaml_to_list(yaml_path = yaml_path, name = name)
  write_json_schema(schema = json, json_path = json_path)
  return(json_path)
}

#' @title Register schema from yaml
#'
#' @description Convert yaml schema to JSON schema and register in Synapse.
#'
#' @param syn Synapse client object
#' @param yaml_path Path to the yaml schema
#' @param name Name of the yaml schema (e.g. main-schema)
#' @param dryRun `TRUE` to check if a schema can be registered in Synapse without
#' registering; `FALSE` (default) to try to register in Synapse.
register_and_report_yaml <- function(syn, yaml_path, name, dryRun = FALSE) {
  json_path <- yaml_to_json_file(yaml_path = yaml_path, name = name)
  register_and_report(syn = syn, file = json_path, dryRun = dryRun)
}

#' @title Convert a Synapse table to yaml anchors
#'
#' @description Convert a Synapse table to yaml anchors.
#'
#' @param syn Synapse client object
#' @param anchor_table synID of table to create anchor list from. Must have
#' columns 'alias' (the name for the anchor), 'schema' (the schema id),
#' 'version' (the version of the schema).
#' @param anchor_path Output path for the yaml anchor list generated. If `NA`
#' (default), will create a temporary file.
#' @return Output path for the yaml anchor list generated.
get_anchors_from_table <- function(syn, anchor_table, anchor_path = NA) {
  if (is.na(anchor_path)) {
    anchor_path <- tempfile(fileext = ".yml")
  }
  anchors <- tryCatch(
    {
      syn$tableQuery(
        glue::glue("SELECT alias, schema, version from {anchor_table}")
      )$asDataFrame()
    },
    error = function(e) {
      stop(glue::glue("There was an issue gathering the table:\n{e$message}"))
    }
  )
  anchor_yaml <- anchor_string(anchors = anchors)
  write(anchor_yaml, anchor_path)
  return(anchor_path)
}

#' @title Create anchor string
#'
#' @description Create the anchor string with correct formatting.
#'
#' @noRd
#' @param anchors A data.frame with columns `alias`, `schema`, `version`.
#' @return A string with all anchors in yaml format.
anchor_string <- function(anchors) {
  if (!all(c("alias", "schema", "version") %in% names(anchors))) {
    stop("`anchors` must have columns: alias, schema, version")
  }
  all_anchors <- purrr::pmap(
    anchors[, c("alias", "schema", "version")],
    function(alias, schema, version) {
      anchor_string_single(
        alias = alias,
        schema = schema,
        version = version
      )
    }
  )
  all_anchors <- glue::glue_collapse(all_anchors)
  all_anchors <- glue::glue("anchors:\n{all_anchors}", .trim = FALSE)
  return(all_anchors)
}

## Helper to `anchor_string`
anchor_string_single <- function(alias, schema, version) {
  glue::glue("  {alias}: &{alias}\n    $ref: {schema}-{version}\n", .trim = FALSE)
}

#' @title Combine the anchor and yaml schema
#'
#' @description Combine the anchors and yaml schema into a single file for
#' dereferencing.
#'
#' @export
#' @param yaml_path Path to the yaml schema
#' @param anchors_path Path to the anchors file
#' @param output_path Path to output file (default: NA creates temp file)
#' @return output_path
combine_anchors_schema <- function(yaml_path, anchors_path, output_path = NA) {
  if (is.na(output_path)) {
    output_path <- tempfile(fileext = ".yml")
  }
  props <- readr::read_lines(anchors_path)
  schema <- readr::read_lines(yaml_path)
  # schema <- update_id(schema = schema)
  write(props, file = output_path)
  write(schema, file = output_path, append = TRUE)
  return(output_path)
}

#' @title Get yaml anchor properties
#'
#' @description Get the anchor properties from a yaml file.
#'
#' @export
#' @inheritParams yaml_to_json_file
#' @param as_table `FALSE` to get the property references as a named list;
#' `TRUE` to get properties as a table with the columns Key, Version, SchemaID.
get_properties_yaml <- function(yaml_path, name = "properties", as_table = TRUE) {
  props <- yaml_to_list(yaml_path, name = name)
  props <- properties_list_clean(properties = props)
  if (!as_table) {
    return(props)
  }
  props <- purrr::map_dfr(props, ~ .)
  return(props)
}

#' @title Get information from id
#'
#' @description Get information from the id.
#'
#' @export
#' @param id Full id with version in the format organization-module.key-version.
#' @param info One of id (unversioned id in the format
#' "organization-module.key"), organization (Synapse schema organization),
#' module (module key belongs to), key (the key), version (the version).
get_id_info <- function(id,
                        info = c("id", "organization", "module", "key", "version")) {
  info <- match.arg(info)
  pattern <- "((^[^-]+)-([[:alnum:]]+)\\.([[:alnum:]]+))-([0-9\\.]+)"
  ## Make sure matches format expected
  if (!grepl(pattern, id)) {
    stop("The id is not in the format organization-module.key-version.")
  }
  switch(
    info,
    id = gsub(pattern, "\\1", id),
    organization = gsub(pattern, "\\2", id),
    module = gsub(pattern, "\\3", id),
    key = gsub(pattern, "\\4", id),
    version = gsub(pattern, "\\5", id)
  )
}
