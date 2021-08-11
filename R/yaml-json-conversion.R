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
  write(json, file = path)
}

#' @title Convert yaml to JSON schema file
#'
#' @description Convert a yaml schema to JSON schema and write to file.
#'
#' @param yaml_path Path to the yaml file
#' @param json_path JSON output file path
#' @param name Name of the schema to return. This is needed if there are
#' multiple sections of the yaml file and only a single section that has the
#' schema needs to be returned.
yaml_to_json_file <- function(yaml_path, json_path, name = NA) {
  json <- yaml_to_json_schema(path = yaml_path, name = name)
  write_json_schema(schema = json, path = json_path)
}
