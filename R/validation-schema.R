#' @title Get validation schema
#'
#' @description Get the dereferenced validation schema from a registered Synapse
#' JSON schema.
#'
#' @export
#' @param syn Synapse client object
#' @param schema_id Registered Synapse schema id
#' @param readable Formats the schema spacing for better readability (default:
#' `FALSE`)
#' @return string with dereferenced JSON schema
get_validation_schema <- function(syn, schema_id, readable = FALSE) {
  ##TODO add a catch for entities without schema
  ## Start the job to compile the validation schema
  token <- rest_post(
    uri = "/schema/type/validation/async/start",
    body = glue::glue("{{$id: \"{schema_id}\"}}")
  )
  ## Should wait until finished
  schema <- get_schema_status(syn, token)
  ## Make prettier for readability
  if (readable) {
    schema <- jsonlite::prettify(jsonlite::toJSON(schema))
  }
  return(schema)
}

## Have to keep checking on status
get_schema_status <- function(syn, token) {
  processing <- TRUE
  ## Check results of registering schema, retrying if the async job hasn't
  ## completed yet
  while (processing) {
    result <- rest_get(
      uri = glue::glue("/schema/type/validation/async/get/{token}")
    )
    ## synapser doesn't return the status codes unfortunately, so we check the
    ## response object to determine what to do. If it contains "jobState",
    ## that's part of the AsynchronousJobStatus, i.e. the async job hasn't
    ## completed yet.
    if (!"jobState" %in% names(result)) {
      processing <- FALSE
    }
  }
  result
}

#' @title Get validation results
#'
#' @description Get the annotation validation results for a Synapse entity.
#'
#' @export
#' @param syn Synapse client object
#' @param entity_id Synapse entity synID
#' @return Synapse validation results
get_validation_results <- function(syn, entity_id) {
  rest_get(
    uri = glue::glue("/entity/{entity_id}/schema/validation")
  )
}

#' @title Simple check for valid annotations
#'
#' @description Check if results say the file is valid or invalid
#'
#' @export
#' @param results Synapse validation results for one or more entities
#' @return `TRUE` if all `results` are valid, else `FALSE` if one or more are
#' invalid
is_valid_result <- function(results) {
  res_is_valid <- purrr::map(results, function(x) {
    x$isValid
  })
  all(unlist(res_is_valid))
}

