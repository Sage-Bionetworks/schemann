#' @title Create schema request body
#'
#' @description Create the body needed for registering a JSON schema in Synapse.
#'
#' @noRd
#' @param file Path to JSON Schema file
#' @param dryRun Should the dryRun field be set to `true`? Defaults to `FALSE`.
#' @return String containing the JSON request body
create_body <- function(file, dryRun = FALSE) {
  stopifnot(dryRun %in% c(TRUE, FALSE))
  paste(
    '{"concreteType": "org.sagebionetworks.repo.model.schema.CreateSchemaRequest", "schema": ',
    paste(readLines(file), collapse = ""),
    ', "dryRun": ',
    ifelse(dryRun, 'true', 'false'),
    '}'
  )
}

#' @title Register schema to Synapse
#'
#' @description Given a path to a file containing a JSON schema, registers that
#' schema on Synapse.
#'
#' @noRd
#' @param syn Synapse object
#' @param file Path to JSON Schema file
#' @param dryRun Should the dryRun field be set to `true`? Defaults to `FALSE`.
#' @return Token object containing the async token used to monitor the job
register_schema <- function(syn, file, dryRun = FALSE) {
  rest_post(
    syn = syn,
    uri = "/schema/type/create/async/start",
    body = create_body(file, dryRun = dryRun)
  )
}
#' Get status of registered schema
#'
#' @export
#' @param syn Synapse object
#' @param token Async token
#' @return Response object containing schema
get_registration_status <- function(syn, token) {
  processing <- TRUE
  ## Check results of registering schema, retrying if the async job hasn't
  ## completed yet
  while (processing) {
    result <- rest_get(
      syn = syn,
      uri = glue::glue("/schema/type/create/async/get/{token}")
    )
    ## Originally made with synapser, which doesn't return the status codes.
    ## Check response object to determine what to do. If it contains "jobState",
    ## that's part of the AsynchronousJobStatus, i.e. the async job hasn't
    ## completed yet.
    ## May be able to change this to using status codes.
    if (!"jobState" %in% names(result)) {
      processing <- FALSE
    }
  }
  result
}
#' Register schema and report on its status
#'
#' Registers a schema and then monitors the asynchronous job until it completes.
#'
#' @export
#' @param syn Synapse object
#' @param file Path to JSON Schema file
#' @param dryRun Should the dryRun field be set to `true`? Defaults to `FALSE`.
#' @return `TRUE` if schema was successfully registered; `FALSE` otherwise.
register_and_report <- function(syn, file, dryRun = FALSE) {
  message(glue::glue("Registering {file} with dryRun = {dryRun}"))
  ## register_schema() can fail if e.g. schemas contain non-ASCII characters --
  ## in this case we catch that error and provide the message.
  token <- try(register_schema(syn, file, dryRun = dryRun), silent = TRUE)
  if (inherits(token, "try-error")) {
    message(token)
    return(FALSE)
  }
  ## Note that this is written for how synapser works.
  ## It should be updated for reticulate, but it currently works as is.
  ## Reasoning for synapser below:
  ##   If we get a 400 status code, synapser will throw an error -- we want to
  ##   catch that and provide the message but not raise an actual R error.
  status <- try(get_registration_status(syn, token$token), silent = TRUE)
  if (inherits(status, "try-error")) {
    message(status)
    FALSE
  } else {
    TRUE
  }
}
