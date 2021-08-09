#' @title Bind schema to entity
#'
#' @description Bind a registered JSON schema to a Synapse entity.
#'
#' @export
#' @param syn Synapse client object
#' @param entity_id Synapse entity synID
#' @param schema_id Registered Synapse schema id
bind_schema <- function(syn, entity_id, schema_id) {
  ## Build strings outside of rest call for reasons
  uri <- glue::glue("/entity/{entity_id}/schema/binding")
  body <- glue::glue("{{entityId: \"{entity_id}\", schema$id: \"{schema_id}\"}}")
  rest_put(
    syn = syn,
    uri = uri,
    body = body
  )
}
