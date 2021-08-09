#' @title Lookup organization by name
get_org_info <- function(syn, name) {
  uri <- glue::glue("/schema/organization?name={name}")
  rest_get(syn = syn, uri = uri)
}


#' @title Organization ACL
#'
get_org_acl <- function(syn, org_id) {
  uri <- glue::glue("/schema/organization/{org_id}/acl")
  rest_get(syn = syn, uri = uri)
}

#' @title List organization schemas
#' TODO missing token handling for more pages
list_org_schemas <- function(syn, name) {
  uri <- "/schema/list"
  body <- glue::glue("{{organizationName: \"{name}\"}}")
  rest_post(syn = syn, uri = uri, body = body)
}
