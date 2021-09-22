#' @title Lookup organization by name
#'
#' @description Get organization information by name.
#'
#' @export
#' @param syn Synapse client object
#' @param name Name of organization
get_org_info <- function(syn, name) {
  uri <- glue::glue("/schema/organization?name={name}")
  rest_get(syn = syn, uri = uri)
}


#' @title Organization ACL
#'
#' @description Get organization's Access Control List (ACL) by organization ID
#' number.
#'
#' @export
#' @param syn Synapse client object
#' @param org_id Organziation ID number
get_org_acl <- function(syn, org_id) {
  uri <- glue::glue("/schema/organization/{org_id}/acl")
  rest_get(syn = syn, uri = uri)
}

#' @title List organization schemas
#'
#' @description List first page of schemas registered to an organization.
#' TODO missing token handling for more pages
#'
#' @export
#' @param syn Synapse client object
#' @param name Name of organization
list_org_schemas <- function(syn, name) {
  uri <- "/schema/list"
  body <- glue::glue("{{organizationName: \"{name}\"}}")
  rest_post(syn = syn, uri = uri, body = body)
}
