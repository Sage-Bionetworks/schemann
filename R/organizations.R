#' @title Create new organization
#'
#' @description Create a new Synapse JSON schema services organization.
#'
#' @export
#' @param syn Synapse client object
#' @param name Name of the organization (must be unique across Synapse)
create_org <- function(syn, name) {
  uri <- "/schema/organization"
  body <- glue::glue("{{\"organizationName\":\"{name}\"}}")
  rest_post(syn, uri = uri, body = body)
}

#' @title Lookup organization by name
get_org_info <- function(syn, name) {
  uri <- glue::glue("/schema/organization?name={name}")
  rest_get(syn = syn, uri = uri)
}

#' @title Organization ACL
get_org_acl <- function(syn, org_id) {
  uri <- glue::glue("/schema/organization/{org_id}/acl")
  rest_get(syn = syn, uri = uri)
}

#' @title Update organization ACL
#'
#' @description Update an organization's ACL. Must have appropriate ACL
#' permissions to update.
#'
#' @export
#' @param syn Synapse client object
#' @param org_id Organization ID number
#' @param acl JSON formmated string with new ACL
update_org_acl <- function(syn, org_id, acl) {
  ## TODO Make ACL bit better
  uri <- glue::glue("/schema/organization/{org_id}/acl")
  rest_put(syn = syn, uri = uri, body = acl)
}

#' @title List organization schemas
#' TODO missing token handling for more pages
list_org_schemas <- function(syn, name) {
  uri <- "/schema/list"
  body <- glue::glue("{{organizationName: \"{name}\"}}")
  rest_post(syn = syn, uri = uri, body = body)
}
