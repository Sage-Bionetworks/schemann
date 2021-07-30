#' @title Log into Synapse
#'
#' @description Loads and logs into Synapse
#'
#' @export
#' @param auth_token Synapse Personal Access Token (default: `NA`). If no
#' `auth_token` given, assumes local .synapseConfig.
#' @return Synapse client object
#' @examples
#' \dontrun{
#' syn <- login_synapse()
#' syn <- login_synapse(auth_token = "my_access_token")
#' }
login_synapse <- function(auth_token = NA) {
  synapse <- reticulate::import("synapseclient")
  syn <- synapse$Synapse()
  if (is.na(auth_token)) {
    ## Use .synapseConfig
    syn$login()
  } else {
    syn$login(authToken = auth_token)
  }
  return(syn)
}

#' @title Set Synapse endpoints
#'
#' @description Set Synapse endpoints to staging or production.
#'
#' @export
#' @param syn Synapse client object
#' @param staging `TRUE` (default) if the endpoints should be set to staging;
#' `FALSE` if the endpoints should be set to production
#' @return Synapse client object
set_synapse_endpoints <- function(syn, staging = TRUE) {
  if (staging) {
    syn$setEndpoints(
      repoEndpoint=synapse$client$STAGING_ENDPOINTS$repoEndpoint,
      authEndpoint=synapse$client$STAGING_ENDPOINTS$authEndpoint,
      fileHandleEndpoint=synapse$client$STAGING_ENDPOINTS$fileHandleEndpoint,
      portalEndpoint=synapse$client$STAGING_ENDPOINTS$portalEndpoint
    )
  } else {
    syn$setEndpoints(
      repoEndpoint=synapse$client$PRODUCTION_ENDPOINTS$repoEndpoint,
      authEndpoint=synapse$client$PRODUCTION_ENDPOINTS$authEndpoint,
      fileHandleEndpoint=synapse$client$PRODUCTION_ENDPOINTS$fileHandleEndpoint,
      portalEndpoint=synapse$client$PRODUCTION_ENDPOINTS$portalEndpoint
    )
  }
  return(syn)
}
