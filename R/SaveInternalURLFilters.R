#' SaveInternalURLFilters
#' 
#' Save Internal URL Filters for one or more Report Suites
#'
#' @param rsid A vector of report suite IDs
#' @param urls A vector of urls to filter, for the target report suite(s)
#'
#' @details 
#' This is a convenient way to batch-add URLs to one or more report suites. 
#' 
#' @note 
#' There is minimal input checking at the moment; this will be addressed prior to release, but 
#' use cautiously at the moment, and please raise an issue if you encounter any strange behavior.
#' 
#' @section Access Privileges: 
#' This function calls an Adobe Analytics method that requires administrative/elevated privileges
#' 
#' @return \code{TRUE}, if successful
#' @export
#'
#' @examples
#' \dontrun{
#' my_url <- c("test_url_1_DOT_COM", "test_url_2.com")
#' my_rs  <- "my_reportSuiteID"
#' 
#' SaveInternalURLFilters(rsid = my_rs, urls = my_url)
#' }
SaveInternalURLFilters <- function(rsid, urls) {
  if(!is.atomic(rsid) || !is.atomic(urls)) {
    stop("rsid and urls must both be atomic vectors")
  }
  #Could also accept list, with c(..., recursive=TRUE)
  
  body <- list(
    rsid_list = rsid,
    internal_url_filters = urls
  )
  # NOTE THE NAME OF THE METHOD!!!
  ApiRequest(
    body = jsonlite::toJSON(body),
    func.name = "ReportSuite.SaveInternalURLFilters"
  )
  
}