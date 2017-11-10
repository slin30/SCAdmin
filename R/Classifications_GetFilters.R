#' Get valid date and classification element fields for filtering
#' 
#' Figure out valid date and classification column filters for an element and report suite
#' 
#' @importFrom RSiteCatalyst ApiRequest
#' @importFrom jsonlite toJSON unbox
#' 
#' @family Classifications methods
#' @inheritSection call.Get_base Access Privileges
#' 
#'
#' @param element (required) The target element (id). Must be a vector of length 1
#' @param rsid_list (required) One or more report suites. 
#'
#' @details 
#' This is a useful ancillary function to obtain filter parameters for use with 
#' e.g. \code{\link{Classifications_CreateExport}}. 
#' 
#' @note 
#' It is assumed that when more than one report suite is called, the results
#' represent the intersection of values, although this remains to be confirmed; it
#' is entirely possible that the results could instead be the union. Or something
#' else altogether.
#' 
#' At the moment, this function provides no error checking. This will be updated
#' in the future.
#' 
#' @return
#' A \code{list} of length 2, containing \code{columns} and \code{filter_dates}. 
#' This holds even when more than one report suite is requested.
#' @export
#'
#' @examples
#' \dontrun{
#' Classifications_GetFilters("evar16", "my_rsid")
#' }
Classifications_GetFilters <- function(element = NULL, rsid_list = NULL) {
  
  element <- unbox(element)
  rsid_list <- c(rsid_list, recursive = TRUE)
  
  body <- list(
    element = element, 
    rsid_list = rsid_list
  )
  
  ApiRequest(toJSON(body), func.name = "Classifications.GetFilters")
}
