#' Permissions.GetGroup call with error handling
#'
#' Get group information via group name or ID, optionally get permissions as well
#' 
#' @importFrom RSiteCatalyst ApiRequest
#' @importFrom jsonlite unbox toJSON
#' 
#' @family get group functions
#'
#' @param x chr or int. a vector of group name(s) and/or group id(s) to query. Duplicates will be automatically removed.
#' @param include_permissions logi. denote whether to also include category_permissions data for each group. 
#' Defaults to \code{FALSE}.
#' @param ... additional arguments to \link[RSiteCatalyst]{ApiRequest} (optional).
#' 
#' @details 
#' This function calls the Adobe Analytics 1.4 
#' \href{https://marketing.adobe.com/developer/documentation/analytics-administration-1-4/r-getgroup-1}{Permissions.GetGroup}
#' method with additional error handling, as the (internal; not exported) \code{.Get_Group} function is wrapped in 
#' purrr::\link[purrr]{safely}. 
#' 
#' This function is not normally needed by itself, but is exported for advanced users who wish to perform their own
#' return value restructuring, rather than use the convenience function \code{Get_Group}, which combines this function 
#' and \code{\link{restr.Get_Group}}. Also useful for troubleshooting in the (likely) event the core API changes and/or if 
#' (less-likely) one of the package dependencies changes in an unexpected manner.
#' 
#' @note 
#' Like all functions in this package that interact with the Adobe Analytics API directly, this function relies upon  
#' \link[RSiteCatalyst]{ApiRequest} for authentication, json parsing, and basic error handling. This means you must 
#' still authenticate through the RSiteCatalyst API, and any fundamental changes to \code{RSiteCatalyst::ApiRequest} have the 
#' potential to change all dependent functions in this package. 
#' 
#' This function should not be confused with RSiteCatalyst::\link[RSiteCatalyst]{GetGroups}, which calls a different 
#' Adobe Analytics method altogether. 
#' 
#' @return
#' A nested list of length (unique) \code{x}. Each first-level list element is a named (using the corresponding call value) 
#' nested list of length two consisting of the named elements \emph{result} and \emph{error}. 
#' 
#' It is possible for either (but not both) \emph{result} or \emph{error} to be \code{NULL}. Because the 
#' \emph{Permissions.GetGroup} method returns errors for invalid call (query) values, this function simply returns the 
#' server error message. In other words, a call value either throws an error (and the error message is captured in \emph{error}) 
#' or succeeds (and the returned data is captured in \emph{result}). 
#' 
#' The actual contents within each of \emph{result} or \emph{error} may vary as follows:
#' 
#' \itemize{
#' \item \emph{error}:
#'     \itemize{
#'     \item If no errors, \code{NULL}
#'     \item If errors, a list of length 2 with named elements \emph{message} and \emph{call}
#'     }
#' \item \emph{result}:
#'     \itemize{
#'         \item If no errors and \code{include_permissions = FALSE} a list of length 6; in order:
#'             \itemize{
#'             \item group_id (\code{integer} vector of length 1)
#'             \item group_name (\code{character} vector of length 1)
#'             \item group_description (\code{character} vector of length 1)
#'             \item all_report_suite_access (\code{logical} vector of length 1)
#'             \item rsid_list (\code{character} vector of length 1)
#'             \item user_list (\code{character} vector)
#'             }
#'        \item If no errors and \code{include_permissions = TRUE}, a the above 6 plus a 7th element of 
#'              \emph{category_permissions}, a nested \code{data.frame} with 4 fields:
#'                 \itemize{
#'                 \item category (\code{character} vector of length 4)
#'                 \item name (\code{character} vector of length 4)
#'                 \item access (\code{character} vector of length 4)
#'                 \item permissions (\code{list} of length 4, each element a \code{data.frame} with 3 fields)
#'                     \itemize{
#'                     \item id (\code{integer} vector)
#'                     \item name (\code{character} vector)
#'                     \item access (\code{logical} vector)
#'                     }
#'                 }
#'       \item If errors, \code{NULL}
#'     }
#' }
#' 
#'  
#' 
#' @export
#' @examples 
#' \dontrun{
#' #Can use either name or id
#' by_id   <- call.Get_Group(136031L, include_permissions = TRUE)
#' by_name <- call.Get_Group("RAP-Mendeley Admin Access", include_permissions = TRUE)
#' all.equal(by_id[[1]], by_name[[1]])
#'  
#' #Can also do both in a single call
#' by_either <- call.Get_Group(c(136031L, "RAP-Mendeley Admin Access"), include_permissions = TRUE)
#' all.equal(by_either[[1]], by_either[[2]])
#'  
#' #All errors
#' err_only <- call.Get_Group(c("a", 1), include_permissions = TRUE)
#'  
#' #Mixed; some error, some working
#' mixed <- call.Get_Group(c(136031L, "a"), include_permissions = TRUE)
#'  
#' #NULL values will not trigger a query and return an empty list
#' null_call <- call.Get_Group(NULL) # returns list(0)
#' identical(null_call, list())
#'  
#' #However, NA will trigger a query and return an error
#' na_call <- call.Get_Group(NA)
#'  
#' #As will blank ("")
#' blank_call <- call.Get_Group("")
#'  
#' #Mixing NULL into a call with non-NULL values will return result of length 
#' # x[!is.null(x)]
#' test_null_mix <- call.Get_Group(c(NULL, "NULL", "NA", "", 135994L), include_permissions = TRUE)
#' length(test_null_mix)
#' }
call.Get_Group <- function(x, include_permissions = NULL, ...) {
  if(any(duplicated(x))) {
    warning("Duplicated values found in rsid values; ", 
            "making values unique via unique()")
    
  }
  
  x <- unique(unlist(x))
  xlen <- length(x)
  call_list <- vector("list", xlen)
  
  for(i in seq_along(x)) {
    message("Processing query ", i, " of ", xlen)
    call_list[[i]] <- .Get_Group(x = x[[i]],
                                 include_permissions = include_permissions, 
                                 ...)
  }
  call_list
  names(call_list) <- x
  
  call_list
}

NULL

#' @keywords internal
.Get_Group <- purrr::safely(
  function(x, include_permissions = NULL, ...) {
    if(is.null(include_permissions)) {
      include_permissions <- FALSE
    }
    #Auto-detect if passing in id or name
    xkey <- .l_helper_name_or_id(x)
    
    body <- list(unbox(x), 
                 include_permissions = unbox(include_permissions))
    names(body)[[1]] <- xkey
    
    query <- toJSON(body)
    fun <- "Permissions.GetGroup"
    ApiRequest(body = query, func.name = fun, ...)
  }
)

NULL

#Auto-detect if passing in name or ID for Group methods
.l_helper_name_or_id <- function(x) {
  x_isID <- grepl("^\\d\\d+", x)
  if(x_isID) {
    return("group_id")
  } else {
    return("group_name")
  }
}