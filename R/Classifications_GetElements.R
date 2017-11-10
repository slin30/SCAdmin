#' Get report suite elements that are classification-enabled
#' 
#' Get the current classification-compatible elements for one or more report suites
#'
#' @importFrom RSiteCatalyst ApiRequest
#' @importFrom jsonlite toJSON unbox
#' 
#' @family Classifications methods
#' @inheritSection call.Get_base Access Privileges
#'
#' @param rsid_list (Required) A character vector of report suite IDs to get compatibility elements for.
#' @param preprocess_rsids (Optional) If \code{TRUE}, uniquify case-normalized \emph{rsid_list} values, removing
#' \code{NA} and \code{""} as well.
#'
#' @return
#' A nested \code{data.frame}, where \code{nrow(data.frame) == length(rsid_list)}, containing report suite id, name,
#' and a nested field, \emph{compatibility_elements}. The latter contains two fields named \code{id,name} denoting 
#' the classification-enabled element \code{id} and element \code{name}, respectively. See e.g. \code{\link[tidyr]{unnest}}
#' to flatten this field or \code{flatten_nested_df} in \code{wzAAHelper}.
#'  
#' @details 
#' This function calls the Adobe Analytics \code{Classifications.GetCompatibilityElements} method. The method supports
#' multiple report suites in a single call, with the condition that the every \emph{rsid_list} value has at least one
#' classification-enabled element. A single report suite where this is not the case will raise an error.
#' 
#' This may be a useful function to call prior to e.g. \code{\link{Classifications_GetTemplate}}, to get valid
#' \emph{element} values.
#' 
#' @note 
#' The API does not distinguish between valid and accessible (within your access credentials) report suites. Completely
#' invalid (i.e. non-existant), valid but inaccessible report suite IDs, and valid report suites that do not have at least
#' one classification-enabled element will raise the same server message:
#' 
#' \preformatted{ERROR: Bad Request  -  Access denied for the selected report suite:}
#' 
#' @export
#'
#' @examples
#' \dontrun{
#' Classifications_GetElements(c("my_report_suite_1", "my_report_suite_2"))
#' }
Classifications_GetElements <- function(rsid_list, preprocess_rsids = TRUE) {
  
  rsid_list <- c(rsid_list, recursive = TRUE)
  
  if(!is.character(rsid_list)) {
    stop("rsid_list must be a character")
  }
  
  # preprocess rsid_list
  if(preprocess_rsids) {
    rsid_list <- tolower(rsid_list[!is.na(rsid_list)])
    rsid_list <- unique(rsid_list)
  }
  
  argList <- list(
    rsid_list = rsid_list
  )
  
  body <- jsonlite::toJSON(argList)
  call_ret <- ApiRequest(body, 
                         func.name = "Classifications.GetCompatibilityElements"
  )
  
  clean_ret <- call_ret[!is.na(call_ret[["rsid"]]), ]
  
  # also kill compatibility elements field, where value is an empty list
  # This should never happen since we uniquify and tolower
  elems_check <- vapply(clean_ret[["compatibility_elements"]], length, integer(1))
  out <- clean_ret[elems_check > 0L, ]
  
  diff <- setdiff(rsid_list, out[["rsid"]])
  
  if(length(diff) > 0L) {
    diff_msg <- paste(diff, collapse = ", ")
    warning("The following rsid_list value(s) did not return valid rows\n\t", 
            substitute(diff_msg), 
            "\n  ...and have been dropped from the return"
    )
  }
  
  out
}