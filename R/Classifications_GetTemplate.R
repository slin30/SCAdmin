#' Get a SAINT import template
#' 
#' Get a template for a single element for one or more report suites
#'
#' @importFrom RSiteCatalyst ApiRequest
#' @importFrom jsonlite unbox toJSON
#' 
#' @inheritSection call.Get_base Access Privileges
#' 
#' @param rsid_list (Required) A character vector of report suite IDs to get templates for.
#' @param element (Required) A character vector of length 1, or of any length but containing a single
#' unique value. This is the parent variable in Adobe Analytics that you wish to classify. See details.
#' @param classification_names (Optional) A character vector of the classification names to include in the template.
#' Will return all names by default. 
#' @param encoding (Optional) The text encoding to use for the output. Defaults to \code{UTF-8} and generally should
#' be left as such. If provided, must be a character vector of length 1.
#'
#' @return
#' A \code{data.frame}, where \code{nrow(data.frame) == length(rsid_list)}, containing metadata and a field 
#' called \emph{template}. Additionally, the requested \code{element} is captured as an attribute called 
#' \emph{classification_element}.
#' 
#' The \emph{template} field can be written out as a tab-separated file, or quickly viewed via \code{cat}.
#' 
#' 
#' @details 
#' This calls the \code{Classifications.GetTemplate} method. The method supports multiple report suites in a single
#' call, with the condition that the specified \emph{element} is available for classification for all requested 
#' report suites. A single report suite where the element is invalid will raise an error.
#' 
#' All values passed to \emph{rsid_list} are case-normalized and uniquified, with \code{NA} and \code{""} dropped, 
#' if present.
#' 
#' @export
#'
#' @examples
#' \dontrun{
#' Classifications_GetTemplate(c("my_suite_prod", "my_suite_dev"), element = "product")
#' }
Classifications_GetTemplate <- function(rsid_list = NULL, element = NULL, 
                        classification_names = NULL, encoding = "UTF-8")
{
  
  # check for input lengths (presence) of required inputs
  checkList <- list(
    rsid_list = rsid_list, 
    element = element 
  )
  
  check_lens <- vapply(checkList, length, integer(1L))
  if(any(check_lens == 0)) {
    stop("One or more required inputs are not present")
  }

  # process required params, since these must be present at this point
  rsid_list <- c(rsid_list, recursive = TRUE)
  rsid_list <- unique(tolower(rsid_list[!is.na(rsid_list)]))
  rsid_list <- rsid_list[!rsid_list %in% c("")]
  element   <- unique(element[!is.na(element)])
  
  if(length(unique(element)) > 1L || !is.character(element)) {
    stop("element must be a single unique value of type character")
  }
  
  # all possible named args
  argList <- list(
    rsid_list = rsid_list, 
    element = element, 
    classification_names = classification_names, 
    encoding = encoding
  )
  
  out_args <- Filter(function(f) !is.null(f), argList)
  
  # denote the data types as scalar or not
  to_scalar <- c("element", "encoding")
  args_scalar <- lapply(out_args[c(to_scalar)], function(f) unbox(f))
  args_array  <- out_args[c(setdiff(names(argList), to_scalar))]
  
  # output the body
  body <- Filter(function(f) !is.null(f), c(args_scalar, args_array))
  
  call_ret <- ApiRequest(body = toJSON(body),
                         func.name = "Classifications.GetTemplate",
                         format = "json"
  )
  
  attr(call_ret, "classification_element") <- element
  
  call_ret
}