#' Get the status of a Classification CreateExport request
#' 
#' Get the status of a submitted request to Classifications_CreateExport
#' 
#' @importFrom RSiteCatalyst ApiRequest
#' @importFrom jsonlite toJSON unbox
#' 
#' @family Classifications methods
#' @inheritSection call.Get_base Access Privileges
#'
#'
#' @param id_or_ret (required) An integer job_id of length 1, or the return from a 
#'        call to \code{Classifications.CreateExport}
#'
#'
#' @details 
#' \href{https://marketing.adobe.com/developer/documentation/classifications-1-4-saint/r-getstatus-1}{Adobe 1.4 documentation}    
#' 
#' This function is required to obtain the \code{file_id} and number of pages, for a 
#' return from Classifications.CreateExport. The expected input can be either an 
#' \code{integer} id of length 1, or the direct return from 
#' \code{\link{Classifications_CreateExport}}. 
#' 
#' @return
#' A \code{data.frame} containing four fields:
#' 
#' \itemize{
#' \item \code{id} The \code{id} for each \code{type}
#' \item \code{type} The type of \code{id} returned. One of \code{job_id, file_id}
#' \item \code{viewable_pages} The number of result pages. For \code{type==file_id}, should be non-zero if
#'        results are ready, or available (possibly at all)
#' \item \code{status} The status for each \code{type}. A finished return should have key-value pairs of
#'       \code{job_id:Completed} and \code{file_id:Ready}. Other status codes are possible, depending 
#'       on the status at time of query.
#' }
#' 
#' In almost all cases, the return will have two rows, although it is possible to have single-row
#' returns during initial processing stages.
#' 
#' @export
#'
#' @examples
#' \dontrun{
#' # Simplest case: pass the job_id directly
#' Classifications_GetStatus(my_integer_id)
#' 
#' # More realistic: Assume you have a job_id return from this call:
#' my_export_request <- 
#' Classifications_CreateExport(
#'     date_filter_start_date = "Jan 2017", 
#'     date_filter_end_date = "Nov 2017", 
#'     element = "evar16", 
#'     email_address = "my_email@xyz.com", 
#'     rsid_list = list("my_rsid_1")
#'     )
#' ## Now pass the value explicitly:
#' Classifications_GetStatus(my_export_request$job_id)
#' ## Or, simply pass the entire return
#' Classifications_GetStatus(my_export_request)
#' }
Classifications_GetStatus <- function(id_or_ret = NULL) {
  
  if(length(id_or_ret) > 1) {
    stop("Input must be length 1")
  }
  
  ret_res <- .is_export_return(id_or_ret)
  
  is_ret <- ret_res[["is_ret"]]
  if(is_ret) {
    id <- ret_res[["res_val"]]
  } else {
    id <- id_or_ret
  }
  
  if(!is.integer(id) || length(id) > 1) {
    stop("Only integer values of length 1 permitted")
  }
  

  body <- list(job_id = unbox(id))
  ApiRequest(toJSON(body), func.name = "Classifications.GetStatus")
  
}

NULL
.is_export_return <- function(x) {
  
  if(length(x) > 1) {
    stop("Input must be length 1")
  }
  
  if(is.integer(x)) {
    return(
      list(is_ret = FALSE,
           res_val = NULL)
    )
  }
  
  req_class <- "list"
  req_nm <- "job_id"
  
  check_class <- class(x) == req_class
  check_nm    <- names(x) == req_nm
  
  x_val <- x[[req_nm]]
  
  res_check <- Reduce("&", c(check_class, check_nm, is.integer(x_val)))
  
  if(res_check) {
    res_val <- x_val
  } else {
    res_val <- NULL
  }
  
  list(is_ret = res_check, 
       res_val = res_val
       )
  
}