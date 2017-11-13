#' Get one or all pages from a complete export job
#' 
#' Get one or all pages from completed classification job export
#' 
#' @importFrom RSiteCatalyst ApiRequest
#' @importFrom jsonlite unbox toJSON
#' 
#' @inheritSection call.Get_base Access Privileges
#' @family Classifications methods
#' 
#' @param status_return (required) The return from a complete export job
#' @param all_pages (optional) Should all available pages be returned? Defaults to \code{TRUE}
#' @param page (optional) If provided, will only return this specific page. Ignored if \code{all_pages} is \code{TRUE}
#'
#' @details 
#' \href{https://marketing.adobe.com/developer/documentation/classifications-1-4-saint/r-getexport}{Adobe 1.4 documentation}
#' 
#' The extends the basic functionality of the baseline method by allowing all possible \code{file_id} pages to be 
#' returned (default). Set the value of \emph{all_pages} to \code{FALSE} to reproduce the behavior of the 
#' default method. 
#' 
#' @note 
#' The returned value is nested one additional value to ensure a consistent structure for single or multi-page
#' returns. 
#' 
#' 
#' @return
#' A named, nested \code{list} with as many elements as the number of pages
#' returned, for each \emph{file_id}. The (Each) first-level element will
#' be of length 3, with the following basic structure:
#' 
#'  \itemize{
#'  \item \code{warnings} A \code{logical} vector of length 1. \code{NA}
#'        if no warnings.
#'  \item \code{header} A \code{list} of length 1, the sub-element containing 
#'        the field names for the returned \code{data}
#'  \item \code{data} A \code{list} of length 1, the sub-element containing a
#'        nested \code{data.frame}. 
#'  } 
#'  
#' Each element is named using the \code{job_id}, \code{file_id}, and page number of the input
#' \emph{status_return}.
#' 
#' @export
#'
#' @examples
#' # TBD
Classifications_GetExport <- function(status_return = NULL, all_pages = TRUE, page = NULL) {

  arglist <- .parse_job_return(status_return)
  
  check_lens <- vapply(arglist, function(f) length(f) == 1L, logical(1))
  if(! all(check_lens)) {
    stop("Only vectors of length 1 allowed as arguments to file_id and page")
  }
  
  # page value validation
  if(!is.null(page)) {
    if(length(page) != 1L) {
      stop("page must be a length 1 vector")
    }
    if(is.na(as.integer(page))) {
      stop("page must be integer or non-NA when coerced to integer")
    }
    if(page < 1L) {
      stop("page must be greater than or equal to 1L")
    }
    if(page > arglist$page) {
      message("page > returned page; using returned value of ", 
              arglist$page)
      page <- NULL
    }
  }
  
  # setting all_pages to FALSE and not providing a page simply returns the first page
  if(!all_pages && is.null(page)) {
    arglist$page <- 1L
  }
  
  if(!is.null(page)) {
    arglist$page <- page
  }
  
  calls <- lapply(seq_len(arglist$page), function(f) 
    list(file_id = arglist$file_id, page = f)
  )
  
  pad_fx <- max(c(2, nchar(arglist$page)))
  fmt <- paste0("%0", pad_fx, "i")
  names(calls) <- paste0(arglist$job_id, "_", arglist$file_id, "_", sprintf(fmt, seq_len(arglist$page)))
  
  bodies <- lapply(calls, function(f) Map(unbox, f))
  
  out <- vector("list", length(bodies))
  names(out) <- names(bodies)
  for(i in seq_along(bodies)) {
    message("In job ",arglist$job_id, ", file ", bodies[[i]]$file_id, ": getting export page ", i, " of ", length(bodies))
    out[[i]] <- ApiRequest(toJSON(bodies[[i]]), func.name = "Classifications.GetExport")
  }
  
  out
  
}

NULL
.parse_job_return <- function(x) {
  
  if(! all(unlist(.check_status_ret(x)))) {
    stop("Input inconsistent with a completed export")
  }
  
  targ_rowidx <- which(x[["type"]] == "file_id")
  targ_row <- x[targ_rowidx, ]
  
  file_id <- targ_row[["id"]]
  page <- targ_row[["viewable_pages"]]
  job_id <- x[x[["type"]] == "job_id", "id"]
  
  Map(
    as.integer, list(job_id = job_id, file_id = file_id, page = page)
  )
}