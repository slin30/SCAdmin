#' Get one or all pages from a completed export job
#' 
#' Get one or all pages from a completed classification job export, for a single file_id
#' 
#' @importFrom RSiteCatalyst ApiRequest
#' @importFrom jsonlite unbox toJSON
#' 
#' @inheritSection call.Get_base Access Privileges
#' @family Classifications methods
#' 
#' @param status_return (required) The return from a complete export job. Expected input is a two-row \code{data.frame}
#'        containing a single \code{job_id} and a single \code{file_id}. 
#' @param all_pages (optional) Should all available pages be returned? Defaults to \code{TRUE}
#' @param page (optional) If provided, an integer vector denoting either a specific page
#'        or a set of pages. Ignored if \code{all_pages} is \code{TRUE}
#' @param dontrun (optional) Mainly for testing purposes; only return the call body (bodies) -- 
#'        do not make any requests to API endpoint.
#'
#' @details 
#' \href{https://marketing.adobe.com/developer/documentation/classifications-1-4-saint/r-getexport}{Adobe 1.4 documentation}
#' 
#' The extends the basic functionality of the baseline method by allowing all possible \code{file_id} pages to be 
#' returned (default). Set the value of \emph{all_pages} to \code{FALSE} and leave the \emph{page} 
#' argument as \code{NULL} to reproduce the behavior of the default method (i.e. pull only the first page). 
#' 
#' If \code{all_pages=FALSE} AND a page value is supplied, the behavior of \code{page} is as follows:
#' 
#' \itemize{
#' \item If \code{< 0}, an error
#' \item If \code{0}, \code{NULL}
#' \item If \code{length == 1}, the requested page
#' \item If \code{length > 1}, all requested pages
#' }
#' 
#' Furthermore, if the (any) value of \emph{page} is greater than the maximum value of the value specified within the,
#' \code{status_return}, the latter is used as the upper limit automatically, with a message.
#' 
#' In all cases, \code{page} values are de-duplicated and requested in the order provided, i.e. if the total number of 
#' pages is 5, and you request pages as \code{c(2,1,5)}, the returned page order will be \code{2,1,5}. 
#' 
#' \code{NA} values are not allowed for page, and will raise an error, even if this occurs during coercion (if needed) 
#' via \code{as.integer}.
#' 
#' @note 
#' The returned value is nested one additional level to ensure a consistent structure for single or multi-page
#' returns. 
#' 
#' Passing in returns that contain more than one \emph{file_id} will fail. If you encounter such cases, try
#' splitting your return using \code{\link{split_multi_jobfile}} and then passing in the splitted \code{data.frame}s
#' via \code{lapply} or, if necessary, \code{Map}.
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
#' \dontrun{
#' # Create a dummy complete status_return
#' ## Note that we have a max page length of 5
#' dummy_return <- data.frame(
#'  id = c(1L, 2L),
#'  type = c("job_id", "file_id"),
#'  viewable_pages = c(0L, 5L),
#'  status = c("Completed", "Ready"),
#'  stringsAsFactors = FALSE
#' )
#' # don't run live-- just view the calls that will be generated
#' Classifications_GetExport(dummy_return, dontrun = TRUE) # basic
#' Classifications_GetExport(dummy_return, dontrun = FALSE, page = 3) # single page
#' Classifications_GetExport(dummy_return, dontrun = FALSE, page = c(1,3,5)) # multiple pages
#' Classifications_GetExport(dummy_return, dontrun = FALSE, page = 1:10) # truncates to max of 5
#' }
Classifications_GetExport <- function(status_return = NULL, all_pages = TRUE, page = NULL, dontrun = FALSE) {
  
  arglist <- .parse_job_return(status_return)
  
  check_lens <- vapply(arglist, function(f) length(f) == 1L, logical(1))
  if(! all(check_lens)) {
    stop("Only vectors of length 1 allowed as arguments to file_id and page")
  }
  
  # early return on zero-length page
  if(arglist$page == 0L) {
    message("In job ", arglist$job_id, ": zero-length page; returning NULL")
    return(NULL)
  }
  
  # page value validation
  if(!is.null(page) && !all_pages) {
    
    page <- unique(as.integer(c(page, recursive = TRUE)))
    
    if(anyNA(page)) {
      stop("page must be integer or all non-NA when coerced to integer")
    }
    if(min(page) < 0L) {
      stop("min 'page' must be greater than or equal to 0L")
    }
    if(max(page) > arglist$page) {
      message("'page' max > returned page; using returned value of ", 
              arglist$page)
      page[page > arglist$page] <- arglist$page
    }
    arglist$page <- unique(page)
  }
  
  # setting all_pages to FALSE and not providing a page simply returns the first page
  if(!all_pages && is.null(page)) {
    arglist$page <- 1L
  }
  # all_pages of TRUE (default) overrides pages, though
  # not sure if we should even request on page == 0?
  if(all_pages) {
      arglist$page <- seq_len(arglist$page)
  }
  
  # note that order of calls will be as stated in page arg if provided
  calls <- lapply(seq_along(arglist$page), function(f) 
    list(file_id = arglist$file_id, page = arglist$page[[f]])
  )
  
  
  pad_fx <- max(c(2, nchar(max(arglist$page))))
  fmt <- paste0("%0", pad_fx, "i")
  names(calls) <- paste0(arglist$job_id, "_", arglist$file_id, "_", sprintf(fmt, arglist$page))
  
  bodies <- lapply(calls, function(f) Map(unbox, f))
  
  if(dontrun) {
    return(bodies)
  }
  
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
  
  if(! all(unlist(.check_status_ret(x)[["report_done"]]))) {
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