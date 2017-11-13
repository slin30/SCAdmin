#' Split a multiple file_id, single job_id CreateExport return
#' 
#' Handle multiple file_id returns as individual data.frames while preserving job_id
#'
#' @family Classifications methods
#'
#' @param x (required) The return from a complete export job
#' @param dropzero (optional) Should zero-row records be dropped? Defaults to \code{TRUE}
#'
#' @details 
#' This is a helper function to handle \code{Classifications.CreateExport} requests that
#' encompass > 1 report suite, which returns more one \code{file_id} record per report suite. 
#' In these cases, passing the return directly to \code{\link{Classifications_GetExport}} will
#' fail, as the expected input can only have a single \code{file_id}. 
#' 
#' Passing returns through this function first will, in such scenarios, return a \code{list} of
#' \code{data.frames} which can be passed to \code{Classifications_GetExport} via e.g. 
#' \code{lapply}, or if using individual pages, \code{Map}. 
#' 
#' @return
#' If more than one file_id detected, a \code{list} of \code{data.frame}s, each 
#' containing the \code{job_id, type, viewable_pages}. 
#' 
#' If a single file_id is detected, the input is still processed, to ensure that the
#' output of this function has a consistent structure. That is, even if the input contains
#' a single \code{file_id} return, and therefore does not need to be splitted, it will still
#' have an additional level of nesting applied upon return.
#' @export
#'
#' @examples
#' # Example multi file_id return
#' multi_file_ret <- structure(list(id = c(151949667L, 144121611L, 144122258L, 144122277L),
#'                                  type = c("job_id", "file_id", "file_id", "file_id"), 
#'                                  viewable_pages = c("0", "5", "5", "0"),
#'                                  status = c("Completed", "Ready", "Ready", "Ready")), 
#'                             .Names = c("id", "type", "viewable_pages", "status"), 
#'                             class = "data.frame", 
#'                             row.names = c(NA, 4L)
#' )
#' split_multi_jobfile(multi_file_ret, dropzero = FALSE) # list of 3
#' split_multi_jobfile(multi_file_ret, dropzero = TRUE) # list of 2
split_multi_jobfile <- function(x, dropzero = TRUE) {
  if(! all(.check_status_ret(x)[["report_done"]])) {
    stop("Input inconsistent with a complete export")
  }
  
  # make data.frames from type, viewable_pages
  xnames <- c("id", "type", "viewable_pages", "status")
  job_rowidx <- which(x[["type"]] == "job_id")
  
  xselect <- x[-job_rowidx, xnames]
  out <- split(xselect, f = xselect[["id"]])
  
  # append the job_id as a row
  append_row <- x[job_rowidx, , drop = FALSE]
  
  splitted <- lapply(out, function(f) rbind(append_row, f))
  
  if(dropzero) {
    splitted <- Filter(function(x) sum(as.integer((x[["viewable_pages"]]))) > 0L, splitted)
  }
  
  splitted
}