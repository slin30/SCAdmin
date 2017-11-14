#' (Experimental) Parse a job export return
#' 
#' Parse the return from a call to Classifications.GetExport
#' 
#' @import data.table
#' @family Classifications methods
#'
#' @param xs (required) A nested \code{list}
#' @param also_bind (optional) Combine all pages into a \code{data.table}? 
#'        Defaults to \code{TRUE}
#' @param ... Additional argumnets to pass to \code{data/table::rbindlist}
#'
#' @details 
#' This is an experimental parser for returns from e.g. \code{\link{Classifications_GetExport}}. 
#' For flexibility, one of two possible structures are permitted, roughly delimited by whether
#' the return has been run through \code{\link{split_multi_jobfile}}.
#' 
#' \itemize{
#' \item A non-nested return, from a single report suite       
#' \item A nested return, typically from more than one report suite, although single-report-suite
#'       returns processed by \code{split_multi_jobfile} would also fall into this categoty
#' }
#' 
#' @note 
#' The \emph{experimental} denomination refers to the fact that classifications can be hierarchical, 
#' and at the moment, this parser does not handle such situations. Furthermore, there is rather
#' limited input checking considering the number of likely (not yet fully understood) potential
#' return structures. 
#' 
#' \code{...}, while optional, should be used to preserve \code{job_id, file_id, page} information. 
#' Furthermore, consider using \code{fill=TRUE} if passing in returns from more than one report suite, 
#' as there is a high likelihood of different columns across suites. 
#' 
#' Finally, \code{NULL} values in multi-report-suite returns are possible, depending on how they
#' are processed. This is not handled at the moment, but their presence (along with zero-row returns)
#' will cause \code{rbindlist} to error.
#' 
#' @return
#' Depending on the input, either a \code{data.table} or a \code{list} of \code{data.table}s. 
#' @export
#'
#' @examples
#' \dontrun{
#' # using \code{...}
#' parse_job_return(x, use.names = TRUE, idcol = "ID")
#' }
parse_job_return <- function(xs, also_bind = TRUE, ...) {
  
  if(is.null(is_nested_return(xs))) {
    stop("Incompatible structure detected")
  }
  
  is_nested <- is_nested_return(xs)
  
  if(is_nested) {
    out <- lapply(xs, function(f)
      lapply(f, .extract_job_return)
    )
    
  } else {
    out <- lapply(xs, .extract_job_return)
  }
  
  if(also_bind) {
    if(is_nested) {
      out <- lapply(out, function(f) rbindlist(f, ...))
    } else {
      out <- rbindlist(out, ...)
    }
  }
  out
}



NULL
.is_valid_jobreturn <- function(x) {
  
  # top-level list, next-level data.frame
  if(!is.list(x) && !is.data.frame(x[[1]])) {
    stop("Input must be a list with a first-level data.frame")
  }
  
  x_1 <- x[[1]]
  reqnms <- c("warnings", "header", "data")
  
  if(! all(reqnms %in% names(x_1))) {
    message("Required names of (", 
            paste(substitute(reqnms), collapse = ", "), 
            ") not all detected in x[[1]]"
    )
    return(invisible(FALSE))
  }
  
  return(invisible(TRUE))
}

NULL
.is_nested_return <- function(x) {
  
  is_straight_valid <- suppressMessages(is_valid_jobreturn(x))
  
  if(is_straight_valid) {
    return(invisible(FALSE))
  }
  if(!is_straight_valid) {
    x_1 <- x[[1]]
  }
  is_nested_valid <- is_valid_jobreturn(x_1)
  if(is_nested_valid) {
    return(invisible(TRUE))
  }
  
  if(!is_straight_valid && !is_nested_valid) {
    return(invisible(NULL))
  }
}


NULL
.extract_job_return <- function(x) {
  
  x_data <- x$data[[1]]$row
  x_colnames <- x$header[[1]]
  
  dats <- do.call(rbind, x_data)
  colnames(dats) <- x_colnames
  dats[dats == ""] <- NA_character_
  
  as.data.table(dats)
}

