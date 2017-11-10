#' Custom recursive status checker for Classifications export job requests
#' 
#' Periodically check the status of a job_id from a call to Classifications.CreateExport
#'
#' @family Classifications methods
#' 
#' @param id (required)
#' @param iters (optional)
#' @param max_iters (optional)
#' @param iter.wait (optional)
#'
#' @return
#' See \code{\link{Classifications_GetStatus}}
#' @export
#'
#' @examples
#' # TBD
CheckExportJobStatus <- function(id, iters = 0L, max_iters = 10L, iter.wait = 10) {
  
  if(iters > max_iters) {
    message("Max iterations (",max_iters, ") exceeded.", 
            "Returning input id")
    return(id)
  }
  
  status_ret <- Classifications_GetStatus(id)
  check <- .check_status_ret(status_ret)
  if(all(unlist(check))) {
    message("Export job_id ", id, " completed in ", 
            iters, " iteration(s)")
    return(status_ret)
  } 
  
  report_done <- check[["report_done"]]
  pages_ready <- check[["pages_ready"]]
  
  m.report_running <- paste0("In iter ",
                             iters, ": report not yet ready. Trying again in ",
                             iter.wait, " seconds")
  m.pages_running <- paste0("In iter ", 
                            iters, ": report COMPLETE, but pages not yet ready. Trying again in ", 
                            iter.wait, " seconds")
  
  message_print <- m.report_running
  if(report_done && ! pages_ready) {
    message_print <- m.pages_running
  }
  
  message(message_print)
  Sys.sleep(iter.wait)
  CheckExportJobStatus(id = id, iters = iters + 1L, max_iters = max_iters, iter.wait = iter.wait)
  
}


NULL
.check_status_ret <- function(x) {
  req_class <- "data.frame"
  req_nms <- c("id", "type", "viewable_pages", "status")
  req_typevals <- c("job_id", "file_id")
  req_fileid_status <- "Ready"
  
  if(class(x) != req_class) {
    stop("Class of '", req_class, "' expected")
  }
  
  xnms <- names(x)
  delta_nms <- setdiff(req_nms, xnms)
  if(length(delta_nms) > 0L) {
    stop("Expected names not detected. The following are missing:\n\t{", 
         paste(delta_nms, collapse = ", "), 
         "}"
    )
  }
  
  status_coll <- paste(x[["status"]], collapse = "|")
  # possible for status to be "In Progress"
  is_inProgress <- grepl("In Progress|Building Results", status_coll)
  is_progress <- grepl("In Progress", status_coll)
  is_building <- grepl("Building Results", status_coll)
  if(is_inProgress) {
    if(is_progress) {
      msg <- "in progress"
    }
    if(is_building) {
      msg <- "building"
    }
    message(paste0("(", msg, ") "), appendLF = FALSE)
    return(
      list(report_done = FALSE, 
           pages_ready = FALSE
      )
    )
  }
  
  type_vals <- x[["type"]]
  check_type_vals <- all(req_typevals %in% type_vals)
  if(! check_type_vals) {
    stop("Not all expected id values found in 'id' column of ", 
         substitute(x)
    )
  }
  
  # coerce viewable_pages to integer
  x[["viewable_pages"]] <- as.integer(x[["viewable_pages"]])
  
  # output list for checking
  fileid_status <- as.list(
    x[x[["type"]] == "file_id", c("status", "viewable_pages")]
  )
  
  
  report_done <- fileid_status[["status"]] == req_fileid_status
  pages_ready <- fileid_status[["viewable_pages"]] > 0L
  
  list(report_done = report_done, 
       pages_ready = pages_ready
  )
  
}

