#' Create a new segment via Segments.Save
#' 
#' Push a new segment to a target report suite via the Adobe Analytics Segments.Save method
#'
#' @importFrom RSiteCatalyst ApiRequest
#' @importFrom jsonlite unbox toJSON
#'
#' @param x A segment body, generally constructed with \code{make_segment_body()}
#' @param ... Additional methods to pass to \code{ApiRequest()}. 
#'
#' @return
#' If successful, a data.frame with the name, report suite ID, a timestamp, the method, 
#' and the new segment_id.
#' 
#' Timestamp is always returned as a character-formatted vector of length 1, with timezone set to "UTC"
#' 
#' @note 
#' Error checking not yet implemented, but will be.
#' @export
#'
#' @examples
#' # Forthcoming
Save_Segment <- function(x, ...) {
  # check that x has required names
  nms <- names(x)
  req_nms <- c("name", "reportSuiteID", "definition")
  if(length(setdiff(req_nms, nms)) > 1) {
    stop("Minimum required named elements not present")
  }
  # Capture the name and rsid
  nm <- as.character(x[["name"]])
  rsid <- as.character(x[["reportSuiteID"]])
  timestamp <- as.character(as.POSIXct(Sys.time(),tz="UTC"))
  method <- "Save" # denote that this is a save, since later can have edit, delete
  
  # not strictly required to create a function here, but maybe useful
  # for abstraction later with edit/delete?
  fun <- function(x) {
    ApiRequest(toJSON(x), "Segments.Save", ...)
  }
  
  out_id <- fun(x) # capture the segment id return
  
  data.frame(nm = nm, 
             rsid = rsid, 
             method = method,
             timestamp = timestamp,
             segment_id = out_id, 
             stringsAsFactors = FALSE
  )
  
}