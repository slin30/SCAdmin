#' Create a new segment via Segments.Save
#' 
#' Push a new segment to a target report suite via the Adobe Analytics Segments.Save method
#'
#' @importFrom RSiteCatalyst ApiRequest
#' @importFrom jsonlite unbox toJSON
#'
#' @param x A segment body. Helper: \code{\link{make_segment_body}}
#' @param ... Additional methods to pass to \code{\link[RSiteCatalyst]{ApiRequest}}
#' @param override_and_edit logical value to override safety check and use this function for editing. 
#' \code{FALSE} by default. Only works if a value for \emph{id} is present
#'
#' @return
#' If successful, a data.frame with the name, report suite ID, a timestamp, the method, 
#' and the new segment_id.
#' 
#' Timestamp is always returned as a character-formatted vector of length 1, with timezone set to "UTC"
#' 
#' @note 
#' Errors if an element named \code{id} is detected, as such elements are used to edit/modify, but
#' not create (brand-new) segments. The error message in this case refers to an EDIT version of this function, 
#' which needs to be written. 
#' 
#' IMPORTANT: For temporary convenience only, this function has an override so it can be used
#' to edit, while the edit method is written. Since there are no checks for EDIT, this is an 
#' unchecked hack to edit. Since this is temporary, there is no check for \emph{id} being
#' the required scalar type (via \code{\link[jsonlite]{unbox}}).
#' @export
#'
#' @examples
#' # Forthcoming
Save_Segment <- function(x, ..., override_and_edit = FALSE) {
  # check that x has required names
  nms <- names(x)
  req_nms <- c("name", "reportSuiteID", "definition")
  if(length(setdiff(req_nms, nms)) > 1) {
    stop("Minimum required named elements not present")
  }
  # stop if an element of id is present
  idPresent <- "id" %in% names(x)
  if(idPresent) {
    ## temporary hook to override and allow edit without validation for dev only!
    if(!override_and_edit) {
      stop(
        paste0("An element of 'id' was detected. This function only saves NEW segments;\n", 
               "please use the corresponding EDIT function to modify existing segments")
      )
    }
  }
  # Capture the name and rsid
  nm <- as.character(x[["name"]])
  rsid <- as.character(x[["reportSuiteID"]])
  timestamp <- as.character(as.POSIXct(Sys.time(),tz="UTC"))
  ## To capture this is an edit, with hack workaround
  if(override_and_edit) {
    method <- "Edit"
  } else {
    method <- "Save" # denote that this is a save, since later can have edit, delete
  }
  
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