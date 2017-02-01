#' Make a complete request body for creating or editing a segment
#' 
#' Create a segment body to pass to Save_Segment()
#' 
#' @importFrom jsonlite unbox toJSON
#' @importFrom magrittr "%>%"
#' 
#' @family make_segment functions
#'
#' @param segment_meta A named list; metadata for container. Generally the output from \code{make_segment_meta}
#' @param segment_container A nested named list; container with one or more rules. Generally the output from 
#' \code{make_segment_container}
#'
#' @return
#' A nested named list. 
#' 
#' @details 
#' This builds up the final required structure, including nesting \emph{segment_container} under a value of
#'  \code{definition}, as required by the API. 
#'  
#' @note 
#' Premliminary checks in place for segment_meta, not yet for segment_container. Additional checks to be 
#' added for both. 
#' 
#' @export
#'
#' @examples
#' # Forthcoming
make_segment_body <- function(segment_meta, segment_container) {
  ##NOTE: Checking names for meta and container here is actually not redundant with 
  ##performing the same checks upstream, as it is possible to append values to inputs
  ##before passing them to this function, so there has to be a check at this level.
  
  ##It may even be worthwhile to check before making the call...
  
  # check that segment_meta has names, and none are blank
  meta_nms <- names(segment_meta)
  
  if(any(meta_nms %in% "")) {
    stop("One or more missing names in segment_meta")
  }
  # this should trap really strange cases
  if(length(segment_meta) != length(unique(meta_nms))) {
    stop("There is an issue in the names of segment_meta")
  }
  
  # check that segment_meta names are all allowed
  allowed_meta <- names(formals(make_segment_meta))
  
  if(length(setdiff(meta_nms, allowed_meta)) > 0L) {
    stop("Illegal name(s) found in segment_meta: \n\t", 
         setdiff(meta_nms, allowed_meta))
  }
  # TODO: Write more checks, also for container
  definition <- list(segment_container)
  
  return(
    c(segment_meta, 
      definition = definition)
    )
}