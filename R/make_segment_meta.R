#' Make segment-level metadata for creating (NOT editing) a segment
#' 
#' For a new segment, create required metadata and output appropriate structure for a call to Get_Segments
#'
#' @importFrom jsonlite unbox toJSON
#' @importFrom magrittr "%>%"
#' 
#' @family make_segment functions
#' 
#' @param name (Required) A character vector of length 1. This is the name (title) of the segment
#' @param reportSuiteID (Required) A character vector of length 1. This is the report suite the segment should
#' belong to. Must be a valid report suite ID and one that your login has permissions to access
#' @param description (Optional) A character vector of length 1. The segment description
#' @param favorite (Optional) A logical vector of length 1. Should the segment be added to the favorite segments list?
#' @param owner (Optional) A character vector of length 1. The Login of the user who will be the owner. Defaults to 
#' the current user if not provided.
#' @param shares NOT CURRENTLY SUPPORTED; when this changes: 
#' (Optional) A named list of length 2 with names \emph{type} (one of \code{group,user}) and \code{name}, 
#' which is the group or user name to share with. Both values must be provided as character vectors of length 1.
#' @param tags NOT CURRENTLY SUPPORTED; when this changes: 
#' (Optional) A character vector of keywords to group segments for filtering
#'
#' @details 
#' WORK IN PROGRESS; this creates the appropriate segment-level metadata, required as a part of creating
#' a new segment. 
#' 
#' The scope of this function is for creating a new segment via the Adobe Analytics \code{Segments.Save}
#' method, specifically for creating a NEW segment, as opposed to editing (updating) an existing segment. 
#' 
#' This separation is intentional, as this function will eventually be integrated into the final exported function
#' \code{Save_Segment()}, which does not yet exist. This means that this particular function will likely NOT be 
#' exported in the public release.
#' 
#' @return
#' A list, possibly nested, containing the required segment-level metadata
#' @export
#'
#' @examples
#' # Forthcoming
make_segment_meta <- function(name = NULL, reportSuiteID = NULL, description = NULL, 
                              favorite = NULL, owner = NULL, 
                              shares = NULL, tags = NULL) {
  
  # don't do anything with shares or tags yet-- those need helper functions to restructure
  # input automatically.
  # Note that `id` is not included, intentionally since that is only for editing
  # Note that `definition` is also required, but will have a separate helper
  
  # Make sure required fields have values
  if(is.null(name)) {
    stop("A value for name (for the segment) is required")
  }
  if(is.null(reportSuiteID)) {
    stop("A value for reportSuiteID is required")
  }
  if(!is.null(favorite) & class(favorite) != "logical") {
    stop("When provided, 'favorite' must be a logical value, but is currently ", 
         class(favorite))
  } 
  
  # denote what should be passed through without being unboxed
  funArgs       <- as.character(names(formals(make_segment_meta)))
  not_to_scalar <- c("shares", "tags")
  to_scalar     <- setdiff(funArgs, not_to_scalar)
  
  # make a list of things to output as scalar
  lst_scalar <- lapply(to_scalar, function(f) get(f))
  names(lst_scalar) <- to_scalar 
  
  out_lst_scalar <- Filter(function(x) !is.null(x), lst_scalar) %>%
    Map(unbox, .)
  # TODO: Helpers for shares and tags so you can pass them in and create a final
  # output list structure
  
}