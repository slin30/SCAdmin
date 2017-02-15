#' split_single_row_ret
#' 
#' Internal function - Further splits and checks output from split_segment_ret
#' 
#' @param seg_ret_single A single row from a split data.frame, i.e. the output of \code{\link{split_segment_ret}}
#' 
#' @return A list of length two, with elements named \code{segment_meta} and \code{segment_def} containing,
#' repectively, the segment metadata (non-definition) and segment definition (\code{definition})
#' 
#' @note This is mainly a helper for \code{\link{recur_flatten_nested_container}}
#' 
#' @family internal
split_single_row_ret <- function(seg_ret_single) {
  # Input must be a df with one row, from seg_ret
  if(!is.data.frame(seg_ret_single) || nrow(seg_ret_single) > 1L) {
    stop("seg_ret must be a single-row data.frame")
  }
  
  if(!"definition" %in% names(seg_ret_single)) {
    stop("definition missing")
  }
  
  seg_meta <- setdiff(names(seg_ret_single), "definition")
  seg_def <- c("definition")
  
  list(segment_meta = seg_ret_single[seg_meta],
       segment_def = seg_ret_single[[seg_def]]
  )
}