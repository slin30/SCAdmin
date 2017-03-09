#' flatten_container
#' 
#' Flattens segment definition returns for non-nested containers
#' 
#' @param x A single-row data.frame, generally the output from \code{\link{split_segment_ret}}
#' 
#' @return A flattened segment definition
#' 
#' @note This is temporarily being exported during development to facilitate abstraction. Might
#' be useful to export ultimately, with some tweaks. 
#' 
#' I suspect if this is to be exported, it should probably also bring in \code{\link{split_segment_ret}} and 
#' handle multi-row inputs using e.g. \code{Map()} or \code{lapply()}. 
#' 
#' At the moment, all this is doing is wrapping around the two (not-exported) functions:
#' 
#' \enumerate{
#' \item \code{\link{split_single_row_ret}}
#' \item \code{\link{recur_flatten_nested_container}}
#' }
#' 
#' @details 
#' This does not work properly for nested containers, where "nested" is as defined in the GUI, 
#' and not in terms of the data structure from the call return. At the moment, it does not
#' error when encoutering such segments, but this will be addressed before public release. 
#' 
#' @section Note to self:
#' May want to explicitly handle nested containers (as created in GUI) with a modified version 
#' of the current recursive (data structure) flattener that can handle both nested (as created in GUI)
#' and non-nested segments. If too messy, make two functions and branch the logic by detecting which
#' structure, since you are handling this row-wise, and not df-wise.
#' 
#' @export
#' @examples 
#' #TBD
flatten_container <- function(x) {
  splitted = split_single_row_ret(x)
  
  cont_parsed <- recur_flatten_nested_container(x = splitted)
  
  flat_cont <- c(splitted[1], cont_parsed)
  return(flat_cont)
}