#' flatten_container
#' 
#' Mainly for debugging and development purposes; flattens segment definitions containing nested containers
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
#' @export
#' @examples 
#' #TBD
flatten_container <- function(x) {
  splitted = split_single_row_ret(x)
  
  cont_parsed <- recur_flatten_nested_container(x = splitted)
  
  flat_cont <- c(splitted[1], cont_parsed)
  return(flat_cont)
}