#' collapse_tags
#' 
#' collapse nested tags list-column into a flattened atomic vector
#'
#' @param x An unnested list of length 1 
#'
#' @return A collapsed atomic vector, with each element of \code{x[[1]]}
#' separated by \code{, } (comma\code{space}). If a comma is encountered
#' in any element of \code{x[[1]]}, ALL elements will additionally be 
#' encapsulated by square brackets as a more readable proxy for 
#' quotes. 
#' 
#' @export
#' 
#' @details 
#' Will likely not be exported for release, but being exported for development. 
#' This does not check for any upsteam (e.g. data.frame) names, and should be used
#' directly on a column accessed by name (or double-subscripted via index/name); can
#' be easily integrated into a pipeline, so this design is intentional in anticipation.
#'
#' @examples
#' # TBD
collapse_tags <- function(x) {
  
  if(class(x) != "list") {
    stop("Expected an input of class 'list', but input is a ", class(x))
  }
  if(length(x) > 1L) {
    stop("Expected an input of length 1, but length is ", length(x))
  }
  
  x_tag <- x[[1]]
  
  # handle empty return
  if(identical(list(), x_tag)) {
    return(NA_character_)
  }
  if(!is.character(x_tag)) {
    stop("character expected for parsing 'tags', but encountered ", 
         class(x[[1]]), " at x[[1]] instead")
  }
  
  comma_present <- any(vapply(x_tag, function(f) grepl(",", f), logical(1)))
  if(comma_present) {
    collapse_prefix <- "["
    collapse_suffix <- "]"
  } else {
    collapse_prefix <- ""
    collapse_suffix <- ""
  }
  
  out <- vapply(x_tag, function(f) paste0(collapse_prefix, f, collapse_suffix), character(1))
  out <- paste(out, collapse = ", " )
  
}