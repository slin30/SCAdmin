#' Collapse a target list-column to a delimited atomic vector
#' 
#' A custom mechanism to unnest simple list-columns by collapsing into a delimited atomic vector
#' 
#' @param x A data.frame
#' @param targ A character vector of length 1 to denote the column in \emph{x} to operate on
#' 
#' 
#' @details This wraps the internal function \code{\link{collapse_simple_listcol}} to handle more (and) typical
#' inputs, with some additional constraints. \emph{targ} is set to a default of \code{NULL}
#' intentionally, for flexibility and to enforce explicit provision of a value. However, it 
#' is expected that the valid values should only be:
#' 
#' \itemize{
#' \item \preformatted{tags}
#' \item \preformatted{compatibility}
#' }
#' 
#' @return A unnested character vector; see \code{collapse_simple_listcol}.
#' 
#' @export
#' @examples 
#' 
#' \dontrun{
#' collapse_tags   <- collapse_simple_target(x, targ = "tags")
#' collapse_compat <- collapse_simple_target(x, targ = "compatibility")
#' }
collapse_simple_target <- function(x, targ = NULL) {
  
  if(missing(targ) || is.null(targ)) {
    stop("A target column name must be specified")
  }
  if(length(targ) > 1L || !is.character(targ)) {
    stop("targ must be a character vector of length 1")
  }
  
  stopifnot(is.data.frame(x))
  
  # anticipating how this would be used, this check should never
  # be triggered unless this function is called on its own.
  if(!targ %in% names(x)) { 
    stop("'", substitute(targ), "' not found in x\n")
  }
  
  targ_col <- x[[targ]]
  
  if(length(targ_col) != nrow(x)) {
    stop("Mismatch in row count and target column length")
  }
  
  if(length(targ_col) > 1L) {
    out <- vector("character", length(targ_col))
    for(i in seq_along(targ_col)) {
      out[i] <- collapse_simple_listcol(targ_col[i])
    }
    return(out)
  } else {
    return(collapse_simple_listcol(targ_col))
  }
  
}

