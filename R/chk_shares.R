#' chk_shares
#' 
#' Internal function - check input structure of 'shares' for a call to Segments.Get with shares
#' 
#' @family internal
#'
#' @param x A list of shares, possibly made with \code{\link{make_sharelist}}
#' 
#' @details 
#' This performs the following checks, in order:
#' 
#' \enumerate{
#' \item High-level structure, by determining if \emph{x} is coercible to 
#'       a \code{data.frame}.
#' \item names, which must be \code{type,name}
#' \item values of field \code{type}, which must be one or both of \code{user,group}
#' \item column classes, which must be \code{scalar}, via \code{\link[jsonlite]{unbox}}
#' }
#' 
#' Failing any of these checks is an error.
#' Passing all of these checks returns \code{TRUE}
#'
#' @return \code{TRUE} or an error (with error message)
#'
#' @examples
#' #TBD
chk_shares <- function(x) {
  
  # bind to df for ease and as check
  share_df <- tryCatch({
    do.call(rbind, Map(as.data.frame, x, stringsAsFactors = FALSE))
  }, error = function(e) {
    message(e, "\r", 
            "This is most likely due to unexpected input structure in 'shares'")
  }, finally = NULL
  )
  
  # check names
  fields_ok <- all(
    ncol(share_df) == 2L &
      names(share_df) %in% c("type", "name")
  )
  if(!fields_ok) {
    stop("Expected names of 'type' and 'name' but instead 
         detected names of: ", names(share_df))
  }
  # check vals of 'type'
  typeVals <- unique(share_df[["type"]])
  valDiff  <- setdiff(typeVals, c("user", "group"))
  
  typeVals_ok <- all(
    length(typeVals) <= 2L &
      length(valDiff) == 0L
  )
  if(!typeVals_ok) {
    stop("Unexpected value(s) detected in 'type':\n\t", 
         paste(valDiff, collapse = ","))
  }
  
  # check class
  class_ok <- vapply(share_df, function(f) 
    "scalar" %in% class(f), logical(1))
  if(!all(class_ok)) {
    stop("Input values for 'shares' must be of class 'scalar'")
  }
  
  return(TRUE)
}