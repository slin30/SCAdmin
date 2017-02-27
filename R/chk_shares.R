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
#' \item values to each of \code{type,name} are \code{character} vectors of length 1
#' }
#' 
#' Failing any of these checks is an error.
#' Passing all of these checks returns \code{TRUE}
#'
#' @return \code{TRUE} or an error (with error message)
#' 
#' @note 
#' Used in the context of \code{\link{make_segment_meta}} to validate inputs to the \emph{shares} argument,
#' and also for testing the output of \code{make_sharelist}. Does not expect, although allows, values that are
#' already coerced to \code{scalar}.
#'
#' @examples
#' #TBD
chk_shares <- function(x) {
  # check input length to ensure nested list fails
  if(length(x[[1]]) == 1L || is.null(names(x[[1]])) || !names(x[[1]]) %in% c("type", "name")) {
    stop(
      paste("Unexpected input structure\n", 
            " x must be nested list of length equal to number of shares with named elements"
    )
    )
  }
  
  # bind to df for ease and as check
  share_df <- tryCatch({
    do.call(rbind, Map(as.data.frame, x, stringsAsFactors = FALSE))
  }, error = function(e) {
    message(e, "\r", 
            "This is most likely due to unexpected input structure in 'shares'")
  }, finally = NULL
  )
  
  # check named element length
  len_ok <- nrow(share_df) == length(x)
  if(!len_ok) {
    stop(
      paste("Mismatch in length of x and expected check length\n", 
             " Most likely this is due to one or more elements being vectors of length > 1")
    )
  }
  
  
  # check names
  fields_ok <- all(
    ncol(share_df) == 2L &
      names(share_df) %in% c("type", "name")
  )
  if(!fields_ok) {
    stop(
      paste("Expected names of 'type' and 'name' but instead",
            "detected names of: \n", names(share_df))
)
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
  
  # check class for character
  class_ok <- vapply(share_df, function(f)
    "character" %in% class(f), logical(1))
  if(!all(class_ok)) {
    stop("Input values for 'shares' must be of class 'character'")
  }
  
  return(TRUE)
}