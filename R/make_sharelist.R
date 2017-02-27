#' Make a nested list of shares to append into segment meta
#' 
#' Create an array of shares per Segments.Save method data structure requirements, with checks
#' 
#' @importFrom jsonlite unbox
#' 
#' @family make_segment functions
#' 
#' @param type A character vector denoting whether \emph{name} is for a \code{group} or \code{user}
#' @param name A character vector denoting the name of the \code{group} or \code{user} as denoted in \emph{type}
#' @param as_df Logical; should the return be a list (default; \code{FALSE}) or a \code{data.frame}? Usually
#' this should be left alone, unless you specifically need a more human-viewable temporary data structure
#'
#' @details 
#' The actual data structure for \emph{shares} is quite simple; this function is therefore
#' likely most useful when you need to create long lists of shares, and/or you have a complex mix of 
#' \emph{type}, perhaps e.g. stored in tabular format. 
#' 
#' \emph{type} is the constraining input in terms of allowable lengths. The assumption is that most 
#' typically, there will be a single value for \emph{type}. Since this is not always true, this function
#' allows \emph{type} to be either a vector of length 1 OR a vector of length equal to \emph{name}.
#' 
#' 
#' @note 
#' The API requires values of class \code{scalar, character}, via \code{\link[jsonlite]{unbox}}. The
#' output of this function is simply \code{character} vectors of length 1. The expected use of the output
#' is to pass to the \code{shares} argument of \code{\link{make_segment_meta}} function, which handles all
#' required argument type conversion. If you have a different application, you will need to handle the
#' coercion yourself.
#' 
#' Also, see example for a quick mechanism to quickly convert a \code{data.frame} output (from this function) 
#' into (or, rather, back into) a list. 
#'
#' @return
#' By default, a nested \code{list} of \code{length(name)}, with two named elements, \code{type,name}
#' of class \code{character}; see \strong{note}.
#' 
#' If \code{as_df = TRUE}, then a \code{data.frame} with fields \code{type,name}, with as many rows as 
#' \code{length(name)}. As is the case for the default output, all values in both fields will be of
#' class \code{character}. Mainly for debugging/troubleshooting.
#' 
#' @export
#'
#' @examples
#' make_sharelist(type = c("user"), name = sample(LETTERS, 10))
#' \dontrun{
#' make_sharelist(type = c("user", "group"), name = sample(LETTERS, 10)) #error
#' make_sharelist(type = c("User"), name = sample(LETTERS, 10)) #also error
#' 
#' # no error, demo of df use case
#' my_shares_df <- make_sharelist(type = c("user"), 
#'   name = sample(LETTERS, 1e3, replace = TRUE), 
#'   as_df = TRUE
#' ) 
#' back_list <- vector("list", nrow(my_shares_df))
#' for(i in seq_len(nrow(my_shares_df))) {
#'   back_list[[i]] <- as.list(my_shares_df[i, ])
#'   back_list[[i]] <- Map(jsonlite::unbox, back_list[[i]])
#' }
#' }
make_sharelist <- function(type, name, as_df = FALSE) {
  if(!all(is.character(type) & is.character(name))) {
    stop("'type' and 'name' must be character inputs")
  }
  # type can be a vector of length 1 or same
  if(!(length(type) == 1L | length(type) == length(name))) {
    stop("'type' must be a vector of length 1 or the same length as 'name'")
  }
  # values for type must be either "user" or "group"
  valid_type <- c("group", "user")
  diff_type  <- setdiff(unique(type), valid_type)
  if(length(diff_type) > 0L) {
    stop("Allowed values for 'type' are {", 
         paste(valid_type, collapse = ","), 
         "}\n", 
         "Unallowed value(s) detected: [", 
         paste(diff_type, collapse = ","), 
         "]"
    )
  }
  out <- Map(list, type = type, name = name)
  names(out) <- NULL
  #out <- rapply(out, function(f) unbox(f), how = "list")
  
  out_df <- do.call(rbind, Map(as.data.frame, out, stringsAsFactors = FALSE))
  if(as_df) {
    return(out_df)
  } else {
    return(out)
  }
  
}
