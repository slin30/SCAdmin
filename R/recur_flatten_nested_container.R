#' recur_flatten_nested_container
#' 
#' Internal function - recursively flattens nested containers (definitions)
#' 
#' @param x The output of \code{\link{split_single_row_ret}}. Must contain an element named \code{segment_def}, 
#' which is output by \code{split_single_row_ret()}.
#' @param lst Used only for recursion; accumulates results
#' @param d Used only for recursion; used to name outputs and as internal index
#' 
#' @return A flattened version of containers with names of definitions (now flattened)
#'  prefixed by \code{L[0-9]+}
#' 
#' @note This function does not do much for simple, i.e. unnested segments, but is required
#' for nested segments, which includes stacked segments. 
#' 
#' This currently does NOT work for segments that have been created sub-optimally. The 
#' definition of \emph{sub-optimally} is subjective, but includes:
#' 
#' \itemize{
#' \item Unnecessarily nested containers
#' \item The above, interspersed with rules at varying hierarchies
#' \item Stacked segments where one or more upstream segments have been created
#'       in accordance with (but not limited to) the aforementioned possibilities
#' }
#' 
#' This function includes a helper that may not be required any longer, but does not hurt
#' 
#' @family internal
recur_flatten_nested_container <- function(x, lst = c(), d = 0L) {
  
  
  if("segment_def" %in% names(x)) {
    x <- x[["segment_def"]]
  }
  
  is_nested <- .l_helper_checkNul_nested(x)
  
  if(!is_nested) {
    message("Took ", d, " recursion(s) to converge")
    
    x.tmp <- x[["container"]]
    x.tmp_out <- Filter(function(x) !is.null(x), x.tmp[["rules"]])
    
    lst_nm <- paste("L", d, sep = "")
    lst <- c(lst,
             structure(
               list(
                 list(cont_meta = x.tmp[setdiff(names(x.tmp), "rules")],
                      cont_rule = x.tmp_out
                 )
               ), .Names = lst_nm
             )
    )
    
    return(lst)
    
  } else {
    
    x <- Filter(function(x) !is.null(x), x)
    
    x.tmp      <- Filter(function(x) !is.null(x), x[["container"]])
    x.tmp_nest <- Filter(function(f) !is.null(x), x.tmp[["rules"]])
    x.tmp_nest <- Filter(function(x) !is.null(x), x.tmp_nest)
    
    #This check no longer is needed for properly stacked segments, so not
    #sure it is needed-- definitely have to tweak
    if(length(x.tmp_nest) > 1L) {
      stop("Detected an unusual structure in segment")
    }
    
    lst_nm <- paste("L", d, sep = "")
    lst <- c(lst,
             structure(
               list(
                 list(cont_meta = x.tmp[setdiff(names(x.tmp), "rules")],
                      cont_rule = x.tmp_nest[[1]][["value"]]
                 )
               ), .Names = lst_nm
             )
    )
    recur_flatten_nested_container(x = x.tmp_nest[[1]], lst = lst, d = d+1L)
    
  }
  
}

# Helper to handle null without pre-parsing, which gets
# complicated with recursion when you actually need
# this functionality to handle (only somewhat) poorly created segments
# that can be parsed with a bit NULL handling
.l_helper_checkNul_nested <- function(x) {
  if(! "container" %in% names(x)) {
    return(FALSE)
  }
  
  x_noNULL <- Filter(function(x) !is.null(x),
                     x[[c("container", "rules")]]
  )
  
  "container" %in% names(x_noNULL[[1]])
  
}

