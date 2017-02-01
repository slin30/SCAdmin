#' Make a segment container, including rules
#' 
#' Construct a segment container (not definition) with one or more rules. Does not yet support sequential segments.
#' 
#' @importFrom jsonlite unbox toJSON
#' @importFrom magrittr "%>%"
#' 
#' @family make_segment functions
#'
#' @param name (Optional) Container name. A character vector of length 1
#' @param type (Required) Container type. One of \code{hits,visits,visitors}. A character vector of length 1. Note
#' restrictions apply for sequential segments. Does not yet support a value of \code{logicgroup} for 
#' sub-containers. 
#' @param operator (Optional) How should rules within the container be evaluated? One of \code{and,or,then}. A
#' character vector of length 1. Note that restrictions apply for \code{then}. If not provided, defaults to \code{and}. 
#' Not meaningful for single-rule containers.
#' @param rules (Required) A list of rules, with required names. Generally created with \code{make_element_rules}.
#' @param exclude (Optional) Should the container exclude, rather than include, data that matches the 
#' rule(s)? A logical vector of length 1. Defaults to \code{FALSE}
#'
#' @return
#' A nested list
#' @export
#'
#' @examples
#' # Forthcoming
make_segment_container <- function(name = NULL, type = NULL, operator = NULL, 
                                   rules = NULL,
                                   exclude = NULL) {
  #TODO: Write input value checks!
  
  # type must be one of hits,visits,visitor
  if(is.null(type)) {
    stop("An argument for type is required")
  }
  # operator must be one of and,or,then
  if(is.null(operator)) {
    message("operator is missing, and will default to 'and'")
    operator <- "and"
  }
  if(is.null(exclude)) {
    exclude <- FALSE
  }
  if(is.null(rules)) {
    stop("A container must have at least one rule")
  }
  
  #TODO: Need to also check input structure of rules
  
  #TODO: This is copied from make_segment_meta; consider extracting into helper
  funArgs       <- as.character(names(formals(make_segment_container)))
  not_to_scalar <- c("rules")
  to_scalar     <- setdiff(funArgs, not_to_scalar)
  
  # make a list of things to output as scalar
  lst_scalar <- lapply(to_scalar, function(f) get(f))
  names(lst_scalar) <- to_scalar
  
  out_lst_scalar <- Filter(function(x) !is.null(x), lst_scalar) %>%
    Map(unbox, .)
  
  out_lst_scalar[["rules"]] <- rules
  
  return(list(container = out_lst_scalar))
}