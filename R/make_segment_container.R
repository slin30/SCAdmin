#' Make a segment container, including rules
#' 
#' Construct a segment container (not definition) with one or more rules. Can be used for nested containers as well.
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
#' @param rules (Required) A list of rules, with required names, usually made using \code{\link{make_element_rules}}.
#' Alternatively, you can pass a \code{list} of container(s) if creating a nested segment.
#' @param exclude (Optional) Should the container exclude, rather than include, data that matches the 
#' rule(s)? A logical vector of length 1. Defaults to \code{FALSE}
#' 
#' @details 
#' A complete segment body comprises two basic parts: the metadata and one or more containers; this is a helper to create
#' the latter. This function constructs rule-based containers, which means you have rules nested within a container. The most
#' common use case is to pass the output of \code{make_element_rules} into the \code{rules} parameter; this 
#' creates a flat container with one or more rules. 
#' 
#' You can also use this function to (recursively, if desired) create nested segment containers. This is useful e.g. when
#' you have one set of rules that comprise inclusion criteria, and another set of rules that comprise exclusion criteria.
#' You would combine the two sets of criteria (as containers) within a top-level container with an \code{and} operator
#' This requires a nested container, because the container operator for each individual container would be \code{or}, 
#' but the two containers should be evaluated with \code{and}. See examples.
#' 
#' 
#' @note 
#' As is true for all \code{make_} functions, there is no direct interaction with any API, as these are mainly helpers. 
#' You are free to construct your own segment components without using any/all of the \code{make_} functions. 
#' 
#'
#' @return
#' A nested list, which may be passed to the appropriate parameter of \code{\link{make_segment_body}}.
#' @export
#'
#' @examples
#' # simple flat container
#' ruleset_1 <- make_element_rules(
#'   element = "page", 
#'   operator = "equals",
#'   rules = c("home", "cart")
#' )
#' container_1 <- make_segment_container(
#'   name = "ruleset1", 
#'   type = "hits", 
#'   operator = "or", 
#'   rules = ruleset_1
#' )
#' # making a nested container
#' # only makes sense if we have more than one container to start with, 
#' #  and therefore, more than one set of rules
#' ruleset_2 <- make_element_rules(
#'   element = "page", 
#'   operator = "not_equals", 
#'   rules = c("contact", "exit")
#' )
#' container_2 <- make_segment_container(
#'   name = "ruleset2",
#'   type = "hits", 
#'   operator = "or", 
#'   rules = ruleset_2
#' )
#' # now call the function again, but pass the two containers in as a list of rules
#' nested_container <- make_segment_container(
#'   name = "nested", 
#'   type = "visits", 
#'   operator = "and", 
#'   rules = list(container_1, container_2)
#' )
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