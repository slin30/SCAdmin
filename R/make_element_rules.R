#' Expand a vector of rules into scalar list-sets
#' 
#' For the most common single element, single operator, many rules use case
#'
#' @importFrom jsonlite unbox toJSON
#' @importFrom magrittr "%>%"
#' 
#' @family make_segment functions
#' 
#' @param element A character vector of length 1. Must be a valid element (id) within access rights of your account
#' @param operator A character vector of length 1. For elements-based rules, the API defines allowable values. See details.
#' @param rules A character vector; if not, will be coerced via \code{as.character}
#' @param how  Not used at the moment. 
#'
#' @return
#' A list of \code{length(rules)}, where length is calcualted after trimming leading and trailing whitespace and
#' de-duplicating \emph{rules} via \code{str_trim} and \code{unique}, respectively. 
#' @export
#'
#' @examples
#' #TBD
make_element_rules <- function(element, operator, rules, how = NULL) {
  # how arg is placeholder for additional options in future e.g. metrics
  if(is.null(how)) {
    how <- "expand"
  }
  
  rules <- .l_helper_prep_rules(x = rules)
  
  if(how == "expand") {
    out <- lapply(rules, function(f) 
      .l_helper_make_rule_elem(element = element, operator = operator, value = f)
    )
  }
  return(out)
}

NULL

.l_helper_prep_rules <- function(x, trim = TRUE, dedupe = TRUE) {
  if(!is.atomic(x)) {
    stop("x must be an atomic vector")
  }
  if(!is.character(x)) {
    x <- as.character(x)
  }
  if(trim) {
    x <- stringr::str_trim(x)
  }
  if(dedupe) {
    x <- unique(x)
  }
  return(x)
}

NULL

# rule operator ref lists, usable for both elements and metrics
.ref_rule_operators <- list(
  element = c(
    "equals", "matches", 
    "contains", "contains_all", 
    "contains_any", 
    "starts_with", "ends_with", 
    "exists"
  ), 
  metric = c("metric_exists")
) %>%
  Map(c, ., Map(paste0, "not_", .))

NULL

# helper to create and check elements-based segments base list
.l_helper_make_rule_elem <- function(element, operator, value) {
  
  baselst <- list(
    element = element, 
    operator = operator, 
    value  = value
  )
  
  # check that no args are of length > 1
  chk_position <- Position(function(x) length(x) > 1L, baselst)
  
  if(!is.na(chk_position)) {
    stop("All inputs must be vectors of length 1; found a violation in '", 
         names(baselst[chk_position]), "'")
  }
  
  # check operator values are valid for element
  ref_oper_elem <- .ref_rule_operators[["element"]]
  chk_val_oper <- operator %in% ref_oper_elem
  
  if(!chk_val_oper) {
    stop("'", operator, "' in arg:operator is not a valid value\n", 
         "The following are allowed values:\n", 
         "---[", paste(unique(sub("^not_", "", ref_oper_elem)), collapse =", "), "]---",
         "\n ...along with the negated forms, prefixed with 'not_'"
    )
  }
  
  Map(unbox, baselst)
}