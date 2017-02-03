#' Expand a vector of rules into scalar list-sets
#' 
#' For the most common single element, single operator, many rules use case
#'
#' @importFrom jsonlite unbox toJSON
#' @importFrom magrittr "%>%"
#' 
#' @family make_segment functions
#' 
#' @param element A character vector. Must be a valid element (id) within access rights of your account
#' @param operator A character vector. For elements-based rules, the API defines allowable values. See details.
#' @param rules A character vector; if not, will be coerced via \code{as.character}
#' @param trim logi. Should leading and trailing whitespace(s) be removed? Defaults to \code{FALSE}
#' @param dedupe logi. Should input \emph{rules} be de-duplicated? Defaults to \code{FALSE}
#' @param how  Not used at the moment. 
#'
#' @return
#' A list of \code{length(rules)}. Will be hte length after whitespace-trimming and/or de-duplication
#' if \emph{trim} and/or \emph{dedupe} are \code{TRUE}, and if so, actions are performed by 
#' \code{str_trim} and \code{unique}, respectively. 
#' 
#' @note 
#' This now accepts inputs to  \emph{element} and \emph{operator} of length > 1. Checks are in place to 
#' prevent ambiguous combinatons of lengths for these two arguments in conjunction with \emph{rules}. This is
#' done to take advantage of standard recycling rules, while constraining the possible combinations of 
#' anticipated use cases. 
#' 
#' Anticipating all possible scenarios is impossible, so if you find any unexpected or overly-restrictive
#' behavior, please file an issue.
#' @export
#'
#' @examples
#' #TBD
make_element_rules <- function(element, operator, rules, trim = FALSE, dedupe = FALSE, how = NULL) {
  # check for input lengths of element, operator, rules
  
  checkList <- list(
    element = element, 
    operator = operator, 
    rules = rules
  )
  
  check_lens <- vapply(checkList, length, integer(1L))
  
  # meeting any one of these is OK, else stop
  all_equal    <- length(unique(check_lens)) == 1L
  one_not_one  <- length(check_lens[check_lens == 1L]) == 2L
  two_uniq_one <- "&"(length(unique(check_lens)) == 2L, 
                      length(check_lens[check_lens == 1L]) == 1L
  )
  
  if(!Reduce("|", c(all_equal, one_not_one, two_uniq_one))) {
    stop("Mismatch in argument length detected: \n\t", 
         paste(names(check_lens), check_lens, sep = ":", collapse = " | ")
    )
  }
  
  # how arg is placeholder for additional options in future e.g. metrics
  if(is.null(how)) {
    how <- "expand"
  }
  
  rules <- .l_helper_prep_rules(x = rules, trim = trim, dedupe = dedupe)
  
  if(how == "expand") {
    out <- Map(.l_helper_make_rule_elem, 
               element = element, 
               operator = operator, 
               value = rules
    )
  }
  names(out) <- NULL
  return(out)
}

NULL

.l_helper_prep_rules <- function(x, trim = FALSE, dedupe = FALSE) {
  if(!is.atomic(x)) {
    stop("x must be an atomic vector")
  }
  if(length(x) == 0L) {
    stop("x cannot be an empty vector")
  }
  if(!is.character(x)) {
    message("x supplied as class ", class(x), 
            " but character required. Automatically coercing"
    )
    x <- as.character(x)
  }
  
  x_origlen <- length(x)
  
  if(trim) {
    x <- stringr::str_trim(x)
  }
  if(dedupe) {
    x <- unique(x)
  }
  # Check and message if any duplicated
  x_newlen <- length(unique(x))
  if(x_origlen != x_newlen) {
    message("length of x is ", x_origlen, 
            " but unique count is ", x_newlen, "\n\t", 
            "If this is not intentional, rerun with 'dedupe = TRUE`"
    )
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