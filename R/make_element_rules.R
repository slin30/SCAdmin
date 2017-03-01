#' Expand a vector of rules into scalar list-sets
#' 
#' For the common single element, single operator, many rules use case (and more)
#'
#' @importFrom jsonlite unbox toJSON
#' @importFrom magrittr "%>%"
#' 
#' @family make_segment functions
#' 
#' @param element (Required) A character vector. Must be a valid element (id) within access rights of your account
#' @param operator (Required) A character vector. For elements-based rules, the API defines allowable values. See details.
#' @param rules (Required) A character vector; if not, will be coerced via \code{as.character}
#' @param classification (Optional) A character vector of length 1. Element classification to segment, i.e. the name of the 
#' classification as returned from \code{\link[RSiteCatalyst]{GetClassifications}}
#' @param trim (Optional) logi. Should leading and trailing whitespace(s) be removed from \emph{rules}? Defaults to \code{FALSE}
#' @param dedupe (Optional) logi. Should input \emph{rules} be de-duplicated? Defaults to \code{FALSE}
#'
#' 
#' @details 
#' This function is only meant to be used for \emph{operator}-based rules, and NOT \emph{container}-based
#' rules. If you do not know what this means, please see:
#' \href{https://marketing.adobe.com/developer/documentation/segments-1-4/r-segment-rule#reference_62E104F8CA1C42819B9715CDDDD61E31}{Adobe documentation}
#'  
#' This function does NOT support the following arguments:
#' 
#' \itemize{
#' \item \preformatted{after}
#' \item \preformatted{within}
#' \item \preformatted{exclude}
#' }
#' 
#' 
#' @note 
#' Inputs to \emph{element} and \emph{operator} of length > 1 are allowed, with constraints. Checks are in place to 
#' prevent ambiguous combinatons of lengths for these two arguments in conjunction with \emph{rules}. This makes it 
#' possible to create certain combinations of element, operator, and rule, where only one of the three variables is
#' variant.
#' 
#' If, however, \emph{classification} is provided, \emph{element} must be a vector of length 1, i.e. 
#' a non-\code{NULL} value for \emph{classification} means that only one of \emph{operator} OR \emph{rules} can be
#' a vector of length > 1.
#' 
#' @return
#' A list of \code{length(rules)}, which may be passed to \code{\link{make_segment_container}}. 
#' 
#' The length of the output will equal the length after whitespace-trimming and/or de-duplication
#' if \emph{trim} and/or \emph{dedupe} are \code{TRUE}, and if so, actions are performed by 
#' \code{str_trim} and \code{unique}, respectively.
#' 
#' @export
#'
#' @examples
#' #TBD
make_element_rules <- function(element, operator, rules, 
                               classification = NULL,
                               trim = FALSE, dedupe = FALSE) {
  # if classification is present, must be length 1
  if(!is.null(classification)) {
    if(length(classification) > 1L || !is.character(classification)) {
      stop("Classification must be a character vector of length 1")
    } 
    if(length(element) > 1L || length(classification) > 1L) {
      stop("When classification is present, element must also be a vector of length 1")
    }
  }
  
  # denote the required and optional args
  req_nms <- c("element", "operator", "rules")
  opt_nms <- c("classification")
  
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
  
  # handle rules via helper
  rules <- .l_helper_prep_rules(x = rules, trim = trim, dedupe = dedupe)
  
  # handle optional (only classification for now) args, that must be 1L (each)
  opt_lst <- list(
    classification = classification
  )
  opt_lst <- Filter(function(x) !is.null(x), opt_lst)
  
  # output
  out <- Map(.l_helper_make_rule_elem,
             element = element, 
             operator = operator, 
             value = rules
  )
  
  if(length(opt_lst) > 0L) {
    opt_lst <- Map(unbox, opt_lst)
    out <- (lapply(out, function(f) c(f, opt_lst)))
  } else {
    out <- out
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

  if(trim) {
    x <- stringr::str_trim(x)
  }
  if(dedupe) {
    x <- unique(x)
  }
  # Check and message if any duplicated
  if(anyDuplicated(x)) {
    message("length of x is ", length(x), 
            " but unique count is ", length(unique(x)), "\n\t", 
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