#' Flatten a nested definition return
#' 
#' Flatten the definition return data structure for nested container(s) and rule(s)
#'
#' @param x The return from a call to Segments.Get with a named element of \emph{definition} 
#' @param d Internal counter to track recursion iterations
#' @param out Accumulator for results
#'
#' @return 
#' A list, currently taking one of two possible patterns, both of which apply to single
#' homogeneous cases:
#' 
#' Nested rules, i.e. a definiton where \emph{rules} is nested within \emph{containers}, 
#' will return a list of length 2, with named elements of \code{res,rules}. 
#' 
#' Nested containers, i.e. a definitions where \emph{container} is nested within \emph{rules}, 
#' will return a list of length \emph{n}, where \emph{n} is the number of nested containers. 
#' 
#' @note 
#' This is not yet complete. There are several helper functions that are not exported, and this
#' function may not be exported when final, and instead wrapped in a public-facing function with
#' more consistent return structures and more error checking.
#' 
#' @export
#'
#' @examples
#' # TBD
flatten_nested_defn <- function(x, d = 0L, out = list()) {
  if("definition" %in% names(x)) {
    x <- x[["definition"]]
  }
  
  if(!(.is_nested_valid(x)) || is.null(x)) {
    out <- append(out, x)
    return(out)
  } else {
    tmp <- .flatten_defn(x)
    flatten_nested_defn(x = tmp[["rem"]], d = d+1L, out = append(out, tmp["res"]))
  }
}


# main fun
.flatten_defn <- function(x) {
  x <- .filt_rule_null(x)
  # get top level names, aside from rule
  nms_L1 <- setdiff(names(x[[1]]), "rules")
  
  out_L1 <- x[[1]][c(nms_L1)]
  #names(out_L1) <- paste0("container.", nms_L1)
  
  # use helper to extract rest of res
  out_L2 <- .detect_and_parse(x)
  
  res <- c(out_L1, out_L2[["res"]])
  rem <- out_L2[["rem"]]
  
  list(res = res, rem = rem)
}


# helper to parse either container or rule nested
# logi are handled here for e.g. exclude
.detect_and_parse <- function(x) {
  
  check <- names(x[[1]][["rules"]][[1]])
  
  if(length(check) == 1L && check == "container") {
    # then nested rules
    extracted <- x[[1]][["rules"]][[1]][["container"]]
    nest_patt <- "rules"
  } else {
    # nested container
    extracted <- x[[1]][["rules"]][[1]]
    nest_patt <- "container"
  }
  
  nms <- setdiff(names(extracted), nest_patt)
  
  out <- extracted[, c(nms)]
  
  # impute defaults for NA
  out <- .fill_default_nm(out, nm = "exclude", FALSE)
  # out <- .fill_default_nm(out, nm = "operator", "and")
  
  
  # the new name should be the opposite pattern
  nm_prefix <- setdiff(c("rules", "container"), nest_patt)
  names(out) <- paste(nm_prefix, nms, sep = ".")
  #names(out) <- paste("rules", nms, sep = ".")
  
  
  # handle rem
  still_nested <- nest_patt %in% names(extracted)
  if(!still_nested) {
    rem <- NULL
  } else {
    rem <- extracted[nest_patt]
  }
  
  list(res = out, rem = rem)
}

# helper null handler
.filt_rule_null <- function(x) {
  if(!.is_nested_valid(x)) {
    stop("a valid structure was not detected")
  }
  
  # create check structure to fill defaults
  # to avoid false positive for anyNA
  x_check <- x[[1]]
  x_check <- .fill_default_nm(x_check, nm = "exclude", FALSE)
  x_check <- .fill_default_nm(x_check, nm = "operator", "and")
  
  # look for NA
  if(!anyNA(x_check)) {
    
    return(x)
  } else {
    diffnms <- setdiff(names(x[[1]]), "rules")
    na_pos <- vapply(x, function(f) which(is.na(f)), integer(length(diffnms)))
    
    out <- data.frame(container = NA)
    out[[1]] <- x[-na_pos, ]
    return(out)
  }
}


# helper to inpute defaults selectively
.fill_default_nm <- function(x, nm, default) {
  
  if(is.null(nm) || is.null(default)) {
    stop("a value for 'nm' and 'default' must be provided")
  }
  if(length(nm) > 1L || length(default) > 1L) {
    stop("value for 'nm' and 'default' must both be of length 1")
  }
  
  if(!nm %in% names(x)) {
    return(x)
  }
  
  x_new <- x[[nm]]
  x_new[is.na(x_new)] <- default
  x[[nm]] <- x_new
  
  return(x)
}

# helper checker for structure validity
.is_nested_valid <- function(x) {
  has_cont      <- names(x) == "container"
  is_df_cont    <- is.data.frame(x)
  
  if(!has_cont || !is_df_cont) {
    return(FALSE)
  }
  
  has_rule <- "rules" %in% names(x[[1]])
  is_rule_list <- class(x[[1]][["rules"]]) == "list"
  
  if(!has_rule || !is_rule_list) {
    stop("x[[1]] must contain a list called 'rule'")
  }
  
  return(TRUE)
  
}