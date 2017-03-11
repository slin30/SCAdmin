# FUNS to flatten nested segment definitions

# Notes:
# first pull out definition
# s1_defn <- clin[["definition"]]
# 
# container must be present
# container must be a data.frame
# container must have rules

# rules must be a list
# rules[[1]] must be a data.frame
#   which means you must Filter on this to kill NULLs

# may also need to handle exclude?


# FUNS --------------------------------------------------------------------


# recursive wrapper fun
flatten_nested_cont <- function(x, d = 0L, out = list()) {
  if("definition" %in% names(x)) {
    x <- x[["definition"]]
  }
  
  if(!(is_nested_valid(x))) {
    out <- append(out, x)
    return(out)
  } else {
    tmp <- flatten_df(x)
    flatten_nested_cont(x = tmp[["rem"]], d = d+1L, out = append(out, tmp["res"]))
  }
}

# main fun
flatten_df <- function(x) {
  x <- filt_rule_null(x)
  # get top level names, aside from rule
  nms_L1 <- setdiff(names(x[[1]]), "rules")
  
  #out_L1 <- list()
  out_L1 <- x[[1]][c(nms_L1)]
  names(out_L1) <- paste0("container.", nms_L1)
  
  #get rules names, aside from container
  nms_L2 <- setdiff(names(x[[1]][["rules"]][[1]]), "container")
  # if length zero, then you have container top-level, so can 
  # use this to switch logic downstream?
  out_L2 <- x[[1]][["rules"]][[1]][, c(nms_L2)]
  names(out_L2) <- paste0("rules.", nms_L2)
  
  res <- c(out_L1, out_L2)
  rem <- extract_one(x)
  
  list(res = res, rem = rem)
  
}

# helper extractor
extract_one <- function(x) {
  tryCatch(
    {
      x <- x[[1]]$rules[[1]]["container"]
    }, 
    error = function(e) {
      invisible(e)
      return(NULL)
    }, 
    finally = filt_rule_null(x)
  )
}

# helper null handler
filt_rule_null <- function(x) {
  if(!is_nested_valid(x)) {
    stop("a valid structure was not detected")
  }
  
  # look for NA
  if(!anyNA(x)) {
    return(x)
  } else {
    diffnms <- setdiff(names(x[[1]]), "rules")
    na_pos <- vapply(x, function(f) which(is.na(f)), integer(length(diffnms)))
    
    out <- data.frame(container = NA)
    out[[1]] <- x[-na_pos, ]
    return(out)
  }
}

# helper checker
is_nested_valid <- function(x) {
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


