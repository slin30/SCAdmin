
# Try to parse clinical, as it is nested, proper

clin <- CALLS$clinical
alt <- splitted$split_wz_all$s300000520_582ca0d2e4b0a4d9dc2936ac



# first pull out definition
s1_defn <- clin[["definition"]]
# 
# container must be present
# container must be a data.frame
# container must have rules

# rules must be a list
# rules[[1]] must be a data.frame
#   which means you must Filter on this to kill NULLs

is_nested_valid <- function(x) {
  has_cont      <- names(x) == "container"
  is_df_cont    <- is.data.frame(x)
  
  if(!has_cont || !is_df_cont) {
    return(NULL)
  }
  
  has_rule <- "rules" %in% names(x[[1]])
  is_rule_list <- class(x[[1]][["rules"]]) == "list"
  
  if(!has_rule || !is_rule_list) {
    stop("x[[1]] must contain a list called 'rule'")
  }
  
  return(TRUE)
  
}


filt_rule_null <- function(x) {
  if(!is_nested_valid(x)) {
    stop("a valid structure was not detected")
  }
  
  # look for NA
  if(!anyNA(x)) {
    return(x)
  }
  
  diffnms <- setdiff(names(x[[1]]), "rules")
  na_pos <- vapply(x, function(f) which(is.na(f)), integer(length(diffnms)))
  
  out <- data.frame(container = NA)
  out[[1]] <- x[-na_pos, ]
  return(out)
}


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


flatten_df <- function(x) {
  x <- filt_rule_null(x)
  # get top level names, aside from rule
  nms_L1 <- setdiff(names(x[[1]]), "rules")
  
  out <- list()
  out <- c(out, x[[1]][c(nms_L1)])
  
  #get rules names, aside from container
  nms_L2 <- setdiff(names(x[[1]][["rules"]][[1]]), "container")

  res <- c(out, x[[1]][["rules"]][[1]][, c(nms_L2)])

  rem <- extract_one(x)
  list(res = res, rem = rem)
  
}


x <- flatten_df(s1_defn)
x1 <- flatten_df(x$rem)



recflat <- function(x, d = 0L, out = list()) {
  
  if(is.null(is_nested_valid(x))) {
    out <- append(out, x)
    return(out)
  } else {
    tmp <- flatten_df(x)
    #list2env(tmp, envir = parent.frame())
    recflat(x = tmp[["rem"]], d = d+1L, out = append(out, tmp["res"]))
  }
}

aaa <- recflat(s1_defn)




