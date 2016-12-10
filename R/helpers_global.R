#Global helpers


.helper_fix_blank <- function(x) {
  x[x==""] <- NA
  x
}

#Auto-detect if passing in name or ID for Group methods
.helper_name_or_id <- function(x) {
  x_isID <- grepl("^\\d\\d+", x)
  if(x_isID) {
    return("group_id")
  } else {
    return("group_name")
  }
}


#error to DT restr helper FUN
#REFACTOR the tmp part of this-- either enforce input name of 'error' or
# try rmatch (below)
.helper_makeErrTbl <- function(x, idname) {
  if(length(idname) != 1 || !is.character(idname)) {
    stop("A length 1 character vector must be provided for output tbl id name")
  }
  
  err_tbl_nms <- c(idname, "msg")
  
  #Handle upstream or downstream transpose condition
  if(!"error" %in% names(x)) {
    tmp <- purrr::transpose(x)
    tmp <- tmp[["error"]]
  } else {
    tmp <- x
  }
  
  if("error" %in% names(tmp)) {
    tmp <- tmp[["error"]]
  }
  
  err.ret <- tryCatch(
    {
      Map(function(f) f[[c("error", "message")]], tmp)
    },
    error = function(e) {
      Map(function(f) f[["message"]], tmp)
    },
    error = function(e) {
      tmp
    },
    finally =
      NULL
  )
  
  err.ret <- Filter(function(f) !is.null(f), err.ret)
  
  err.dt  <- data.table(names(err.ret), unlist(err.ret))
  setnames(err.dt, err_tbl_nms)
  
}


#helper for err.ret handling in .helper_makeErrTbl refactoring
.errMsgChk <- function(x) {
  
  chkList <- list(
    errMsg = c("error", "message"), 
    errOnly = "error", 
    msgOnly = "message")
  
  xnames <- list_names(x)
  if(all(is.null(xnames)) || length(xnames) == 0) {
    return(x)
  }
  
  if(all(chkList$errMsg %in% xnames)) {
    Map(function(f) f[[chkList$errMsg]], x)
  } else if (chkList$errOnly %in% xnames) {
    Map(function(f) f[[chkList$errOnly]], x)
  } else if (chkList$msgOnly %in% xnames) {
    Map(function(f) f[[chkList$msgOnly]], x)
  } else
    x
}

#empty list error handler; if both empty list result and error, 
# prefer to keep the error and make empty list NULL

#If result empty list, if error NULL, then 
# error <- empty list message for result (.helper_emptyList_toMsg)
#If result empty list, then
# result <- NULL
.helper_emptyResultHandler <- function(x) {
  xname <- names(x)
  if(!all(xname %in% c("result", "error"))) {
    stop("input requires both 'result' and 'error' as named elements of x")
  }
  
  zeroChk <- .helper_is_emptyList(x[["result"]])
  nullChk <- is.null(x[["error"]])
  
  if(zeroChk) {
    if(nullChk) {
      x[["error"]] <- .helper_emptyList_toMsg(x[["result"]])
    }
    x["result"]  <- list(NULL)
  }
  x
}
