# Global helpers
# These are, or can be used across >1 function


#' @importFrom magrittr "%>%"


.g_helper_fix_blank <- function(x) {
  x[x==""] <- NA
  x
}


#error to DT restr helper FUN
.g_helper_makeErrTbl <- function(x, idname) {
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


#empty list base helper
.g_helper_is_emptyList <- function(x) {
  ref_str <- structure(list())
  identical(x, ref_str)
}
#empty list message generator
.g_helper_emptyList_toMsg <- function(x) {
  chk <- .g_helper_is_emptyList(x)
  if(chk) {
    x <- "Call returned no value"
  } else {
    x
  }
}

#empty list error handler; if both empty list result and error, 
# prefer to keep the error and make empty list NULL
#If result empty list, if error NULL, then 
# error <- empty list message for result (.helper_emptyList_toMsg)
#If result empty list, then
# result <- NULL
.g_helper_emptyResultHandler <- function(x) {
  xname <- names(x)
  if(!all(xname %in% c("result", "error"))) {
    stop("input requires both 'result' and 'error' as named elements of x")
  }
  
  zeroChk <- .g_helper_is_emptyList(x[["result"]])
  nullChk <- is.null(x[["error"]])
  
  if(zeroChk) {
    if(nullChk) {
      x[["error"]] <- .g_helper_emptyList_toMsg(x[["result"]])
    }
    x["result"]  <- list(NULL)
  }
  x
}
