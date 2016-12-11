#' Create user-friendly output from call.Get_Group return
#' 
#' Handle all return possibilities from call.Get_Group, with or without permissions
#' 
#' @import data.table
#' @importFrom magrittr "%>%"
#' 
#' @family get group functions
#' 
#' @param call_ret result from call.Get_Group
#'
#' @return list of data.tables
#' @export
#'
#' @examples
#' #TBD
restr.Get_Group <- function(call_ret) {
  
  idname <- "group_name_or_id" #set for error DT id col name
  
  #Empty list return to message
  call_ret <- lapply(call_ret, .g_helper_emptyResultHandler)
  
  #check for all null results
  res_check <- Map("[[", call_ret, "result") %>%
    unlist %>%
    is.null %>%
    all
  #handle all null results
  if(res_check) {
    warning("There were no valid results, returning error message(s)")
    return(list(
      result = "No valid results; see errors", 
      errors = .g_helper_makeErrTbl(x = call_ret, idname = idname)
      
    )
    )
  }
  
  #Drop empty user_list in result, handle dropped records in error, transpose
  to_keep  <- purrr::keep(call_ret, function(f) length(f[["result"]][["user_list"]])>0) %>%
    purrr::transpose(.)
  to_drop  <- purrr::discard(call_ret, function(f) length(f[["result"]][["user_list"]])>0) %>%
    purrr::transpose(.)
  
  #errors
  if(length(to_drop) == 0) { 
    err <- "No errors triggered by any group_name or group_id values"
  } else {
    err.tmp <- lapply(to_drop[["error"]], .l_helper_GroupError)
    err <- .g_helper_makeErrTbl(x = err.tmp, idname = idname)
    
  }
  
  #results
  if(length(to_keep[["result"]]) > 0L) {
    res_names <- to_keep[["result"]] %>% Map(names, .) %>% unique #all names
    
    to_extract <- c("group_name", "group_id", "group_description", 
                    "all_report_suite_access", "user_list")
    
    extracted <- lapply(to_extract, function(f) purrr::at_depth(to_keep[["result"]], 1, f))
    
    if("category_permissions" %in% unlist(res_names)) {
      #yes, transposing back-- not terribly elegant...
      to_keep_perm <- purrr::transpose(to_keep)
      permDTList   <- Map(restr_permissions, to_keep_perm)
      permissions  <- rbindlist(permDTList, use.names = FALSE)
    } else {
      permissions <- "No permissions requested, none returned"
    }
    
    full_dt <- purrr::transpose(extracted) %>%
      Map(as.data.table, .) %>%
      lapply(X = ., function(f) setnames(f, to_extract)) %>%
      rbindlist(use.names = FALSE) %>%
      .[, lapply(.SD, .g_helper_fix_blank)]
    
    usr_dt   <- full_dt[, c("group_name", "group_id", "user_list")]
    group_dt <- dcast.data.table(full_dt, ... ~ ., value.var = "user_list", 
                                 fun.aggregate = uniqueN) %>%
      setnames(".", "n_users")
  } else {
    user_table <- "No valid results"
    group_table <- "No valid results"
    permissions <- NULL
  }
  
  list(
    user_table = usr_dt, 
    group_table = group_dt, 
    permission = permissions,
    errors = err
  )
  
}

NULL

#' @keywords internal
.l_helper_GroupError <- function(x) {
  # xname <- names(x)
  if(is.null(x[["message"]])) {
    x <- "group_name or group_id did not return any users"
  } else
    x <- x[["message"]]
  names(x) <- "error" #needed for .g_helper_makeErrTbl
  x
}