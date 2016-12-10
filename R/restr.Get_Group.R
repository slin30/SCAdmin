#Restructuring FUN; pass in call.Get_Group output
#' 
#' Restructure json return from call.Get_Group
#' @import data.table
#' @importFrom magrittr "%>%"
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
  call_ret <- lapply(call_ret, .helper_emptyResultHandler)
  
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
      errors = .helper_makeErrTbl(x = call_ret, idname = idname)
      
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
    err.tmp <- lapply(to_drop[["error"]], .helper_GroupError)
    err <- .helper_makeErrTbl(x = err.tmp, idname = idname)
    
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
      permDTList   <- Map(.restr_permissions, to_keep_perm)
      permissions  <- rbindlist(permDTList, use.names = FALSE)
    } else {
      permissions <- "No permissions requested, none returned"
    }
    
    full_dt <- purrr::transpose(extracted) %>%
      Map(as.data.table, .) %>%
      lapply(X = ., function(f) setnames(f, to_extract)) %>%
      rbindlist(use.names = FALSE) %>%
      .[, lapply(.SD, .helper_fix_blank)]
    
    usr_dt   <- full_dt[, .(group_name, group_id, user_list)]
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



#Handle error to DT output for restr
.helper_GroupError <- function(x) {
  xname <- names(x)
  if(is.null(x[["message"]])) {
    x <- "group_name or group_id did not return any users"
  } else
    x <- x[["message"]]
  names(x) <- "error" #needed for .helper_makeErrTbl
  x
}

#Restructure permissions if present
#expect dt of 4 with specific names, with data.table (i.e. nested list)
.helper_restr_permissions <- function(x) {
  if(!(is.data.table(x) | is.data.frame(x))) {
    stop("Required input structure is a data.table or data.frame", 
         " with additional dependencies\n", 
         "  Input currently does not", 
         " satisfy the very first dependency")
  }
  if(!all(names(x) %in% c("category", "name", "access", "permissions"))) {
    stop("Input expects names of: 'category', 'name', 'access', 'permissions'")
  }
  if(!is.list(x[["permissions"]])) {
    stop("Column 'permissions' must be of type list")
  }
  if(!is.data.table(x)) {
    x <- as.data.table(x)
  }
  targ_parent_names <- c("name", "category", "access")
  merged_name       <- "context"
  parent_dt <- x[, c(targ_parent_names), with = FALSE]
  parent_dt[, c(merged_name) := apply(.SD, 1, function(f) paste(f, collapse = "|"))]
  
  permissions_dtList <- x[["permissions"]]
  names(permissions_dtList) <- parent_dt[[merged_name]]
  
  permissions_dt <- rbindlist(permissions_dtList, use.names = TRUE, idcol = merged_name)
  permissions_dt[, c(paste0("parent_", targ_parent_names)) := 
                   tstrsplit(x = get(merged_name), split = "\\|", fixed = FALSE, perl = TRUE)]
  permissions_dt[, c(merged_name) := NULL]
  
}

#actually perform restructuring
#Expect untransposed to_keep
.restr_permissions <- function(x) {
  
  #handle input where 'result' is not isolated
  input_names <- names(x)
  
  if("result" %in% input_names) {
    x <- x[["result"]]
  }
  
  #handle nesting even after extracting result
  if(length(x) == 1) {
    x <- x[[1]]
  }
  
  targ <- "category_permissions"
  
  if(!targ %in% names(x)) {
    return(NULL)
  }
  
  #pull in required group identifiers
  grp_name <- x[["group_name"]]
  grp_id   <- x[["group_id"]]
  
  #index category_permissions DT out, unnest one level
  x_targ <- x[[targ]] #do here, else need to handle scope for
  # helper FUN
  outdt <- .helper_restr_permissions(x_targ)
  outdt[, ":="(group_name = grp_name, group_id = grp_id)]
  
}