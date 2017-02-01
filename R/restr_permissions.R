#' Restructure permissions from call.Get_Group
#' 
#' Function to handle permissions call return from call.Get_Group, permission = TRUE
#' 
#' @family get group functions
#'
#' @param x non-recursively unlisted result from call.Get_Group; see details
#' 
#' @details Normally used in the context of restr.Get_Group. Exported for flexibility. 
#'
#' @return a data.table or NULL
#' @export
#'
#' @examples
#' #TBD
restr_permissions <- function(x) {
  
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
  # internal check FUN
  outdt <- .l_helper_check_permissions(x_targ)
  outdt[, ":="(group_name = grp_name, group_id = grp_id)]
  
}
NULL
#expect dt of 4 with specific names, with data.table (i.e. nested list)
.l_helper_check_permissions <- function(x) {
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