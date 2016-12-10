#' call.Get_Group
#'
#' Base call to Permissions.GetGroup
#'
#' @param x a group name or group id of length 1
#' @param include_permissions denote whether to also include permissions data for each group
#' @param ... additional arguments to ApiRequest
#'
#' @return json
#'
#' @importFrom RSiteCatalyst ApiRequest
#' @importFrom jsonlite unbox toJSON
#'
#' @export

call.Get_Group <- function(x, include_permissions = NULL, ...) {
  if(any(duplicated(x))) {
    warning("Duplicated values found in rsid values; ", 
            "making values unique via unique()")
    
  }
  
  x <- unique(unlist(x))
  xlen <- length(x)
  call_list <- vector("list", xlen)
  
  for(i in seq_along(x)) {
    message("Processing query ", i, " of ", xlen)
    call_list[[i]] <- .Get_Group(x = x[[i]],
                                 include_permissions = include_permissions, 
                                 ...)
  }
  call_list
  names(call_list) <- x
  
  call_list
}


#Base internal function
.Get_Group <- purrr::safely(
  function(x, include_permissions = NULL, ...) {
    if(is.null(include_permissions)) {
      include_permissions <- FALSE
    }
    #Auto-detect if passing in id or name
    xkey <- .helper_name_or_id(x)
    
    body <- list(unbox(x), 
                 include_permissions = unbox(include_permissions))
    names(body)[[1]] <- xkey
    
    query <- toJSON(body)
    fun <- "Permissions.GetGroup"
    ApiRequest(body = query, func.name = fun, ...)
  }
)


#Auto-detect if passing in name or ID for Group methods
.helper_name_or_id <- function(x) {
  x_isID <- grepl("^\\d\\d+", x)
  if(x_isID) {
    return("group_id")
  } else {
    return("group_name")
  }
}
