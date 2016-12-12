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
#' @details 
#' This function is not normally needed by itself, but is exported to support future feature requests, 
#' troubleshooting, and bug fixes. This also serves as the primary documentation resource for
#' the restructuring function for \code{\link{call.Get_Group}}.
#' 
#' Restructuring functions in this package all perform the same basic tasks:
#' 
#' \enumerate{
#'   \item Output user-friendly data structures
#'       \itemize{
#'       \item Generic where possible
#'       \item Stakeholder-specific where necessary
#'       }
#'   \item Handle empty results as errors, with custom messaging
#' }
#' 
#' @note 
#' Like all restructuring functions, this only restructures outputs from the upstream \code{call.} 
#' function, and therefore does not require access (or authentication) to the API. Practically, this
#' is only helpful if for some reason a set of call return values is written out for later restructuring.
#'
#' @return 
#' A \code{list} of length 2 or 4:  
#' 
#' If ALL values in \code{call_ret} throw errors, prints a warning message and returns a
#' \code{list} of length 2 with the following elements:
#' \itemize{
#' \item \code{result}: A custom message, \emph{No valid results; see errors}
#' \item \code{errors}: A two-column \code{data.table}, \code{nrow == length(call_ret)}:
#'     \itemize{
#'     \item \code{group_name_or_id}: Values that resulted in errors (\code{character})
#'     \item \code{msg}: Error message, possibly custom depending on error type (\code{character})
#'     }
#' }
#' If ANY value in \code{call_ret} succeeds AND returns a non-empty \code{user_list}, 
#' a \code{list} of length 4 with the following elements:
#' \itemize{
#' \item \code{user_table}: A three-column \code{data.table}:
#'     \itemize{
#'     \item \code{group_name}: The name of the group. If a \emph{group_name} was passed to 
#'           \code{call.Get_Group}, these values will match (\code{character})
#'     \item \code{group_id}: The ID of the group. If a \emph{group_id} was passed to 
#'           \code{call.Get_Group}, these values will match (\code{integer})
#'     \item \code{user_list}: The user corresponding to the \code{group_name} and \code{group_id}. 
#'           For groups with >1 user, there will be one row per unique user per group (\code{character})
#'     }
#' \item \code{group_table}: A five-column \code{data.table}:
#'     \itemize{
#'     \item \code{group_name}: As described in \code{user_table} above (\code{character})
#'     \item \code{group_id}: As described in \code{user_table} above (\code{integer})
#'     \item \code{group_description}: The description of the group, if available, else \code{NA} (\code{character})
#'     \item \code{all_report_suite_access}: (\code{logical})
#'     \item \code{n_users}: A custom field denoting number of unique items in \code{user_list} from \code{user_table}, 
#'           by group (name or ID). The sum of this column should equal the number of rows in \code{user_table} (\code{integer})
#'     }
#' \item \code{permission}: If NOT requested, a message \emph{No permissions requested, none returned}; else, an 
#'       eight-column \code{data.table}:
#'     \itemize{
#'     \item \code{id}: 
#'     }
#' }
#'
#' 
#' 
#' @export
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
    usr_dt <- "No valid results"
    group_dt <- "No valid results"
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