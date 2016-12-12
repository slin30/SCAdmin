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
#' \code{list} of length 2 (Scenario A) or 4 (Scenario B):  
#' 
#' A) If all values in \code{call_ret} throw errors, prints a warning message and returns a
#' \code{list} of length 2 with the following elements:
#' \itemize{
#' \item \code{result}: A custom message, \emph{No valid results; see errors}
#' \item \code{errors}: A two-column \code{data.table}, \code{nrow == length(call_ret)}:
#'     \itemize{
#'     \item \code{group_name_or_id}: Values that resulted in errors (\code{character})
#'     \item \code{msg}: Error message, possibly custom depending on error type (\code{character})
#'     }
#' }
#' B) If any value in \code{call_ret} succeeds and returns a non-empty \code{user_list}, 
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
#'           by group (name or ID). The sum of this column should equal the number of rows in \code{user_table} 
#'           UNLESS you query for a \emph{group_name} and its corresponding \emph{group_id}. In such cases, the
#'           \code{call.Get_Group} function treats each value as individual, and will return identical information 
#'           for the two values, since they describe the same core entity (\code{integer})
#'     }
#' \item \code{permission}: If not requested, a message \emph{No permissions requested, none returned}; else, an 
#'       eight-column \code{data.table}:
#'     \itemize{
#'     \item \code{id}: ID for the permission \emph{name} (\code{integer})
#'     \item \code{name}: Name of the permission. Within the entire table, there can be identical \emph{name}s, but 
#'           each \emph{name} is unique in the context of its respective \code{parent_name} (\code{character})
#'     \item \code{access} (\code{logical})
#'     \item \code{parent_name}: Custom-named field; derives from the \code{category_permissions data.frame}, which
#'           has a default name of \emph{name}. Restructured within \code{\link{restr_permissions}} to maintain 
#'           hierarchical relationship between top-level and nested \code{data.frame} outputs from the return value, and
#'           renamed to \code{parent_name} to avoid name conflict with \emph{name} (above) (\code{character})
#'     \item \code{parent_category}: Custom-named field; see \code{parent_name} documentation (above). Originally called
#'           \emph{category} (\code{character})
#'     \item \code{parent_access}: Custom-named field; see \code{parent_name} documentation (above). Originally called 
#'            \emph{access} (\code{character})
#'     \item \code{group_name}: Custom field added to denote group scoping, here via \emph{group_name} (\code{character})
#'     \item \code{group_id}: Custom field added to denote group scoping, here via \emph{group_id} (\code{integer})
#'     }
#' \item \code{errors}: If no errors, a message \emph{No errors triggered by any group_name or group_id values}; else, a
#'       two-column \code{data.table}; see \code{errors} in \emph{Scenario A} (above) for details. 
#' }
#'
#' 
#' @export
#' @examples
#' \dontrun{
#' # Scenario A
#' call_err <- call.Get_Group(c("a", "b"))
#' data_err <- restr.Get_Group(call_err)
#' 
#' # Scenario B, with permissions
#' # Note that these two values describe the same thing, which is an unusual scenario, but intentionally
#' # shown here. This is intentionally not handled, although it's easy to address downstream via unique().
#' call_no_err.P <- call.Get_Group(c(136031L, "RAP-Mendeley Admin Access"), include_permissions = TRUE)
#' data_no_err.P <- restr.Get_Group(call_no_err.P) #call unique() on result to de-duplicate, if you wish
#' }
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