#' Delete one or more segments by segment id, with checks
#' 
#' For a vector of segments, check, and possibly delete segment(s), with output report 
#' 
#' @importFrom RSiteCatalyst ApiRequest
#' @importFrom jsonlite unbox toJSON
#' 
#' @param id A character vector of segment IDs to delete
#' @param check_only If \code{TRUE} (default), will not execute the \code{DELETE} action
#' but instead returns a list of (\code{quoted}) expressions that must be evaluated to 
#' complete the deletion(s).
#'
#' @details 
#' This function calls the \emph{Segments.Delete} method, and attempts to make deletion
#' of one or more segments safer/more robust by adding two checks:
#' 
#' First, this function calls \emph{Segments.Get}, via \code{\link{Segments_Get}}, to 
#' provide a report and also to run a live check that the requested segments all exist, and if
#' not, throws an error. 
#' 
#' Second, this function does \strong{NOT}, by default, actually execute the delete action, 
#' but rather returns a \code{list} containing the aforementioned report, along with a list
#' of unevaluated expressions. See \code{Examples}.
#' 
#' The workflow can be streamlined by setting \emph{check_only} to \code{FALSE}, in which case
#' deletion is a single-step process (and a report is still output).
#' 
#' @return
#' If called with \code{check_only = TRUE} (default), a \code{list} of length 2, 
#' with named elements:
#' 
#' \enumerate{
#' \item \code{checks}: A data.frame containing segment information 
#'       (\code{id,name,description,reportSuiteID,owner,tags,modified}). The 
#'       number of rows corresponds to the length of \emph{id}.
#'  \item \code{eval_to_delete}: A list of unevaluated expressions, each element
#'        containing an unevaluated (\code{quoted}) expression, one for each
#'        \emph{id}. Evaluate element-wise with \code{eval}.
#' }
#' 
#' If called with \code{check_only = FALSE}, a \code{data.frame} containing the 
#' same information as listed above (for \code{checks}), with an additional column
#' named \code{DELETED}, where values of \code{TRUE} denote which \emph{id}s have
#' been successfully deleted.
#' 
#' @export
#' 
#' @section Access Privileges: 
#' This function calls an Adobe Analytics method that requires administrative/elevated privileges
#'
#' @examples
#' \dontrun{
#' # Default call and return:
#' yes_check <- Delete_Segment(valid_targs[1:2], check_only = TRUE)
#' # to execute the deletion:
#' lapply(yes_check$eval_to_delete, eval)
#' 
#' # If check_only = FALSE, single-step:
#' no_check  <- Delete_Segment(valid_targs[1:2], check_only = FALSE)
#' }
Segments_Delete <- function(id, check_only = TRUE) {
  
  if(!is.atomic(id) || !is.character(id)) {
    stop("'id' must be an atomic vector of type 'character'")
  }
  
  checks <- Segments_Get(selected = id, 
                         fields = c("reportSuiteID", "owner", "modified", 
                                    "tags", "description")
  )
  if(identical(list(), checks) || nrow(checks) != length(id)) {
    stop("One or more segments were not found")
  }
  
  # unbox ids
  queries <- lapply(id, function(f) unbox(f))
  
  # create list of formatted bodies
  to_delete <- vector("list", length(queries))
  for(i in seq_along(id)) {
    to_delete[[i]] <- toJSON(list(segmentID = queries[[i]]))
  }
  # create list of function calls, must 'eval' list elements to execute
  out_funs <- function(x = to_delete) {
    funlist <- vector("list", length(x))
    for(i in seq_along(x)) {
      funlist[[i]] <- bquote(ApiRequest(body = .(x[[i]]), fun = "Segments.Delete"))
    }
    return(funlist)
  }
  
  out_funlist <- out_funs(x = to_delete)
  
  if(check_only) { # do not execute
    return(
      list(checks = checks, eval_to_delete = out_funlist)
    )
  }
  
  # else, delete, and provide a report
  deleted <- vector("list", length(queries))
  for(i in seq_along(queries)) {
    message("deleting ", i, " of ", length(queries))
    deleted[[i]] <- eval(out_funlist[[i]])
  }
  
  deleted_df <- data.frame(id = id, DELETED = unlist(deleted))
  merge(checks, deleted_df, by = "id")
}