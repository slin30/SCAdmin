#' Get Adobe Analytics Segments information
#' 
#' Query the AA segments API to return segment information based on different criteria, at 
#' the level of desired detail
#' 
#' @importFrom RSiteCatalyst ApiRequest
#' @importFrom jsonlite unbox toJSON
#' 
#' @family get segments functions
#'
#' @param accessLevel (optional) A character vector of length 1. Must be one of \code{all, shared, owned}. 
#' If not specified, defaults to \code{owned}. 
#' @param fields (optional) A character vector denoting the quantity, depth, and general detail of information desired. 
#' Must be one of 
#' \code{tags, shares, description, owner, modified, compatibility, favorite, reportSuiteID, definition}. 
#' The API always includes \code{id} and \code{name}, by default.
#' @param selected (optional) A character vector of segment ID(s) you wish to query for. If both \code{selected} and 
#' \code{accessLevel} are provided, \code{selected} take precedence.
#' @param sort (optional) A character vector of length 1. Must be one of \code{id, name, description, reportSuiteID,
#' owner, modified, favorite}. If not specified, defaults to \code{id}.
#' @param filters (optional) A named \code{list}. Valid names include
#' \code{approved, favorite, name, owner, reportSuiteID, tags}. For \code{tags}, character vectors of length > 1 are supported, 
#' and will be collapsed into comma-separated vectors of length 1 per API requirements. For the other fields, the API appears to 
#' only support character vectors of length 1. 
#' @param ... Additional args to pass to \code{ApiRequest}
#'
#' @return
#' A data.frame, possibly with nested columns depending on requested parameters within \emph{fields}. Notably, 
#' the following values in \emph{fields} return list-columns of varying complexity:
#' 
#' \itemize{
#' \item{tags}
#' \item{shares}
#' \item{compatibility}
#' \item{definition}
#' }
#' 
#' The number of rows corresponds to the number of unique segments, identified by the \code{id} field. With default
#' settings, a successful return will contain two fields, \code{id} and \code{name}. 
#' 
#' @note 
#' It is expected that once the full method is completed, this function will no longer be required by itself, 
#' although likely will still be exported for flexibility and debugging. 
#' 
#' The documentation is somewhat unclear as to whether values for \emph{filters} with names of \code{name, owner, reportSuiteID} 
#' actually support vectors of length > 1. Initial testing suggests not; the function will accept character vectors of length >1 for
#' such named fields at the moment, and will be updated accordingly when further testing to complete, i.e. will enforce input lengths
#' in a manner consistent with API expectations. 
#' 
#' @export
#'
#' @examples
#' # These should be thrown into tests; being lazy for now
#' \dontrun{
#' # Get your segments, with id and name; no restructuring required
#' my_own_simple <- call.Get_Segments()
#' 
#' # filters must be a list
#' call.Get_Segments(filters = c("A", "a"))
#' # filters must be a named list
#' call.Get_Segments(filters = list("A", "B"))
#' # filters must be a named list, and all names must be valid
#' call.Get_Segments(filters = list(name = "A", alt="B"))
#'  
#' # accessLevel must be vector of length 1
#' call.Get_Segments(accessLevel = c("all", "owned"))
#' # accessLevel cannot contain invalid values
#' call.Get_Segments(accessLevel = "A")
#' # although if you pass in a list, helper will coerce to vector
#' ## this works:
#' call.Get_Segments(accessLevel = list("owned")) 
#' 
#' # sort must be a vector of length 1
#' call.Get_Segments(sort = 1:2)
#' # sort cannot contain invalid values
#' call.Get_Segments(sort = 1)
#' # although if you pass in a list, helper will coerce to vector
#' ## this works:
#' call.Get_Segments(sort = list("name")) 
#' 
#' # fields must contain valid values
#' ## error; always get `id`, but not allowed value:
#' call.Get_Segments(fields = c("id"))
#' # fields must contain ALL valid values
#' ## 'reportSuiteID' is valid, 'id' is not
#' call.Get_Segments(fields = c("id", "reportSuiteID")) 
#' ## this works:
#' call.Get_Segments(fields = c("owner", "reportSuiteID"))
#' # if you pass in a list, helper will coerce to vector
#' call.Get_Segments(fields = list("owner", "reportSuiteID"))
#' 
#' # Parsing is needed for certain fields, in particular 'definition'
#' # This returns some nested fields
#' needs_parsing_1 <- call.Get_Segments(fields = c("tags", "shares", "compatibility"))
#' # `definition` is the most complex
#' needs_parsing_2 <- call.Get_Segments(fields = c("definition"))
#' # Here's what it looks like if we ask for all fields
#' needs_parsing_3 <- call.Get_Segments(fields = c("compatibility", "definition", 
#'                                                 "favorite", "modified", 
#'                                                 "owner", "reportSuiteID", 
#'                                                 "shares", "tags")
#'                                      )
#' }
call.Get_Segments <- function(accessLevel = NULL, fields = NULL, 
                              selected = NULL, sort = NULL, 
                              filters = NULL, ...) {
  
  # accessLevel, must be vetor of length 1
  validAccessLevel <- c("all", "shared", "owned")
  if(!is.null(accessLevel)) {
    # check input length, must be length 1
    if(length(accessLevel) > 1L) {
      stop("'accessLevel' must be a vector of length 1")
    }
    accessLevel <- .l_helper_inputCheck(nm = "accessLevel", 
                                        input = accessLevel, 
                                        ref = validAccessLevel, 
                                        unbox = TRUE
    )
  } else {
    accessLevel <- unbox("owned") # default
  }
  
  # fields, will always include id and name
  validFields <- c("tags", "shares",
                   "description", "owner",
                   "modified", "compatibility",
                   "favorite", "reportSuiteID",
                   "definition"
  )
  if(!is.null(fields)) {
    fields <- .l_helper_inputCheck(nm = "fields", 
                                   input = fields, 
                                   ref = validFields, 
                                   unbox = FALSE)
  }
  
  # selected, will override accessLevel if present
  if(!is.null(selected)) {
    selected <- c(selected, recursive=TRUE)
  } 
  
  # sort, must be vector of length 1
  validSort <- c("id", "name", "description", "reportSuiteID",
                 "owner", "modified", "favorite"
  )
  if(!is.null(sort)) {
    # check input length, must be length 1
    if(length(sort) > 1L) {
      stop("'sort' must be a vector of length 1")
    }
    sort <- .l_helper_inputCheck(nm = "sort", 
                                 input = sort, 
                                 ref = validSort, 
                                 unbox = TRUE)
  } else {
    sort <- unbox("id")
  }
  
  # filters; has own helper, as must provide a named list to arg
  if(!is.null(filters)) {
    filters <- .l_helper_process_filters(filters)
    filters <- lapply(filters, function(f) unbox(f))
  }
  
  body <- list(accessLevel = accessLevel, 
               fields = fields, 
               selected = selected, 
               sort = sort, 
               filters = filters
  )
  
  body <- Filter(function(x) !is.null(x), body)
  
  query <- toJSON(body)
  fun <- "Segments.Get"
  out <- ApiRequest(body = query, func.name = fun, ...)
  
  return(out)
}

NULL

# helper to validate and preprocess filters arg
# TODO: Carefully check that the API indeed will NOT support character vectors of length > 1L
# for owner, rsid, and name. If indeed the case, put appropriate input checks and handle 
# the new restrictions in this function. 
.l_helper_process_filters <- function(arglst) {
  if(!is.list(arglst)) {
    stop("Class of 'filters' must be a list, but is currently ", 
         class(arglst), call. = FALSE)
  }
  argNms <- names(arglst)
  if(any(argNms %in% c("")) | all(is.null(argNms))) {
    stop("One or more names missing in 'filters'; all elements of 'filters' must be named", 
         call. = FALSE)
  }
  validNms <- c("approved", "favorite",
                "owner", "name", 
                "reportSuiteID", "tags"
  )
  if(! all(argNms %in% validNms)) {
    .l_helper_inputCheck(nm = "filters", 
                         input = argNms, 
                         ref = validNms, 
                         chk_names_instead = TRUE)
  }
  
  # process args for certain names by data type req
  logi_nms <- c("approved", "favorite")
  chr_nms  <- setdiff(validNms, logi_nms)
  
  logi_elems <- Filter(function(x) !is.null(x), arglst[logi_nms])
  chr_elems  <- Filter(function(x) !is.null(x), arglst[chr_nms])
  
  if(length(logi_elems) > 0L) {
    logi_out <- lapply(logi_elems, function(f) as.logical(f))
  } else {
    logi_out <- NULL
  }
  if(length(chr_elems) > 0L) {
    chr_out <- lapply(chr_elems, function(f) paste(f, collapse = ",", sep = ","))
  } else {
    chr_out < NULL
  }
  
  c(chr_out, logi_out)
}

NULL

# helper to validate, optionally preprocess, and return values or error msg
.l_helper_inputCheck <- function(nm, input, ref, 
                                 collapse_lst = TRUE, dedupe = TRUE, unbox = FALSE, 
                                 msgOnly = FALSE, 
                                 chk_names_instead = FALSE) {
  # allow input to be vector of mode atomic and 'list'
  if(collapse_lst & is.list(input)) {
    input <- c(input, recursive = TRUE)
  }
  # pretty sure default of TRUE is the right call here
  if(dedupe) {
    input <- unique(input)
  }
  # Handle messaging for errors, or valid returns, assuming msgOnly is FALSE (default)
  # chk_names_instead to ensure sensible message in case of filters, where checking is at
  #  element names level
  delta <- setdiff(input, ref)
  if(chk_names_instead) {
    msg_base = "name(s)"
  } else {
    msg_base = "values(s)"
  }
  # if errors, stop with message, else depends on unbox and msgOnly vals
  if(length(delta) > 0L) {
    msg <- paste(
      "Invalid input ", msg_base, " detected in '", nm, "':",
      "\n\t", paste(delta, collapse = ", "),
      "\nValid ", msg_base, " are: ", paste(sort(ref), collapse = ", "), 
      sep = ""
    )
    stop(message = msg, call. = FALSE)
  } else {
    if(msgOnly) {
      return(invisible(NULL))
    }
    if(unbox) {
      return(unbox(input))
    } else {
      return(input)
    }
  }
}