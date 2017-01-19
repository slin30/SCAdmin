#' Get Adobe Analytics Segments information
#' 
#' Query the AA segments API to return segment information based on different criteria, at 
#' the level of desired detail
#' 
#' @importFrom RSiteCatalyst ApiRequest
#' @importFrom jsonlite unbox toJSON
#'
#' @param accessLevel A character vector of length 1. Must be one of \code{all}, \code{shared}, or 
#' \code{owned}. If not specified, defaults to \code{owned}. 
#' @param fields A character vector denoting the quantity, depth, and general detail of information desired. 
#' See details for permissible values. The API always includes \code{id} and \code{name}, by default.
#' @param selected A character vector of segment ID(s) you wish to query for. If both \code{selected} and 
#' \code{accessLevel} are provided, \code{selected} take precedence.
#' @param sort A character vector 
#' @param filters A character vector
#' @param handle_tagsCol A logical vector of length 1
#' @param ... Additional args to pass to \code{ApiRequest}
#'
#' @return
#' A data.frame if successful. 
#' @export
#'
#' @examples
#' # Forthcoming
Get_Segments <- function(accessLevel = NULL, fields = NULL, 
                               selected = NULL, sort = NULL, 
                               filters = NULL, handle_tagsCol = FALSE,
                               ...) {
  
  # accessLevel, must be vetor of length 1
  # Note that accessLevel is overriden if 'selected' has value
  validAccessLevel <- c("all", "shared", "owned")
  if(!is.null(accessLevel)) {
    # check input length, must be length 1
    if(length(accessLevel) > 1L) {
      stop("accessLevel must be a vector of length 1")
    }
    # check input value validity
    accessLevel <- .l_helper_inputCheck(nm = "accessLevel", 
                                        input = accessLevel, 
                                        ref = validAccessLevel, 
                                        unbox = TRUE
    )
  } else {
    accessLevel <- jsonlite::unbox("owned") # default
  }
  
  # fields, will always include id and name
  # valid fields
  validFields <- c("tags", "shares",
                   "description", "owner",
                   "modified", "compatibility",
                   "favorite", "reportSuiteID",
                   "definition"
  )
  if(!is.null(fields)) {
    # make sure all fields are valid
    # tmp_fields <- unique(
    #   c(fields, recursive=TRUE, use.names=FALSE)
    # )
    # diff_fieldCheck <- setdiff(tmp_fields, validFields)
    # if(length(diff_fieldCheck) > 0L) {
    #   stop("The following illegal values were found in fields:\n\t", 
    #        diff_fieldCheck)
    # } else {
    #   fields <- tmp_fields
    # }
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
    sort <- .l_helper_inputCheck(nm = "sort", 
                                 input = sort, 
                                 ref = validSort, 
                                 unbox = TRUE)
  } else {
    sort <- unbox("id")
  }
  
  # filters; this is the most complicated
  # must provide a named list to arg if using
  if(!is.null(filters)) {
    filters <- .process_filters(filters)
    filters <- lapply(filters, function(f) jsonlite::unbox(f))
  }
  
  body <- list(accessLevel = accessLevel, 
               fields = fields, 
               selected = selected, 
               sort = sort, 
               filters = filters
  )
  
  body <- Filter(function(x) !is.null(x), body)
  
  #query <- jsonlite::toJSON(body)
  #fun <- "Segments.Get"
  #out <- ApiRequest(body = query, func.name = fun, ...)
  
  # handle tags here, since it is a simple list colum
  # if("tags" %in% names(out) & handle_tagsCol) {
  #   out <- tidyr::unnest_(out, "tags")
  # }
  # return(out)
}

NULL
# helper to validate, optionally preprocess, and return values or error msg
.l_helper_inputCheck <- function(nm, input, ref, 
                                 collapse_lst = TRUE, dedupe = TRUE, unbox = FALSE, 
                                 single_value = FALSE
) {
  if(collapse_lst & is.list(input)) {
    input <- c(input, recursive = TRUE)
  }
  
  if(dedupe) {
    input <- unique(input)
  }
  
  delta <- setdiff(input, ref)
  if(length(delta) > 0L) {
    if(length(delta) == 1L)
      msg <- paste(
      "The following illegal value was found in ", nm,  
      "\n\t", delta, 
      sep = " "
    )
    if(length(delta) > 1L)
      msg <- paste(
        "The following illegal values were found in ", nm, 
        "\n\t", delta,
        sep = " "
      )
    stop(msg, call. = FALSE)
  } else {
    if(unbox) {
      return(unbox(input))
    } else {
      return(input)
    }
  }
}

NULL
# helper to validate and preprocess filters arg
.l_helper_process_filters <- function(arglst) {
  if(!is.list(arglst)) {
    stop("argLst must be a list")
  }
  argNms <- names(arglst)
  validNms <- c("approved", "favorite",
                "owner", "name", 
                "reportSuiteID", "tags"
  )
  if(! all(argNms %in% validNms)) {
    diff <- setdiff(argNms, validNms)
    stop("The following invalid names were found in arglst:\n\t", 
         diff)
  }
  
  # process args for certain names
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
  
  # append and output
  c(chr_out, logi_out)
}
