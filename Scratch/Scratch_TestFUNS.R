Get_Segments <- function(accessLevel = NULL, fields = NULL, 
                         selected = NULL, sort = NULL, 
                         filters = NULL, handle_tagsCol = FALSE,
                         ...) {
  
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
  
  # handle tags here, since it is a simple list colum
  # if("tags" %in% names(out) & handle_tagsCol) {
  #   out <- tidyr::unnest_(out, "tags")
  # }
  return(out)
}

NULL

# helper to validate and preprocess filters arg
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


# restructure definition return from Segments.Get with definition pull
restr_segRules <- function(x) {
  
  x_filt <- lapply(x, class)
  
  # capture vector, pass on data.frame
  x_vec       <- x[x_filt != "data.frame"]
  x_container <- as.list(x[[c("definition", "container")]])
  
  
  x_rules <- x_container[[c("rules")]] %>%
    Map(as.data.table, .)
  
  new_rulenames <- lapply(x_rules, function(f) paste0("rule_", names(f)))
  Map(setnames, x_rules, new_rulenames)
  
  cont_meta <- x_container[!names(x_container) %in% "rules"]
  
  out <- lapply(x_rules, function(f) f[, c(names(cont_meta)) := cont_meta])
  out <- lapply(x_rules, function(f) f[, c(names(x_vec)) := x_vec])
  
  rbindlist(out)
  
}
