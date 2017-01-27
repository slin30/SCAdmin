#' restr.Get_Segments
#'
#' Restructure the return from a call to Segments.Get, via Get_Segments. Currently does NOT work
#' properly for nested containers or stacked segments! Much more work required, but this works 
#' for the most basic cases. 
#' 
#' @importFrom RSiteCatalyst ApiRequest
#' @importFrom jsonlite unbox toJSON
#' @import data.table
#' @importFrom magrittr "%>%"
#' 
#' @param x A call return from \code{Get_Segments}
#' @param merge_rules logi. Should the function return a tidy \code{data.table}? Defaults to \code{TRUE}
#' @param ... Additional named args to pass to parse functions. Should not normally be needed.
#' @return
#' Depending on the values passed to \emph{collapse_rules} and \emph{bind_rules}, a list of outputs of
#' varying lengths. With default settings, \code{list} of two \code{data.tables}:
#' \enumerate{
#'     \item{\emph{segment_meta}: Segment-level metadata}
#'     \item{\emph{defn}: Rule-level data}
#' }
#' Records across both list elements are uniquely identified by the \emph{segment_id} field, for 
#' easy joining, if desired. 
#' @details 
#' This function is still in progress; to properly utilize this function, each row of the return from
#' \code{Get_Segments} must be handled individually. The splitting function is ready, but not yet
#' integrated. 
#' @note 
#' At the moment, \code{...} can be used to pass \emph{collapse_rules} and \emph{bind_rules} (logi) 
#' args to the helper \code{.parse_container} function. This is primarily useful for debugging purposes. 
#' 
#' This function still does not properly parse rules for nested containers, nor stacked segments. A 
#' workaround is in progress for nested containers, but support for parsing stacked segments will take
#' some time, and there is no guarantee that this will be possible.
#' 
#' At the moment, segments that cannot be successfully parsed are passed through, and an error is logged. 
#' This is reflected in the \emph{defn} table, in the field \emph{error}.
#' 
#' This is definitely not a complete function yet; you have been warned. 
#' @export
#'
#' @examples
#' #Forthcoming
restr.Get_Segments <- function(x, merge_rules = TRUE, ...) {
  
  x_meta <- .parse_segMeta(x)
  x_cont <- tryCatch(
    {
      .parse_container(x, merge_rules = merge_rules, ...)
    }, 
    error = function(e) {
      invisible(e)
      message("error in: ", x[["id"]])
      data.table(segment_id = x[["id"]], error = e[[1]])
    }, 
    finally = 
      NULL
  )

  list(segment_meta = x_meta, 
       defn = x_cont)
  
}
NULL
# Extractors --------------------------------------------------------------
# need extractors for shares, compatibility, and tags

# extract definition out of different structures
.extract_defn <- function(x) {
  # if none of the expected names are present, stop
  possible_names <- c("definition", "type", "operator", "rules")
  
  if(is.null(names(unlist(x)))) {
    stop("No names detected in input")
  }
  
  flat_nms <- names(unlist(x)) %>%
    gsub("\\d", "", .) %>%
    unique
  
  nms_chk <- lapply(possible_names, function(f) grepl(f, flat_nms)) %>%
    unlist %>%
    Reduce("|", .)
  
  if(!nms_chk || is.null(nms_chk)) {
    stop("No expected names detected in input")
  }
  
  # Need to normalize input structure to df called "container" with rules nested df
  # handle input of df with container nested within definition
  if("definition" %in% names(x) & is.data.frame(x)) {
    cont <- x[["definition"]]
    return(cont)
  }
  # pass thru
  if(length(names(x)) == 1L && names(x) == "container") {
    return(x)
  }
  # However, if someone passed in rules , need to nest it a level as df
  if(all(c("type", "operator", "rules") %in% names(x))) {
    defn <- data.frame(container = seq_len(nrow(x)))
    defn$container <- data.frame(type = x[["type"]])
    
    x_class <- vapply(x, class, FUN.VALUE = character(1))
    x_flat <- x[, which(x_class != "list")]
    
    defn$container <- x_flat
    defn$container$rules <- x[["rules"]]
    return(defn)
  }
  
}

NULL
# Parsers -----------------------------------------------------------------
NULL
# parse top-level segment metadata
.parse_segMeta <- function(x) {
  targs_meta <- vapply(x, class, FUN.VALUE = character(1))
  
  x_meta <- x[, names(targs_meta)[
    !targs_meta %in% c("list", "data.frame")]
    ] %>%
    as.data.table
  
  setnames(x_meta, paste0("segment_", names(x_meta)))
  
  return(x_meta)
  
}
NULL
# parse container, calls .extract_defn
.parse_container <- function(x, collapse_rules = FALSE, bind_rules = FALSE, merge_rules = FALSE,
                             field = "definition", subfield = "container", subfield_type = "rules") {
  
  # first extract or verify required data structure
  defn <- .extract_defn(x)
  
  # Then get ID
  id <- x[["id"]]
  if(is.null(id)) {
    stop("id is missing in x")
  }
  
  
  # only extract non-list elements from cont$container
  targs_container <- vapply(defn$container, function(f) 
    class(f), FUN.VALUE = character(1)
  )
  container_meta <- defn$container[, names(targs_container)[
    !targs_container %in% c("list", "data.frame")]
    ]
  # append field, subfield
  container_meta[["field"]] <- field
  container_meta[["subfield"]] <- subfield
  # set to DT, fix names
  container_meta <- as.data.table(container_meta)
  # operator needs to be identified...should not hard-code this
  container_meta[["segment_id"]] <- id
  
  
  # Now handle rules
  container_rules <- defn$container[["rules"]] %>%
    lapply(X=., as.data.table) %>%
    lapply(X=., function(f) f[, ":="(field = field, 
                                     subfield = subfield, 
                                     subfield_type = subfield_type)])
  names(container_rules) <- id # to be safe
  for(i in seq_along(id)) {
    container_rules[[i]][, c("segment_id") := id[[i]]]
  }
  
  # identify overlapping names between meta and rules
  # but make sure you exclude standard names
  exclude_nms <- c(quote(field), quote(subfield), "segment_id")
  nms_overlap <- intersect(names(container_meta), 
                           names(container_rules[[1]])
  ) %>%
    setdiff(., exclude_nms)
  
  # if any overlapping names, handle them, for container_meta
  if(length(nms_overlap) > 0L) {
    setnames(container_meta, nms_overlap, paste0("container_", nms_overlap))
  }
  
  if(collapse_rules || merge_rules) {
    rules <- lapply(container_rules, function(f) dcast(
      f, ... ~ subfield_type, 
      value.var = "value", 
      fun.aggregate = function(x) paste(unique(x[!is.na(x)]), collapse = ", "))
    ) 
  } else {
    rules <- container_rules
  }
  
  if(bind_rules || merge_rules) {
    rules <- rbindlist(rules, use.names = TRUE, fill = TRUE)
  }
  
  if(merge_rules) {
    out <- merge(container_meta, rules, by = c("segment_id", "field", "subfield"))
    
  } else {
    out <- list(container_meta = container_meta, 
                rules = rules)
  }
  return(out)
}



# To be integrated --------------------------------------------------------

.split_segment_ret <- function(seg_ret) {
  # check class
  if(!is.list(seg_ret)) {
    stop("seg_ret is of class ",
         class(seg_ret), " but expected a data.frame"
    )
  }
  # get id
  id <- seg_ret[["id"]]
  # check row and id match
  if(nrow(seg_ret) != length(id)) {
    stop("There are ", nrow(seg_ret),
         " but only ", length(id), " ids"
    )
  }

  out <- lapply(seq_along(id), function(f)
    seg_ret[f, ])
  names(out) <- id

  return(out)
}


# easy-mode get all; template of sorts, before packaging
# easy.Get_Segments <- function(..., fun = NULL) {
#   
#   if(is.null(fun)) {
#     fun <- "GS_ALL"
#   }
#   
#   seg_call  <- match.fun(fun)(...)
#   restr_call <- .split_segment_ret(seg_call) %>%
#     Map(restr.Get_Segments, ., merge_rules = TRUE) %>%
#     purrr::transpose(.)
#   
#   lapply(restr_call, function(f) 
#     rbindlist(f, use.names = TRUE, fill = TRUE))
# }