GS_ALL <- function(...) {
  Segments_Get(fields = c("tags", "shares",
                               "description", "owner",
                               "modified", "compatibility",
                               "favorite", "reportSuiteID",
                               "definition"),  
                    ...)
}

# # easy-mode get all; template of sorts, before packaging
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



# Restructuring, for integration ------------------------------------------

# if cont_rule is in names, then rbindlist it
# helper to .split_flat_cont
.l_helper_bind_rules <- function(x) {
  
  if(!"cont_meta" %in% names(x)) {
    stop("content_meta missing")
  }
  
  if(!"cont_rule" %in% names(x)) {
    return(x)
  }
  
  # handle inputs where cont_meta is present along with cont_rule
  
  cont_meta <- x[["cont_meta"]]
  cont_rule <- x[["cont_rule"]]
  
  
  # check that all elements are df
  df_check <- vapply(seq_along(cont_rule), 
                     function(f) is.data.frame(cont_rule[[f]]), FUN.VALUE = logical(1))
  if(!all(df_check)) {
    stop(
      "All elements of x are not of class data.frame"
    )
  }
  
  # process as DT, make keys to merge
  out_rules <- rbindlist(cont_rule, 
                         use.names = TRUE, 
                         fill = TRUE, 
                         idcol = "rule_set_ID"
  )
  out_meta  <- as.data.table(cont_meta)
  out_meta[, rule_set_ID := .I]
  
  list(
    cont_meta = out_meta, 
    cont_rule = out_rules
  )
  
}

# split a parsed return from parse_seg_return, remove NULLs, bind rules
bind_flat_cont <- function(x) {
  targ <- "segment_meta"
  
  if(!targ %in% names(x)) {
    stop("Required input element of ", substitute(targ), " not present")
  }
  
  segment_meta <- x[[targ]]
  segment_cont <- x[setdiff(names(x), substitute(targ))] %>%
    lapply(X=., function(f) Filter(function(x) !is.null(x), f)) 
  
  # make sure other names have not crept into segment_cont
  segCont_nms <- paste0("L", seq_along(segment_cont)-1L)
  if(length(setdiff(names(segment_cont), segCont_nms)) > 0L) {
    stop("Mismatch in names in segment_cont and expected names")
  }
  
  segment_cont <- lapply(segment_cont, .l_helper_bind_rules)
  
  list(segment_meta = segment_meta, 
       segment_cont = segment_cont
  )
}

# For Daf-- SaveInternalURLFilters draft ----------------------------------

# This is very much a WIP-- no error checking at all!
# rsid_list and internal_url_filters are arrays, so use c() without unbox, and 
# toJSON the body.
SaveInternalURLFilters <- function(rsid, urls) {
  if(!is.atomic(rsid) || !is.atomic(urls)) {
    stop("rsid and urls must both be atomic vectors")
  }
  #Could also accept list, with c(..., recursive=TRUE)
  
  body <- list(
    rsid_list = rsid,
    internal_url_filters = urls
  )
  # NOTE THE NAME OF THE METHOD!!!
  ApiRequest(
    body = jsonlite::toJSON(body),
    func.name = "ReportSuite.SaveInternalURLFilters"
    )
  
}

# my_url <- c("www.mouselivercells.com")
# my_rs  <- "elsevier-ha-prod"
# 
# SaveInternalURLFilters(rsid = my_rs, urls = my_url)
# GetInternalURLFilters("elsevier-ha-prod") # check it



