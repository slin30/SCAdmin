# # draft restructuring fun
# restr_segRules <- function(x, ...) {
#   
#   x_meta <- .parse_segMeta(x)
#   x_cont <- .parse_container(x, ...)
#   
#   list(segment_meta = x_meta, 
#        defn = x_cont)
# 
# }
# NULL

# Extractors --------------------------------------------------------------
# need extractors for shares, compatibility, and tags

# # extract definition out of different structures
# .extract_defn <- function(x) {
#   # if none of the expected names are present, stop
#   possible_names <- c("definition", "type", "operator", "rules")
#   
#   if(is.null(names(unlist(x)))) {
#     stop("No names detected in input")
#   }
#   
#   flat_nms <- names(unlist(x)) %>%
#     gsub("\\d", "", .) %>%
#     unique
#   
#   nms_chk <- lapply(possible_names, function(f) grepl(f, flat_nms)) %>%
#     unlist %>%
#     Reduce("|", .)
#   
#   if(!nms_chk || is.null(nms_chk)) {
#     stop("No expected names detected in input")
#   }
#   
#   # Need to normalize input structure to df called "container" with rules nested df
#   # handle input of df with container nested within definition
#   if("definition" %in% names(x) & is.data.frame(x)) {
#     cont <- x[["definition"]]
#     return(cont)
#   }
#   # pass thru
#   if(length(names(x)) == 1L && names(x) == "container") {
#     return(x)
#   }
#   # However, if someone passed in rules , need to nest it a level as df
#   if(all(c("type", "operator", "rules") %in% names(x))) {
#     defn <- data.frame(container = seq_len(nrow(x)))
#     defn$container <- data.frame(type = x[["type"]])
#     
#     x_class <- vapply(x, class, FUN.VALUE = character(1))
#     x_flat <- x[, which(x_class != "list")]
#     
#     defn$container <- x_flat
#     defn$container$rules <- x[["rules"]]
#     return(defn)
#   }
#   
# }
# # 

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



