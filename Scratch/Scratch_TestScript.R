library(magrittr)
library(RSiteCatalyst)
library(data.table)
library(jsonlite)
library(SCAdmin)

SCAuth(key = Sys.getenv("wz_sc_id"), Sys.getenv("wz_sc_pw"))


# FUNS --------------------------------------------------------------------

source("./Scratch/Scratch_TestFUNS.R") # temporarily pull in .extract_defn for testing
#source("./Scratch/Scratch_TestFUNS_nested.R")

fix_blank <- function(x) {
  x[x==""] <- NA
  x
}

GS_ALL <- function(...) {
  Get_Segments(fields = c("tags", "shares",
                          "description", "owner",
                          "modified", "compatibility",
                          "favorite", "reportSuiteID",
                          "definition"),  
               ...)
}

# splitter for get_segments return
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
easy.Get_Segments <- function(..., fun = NULL) {
  
  if(is.null(fun)) {
    fun <- "GS_ALL"
  }

  seg_call  <- match.fun(fun)(...)
  restr_call <- .split_segment_ret(seg_call) %>%
    Map(restr.Get_Segments, ., merge_rules = TRUE) %>%
    purrr::transpose(.)
  
  lapply(restr_call, function(f) 
    rbindlist(f, use.names = TRUE, fill = TRUE))
}

# Errors in input testing -------------------------------------------------

Get_Segments(filters = c("A", "a"))
Get_Segments(filters = list("A", "B"))

Get_Segments(fields = "All")
Get_Segments(accessLevel = "A")
Get_Segments(sort = 1:2)
Get_Segments(sort = "A")
Get_Segments(filters = list(name = "A", alt="B"))


# Working -----------------------------------------------------------------

full_single <- easy.Get_Segments(filters = list(name = "aarti"), 
                                 fields = c("definition", "description", 
                                            "owner", "modified", 
                                            "reportSuiteID"), 
                                 accessLevel = "owned", 
                                 fun = "Get_Segments"
)
full_multi <- easy.Get_Segments(filters = list(name = "aarti"), 
                                fields = c("definition", "description",
                                           "owner", "modified", 
                                           "reportSuiteID"), 
                                accessLevel = "all", 
                                fun = "Get_Segments"
)
MG_all <- easy.Get_Segments(filters = list(name = "Knovel"))
WZ_all <- easy.Get_Segments() # test all mine; there are 34, 3 are problematic

WZ_out <- merge(WZ_all$segment_meta, WZ_all$defn, by = c("segment_id"))

WZ_call <- GS_ALL()
WZ_call_split <- .split_segment_ret(WZ_call)

type_1 <- WZ_call_split$s300000520_57e0327ae4b007430cbdcdc0 # this one part of the stacked segment. Not created properly
# since is unnecessarily nested, but perhaps a good test case
type_2 <- WZ_call_split$s300000520_57e0343be4b007430cbdcdc3 # this is the stacked segment
type_3 <- WZ_call_split$s300000520_582ca0d2e4b0a4d9dc2936ac # this is a pretty terribly created (TLV) nested
# unnecessarily as well.

# Dirty return parsing ----------------------------------------------------

# This currently works for nested elements, but not stacked segments, 
#  and need to figure out the logic for > 1 segment in a single call where one or more
#  is nested (but NOT stacked)
# Which means I need to identify stacked segments and filter them out
extract_nested <- function(x, d = 0L, out = vector("list", 0L)) {
  # keep it simple for testing for now
  # assume you get the right input
  message("In iter ", d, " input form of x is", str(x))
  
  # check if container is present
  if("definition" %in% names(x)) {
    x <- x[["definition"]][["container"]][["rules"]][[1]]
    out[[paste0("iter_", d)]] <- list(x)
    d <- d+1L
  }
  
  if("container" %in% names(x)) {
    x <- x[["container"]]
  }
  
  if(is.data.frame(x)) {
    x <- as.list(x)
  }

  message("In iter ", d, " intermediate form of x is", str(x))
  
  
  chk_lst <- is.list(x)
  chk_nm <- "rules" %in% names(x)
  
  if(chk_lst & chk_nm) {
    x <- Filter(function(x) !is.null(x), x[["rules"]])
  }
  
  message("In iter ", d, " Filtered structure of x is", str(x))
  
  if(is.null(x)) {
    return(out)
  }
  
  chk_L1 <- is.data.frame(x[[1]])
  chk_L1_nm <- "container" %in% names(x[[1]])
  
  # if(all(chk_lst, chk_nm, chk_L1, chk_L1_nm) && !is.null(x)) {
  if(all(chk_L1, chk_L1_nm) && !is.null(x)) {
    
    out[[paste0("iter_", d)]] <- x
    # out <- lapply(out, function(f) Filter(function(x) !is.null(x), f))
    # extract_nested(x = x[[1]][["container"]], d = d + 1L, out)
    extract_nested(x = x[[1]][["container"]], d = d + 1L, out)
  } else { # we are at the lowest level; still needs to tweaking 
    x <- x
    if(length(x) > 1L) {
      x <- do.call(rbind, x)
    }
    new_nm <- paste0("iter_", d+1L)
    if(is.list(x) && ! is.data.frame(x)) {
      out[[new_nm]] <- x
    } else {
      out[[new_nm]] <- list(x)
    }
    return(out)
  }
  
}
# could make this the FUN, which calls extract_nested
# although potentially more elegant if you can stop within extract_nested?
# Either way, want to error, so you can handle it 
out_recur <- extract_nested(WZ_all_dirty) %>%
  lapply(X=., function(f) f[[1]][c("name", "element", "operator", "value")]) %>%
  rbindlist %>%
  .[!is.na(name)] %>%
  .[, ":="(field = "definition", subfield = "container")]



# # or a simple way to get atomic stuff
# recur_get_what <- function(x, what = "value") {
#   ul <- unlist(x)
#   filt <- grepl(what, names(ul))
#   
#   out <- unlist(unname(ul[filt]))
#   unique(out[!is.na(out)])
# }
# 
# 
# vals <- recur_get_what(WZ_all_dirty, what = "value")
# alt_vals <- recur_get_what(tst1, what = "value")
# whats <- list("id", "operator", "value", "type", "element", "name")
# names(whats) <- whats
# 
# my_whats <- lapply(whats, function(f) recur_get_what(WZ_all, f))


# recur_extract <- function(x, d = 0L, out = vector("list", 0L)) {
#   print(d)
#   x <- x[[c("container", "rules")]][[1]]
#   
#   if(is.null(x)) {
#     return(out)
#   } else {
#     recur_extract(x = x, d = d + 1L, out = append(out, x))
#   }
#   
# }

#####

# Delete dupe in progress -------------------------------------------------
# 
# dupe_ID <- def_out[dupe == TRUE, segment_id]
# 
# double_check <- GS_ALL(selected = dupe_ID) %>%
#   restr.Get_Segments(., collapse_rules = TRUE, bind_rules = TRUE)
# 
# 
# # WARNING
# qstring <- list(
#   segmentID = unbox(dupe_ID)
# )
# 
# # #ApiRequest(toJSON(qstring), "Segments.Delete")
# 
# Get_Segments(selected = dupe_ID) # should be NULL, but not found returns list(), so handle
# # this in the core FUN later!
