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

# Errors in input testing -------------------------------------------------

Get_Segments(filters = c("A", "a"))
Get_Segments(filters = list("A", "B"))

Get_Segments(fields = "All")
Get_Segments(accessLevel = "A")
Get_Segments(sort = 1:2)
Get_Segments(sort = "A")
Get_Segments(filters = list(name = "A", alt="B"))


# Working -----------------------------------------------------------------

full_single <- Get_Segments(filters = list(name = "aarti"), 
                            fields = c("definition", "description", "owner", "modified", "reportSuiteID"), 
                            accessLevel = "owned")
full_multi <- Get_Segments(filters = list(name = "aarti"), 
                           fields = c("definition", "description", "owner", "modified", "reportSuiteID"), 
                           accessLevel = "all")
MG_all <- GS_ALL(filters = list(name = "Knovel"))
WZ_all <- GS_ALL() # test all mine; there are 34, 3 are problematic

#final invalid indices; hack for now
cont_WZ <- .extract_defn(WZ_all) # right now, have to handle problematic via hack
valids <- lapply(cont_WZ$container$rules, function(f) f[["name"]])
valids_filt_null <- lapply(valids, function(f) !is.null(f))
valids_filt_NA   <- lapply(valids, function(f) all(!is.na(f)))

valids_filt_out <- Map("&", valids_filt_null,valids_filt_NA)
valids_idx <- which(unlist(valids_filt_out))
invalids_idx <- setdiff(seq_len(nrow(WZ_all)), valids_idx) # for testing  purposes

######

p.full_single <- restr.Get_Segments(full_single, collapse_rules = TRUE, bind_rules = TRUE)
p.full_multi  <- restr.Get_Segments(full_multi, collapse_rules = TRUE, bind_rules = TRUE)
p.MG_all      <- restr.Get_Segments(MG_all, collapse_rules = TRUE, bind_rules = FALSE)
p.WZ_all      <- restr.Get_Segments(WZ_all, collapse_rules = FALSE, bind_rules = FALSE) # at least this works...


###
WZ_meta <- p.WZ_all$segment_meta

### Address invalids by re-querying with clean ID
valid_seg <- WZ_meta[valids_idx, segment_id]
invalid_seg <- WZ_meta[invalids_idx, segment_id]

WZ_all_clean <- GS_ALL(selected = valid_seg)
WZ_all_dirty <- GS_ALL(selected = invalid_seg[[1]]) # just one for now; recursive extraction may require
# only one segment at a time for now

p.WZ_all_clean <- restr.Get_Segments(WZ_all_clean, collapse_rules = TRUE, bind_rules = TRUE)
#p.WZ_all_dirty <- restr.Get_Segments(WZ_all_dirty, collapse_rules = TRUE, bind_rules = TRUE) #error


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

out_recur <- extract_nested(WZ_all_dirty) %>%
  lapply(X=., function(f) f[[1]][c("name", "element", "operator", "value")]) %>%
  rbindlist %>%
  .[!is.na(name)] %>%
  .[, ":="(field = "definition", subfield = "container")]



# or a simple way to get atomic stuff
recur_get_what <- function(x, what = "value") {
  ul <- unlist(x)
  filt <- grepl(what, names(ul))
  
  out <- unlist(unname(ul[filt]))
  unique(out[!is.na(out)])
}


vals <- recur_get_what(WZ_all_dirty, what = "value")
alt_vals <- recur_get_what(tst1, what = "value")
whats <- list("id", "operator", "value", "type", "element", "name")
names(whats) <- whats

my_whats <- lapply(whats, function(f) recur_get_what(WZ_all, f))


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

## In process
dt_seg <- p.WZ_all_clean$segment_meta %>% setkey(., segment_id)
dt_def_meta <- p.WZ_all_clean$defn$container_meta %>% setkey(., segment_id)
dt_def_dat  <- p.WZ_all_clean$defn$rules %>% setkey(., segment_id)
def_mrg <- merge(dt_def_meta, dt_def_dat, by = c("segment_id", "field", "subfield"))

def_mrg[, dupe := duplicated(.SD),  .SDcols = -c("segment_id")]

# and append other meta
def_out <- merge(dt_seg, def_mrg, by = "segment_id")


# Delete dupe in progress -------------------------------------------------

dupe_ID <- def_out[dupe == TRUE, segment_id]

double_check <- GS_ALL(selected = dupe_ID) %>%
  restr.Get_Segments(., collapse_rules = TRUE, bind_rules = TRUE)


# WARNING
qstring <- list(
  segmentID = unbox(dupe_ID)
)

# #ApiRequest(toJSON(qstring), "Segments.Delete")

Get_Segments(selected = dupe_ID) # should be NULL, but not found returns list(), so handle
# this in the core FUN later!


# Can we split a multi segment call up? -----------------------------------

split_wz_all <- lapply(seq_len(nrow(WZ_all)), function(f) WZ_all[f, ])
names(split_wz_all) <- WZ_all$id


possible_restr <- purrr::safely(restr.Get_Segments)
pars_wz_all <- lapply(split_wz_all, function(f) possible_restr(f, collapse_rules = TRUE, bind_rules = TRUE)) %>%
  purrr::transpose(.)

bad <- pars_wz_all$error %>% Filter(function(x) !is.null(x), .)
good <- pars_wz_all$result %>% Filter(function(x) !is.null(x), .)


