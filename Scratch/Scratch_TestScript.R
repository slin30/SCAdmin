library(magrittr)
library(RSiteCatalyst)
library(data.table)
library(jsonlite)
library(SCAdmin)

SCAuth(key = Sys.getenv("wz_sc_id"), Sys.getenv("wz_sc_pw"))


# FUNS --------------------------------------------------------------------

#source("./Scratch/Scratch_TestFUNS.R")
source("./Scratch/Scratch_TestFUNS_nested.R")

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
p.full_multi  <- restr.Get_Segments(full_multi, collapse_rules = FALSE, bind_rules = TRUE)
p.MG_all      <- restr.Get_Segments(MG_all)
p.WZ_all      <- restr.Get_Segments(WZ_all, collapse_rules = FALSE, bind_rules = FALSE) # at least this works...


###
WZ_meta <- p.WZ_all$segment_meta
# WZ_cmeta <- p.WZ_all$defn$container_meta
# WZ_cdata <- p.WZ_all$defn$rules

### Address invalids by re-querying with clean ID
valid_seg <- WZ_meta[valids_idx, segment_id]
invalid_seg <- WZ_meta[invalids_idx, segment_id]

WZ_all_clean <- GS_ALL(selected = valid_seg)
WZ_all_dirty <- GS_ALL(selected = invalid_seg[[3]]) # just one for now; recursive extraction may require
# only one segment at a time for now

p.WZ_all_clean <- restr.Get_Segments(WZ_all_clean, collapse_rules = TRUE, bind_rules = TRUE)
#p.WZ_all_dirty <- restr.Get_Segments(WZ_all_dirty, collapse_rules = TRUE, bind_rules = TRUE) #error


# Dirty return parsing ----------------------------------------------------

extract_nested <- function(x, d = 0L, out = vector("list", 0L)) {
  # keep it simple for testing for now
  # assume you get the right input
  message("In iter ", d, " input form of x is", str(x))
  
  # check if container is present
  if("definition" %in% names(x)) {
    x <- x[["definition"]]
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
  

  if(all(chk_lst, chk_nm, chk_L1, chk_L1_nm) && !is.null(x)) {
    extract_nested(x = x[[1]], d = d + 1L, out = append(list(x[[1]]), out) )
  } else {
    return(out)
  }
  
}

tst <- extract_nested(WZ_all_dirty)
lapply(tst, wzMisc::depth)

# or a simple way to get atomic stuff
recur_get_what <- function(x, what = "value") {
  ul <- unlist(x)
  filt <- grepl(what, names(ul))
  
  out <- unlist(unname(ul[filt]))
  unique(out[!is.na(out)])
}


vals <- recur_get_what(WZ_all_dirty, what = "value")
whats <- list("id", "operator", "value", "type", "element", "name")
names(whats) <- whats

my_whats <- lapply(whats, function(f) recur_get_what(WZ_all, f))

# x <- WZ_all_dirty
# "definition" %in% names(x)
# x <- x[["definition"]]
# "container" %in% names(x)
# x <- x[["container"]]
# 
# chk_lst <- is.list(x)
# chk_lst_nm <- "rules" %in% names(x)
# is.data.frame(x[["rules"]][[1]])
# 
# a <- x[[c("rules")]][[1]][["container"]]
# 
# 
# 
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
# 
# 
# 
# tst_recur <- recur_extract(WZ_all_dirty$definition)
# 
# 
# 
# recur_extract2 <- function(x, d = 0L, out = vector("list", 0L)) {
#   print(d)
#   x <- x[[c("rules")]][[1]][["container"]]
#   
#   if(is.null(x)) {
#     return(out)
#   } else {
#     recur_extract2(x = x, d = d + 1L, out = append(out, x))
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

