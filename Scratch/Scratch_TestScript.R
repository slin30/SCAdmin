library(magrittr)
library(RSiteCatalyst)
library(data.table)
library(jsonlite)
library(SCAdmin)

SCAuth(key = Sys.getenv("wz_sc_id"), Sys.getenv("wz_sc_pw"))


# FUNS --------------------------------------------------------------------

source("./Scratch/Scratch_TestFUNS.R")
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

p.full_single <- restr_segRules(full_single, collapse_rules = TRUE, bind_rules = TRUE)
p.full_multi  <- restr_segRules(full_multi, collapse_rules = FALSE, bind_rules = TRUE)
p.MG_all      <- restr_segRules(MG_all)
p.WZ_all      <- restr_segRules(WZ_all, collapse_rules = FALSE, bind_rules = FALSE) # at least this works...


###
WZ_meta <- p.WZ_all$segment_meta
# WZ_cmeta <- p.WZ_all$defn$container_meta
# WZ_cdata <- p.WZ_all$defn$rules

### Address invalids by re-querying with clean ID
valid_seg <- WZ_meta[valids_idx, segment_id]
invalid_seg <- WZ_meta[invalids_idx, segment_id]

WZ_all_clean <- GS_ALL(selected = valid_seg)
WZ_all_dirty <- GS_ALL(selected = invalid_seg[[2]]) # just one for now

p.WZ_all_clean <- restr_segRules(WZ_all_clean, collapse_rules = TRUE, bind_rules = TRUE)
#p.WZ_all_dirty <- restr_segRules(WZ_all_dirty, collapse_rules = TRUE, bind_rules = TRUE) #error


# Dirty return parsing ----------------------------------------------------

def_0 <- .extract_defn(WZ_all_dirty)
def_1 <- def_0$container$rules # 
def_2 <- def_1[[1]] # need to get all the way to this point, identify it


def_0$container %>% str # 3 fields, type,operator,rules; 1 row
#rules is a list of 1 with a nested DF
def_0$container$rules %>% str # a list of 1, with a df, so need to tunnel down one more level
def_0$container$rules[[1]] %>% str # 2 fields, since the first level is another df, container
def_0$container$rules[[1]]$container %>% str # 4 fields, name,type,operator,rules; 2 rows

# which means the next repetition if present should be:
#                                   _ <- right here
def_0$container$rules[[1]]$container$rules[[1]]$container %>% str # and there it is
# 2 fields, type,rules; 11 rows
# and this is as deep as this one goes-- you know this because 
#  1. Number of rows in df equals length of rules list-- this is necessary but not sufficient
#  2. if you tunnel down to rules[[1]]$container, result is NULL: see for yourself
def_0$container$rules[[1]]$container$rules[[1]]$container$rules[[1]]$container #NULL

extract_nested <- function(x) {
  # keep it simple for testing for now
  # assume you get the right input
  
  tunnel <- c("container", "rules")
  x[[tunnel]][[1]]
}

tst <- extract_nested(def_0)
tst1 <- extract_nested(tst)
tst2 <- extract_nested(tst1) # NULL

#ok, now test recursive version

recur_extract <- function(x, d = 0L, out = vector("list", 0L)) {
  print(d)
  x <- x[[c("container", "rules")]][[1]]
  
  if(is.null(x)) {
    return(out)
  } else {
    recur_extract(x = x, d = d + 1L, out = append(out, x))
  }
  
}

tst_recur <- recur_extract(def_0)
tst_recur_orig <- recur_extract(WZ_all_dirty$definition)

recur_extract2 <- function(x, d = 0L, out = vector("list", 0L)) {
  print(d)
  x <- x[[c("rules")]][[1]][["container"]]
  
  if(is.null(x)) {
    return(out)
  } else {
    recur_extract2(x = x, d = d + 1L, out = append(out, x))
  }
  
}

tst_recur2 <- recur_extract2(def_0) # not quite what you want, but kinda on the right path?

##The issue is you need to detect a data.frame, which 


str(tst_recur)
lapply(tst_recur, depth)
# so there are 2 containers; can we parse each individually?
names(tst_recur) <- make.unique(names(tst_recur))
names(tst_recur)

tr_cont1 <- .extract_defn(tst_recur$container)
tr_cont2 <- .extract_defn(tst_recur$container.1) # not quite...

# then need to recursive handle rules? or can we just Map and rbindlist, 
# handling NULLs with new funs?


wtf <- WZ_all_dirty

library(wzMisc)
depth(wtf) # NA
depth_while(wtf) # 14
depth(wtf$definition) # 13
depth_while(wtf$definition) # 13

depth(def_0)
depth(def_1)
depth(def_2)

# look for data.frame without a name
# extract_df <- function(x, lst = vector("list")) {
#   if(is.data.frame(x)[1] && names(x)[1] == "container") {
#     
#     lst <- append(lst, .extract_defn(x))
#   }
#   extract_df()
#     
# }

#####

## In process
dt_seg <- p.WZ_all_clean$segment_meta %>% setkey(., segment_id)
dt_def_meta <- p.WZ_all_clean$defn$container_meta %>% setkey(., segment_id)
dt_def_dat  <- p.WZ_all_clean$defn$rules %>% setkey(., segment_id)
def_mrg <- merge(dt_def_meta, dt_def_dat, by = c("segment_id", "field", "subfield"))

def_mrg[, dupe := duplicated(.SD),  .SDcols = -c("segment_id")]

