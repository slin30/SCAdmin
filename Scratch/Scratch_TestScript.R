library(magrittr)
library(RSiteCatalyst)
library(data.table)
library(jsonlite)
library(SCAdmin)

SCAuth(key = Sys.getenv("wz_sc_id"), Sys.getenv("wz_sc_pw"))


# FUNS --------------------------------------------------------------------

source("./Scratch/Scratch_TestFUNS.R")
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
######

p.full_single <- restr_segRules(full_single, collapse_rules = TRUE, bind_rules = TRUE)
p.full_multi  <- restr_segRules(full_multi, collapse_rules = FALSE, bind_rules = TRUE)
p.MG_all      <- restr_segRules(MG_all)
p.WZ_all      <- restr_segRules(WZ_all, collapse_rules = FALSE, bind_rules = FALSE) # at least this works...


###
WZ_meta <- p.WZ_all$segment_meta
WZ_cmeta <- p.WZ_all$defn$container_meta
WZ_cdata <- p.WZ_all$defn$rules

### Address invalids by re-querying with clean ID
retest <- WZ_meta[valids_idx, segment_id]
WZ_all_clean <- GS_ALL(selected = retest)
p.WZ_all_clean <- restr_segRules(WZ_all_clean, collapse_rules = TRUE, bind_rules = TRUE)


## In process
dt_seg <- p.WZ_all_clean$segment_meta %>% setkey(., segment_id)
dt_def_meta <- p.WZ_all_clean$defn$container_meta %>% setkey(., segment_id)
dt_def_dat  <- p.WZ_all_clean$defn$rules %>% setkey(., segment_id)
def_mrg <- merge(dt_def_meta, dt_def_dat, by = c("segment_id", "field", "subfield"))

def_mrg[, dupe := duplicated(.SD),  .SDcols = -c("segment_id")]

