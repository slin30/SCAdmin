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


# Some test datasets ------------------------------------------------------

CALLS <- vector("list", 0L)
CALLS$seg_wz_all <- GS_ALL() #has positive and negative controls

# Lots of shares for testing
CALLS$seg_mg_shares <- GS_ALL(accessLevel = "all", 
                     filters = list(name = "exclude internal knovel", 
                                    owner = "m.gray")
)

# Here are some properly built nested segments, as a first test case:
CALLS$seg_ret.tst <- Segments_Get(accessLevel = "all", filters = list(name = "DB Session", 
                                                                  owner = "m.gray"), 
                              fields = c("tags", "shares",
                                         "description", "owner",
                                         "modified", "compatibility",
                                         "favorite", "reportSuiteID",
                                         "definition")
)
# Reference set-- an unnested segment for comparison
CALLS$seg_ret.ref <- Segments_Get(accessLevel = "owned", filters = list(name = "ThermoPhys"), 
                             fields = c("tags", "shares",
                                        "description", "owner",
                                        "modified", "compatibility",
                                        "favorite", "reportSuiteID",
                                        "definition")
)
# no definition requested
CALLS$seg_simple <- Segments_Get(fields = c("tags", "shares",
                                          "description", "owner",
                                          "modified", "compatibility",
                                          "favorite", "reportSuiteID")
)
# a nested segment from clinical, not sure if nesting is needed here
CALLS$clinical <- Segments_Get(filters = list(name = "abb CK-P Browse Page Entry"), 
                                           accessLevel = "all", 
                               fields = c("tags", "shares",
                                          "description", "owner",
                                          "modified", "compatibility",
                                          "favorite", "reportSuiteID", 
                                          "definition")
)

# Parsing definition ------------------------------------------------------

# Let's grab only a single segment from each
splitted <- vector("list", 0L)
splitted$split_tst <- SCAdmin:::split_segment_ret(CALLS$seg_ret.tst)
splitted$split_ref <- SCAdmin:::split_segment_ret(CALLS$seg_ret.ref)
splitted$split_wz_all  <- SCAdmin:::split_segment_ret(CALLS$seg_wz_all)
splitted$split_mg_shares <- SCAdmin:::split_segment_ret(CALLS$seg_mg_shares)
splitted$split_simple <- SCAdmin:::split_segment_ret(CALLS$seg_simple)
splitted$split_clinical <- SCAdmin:::split_segment_ret(CALLS$clinical)

# list2env(splitted, envir = globalenv())
# rm(list = ls(pattern = "split_.*"))

# Flatten with wrapper FUN
single_flats <- vector("list", 0L)
single_flats$flat_ref <- flatten_container(splitted$split_ref$s300000520_589a29f3e4b0cfc8b41c8978)
single_flats$flat_tst <- flatten_container(splitted$split_tst$s300000520_589a1638e4b0cfc8b41c8960)
single_flats$flat_wz  <- flatten_container(splitted$split_wz_all$s300000520_57e0343be4b007430cbdcdc3)
single_flats$flat_wz_neg  <- flatten_container(splitted$split_wz_all$s300000520_582ca0d2e4b0a4d9dc2936ac)
single_flats$flat_mg  <- flatten_container(splitted$split_mg_shares$`557a2135e4b093528ae4d446`)
single_flats$flat_simple <- flatten_container(splitted$seg_simple) # error!
single_flats$flat_clinical <- flatten_container(splitted$split_clinical$s300000520_584af94be4b015df188ac7a4)

# list2env(single_flats, envir = globalenv())
# rm(list = ls(pattern = "flat_.*"))

# Bind --------------------------------------------------------------------
binds <- vector("list", 0L)
binds$bnd_ref <- bind_flat_cont(single_flats$flat_ref)
binds$bnd_tst <- bind_flat_cont(single_flats$flat_tst)
binds$bnd_wz  <- bind_flat_cont(single_flats$flat_wz)
binds$bnd_wz_neg <- bind_flat_cont(single_flats$flat_wz_neg) # error!
binds$bnd_mg  <- bind_flat_cont(single_flats$flat_mg)
binds$bnd_clinical <- bind_flat_cont(single_flats$flat_clinical) # error!

# list2env(binds, envir = globalenv())
# rm(list = ls(pattern = "bnd_.*"))


# Shares ------------------------------------------------------------------


# Saving segment with shares ----------------------------------------------

# For DAF -----------------------------------------------------------------
# # Daf segments
# 
# dseg <- Segments_Get(accessLevel = "all", 
#                           filters = list(reportSuiteID = "elsevier-ha-prod", 
#                                          tags = "JBS-Journal"), 
#                           fields = c("description", "owner", "definition", 
#                                      "reportSuiteID", "modified")
# )
# 
# dseg_split <- SCAdmin:::split_segment_ret(dseg)
# 
# # DAF restr (WIP)
# dseg_flat <- lapply(dseg_split, function(f) flatten_container(f))
# dseg_bind <- lapply(dseg_flat, function(f) bind_flat_cont(f))
# 
# 
# tst_merge <- function(x) {
#   cont <- x[["segment_cont"]][[1]]
#   
#   cont_bind <- merge(cont[["cont_meta"]], cont[["cont_rule"]], by = "rule_set_ID")
#   
#   out <- list(x[["segment_meta"]], cont_bind)
#   names(out) <- names(x)
#   return(out)
# }
# 
# dseg_mrg <- lapply(dseg_bind, function(f) tst_merge(f))
# dseg_rbl <- lapply(dseg_mrg, function(f) do.call(cbind, f))
# 
# out <- rbindlist(dseg_rbl, use.names = TRUE)
# 
# library(xlsx)
# 
# write.xlsx2(out, "C:/Users/WZDESKTOP/Desktop/DAF_SEGMENTS.xlsx", row.names = FALSE, sheetName = "DATA")

# Make segments testing ---------------------------------------------------

# this is a segment that contains two rules with exclude

mg_exclude <- Segments_Get(selected = "s300000520_589774a4e4b08939f9d6e818",
                                fields = c("description", "definition", "owner", "modified", 
                                           "reportSuiteID"), 
                                accessLevel = "all")
mg_exclude.parse <- flatten_container(mg_exclude)

# to recreate this

## and to edit; get the ID first
Segments_Get(filters = list(name = "ThermoPhysDyn_ContentName"))
#s300000520_589a29f3e4b0cfc8b41c8978 is the ID for edit

seg_shares <- make_sharelist("user", c("m.gray"))

seg_meta <- make_segment_meta(name = "ThermoPhysDyn_ContentName", 
                              reportSuiteID = mg_exclude$reportSuiteID, 
                              owner = "w.zhang", 
                              tags = "SA_CNAME", 
                              shares = seg_shares, 
                              favorite = FALSE
)

# have to use a temporary DT while figuring out the parsing mechanism to completion for
# simple nested segments
rule_dt.tmp <- mg_exclude.parse$L1$cont_rule[[1]] %>% as.data.table

# and pull rule vec from here
rule_vec <- rule_dt.tmp[element == "evar75", value]

# check if any of element, operator, or rules is > 1L
# use the temporary alternative function
seg_rules <- make_element_rules(element = "evar75", 
                                rules = rule_vec, 
                                operator = "equals"
)


seg_container <- make_segment_container(type = "hits", 
                                        operator = "or", 
                                        exclude = FALSE, 
                                        rules = seg_rules
)

seg_body <- make_segment_body(segment_container = seg_container, segment_meta = seg_meta)


# test edit
seg_body$id <- jsonlite::unbox("s300000520_589a29f3e4b0cfc8b41c8978")
# #my_new_seg <- Save_Segment(seg_body)
# #my_edit_seg <- Save_Segment(seg_body, override_and_edit = TRUE)

Segments_Get(filters = list(tags = "SA_CNAME")) # quick check
recheck <- Segments_Get(filters = list(tags = "SA_CNAME"), 
                  fields = c("shares", "favorite", "owner", "shares", "definition", "description")
)

collapse_shares(recheck)
