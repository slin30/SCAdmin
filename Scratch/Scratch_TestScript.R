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

# single-rows
splitted <- lapply(CALLS, SCAdmin:::split_segment_ret)

# list2env(splitted, envir = globalenv())
# rm(list = ls(pattern = "split_.*"))

# flatten
# capture nesting depth
DT_iters <- lapply(splitted, function(f) 
  lapply(f, function(z) 
    testthat::capture_messages(flatten_nested_defn(z))
  )
) %>%
  lapply(X=., function(f) data.table(segment.id = names(f), msg = unlist(f))) %>%
  lapply(X=., function(f) f[, ":="(iters = as.integer(stringr::str_extract(msg, "\\d+")))]) %>%
  rbindlist(., use.names = TRUE, idcol = "context")


flats <- lapply(splitted, function(f) lapply(f, flatten_nested_defn))
