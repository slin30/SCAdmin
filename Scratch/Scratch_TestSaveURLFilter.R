# This is only for pushing new URL Filters for Daf, at the moment

library(magrittr)
library(RSiteCatalyst)
library(data.table)
library(jsonlite)
library(SCAdmin)

SCAuth(key = Sys.getenv("wz_sc_id"), Sys.getenv("wz_sc_pw"))

# FUNS --------------------------------------------------------------------

source("./Scratch/Scratch_TestFUNS.R") # temporary stuff


# Read in URLs ------------------------------------------------------------

rawURLs <- scan("C:/Users/WZDESKTOP/Dropbox/HA Report Suite Internal Domains.txt", 
                what = "character", 
                sep = "\n", 
                fileEncoding = "UTF-8-BOM", 
                multi.line = TRUE
)

# View a few samples
rawURLs[sample(seq_along(rawURLs), 10)] %>% cat(., sep = "\n")

# View as tbl
View(data.table(rawURLs))


# vars --------------------------------------------------------------------

my_rsid <- "elsevier-ha-prod"


# GET ---------------------------------------------------------------------

before_push <- GetInternalURLFilters(reportsuite.ids = my_rsid)



# Split into two sets for pushing -----------------------------------------

set_1 <- rawURLs[1:10]
set_2 <- setdiff(rawURLs, set_1)
# remove https: (literal), after confirming with Daf
set_2 <- set_2[set_2 != "https:"]


# Push set_1 --------------------------------------------------------------

# SaveInternalURLFilters(rsid = my_rsid, urls = set_1)
# after_set_1 <- GetInternalURLFilters(my_rsid)


# Push set_2 --------------------------------------------------------------

# SaveInternalURLFilters(rsid = my_rsid, urls = set_2)
# after_set_2 <- GetInternalURLFilters(my_rsid)


# Write out as xlsx -------------------------------------------------------

# library(xlsx)
# 
# xl_out_path <- "C:/Users/WZDESKTOP/Desktop/elsever-ha-prod_InternalURLFilters.xlsx"
# 
# write.xlsx2(before_push, xl_out_path, sheetName = "before_push", row.names = FALSE, append = FALSE)
# write.xlsx2(after_set_1, xl_out_path, sheetName = "after_set_1", row.names = FALSE, append = TRUE)
# write.xlsx2(after_set_2, xl_out_path, sheetName = "after_set_2", row.names = FALSE, append = TRUE)
