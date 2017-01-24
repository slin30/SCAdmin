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


p.full_single <- restr_segRules(full_single)
p.full_multi  <- restr_segRules(full_multi)
p.MG_all      <- restr_segRules(MG_all)

bind.full_single <- rbindlist(p.full_single, use.names = TRUE, fill = TRUE)
bind.full_multi  <- rbindlist(p.full_multi, use.names = TRUE, fill = TRUE)
bind.MG_all      <- rbindlist(p.MG_all, use.names = TRUE, fill = TRUE)

cast.MG_all <- dcast(bind.MG_all, ... ~ variable, value.var = "value", 
                     fun.aggregate = function(x) paste(unique(x[!is.na(x)]), collapse = ", ")
) %>% setorderv(., c("field"), order = -1L)

