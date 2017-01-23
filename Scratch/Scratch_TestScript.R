library(magrittr)
library(RSiteCatalyst)
library(data.table)
library(jsonlite)

SCAuth(key = Sys.getenv("wz_sc_id"), Sys.getenv("wz_sc_pw"))

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

####

#Helper FUN to make DF from lst or df (L1 or L2)
.h_lst_to_dt.l1 <- function(x, rn = "field", hierarchy) {
  if(missing(rn)) {
    stop("rn is missing")
  }
  bind <- do.call(cbind, x) # handle list elements that are vectors > 1L
  # TODO: Check that all vectors are same length
  dt <- as.data.table(t(bind), keep.rownames = rn)
  out <- melt(dt, id.vars = rn, variable.name = "idx", variable.factor = FALSE)
  out[, "idx" := as.integer(stringr::str_extract(get("idx"), "\\d+"))]
  out[, "hierarchy" := hierarchy]
}

.h_lst_to_dt.l2 <- function(x, type, autoID = FALSE, hierarchy) {
  if(missing(type)) {
    stop("rn and/or type  missing")
  }
  bind <- do.call(cbind, x) # handle list elements that are vectors > 1L
  # TODO: Check that all vectors are same length
  dt <- as.data.table(t(bind), keep.rownames = "subfield")
  out <- melt(dt, id.vars = "subfield", variable.name = "subidx", variable.factor = FALSE)
  out[, "field" := type]
  out[, "hierarchy" := as.integer(hierarchy)]
  
  if(autoID) {
    out[, "idx" := as.integer(stringr::str_extract(get("subidx"), "\\d+"))]
  }
  
  return(out)
}



x <- full_multi

x_filt <- lapply(x, class)

# capture vector, pass on data.frame
x_vec       <- x[x_filt != "data.frame"]
x_container <- as.list(x[[c("definition", "container")]])

# handle x_vec, will always have values unless error
dt.x_vec <- .h_lst_to_dt.l1(x_vec, rn = "field", hierarchy = 1L)

# Now handle things that need to be unnested

# container needs to be additionally separated
x_container_only <- x_container[!names(x_container) == "rules"]
dt.x_container_only <- .h_lst_to_dt.l2(x_container_only, type = "container", autoID = TRUE, hierarchy = 2L)
dt.x_container_only[, subidx:= NULL]


x_rules <- x_container[[c("rules")]] %>%
  Map(as.data.table, .)


dt.x_rules <- lapply(x_rules, function(f) .h_lst_to_dt.l2(f, type = "rule", autoID = FALSE, hierarchy = 3L)) %>%
  rbindlist(., use.names = TRUE, idcol = "idx")


out <- rbindlist(list(dt.x_vec, dt.x_container_only, dt.x_rules), use.names = TRUE, fill = TRUE, idcol = FALSE)
out[, "value" := fix_blank(get("value"))]

setcolorder(out, c("hierarchy", "field", "idx", "subfield", "subidx", "value"))

out_cast <- dcast(out, hierarchy + field + subfield + subidx~ idx, value.var = "value")
setorder(out_cast, hierarchy, subidx)
####




all_single <- GS_ALL(filters = list(name = "aarti"), 
                     accessLevel = "all")

all_mg <- GS_ALL(filters = list(name = "KNOVEL Paying"))

