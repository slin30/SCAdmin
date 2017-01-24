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
# recur_get_names <- function(x, i = 0L, nm =NULL) {
#   
#   if(!is.list(x)) {
#     nm <- c(names(x), nm)
#     return(nm)
#   } else {
#     print(i)
#     recur_get_names(x = unlist(x, recursive = FALSE), i = i+1, nm = c(nm))
#   }
#   
# }


.parse_container <- function(x) {
  # if none of the expected names are present, stop
  possible_names <- c("definition", "type", "operator", "rules")
  
  if(is.null(names(unlist(x)))) {
    stop("No names detected in input")
  }
  
  flat_nms <- names(unlist(x)) %>%
    gsub("\\d", "", .) %>%
    unique
  
  nms_chk <- lapply(possible_names, function(f) grepl(f, flat_nms)) %>%
    unlist %>%
    Reduce("|", .)
  
  if(!nms_chk || is.null(nms_chk)) {
    stop("No expected names detected in input")
  }
  
  # Need to normalize input structure to df called "container" with rules nested df
    # handle input of df with container nested within definition
  if("definition" %in% names(x) & is.data.frame(x)) {
    cont <- x[["definition"]]
    return(cont)
  }
    # pass thru
  if(length(names(x)) == 1L && names(x) == "container") {
    return(x)
  }
    # However, if someone passed in rules , need to nest it a level as df
  if(all(c("type", "operator", "rules") %in% names(x))) {
    cont <- data.frame(container = seq_len(nrow(x)))
    cont$container <- data.frame(type = x[["type"]])
    
    x_class <- vapply(x, class, FUN.VALUE = character(1))
    x_flat <- x[, which(x_class != "list")]
    
    cont$container <- x_flat
    cont$container$rules <- x[["rules"]]
    return(cont)
  }
  
}

# a <- full_multi[["definition"]]
# b <- a$container
# 
# 
# p1 <- .parse_container(full_multi)
# p2 <- .parse_container(a)
# p3 <- .parse_container(b)

# need parsers for shares, compatibility, and tags



# Testing -----------------------------------------------------------------
all_mg <- GS_ALL(filters = list(name = "KNOVEL Paying"))


#x <- full_multi
x <- all_mg

x_filt <- lapply(x, class)

# capture vector, pass on data.frame
x_vec       <- x[x_filt != "data.frame"]
x_container <- as.list(x[[c("definition", "container")]])

# handle x_vec, will always have values unless error
#dt.x_vec <- .h_lst_to_dt.l1(x_vec, rn = "field", hierarchy = 1L)
dt.x_vec <- do.call(cbind, x_vec) %>% t %>% as.data.table(., keep.rownames = "field")

out_dt.x_vec <- melt(dt.x_vec, id.vars = "field", variable.factor = FALSE)
out_dt.x_vec[, value := fix_blank(value)]

# Now handle things that need to be unnested

# container needs to be additionally separated
#x_container_only <- x_container[!names(x_container) == "rules"]
x_container_only <- .parse_container(x)
x_container_only <- do.call(cbind, x_container_only$container[, c("type", "operator")]) %>%
  t %>% as.data.table(., keep.rownames = "subfield")
# append field, which is container
x_container_only$field <- "container"
out_x_container_only <- melt(x_container_only, id.vars = c("field", "subfield"), variable.factor = FALSE)
out_x_container_only[, value := fix_blank(value)]

# Now handle rules
x_rules <- x_container[[c("rules")]] %>%
  Map(as.data.table, .)
x_rules <- lapply(x_rules, function(f) f[, "field" := "container"])
x_rules <- lapply(x_rules, function(f) f[, "subfield" := "rules"])
x_rules_melt <- lapply(x_rules, function(f) melt(f, id.vars = c("field", "subfield"), variable.name = "subfield_name", variable.factor = FALSE))
x_rules_melt <- rbindlist(x_rules_melt, use.names = TRUE, idcol = "variable")
x_rules_melt[, variable := paste0("V", variable)]

# Outputs
out <- rbindlist(list(out_dt.x_vec, out_x_container_only, x_rules_melt), use.names = TRUE, fill = TRUE) %>%
  setnames(., "variable", "idx") %>%
  .[, value := fix_blank(value)] %>%
  .[!is.na(value)] %>%
  melt(., id.vars = c("field", "subfield", "subfield_name", "idx"), measure.vars = list("value")) %>%
  .[, variable := NULL]

out_cast <- dcast(out, ... ~ idx, value.var = "value", fun.aggregate = function(x) paste(unique(x), collapse = ", "))

remelt <- melt(out_cast, measure.vars = patterns("V\\d+"), na.rm = TRUE) # this should be the melted output
recast <- dcast(remelt, ... ~ variable, value.var = "value") # just a check
####





# Testing output space ----------------------------------------------------




