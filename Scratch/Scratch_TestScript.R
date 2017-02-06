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
  call.Get_Segments(fields = c("tags", "shares",
                          "description", "owner",
                          "modified", "compatibility",
                          "favorite", "reportSuiteID",
                          "definition"),  
               ...)
}

# splitter for get_segments return
.split_segment_ret <- function(seg_ret) {
  # check class
  if(!is.list(seg_ret)) {
    stop("seg_ret is of class ", 
         class(seg_ret), " but expected a data.frame"
    )
  }
  # get id
  id <- seg_ret[["id"]]
  # check row and id match
  if(nrow(seg_ret) != length(id)) {
    stop("There are ", nrow(seg_ret), 
         " but only ", length(id), " ids"
    )
  }
  
  out <- lapply(seq_along(id), function(f) 
    seg_ret[f, ])  
  names(out) <- id
  
  return(out)
}


# easy-mode get all; template of sorts, before packaging
easy.Get_Segments <- function(..., fun = NULL) {
  
  if(is.null(fun)) {
    fun <- "GS_ALL"
  }

  seg_call  <- match.fun(fun)(...)
  restr_call <- .split_segment_ret(seg_call) %>%
    Map(restr.Get_Segments, ., merge_rules = TRUE) %>%
    purrr::transpose(.)
  
  lapply(restr_call, function(f) 
    rbindlist(f, use.names = TRUE, fill = TRUE))
}

# Errors in input testing -------------------------------------------------

call.Get_Segments(filters = c("A", "a"))
call.Get_Segments(filters = list("A", "B"))

call.Get_Segments(fields = "All")
call.Get_Segments(accessLevel = "A")
call.Get_Segments(sort = 1:2)
call.Get_Segments(sort = "A")
call.Get_Segments(filters = list(name = "A", alt="B"))



# Initial calls for next step testing -------------------------------------


full_single <- easy.Get_Segments(filters = list(name = "aarti"), 
                                 fields = c("definition", "description", 
                                            "owner", "modified", 
                                            "reportSuiteID"), 
                                 accessLevel = "owned", 
                                 fun = "call.Get_Segments"
)
full_multi <- easy.Get_Segments(filters = list(name = "aarti"), 
                                fields = c("definition", "description",
                                           "owner", "modified", 
                                           "reportSuiteID"), 
                                accessLevel = "all", 
                                fun = "call.Get_Segments"
)
MG_all <- easy.Get_Segments(filters = list(name = "Knovel"))
WZ_all <- easy.Get_Segments() # test all mine; there are 34, 3 are problematic

WZ_out <- merge(WZ_all$segment_meta, WZ_all$defn, by = c("segment_id"))

WZ_call <- GS_ALL()
WZ_call_split <- .split_segment_ret(WZ_call)

# this one part of the stacked segment. Not created properly
# since is unnecessarily nested, but perhaps a good test case
type_1 <- WZ_call_split$s300000520_57e0327ae4b007430cbdcdc0 
# this is the stacked segment
type_2 <- WZ_call_split$s300000520_57e0343be4b007430cbdcdc3 
# this is a pretty terribly created (TLV) nested
# unnecessarily as well.
type_3 <- WZ_call_split$s300000520_582ca0d2e4b0a4d9dc2936ac 


# Make segments testing ---------------------------------------------------

# this is a segment that contains two rules with exclude

mg_exclude <- call.Get_Segments(filters = list(name = "Knovel exclude toc"),
                                fields = c("description", "definition", "owner", "modified", 
                                           "reportSuiteID"), 
                                accessLevel = "all")

mg_dt <- restr.Get_Segments(mg_exclude)

# to recreate this
seg_meta <- make_segment_meta(name = mg_exclude$name, 
                              reportSuiteID = mg_exclude$reportSuiteID, 
                              owner = "w.zhang")

rule_vec <- mg_exclude$definition$container$rules[[1]]$value

# now, note that there are two elements, two operators
# if there is more than one of either element or operator in a rule, handle it

# check if any of element, operator, or rules is > 1L
# use the temporary alternative function
seg_rules <- make_element_rules(element = mg_dt$defn$element, 
                                rules = mg_dt$defn$rules, 
                                operator = mg_dt$defn$operator)


seg_container <- make_segment_container(type = "hits", operator = "or", exclude = TRUE, rules = seg_rules)

seg_body <- make_segment_body(segment_container = seg_container, segment_meta = seg_meta)




# More nested container parsing -------------------------------------------

## With a bit more insight into how nested containers can be built, properly, take another
## stab at parsing them


# Here are some properly built nested segments, as a first test case:
seg_test <- call.Get_Segments(accessLevel = "all", filters = list(name = "Databases all"), 
                               fields = c("tags", "shares",
                                          "description", "owner",
                                          "modified", "compatibility",
                                          "favorite", "reportSuiteID",
                                          "definition")
                               )

# Let's grab only a sinle segment
test_split <- .split_segment_ret(seg_test)
single_test <- test_split[[1]]

## Reference set-- an unnested segment for comparison
seg_ref <- call.Get_Segments(accessLevel = "owned", filters = list(name = "ThermoPhysProd"), 
                              fields = c("tags", "shares",
                                         "description", "owner",
                                         "modified", "compatibility",
                                         "favorite", "reportSuiteID",
                                         "definition")
)

ref_split <- .split_segment_ret(seg_ref)
single_ref <- ref_split[[1]]

### Focus on rules

# first, simply split into definition and not definition
split_seg_return <- function(x) {
  if(!"definition" %in% names(x)) {
    stop("definition missing")
  }
  
  seg_meta <- setdiff(names(x), "definition")
  seg_def <- c("definition")
  
  
  
  list(segment_meta = x[seg_meta], 
       segment_def = x[[seg_def]]
       )
}

p1.ref <- split_seg_return(single_ref)
p1.tst <- split_seg_return(single_test)




# for now, input the "segment_def" part of split_set_ret
parse_nested_container <- function(x, lst = c(), d = 0L) {
  
  if("segment_def" %in% names(x)) {
    x <- x[["segment_def"]]
  }
  
  is_nested <- "container" %in% names(x[[c("container", "rules")]][[1]])

  if(!is_nested) {
    message("at iteration ", d, ", x is no longer nested")
    
    x.tmp <- x[["container"]]
    
    if(d == 0L) { # then was not originally nested
      lst <- c(lst, 
               list(cont_meta = x.tmp[setdiff(names(x.tmp), "rules")],
                    cont_rule = x.tmp[["rules"]]
               )
      )
    } else { # then was originally nested, so all we are changing is names
      lst <- c(lst, 
               list(sub_cont_meta = x.tmp[setdiff(names(x.tmp), "rules")],
                    sub_cont_rule = x.tmp[["rules"]]
               )
      )
    }
    
    return(lst)
    
  } else {
    x.tmp      <- x[["container"]]
    x.tmp_nest <- x.tmp[["rules"]][[1]]
    
    lst <- c(lst, 
             list(cont_meta = x.tmp[setdiff(names(x.tmp), "rules")]
                  #cont_rule = x.tmp_nest # I think this is actually redundant for natively recursive inputs...
             )
    )
    parse_nested_container(x = x.tmp_nest, lst = lst, d = d+1L)
    
  }
  
}

parse_p1.ref <- parse_nested_container(p1.ref)
parse_p1.tst <- parse_nested_container(p1.tst) 


# Stringing these two functions together: 

parse_seg_return <- function(x) {
  splitted = split_seg_return(x)
  
  cont_parsed <- parse_nested_container(x = splitted)
  
  c(splitted[1], cont_parsed)
}


ref_parsed <- parse_seg_return(single_ref)
tst_parsed <- parse_seg_return(single_test)

