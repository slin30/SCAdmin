library(magrittr)
library(RSiteCatalyst)
library(data.table)
library(jsonlite)
library(SCAdmin)

SCAuth(key = Sys.getenv("wz_sc_id"), Sys.getenv("wz_sc_pw"))


# FUNS --------------------------------------------------------------------


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


# full_single <- easy.Get_Segments(filters = list(name = "aarti"), 
#                                  fields = c("definition", "description", 
#                                             "owner", "modified", 
#                                             "reportSuiteID"), 
#                                  accessLevel = "owned", 
#                                  fun = "call.Get_Segments"
# )
# full_multi <- easy.Get_Segments(filters = list(name = "aarti"), 
#                                 fields = c("definition", "description",
#                                            "owner", "modified", 
#                                            "reportSuiteID"), 
#                                 accessLevel = "all", 
#                                 fun = "call.Get_Segments"
# )
# MG_all <- easy.Get_Segments(filters = list(name = "Knovel"))
# WZ_all <- easy.Get_Segments() # test all mine; there are 34, 3 are problematic
# 
# WZ_out <- merge(WZ_all$segment_meta, WZ_all$defn, by = c("segment_id"))

WZ_call <- GS_ALL()
WZ_call_split <- SCAdmin:::split_segment_ret(WZ_call)

# this one part of the stacked segment. Not created properly
# since is unnecessarily nested, but perhaps a good test case
# this is the stacked segment
bad_wz_stacked <- WZ_call_split$s300000520_57e0343be4b007430cbdcdc3 
# this is a pretty terribly created (TLV) nested
# unnecessarily as well.
bad_wz_nested <- WZ_call_split$s300000520_582ca0d2e4b0a4d9dc2936ac 


# More nested container parsing -------------------------------------------

## With a bit more insight into how nested containers can be built, properly, take another
## stab at parsing them


# Here are some properly built nested segments, as a first test case:
seg_ret.tst <- call.Get_Segments(accessLevel = "all", filters = list(name = "DB Session", 
                                                                  owner = "m.gray"), 
                              fields = c("tags", "shares",
                                         "description", "owner",
                                         "modified", "compatibility",
                                         "favorite", "reportSuiteID",
                                         "definition")
)

# Let's grab only a sinle segment
split_tst_ret <- SCAdmin:::split_segment_ret(seg_ret.tst)

## Reference set-- an unnested segment for comparison
seg_ret.ref <- call.Get_Segments(accessLevel = "owned", filters = list(name = "ThermoPhys"), 
                             fields = c("tags", "shares",
                                        "description", "owner",
                                        "modified", "compatibility",
                                        "favorite", "reportSuiteID",
                                        "definition")
)

split_ref_ret <- SCAdmin:::split_segment_ret(seg_ret.ref)


# Flatten with wrapper FUN
flat_ref_ret <- flatten_container(split_ref_ret$s300000520_589a29f3e4b0cfc8b41c8978)
flat_tst_ret <- flatten_container(split_tst_ret$s300000520_589a1638e4b0cfc8b41c8960)

# test WZ; note that type_2 needs tryCatch to handle better, exclude for now to test
# other downstream functions first, come back to it.
flat_wz <- lapply(list(bad_stacked = bad_wz_stacked, bad_nested = bad_wz_nested), 
                  function(f) SCAdmin:::split_segment_ret(f)
) %>%
  lapply(X=., function(f) flatten_container(f[[1]]))


# all_test_parsed <- lapply(test_split, parse_seg_return)



# Stringing all together --------------------------------------------------


# if cont_rule is in names, then rbindlist it
# helper to .split_flat_cont
.l_helper_bind_rules <- function(x) {
  
  if(!"cont_meta" %in% names(x)) {
    stop("content_meta missing")
  }
  
  if(!"cont_rule" %in% names(x)) {
    return(x)
  }
  
  # handle inputs where cont_meta is present along with cont_rule

  cont_meta <- x[["cont_meta"]]
  cont_rule <- x[["cont_rule"]]

  
  # check that all elements are df
  df_check <- vapply(seq_along(cont_rule), 
                     function(f) is.data.frame(cont_rule[[f]]), FUN.VALUE = logical(1))
  if(!all(df_check)) {
    stop(
      "All elements of x are not of class data.frame"
    )
  }
  
  # process as DT, make keys to merge
  out_rules <- rbindlist(cont_rule, 
                         use.names = TRUE, 
                         fill = TRUE, 
                         idcol = "rule_set_ID"
  )
  out_meta  <- as.data.table(cont_meta)
  out_meta[, rule_set_ID := .I]
  
  list(
    cont_meta = out_meta, 
    cont_rule = out_rules
  )
  
}

# split a parsed return from parse_seg_return, remove NULLs, bind rules
bind_flat_cont <- function(x) {
  targ <- "segment_meta"
  
  if(!targ %in% names(x)) {
    stop("Required input element of ", substitute(targ), " not present")
  }
  
  segment_meta <- x[[targ]]
  segment_cont <- x[setdiff(names(x), substitute(targ))] %>%
    lapply(X=., function(f) Filter(function(x) !is.null(x), f)) 
  
  # make sure other names have not crept into segment_cont
  segCont_nms <- paste0("L", seq_along(segment_cont)-1L)
  if(length(setdiff(names(segment_cont), segCont_nms)) > 0L) {
    stop("Mismatch in names in segment_cont and expected names")
  }
  
  segment_cont <- lapply(segment_cont, .l_helper_bind_rules)

  list(segment_meta = segment_meta, 
       segment_cont = segment_cont
       )
}

bound_ref_ret <- bind_flat_cont(flat_ref_ret)
bound_tst_ret <- bind_flat_cont(flat_tst_ret)

View(bound_tst_ret$segment_cont$L1$cont_rule)

collapse_simple_target(bound_ref_ret$segment_meta, "tags") %>% print
collapse_simple_target(bound_tst_ret$segment_meta, "tags") %>% print


collapse_simple_target(bound_ref_ret$segment_meta, "compatibility") %>% print
collapse_simple_target(bound_tst_ret$segment_meta, "compatibility") %>% print

collapse_simple_target(bound_ref_ret$segment_cont$L0$cont_meta, "tags") # error, good
collapse_simple_target(bound_ref_ret$segment_cont$L0$cont_meta, "compatibility") # error, good

wz_simple <- call.Get_Segments(fields = c("tags", "shares",
                                          "description", "owner",
                                          "modified", "compatibility",
                                          "favorite", "reportSuiteID")
)
collapse_simple_target(wz_simple, "tags")
collapse_simple_target(wz_simple, "compatibility")

wz_simple$compatibility <- collapse_simple_target(wz_simple, "compatibility")
wz_simple$tags <- collapse_simple_target(wz_simple, "tags")

# here is a simple way to unnest-- or could just return another data.frame altogether
# for shares, and return a list of data.frames instead? Do not need to use 
# tidyr in this case.
shares_unnest <- tidyr::unnest(wz_simple, shares)

# Handle shares testing

wz_shares <- wz_simple$shares
collapse_shares(wz_simple)
collapse_shares(bound_tst_ret$segment_meta)

# tst <- Map(collapse_shares, WZ_call_split)
# rbindlist(tst[!is.na(tst)])

mg_exclude <- GS_ALL(accessLevel = "all", 
                     filters = list(name = "exclude internal knovel", 
                                    owner = "m.gray")
                     )


collapse_shares(mg_exclude)

# A fully-parsed segment return, not including definition
wz_simple_parse <- list(segment_meta = wz_simple[!names(wz_simple) == "shares"],
                        shares = collapse_shares(wz_simple)
                        )
wz_simple_parse
View(wz_simple_parse$segment_meta)
View(wz_simple_parse$shares)
# Note: With updated segment, fixing original db_like, this 
# actually works for stacked segments, so change the error handling as it does 
# not apply-- rather, put generic handler for poorly made
# segments, and consider throwing error on things that are just plain
# wrong, or simply returning what you can.

# Note that stacked segments different order for the element of 'operator'
# within 'container'. Check if this is a notable/specific characteristic
bound_wz_stacked <- bind_flat_cont(flat_wz$bad_stacked)

bound_wz_nested <- bind_flat_cont(flat_wz$bad_nested) # still errors, so handle it

# mgDB <- GS_ALL(accessLevel = "all", filters = list(owner = "m.gray", name = "databases all"))
# mgDB_restr <- .split_segment_ret(mgDB) %>% 
#   Map(flatten_container, .) %>% 
#   Map(bind_flat_cont, .)



# For DAF -----------------------------------------------------------------
# # Daf segments
# 
# dseg <- call.Get_Segments(accessLevel = "all", 
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

mg_exclude <- call.Get_Segments(selected = "s300000520_589774a4e4b08939f9d6e818",
                                fields = c("description", "definition", "owner", "modified", 
                                           "reportSuiteID"), 
                                accessLevel = "all")
mg_exclude.parse <- flatten_container(mg_exclude)

# to recreate this

## and to edit; get the ID first
call.Get_Segments(filters = list(name = "ThermoPhysDyn_ContentName"))
#s300000520_589a29f3e4b0cfc8b41c8978 is the ID for edit

seg_meta <- make_segment_meta(name = "ThermoPhysDyn_ContentName", 
                              reportSuiteID = mg_exclude$reportSuiteID, 
                              owner = "w.zhang", 
                              tags = "SA_CNAME"
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
#my_new_seg <- Save_Segment(seg_body)
#my_edit_seg <- Save_Segment(seg_body, override_and_edit = TRUE)

call.Get_Segments(filters = list(tags = "SA_CNAME"))
