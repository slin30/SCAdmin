## In progress parsers for recursive containers

# base 
.nul_to_DT <- function(x, default = "name") {
  if(is.null(x)) {
    out <- data.table(NA_character_)
    setnames(out, default)
    return(out)
  } else {
    return(x)
  }
}

.df_nul_to_DT <- function(x, autoID = TRUE) {
  
  autoID_nm <- "rowid"
  
  targ_class <- vapply(x, class, FUN.VALUE = character(1))
  
  listcols <- names(targ_class)[targ_class %in% c("list", "data.frame")]
  vectcols <- setdiff(names(targ_class), listcols)
  
  sub_x <- x[, listcols]
  sub_x <- lapply(sub_x, function(f) .nul_to_DT(f))
  fixed <- rbindlist(sub_x, use.names = TRUE, fill = TRUE, idcol = paste0(autoID_nm))
  fixed[[autoID_nm]] <- as.integer(fixed[[autoID_nm]])
  
  # make non list cols dt, setnames
  vect_dt <- as.data.table(x[, vectcols])
  setnames(vect_dt, vectcols)
  if(autoID) {
    vect_dt[, c(autoID_nm) := .I]
    out <- merge(vect_dt, fixed, by = get("autoID_nm"))
  } else {
    out <- cbind(fixed, vect_dt)
  }
  
}

fix_container <- function(x, autoID = TRUE) {
  
  autoID_nm <- "rowid"
  
  # split out, looking for class of data.frame
  targ_class <- vapply(x, class, FUN.VALUE = character(1))
  
  dfcols   <- names(targ_class)[targ_class %in% c("data.frame")]
  vectcols <- setdiff(names(targ_class), dfcols)
  
  # pull out what needs to be fixed, fix it
  to_fix <- x[, dfcols]
  fixed  <- .df_nul_to_DT(to_fix)
  
  # make non list cols dt, setnames
  vect_dt <- as.data.table(x[, vectcols])
  setnames(vect_dt, vectcols)
  if(autoID) {
    vect_dt[, c(autoID_nm) := .I]
  }
  
  list(x = vect_dt, 
       y = fixed)
}

# expect input from fix_container
collapse_container <- function(x) {
 
  # make sure input is correct
  nms_chk <- names(x)
  if(! all(nms_chk %in%  c("x", "y")) && length(nms_chk) == 2L) {
    stop("required input structure (names) not present")
  }
  
  shared_nms <- do.call(intersect, Map(names, x))
  
  shared_elems <- lapply(x, function(f) f[, c(shared_nms), with = FALSE]) %>%
    lapply(X=., function(f) as.list(f)) %>%
    Filter(function(x) length(x) > 0L, .) %>%
    Reduce(function(a, b) ifelse(is.na(a), b, a), .) %>%
    as.data.table
  
  own_elems    <- lapply(x, function(f) f[, c(setdiff(names(f), shared_nms)), with = FALSE]) %>%
    lapply(X=., function(f) as.list(f)) %>%
    Filter(function(x) length(x) > 0L, .) %>%
    Map(as.data.table, .) %>%
    rbindlist(., use.names = TRUE, fill = TRUE)
  
  cbind(shared_elems, own_elems)
}