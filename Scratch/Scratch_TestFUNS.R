# draft restructuring fun
restr_segRules <- function(x) {
  
  x_filt <- lapply(x, class)
  
  # capture vector, pass on data.frame
  x_vec       <- x[x_filt != "data.frame"]
  # handle x_vec, will always have values unless error
  dt.x_vec <- do.call(cbind, x_vec) %>% t %>% as.data.table(., keep.rownames = "field")
  
  out_dt.x_vec <- melt(dt.x_vec, id.vars = "field", variable.factor = FALSE)
  out_dt.x_vec[, value := fix_blank(value)]
  
  out_x_container <- .parse_container(x)
  
  c(list(meta = out_dt.x_vec), out_x_container)
}

NULL
# helper one, extract container out of different structures
.extract_container <- function(x) {
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

# need parsers for shares, compatibility, and tags
NULL
# parse container, calls .extract_container
.parse_container <- function(x) {
  # first extract or verify required data structure
  x_container <- .extract_container(x)
  
  
  # handle non-rules metadata
  x_container_only <- t(do.call(cbind, 
                                x_container$container[, c("type", "operator")]
  )
  ) %>%
    as.data.table(., keep.rownames = "subfield")
  # append field, which is container
  x_container_only$field <- "container"
  # output
  out_x_container_only <- melt(x_container_only, 
                               id.vars = c("field","subfield"), 
                               variable.factor = FALSE
  )
  out_x_container_only[, value := fix_blank(value)]
  
  # handle rules
  x_rules <- x_container[[c("container", "rules")]] %>%
    Map(as.data.table, .) %>%
    lapply(X=., function(f) 
      f[, ":="(field = "container", subfield = "rules")]
    )
  
  x_rules_melt <- lapply(x_rules, function(f)
    melt(f, id.vars = c("field", "subfield"),
         variable.name = "subfield_name",
         variable.factor = FALSE)
  )
  
  out_x_rules <- rbindlist(x_rules_melt, use.names = TRUE, idcol = "variable")
  out_x_rules[, "variable" := paste0("V", get("variable"))]
  
  # out
  list(container = out_x_container_only, 
       rules = out_x_rules)
  
}