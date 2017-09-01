#' Flatten a data.frame with one or more nested data.frame columns
#'
#' Custom unnesting function that tries to preserve name uniqueness
#'
#' @import data.table
#'
#' @param df A data.frame or data.table.
#'
#' @return
#' If no nested columns are detected, the input is returned as-is.
#'
#' If nested columns are detected, a \code{data.table} with all nested
#' columns unnested.
#'
#' @details
#' This is an alternative to \code{\link[tidyr]{unnest}}, although
#' it is not meant to be a strict replacement. The key difference is that the original
#' input is returned as a single, flattened table (meaning any rows that must be repeated, are).
#' Furthermore, the field name of the (each) nested column is appended to the (each) flattened
#' field-set, to make it easier to distinguish between (and trace) parent-child relationships
#' by name.
#'
#' Finally, this function does not care about \code{NULL} element-containing nested lists, as it
#' uses indexing to extract nested rows on a per-row, per-column basis.
#'
#' @note
#' A common use-case is for Adobe Analytics Classification tables that contain child
#' relationships, which are returned as nested \code{data.frame}s.
#'
#' @export
#'
#' @examples
#' # First nested column
#' V2 <- list(
#'   data.frame(col1 = 1:5),
#'   data.frame(col1 = 2:3)
#' )
#' V2[3:10] <- list(NULL)
#'
#' # Second nested column
#' V3 <- list()
#' V3[1:2] <- list(NULL)
#' V3[[3]] <- data.frame(key2 = letters[1:5], stringsAsFactors = FALSE)
#' V3[4:8] <- list(NULL)
#' V3[[9]] <- data.frame(key1 = letters[1:2], stringsAsFactors = FALSE)
#' V3[10]  <- list(NULL)
#'
#' # Try it
#' flatten_nested_df(
#'   data.frame(
#'     V1 = LETTERS[1:10],
#'     V2 = cbind(V2),
#'     V3 = cbind(V3),
#'     V4 = letters[11:20]
#'   )
#' )
flatten_nested_df <- function(df) {
  
  # input must be a data.frame or data.table
  if(!is.data.frame(df)) {
    stop("Input must be a data.frame or data.table")
  }
  
  # identify columns that are list, possible early return
  is_listcol <- vapply(df, is.list, logical(1))
  if(!any(is_listcol)) {
    return(df)
  }
  listcols <- names(df)[is_listcol]
  
  # for each listcol, test if sub-elements contain data.frame
  test_df_col <- lapply(listcols, function(f)
    vapply(df[[f]], function(z) is.data.frame(z), logical(1))
  )
  names(test_df_col) <- listcols
  
  check_df_col <- vapply(test_df_col, any, logical(1))
  # Early return if there are list-columns, but none are data.frame
  if(all(!check_df_col)) {
    return(df)
  }
  
  # coerce to data.table if needed
  if(!is.data.table(df)) {
    invisible("data.frame input detected; coercing to data.table")
    df <- as.data.table(df)
  }
  is_df_col <- test_df_col[check_df_col]
  
  # for each listcol, and each df-containing element
  #  pull the rows from data.table input
  dt_meta <- df[, -c(get("listcols"))]
  meta_dt_sub <- lapply(is_df_col, function(f)
    dt_meta[f]
  ) # append this back, later
  names(meta_dt_sub) <- listcols
  
  # for each listcol, extract out what you need to rbindlist
  dt_restr <- df[, c(get("listcols"))]
  
  restr_dt_sub <- Map("[", dt_restr, is_df_col)
  restr_dt_sub <- lapply(restr_dt_sub, function(f)
    Map(as.data.table, f)
  )
  
  # set names before cbinding
  old_nms <- lapply(restr_dt_sub, function(f)
    Map(names, f)
  )
  for(i in seq_along(old_nms)) {
    nm <- names(old_nms[i])
    old_nms[[i]] <- Map(paste0, nm, ".", old_nms[[i]])
    names(old_nms[[i]]) <- NULL
  }
  
  # set with a simple loop, assuming structures are compatible
  for(i in seq_along(restr_dt_sub)) {
    Map(setnames, restr_dt_sub[[i]], old_nms[[i]])
  }
  
  # ugly but working nested loop
  out <- vector("list", length(meta_dt_sub))
  for(i in seq_along(meta_dt_sub)) {
    for(j in seq_len(nrow(meta_dt_sub[[i]]))) {
      out[[i]][[j]] <- cbind(meta_dt_sub[[i]][j, ], restr_dt_sub[[i]][[j]])
    }
  }
  
  out_rbind <- lapply(out, function(f) rbindlist(f, use.names = TRUE, fill = TRUE))
  names(out_rbind) <- listcols
  
  out_dt <- rbindlist(out_rbind, use.names =TRUE, fill = TRUE)
  
  out_dt[dt_meta, on = c(names(dt_meta))]
  
}
