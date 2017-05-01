# Integration script


# libs --------------------------------------------------------------------

library(magrittr)


# FUNS --------------------------------------------------------------------

add_extension <- function(x, extension = "R") {
  out <- rep(NA, length(x))
  
  has_ext <- grepl("(?<=\\.)((?i)[A-Z]+$)", x, perl = TRUE)

  out[has_ext] <- x[has_ext]
  out[!has_ext] <- paste0(x[!has_ext], ".", extension)
  
  return(out)
}


# check that all files exist
make_targ_paths <- function(dir, subdir, files, new_dir) {

  base_paths <- paste0(dir, subdir)
  full_paths <- Map(paste0, base_paths, files)
  paths_exist <- Map(file.exists, full_paths)
  
  check <- Map(all, paths_exist) 
  
  if(! all(unlist(check))) {
    stop("All files do not exist\n", 
         paste(check, collapse = "\n")
    )
  }
  
  new_base_paths <- paste0(new_dir, subdir)
  
  Map(dir.create, new_base_paths,
        showWarnings = FALSE, 
        recursive = TRUE)
  
  message("created \n", 
          paste(new_base_paths, collapse = "\n")
  )
  
  full_paths_new <- Map(paste0, new_base_paths, files)
  
  list(
    old_paths = full_paths, 
    new_paths = full_paths_new
  )
    
}



# Files required ----------------------------------------------------------

# Files in /R
r_files <- c(
  "Segments_Get", 
  "CalculatedMetrics_Get", 
  "call.Get_base", 
  "collapse_simple_target", 
  "collapse_simple_listcol", 
  "parse_shares", 
  "parse_segment_defn", 
  "flatten_nested_defn", 
  "split_segment_ret"
)

# Files in /tests/testthat
test_files <- c(
  "test-call.Get_base", 
  "test-collapse_simple", 
  "test-flatten_nested_defn", 
  "test-parse_shares"
)

# Files in tests/testdata
test_data <- c(
  "test_set_unnested.txt", 
  "nested_rule.txt", 
  "nested_container.txt", 
  "nested_stacked.txt", 
  "refOut_nested_rule.txt", 
  "refOut_nested_container.txt", 
  "refOut_nested_stacked.txt"
)


# Define dirs -------------------------------------------------------------

from_dir <- "../SCAdmin/"
to_dir   <- "../RSiteCatalyst_temp/"

targ_dirs <- c(
  r_files = "R/", 
  test_files = "tests/testthat/", 
  test_data = "tests/testdata/"
)


# Make full paths ---------------------------------------------------------

file_list <- Map(
  add_extension, 
  list(
    r_files = r_files, 
    test_files = test_files, 
    test_data = test_data
  )
)


file_copy_list <- make_targ_paths(from_dir, targ_dirs, file_list, new_dir = to_dir)


# Copy --------------------------------------------------------------------

Map(file.copy, 
    from = file_copy_list$old_paths, 
    to = file_copy_list$new_paths
)
