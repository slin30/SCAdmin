#' Write out a SAINT import template
#' 
#' For a valid call return from GetTemplate, write out a valid text file for downstream use
#' 
#' @family Classifications methods
#'
#' @param x (Required) A length-1 character containing the template to write out. See details
#' @param outfile (Optional) A length-1 file path; if not specified, will write out to a temporary file
#' @param wb (Optional) Use Windows Binary line termination format? Defaults to \code{TRUE}. 
#'
#' @return
#' A tab-delimited file in the location specified by \emph{outfile}, or if not specified, a temporary
#' file within a temporary directory. Additionally, a console \emph{message} denoting the file name and
#' directory.
#' 
#' @details 
#' This function will operate on a single input and, if provided, write the input to the location
#' specified by \emph{outfile}. For writing out more than one template, call this with e.g. \code{Map} and 
#' pass in lists of templates and (optionally) \emph{outfile}s-- of equal length.
#' 
#' The expected typical input is a valid return from \code{\link{Classifications.GetTemplate}}, passed in
#' row-wise if >1 row. Alternatively, call this function with a call to \code{Map} on the \emph{template}
#' field. See examples
#' 
#' @export
#'
#' @examples
#' \dontrun{
#' # On a single-row return
#' # First get a template
#' single_template <- GetTemplate("my_rsid", "my_element")
#' # Then write it out
#' WriteTemplate(single_template)
#' 
#' # On a multi-row return
#' multi_template <- GetTemplate(c("my_rsid_1", "my_rsid_2"), "my_element")
#' my_files <- c("./rsid_1.tab", "./rsid_2") # file extension will automatically be stripped and output as '.tab'
#' Map(WriteTemplate, multi_template[["template"]], my_files)
#' }
WriteTemplate <- function(x, outfile = NULL, wb = TRUE) {
  
  if(is.data.frame(x)) {
    if("template" %in% names(x)) {
      to_write <- x[["template"]]
    } else {
      stop("Provided input as a data.frame but expected name of 'template' was not found")
    }
  } else {
    to_write <- x
  }
  
  if(!is.character(to_write) || length(to_write) != 1L) {
    stop(
      paste0("x must be provided as a length 1 character or ",
             "extractable from a single-row data.frame"
      )
    )
  }
  
  chk <- grepl("SiteCatalyst SAINT Import File", to_write)
  if(!chk) {
    stop("Expected string denoting a valid SAINT Import File was not found")
  }
  
  if(is.null(outfile)) {
    outfile <- tempfile(fileext = ".tab", pattern = "")
  }
  
  # get the name without extension, reappend the extension to base
  base_fname <- paste0(
    sub(pattern = "(.*)\\..*$", replacement = "\\1", basename(outfile)), 
    ".tab"
  )
  rem_fpath  <- dirname(outfile)
  out_fname  <- paste0(rem_fpath, "/", base_fname)
  
  # open the connection, close on exit
  if(wb) {
    output.file <- file(out_fname, open = "wb")
  } else {
    output.file <- file(out_fname)
  }
  
  on.exit(close(output.file))
  cat(to_write, file = output.file)
  
  fname_msg <- gsub("\\\\", "/", rem_fpath)
  message(base_fname, " written to ", fname_msg, "/")
  
}