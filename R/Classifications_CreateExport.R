#' Create an export job for SAINT classifications
#' 
#' Instantiate an export request to pull classified values from one or more report suites
#' 
#' @importFrom RSiteCatalyst ApiRequest
#' @importFrom jsonlite unbox toJSON
#' 
#' @inheritSection call.Get_base Access Privileges
#' @family Classifications methods
#' 
#'
#' @param date_filter_start_date (required) The start month and year. See Notes for possible format restrictions.
#' @param date_filter_end_date (required) The end month and year. See Notes for possible format restrictions.
#' @param element (required) The element id. Must be a valid (Classification-enabled) element within the scope of the
#'        report suite(s).
#' @param rsid_list (required) One or more report suite id(s). This is the only parameter that accepts vectors of length >1.
#'        May be supplied as an atomic vector or list, and if the latter, will be flattned.
#' @param email_address (required) A valid email address to send the processing status notification to. 
#' @param row_count (optional) The maximum number of rows to return. Overridden if \emph{all_rows} is \code{TRUE}, which is the 
#'        default for that parameter. Defaults to \code{1000} (this is enforced by the server)
#' @param encoding (optional) File encoding. Likely restricted to certain allowed values, and defaults to \code{UTF-8}
#' @param all_rows (optional) Return all available rows? Overrides \emph{row_count} if set to \code{TRUE}, which is the default.
#'        Setting this to \code{FALSE} and failing to provide a value to \emph{row_count} restricts the number of rows to the 
#'        server default of \code{1000}.
#' @param quote_output (optional) Should outputs be quoted? Defaults to \code{TRUE}
#' @param row_filter_column_name (optional) The classification field name to filter by. Does nothing if a valid value is not passed
#'        to \emph{row_filter_column_value}
#' @param row_filter_column_value (optional) The value to filter by, based on the \emph{row_filter_column_name}.
#' @param row_filter_empty_column (optional) Can be set to one of a set of possible values. See Details.
#' @param campaign_filter_end_date (optional) The campaign start date. Off by default. See Details for allowed values.
#' @param campaign_filter_start_date (optional) The campaign end date.
#'
#' @details 
#' \href{https://marketing.adobe.com/developer/documentation/classifications-1-4-saint/r-createexport}{Adobe 1.4 documentation}  
#' 
#' This function is the first step when you wish to download classified data, 
#' provided you know the element (id) and one or more valid report suites. The
#' functionality replicates the browser export option in the UI. 
#' 
#' The call accepts arguments of length 1 for all parameters except for 
#' \emph{rsid_list}. Furthermore, the endpoint expects empty values (but still keys)
#' for all parameters, even though not all parameters are actually required. As a 
#' result, this function ensures that the minimally required parameters are supplied,
#' supplies server defaults where relevant, and passes blank values for optional 
#' parameters. 
#' 
#' Note that the \emph{row_filter} parameters have some nuances in the values and
#' combinations they expect. See examples for the most common use case (returning
#' only non-null rows based on a known classification field). See \code{\link{Classifications_GetFilters}}
#' for the method to figure out what possible filter values are.
#' 
#' Per the documentation, \emph{row_filter_empty_column} can take one of the following:
#' 
#' \itemize{
#' \item \code{::all::} - Returns rows that have all columns empty.
#' \item \code{::any::} - Returns rows that have 1 or more empty column.
#' \item Any column name - Returns rows where the given column is empty.
#' }
#' 
#' 
#' Per the documentation, \emph{campaign_filter_end_date} can take one of the following:
#' 
#' \itemize{
#' \item \code{::all::}
#' \item \code{::active::}
#' }
#' 
#' ...to filter by all campaigns, or by only active ones.
#' 
#' @note 
#' Though not specified in the documentation, it appears that the string format for 
#' date filters is expected to be a three-letter month abbreviation followed by the four-digit year. 
#' In other words, \emph{Jun 2017} as opposed to \emph{June 2017}. Per Adobe 1.4 documentation, 
#' other allowable formats include any parsable by \code{php:date()}. 
#' This includes e.g. \code{YYYY-MM-DD}. 
#' 
#' @return
#' A \code{list} of length 1, containing the \emph{job_id} \code{integer}.
#' @export
#'
#' @examples
#' \dontrun{
#' # A basic request
#' Classifications_CreateExport(
#'     date_filter_start_date = "Jan 2017", 
#'     date_filter_end_date = "Nov 2017", 
#'     element = "evar16", 
#'     email_address = "my_email@xyz.com", 
#'     rsid_list = list("my_rsid_1")
#'     )
#'     
#' # Using row_filter to only return non-null rows based on that column
#' Classifications_CreateExport(
#'     date_filter_start_date = "Jan 2017", 
#'     date_filter_end_date = "Nov 2017", 
#'     element = "evar16", 
#'     email_address = "my_email@xyz.com", 
#'     rsid_list = list("my_rsid_1"), 
#'     row_filter_column_name = "some_valid_classification_field_name", 
#'     row_filter_column_value = ".*",
#'     all_rows = TRUE
#'     )
#'     
#'# Also limit to only 100 rows max; must set all_rows to FALSE
#' Classifications_CreateExport(
#'     date_filter_start_date = "Jan 2017", 
#'     date_filter_end_date = "Nov 2017", 
#'     element = "evar16", 
#'     email_address = "my_email@xyz.com", 
#'     rsid_list = list("my_rsid_1"), 
#'     row_filter_column_name = "some_valid_classification_field_name", 
#'     row_filter_column_value = ".*",
#'     all_rows = FALSE, 
#'     row_count = 100
#'     )
#' }
Classifications_CreateExport <- function(
  date_filter_start_date = NULL, 
  date_filter_end_date = NULL, 
  element = NULL, 
  rsid_list = NULL, 
  email_address = NULL, 
  row_count = "", 
  encoding = "UTF-8", 
  all_rows = TRUE, 
  quote_output = TRUE,
  row_filter_column_name = "", 
  row_filter_column_value = "", 
  row_filter_empty_column = "", 
  campaign_filter_end_date = "", 
  campaign_filter_start_date = "" 
) {
  
  # required args
  req_args <- c(
    "date_filter_start_date", 
    "date_filter_end_date", 
    "element", 
    "rsid_list", 
    "email_address"
  )
  # check that required are present
  args_req <- mget(req_args)
  check_req <- setdiff(req_args, names(Filter(function(x) !is.null(x), args_req)))
  if(length(check_req) > 0L) {
    stop("The following required args are NULL:\n\t{", 
         paste(check_req, collapse = ", "), 
         "}"
    )
  }
  
  # all args are scalar, except for rsid_list
  args_all <- mget(names(formals(Classifications_CreateExport)))
  args_all.present <- Filter(function(x) !is.null(x), args_all)
  
  # define non-scalar args
  not_scalar <- c("rsid_list") # only one for now; add here if others in future
  
  # split out scalar and array
  args_scalar <- args_all.present[setdiff(names(args_all.present), not_scalar)]
  args_array  <- args_all.present[not_scalar]
  
  # check args_scalar ahead of time; more helpful error than unbox default
  check_args_scalar <- vapply(args_scalar, length, integer(1))
  if(any(check_args_scalar > 1L)) {
    stop("Only vectors of length 1 are allowed for:\n\t{",
         paste(
           names(check_args_scalar[check_args_scalar > 1]), 
           collapse = ","
         ), 
         "}"
    )
  }
  
  # assemble body
  body.scalar <- Map(unbox, args_scalar)
  body.array  <- lapply(args_array, function(f) c(f, recursive = TRUE))
  body <- c(body.scalar, body.array)
  
  # final check for NA values
  check_NA <- vapply(body, anyNA, logical(1))
  if(any(check_NA)) {
    stop("NA values found in:\n\t{",
         paste(names(which(check_NA)), collapse = ","), 
         "}"
    )
  }
  
  # call
  ApiRequest(body = toJSON(body), func.name = "Classifications.CreateExport")
}