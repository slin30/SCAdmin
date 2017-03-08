#' CalculatedMetrics.Get
#' 
#' Call the Adobe Analytics 1.4 CalculatedMetrics.Get method
#' 
#' @importFrom RSiteCatalyst ApiRequest
#' @importFrom jsonlite unbox toJSON
#' 
#' @export
#' 
#' @inherit call.Get_base
#' @inheritSection call.Get_base Access Privileges
#' 
#' @section Definition Parsing:
#' This is not yet ready for Calculated Metrics. 
#' 
#' @section API Method Info:
#' This function calls the Adobe Analytics 1.4
#' \href{https://marketing.adobe.com/developer/documentation/segments-1-4/calculated-metrics}{CalculatedMetrics.Get}
#' method, which supercedes the deprecated \emph{ReportSuite.GetCalculatedMetrics} method. 
#' The \emph{CalculatedMetrics.Get} method, and therefore this function, 
#' allows essentially all available information about one or more calculated metrics to be returned.
#' 
#' As such, it is now possible to download one or more complete calculated metric definitions, which
#' may be useful for batch auditing, back-up, and much more. Note, though, that \emph{CalculatedMetrics.Get}
#' operates at the calculated metrics ownership level, as opposed to the reportsuite ID level, 
#' which means this is not a strict replacement for the (deprecated) \emph{ReportSuite.GetCalculatedMetrics} method.
#' 
#' @examples 
#' \dontrun{
#' # Get all calculated metrics you own
#' CalculatedMetrics.Get()
#' 
#' # Get all calculated metrics that are shared with you
#' CalculatedMetrics.Get(accessLevel = "shared")
#' 
#' # Get all calculated metrics, period
#' # Note this requires admin privileges
#' CalculatedMetrics.Get(accessLevel = "all")
#' 
#' # Parsing is needed for certain fields, in particular 'definition'
#' # This returns some nested fields, but tags and compatibility are collapsed automatically...
#' needs_parsing_1 <- CalculatedMetrics.Get(fields = c("tags", "shares", "compatibility"))
#' # ...unless you request otherwise
#' needs_parsing_1_alt <- CalculatedMetrics.Get(fields = c("tags", "shares", "compatibility"), 
#'                                          collapse_simple = FALSE
#' )
#' 
#' # `definition` is the most complex
#' needs_parsing_2 <- CalculatedMetrics.Get(fields = c("definition"))
#' 
#' # Here's what it looks like if we ask for all fields
#' needs_parsing_3 <- CalculatedMetrics.Get(fields = c("compatibility", "definition", 
#'                                                 "favorite", "modified", 
#'                                                 "owner", "reportSuiteID", 
#'                                                 "shares", "tags")
#' )
#' 
#' }
CalculatedMetrics.Get <- function(accessLevel = NULL, fields = NULL, 
                                  selected = NULL, sort = NULL, 
                                  filters = NULL, 
                                  collapse_simple = TRUE) {
  
  call.Get_base(accessLevel = accessLevel, fields = fields, 
                selected = selected, sort = sort, 
                filters = filters, 
                collapse_simple = collapse_simple,
                func.name = "CalculatedMetrics.Get")
  
}