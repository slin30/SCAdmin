% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/make_element_rules.R
\name{make_element_rules}
\alias{make_element_rules}
\title{Expand a vector of rules into scalar list-sets}
\usage{
make_element_rules(element, operator, rules, trim = FALSE, dedupe = FALSE,
  how = NULL)
}
\arguments{
\item{element}{A character vector. Must be a valid element (id) within access rights of your account}

\item{operator}{A character vector. For elements-based rules, the API defines allowable values. See details.}

\item{rules}{A character vector; if not, will be coerced via \code{as.character}}

\item{trim}{logi. Should leading and trailing whitespace(s) be removed? Defaults to \code{FALSE}}

\item{dedupe}{logi. Should input \emph{rules} be de-duplicated? Defaults to \code{FALSE}}

\item{how}{Not used at the moment.}
}
\value{
A list of \code{length(rules)}. Will be hte length after whitespace-trimming and/or de-duplication
if \emph{trim} and/or \emph{dedupe} are \code{TRUE}, and if so, actions are performed by 
\code{str_trim} and \code{unique}, respectively.
}
\description{
For the most common single element, single operator, many rules use case
}
\note{
This now accepts inputs to  \emph{element} and \emph{operator} of length > 1. Checks are in place to 
prevent ambiguous combinatons of lengths for these two arguments in conjunction with \emph{rules}. This is
done to take advantage of standard recycling rules, while constraining the possible combinations of 
anticipated use cases. 

Anticipating all possible scenarios is impossible, so if you find any unexpected or overly-restrictive
behavior, please file an issue.
}
\examples{
#TBD
}
\seealso{
Other make_segment functions: \code{\link{make_segment_body}},
  \code{\link{make_segment_container}},
  \code{\link{make_segment_meta}}
}
