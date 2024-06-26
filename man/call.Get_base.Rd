% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/call.Get_base.R
\name{call.Get_base}
\alias{call.Get_base}
\title{Get Adobe Analytics Segments or Calculated Metrics}
\usage{
call.Get_base(accessLevel = NULL, fields = NULL, selected = NULL,
  sort = NULL, filters = NULL, collapse_simple = TRUE,
  func.name = c("Segments.Get", "CalculatedMetrics.Get"))
}
\arguments{
\item{accessLevel}{(optional) A character vector of length 1. Must be one of \code{all, shared, owned}. 
If not specified, defaults to \code{owned}; \code{all} can only be used by an admin.}

\item{fields}{(optional) A character vector denoting the quantity, depth, and general detail of information desired. 
Must be one of 
\code{tags, shares, description, owner, modified, compatibility, favorite, reportSuiteID, definition}. 
The API always includes \code{id} and \code{name}, by default.}

\item{selected}{(optional) A character vector of ID(s) you wish to query for. If both \code{selected} and 
\code{accessLevel} are provided, \code{selected} take precedence.}

\item{sort}{(optional) A character vector of length 1. Must be one of \code{id, name, description, reportSuiteID,
owner, modified, favorite}. If not specified, defaults to \code{id}.}

\item{filters}{(optional) A named \code{list}. Valid names include
\code{approved, favorite, name, owner, reportSuiteID, tags}. For \code{tags}, character vectors of length > 1 are supported, 
and will be collapsed into comma-separated vectors of length 1 per API requirements. For the other fields, the API supports 
only vectors of length 1.}

\item{collapse_simple}{Should we parse simple list-columns, i.e. \code{tags} and \code{compatibility} in the return
value? Defaults to \code{TRUE} and only applies if these columns are requested via \emph{fields}}

\item{func.name}{Passed to \code{ApiRequest} to denote which method to call. Must be one of 
\code{Segments.Get,CalculatedMetrics.Get}.}
}
\value{
A \code{data.frame}; the number of rows corresponds to the number of unique \code{id}s.
With default settings, a successful return will contain two fields, \code{id} and \code{name}. 

If requested (via \emph{fields}), the following columns are returned as list-columns of varying complexity:

\itemize{
\item{tags*}
\item{compatibility*}
\item{shares}
\item{definition}
}

\code{tags} and \code{compatibility} are called out because they are, by default, automatically collapsed into 
atomic vectors (i.e. unnested columns) if requested within \emph{fields}. Collapsing is performed by 
\code{\link{collapse_simple_target}}. 

Please see \code{\link{parse_shares}} for handling shares.
}
\description{
Internal function - base args and doc for exported Segments and CM GET functions
}
\details{
It is possible to constrain results at the reportsuite ID (and more) level through the new \emph{filters}
argument. Note that \emph{filters} has some nuances; there are six fields, which are grouped by argument length,
then type, below:

\itemize{
\itemize{length 1, \code{character}; partial case-insensitive matching
    \item{name}
    \item{owner}
    \item{reportSuiteID}
    }
\itemize{length 1, \code{logical} (or coercible to logical, without
          generating \code{NA}); \code{TRUE}/\code{FALSE} selection
    \item{approved}
    \item{favorite}
    }
\itemize{> length 1, \code{character}; exact matching
    \item{tags}
    }
}

The \code{filters} argument as a whole is optional, but if used, the input must be a named list, where
the name(s) denote the field(s) to filter, by the provided value. Of the six available fields, only 
\code{tags} accepts inputs of length \code{>1}. Passing vectors of length \code{>1} to any of the other
fields will raise an error.
}
\section{Access Privileges}{
 
This function calls an Adobe Analytics method that requires administrative/elevated privileges for
some or all functionality.
}

\examples{
\dontrun{
# Get your segments or CMs, with id and name
my_own_simple.segments <- call.Get_base(func.name = "Segments.Get")
my_own_simple.calcMetrics <- call.Get_base(func.name = "CalculatedMetrics.Get")

}
}
\seealso{
Other internal: \code{\link{chk_shares}},
  \code{\link{collapse_simple_listcol}},
  \code{\link{collapse_simple_target}},
  \code{\link{split_segment_ret}}
}
\concept{internal}
