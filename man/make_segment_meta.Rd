% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/make_segment_meta.R
\name{make_segment_meta}
\alias{make_segment_meta}
\title{Make segment-level metadata for creating (NOT editing) a segment}
\usage{
make_segment_meta(name = NULL, reportSuiteID = NULL,
  description = NULL, favorite = NULL, owner = NULL, shares = NULL,
  tags = NULL)
}
\arguments{
\item{name}{(Required) A character vector of length 1. This is the name (title) of the segment}

\item{reportSuiteID}{(Required) A character vector of length 1. This is the report suite the segment should
belong to. Must be a valid report suite ID and one that your login has permissions to access}

\item{description}{(Optional) A character vector of length 1. The segment description}

\item{favorite}{(Optional) A logical vector of length 1. Should the segment be added to the favorite segments list?}

\item{owner}{(Optional) A character vector of length 1. The Login of the user who will be the owner. Defaults to 
the current user if not provided.}

\item{shares}{(Optional) A named list of length 2 with names \emph{type} (one of \code{group,user}) and \code{name}, 
which is the group or user name to share with. Both values must be provided as character vectors of length 1.
Helper: \code{\link{make_sharelist}}}

\item{tags}{(Optional) A character vector of keywords to group segments for filtering}
}
\value{
A list, possibly nested, containing the required segment-level metadata
}
\description{
For a new segment, create required metadata and output appropriate structure for a call to the AA method Segments.Save
}
\details{
This creates the appropriate segment-level metadata, required as a part of creating
a new segment. Segment-level metadata comprises half of a complete segment body. The other half is the 
container; see \code{\link{make_segment_container}} for more details.

All fields from the AA \emph{Segments.Save} method are supported by this function, except for, \emph{id}. 
This is intentional; a field named \emph{id} containing a valid (existing) segment ID is the sole variable 
that determines whether the AA \emph{Segments.Save} method edits (read: overwrites) an existing segment 
or creates a new segment.

Since \emph{id} is NOT a supported argument, you cannot accidentally pass in a segment id, period. 
This means you never have to worry that you might accidentally overwrite an existing segment so long as you
use this function, and ONLY this function, to create the segment metadata. 

There is nothing stopping you from appending an element named \emph{id} along with a segment ID value to the 
output (of this function) for an edit use case.
}
\note{
None of the \code{make_} functions in this package directly interact with any API, nor are they 
strictly required to use \code{\link{Segments_Save}}. 

You are free to create any and all parts of a segment body on your own, as needed.
}
\examples{
# Forthcoming
}
\seealso{
Other make_segment functions: \code{\link{make_element_rules}},
  \code{\link{make_segment_body}},
  \code{\link{make_segment_container}},
  \code{\link{make_sharelist}}
}
\concept{make_segment functions}
