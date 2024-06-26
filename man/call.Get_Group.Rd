% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/call.Get_Group.R
\name{call.Get_Group}
\alias{call.Get_Group}
\title{Permissions.GetGroup call with error handling}
\usage{
call.Get_Group(x, include_permissions = NULL, ...)
}
\arguments{
\item{x}{chr or int. a vector of group name(s) and/or group id(s) to query. Duplicates will be automatically removed.}

\item{include_permissions}{logi. denote whether to also include category_permissions data for each group. 
Defaults to \code{FALSE}.}

\item{...}{additional arguments to \link[RSiteCatalyst]{ApiRequest} (optional).}
}
\value{
A nested list of length (unique) \code{x}. Each first-level list element is a named (using the corresponding call value) 
nested list of length two consisting of the named elements \emph{result} and \emph{error}. 

It is possible for either (but not both) \emph{result} or \emph{error} to be \code{NULL}. Because the 
\emph{Permissions.GetGroup} method returns errors for invalid call (query) values, this function simply returns the 
server error message. In other words, a call value either throws an error (and the error message is captured in \emph{error}) 
or succeeds (and the returned data is captured in \emph{result}). 

The actual contents within each of \emph{result} or \emph{error} may vary as follows:

\itemize{
\item \emph{error}:
    \itemize{
    \item If no errors, \code{NULL}
    \item If errors, a list of length 2 with named elements \emph{message} and \emph{call}
    }
\item \emph{result}:
    \itemize{
        \item If no errors and \code{include_permissions = FALSE} a list of length 6; in order:
            \itemize{
            \item group_id (\code{integer} vector of length 1)
            \item group_name (\code{character} vector of length 1)
            \item group_description (\code{character} vector of length 1)
            \item all_report_suite_access (\code{logical} vector of length 1)
            \item rsid_list (\code{character} vector of length 1)
            \item user_list (\code{character} vector)
            }
       \item If no errors and \code{include_permissions = TRUE}, a the above 6 plus a 7th element of 
             \emph{category_permissions}, a nested \code{data.frame} with 4 fields:
                \itemize{
                \item category (\code{character} vector of length 4)
                \item name (\code{character} vector of length 4)
                \item access (\code{character} vector of length 4)
                \item permissions (\code{list} of length 4, each element a \code{data.frame} with 3 fields)
                    \itemize{
                    \item id (\code{integer} vector)
                    \item name (\code{character} vector)
                    \item access (\code{logical} vector)
                    }
                }
      \item If errors, \code{NULL}
    }
}
}
\description{
Get group information via group name or ID, optionally get permissions as well
}
\details{
This function calls the Adobe Analytics 1.4 
\href{https://marketing.adobe.com/developer/documentation/analytics-administration-1-4/r-getgroup-1}{Permissions.GetGroup}
method with additional error handling, as the (internal; not exported) \code{.Get_Group} function is wrapped in 
purrr::\link[purrr]{safely}. 

This function is not normally needed by itself, but is exported for advanced users who wish to perform their own
return value restructuring, rather than use the convenience function \code{Get_Group}, which combines this function 
and \code{\link{restr.Get_Group}}. Also useful for troubleshooting in the (likely) event the core API changes and/or if 
(less-likely) one of the package dependencies changes in an unexpected manner.
}
\note{
Like all functions in this package that interact with the Adobe Analytics API directly, this function relies upon  
\link[RSiteCatalyst]{ApiRequest} for authentication, json parsing, and basic error handling. This means you must 
still authenticate through the RSiteCatalyst API, and any fundamental changes to \code{RSiteCatalyst::ApiRequest} have the 
potential to change all dependent functions in this package. 

This function should not be confused with RSiteCatalyst::\link[RSiteCatalyst]{GetGroups}, which calls a different 
Adobe Analytics method altogether.
}
\examples{
\dontrun{
#Can use either name or id
by_id   <- call.Get_Group(136031L, include_permissions = TRUE)
by_name <- call.Get_Group("RAP-Mendeley Admin Access", include_permissions = TRUE)
all.equal(by_id[[1]], by_name[[1]])
 
#Can also do both in a single call
by_either <- call.Get_Group(c(136031L, "RAP-Mendeley Admin Access"), include_permissions = TRUE)
all.equal(by_either[[1]], by_either[[2]])
 
#All errors
err_only <- call.Get_Group(c("a", 1), include_permissions = TRUE)
 
#Mixed; some error, some working
mixed <- call.Get_Group(c(136031L, "a"), include_permissions = TRUE)
 
#NULL values will not trigger a query and return an empty list
null_call <- call.Get_Group(NULL) # returns list(0)
identical(null_call, list())
 
#However, NA will trigger a query and return an error
na_call <- call.Get_Group(NA)
 
#As will blank ("")
blank_call <- call.Get_Group("")
 
#Mixing NULL into a call with non-NULL values will return result of length 
# x[!is.null(x)]
test_null_mix <- call.Get_Group(c(NULL, "NULL", "NA", "", 135994L), include_permissions = TRUE)
length(test_null_mix)
}
}
\seealso{
Other get group functions: \code{\link{restr.Get_Group}},
  \code{\link{restr_permissions}}
}
\concept{get group functions}
