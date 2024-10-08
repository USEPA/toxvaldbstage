% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/runQuery.R
\name{runQuery}
\alias{runQuery}
\title{runQuery}
\usage{
runQuery(query = NULL, db, do.halt = TRUE, verbose = FALSE)
}
\arguments{
\item{query}{a properly formatted SQL query as a string}

\item{db}{the name of the database}

\item{do.halt}{if TRUE, halt on errors or warnings}

\item{verbose}{if TRUE, print diagnostic information}
}
\value{
Query results
}
\description{
Runs a database query and returns a result set
}
\details{
DETAILS
}
\examples{
\dontrun{
if(interactive()){
 #EXAMPLE1
 }
}
}
\seealso{
\code{\link[RMySQL]{character(0)}}, \code{\link[RMySQL]{MySQLDriver-class}}
 \code{\link[utils]{flush.console}}
}
