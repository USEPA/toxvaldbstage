% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/set_field_SQL_type.R
\name{set_field_SQL_type}
\alias{set_field_SQL_type}
\title{set_field_SQL_type}
\usage{
set_field_SQL_type(src_f = NULL, default_fields = NULL)
}
\arguments{
\item{src_f}{Dataframe to generate field types from}

\item{default_fields}{Default fields already handled by input generic SQL}
}
\value{
SQL string for the input dataframe's fields
}
\description{
Helper function to generate SQL field types based on dataframe field types
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
\code{\link[dplyr]{bind}}
}
