% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/import_oppt_source.R
\name{import_oppt_source}
\alias{import_oppt_source}
\title{FUNCTION_TITLE}
\usage{
import_oppt_source(db, infile = "OPPT_data_20181219.xlsx", chem.check.halt = T)
}
\arguments{
\item{db}{The version of toxval_source into which the source info is loaded.}

\item{infile}{The input file ./oppt/oppt_files/OPPT_data_20181219.xlsx}

\item{chem.check.halt}{If TRUE, stop if there are problems with the chemical mapping}
}
\value{
OUTPUT_DESCRIPTION
}
\description{
FUNCTION_DESCRIPTION
}
\details{
#' Load OPPT Source Info into toxval_source

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
\code{\link[openxlsx]{read.xlsx}}
}
