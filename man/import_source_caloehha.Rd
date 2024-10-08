% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/import_source_caloehha.R
\name{import_source_caloehha}
\alias{import_source_caloehha}
\title{import_source_caloehha}
\usage{
import_source_caloehha(
  db,
  chem.check.halt = FALSE,
  do.reset = FALSE,
  do.insert = FALSE,
  do.summary_data = FALSE
)
}
\arguments{
\item{db}{The version of toxval_source into which the source is loaded.}

\item{chem.check.halt}{If TRUE and there are problems with chemicals CASRN checks, halt the program}

\item{do.reset}{PARAM_DESCRIPTION, Default: FALSE}

\item{do.insert}{PARAM_DESCRIPTION, Default: FALSE}

\item{do.summary_data}{If TRUE, add Cal OEHHA Summary data to table before insertion}

\item{infile}{The input file ="../caloehha/caloehha_files/OEHHA-chemicals_2018-10-30T08-50-47.xlsx",}
}
\value{
None; data is pushed to ToxVal_Source
}
\description{
Load caloehha Source file into toxval_source
The raw data can be exported as an Excel sheet from the web site
https://oehha.ca.gov/chemicals, selecting the link "Export database as .CSV file"

This method parses that file and prepares for loading into toxval source
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
\code{\link[openxlsx]{read.xlsx}}
}
