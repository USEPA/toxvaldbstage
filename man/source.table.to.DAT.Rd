% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/source.table.to.DAT.R
\name{source.table.to.DAT}
\alias{source.table.to.DAT}
\title{source.table.to.DAT}
\usage{
source.table.to.DAT(source.db, source_table, limit = 1e+06, sample_p = NA)
}
\arguments{
\item{source.db}{The version of toxval source to use.}

\item{source_table}{The name of toxval source table to use. If a DataFrame, input data will be #' processing and returned without saving to file.}

\item{limit}{Excel file grouping limit (default is max XLSX row limit)}

\item{sample_p}{Percentage of records to sample down to}

\item{source}{The name of toxval source to use.}
}
\value{
Processed source table to DAT format cached and returned.
}
\description{
Convert toxval source table to DAT format for loading to DAT
application
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
\code{\link[dplyr]{rename}}, \code{\link[dplyr]{filter}}, \code{\link[dplyr]{slice}}, \code{\link[dplyr]{select}}
 \code{\link[tidyr]{pivot_longer}}, \code{\link[tidyr]{reexports}}
 \code{\link[writexl]{write_xlsx}}
}
