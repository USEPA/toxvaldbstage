% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/import_source_hess.R
\name{import_source_hess}
\alias{import_source_hess}
\title{import_source_hess}
\usage{
import_source_hess(
  db,
  chem.check.halt = FALSE,
  do.reset = FALSE,
  do.insert = FALSE
)
}
\arguments{
\item{db}{The version of toxval_source into which the source is loaded.}

\item{chem.check.halt}{If TRUE and there are bad chemical names or casrn,}

\item{do.reset}{If TRUE, delete data from the database for this source before}

\item{do.insert}{If TRUE, insert data into the database, default FALSE}
}
\value{
None; data is pushed to toxval_source
}
\description{
Load HESS data into toxval_source
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
\code{\link[readxl]{read_excel}}
 \code{\link[tidyr]{pivot_longer}}, \code{\link[tidyr]{unite}}, \code{\link[tidyr]{drop_na}}
 \code{\link[dplyr]{mutate}}, \code{\link[dplyr]{across}}, \code{\link[dplyr]{reexports}}, \code{\link[dplyr]{na_if}}, \code{\link[dplyr]{case_when}}, \code{\link[dplyr]{tidyeval-compat}}, \code{\link[dplyr]{select}}, \code{\link[dplyr]{rename}}, \code{\link[dplyr]{mutate-joins}}
 \code{\link[stringr]{str_replace}}, \code{\link[stringr]{str_trim}}, \code{\link[stringr]{case}}, \code{\link[stringr]{str_extract}}
}
