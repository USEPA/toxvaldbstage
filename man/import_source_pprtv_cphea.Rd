% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/import_source_iris.R,
%   R/import_source_pprtv_cphea.R
\name{import_source_iris}
\alias{import_source_iris}
\alias{import_source_pprtv_cphea}
\title{import_source_iris}
\usage{
import_source_iris(
  db,
  chem.check.halt = FALSE,
  do.reset = FALSE,
  do.insert = FALSE,
  do.summary_data = FALSE
)

import_source_pprtv_cphea(
  db,
  chem.check.halt = FALSE,
  do.reset = FALSE,
  do.insert = FALSE,
  do.summary_data = FALSE
)
}
\arguments{
\item{db}{The version of toxval_source into which the source is loaded.}

\item{chem.check.halt}{If TRUE and there are bad chemical names or casrn,}

\item{do.reset}{If TRUE, delete data from the database for this source before}

\item{do.insert}{If TRUE, insert data into the database, default FALSE}

\item{do.summary_data}{If TRUE, add PPRTV CPHEA Summary data to table before insertion}
}
\value{
None; data is pushed to toxval_source

None; data is pushed to toxval_source
}
\description{
Import of IRIS 2023-05-09 source into toxval_source

Import PPRTV (CPHEA) source data into toxval_source
}
\details{
DETAILS

DETAILS
}
\examples{
\dontrun{
if(interactive()){
 #EXAMPLE1
 }
}
\dontrun{
if(interactive()){
 #EXAMPLE1
 }
}
}
\seealso{
\code{\link[readxl]{read_excel}}
 \code{\link[dplyr]{mutate}}, \code{\link[dplyr]{filter}}, \code{\link[dplyr]{select}}, \code{\link[dplyr]{across}}, \code{\link[dplyr]{rename}}, \code{\link[dplyr]{c("rowwise", "rowwise", "rowwise")}}, \code{\link[dplyr]{distinct}}
 \code{\link[tidyr]{pivot_longer}}, \code{\link[tidyr]{reexports}}, \code{\link[tidyr]{separate}}, \code{\link[tidyr]{replace_na}}
 \code{\link[stringr]{str_trim}}, \code{\link[stringr]{str_replace}}, \code{\link[stringr]{str_extract}}

\code{\link[readxl]{read_excel}}
 \code{\link[tidyr]{pivot_longer}}, \code{\link[tidyr]{reexports}}, \code{\link[tidyr]{separate}}, \code{\link[tidyr]{replace_na}}, \code{\link[tidyr]{drop_na}}
 \code{\link[dplyr]{mutate}}, \code{\link[dplyr]{across}}, \code{\link[dplyr]{case_when}}, \code{\link[dplyr]{select}}, \code{\link[dplyr]{reexports}}, \code{\link[dplyr]{distinct}}
 \code{\link[stringr]{str_trim}}, \code{\link[stringr]{str_extract}}
 \code{\link[tidyselect]{all_of}}
}
