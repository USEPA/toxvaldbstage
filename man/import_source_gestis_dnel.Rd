% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/import_source_gestis_dnel.R
\name{import_source_gestis_dnel}
\alias{import_source_gestis_dnel}
\title{import_source_gestis_dnel}
\usage{
import_source_gestis_dnel(
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
OUTPUT_DESCRIPTION
}
\description{
Import GESTIS DNEL into toxval_source
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
[read_xlsx][readxl::read_xlsx]
 [str_squish][stringr::str_squish]
 [mutate][dplyr::mutate], [across][dplyr::across], [select][dplyr::select], [distinct][dplyr::distinct], [filter][dplyr::filter]
 [pivot_longer][tidyr::pivot_longer]
}
