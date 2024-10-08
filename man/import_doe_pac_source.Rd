% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/import_doe_pac_source.R
\name{import_doe_pac_source}
\alias{import_doe_pac_source}
\title{import_doe_pac_source}
\usage{
import_doe_pac_source(
  db,
  chem.check.halt = FALSE,
  do.reset = FALSE,
  do.insert = FALSE
)
}
\arguments{
\item{db}{The version of toxval_source into which the source is loaded.}

\item{chem.check.halt}{If TRUE, stop if there are problems with the chemical mapping}

\item{do.reset}{PARAM_DESCRIPTION, Default: FALSE}

\item{do.insert}{PARAM_DESCRIPTION, Default: FALSE}
}
\value{
OUTPUT_DESCRIPTION
}
\description{
Load DOE Source into toxval_source
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
 [str_squish][stringr::str_squish], [str_extract_all][stringr::str_extract_all]
 [mutate][dplyr::mutate], [across][dplyr::across], [rename][dplyr::rename], [select][dplyr::select], [rowwise][dplyr::rowwise], [ungroup][dplyr::ungroup]
 [pivot_longer][tidyr::pivot_longer]
}
