% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/import_actor_source.R
\name{import_actor_source}
\alias{import_actor_source}
\title{FUNCTION_TITLE}
\usage{
import_actor_source(toxval.db, infile, filepath, verbose = F)
}
\arguments{
\item{toxval.db}{The version of toxval source into which the tables are loaded.}

\item{infile}{The input file ./ACToR replacements/ACToR_2021/assay_table_hazard prioritized for use.xlsx}

\item{filepath}{The path for all the input xlsx files ./ACToR replacements/ACToR_2021}

\item{verbose}{Whether the loaded rows should be printed to the console.}

\item{do.init}{if TRUE, read the data in from the res_actor_2021q4 database and set up the matrix}
}
\value{
OUTPUT_DESCRIPTION
}
\description{
Extract ACToR1 data to toxval source
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
\code{\link[openxlsx]{read.xlsx}}, \code{\link[openxlsx]{write.xlsx}}
 \code{\link[stringr]{str_replace}}
 \code{\link[dplyr]{group_by}}, \code{\link[dplyr]{select}}, \code{\link[dplyr]{mutate-joins}}, \code{\link[dplyr]{mutate_all}}
 \code{\link[tidyr]{spread}}
 \code{\link[stats]{aggregate}}, \code{\link[stats]{na.fail}}
}
