% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fix.replace.unicode.R
\name{fix.replace.unicode}
\alias{fix.replace.unicode}
\title{fix.replace.unicode}
\usage{
fix.replace.unicode(df)
}
\arguments{
\item{df}{Character vector to check/replace unicode symbols.}
}
\value{
Returns a modified version of the input vector with unicode replacements.
}
\description{
A function to check all character fields and handle unicode symbols,
either by removing them or replacing them with alphabetic equivalents.
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
[stri_escape_unicode][stringi::stri_escape_unicode]
 [str_extract][stringr::str_extract]
}
