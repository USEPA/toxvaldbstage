library(RCurl)
library(tidyr)
library(XML)
library('openxlsx')
#-------------------------------------------------------------------------------------
#' Prepare the PPRTV data downloaded October 2018. Go to the web site
#' https://hhpprtv.ornl.gov/quickview/pprtv.php 
#'
#' @export
#-------------------------------------------------------------------------------------
pprtv.ornl.scraper <- function() {
  printCurrentFunction()

  target = "https://hhpprtv.ornl.gov/quickview/pprtv.php"

  con = url(target)
  namespace = readLines(con)
  close(con)
  scope = namespace[grep("option value=",namespace)]
  scope = gsub('\t<option value=\"','',scope)
  scope = gsub('</option>','',scope)
  scope = separate(data.frame(raw=scope), raw, c("value", "chemical"), '\">')

  rfd = data.frame(character(0), character(0), character(0), character(0), character(0), character(0), character(0), character(0), character(0), character(0), character(0), stringsAsFactors = FALSE)
  names(rfd) = c("name", "Effect Level", "UF", "toxval_numeric", "Species", "Route", "Duration", "Confidence", "Target", "Critical Effect", "toxval_type")

  for(i in 2:nrow(scope)){
    result = postForm(target, chemical = scope[i,1])
    resultvector = unlist(strsplit(result, "\n"))
    readHTMLTable(resultvector) -> resultList

    if(length(resultList)>1){
      for(j in seq(2,length(resultList),2)){
        if(names(resultList[[j]])[2]=="UF"){
          new.row = cbind(
            data.frame(name = scope[i,2]),
            resultList[[j]],
            resultList[[j+1]]
          )
          new.row$toxval_type = names(new.row)[4]
          names(new.row)[4] = "toxval_numeric"
          names(new.row)[8] = "Confidence"
          rfd = rbind(rfd,new.row)
        }
      }
    }
    print(paste0(i,", ",scope[i,2]))
  }
  write.xlsx(rfd, paste0("../pprtv_ornl/pprtv_ornl_files/PPRTV_ORNL scrape ",Sys.Date(),".xlsx"))
}
