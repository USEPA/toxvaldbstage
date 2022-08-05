library(XML)
library(tidyr)
library('openxlsx')
#--------------------------------------------------------------------------------------
#' Scrape the IRIS website
#'
#--------------------------------------------------------------------------------------
iris.scraper = function(){
  printCurrentFunction()

  headnode = "https://cfpub.epa.gov/ncea/iris/search/basic/index.cfm"
  con <- url(headnode)
  res <- readLines(con)
  close(con)
  res[grep("href='https://cfpub.epa.gov/ncea/iris2/chemicalLanding.cfm?substance_nmbr=",res)]

  url.nums <- data.frame(X1 = unlist(lapply(strsplit(res[grep("substance_nmbr=",res)],"substance_nmbr="), '[[',2)))

  url.nums <- separate(url.nums, X1, c("number","name"), "'>")

  url.nums$name <- gsub("</a> ","",url.nums$name)

  chem.table <- readHTMLTable(res,stringsAsFactors = FALSE)[[1]]

  rfd <- data.frame(character(0), character(0), character(0), character(0), character(0), character(0), character(0), character(0), character(0), stringsAsFactors = FALSE)
  names(rfd) <- c("casrn","name","url", "System", "RfD (mg/kg-day)", "Basis", "PoD", "Composite UF", "Confidence")

  rfc <- data.frame(character(0), character(0), character(0), character(0), character(0), character(0), character(0), character(0),character(0),  stringsAsFactors = FALSE)
  names(rfc) <- c("casrn","name","url", "System", "RfC (mg/m3)", "    Basis", "PoD", "Composite UF", "Confidence")

  woe <- data.frame(character(0), character(0), character(0), character(0), character(0), stringsAsFactors = FALSE)
  names(woe) <- c("casrn","name","url", "WOE Characterization", "Framework for WOE Characterization")


  name.list <- c("casrn","name","variable","value","url")
  row <- as.data.frame(matrix(nrow=1,ncol=length(name.list)))
  names(row) <- name.list
  cdata <- NULL
  n <- nrow(url.nums)
  #n <- 5
  for(i in 1:n){
    number <- url.nums$number[i]
    casrn <- chem.table[i,"CASRN"]
    name <- url.nums$name[i]
    target <- paste0("https://cfpub.epa.gov/ncea/iris2/chemicalLanding.cfm?substance_nmbr=",number)
    con <- url(target)
    res <- readLines(con)
    #on.exit(close(con))
    close(con)
    tables <- readHTMLTable(res)
    if(length(tables)>0){
      for(j in 1:length(tables)){
        cat(j,names(tables[[j]]),"\n")
        if(names(tables[[j]])[2] == "RfD (mg/kg-day)"){
          names(tables[[j]]) = c("System", "RfD (mg/kg-day)", "Basis", "PoD", "Composite UF", "Confidence")
          rfd <- rbind(rfd,cbind(casrn,url.nums$name[i],target,tables[[j]]))
        }
        if(names(tables[[j]])[2] == "RfC (mg/m3)"){
          names(tables[[j]]) = c("System", "RfC (mg/m3)",     "Basis", "PoD", "Composite UF", "Confidence")
          rfc <- rbind(rfc,cbind(casrn,url.nums$name[i],target,tables[[j]]))
        }
        if(names(tables[[j]])[1] == "WOE Characterization"){
          names(tables[[j]]) = c("WOE Characterization", "Framework for WOE Characterization")
          woe <- rbind(woe,cbind(casrn,url.nums$name[i],target,tables[[j]]))
        }
      }
    }
    print(i)
    variable.list <- c(
      "Oral Slope Factor",
      "Drinking Water Unit Risk",
      "Extrapolation Method",
      "Tumor site(s)",
      "Tumor type(s)",
      "Inhalation Unit Risk"
    )
    for(j in 1:length(res)) {
      line <- res[j]
      for(variable in variable.list) {
        if(contains(line,variable)) {
          j <- j+1
          value <- res[j]
          value = gsub("\n","",value)
          value = gsub("\t","",value)
          value = gsub("</sup>","",value)
          value = gsub("<sup>","",value)
          value = gsub("<strong>","",value)
          value = gsub("</strong>","",value)
          value = gsub("<br />","",value)
          value = gsub("<br/>","",value)
          value = gsub("</p>","",value)
          value = gsub("<sup>","",value)

          row[1,"casrn"] <- casrn
          row[1,"name"] <- name
          row[1,"variable"] <- variable
          row[1,"value"] <- value
          row[1,"url"] <- target
          cdata <- rbind(cdata,row)
          #browser()
        }
      }

    }
    #browser()
  }

  rfd <- unique(rfd)
  rfc <- unique(rfc)
  woe <- unique(woe)

  names(rfd)[2] <- "name"
  names(rfc)[2] <- "name"
  names(woe)[2] <- "name"
  # Remove the \n's and \t's
  rfc$PoD = gsub("\n","",rfc$PoD)
  rfc$PoD = gsub("\t","",rfc$PoD)
  rfc$`Composite UF` = gsub("\n","",rfc$`Composite UF`)
  rfc$`Composite UF` = gsub("\t","",rfc$`Composite UF`)

  rfd$PoD = gsub("\n","",rfd$PoD)
  rfd$PoD = gsub("\t","",rfd$PoD)
  rfd$`Composite UF` = gsub("\n","",rfd$`Composite UF`)
  rfd$`Composite UF` = gsub("\t","",rfd$`Composite UF`)
  #
  #new fields

  rfd$toxval_units <- "mg/kg-day"
  rfd$toxval_type <- "RfD"
  names(rfd)[3] <- "record_url"
  names(rfd)[5] <- "toxval_numeric"

  rfc$toxval_units <- "mg/m3"
  rfc$toxval_type <- "RfC"
  names(rfc)[3] <- "record_url"
  names(rfc)[5] <- "toxval_numeric"

  rfd.rfc <- rbind(rfd,rfc)
  write.xlsx(rfd.rfc,paste0("iris_scrape_rfd_rfc ",Sys.Date(),".xlsx"))
  write.xlsx(woe,paste0("iris_scrape_woe ",Sys.Date(),".xlsx"))
}
