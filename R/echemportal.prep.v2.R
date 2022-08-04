#--------------------------------------------------------------------------------------
#' Process the ECHA eChemPortal data from 2020
#'
#' @param do.load If TRUE, laod all of the in vivo data
#' @export
#--------------------------------------------------------------------------------------
echemportal.prep.v2 <- function(do.load=F) {
  printCurrentFunction()
  if(do.load) {
    file <- "../echa/echa_files/eChemPortal mammalian data 2020.xlsx"
    MAT <<- read.xlsx(file)
  }
  mat <- MAT
  tag.list <- NULL
  name.list <- c("casrn","name","source","subsource","source_url",
                 "toxval_type","toxval_numeric","toxval_numeric_qualifier","toxval_units",
                 "study_type","study_duration_class",
                 "species","strain",
                 "exposure_route","exposure_method",
                 "generation",
                 "year","url",
                 "guideline","glp","quality")
  res <- as.data.frame(matrix(nrow=nrow(mat),ncol=length(name.list)))
  names(res) <- name.list
  res[] <- ""
  res$toxval_count <- 0

  counter <- 0
  istart <- 1
  istop <- nrow(mat)
  #istart <- 8968
  #istart <- 1
  #istop <- 10
  for(i in istart:istop) {
    res[i,"name"] <- mat[i,"Substance.Name"]
    res[i,"casrn"] <- mat[i,"Number"]
    res[i,"source"] <- "eChemPortal v2"
    res[i,"subsource"] <- mat[i,"Participant"]
    res[i,"study_duration_class"] <- mat[i,"Folder.name"]
     res[i,"source_url"] <- "https://www.echemportal.org/echemportal/"
    res[i,"url"] <-  mat[i,"Section.Link"]
    temp <- mat[i,"Section"]
    x <- str_split(temp,":")[[1]]
    if(length(x)==1) {
      res[i,"study_type"] <- tolower(x[1])
    }
    else if(length(x)==2) {
      res[i,"study_type"] <- tolower(x[1])
      res[i,"exposure_route"] <- tolower(str_trim(x[2],side="both"))
    }

    value <- mat[i,"Values"]

    block <- str_split(value,"\n")[[1]]
    guideline <- ""
    for(line in block) {
      temp <- strsplit(line,":")[[1]]
      tag <- temp[1]
      val <- substr(line,nchar(tag)+2,nchar(line))
      #val <- str_replace_all(line,paste0(tag,":"),"")
      #val <- str_replace_all(val,paste0(tag,":"),"")
      tag <- str_trim(tag,side="both")
      val <- str_trim(val,side="both")
      #cat(line,"\n")
      #cat(i,tag,"...",val,"\n")
      #if(contains(val,tag)) {
      #  val <- substr(val,nchar(tag)+2,nchar(val))
      #  cat(i,tag,"...",val,"\n")
      #  browser()
      #}
      if(!is.na(tag)) {
        if(!is.element(tag,tag.list)) tag.list <- c(tag.list,tag)
        if(tag=="Species") {res[i,"species"] <- val}
        else if(tag=="Strain") {res[i,"strain"] <- val}
        else if(tag=="GLP compliance") {res[i,"glp"] <- val}
        else if(tag=="Test guideline, Qualifier") {res[i,"guideline"] <- paste0(res[i,"guideline"],", ",val)}
        else if(tag=="Test guideline, Guideline") {res[i,"guideline"] <- paste0(res[i,"guideline"],", ",val)}
        else if(tag=="Route of administration") {res[i,"exposure_route"] <- paste0(res[i,"exposure_route"],", ",val)}
        else if(tag=="Type of inhalation exposure") {res[i,"exposure_method"] <- val}
        else if(tag=="Type of coverage") {res[i,"exposure_method"] <- val}
        else if(tag=="Reliability") {res[i,"quality"] <- val}
        else if(tag=="Reference, Year") {res[i,"year"] <- val}
        else if(tag=="Test type") {res[i,"study_type"] <- paste0(res[i,"study_type"],", ",val)}
        else if(tag=="Endpoint") {res[i,"study_type"] <- paste0(res[i,"study_type"],", ",val)}
        else if(tag=="Effect levels, Dose descriptor") {
          res[i,"toxval_type"] <- paste0(res[i,"toxval_type"],"|",val)
        }
        else if(tag=="Effect levels, Effect level") {
          tv <- parse.effect.level(val)
          res[i,"toxval_numeric_qualifier"] <- paste0(res[i,"toxval_numeric_qualifier"],"|",tv$qualifier)
          res[i,"toxval_numeric"] <- paste0(res[i,"toxval_numeric"],"|",tv$value)
          res[i,"toxval_units"] <- paste0(res[i,"toxval_units"],"|",tv$units)
          res[i,"toxval_count"] <- res[i,"toxval_count"] + 1
        }
        else if(tag=="Effect levels (fetuses), Dose descriptor") {
          res[i,"toxval_type"] <- paste0(res[i,"toxval_type"],"|",val)
          res[i,"generation"] <- paste0(res[i,"generation"],"|","fetus")
         }
        else if(tag=="Effect levels (maternal animals), Dose descriptor") {
          res[i,"toxval_type"] <- paste0(res[i,"toxval_type"],"|",val)
          res[i,"generation"] <- paste0(res[i,"generation"],"|","maternal")
        }
        else if(tag=="Effect levels (F1), Dose descriptor") {
          res[i,"toxval_type"] <- paste0(res[i,"toxval_type"],"|",val)
          res[i,"generation"] <- paste0(res[i,"generation"],"|","F1")
        }
        else if(tag=="Effect levels (F2), Dose descriptor") {
          res[i,"toxval_type"] <- paste0(res[i,"toxval_type"],"|",val)
          res[i,"generation"] <- paste0(res[i,"generation"],"|","F2")
        }
        else if(tag=="Effect levels (P0), Dose descriptor") {
          res[i,"toxval_type"] <- paste0(res[i,"toxval_type"],"|",val)
          res[i,"generation"] <- paste0(res[i,"generation"],"|","P0")
        }
        else if(tag=="Effect levels (fetuses), Effect level") {
          tv <- parse.effect.level(val)
          res[i,"toxval_numeric_qualifier"] <- paste0(res[i,"toxval_numeric_qualifier"],"|",tv$qualifier)
          res[i,"toxval_numeric"] <- paste0(res[i,"toxval_numeric"],"|",tv$value)
          res[i,"toxval_units"] <- paste0(res[i,"toxval_units"],"|",tv$units)
          res[i,"toxval_count"] <- res[i,"toxval_count"] + 1
        }
        else if(tag=="Effect levels (maternal animals), Effect level") {
          tv <- parse.effect.level(val)
          res[i,"toxval_numeric_qualifier"] <- paste0(res[i,"toxval_numeric_qualifier"],"|",tv$qualifier)
          res[i,"toxval_numeric"] <- paste0(res[i,"toxval_numeric"],"|",tv$value)
          res[i,"toxval_units"] <- paste0(res[i,"toxval_units"],"|",tv$units)
          res[i,"toxval_count"] <- res[i,"toxval_count"] + 1
        }
        else if(tag=="Effect levels (F1), Effect level") {
          tv <- parse.effect.level(val)
          res[i,"toxval_numeric_qualifier"] <- paste0(res[i,"toxval_numeric_qualifier"],"|",tv$qualifier)
          res[i,"toxval_numeric"] <- paste0(res[i,"toxval_numeric"],"|",tv$value)
          res[i,"toxval_units"] <- paste0(res[i,"toxval_units"],"|",tv$units)
          res[i,"toxval_count"] <- res[i,"toxval_count"] + 1
        }
        else if(tag=="Effect levels (F2), Effect level") {
          tv <- parse.effect.level(val)
          res[i,"toxval_numeric_qualifier"] <- paste0(res[i,"toxval_numeric_qualifier"],"|",tv$qualifier)
          res[i,"toxval_numeric"] <- paste0(res[i,"toxval_numeric"],"|",tv$value)
          res[i,"toxval_units"] <- paste0(res[i,"toxval_units"],"|",tv$units)
          res[i,"toxval_count"] <- res[i,"toxval_count"] + 1
        }
        else if(tag=="Effect levels (P0), Effect level") {
          tv <- parse.effect.level(val)
          res[i,"toxval_numeric_qualifier"] <- paste0(res[i,"toxval_numeric_qualifier"],"|",tv$qualifier)
          res[i,"toxval_numeric"] <- paste0(res[i,"toxval_numeric"],"|",tv$value)
          res[i,"toxval_units"] <- paste0(res[i,"toxval_units"],"|",tv$units)
          res[i,"toxval_count"] <- res[i,"toxval_count"] + 1
        }
        else if(tag=="Type of information") {}

        else {
          cat("missing tag: ",i,tag,"\n")
          browser()
        }
      }
    }
    if(res[i,"toxval_type"]=="") {
      if(res[i,"study_type"]=="developmental toxicity / teratogenicity") {
        count <- res[i,"toxval_count"]
        string <- ""
        for(j in 1:count) string <- paste0(string,"|LEL")
        res[i,"toxval_type"] <- string
      }
      else if(mat[i,"Folder.name"]=="acute") {
        count <- res[i,"toxval_count"]
        string <- ""
        for(j in 1:count) string <- paste0(string,"|LD50")
        res[i,"toxval_type"] <- string
      }
    }
    if(i%%1000==0) cat("finished ",i," out of ",nrow(mat),"\n")
  }
  #tag.list <- unique(tag.list)
  #file <- "../echa/echa_files/tag.list.xlsx"
  #write.xlsx(tag.list,file)
  file <- "../echa/echa_files/eChemPortal mammalian data 2020 step 1.xlsx"
  write.xlsx(res,file)
}
#--------------------------------------------------------------------------------------
#' Process the the string that has the qualifier, value and units
#'
#' @param val The string
#' @return a list with three values: qualifier, value, units
#' @export
#--------------------------------------------------------------------------------------
parse.effect.level <- function(val) {
  val0 <- val
  qualifier <- "="
  if(substr(val,1,5) == ">= <=" ) {
    qualifier <- substr(val,1,5)
    val <- substr(val,7,nchar(val))
  }
  else if(substr(val,1,2) == "<=" || substr(val,1,2) == ">=" ) {
    qualifier <- substr(val,1,2)
    val <- substr(val,4,nchar(val))
  }
  else if(substr(val,1,1)==">" || substr(val,1,1)=="<") {
    qualifier <- substr(val,1,1)
    val <- substr(val,3,nchar(val))
  }
  else if(substr(val,1,3)=="ca." ) {
    qualifier <- substr(val,1,3)
    val <- substr(val,5,nchar(val))
  }
  val <- str_trim(val,side="both")
  temp <- strsplit(val," ")[[1]]
  tvn <- temp[1]
  if(is.na(as.numeric(tvn))) {
    if(contains(temp[1],"—")) {
      temp2 <- strsplit(tvn,"—")[[1]]
      tvn <- temp2[1]
    }
    else {
      cat("Bad value: ",val,"\n")
      browser()
    }
  }
  tvn <- as.numeric(tvn)
  units <- str_trim(str_replace(val,temp[1],""),side="both")
  do.trim <- F
  char.list <- c("<=",">=","<",">","ca.")
  for(char in char.list) {
    if(contains(units,char)) {
      units <- str_replace_all(units,char,"")
      units <- str_trim(units,side="both")
      #do.trim <- T
      #cat(val0,"\n")
      prefix <- str_split(units," ")[[1]][1]
      #cat(units,"...",prefix,"\n")
      units <- str_replace_all(units,prefix,"")
      units <- str_trim(units,side="both")
      #cat(units,"\n")
      #browser()
    }
  }
  return( list(qualifier=qualifier, value=tvn, units=units) )
}
