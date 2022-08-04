library("openxlsx")
library('stringr')
library('dplyr')
#-------------------------------------------------------------------------------------
#' Prepare all of the PPRTV data from files downloaded May 2020. Go to the web site
#' https://hhpprtv.ornl.gov/quickview/pprtv_compare.php and download the individual
#' files listed here. The combined data is in the file
#' ../pprtv_ornl/pprtv_ornl_files/PPRTV_ORNL_noncancer.xlsx
#' @param filepath The input files are in path ../pprtv_ornl/pprtv_ornl_files
#' @export
#-------------------------------------------------------------------------------------
pprtv.ornl.load.all.new <- function(filepath, verbose = F){
  printCurrentFunction()
  
  files.list <- list.files(filepath,pattern = "*RfDs.xlsx|*RfCs.xlsx")
  files.list <- paste0(filepath, '/', files.list)
  res <- lapply(files.list,read.xlsx)
  
  t1 <- res[[1]][,c(1:7)]
  t1["toxval_type"] <- c(rep(names(res[[1]][4]), nrow(t1)))
  colnames(t1)[4] <- c("toxval_string")
  colnames(t1)[5] <- c("toxval_basis_string")
  
  t2 <- res[[2]][,c(1:7)]
  t2["toxval_type"] <- c(rep(names(res[[2]][4]), nrow(t2)))
  colnames(t2)[4] <- c("toxval_string")
  colnames(t2)[5] <- c("toxval_basis_string")
  
  t3 <- res[[3]][,c(1:7)]
  t3["toxval_type"] <- c(rep(names(res[[3]][4]), nrow(t3)))
  colnames(t3)[4] <- c("toxval_string")
  colnames(t3)[5] <- c("toxval_basis_string")
  
  t4 <- res[[4]][,c(1:7)]
  t4["toxval_type"] <- c(rep(names(res[[4]][4]), nrow(t4)))
  colnames(t4)[4] <- c("toxval_string")
  colnames(t4)[5] <- c("toxval_basis_string")
  
  nc0 <- rbind(t1,t2,t3,t4)
  non_empty_toxval <- grep("[[:alnum:]]",nc0$toxval_basis_string)
  nc0 <- nc0[non_empty_toxval,]
  non_numeric_toxval <- grep("\\d+", nc0$toxval_string, invert = T)
  nc0[non_numeric_toxval,"toxval_string"] <- ""
  
  multiple_toxval_num <- grep("\\,", nc0$toxval_string)
  #take the lowest of the given toxval values for each element
  
  for (i in 1:length(multiple_toxval_num)){
    nc0[multiple_toxval_num,"toxval_string"][i] <-min(as.numeric(str_split(nc0[multiple_toxval_num,"toxval_string"][i], ",")[[1]]))
  }
  
  nc0$toxval_string <- as.numeric(nc0$toxval_string)
  nc0$exposure_route <- gsub("(.*)\\.(.*)", "\\1", nc0$toxval_type)
  nc0$toxval_type <- gsub("(.*)\\.(.*)", "\\2", nc0$toxval_type)
  nc0$risk_assessment_class <- gsub("(S)(.*)","\\1",nc0$toxval_type)
  nc0$risk_assessment_class <-gsub("S","subchronic",nc0$risk_assessment_class)
  nc0$risk_assessment_class <- gsub("RfD|RfC","chronic",nc0$risk_assessment_class)
  nc0$toxval_type <- gsub("S","",nc0$toxval_type)
  
  nc0$url <- paste0("https://hhpprtv.ornl.gov/issue_papers/",gsub("[^[:alnum:]]","",nc0$Substance),".pdf")
  
  names.list <- c("name","casrn","critical_effect","toxval.string","toxval.basis.string","confidence","old.url","toxval.type",
                  "exposure_route","risk_assessment_class","url") 
  
  names(nc0) <- names.list
  
  nc0$pod.type <- gsub("(.*)\\s+(.*)\\s+(.*)","\\1",nc0$toxval.basis.string)
  nc0$pod.value <- gsub("(.*)\\s+(.*)\\s+(.*)","\\2",nc0$toxval.basis.string)
  nc0$pod.units <- gsub("(.*)\\s+(.*)\\s+(.*)","\\3",nc0$toxval.basis.string)
  
  nc0 <- lapply(nc0, function(x) type.convert(as.character(x), as.is = T))
  nc0 <- data.frame(nc0, stringsAsFactors = F)
  
  file <- paste0(filepath, '/', "new_PPRTV_ORNL_noncancer.xlsx")
  
  write.xlsx(nc0,file)
  
  #-------------------------------------------------------------------------------------
  # file <- paste0("new_PPRTV_ORNL_noncancer.xlsx")
  # nc <- read.xlsx(file)
  nc<- nc0
  file <- paste0(filepath, '/', "PPRTV_ORNL scrape 2020-05-28.xlsx")
  
  #file <- paste0("PPRTV_ORNL scrape 2020-05-28.xlsx")
  ncs <- read.xlsx(file)
  ncs$UF <- gsub("\\s+","", ncs$UF)
  ncs <- lapply(ncs, function(x) type.convert(as.character(x), as.is = T))
  ncs <- data.frame(ncs, stringsAsFactors = F)
  ncs$Effect.Level <- str_replace_all(ncs$Effect.Level,"\\s+"," ")
  ncs$Effect.Level <- str_replace_all(ncs$Effect.Level,"\\s+\\-\\s+","\\-")
  #ncs$Effect.Level <- gsub("\\s+"," ", ncs$Effect.Level)
  ncs_dur_1 <- grep('(^\\d+)\\s+(\\w+$)',ncs$Duration)
  ncs_dur_2 <- grep('(^\\d+)\\s+(\\w+$)',ncs$Duration, invert = T)
  #ncs$Duration
  ncs[ncs_dur_2,"Duration"] <- gsub('(\\s+lifetime)(.*)', "1\\1", ncs[ncs_dur_2,"Duration"])
  ncs[ncs_dur_2,"Duration"] <- gsub('(.*\\-.*)(\\d+\\s+generations)(.*)', "\\2", ncs[ncs_dur_2,"Duration"])
  ncs[ncs_dur_2,"Duration"] <- gsub('(\\d+\\s+generation)(.*)', "\\1", ncs[ncs_dur_2,"Duration"])
  ncs[ncs_dur_2,"Duration"] <- gsub('(\\d+\\s+years)(.*)', "\\1", ncs[ncs_dur_2,"Duration"])
  ncs[ncs_dur_2,"Duration"] <- gsub('(\\d+\\s+months)(.*)', "\\1", ncs[ncs_dur_2,"Duration"])
  ncs[ncs_dur_2,"Duration"] <- gsub('(\\d+\\s+weeks)(.*)', "\\1", ncs[ncs_dur_2,"Duration"])
  ncs[ncs_dur_2,"Duration"] <- gsub('(\\d+\\s+days)(.*)', "\\1", ncs[ncs_dur_2,"Duration"])
  ncs[ncs_dur_2,"Duration"] <- gsub('(GD\\s+\\d+\\-\\d+)(.*)', "\\1", ncs[ncs_dur_2,"Duration"])
  ncs[ncs_dur_2,"Duration"] <- gsub('(Gestation days\\s+\\d+\\-\\d+)(.*)', "\\1", ncs[ncs_dur_2,"Duration"])
  ncs[ncs_dur_2,"Duration"] <- gsub('(\\d+\\s+hours)(.*)', "\\1", ncs[ncs_dur_2,"Duration"])
  ncs[ncs_dur_2,"Duration"] <- gsub('(\\d+\\-\\d+\\s+exposures)(.*)', "\\1", ncs[ncs_dur_2,"Duration"])
  ncs[ncs_dur_2,"Duration"] <- gsub("(^\\d+\\-)(\\d+\\s+\\w+$)","\\2",ncs[ncs_dur_2,"Duration"] )
  
  ncs_dur_3 <- grep('(^\\d+)\\s+(\\w+$)',ncs[,"Duration"], invert = T)
  ncs[ncs_dur_3,"Duration"] <- gsub('(GD\\s+|Gestation days\\s+)(\\d+\\-)(\\d+)', "\\1\\3", ncs[ncs_dur_3,"Duration"])
  ncs[ncs_dur_3,"Duration"] <- gsub("~","", ncs[ncs_dur_3,"Duration"])
  
  ncs_dur_4 <- grep('(^GD|^Gestation days)\\s*(\\d+$)|(^\\d+)\\s+(\\w+$)|(^\\d+\\.*\\d+)\\s+(\\w+$)',ncs[,"Duration"], invert = T)
  ncs[ncs_dur_4,"Duration"] <- gsub('(.*)(\\d+\\.\\d+\\s+years)(.*)', "\\2", ncs[ncs_dur_4,"Duration"])
  ncs[ncs_dur_4,"Duration"] <- gsub('(.*)(\\d+\\s+lifetime)(.*)', "\\2", ncs[ncs_dur_4,"Duration"])
  ncs[ncs_dur_4,"Duration"] <- gsub('(.*\\s+|.*\\-)(\\d+\\s+\\w+$)', "\\2", ncs[ncs_dur_4,"Duration"])
  
  ncs_dur_5 <- grep('(^GD|^Gestation days)\\s*(\\d+$)|(^\\d+)\\s+(\\w+$)|(^\\d+\\.*\\d+)\\s+(\\w+$)',ncs[,"Duration"], invert = T)
  
  ncs[ncs_dur_5,"Duration"] <- gsub('(.*\\s+)(\\w+\\s+\\d+)(.*)', "\\2", ncs[ncs_dur_5,"Duration"])
  ncs[ncs_dur_5,"Duration"] <- gsub('(.*\\s+)(\\d+)(weeks$)', "\\2 \\3", ncs[ncs_dur_5,"Duration"])
  # put a flag of -999 for all character duration elements
  ncs[ncs_dur_5,"Duration"] <- gsub('(^[a-zA-Z]+\\s*[a-zA-Z]+$)', "-999 \\1", ncs[ncs_dur_5,"Duration"])
  ncs$Duration <- gsub("^\\s+$","",ncs$Duration)
  #ncs$Duration
  #ncs$study_duration_value <- gsub("([a-zA-Z]+)","",ncs$Duration)
  #ncs$study_duration_value <- gsub("\\s+","",ncs$study_duration_value)
  #ncs$study_duration_units <- gsub("([0-9]+)","",ncs$Duration)
  #ncs$study_duration_units <- gsub("^[^[:alnum:]]|\\s+$","",ncs$study_duration_units)
  #ncs$study_duration_units <- gsub("^\\s+","",ncs$study_duration_units)
  ncs$UF <- str_replace_all(ncs$UF,"\\s+","")
  ncs$UF <- as.numeric(ncs$UF)
  
  nc$species <- "-"
  nc$study_duration_value <- -1
  nc$study_duration_units <- "-"
  nc$species <- "-"
  nc$uf <- -1
  nc$toxval.basis.string <- str_replace_all(nc$toxval.basis.string,"\\s+"," ")
  nc$toxval.basis.string <- str_replace_all(nc$toxval.basis.string,"\\s+\\-\\s+","\\-")
  
  
  for(i in 1:nrow(nc)) {
    name <- nc[i,"name"]
    bs <- nc[i,"toxval.basis.string"]
    temp1 <- ncs[is.element(ncs[,"name"],name),]
    if(nrow(temp1)>0) {
      temp2 <- temp1[is.element(temp1[,"Effect.Level"],bs),]
      sd <- temp2[1,"Duration"]
      species <- tolower(temp2[1,"Species"])
      uf <- tolower(temp2[1,"UF"])
      if(is.na(species)) species <- "-"
      study_duration_value <- -1
      study_duration_units <- "-"
      new_sd <- sd[grep("^[^[:alnum:]]$",sd)]
      new_sd[grep("^[^[:alnum:]]$",new_sd)] <- NA
      sd[grep("^[^[:alnum:]]$",sd)] <- new_sd[grep("^[^[:alnum:]]$",sd)]
      if(!is.na(sd)) {
        sd2 <- sd
        study_duration_value <- gsub("([a-zA-Z]+)","",sd2)
        study_duration_value <- gsub("\\s+","",study_duration_value)
        study_duration_value <- as.numeric(study_duration_value)
        study_duration_units <- gsub("([0-9]+)","",sd2)
        study_duration_units <- gsub("^[^[:alnum:]]|\\s+$","",study_duration_units)
        study_duration_units <- gsub("^\\s+","",study_duration_units)

      }
      
      nc[i,"species"] <- species
      nc[i,"uf"] <- uf
      nc[i,"study_duration_value"] <- study_duration_value
      nc[i,"study_duration_units"] <- study_duration_units
    }
  }

  name.list <- c("name","casrn","toxval_type","toxval_numeric","toxval_numeric_qualifier","toxval_units",
                 "critical_effect","species","study_duration_value","study_duration_units",
                 "exposure_route","exposure_method","risk_assessment_class","study_type","record_url",
                 "source","subsource","source_url","details_text","pod","rfd")
  row <- as.data.frame(matrix(nrow=1,ncol=length(name.list)))
  names(row) <- name.list

  #name.list <- c("casrn","toxval_units","exposure_route","critical_effect",
  #               "risk_assessment_class","record_url","pod","rfd","uf")
  #row1 <- as.data.frame(matrix(nrow=1,ncol=length(name.list)))
  #names(row1) <- name.list

  mat <- NULL
  #mat1 <- NULL
  for(i in 1:nrow(nc)) {
    casrn <- nc[i,"casrn"]
    name <- nc[i,"name"]
    exposure_route <- nc[i,"exposure_route"]
    rac <- nc[i,"risk_assessment_class"]
    units <- nc[i,"pod.units"]
    url <- nc[i,"url"]
    critical_effect <- nc[i,"critical_effect"]
    value1 <- nc[i,"toxval.string"]
    tt1 <- nc[i,"toxval.type"]
    value2 <- nc[i,"pod.value"]
    tt2 <- nc[i,"pod.type"]
    dt <- paste0("Confidence: ",nc[i,"confidence"])
    species <- nc[i,"species"]
    study_duration_value <- nc[i,"study_duration_value"]
    study_duration_units <- nc[i,"study_duration_units"]
    uf <- nc[i,"uf"]
    #row1[1,"casrn"] <- casrn
    #row1[1,"toxval_units"] <- units
    #row1[1,"critical_effect"] <- critical_effect
    #row1[1,"exposure_route"] <- exposure_route
    #row1[1,"risk_assessment_class"] <- rac
    #row1[1,"record_url"] <- url
    #row1[1,"uf"] <- uf

    if(!is.na(value1)) {
      row[1,"name"] <- name
      row[1,"casrn"] <- casrn
      row[1,"toxval_type"] <- tt1
      row[1,"toxval_numeric"] <- value1
      row[1,"rfd"] <- value1
      row[1,"toxval_numeric_qualifier"] <- "="
      row[1,"toxval_units"] <- units
      row[1,"critical_effect"] <- critical_effect
      row[1,"exposure_route"] <- exposure_route
      row[1,"exposure_method"] <- "-"
      row[1,"risk_assessment_class"] <- rac
      row[1,"study_type"] <- rac
      row[1,"record_url"] <- url
      row[1,"source"] <- "PPRTV (ORNL)"
      row[1,"subsource"] <- "EPA ORD NCEA"
      row[1,"source_url"] <- "https://hhpprtv.ornl.gov/"
      row[1,"details_text"] <- dt
      row[1,"species"] <- species
      row[1,"study_duration_value"] <- study_duration_value
      row[1,"study_duration_units"] <- study_duration_units
      row[1,"uf"] <- uf
      mat <- rbind(mat,row)
    }
    if(!is.na(value2)) {
      row[1,"name"] <- name
      row[1,"casrn"] <- casrn
      row[1,"toxval_type"] <- tt2
      row[1,"toxval_numeric"] <- value2
      row[1,"pod"] <- value2
      row[1,"toxval_numeric_qualifier"] <- "="
      row[1,"toxval_units"] <- units
      row[1,"critical_effect"] <- critical_effect
      row[1,"exposure_route"] <- exposure_route
      row[1,"exposure_method"] <- "-"
      row[1,"risk_assessment_class"] <- rac
      row[1,"study_type"] <- rac
      row[1,"record_url"] <- url
      row[1,"source"] <- "PPRTV (ORNL)"
      row[1,"subsource"] <- "EPA ORD NCEA"
      row[1,"source_url"] <- "https://hhpprtv.ornl.gov/"
      row[1,"details_text"] <- dt
      row[1,"species"] <- species
      row[1,"study_duration_value"] <- study_duration_value
      row[1,"study_duration_units"] <- study_duration_units
      row[1,"uf"] <- uf
      mat <- rbind(mat,row)
    }
    #if(!is.na(row1[1,"pod"]) && !is.na(row1[,1,"rfd"])) mat1 <- rbind(mat1,row1)
  }
  mat <- mat[!is.na(mat[,"toxval_numeric"]),]
  file <- paste0(filepath, '/', "PPRTV_COMPARE_20200528201353_Oral Slope Factors Drinking Wa.xlsx")

  #file <- paste0("PPRTV_COMPARE_20200528201353_Oral Slope Factors Drinking Wa.xlsx")
  osf <- read.xlsx(file)

  non_empty_osf <- grep("\\d+",osf$Oral.Slope.Factors)
  osf <- osf[non_empty_osf,]
  names(osf) <- gsub("\\.", " ",names(osf))
  osf <- lapply(osf, function(x) type.convert(as.character(x), as.is = T))
  osf <- data.frame(osf, stringsAsFactors = F)


  for(i in 1:nrow(osf)) {
    name <- osf[i,1]
    casrn <- osf[i,2]
    critical_effect <- osf[i,3]
    toxval_numeric <- osf[i,4]
    units <- "(mg/kg-day)-1"
    exposure_method <- osf[i,6]
    exposure_route <- "oral"
    rac <- "chronic"
    cname <- name
    # cname <- str_replace_all(cname," ","")
    # cname <- str_replace_all(cname,"\\( ","")
    # cname <- str_replace_all(cname,"\\)","")
    # cname <- str_replace_all(cname,",","")
    # cname <- str_replace_all(cname,"-","")
    # cname <- str_replace_all(cname,"'","")
    url <- paste0("https://hhpprtv.ornl.gov/issue_papers/",gsub("[^[:alnum:]]","",osf$cname),".pdf")
    row[1,"name"] <- name
    row[1,"casrn"] <- casrn
    row[1,"toxval_type"] <- "cancer slope factor"
    row[1,"toxval_numeric"] <- toxval_numeric
    row[1,"toxval_numeric_qualifier"] <- "="
    row[1,"toxval_units"] <- units
    row[1,"critical_effect"] <- critical_effect
    row[1,"exposure_route"] <- exposure_route
    row[1,"exposure_method"] <- exposure_method
    row[1,"risk_assessment_class"] <- rac
    row[1,"study_type"] <- rac
    row[1,"record_url"] <- url
    row[1,"source"] <- "PPRTV (ORNL)"
    row[1,"subsource"] <- "EPA ORD NCEA"
    row[1,"source_url"] <- "https://hhpprtv.ornl.gov/"
    mat <- rbind(mat,row)
  }

  file <- paste0(filepath, '/', "PPRTV_COMPARE_20200528201521_Inhalation Unit Risks.xlsx")

  #file <- paste0("PPRTV_COMPARE_20200528201521_Inhalation Unit Risks.xlsx")
  iur <- read.xlsx(file)

  non_empty_iur <- grep("\\d+", iur$Inhalation.Unit.Risks)
  iur <- iur[non_empty_iur,]

  iur <- lapply(iur, function(x) type.convert(as.character(x), as.is = T))
  iur <- data.frame(iur, stringsAsFactors = F)


  for(i in 1:nrow(iur)) {
    name <- iur[i,1]
    casrn <- iur[i,2]
    critical_effect <- iur[i,3]
    toxval_numeric <- iur[i,4]
    units <- "(mg/m3)-1"
    exposure_method <- iur[i,5]
    exposure_route <- "inhalation"
    rac <- "chronic"
    cname <- name
    # cname <- str_replace_all(cname," ","")
    # cname <- str_replace_all(cname,"\\( ","")
    # cname <- str_replace_all(cname,"\\)","")
    # cname <- str_replace_all(cname,",","")
    # cname <- str_replace_all(cname,"-","")
    # cname <- str_replace_all(cname,"'","")
    url <- paste0("https://hhpprtv.ornl.gov/issue_papers/",gsub("[^[:alnum:]]","",iur$cname),".pdf")
    row[1,"name"] <- name
    row[1,"casrn"] <- casrn
    row[1,"toxval_type"] <- "cancer unit risk"
    row[1,"toxval_numeric"] <- toxval_numeric
    row[1,"toxval_numeric_qualifier"] <- "="
    row[1,"toxval_units"] <- units
    row[1,"critical_effect"] <- critical_effect
    row[1,"exposure_route"] <- exposure_route
    row[1,"exposure_method"] <- exposure_method
    row[1,"risk_assessment_class"] <- rac
    row[1,"study_type"] <- rac
    row[1,"record_url"] <- url
    row[1,"source"] <- "PPRTV (ORNL)"
    row[1,"subsource"] <- "EPA ORD NCEA"
    row[1,"source_url"] <- "https://hhpprtv.ornl.gov/"
    mat <- rbind(mat,row)
  }

  mat <- lapply(mat, function(x) type.convert(as.character(x), as.is = T))
  mat <- data.frame(mat, stringsAsFactors = F)


  file <- paste0(filepath, '/', "new_PPRTV_ORNL cancer noncancer.xlsx")
  #file <- paste0("new_PPRTV_ORNL cancer noncancer2.xlsx")
  write.xlsx(mat,file)
}
