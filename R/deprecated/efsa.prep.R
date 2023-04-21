library('openxlsx')
library('stringr')
library('stringi')
#-------------------------------------------------------------------------------------
#' Prepare the new EFSA data
#' @param dir The directory where the input data sits, ../efsa2/efsa2_files/
#' @param step1 Run the first step of the processing
#' @param step2 Run the second step of the processing
#' @param step3 Run the third step of the processing
#' @param step4 Run the fourth step of the processing
#'
#--------------------------------------------------------------------------------------
efsa.prep <- function(dir,step1=F,step2=F,step3=F,step4=F) {
  printCurrentFunction()
  file <- paste0(dir,"Bioassay/AssayDescriptions.txt")
  print(file)
  desc <- readLines(file)

  ##########################################################################
  if(step1) {
    # step 1: QC this file
    for(i in 1:length(desc)) {
      line <- desc[i]
      if(line=="ASSAY_COMP_UNIT\t") desc[i] <- "ASSAY_COMP_UNIT\tunitless"
      if(line=="ASSAY_COMP_UNIT") desc[i] <- "ASSAY_COMP_UNIT\tunitless"
    }
    tag.list <- NULL
    for(i in 1:length(desc)) {
      line <- desc[i]
      doit <- T
      if(line=="ASSAY_NOTE") doit <- F
      if(line=="END") doit <- F
      if(line=="ENDEND") doit <- F

      if(doit) {
        line <- stri_encode(line, "", "UTF-8")
        if(!contains(line,"\t")) {
          cat("Line: ",i,":[",line,"]\n")
          #browser()
        }
      }
      temp <- str_split(line,"\t")[[1]]
      tag <- temp[1]
      if(is.element(tag,c("NOTE","ENDSOURCE_NAME_AID","OURCE_NAME_AID","ASSAY_COMP_VALUE_TYPE FLOAT"))) {
        cat("Line: ",i,":[",line,"]\n")
        #browser()
      }
      if(!is.element(tag,tag.list)) tag.list <- c(tag.list,tag)
    }
  }

  ##########################################################################
  if(step2) {
    pointer <- 1
    aid <- NA
    assay_name <- NA
    assay_desc <- NA
    assay_url <- NA
    assay_comp_id <- NA
    assay_comp_name <- NA
    assay_comp_desc <- NA
    units <- NA
    data <- NULL
    id.list <- NULL
    name.list <- NULL
    while(pointer <= length(desc)) {
      line <- desc[pointer]
      temp <- str_split(line,"\t")[[1]]
      tag <- temp[1]
      pointer <- pointer + 1
      if(tag=="ENDEND") return()
      if(tag=="END") {
        for(k in 1:length(name.list)) names(bioassay)[k+1] <- name.list[k]
        #if(aid=="EFSA_FGE12R3_Supporting_AID_1") browser()
        names <- c("casrn","name",names(bioassay)[!is.element(names(bioassay),c("casrn","name"))])
        bioassay <- bioassay[,names]
        bioassay$assay_name <- assay_name
        bioassay$assay_url <- assay_url
        bioassay$aid <- aid
        file <- paste0(dir,"merge1/",aid,".xlsx")
        names(bioassay) <- stri_encode(names(bioassay), "", "UTF-8")
        write.xlsx(bioassay,file)
        aid <- NA
        assay_name <- NA
        assay_desc <- NA
        assay_url <- NA
        assay_comp_id <- NA
        assay_comp_name <- NA
        assay_comp_desc <- NA
        units <- NA
        id.list <- NULL
        name.list <- NULL
      }
      else {
        value <- temp[2]
        if(tag=="ASSAY_COMP_ID") {
          assay_comp_id <- value
          assay_comp_name <- NA
          assay_comp_desc <- NA
          units <- NA
          id.list <- c(id.list,value)
        }
        else if(tag=="ASSAY_COMP_NAME") {
          assay_comp_name <- value
          name.list <- c(name.list,value)
        }
        else if(tag=="ASSAY_COMP_DESC") assay_comp_desc <- value
        else if(tag=="ASSAY_COMP_UNIT") {}
        else if(tag=="ASSAY_NAME") assay_name <- value
        else if(tag=="ASSAY_DESCRIPTION") assay_desc <- value
        else if(tag=="ASSAY_URL") assay_url <- value
        else if(tag=="SOURCE_NAME_AID") {
          aid <- value
          file <- paste0(dir,"Bioassay/",aid,".xlsx")
          #print(file)
          bioassay <- read.xlsx(file)
          prefix <- str_split(aid,"_AID")[[1]][1]
          file <- paste0(dir,"Substance/",prefix,"_Substance.xlsx")
          #print(file)
          substance <- read.xlsx(file)
          substance <- unique(substance)
          rownames(substance) <- substance$SOURCE_NAME_SID
          bioassay$casrn <- NA
          bioassay$name <- NA
          for(i in 1:nrow(bioassay)) {
            snaid <- bioassay[i,"SOURCE_NAME_SID"]
            bioassay[i,"casrn"] <- substance[snaid,"CASRN"]
            bioassay[i,"name"] <- substance[snaid,"NAME"]
          }
          #browser()
        }
      }
    }

  }

  ##########################################################################
  if(step3) {
    acute <- NULL
    tox <- NULL
    dart <- NULL
    file.list <- list.files(paste0(dir,"merge1/"))
    print(file.list)

    for(f in file.list) {
      file <- paste0(dir,"merge1/",f)
      if(contains(f,"AcuteTox")) {
        print(file)
        mat <- read.xlsx(file)
        mat <- mat[,!is.element(names(mat),c("Structural.Group","6","7","8","9","10","Group"))]
        #print(names(mat))
        acute <- rbind(acute,mat)
      }
      if(contains(f,"ReproTox_DevTox")) {
        print(file)
        mat <- read.xlsx(file)
        mat <- mat[,!is.element(names(mat),c("Structural.Group","6","7","8","9","10","Group"))]
        names(mat)[is.element(names(mat),"Durations")] <- "Duration"
        #print(names(mat))
        dart <- rbind(dart,mat)
      }
      if(contains(f,"ToxData")) {
        print(file)
        mat <- read.xlsx(file)
        mat <- mat[,!is.element(names(mat),c("Structural.Group","6","7","8","9","10","Group","Additional.Comments","Other.identifier(s)"))]
        names(mat)[is.element(names(mat),"Result")] <- "NOAEL"
        names(mat)[is.element(names(mat),"NOEL")] <- "NOAEL"
        #print(names(mat))
        tox <- rbind(tox,mat)
      }
    }
    file <- paste0(dir,"merge2/","EFSA_acute.xlsx")
    write.xlsx(acute,file)
    file <- paste0(dir,"merge2/","EFSA_dev_repro.xlsx")
    write.xlsx(dart,file)
    file <- paste0(dir,"merge2/","EFSA_repeat_dose.xlsx")
    write.xlsx(tox,file)
  }
  ##########################################################################
  if(step4) {

    file <- paste0(dir,"merge2/EFSA_acute.xlsx")
    acute <- read.xlsx(file)
    file <- paste0(dir,"merge2/EFSA_repeat_dose.xlsx")
    repdose <- read.xlsx(file)
    file <- paste0(dir,"merge2/EFSA_dev_repro.xlsx")
    dart <- read.xlsx(file)
    file <- paste0(dir,"merge2/EFSA_file_list.txt")
    flist <- read.table(file)
    flist$prefix <- flist[,1]
    names(flist) <- c("document_name","efsa_group")
    flist[,1] <- as.character(flist[,1])
    flist[,2] <- as.character(flist[,2])
    for(i in 1:nrow(flist)) {
      x <- flist[i,"document_name"]
      flist[i,"efsa_group"] <- str_split(x,"_")[[1]][1]
    }
    rownames(flist) <- flist$efsa_group
    name.list <- c("casrn","name","study_type","study_duration_value","study_duration_units","species_original","sex","exposure_route","toxval_type","toxval_numeric_qualifier","toxval_numeric","toxval_units","long_ref","url","aid","document_name")

    res1 <- as.data.frame(matrix(nrow=nrow(acute),ncol=length(name.list)))
    names(res1) <- name.list
    res1[,"casrn"] <- str_trim(acute[,"casrn"])
    res1[,"name"] <- str_trim(acute[,"name"])
    res1[,"study_type"] <- "acute"
    res1[,"study_duration_value"] <- 1
    res1[,"study_duration_units"] <- "day"
    res1[,"species_original"] <- tolower(str_trim(acute[,"Species"]))
    res1[,"sex"] <- tolower(str_trim(acute[,"Sex"]))
    res1[,"exposure_route"] <- tolower(str_trim(acute[,"Route"]))
    res1[,"toxval_type"] <- "LD50"
    res1[,"toxval_numeric"] <- str_trim(acute[,"LD50"])
    res1[,"toxval_numeric_qualifier"] <- "="
    res1[,"toxval_units"] <- "mg/kg"
    res1[,"long_ref"] <- str_trim(paste(acute[,"assay_name"],acute[,"Reference"],acute[,"Reference.1"]))
    res1[,"url"] <- str_trim(acute[,"assay_url"])
    res1[,"aid"] <- str_trim(acute[,"aid"])

    for(i in 1:nrow(res1)) {
      aid <- res1[i,"aid"]
      efsa_group <- str_split(aid,"_")[[1]][2]
      res1[i,"document_name"] <- flist[efsa_group,"document_name"]

      x <- res1[i,"toxval_numeric"]
      if(contains(x,">")) {
        res1[i,"toxval_numeric"] <- str_replace(x,">","")
        res1[i,"toxval_numeric_qualifier"] <- ">"
      }
    }

    res2 <- as.data.frame(matrix(nrow=nrow(repdose),ncol=length(name.list)))
    names(res2) <- name.list
    res2[,"casrn"] <- str_trim(repdose[,"casrn"])
    res2[,"name"] <- str_trim(repdose[,"name"])
    res2[,"study_type"] <- "repeat dose"
    res2[,"study_duration_value"] <- repdose[,"Duration"]
    res2[,"study_duration_units"] <- repdose[,"Duration"]
    res2[,"species_original"] <- tolower(str_trim(repdose[,"Species;.Sex"]))
    res2[,"sex"] <- tolower(str_trim(repdose[,"Species;.Sex"]))
    res2[,"exposure_route"] <- tolower(str_trim(repdose[,"Route"]))
    res2[,"toxval_type"] <- "NOAEL"
    res2[,"toxval_numeric"] <- str_trim(repdose[,"NOAEL"])
    res2[,"toxval_numeric_qualifier"] <- "="
    res2[,"toxval_units"] <- "mg/kg-day"
    res2[,"long_ref"] <- str_trim(paste(repdose[,"assay_name"],repdose[,"Reference"],repdose[,"Reference.1"]))
    res2[,"url"] <- str_trim(repdose[,"assay_url"])
    res2[,"aid"] <- str_trim(repdose[,"aid"])

    for(i in 1:nrow(res2)) {
      aid <- res2[i,"aid"]
      efsa_group <- str_split(aid,"_")[[1]][2]
      res2[i,"document_name"] <- flist[efsa_group,"document_name"]

      x <- res2[i,"toxval_numeric"]
      if(contains(x,">")) {
        res2[i,"toxval_numeric"] <- str_replace(x,">","")
        res2[i,"toxval_numeric_qualifier"] <- ">"
      }
    }

    res3 <- as.data.frame(matrix(nrow=nrow(dart),ncol=length(name.list)))
    names(res3) <- name.list
    res3[,"casrn"] <- str_trim(dart[,"casrn"])
    res3[,"name"] <- str_trim(dart[,"name"])
    res3[,"study_type"] <- str_trim(dart[,"Study.type"])
    res3[,"study_duration_value"] <- dart[,"Duration"]
    res3[,"study_duration_units"] <- dart[,"Duration"]
    res3[,"species_original"] <- tolower(str_trim(dart[,"Species;.Sex"]))
    res3[,"sex"] <- tolower(str_trim(dart[,"Species;.Sex"]))
    res3[,"exposure_route"] <- tolower(str_trim(dart[,"Route"]))
    res3[,"toxval_type"] <- "NOAEL"
    res3[,"toxval_numeric"] <- str_trim(dart[,"NOAEL"])
    res3[,"toxval_numeric_qualifier"] <- "="
    res3[,"toxval_units"] <- "mg/kg-day"
    res3[,"long_ref"] <- str_trim(paste(dart[,"assay_name"],dart[,"Reference"],dart[,"Reference.1"]))
    res3[,"url"] <- str_trim(dart[,"assay_url"])
    res3[,"aid"] <- str_trim(dart[,"aid"])

    for(i in 1:nrow(res3)) {
      aid <- res3[i,"aid"]
      efsa_group <- str_split(aid,"_")[[1]][2]
      res3[i,"document_name"] <- flist[efsa_group,"document_name"]

      x <- res3[i,"toxval_numeric"]
      if(contains(x,">")) {
        res3[i,"toxval_numeric"] <- str_replace(x,">","")
        res3[i,"toxval_numeric_qualifier"] <- ">"
      }
    }

    res <- rbind(res1,res2,res3)
    file <- paste0(dir,"merge2/EFSA_combined.xlsx")
    write.xlsx(res,file)
  }
}

