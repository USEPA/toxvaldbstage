#--------------------------------------------------------------------------------------
#' Load PPRTV NCEA Source Info into toxval_source
#' @param db The version of toxval_source into which the source info is loaded.
#' @param csvfile The input csv file ./pprtv_ncea/pprtv_ncea_files/dose_reg2.csv
#' @param scrapepath The path for new_pprtv_ncea_scrape_table file ./pprtv_ncea/PPRTV_scrape2020-04-08.xlsx
#' @param chem.check.halt If TRUE and there are problems with chemicals CASRN checks, halt the program
#--------------------------------------------------------------------------------------
import_pprtv_ncea_source <- function(db,
                                     csvfile="../pprtv_ncea/pprtv_ncea_files/dose_reg2.csv",
                                     scrapepath="../pprtv_ncea/PPRTV_scrape2020-04-08.xlsx",
                                     chem.check.halt=F) {
  printCurrentFunction(db)

  filepath = paste0(toxval.config()$datapath,"pprtv_ncea/pprtv_ncea_files")
  csvfile = paste0(toxval.config()$datapath,"pprtv_ncea/pprtv_ncea_files/",csvfile)
  scrapepath = paste0(toxval.config()$datapath,"pprtv_ncea/pprtv_ncea_files/",scrapepath)

  #####################################################################
  cat("Build all input pprtv_ncea tables \n")
  #####################################################################
  files.list <- list.files(path = filepath, pattern = "*.xlsx")
  any_temp_files <- grep("^\\~\\$.*", files.list, value = T)
  files.list <- files.list[! files.list %in% any_temp_files]
  files.list <- paste0( filepath, '/',files.list)
  res <- lapply(files.list,openxlsx::read.xlsx)
  pprtv_ncea_25 <- read.csv(csvfile, header = T, sep = ",")
  res <- c(res,list(pprtv_ncea_25))
  rm(pprtv_ncea_25)
  pprtv_ncea_26 <- read.xlsx(scrapepath,1)
  res <- c(res, list(pprtv_ncea_26))
  rm(pprtv_ncea_26)

  #####################################################################
  cat("Provide dataframe names prefixed with pprtv_ncea and update column names by making necessary substitutions \n")
  #####################################################################
  res_names <- c('assessment_study', 'assessments','cancer_tt','cancer_types','cancer','chem_groups',
                 'dose_reg','dose','dosimetry_lkp', 'dosimetry_rqd', 'endpoints_tier',
                 'endpoints','exposure_route','exposure_units','PPRTV_scrape_11_2017',
                 'ref_value_type','ref_value_units','reference_tt','reference','species',
                 'study_type','study', 'tissue_gen_types','tissue_gen','dose_reg2','new_scrape_table' )

  names(res) <- paste0("pprtv_ncea_", res_names)
  res <- lapply(res, function(x) setNames(x, gsub("\\.+","\\_", names(x))))
  res <- lapply(res, function(x) setNames(x, gsub("\\'|\\?","", names(x))))

  #####################################################################
  cat("Subset the source list of dataframes by excluding duplicated dataframes(cancer and reference) \n")
  #####################################################################
  res <- res[!(names(res)) %in% c("pprtv_ncea_cancer_tt","pprtv_ncea_reference_tt")]

  #####################################################################
  cat("Endpoints file has a header column in row 65534, hence have to remove it \n")
  #####################################################################
  res$pprtv_ncea_endpoints <- res$pprtv_ncea_endpoints[-65534,]

  #####################################################################
  cat("Assign appropriate data types \n")
  #####################################################################
  for (i in 1:length(res)) {
    res[[i]] <- lapply(res[[i]], function(x) type.convert(as.character(x), as.is = T))
    res[[i]] <- data.frame(res[[i]], stringsAsFactors = F)
  }

  #####################################################################
  cat("Assign columns with all NA values as empty \n")
  #####################################################################
  for (i in 1:length(res)) {
    res[[i]][sapply(res[[i]], function(x) all(is.na(x) == T))] <- ""
  }

  #####################################################################
  cat("Create a data frame to house all replacement column names and use it to
      replace the existing column names \n")
  #####################################################################
  cols_2_rename <- c("No_Value_Document", "InChlID","Date_Created","Date_QAd", "Cancer_Value_Type", "Study", "Source_of_POD","Dose_Regimen_ID",
                     "Body_Weight_Provided", "Body_Weight","Body_Weight_Units","Food_Consumption_Value","Food_Consumption_Units",
                     "Water_Consumption_Value","Water_Consumption_Units","ET_Dose","TB_Dose","PU_Dose","TOT_Dose",
                     "SYS_Dose","MMAD","MMAD_Units","Duration_Classification", "Animal_Species", "Animal_Strain","Number_of_Doses_Administered",
                     "Dose_Units","Converted_Dose_Units","Dosimetry_Comment","Exposure_Route","Reference_Value_Type","UFa","UFd","UFh",
                     "UFl","UFs","UFc","Year", "Title", "HERO_ID","Study_Not_Available",
                     "Tissue_ID", "Dose_Regimen_ID", "Tissue.Generation","Co.Critical_Effect","CE_LOC","Confidence_in_Endpoint_Assessment","Tissue.Generation")

  renamed_cols <- c("No_value", "InChl_ID","Created_Date","QAd_Date", "Type", "Study_ID", "POD_Source","DoseReg_ID",
                    "Body_Weight", "Average_BW","BW_Units","Average_FC","FC_Units",
                    "Average_WC","WC_Units","IC_ET_DOSE","IC_TB_DOSE","IC_PU_DOSE","IC_TOT_DOSE",
                    "IC_SYS_DOSE","Diameter","Diameter_Units","Duration_Class", "Species", "Strain","Number_of_Doses",
                    "Orig_Dose_Units","Conv_Dose_Units","Dosimetry_Notes","Route","Type","UF_A","UF_D","UF_H",
                    "UF_L","UF_S","UF_C","Study_Year", "Study_Title", "HERO","PDFNotAvailable",
                    "TisGen_ID", "DoseReg_ID", "TissueOrGen","Co_Crit","LOC_CancerEffect","NOELConfidence","Tissue_Types")
  colnames_res_to_change <-  data.frame(cols_2_rename, renamed_cols, stringsAsFactors = F)

  #runInsertTable(colnames_res_to_change, "pprtv_ncea_modified_colnames",db,do.halt=T,verbose=F )
  for (i in 1:length(res)){
    for (j in 1:nrow(colnames_res_to_change)){
      names(res[[i]])[match(colnames_res_to_change$cols_2_rename[j], names(res[[i]]))] = colnames_res_to_change$renamed_cols[j]
    }
  }

  #####################################################################
  cat("Fix date conversions for Created_Date and QAd_Date in all dataframes with these fields \n")
  #####################################################################
  Date_to_fix <- lapply(res, function(x) {
    grep("Date", names(x), ignore.case = T, value = T)
  })
  Date_to_fix2 <-""
  for (i in 1:length(res)){
    Date_to_fix2[i]<- lapply(res[i], "[", Date_to_fix[[i]])
  }
  for (i in 1:length(res)){
    for (j in 1:ncol(Date_to_fix2[[i]])){
      if (ncol(Date_to_fix2[[i]]) != 0){
        if (names(Date_to_fix2[[i]])[j] %in% names(res[[i]])) {
          res[[i]][,names(Date_to_fix2[[i]])[j]] <- excel_numeric_to_date(as.numeric(as.character(res[[i]][,names(Date_to_fix2[[i]])[j]])), date_system = "modern")
          res[[i]][,names(Date_to_fix2[[i]])[j]] <- format(res[[i]][,names(Date_to_fix2[[i]])[j]], format = "%d-%b-%y")
        }
      }
    }
  }
  # #####################################################################
  # cat("Fix encoding changes \n")
  # #####################################################################
  res$pprtv_ncea_endpoints$Details <- iconv(res$pprtv_ncea_endpoints$Details,"UTF-8","ASCII","-")
  res$pprtv_ncea_PPRTV_scrape_11_2017$Effect_Level <- iconv(res$pprtv_ncea_PPRTV_scrape_11_2017$Effect_Level,"UTF-8","ASCII"," ")
  res$pprtv_ncea_new_scrape_table$Effect_Level = iconv(res$pprtv_ncea_new_scrape_table$Effect_Level,"latin1","ASCII","~")
  res$pprtv_ncea_new_scrape_table$toxval_numeric = iconv(res$pprtv_ncea_new_scrape_table$toxval_numeric,"latin1","ASCII","~")
  res$pprtv_ncea_new_scrape_table$Species <- iconv(res$pprtv_ncea_new_scrape_table$Species,"UTF-8","ASCII"," ")
  res$pprtv_ncea_new_scrape_table$Route <- iconv(res$pprtv_ncea_new_scrape_table$Route,"UTF-8","ASCII"," ")
  res$pprtv_ncea_new_scrape_table$Duration <- iconv(res$pprtv_ncea_new_scrape_table$Duration,"UTF-8","ASCII"," ")
  res$pprtv_ncea_new_scrape_table$Confidence <- iconv(res$pprtv_ncea_new_scrape_table$Confidence,"UTF-8","ASCII"," ")
  res$pprtv_ncea_new_scrape_table$Target <- iconv(res$pprtv_ncea_new_scrape_table$Target,"UTF-8","ASCII"," ")
  res$pprtv_ncea_new_scrape_table$Critical_Effect <- iconv(res$pprtv_ncea_new_scrape_table$Critical_Effect,"UTF-8","ASCII"," ")
  res$pprtv_ncea_new_scrape_table$UF <- iconv(res$pprtv_ncea_new_scrape_table$UF,"UTF-8","ASCII"," ")
  #####################################################################
  cat("convert character UF to numeric in pprtv_ncea_new_scrape_table  \n")
  #####################################################################
  res$pprtv_ncea_new_scrape_table$UF <- as.numeric(res$pprtv_ncea_new_scrape_table$UF)

  #####################################################################
  cat("Create PPRTV Source tables \n")
  #####################################################################
  table_names <- tolower(c("pprtv_ncea_tbl_Assessment_Study","pprtv_ncea_tbl_Assessment",
                           "pprtv_ncea_ztbl_Cancer_Type","pprtv_ncea_tbl_Cancer","pprtv_ncea_ztbl_ChemGroups",
                           "pprtv_ncea_tbl_DoseReg","pprtv_ncea_tbl_Dose","pprtv_ncea_ztbl_DosimetryLkp","pprtv_ncea_ztbl_DosimetryRqd",
                           "pprtv_ncea_ztbl_Endpoints_tier","pprtv_ncea_tbl_Endpoints", "pprtv_ncea_ztbl_ExpRoute","pprtv_ncea_ztbl_ExpUnits","pprtv_ncea_ztbl_Scrape11_2017",
                           "pprtv_ncea_ztbl_RfV_Type","pprtv_ncea_ztbl_RfV_Units","pprtv_ncea_tbl_Reference","pprtv_ncea_ztbl_Species",
                           "pprtv_ncea_ztbl_StudyType","pprtv_ncea_tbl_Study","pprtv_ncea_ztbl_TissueGenTypes","pprtv_ncea_tbl_Tis_Gen","pprtv_ncea_ztbl_DoseReg2","pprtv_ncea_ztbl_new_scrape_2020"))
  stop = FALSE
  for( i in 1:length(res)){
    for (j in 1:length(table_names)){
      res[[i]] = fix.non_ascii.v2(res[[i]],table_names[j])
      runInsertTable(res[[i]],table_names[j],db,do.halt=T,verbose=F)
      i <- i+1
      if (i == length(res)+1){
        stop = TRUE
        break
      }
    }
    if (stop){break}
  }

  ####################################################################
  cat("Build new_pprtv_ncea \n")
  #####################################################################
  query2 <- "select casrn,chemical_name as name,RFV_ID,Type as toxval_type,reference_value as toxval_numeric,
  RFV_Units as toxval_units,studytype as study_type,tissue_gen as toxval_subtype,endpoint as phenotype,
  Point_Of_Departure as POD_numeric,PoD_Source as POD_type,PoD_Units as POD_units,
  UF_A,UF_D,UF_H,UF_L,UF_S,UF_C,Study_Year as year,author,study_title as title,Full_Reference as long_ref,
  species,strain,sex,substring_index(Route_of_Exposure,' - ',1) as exposure_route,
  substring_index(Route_of_Exposure,' - ',-1) as exposure_method,Duration_Class as study_duration_class,
  Duration_of_Study as study_duration_value,Duration_Units as study_duration_units
  from pprtv_ncea_tbl_assessment_study a
  inner join pprtv_ncea_tbl_assessment c on c.Assessment_ID = a.Assessment_ID
  inner join pprtv_ncea_tbl_study e on e.Study_ID = a.Study_ID
  inner join
  (select AsmtStudy_ID, Species, Strain, Route_of_Exposure, Duration_Class, Duration_of_Study, Duration_Units, group_concat(distinct Gender) as sex
    from pprtv_ncea_tbl_dosereg
    group by AsmtStudy_ID, Species, Strain, Route_of_Exposure, Duration_Class, Duration_of_Study, Duration_Units) d on d.AsmtStudy_ID = a.AsmtStudy_ID
  inner join pprtv_ncea_tbl_reference b on a.Assessment_ID = b.Assessment_ID and a.Study_ID = b.StudyID;"

  new_pprtv_ncea <- runQuery(query2, db)
  print(new_pprtv_ncea[new_pprtv_ncea$casrn=="110-54-3","name"])

  new_pprtv_ncea["pprtv_ncea_id"] <- c(1:length(new_pprtv_ncea[,1]))
  print(new_pprtv_ncea[new_pprtv_ncea$casrn=="110-54-3","name"])

  new_pprtv_ncea <- new_pprtv_ncea[c("pprtv_ncea_id",names(new_pprtv_ncea[-31]))]
  print(new_pprtv_ncea[new_pprtv_ncea$casrn=="110-54-3","name"])

  res = as.data.frame(new_pprtv_ncea)
  res = res[!is.element(res$casrn,"VARIOUS"),]

  res = subset(res,select=-c(pprtv_ncea_id))
  nlist = c("casrn","name","rfv_id","toxval_type",
            "toxval_numeric","toxval_units","study_type","toxval_subtype","phenotype",
            "pod_numeric","pod_type","pod_units",
            "uf_a","uf_d","uf_h","uf_l","uf_s","uf_c",
            "year","author","title","long_ref","species","strain",
            "sex","exposure_route","exposure_method",
            "study_duration_class","study_duration_value","study_duration_units")
  names(res) = nlist
  res[is.element(res$casrn,"64724-95-6"),"casrn"] = "64742-95-6"
  #####################################################################
  cat("Prep and load the data\n")
  #####################################################################
  source_prep_and_load(db,source="PPRTV (NCEA)",table="source_pprtv_ncea",res=res,F,T,T)
}
