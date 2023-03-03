#--------------------------------------------------------------------------------------
#' Import of rsl 2022 source into toxval_source
#'
#' @param db The version of toxval_source into which the source is loaded.
#' @param infile1 The input file ./rsl/rsl_files/rsl_thq10_nov_2022.xlsx
#' @param infile2 The input file ./rsl/rsl_files/rsl_thq01_nov_2022.xlsx
#' @param infile3 The input file ./rsl/rsl_files/rsl_subchronic_nov_2022.xlsx
#' @param chem.check.halt If TRUE and there are bad chemical names or casrn,
#--------------------------------------------------------------------------------------
import_rsl_source <- function(db,chem.check.halt=F) {
  printCurrentFunction(db)
  source = "rsl"
  source_table = "source_rsl"
  dir = paste0(toxval.config()$datapath,"rsl/rsl_files/")
  file0 = paste0(dir,"rsl_thq10_nov_2022.xlsx")
  file1 = paste0(dir,"rsl_thq01_nov_2022.xlsx")
  file2 = paste0(dir,"rsl_subchronic_nov_2022.xlsx")

  res0 = readxl::read_xlsx(file0, skip=2)
  res1 = readxl::read_xlsx(file1, skip=2)
  res2 = readxl::read_xlsx(file2, skip=2)
  #####################################################################
  cat("Do any non-generic steps to get the data ready \n")
  #####################################################################

  # Standardize the names
  names(res0) <- names(res0) %>%
    stringr::str_squish() %>%
    # Replace whitespace and periods with underscore
    gsub("[[:space:]]|[.]", "_", .) %>%
    gsub("___", "_", .) %>%
    gsub("k_e_y", "key", .)

  names(res1) <- names(res1) %>%
    stringr::str_squish() %>%
    # Replace whitespace and periods with underscore
    gsub("[[:space:]]|[.]", "_", .) %>%
    gsub("___", "_", .) %>%
    gsub("k_e_y", "key", .)

  names(res2) <- names(res2) %>%
    stringr::str_squish() %>%
    # Replace whitespace and periods with underscore
    gsub("[[:space:]]|[.]", "_", .) %>%
    gsub("___", "_", .) %>%
    gsub("k_e_y", "key", .)


  # changes for res2, subchronic table
  # rename columns in a way that they can be separated later for other column values
  res2_1 <- res2 %>%
    dplyr::rename( "SRfCi_subchronic_inhalation_mg/m3" = "SRfCi(mg/m3)",
                   "SRfDo_subchronic_oral_mg/kg-day" = "SRfDo(mg/kg-day)",
                   "RfDo_chronic_oral_mg/kg-day" = "RfDo(mg/kg-day)",
                   "RfCi_chronic_inhalation_mg/m3" = "RfCi(mg/m3)",
                   "casrn" = "CAS_No_",
                   "name" = "Analyte",
                   "RfD Reference" = "RfD_Reference",
                   "SRfC Reference" = "SRfC_Reference",
                   "SRfD Reference" = "SRfD_Reference",
                   "RfC Reference" = "RfC_Reference")
  # make table longer based on toxval_type
  res2_2 <- res2_1 %>%
    tidyr::pivot_longer(
      cols= c("SRfCi_subchronic_inhalation_mg/m3",
              "SRfDo_subchronic_oral_mg/kg-day",
              "RfDo_chronic_oral_mg/kg-day",
              "RfCi_chronic_inhalation_mg/m3"),
      names_to= "toxval_type",
      values_to= "toxval_numeric"
    )
  
  # filter out RfDo and RfCi in subchronic file
  res2_2_1 <- res2_2 %>%
  filter(grepl("^S", toxval_type))
  
  # seperate out toxval_type to make other toxval columns and add input file column
  res2_3 <- res2_2_1 %>%
  tidyr::separate(toxval_type, c("toxval_type", "risk_assessment_class", "exposure_route","toxval_unit"), sep="_", fill="right", remove=TRUE) %>%
    mutate(raw_input_file= "rsl_subchronic_nov_2022.xlsx")

  # make new column for the key values to go in
  res2_3$study_type = res2_3$toxval_type
  # replace values with key columns values
  res2_4 <- res2_3 %>%
    mutate(study_type = ifelse(toxval_type == 'SRfCi', res2_3$`SRfC Reference`,study_type)) %>%
    mutate(study_type = ifelse(toxval_type == 'SRfDo', res2_3$`SRfD Reference`, study_type )) %>%
    mutate(study_type = ifelse(toxval_type == 'RfDo', res2_3$`RfD Reference`, study_type)) %>%
    mutate(study_type = ifelse(toxval_type == 'RfCi', res2_3$`RfC Reference`, study_type))

  # make dictionary of values in annotation column that needs to go
  rep_str0 = c('SRfCi'='',
              'SRfDo'='',
              'RfDo'='',
              'RfCi'='')

  # get rid of those values in annotation columns
  res2_4$study_type <- str_replace_all(res2_4$study_type, rep_str0)

  # add input file columns and toxval_subtype columns to res0 and res1, thq tables
  res0_0 <- res0 %>%
    mutate(raw_input_file= "rsl_thq10_nov_2022.xlsx_",
           toxval_subtype = "Thq = 1.0")
  res1_0 <- res1 %>%
    mutate(raw_input_file= "rsl_thq01_nov_2022.xlsx",
           toxval_subtype = "Thq = 0.1")

  # combine res0 and res1
  res_combo_01 <- res0_0 %>%
    bind_rows(res1_0 %>%
                mutate(across(c("SFO(mg/kg-day)-1"), ~as.numeric(.))))

  # rename columsn in a way that they can be separated later for other toxval column values
  res_combo_01_2 <- res_combo_01 %>%
    dplyr::rename("Screening Level (Resident Soil)_chronic_mg/kg" = "Resident_Soil_(mg/kg)",
                  "Screening Level (Industrial Soil)_chronic_mg/kg" = "Industrial_Soil_(mg/kg)",
                  "Screening Level (Resident Air)_chronic_ug/m3" = "Resident_Air_(ug/m3)",
                  "Screening Level (Industrial Air)_chronic_ug/m3" = "Industrial_Air_(ug/m3)",
                  "Screening Level (Tap Water)_chronic_ug/L" = "Tap_Water_(ug/L)",
                  "Screening Level (MCL)_chronic_ug/L" = "MCL_(ug/L)",
                  "Protection of Groundwater: Risk-based SSL_chronic_mg/kg" = "Risk-based_SSL_(mg/kg)",
                  "Protection of Groundwater: MCL-based SSL_chronic_mg/kg" = "MCL-based_SSL_(mg/kg)",
                  "SFO_chronic_(mg/kg-day)-1" = "SFO_(mg/kg-day)-1",
                  "IUR_chronic_(ug/m3)-1" = "IUR_(ug/m3)-1",
                  "RfDo_chronic_mg/kg-day" = "RfDo(mg/kg-day)",
                  "RfCi_chronic_mg/m3" = "RfCi(mg/m3)",
                  "Csat_chronic_mg/kg" = "Csat(mg/kg)",
                  "GIABS_PhysChem" = "GIABS",
                  "ABSd_PhysChem" = "ABSd",
                  "vol" = "v_o_l",
                  "casrn" = "CAS_No_",
                  "name" = "Analyte",
                  "key1" = "key_2",
                  "key2" = "key_4",
                  "key3" = "key_6",
                  "key4" = "key_8",
                  "key5" = "key_17",
                  "key6" = "key_19",
                  "key7" = "key_21",
                  "key8" = "key_23",
                  "key9" = "key_25",
                  "key10" = "key_28")
  
  # remove "vol" and "mutagen" columns
  res_combo_01_2_1 <- res_combo_01_2[ , !names(res_combo_01_2) %in% 
                  c("vol", "mutagen")]


  # make table longer based on toxval_type
  res_combo_01_3 <- res_combo_01_2_1 %>%
  tidyr::pivot_longer(
          cols= c("Screening Level (Resident Soil)_chronic_mg/kg",
                  "Screening Level (Industrial Soil)_chronic_mg/kg",
                  "Screening Level (Resident Air)_chronic_ug/m3",
                  "Screening Level (Industrial Air)_chronic_ug/m3",
                  "Screening Level (Tap Water)_chronic_ug/L",
                  "Screening Level (MCL)_chronic_ug/L",
                  "Protection of Groundwater: Risk-based SSL_chronic_mg/kg",
                  "Protection of Groundwater: MCL-based SSL_chronic_mg/kg",
                  "SFO_chronic_(mg/kg-day)-1",
                  "IUR_chronic_(ug/m3)-1",
                  "RfDo_chronic_mg/kg-day",
                  "RfCi_chronic_mg/m3",
                  "GIABS_PhysChem",
                  "ABSd_PhysChem",
                  "Csat_chronic_mg/kg"),
          names_to= "toxval_type",
          values_to= "toxval_numeric",
          values_transform = list(toxval_numeric = as.numeric)
  )


  # separate out toxval_type into other toxval columns
  res_combo_01_4 <- res_combo_01_3 %>%
    tidyr::separate(toxval_type, c("toxval_type", "risk_assessment_class", "toxval_unit"), sep="_", fill="right", remove=TRUE)

  # make new column for the key values to go in
  res_combo_01_4$study_type = res_combo_01_4$toxval_type
  # replace values with key columns values
  res_combo_01_5 <- res_combo_01_4 %>%
    mutate(study_type = ifelse(toxval_type == 'SFO', key1,study_type)) %>%
    mutate(study_type = ifelse(toxval_type == 'IUR', key2, study_type )) %>%
    mutate(study_type = ifelse(toxval_type == 'RfDo', key3, study_type)) %>%
    mutate(study_type = ifelse(toxval_type == 'RfCi', key4, study_type)) %>%
    mutate(study_type = ifelse(toxval_type == 'Screening Level (Resident Soil)', key5, study_type)) %>%
    mutate(study_type = ifelse(toxval_type == 'Screening Level (Industrial Soil)', key6, study_type)) %>%
    mutate(study_type = ifelse(toxval_type == 'Screening Level (Resident Air)', key7, study_type)) %>%
    mutate(study_type = ifelse(toxval_type == 'Screening Level (Industrial Air)', key8, study_type)) %>%
    mutate(study_type = ifelse(toxval_type == 'Screening Level (Tap Water)', key9, study_type)) %>%
    mutate(study_type = ifelse(toxval_type == 'Protection of Groundwater: Risk-based SSL', key10, study_type)) %>%
    mutate(study_type = ifelse(toxval_type == 'Screening Level (MCL)', risk_assessment_class, study_type))

  # make dictionary of values in annotation column that needs to go
  rep_str = c('SFO'='',
              'IUR'='',
              'RfDo'='',
              'RfCi'='',
              'Screening Level (Resident Soil)'='',
              'Screening Level (Industrial Soil)'='',
              'Screening Level (Resident Air)'='',
              'Screening Level (Industrial Air)'='',
              'Screening Level (Tap Water)'='',
              'Protection of Groundwater: Risk-based SSL' = '',
              'mutagen' = '',
              'GIABS' = '',
              'ABSd' = '',
              'Csat' = '',
              'Protection of Groundwater: MCL-based SSL' = '',
              'Screening Level (MCL)' = '',
              'chronic' = '',
              'PhysChem' = '')

  # get rid of those values in annotation columns
  res_combo_01_5$study_type <- str_replace_all(res_combo_01_5$study_type, rep_str)


  # combine res2 (subchronic table) and the combined res0/res1 tables (thq table)
  res3 <- bind_rows(res_combo_01_5, res2_4)

  res4 <- res3 %>%
    # replace key abbreviated values with text values using dictionary
    dplyr::mutate(across(matches("key|vol|study_type"),
                  .fns = ~ case_when(. =="I" ~ "IRIS",
                            . =="P" ~ "PPRTV",
                            . =="O" ~ "OPP",
                            . =="A" ~ "ATSDR",
                            . =="C" ~ "Cal EPA",
                            . =="X" ~ "PPRTV Screening Level",
                            . =="H" ~ "HEAST",
                            . =="D" ~ "OW",
                            . =="W" ~ "TEF applied",
                            . =="E" ~ "RPF applied",
                            . =="G" ~ "see user's guide",
                            . =="U" ~ "user provided",
                            . =="c" ~ "cancer",
                            . =="n" ~ "noncancer",
                            . =="max" ~ "ceiling limit exceeded",
                            . =="m" ~ "ceiling limit exceeded",
                            . =="sat" ~ "Csat exceeded",
                            . =="s" ~ "Csat exceeded",
                            . =="c*" ~ "cancer where noncancer SL < 100X cancer SL",
                            . =="c**" ~ "cancer where noncancer SL < 10X cancer SL",
                            . =="nm" ~ "noncancer, ceiling limit exceeded",
                            . =="ns" ~ "noncancer, Csat exceeded",
                            . =="nms" ~ "noncancer, ceiling limit exceeded, Csat exceeded",
                            . =="DAF" ~ "dilution attentuation factor",
                            . =="c*R" ~ "cancer where noncancer SL < 100X cancer SL",
                            . =="c**R" ~ "cancer where noncancer SL < 10X cancer SL",
                            . =="cR" ~ "cancer",
                            # . == "V" ~ "Vol",
                            TRUE ~ .
                            )
                  )
           )

 # adding source_url column
 res5 <- res4 %>%
   mutate(source_url= "https://www.epa.gov/risk/regional-screening-levels-rsls-generic-tables")
 # dropping columns without a toxval_numeric value for they are not needed
 res6 <- res5 # %>%
   # drop_na(toxval_numeric)
 # drop key and reference columns
 res7 <- res6[ , !names(res6) %in% 
       c("key1","key2","key3","key4","key5","key6","key7","key8",
         "key9","key10","RfD Reference","SRfD Reference",
         "RfC Reference","SRfC Reference")]
 
 # make subsource column and leave cancer/noncancer values in annotation column
 res7$subsource = res7$study_type
 
 res7$subsource<-gsub("^noncancer","",as.character(res7$subsource))
 res7$subsource<-gsub("^cancer where noncancer SL < 100X cancer SL","",as.character(res7$subsource))
 res7$subsource<-gsub("^cancer where noncancer SL < 10X cancer SL","",as.character(res7$subsource))
 res7$subsource<-gsub("^cancer","",as.character(res7$subsource))
 res7$subsource<-gsub("^noncancer, ceiling limit exceeded","",as.character(res7$subsource))
 res7$subsource<-gsub("^noncancer, Csat exceeded","",as.character(res7$subsource))
 res7$subsource<-gsub("^noncancer, ceiling limit exceeded, Csat exceeded","",as.character(res7$subsource))
 
 
 res8 <- res7 %>%
 dplyr::mutate(across(matches("study_type"),
                      .fns = ~ case_when(. =="IRIS" ~ "",
                                         . =="PPRTV" ~ "",
                                         . =="OPP" ~ "",
                                         . =="ATSDR" ~ "",
                                         . =="Cal EPA" ~ "",
                                         . =="PPRTV Screening Level" ~ "",
                                         . =="HEAST" ~ "",
                                         . =="OW" ~ "",
                                         . =="TEF applied" ~ "",
                                         . =="RPF applied" ~ "",
                                         . =="see user's guide" ~ "",
                                         . =="user provided" ~ "",
                                         . =="ceiling limit exceeded" ~ "",
                                         . =="ceiling limit exceeded" ~ "",
                                         . =="Csat exceeded" ~ "",
                                         . =="Csat exceeded" ~ "",
                                         . =="dilution attentuation factor" ~ "",
                                         . =="SCREEN" ~ "",
                                         . =="DWSHA" ~ "",
                                         # . == "V" ~ "Vol",
                                         TRUE ~ .
                                         )
                            )
          )

 # add "source" column and fill it with 'RSL'
 res8$source <- 'RSL'
 
 # add 'EPA Regions' to "subsource" column for all 'Screening' toxval_type entries
   res9 <- res8 %>%
     dplyr::mutate(across(matches("subsource"),
                          .fns = ~ case_when(toxval_type =="Screening Level (Industrial Soil)" ~ "EPA Regions",
                                             toxval_type =="Screening Level (Resident Air)" ~ "EPA Regions",
                                             toxval_type =="Screening Level (Industrial Air)" ~ "EPA Regions",
                                             toxval_type == "Screening Level (Tap Water)" ~ "EPA Regions",
                                             toxval_type == "Screening Level (Resident Soil)" ~ "EPA Regions",
                                             toxval_type == "Screening Level (MCL)" ~ "EPA Regions",
                                             TRUE ~ .
                          )
                )
     )
   
  # make "risk_assessment_type" column and fill it with 'carcinogenicity' for all 'SFO', 'IUR' toxval_type entries 
   res10 <- res9 %>%
     add_column(risk_assessment_type =  "") %>%
     dplyr::mutate(across(matches("risk_assessment_type"),
                          .fns = ~ case_when(toxval_type =="SFO" ~ "carcinogenicity",
                                             toxval_type =="IUR" ~ "carcinogenicity",
                                             TRUE ~ .
                          )
                )
     )
   
   
   # make "toxval_subtype" column for all 'Screening' toxval_type entries with a "cancer" risk_assessment_class
   res10 <- mutate(res10, toxval_subtype = case_when(toxval_type =="Screening Level (Industrial Soil)"& study_type == "cancer"  ~ "TR=1E-06",
                                                     toxval_type =="Screening Level (Resident Air)"& study_type == "cancer" ~ "TR=1E-06",
                                                     toxval_type =="Screening Level (Industrial Air)"& study_type == "cancer" ~ "TR=1E-06",
                                                     toxval_type == "Screening Level (Tap Water)"& study_type == "cancer" ~ "TR=1E-06",
                                                     toxval_type == "Screening Level (Resident Soil)"& study_type == "cancer" ~ "TR=1E-06",
                                                     toxval_type == "Screening Level (MCL)"& study_type == "cancer" ~ "TR=1E-06",
                                                     toxval_type =="Screening Level (Industrial Soil)"& study_type == "cancer where noncancer SL < 100X cancer SL"  ~ "TR=1E-06",
                                                     toxval_type =="Screening Level (Resident Air)"& study_type == "cancer where noncancer SL < 100X cancer SL" ~ "TR=1E-06",
                                                     toxval_type =="Screening Level (Industrial Air)"& study_type == "cancer where noncancer SL < 100X cancer SL" ~ "TR=1E-06",
                                                     toxval_type == "Screening Level (Tap Water)"& study_type == "cancer where noncancer SL < 100X cancer SL" ~ "TR=1E-06",
                                                     toxval_type == "Screening Level (Resident Soil)"& study_type == "cancer where noncancer SL < 100X cancer SL" ~ "TR=1E-06",
                                                     toxval_type == "Screening Level (MCL)"& study_type == "cancer where noncancer SL < 100X cancer SL" ~ "TR=1E-06",
                                                     toxval_type =="Screening Level (Industrial Soil)"& study_type == "cancer where noncancer SL < 10X cancer SL"  ~ "TR=1E-06",
                                                     toxval_type =="Screening Level (Resident Air)"& study_type == "cancer where noncancer SL < 10X cancer SL" ~ "TR=1E-06",
                                                     toxval_type =="Screening Level (Industrial Air)"& study_type == "cancer where noncancer SL < 10X cancer SL" ~ "TR=1E-06",
                                                     toxval_type == "Screening Level (Tap Water)"& study_type == "cancer where noncancer SL < 10X cancer SL" ~ "TR=1E-06",
                                                     toxval_type == "Screening Level (Resident Soil)"& study_type == "cancer where noncancer SL < 10X cancer SL" ~ "TR=1E-06",
                                                     toxval_type == "Screening Level (MCL)"& study_type == "cancer where noncancer SL < 10X cancer SL" ~ "TR=1E-06",
                                                     TRUE ~ toxval_subtype
                         )
   )
   
   
   # update "exposure_route" column based on "toxval_type"
   res10 <- mutate(res10, exposure_route = case_when(toxval_type =="RfCi"  ~ "inhalation",
                                                     toxval_type =="RfDo"  ~ "oral",
                                                     toxval_type =="Protection of Groundwater: MCL-based SSL"  ~ "oral",
                                                     toxval_type =="Protection of Groundwater: Risk-based SSL"  ~ "oral",
                                                     toxval_type =="Screening Level (Resident Soil)"  ~ "oral, dermal, and inhalation",
                                                     toxval_type =="Screening Level (Industrial Soil)"  ~ "oral and inhalation",
                                                     toxval_type =="Screening Level (Resident Air)"  ~ "inhalation",
                                                     toxval_type =="Screening Level (Industrial Air)"  ~ "inhalation",
                                                     toxval_type =="Screening Level (Tap Water)"  ~ "oral, dermal, and inhalation",
                                                     toxval_type =="Screening Level (MCL)"  ~ "vary by chemical",
                                                     TRUE ~ exposure_route
                                )
           )

 # relacing empty strings with NA value
 res <- replace(res10, res10=="", NA)
 
 


  # the final file should have column names that include "name" and "casrn"
  # additionally, the names in res need to match names in the source
  # database table. You do not need to add any of the generic columns
  # described in the SOP - they will get added in source_prep_and_load
  #

  # Standardize the names
  names(res) <- names(res) %>%
    stringr::str_squish() %>%
    # Replace whitespace and periods with underscore
    gsub("[[:space:]]|[.]", "_", .) %>%
    tolower()

  #####################################################################
  cat("Prep and load the data\n")
  #####################################################################
  source_prep_and_load(db,source=source,table=source_table,res=res,F,T,T)
}
