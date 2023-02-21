#--------------------------------------------------------------------------------------
#' Import of rsl 2022 source into toxval_source
#'
#' @param db The version of toxval_source into which the source is loaded.
#' @param infile1 The input file ./rsl/rsl_files/rsl_thq10_nov_2022.xlsx
#' @param infile2 The input file ./rsl/rsl_files/rsl_thq01_nov_2022.xlsx
#' @param infile3 The input file ./rsl/rsl_files/rsl_subchronic_nov_2022.xlsx
#' @param chem.check.halt If TRUE and there are bad chemical names or casrn,
#--------------------------------------------------------------------------------------
import_generic_source <- function(db,chem.check.halt=F) {
  printCurrentFunction(db)
  source = "rsl"
  source_table = "source_rsl"
  dir = paste0(toxval.config()$datapath,"rsl/rsl_files/")
  file0 = paste0(dir,"rsl_thq10_nov_2022.xlsx")
  file1 = paste0(dir,"rsl_thq01_nov_2022.xlsx")
  file2 = paste0(dir,"rsl_subchronic_nov_2022.xlsx")

  res0 = readxl::read_xlsx(file0)
  res1 = readxl::read_xlsx(file1)
  res2 = readxl::read_xlsx(file2)
  #####################################################################
  cat("Do any non-generic steps to get the data ready \n")
  #####################################################################
 # drop first couple header columns
  colnames(res0) <- res0[2, ]  
  colnames(res1) <- res1[2, ]  
  colnames(res2) <- res2[2, ]  
  res0 <- res0[-c(1,2),]
  res1 <- res1[-c(1,2),]
  res2 <- res2[-c(1,2),]
  
  # changes for res2, subchronic table
  # rename columns in a way that they can be separated later for other column values
  res2_1 <- res2 %>%
    dplyr::rename( "SRfCi_subchronic_inhalation_mg/m3" = "SRfCi(mg/m3)",
                   "SRfDo_subchronic_oral_mg/kg-day" = "SRfDo(mg/kg-day)",
                   "RfDo_chronic_oral_mg/kg-day" = "RfDo(mg/kg-day)",
                   "RfCi_chronic_inhalation_mg/m3" = "RfCi(mg/m3)",
                   "casrn" = "CAS No.",
                   "name" = "Analyte",
                   "RfD Reference" = "RfD\r\nReference",
                   "SRfC Reference" = "SRfC\r\nReference",
                   "SRfD Reference" = "SRfD\r\nReference",
                   "RfC Reference" = "RfC\r\nReference") 
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
  # seperate out toxval_type to make other toxval columns and add input file column
  res2_3 <- res2_2 %>%
  tidyr::separate(toxval_type, c("toxval_type", "risk_assessment", "exposure_route","toxval_unit"), sep="_", fill="right", remove=TRUE) %>%
    mutate(raw_input_file= "rsl_subchronic_nov_2022.xlsx")
  
  # make new column for the key values to go in 
  res2_3$annotation = res2_3$toxval_type
  # replace values with key columns values
  res2_4 <- res2_3 %>%
    mutate(annotation = ifelse(toxval_type == 'SRfCi', res2_3$`SRfC Reference`,annotation)) %>% 
    mutate(annotation = ifelse(toxval_type == 'SRfDo', res2_3$`SRfD Reference`, annotation )) %>%
    mutate(annotation = ifelse(toxval_type == 'RfDo', res2_3$`RfD Reference`, annotation)) %>%
    mutate(annotation = ifelse(toxval_type == 'RfCi', res2_3$`RfC Reference`, annotation))
  
  # make dictionary of values in annotation column that needs to go
  rep_str0 = c('SRfCi'='',
              'SRfDo'='',
              'RfDo'='',
              'RfCi'='')
  
  # get rid of those values in annotation columns
  res2_4$annotation <- str_replace_all(res2_4$annotation, rep_str0)
  
          
  # add input file columns and toxval_subtype columns to res0 and res1, thq tables
  res0_0 <- res0 %>%
    mutate(raw_input_file= "rsl_thq10_nov_2022.xlsx_", 
           toxval_subtype = "Thq = 1")
  res1_0 <- res1 %>%
    mutate(raw_input_file= "rsl_thq01_nov_2022.xlsx",
           toxval_subtype = "Thq = 0.1")
  
  # combine res0 and res1
  res_combo_01 <- bind_rows(res0_0, res1_0)
  
  # rename columsn in a way that they can be separated later for other toxval column values
  res_combo_01_2 <- res_combo_01 %>%
    dplyr::rename("Screening Level (Resident Soil)_chronic_mg/kg" = "Resident Soil\r\n(mg/kg)",
                  "Screening Level (Industrial Soil)_chronic_mg/kg" = "Industrial Soil\r\n(mg/kg)",
                  "Screening Level (Resident Air)_chronic_ug/m3" = "Resident Air\r\n(ug/m3)",
                  "Screening Level (Industrial Air)_chronic_ug/m3" = "Industrial Air\r\n(ug/m3)",
                  "Screening Level (Tap Water)_chronic_ug/L" = "Tap Water\r\n(ug/L)",
                  "Screening Level (MCL)_chronic_ug/L" = "MCL\r\n(ug/L)",
                  "Protection of Groundwater: Risk-based SSL_chronic_mg/kg" = "Risk-based\r\nSSL\r\n(mg/kg)",
                  "Protection of Groundwater: MCL-based SSL_chronic_mg/kg" = "MCL-based\r\nSSL\r\n(mg/kg)",
                  "SFO_chronic_(mg/kg-day)-1" = "SFO\r\n(mg/kg-day)-1",
                  "IUR_chronic_(ug/m3)-1" = "IUR\r\n(ug/m3)-1",
                  "RfDo_chronic_mg/kg-day" = "RfDo(mg/kg-day)",
                  "RfCi_chronic_mg/m3" = "RfCi(mg/m3)",
                  "Csat_chronic_mg/kg" = "Csat(mg/kg)",
                  "GIABS_PhysChem" = "GIABS",
                  "ABSd_PhysChem" = "ABSd",
                  "vol" = "v\r\no\r\nl",
                  "casrn" = "CAS No.",
                  "name" = "Analyte",
                  "key1" = 2,
                  "key2" = 4,
                  "key3" = 6,
                  "key4" = 8,
                  "key5" = 17,
                  "key6" = 19,
                  "key7" = 21,
                  "key8" = 23,
                  "key9" = 25,
                  "key10" = 28)

  
  # make table longer based on toxval_type
  res_combo_01_3 <- res_combo_01_2 %>%
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
                  "mutagen",
                  "GIABS_PhysChem",
                  "ABSd_PhysChem",
                  "Csat_chronic_mg/kg"),
          names_to= "toxval_type",
          values_to= "toxval_numeric"
  )
  
  
  # separate out toxval_type into other toxval columns
  res_combo_01_4 <- res_combo_01_3 %>%
    tidyr::separate(toxval_type, c("toxval_type", "risk_assessment", "toxval_unit"), sep="_", fill="right", remove=TRUE)
  
  # make new column for the key values to go in 
  res_combo_01_4$annotation = res_combo_01_4$toxval_type
  # replace values with key columns values
  res_combo_01_5 <- res_combo_01_4 %>%
    mutate(annotation = ifelse(toxval_type == 'SFO', key1,annotation)) %>% 
    mutate(annotation = ifelse(toxval_type == 'IUR', key2, annotation )) %>%
    mutate(annotation = ifelse(toxval_type == 'RfDo', key3, annotation)) %>%
    mutate(annotation = ifelse(toxval_type == 'RfCi', key4, annotation)) %>%
    mutate(annotation = ifelse(toxval_type == 'Screening Level (Resident Soil)', key5, annotation)) %>%
    mutate(annotation = ifelse(toxval_type == 'Screening Level (Industrial Soil)', key6, annotation)) %>%
    mutate(annotation = ifelse(toxval_type == 'Screening Level (Resident Air)', key7, annotation)) %>%
    mutate(annotation = ifelse(toxval_type == 'Screening Level (Industrial Air)', key8, annotation)) %>%
    mutate(annotation = ifelse(toxval_type == 'Screening Level (Tap Water)', key9, annotation)) %>%
    mutate(annotation = ifelse(toxval_type == 'Protection of Groundwater: Risk-based SSL', key10, annotation)) %>%
    mutate(annotation = ifelse(toxval_type == 'Screening Level (MCL)', risk_assessment, annotation)) 
  
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
  res_combo_01_5$annotation <- str_replace_all(res_combo_01_5$annotation, rep_str)

  
  # combine res2 (subchronic table) and the combined res0/res1 tables (thq table)
  res3 <- bind_rows(res_combo_01_5, res2_4)
  
 res4 <- res3 %>%
 # replace key abbreviated values with text values using dictionary
  mutate_all(~case_when(. =="I" ~"IRIS", 
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
                        . =="nm" ~ "noncancer; ceiling limit exceeded",
                        . =="ns" ~ "noncancer; Csat exceeded",
                        . =="nms" ~ "noncancer; ceiling limit exceeded; Csat exceeded",
                        . =="DAF" ~ "dilution attentuation factor",
                        . =="c*R" ~ "cancer where noncancer SL < 100X cancer SL; R",
                        . =="c**R" ~ "cancer where noncancer SL < 10X cancer SL; R",
                        . =="cR" ~ "cancer; R",
                        TRUE ~ .)) 
 
 # adding source_url column
 res5 <- res4 %>%
   mutate(source_url= "https://www.epa.gov/risk/regional-screening-levels-rsls-generic-tables") 
 # dropping columns without a toxval_numeric value for they are not needed
 res6 <- res5 %>%
   drop_na(toxval_numeric)
 # relacing empty strings with NA value
 res <- replace(res6, res6=="", NA)


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
  
  res = source.specific.transformations(res5)
  
  
  #####################################################################
  cat("Prep and load the data\n")
  #####################################################################
  source_prep_and_load(db,source=source,table=source_table,res=res,F,T,T)
}
