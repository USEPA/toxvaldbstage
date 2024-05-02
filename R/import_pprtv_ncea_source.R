#--------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------
#' @title import_pprtv_ncea_source
#' @description Load PPRTV (NCEA) data to toxval_source
#' @param db The version of toxval_source into which the source is loaded.
#' @param chem.check.halt If TRUE and there are bad chemical names or casrn,
#' @param do.reset If TRUE, delete data from the database for this source before
#' @param do.insert If TRUE, insert data into the database, default FALSE
#' @return None; data is pushed to ToxVal
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[purrr]{map}}
#'  \code{\link[readxl]{read_excel}}
#'  \code{\link[readr]{read_delim}}
#'  \code{\link[dplyr]{mutate}}, \code{\link[dplyr]{mutate-joins}}, \code{\link[dplyr]{select}}, \code{\link[dplyr]{group_by}}, \code{\link[dplyr]{summarise}}, \code{\link[dplyr]{filter}}, \code{\link[dplyr]{case_when}}, \code{\link[dplyr]{rename}}, \code{\link[dplyr]{distinct}}, \code{\link[dplyr]{na_if}}, \code{\link[dplyr]{across}}
#'  \code{\link[tidyselect]{all_of}}
#'  \code{\link[tidyr]{separate}}, \code{\link[tidyr]{unite}}, \code{\link[tidyr]{drop_na}}
#'  \code{\link[stringr]{str_trim}}
#' @rdname import_generic_source
#' @export
#' @importFrom purrr map
#' @importFrom readxl read_xlsx
#' @importFrom readr read_csv
#' @importFrom dplyr mutate left_join select group_by summarize filter case_when rename distinct na_if across ungroup
#' @importFrom tidyselect all_of any_of
#' @importFrom tidyr separate unite drop_na
#' @importFrom stringr str_squish
import_pprtv_ncea_source <- function(db,chem.check.halt=FALSE, do.reset=FALSE, do.insert=FALSE) {
  printCurrentFunction(db)
  source <- "PPRTV (NCEA)"
  source_table = "source_pprtv_ncea"
  # Date provided by the source or the date the data was extracted
  src_version_date = as.Date("2020-04-08")

  # Maintain old logic (with Tidyverse updates where applicable) for reading files

  # Initialize filepaths
  csvfile="dose_reg2.csv"
  scrapepath="PPRTV_scrape2020-04-08.xlsx"
  filepath = paste0(toxval.config()$datapath,"pprtv_ncea/pprtv_ncea_files/")
  csvfile = paste0(toxval.config()$datapath,"pprtv_ncea/pprtv_ncea_files/",csvfile)
  scrapepath = paste0(toxval.config()$datapath,"pprtv_ncea/",scrapepath)

  #####################################################################
  cat("Build input pprtv_ncea tibble\n")
  #####################################################################
  # Get list of relevant files
  files.list <- list.files(path = filepath, pattern = "*.xlsx")
  any_temp_files <- grep("^\\~\\$.*", files.list, value = TRUE)
  files.list <- files.list[! files.list %in% any_temp_files]
  files.list <- paste0(filepath, files.list)

  # Read files into tibbles
  res_list <- files.list %>%
    purrr::map(\(x) readxl::read_xlsx(x, col_types = "text"))

  # Add CSV and scrape file information to res_list
  pprtv_ncea_25 <- readr::read_csv(csvfile, show_col_types=FALSE)
  res_list <- c(res_list, list(pprtv_ncea_25))
  pprtv_ncea_26 <- readxl::read_xlsx(scrapepath, sheet=1)
  res_list <- c(res_list, list(pprtv_ncea_26))

  # Delete separate CSV/scrape tibble objects
  rm(pprtv_ncea_25)
  rm(pprtv_ncea_26)

  #####################################################################
  cat("Provide dataframe names prefixed with pprtv_ncea and update column names by making necessary substitutions\n")
  #####################################################################
  res_list_names <- c('assessment_study', 'assessments','cancer', 'cancer_tt','cancer_types','chem_groups',
                      'dose','dose_reg','dosimetry_lkp', 'dosimetry_rqd', 'endpoints',
                      'endpoints_tier','exposure_route','exposure_units','PPRTV_scrape_11_2017',
                      'ref_value_type','ref_value_units','reference','reference_tt','species',
                      'study','study_type', 'tissue_gen','tissue_gen_types','dose_reg2','new_scrape_table')
  names(res_list) <- paste0("pprtv_ncea_", res_list_names)

  # Handle difference between Linux and Windows file sorting (flips endpoints and endpoints_tier load order)
  if(is.null(res_list$pprtv_ncea_endpoints$Details)){
    names(res_list)[names(res_list) == "pprtv_ncea_endpoints"] <- "tmp_swap"
    names(res_list)[names(res_list) == "pprtv_ncea_endpoints_tier"] <- "pprtv_ncea_endpoints"
    names(res_list)[names(res_list) == "tmp_swap"] <- "pprtv_ncea_endpoints_tier"
  }

  #####################################################################
  cat("Subset the source list of dataframes by excluding duplicated dataframes(cancer and reference)\n")
  #####################################################################
  res_list <- res_list[!(names(res_list)) %in% c("pprtv_ncea_cancer_tt","pprtv_ncea_reference_tt")]

  #####################################################################
  cat("Endpoints file has a header column in row 65534, hence have to remove it\n")
  #####################################################################
  res_list$pprtv_ncea_endpoints <- res_list$pprtv_ncea_endpoints[-65534,]

  #####################################################################
  cat("Create a data frame to house all replacement column names and use it to
      replace the existing column names\n")
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

  for (i in 1:length(res_list)){
    for (j in 1:nrow(colnames_res_to_change)){
      names(res_list[[i]])[match(colnames_res_to_change$cols_2_rename[j], names(res_list[[i]]))] = colnames_res_to_change$renamed_cols[j]
    }
  }

  #####################################################################
  cat("Fix encoding changes\n")
  #####################################################################
  res_list$pprtv_ncea_endpoints = res_list$pprtv_ncea_endpoints %>%
    dplyr::mutate(Details = iconv(Details,"UTF-8","ASCII","-"))

  res_list$pprtv_ncea_PPRTV_scrape_11_2017 =  res_list$pprtv_ncea_PPRTV_scrape_11_2017 %>%
    dplyr::mutate(`Effect Level` = iconv(`Effect Level`,"UTF-8","ASCII"," "))

  res_list$pprtv_ncea_new_scrape_table = res_list$pprtv_ncea_new_scrape_table %>%
    dplyr::mutate(
      `Effect Level` = iconv(`Effect Level`,"latin1","ASCII","~"),
      toxval_numeric = iconv(toxval_numeric,"latin1","ASCII","~"),
      Species = iconv(Species,"UTF-8","ASCII"," "),
      Route = iconv(Route,"UTF-8","ASCII"," "),
      Duration = iconv(Duration,"UTF-8","ASCII"," "),
      Confidence = iconv(Confidence,"UTF-8","ASCII"," "),
      Target = iconv(Target,"UTF-8","ASCII"," "),
      `Critical Effect` = iconv(`Critical Effect`,"UTF-8","ASCII"," "),
      UF = iconv(UF,"UTF-8","ASCII"," "),
    )

  ####################################################################
  cat("Build res from input files\n")
  #####################################################################
  # Transition to using dplyr rather than creating database tables
  res0 = res_list$pprtv_ncea_assessment_study %>%
    dplyr::mutate(raw_input_file="pprtv_ncea_assessment_study") %>%
    dplyr::left_join(res_list$pprtv_ncea_assessments %>%
                       dplyr::mutate(raw_input_file="pprtv_ncea_assessment"),
                     by = "Assessment ID") %>%

    dplyr::left_join(res_list$pprtv_ncea_study %>%
                       dplyr::mutate(raw_input_file="pprtv_ncea_study"),
                     by = "Study ID") %>%

    dplyr::left_join(res_list$pprtv_ncea_dose_reg %>%
                       dplyr::select(tidyselect::all_of(c("AsmtStudy_ID", "Animal Species", "Animal Strain",
                                                          "Route of Exposure", "Duration Classification",
                                                          "Duration of Study", "Duration Units", "Gender"))) %>%
                       dplyr::mutate(raw_input_file="pprtv_ncea_dose_reg") %>%
                       dplyr::group_by(`AsmtStudy_ID`, `Animal Species`, `Animal Strain`, `Route of Exposure`,
                                       `Duration Classification`, `Duration of Study`, `Duration Units`) %>%
                       dplyr::summarize(sex = toString(unique(Gender)), .groups = "keep"),
                     by = "AsmtStudy_ID") %>%

    dplyr::left_join(res_list$pprtv_ncea_reference %>%
                       dplyr::mutate(raw_input_file="pprtv_ncea_reference"),
                     by = c("Assessment ID", "Study ID" = "StudyID"),
                     relationship="many-to-many") %>%

    dplyr::select(casrn=CASRN,name=`Chemical Name`,RFV_ID,toxval_type=`Reference Value Type`,toxval_numeric=`Reference Value`,
                  toxval_units=`RfV Units`,study_type=StudyType,`Tissue_Gen`,phenotype=Endpoint,
                  POD_numeric=`Point of Departure`,POD_type=`Source of POD`,POD_units=`PoD Units`,
                  UF_A,UF_D,UF_H,UF_L,UF_S,UF_C,year=`Study_Year`,Author,title=`Study_Title`,long_ref=`Full Reference`,
                  species=`Animal Species`,strain=`Animal Strain`,sex,`Route of Exposure`,study_duration_class=`Duration Classification`,
                  study_duration_value=`Duration of Study`,study_duration_units=`Duration Units`,
                  raw_input_file.x, raw_input_file.x.x, raw_input_file.y, raw_input_file.y.y) %>%

    tidyr::separate(`Route of Exposure`, c("exposure_route", "exposure_method"), sep = " - ", fill = "right") %>%
    tidyr::unite(col="raw_input_file",
                 raw_input_file.x, raw_input_file.x.x, raw_input_file.y, raw_input_file.y.y,
                 sep="; ",
                 remove=TRUE,
                 na.rm=TRUE)

  #####################################################################
  cat("Do any non-generic steps to get the data ready\n")
  #####################################################################
  res = res0 %>%
    # Add transformations from old load script
    dplyr::filter(casrn != "VARIOUS") %>%
    dplyr::mutate(
      # Add new columns
      phenotype = stringr::str_squish(phenotype),
      subsource = "EPA ORD",
      source_url = "https://www.epa.gov/pprtv/basic-information-about-provisional-peer-reviewed-toxicity-values-pprtvs",
      human_eco = "human_health",
      species = species %>%
        tolower() %>%
        stringr::str_squish(),
      strain = strain %>%
        tolower() %>%
        stringr::str_squish(),

      # Manually adjust erroneous casrn
      casrn = dplyr::case_when(
        name == "Inorganic Phosphates" ~ "98059-61-1",
        TRUE ~ casrn
      ),

      # Clean study fields (keep ranged values in import)
      study_duration_value = study_duration_value %>%
        gsub(" \\- ", "-", .) %>%
        stringr::str_squish(),
      study_duration_units = study_duration_units %>%
        tolower() %>%
        stringr::str_squish(),
      study_duration_class = study_duration_class %>%
        tolower(),
      study_type = study_type %>%
        tolower() %>%
        stringr::str_squish(),

      # Clean exposure fields
      exposure_route = exposure_route %>%
        gsub("\\s.+", "", .) %>%
        tolower() %>%
        stringr::str_squish(),
      exposure_method = exposure_method %>%
        tolower() %>%
        stringr::str_squish(),

      # Clean sex field
      sex = dplyr::case_when(
        grepl("F", sex) & grepl("M", sex) ~ "male/female",
        grepl("F", sex) ~ "female",
        grepl("M", sex) ~ "male",
        TRUE ~ as.character(NA),
      )
    ) %>%
    tidyr::unite(col="critical_effect", `Tissue_Gen`, phenotype,
                 sep = ": ", na.rm = TRUE, remove = FALSE)

  # Separate out POD data and add back to res
  pod_res = res %>%
    dplyr::select(!tidyselect::any_of(c("toxval_type", "toxval_numeric", "toxval_units"))) %>%
    dplyr::rename(toxval_type=POD_type, toxval_numeric=POD_numeric, toxval_units=POD_units)

  res = res %>%
    dplyr::select(!tidyselect::any_of(c("POD_type", "POD_numeric", "POD_units"))) %>%
    rbind(pod_res) %>%

    dplyr::distinct() %>%

    dplyr::mutate(
      critical_effect = critical_effect %>%
        dplyr::na_if(""),
      # Preserve toxval_type additional detail from units
      toxval_type_2 = toxval_units %>%
        stringr::str_extract("ADD|HED|HEC") %>%
        # Add ending parentheses for later unite with toxval_type
        paste0(")") %>%
        gsub("NA\\)", NA, .),
      # Clean toxval_units
      toxval_units = toxval_units %>%
        fix.replace.unicode() %>%
        gsub("ADD|HED|HEC", "", .) %>%
        stringr::str_squish(),

      # Add appropriate species/human_ra based on toxval_type
      species = dplyr::case_when(
        grepl("RfC|RfD", toxval_type) ~ as.character(NA),
        TRUE ~ species
      ),
      human_ra = dplyr::case_when(
        grepl("RfC|RfD", toxval_type) ~ "Y",
        TRUE ~ "N"
      ),
      target_species = dplyr::case_when(
        grepl("RfC|RfD", toxval_type) ~ "human",
        TRUE ~ as.character(NA)
      ),
      strain = dplyr::case_when(
        is.na(species) ~ as.character(NA),
        TRUE ~ strain
      ),

      # Ensure numeric fields are of numeric type
      toxval_numeric = as.numeric(toxval_numeric),
      year = year %>%
        dplyr::na_if("ND") %>%
        as.numeric(year)
    ) %>%
    tidyr::unite(col="toxval_type", toxval_type, toxval_type_2, sep = " (", na.rm = TRUE) %>%

    # Drop entries missing required fields (both name and casrn blank for same entries)
    tidyr::drop_na(toxval_numeric, toxval_units, toxval_type, name) %>%
    dplyr::distinct() %>%
    dplyr::mutate(toxval_subtype = toxval_type)

  # Standardize the names
  names(res) <- names(res) %>%
    stringr::str_squish() %>%
    # Replace whitespace and periods with underscore
    gsub("[[:space:]]|[.]", "_", .) %>%
    tolower()

  # Fill blank hashing cols
  res[, toxval.config()$hashing_cols[!toxval.config()$hashing_cols %in% names(res)]] <- "-"

  # Perform deduping
  res = toxval.source.import.dedup(res,
                                   # dedup_fields=c("uf_a", "uf_d", "uf_h", "uf_l", "uf_s", "uf_c", "rfv_id")
  )

  # Add version date. Can be converted to a mutate statement as needed
  res$source_version_date <- src_version_date
  #####################################################################
  cat("Prep and load the data\n")
  #####################################################################
  source_prep_and_load(db=db,
                       source=source,
                       table=source_table,
                       res=res,
                       do.reset=do.reset,
                       do.insert=do.insert,
                       chem.check.halt=chem.check.halt,
                       hashing_cols=toxval.config()$hashing_cols)
}
