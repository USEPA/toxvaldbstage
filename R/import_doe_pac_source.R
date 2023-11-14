#' @description Load DOE Source into toxval_source
#' @param db The version of toxval_source into which the source is loaded.
#' @param infile The input file ./doe/doe_files/source_doe_pac_oct_2023.xlsx
#' @param chem.check.halt If TRUE, stop if there are problems with the chemical mapping
#' @title import_doe_pac_source
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[readxl]{read_excel}}
#'  \code{\link[dplyr]{mutate}}, \code{\link[dplyr]{filter}}, \code{\link[dplyr]{select}}, \code{\link[dplyr]{across}}, \code{\link[dplyr]{rename}}, \code{\link[dplyr]{c("rowwise", "rowwise", "rowwise")}}, \code{\link[dplyr]{distinct}}
#'  \code{\link[tidyr]{pivot_longer}}, \code{\link[tidyr]{reexports}}, \code{\link[tidyr]{separate}}, \code{\link[tidyr]{replace_na}}
#'  \code{\link[stringr]{str_trim}}, \code{\link[stringr]{str_replace}}, \code{\link[stringr]{str_extract}}
#' @rdname import_doe_pac_source
#' @export
#' @importFrom readxl read_xlsx
#' @importFrom dplyr mutate filter select across rename rowwise distinct
#' @importFrom tidyr pivot_longer all_of separate replace_na
#' @importFrom stringr str_squish str_replace_all str_extract
#--------------------------------------------------------------------------------------
import_doe_pac_source <- function(db,
                                  infile="source_doe_pac_oct_2023.xlsx",
                                  chem.check.halt=FALSE,
                                  do.reset=FALSE,
                                  do.insert=FALSE) {
  printCurrentFunction(db)
  source = "doe_pac"
  source_table = "source_{source}"

  # Date provided by the source or the date the data was extracted
  src_version_date = as.Date("2023-10-01")

  dir = paste0(toxval.config()$datapath,"{source}/{source}_files/")
  
  file = paste0(dir, infile)
  
  # NOTE: Information on REC TEELS sheet is redundant (included on Input sheet)
  # RELEVANT SOURCE TEXT: "Table 2 PACs by Chemical Name is a list of the same
  # PAC values as presented in Table 1, but only shows the PACs and Source for
  # the PACs values. They are presented in either ppm or mg/m³."
  res0 <- readxl::read_xlsx(file, sheet=1, range="A4:V3149", col_names=FALSE)

  #####################################################################
  cat("Build new_doe_table\n")

  # Extract header  
  header <- readxl::read_xlsx(file, sheet=1, range="A1:V2", col_names=FALSE)
  header <- unlist(header)
  header <- unname(header[!is.na(header)])
  
  # Add original column names
  names(res0)[1:10] = header[1:10]
  
  # Vapor pressure
  names(res0)[11] = paste0(header[11], ",", header[12])
  names(res0)[12] = paste0(header[11], ",", header[13])
  
  names(res0)[13:14] = header[14:15]
  
  # PAC values
  names(res0)[15] = paste0(header[16], ",", header[17])
  names(res0)[16] = paste0(header[16], ",", header[18])
  names(res0)[17] = paste0(header[16], ",", header[19])
  
  names(res0)[18:19] = header[20:21]
  
  # PAC dates
  names(res0)[20] = paste0(header[22], ",", header[23])
  names(res0)[21] = paste0(header[22], ",", header[24])
  names(res0)[22] = paste0(header[22], ",", header[25])
  
  # Remove excess formatting
  names(res0) <- gsub("\\\r","",names(res0))
  names(res0)[8] <- gsub("\n"," ",names(res0)[8])
  names(res0) <- gsub("\n","",names(res0))
  names(res0) <- gsub("  "," ",names(res0))
  names(res0) <- gsub("  "," ",names(res0))
  
  # Remove artifacts from weight column
  # If weight is shown in a range, then record only the lower weight
  res0$`Molecular Weight (MW)` <- gsub("\\-.*", "", res0$`Molecular Weight (MW)`)
  res0$`Molecular Weight (MW)` <- gsub("(~)", "", res0$`Molecular Weight (MW)`)
  res0$`Molecular Weight (MW)` <- gsub("\\s+$", "", res0$`Molecular Weight (MW)`)
  
  # Handle non-numeric values in Molecular Weight Column
  non_numeric_mw <- grep("(kDa)", res0$`Molecular Weight (MW)`, value = T)
  non_numeric_val <- gsub("(kDa)", "", non_numeric_mw)
  non_numeric_val <- gsub("\\s+$", "", non_numeric_val)
  non_numeric_val <- as.numeric(non_numeric_val)
  non_numeric_val <- 1000 * non_numeric_val
  
  res0$`Molecular Weight (MW)`[res0$`Molecular Weight (MW)` %in% non_numeric_mw] <- non_numeric_val
  res0$`Molecular Weight (MW)` <- as.numeric(res0$`Molecular Weight (MW)`)
  
  # Handle PAC-1
  key_pac1 <- grep("[*]", res0$`PACs based on AEGLs, ERPGs, or TEELs,PAC-1`, value = T)
  pac1_val <- gsub("[*]", "", key_pac1)
  pac1_key <- gsub("[^*]", "", key_pac1)
  res0$pac1_keys[res0$`PACs based on AEGLs, ERPGs, or TEELs,PAC-1` %in% key_pac1] <- pac1_key
  res0$`PACs based on AEGLs, ERPGs, or TEELs,PAC-1`[res0$`PACs based on AEGLs, ERPGs, or TEELs,PAC-1` %in% key_pac1] <- pac1_val
  res0$pac1_keys <- NULL
  
  # Handle PAC-2
  key_pac2 <- grep("[*]", res0$`PACs based on AEGLs, ERPGs, or TEELs,PAC-2`, value = T)
  pac2_val <- gsub("[*]", "", key_pac2)
  pac2_key <- gsub("[^*]", "", key_pac2)
  res0$pac2_keys[res0$`PACs based on AEGLs, ERPGs, or TEELs,PAC-2` %in% key_pac2] <- pac2_key
  res0$`PACs based on AEGLs, ERPGs, or TEELs,PAC-2`[res0$`PACs based on AEGLs, ERPGs, or TEELs,PAC-2` %in% key_pac2] <- pac2_val
  res0$pac2_keys <- NULL
  
  # Handle PAC-3
  key_pac3 <- grep("[*]", res0$`PACs based on AEGLs, ERPGs, or TEELs,PAC-3`, value = T)
  pac3_val <- gsub("[*]", "", key_pac3)
  pac3_key <- gsub("[^*]", "", key_pac3)
  res0$pac3_keys[res0$`PACs based on AEGLs, ERPGs, or TEELs,PAC-3` %in% key_pac3] <- pac3_key
  res0$`PACs based on AEGLs, ERPGs, or TEELs,PAC-3`[res0$`PACs based on AEGLs, ERPGs, or TEELs,PAC-3` %in% key_pac3] <- pac3_val
  res0$pac3_keys <- NULL

  # Ensure that PAC columns are numeric
  res0$`PACs based on AEGLs, ERPGs, or TEELs,PAC-1` <- as.numeric(res0$`PACs based on AEGLs, ERPGs, or TEELs,PAC-1`)
  res0$`PACs based on AEGLs, ERPGs, or TEELs,PAC-2` <- as.numeric(res0$`PACs based on AEGLs, ERPGs, or TEELs,PAC-2`)
  res0$`PACs based on AEGLs, ERPGs, or TEELs,PAC-3` <- as.numeric(res0$`PACs based on AEGLs, ERPGs, or TEELs,PAC-3`)
  
  # Set significant figures to 3 for PAC columns
  res0$`PACs based on AEGLs, ERPGs, or TEELs,PAC-1` <- signif(res0$`PACs based on AEGLs, ERPGs, or TEELs,PAC-1`, digits = 3)
  res0$`PACs based on AEGLs, ERPGs, or TEELs,PAC-2` <- signif(res0$`PACs based on AEGLs, ERPGs, or TEELs,PAC-2`, digits = 3)
  res0$`PACs based on AEGLs, ERPGs, or TEELs,PAC-3` <- signif(res0$`PACs based on AEGLs, ERPGs, or TEELs,PAC-3`, digits = 3)
  
  # Fix BP column
  names(res0)[10] = gsub(" @ 760 mm Hg unless indicated", "", names(res0)[10])
  res0$`BP (°C)`[is.na(res0$`BP (°C)`)] <- 760
  
  # Fix SP column
  names(res0)[13] = gsub(" @ 25°C unless indicated", "", names(res0)[13])
  res0$`SG`[is.na(res0$`SG`)] <- 25

  # Add new column names while maintaining original columns/names
  # Add name column - copy of Chemical Compound column
  res0$`name` = res0$`Chemical Compound`
  
  # Add casrn column - copy of CAS Number (CASRN) column
  res0$`casrn` = res0$`CAS Number (CASRN)`
  
  # Add toxval_type and toxval_numeric columns
  res0 <- res0 %>%
    pivot_longer(cols = c('PACs based on AEGLs, ERPGs, or TEELs,PAC-1',
                          'PACs based on AEGLs, ERPGs, or TEELs,PAC-2',
                          'PACs based on AEGLs, ERPGs, or TEELs,PAC-3'),
                 names_to = 'toxval_type',
                 values_to = 'toxval_numeric'
                 )
  
  # Add toxval_subtype column (empty)
  res0$`toxval_subtype` <- NA
  
  # Add toxval_units column - copy of "Units" column
  res0$`toxval_units` = res0$`Units`
  
  # Add toxval_numeric_qualifier column (empty)
  res0$`toxval_numeric_qualifier` <- NA
  
  # Add study_type column - hardcode as "acute"
  res0$`study_type` <- "acute"
  
  # Add study_duration_value column (empty)
  res0$`study_duration_value` <- NA
  
  # Add study_duration_units column (empty)
  res0$`study_duration_units` <- NA
  
  # Add species column
  # Species list to attempt string matches
  species_list <- list(dog=list("dog", "dogs"),
                       human=list("human", "humans", "occupational", "epidemiology", "epidemiological", "epidemiologic"),
                       mouse=list("mouse", "mice", "mouses"),
                       `monkey`=list("nonhuman primate", "monkey", "primate", "monkies", "monkeys",
                                     "rhesus monkeys (macaca mulatta)", "rhesus monkeys",
                                     "cynomolgus monkeys (macaca fascicularis)"),
                       rat=list("rat", "rats"),
                       rabbit = list("rabbit", "rabbits"),
                       `guinea pig` = list("guinea pig", "guinea pigs"),
                       frog = list("frog", "frogs"),
                       hen = list("hen")
  ) %>% unlist() %>%
    paste0(collapse="|")
  
  res0 <- res0 %>%
    dplyr::rowwise() %>%
    dplyr::mutate(species = `Source of PACsPAC-1, PAC-2, PAC-3` %>%
                    tolower() %>%
                    # Extract species from `Source of PACsPAC-1, PAC-2, PAC-3`
                    stringr::str_extract_all(., species_list) %>%
                    unlist() %>%
                    unique() %>%
                    paste0(collapse="; "))
  # Fill in missing with "-"
  res0$species[res0$species %in% c("", "NA")] <- "-"
  # Set occupational or epidemilog* studies to human species
  res0$species[grepl("occupation|epidemiolog", res0$species)] <- "human"
  # res0 %>% select(`PRINCIPAL STUDY`, species_original) %>% distinct() %>% View()
  
  # Add strain column (empty)
  res0$`strain` <- NA
  
  # Add sex column (empty)
  res0$`sex` <- NA
  
  # Add critical_effect column (empty)
  res0$`critical_effect` <- NA
  
  # Add population column (empty)
  res0$`population` <- NA
  
  # Add exposure_route column - hardcode as "inhalation"
  res0$`exposure_route` <- "inhalation"
  
  # Add exposure_method column (empty)
  res0$`exposure_method` <- NA
  
  # Add exposure_form column (empty)
  res0$`exposure_form` <- NA
  
  # Add media column (empty)
  res0$`media` <- NA
  
  # Add lifestage column (empty)
  res0$`lifestage` <- NA
  
  # Add generation column (empty)
  res0$`generation` <- NA
  
  # Add year column - from "Last Revised" first
  res0$`year` <- format(as.Date(res0$`PAC-TEEL Derivation/Review/Revision Dates,Last Revised`, format="%m/%d/%y"),"%y")
  
  # If no Last Revised, then use Last Reviewed
  res0 <- within(res0, {
    val <- is.na(year)
    year[val] <- format(as.Date(`PAC-TEEL Derivation/Review/Revision Dates,Last Reviewed`[val], format="%m/%d/%y"),"%y")
  })
  
  # If no Last Reviewed, then use Originally Derived
  res0 <- within(res0, {
    val <- is.na(year)
    year[val] <- format(as.Date(`PAC-TEEL Derivation/Review/Revision Dates,Originally Derived`[val], format="%m/%d/%y"),"%y")
  })
  res0$`val` <- NULL
  
  #####################################################################

  #
  # the final file should have column names that include "name" and "casrn"
  # additionally, the names in res need to match names in the source
  # database table. You do not need to add any of the generic columns
  # described in the SOP - they will get added in source_prep_and_load
  #

  # Standardize the names
  names(res0) <- names(res0) %>%
    stringr::str_squish() %>%
    # Replace whitespace and periods with underscore
    gsub("[[:space:]]|[.]", "_", .) %>%
    tolower()

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
                       chem.check.halt=chem.check.halt)
}




