#--------------------------------------------------------------------------------------
#' @description Load HAWC PFAS Source into toxval_source
#' @param db The version of toxval_source into which the source is loaded.
#' @param hawc_num The HAWC number being processed (e.g. 150, 430)
#' @param chem.check.halt If TRUE, stop if there are problems with the chemical mapping
#' @title FUNCTION_TITLE
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso 
#'  \code{\link[openxlsx]{read.xlsx}}
#'  \code{\link[dplyr]{select}}, \code{\link[dplyr]{distinct}}, \code{\link[dplyr]{mutate-joins}}, \code{\link[dplyr]{arrange}}, \code{\link[dplyr]{count}}, \code{\link[dplyr]{mutate}}, \code{\link[dplyr]{filter}}
#'  \code{\link[tidyr]{pivot_wider}}, \code{\link[tidyr]{unite}}, \code{\link[tidyr]{pivot_longer}}, \code{\link[tidyr]{separate}}
#'  \code{\link[stringr]{str_trim}}
#'  \code{\link[purrr]{map2}}
#'  \code{\link[digest]{digest}}
#' @rdname import_hawc_pfas_source
#' @export 
#' @importFrom openxlsx read.xlsx
#' @importFrom dplyr select distinct left_join arrange count mutate filter
#' @importFrom tidyr pivot_wider unite pivot_longer separate
#' @importFrom stringr str_squish
#' @importFrom purrr map2_chr
#' @importFrom digest digest
#--------------------------------------------------------------------------------------
import_hawc_pfas_source <- function(db,
                                        hawc_num = NULL,
                                        chem.check.halt=F) {
  printCurrentFunction(db)
  if(is.null(hawc_num)) {
    stop("hawc_num parameter must have a value")
    return()
  }
  infile1 = paste0(toxval.config()$datapath,
                   "hawc_pfas_",hawc_num,"/hawc_pfas_",hawc_num,"_files/",
                   "hawc_pfas_",hawc_num,"_raw3.xlsx")
  infile2 = paste0(toxval.config()$datapath,
                   "hawc_pfas_",hawc_num,"/hawc_pfas_",hawc_num,"_files/",
                   "hawc_pfas_",hawc_num,"_doses3.xlsx")
  infile3 = paste0(toxval.config()$datapath,
                   "hawc_pfas_",hawc_num,"/hawc_pfas_",hawc_num,"_files/",
                   "hawc_pfas_",hawc_num,"_groups3.xlsx")
  #####################################################################
  cat("Build original_hawc_pfas table from source file  \n")
  #####################################################################
  res_pfas3 <- openxlsx::read.xlsx(infile1)
  res_pfas3[] = lapply(res_pfas3, as.character)
  #res_pfas3 <- lapply(res_pfas3, function(x) type.convert(as.character(x), as.is = T))
  #res_pfas3 <- data.frame(res_pfas3, stringsAsFactors = F)
  dim(res_pfas3)

  #####################################################################
  cat("All closed empty square bracket entries to - in effects\n")
  #####################################################################
  res_pfas3$effects <- gsub("^\\[\\]$","-",res_pfas3$effects)

  #####################################################################
  cat("Strip square brackets from beginning and end\n")
  #####################################################################
  res_pfas3$animal_group.experiment.study.searches <- gsub("\\]|\\[", "", res_pfas3$animal_group.experiment.study.searches)
  res_pfas3$animal_group.experiment.study.identifiers <- gsub("\\]|\\[", "", res_pfas3$animal_group.experiment.study.identifiers)

  #####################################################################
  # cat("All na columns to character type\n")
  # #####################################################################
  # for (i in 1:ncol(res_pfas3[, colSums(is.na(res_pfas3)) == nrow(res_pfas3)])){
  #   res_pfas3[,names(res_pfas3[, colSums(is.na(res_pfas3)) == nrow(res_pfas3)][i])] <- as.character(res_pfas3[,names(res_pfas3[, colSums(is.na(res_pfas3)) == nrow(res_pfas3)][i])])
  # }
  #variations in NA to NA
  res_pfas3$bmd_notes <- gsub("[NA\\,\\\n]+",NA,res_pfas3$bmd_notes)
  res_pfas3$confidence_interval <- gsub("[NA\\,\\\n]+",NA,res_pfas3$confidence_interval)
  res_pfas3$animal_group.experiment.study.block_id <- gsub("[NA\\,\\\n]+",NA,res_pfas3$animal_group.experiment.study.block_id)
  # strip begining and ending quotation marks
  res_pfas3 <- as.data.frame(sapply(res_pfas3, function(x) gsub("\"", "", x)))
  ##################################################################################
  #res_dose3 <- openxlsx::read.xlsx(infile2,sheetIndex = 1, encoding="UTF-8")
  res_dose3 <- openxlsx::read.xlsx(infile2) %>%
    dplyr::select(dose_regime, dose_group_id, dose, dose_units.name) %>%
    dplyr::distinct()
  res_dose3[] = lapply(res_dose3, as.character)
  #res_dose3 <- lapply(res_dose3, function(x) type.convert(as.character(x), as.is = T))
  #res_dose3 <- data.frame(res_dose3, stringsAsFactors = F)
  print(dim(res_dose3))
  #res_groups3 <- openxlsx::read.xlsx(infile3,sheetIndex = 1, encoding="UTF-8")
  res_groups3 <- openxlsx::read.xlsx(infile3) %>%
    dplyr::select(#endpoint,
      dose_group_id) %>%
    dplyr::distinct()
  res_groups3[] = lapply(res_groups3, as.character)
  #res_groups3 <- lapply(res_groups3, function(x) type.convert(as.character(x), as.is = T))
  #res_groups3 <- data.frame(res_groups3, stringsAsFactors = F)
  print(dim(res_groups3))

  names.list <- c("assessment","name" ,"organ","NOEL","LOEL",
                  "FEL","url", "data_location",
                  "bmd","animal_group.experiment.study.id","animal_group.experiment.study.title" ,"animal_group.experiment.study.authors_short",
                  "animal_group.experiment.study.authors","animal_group.experiment.study.year","animal_group.experiment.study.journal",
                  "animal_group.experiment.study.full_text_url","animal_group.experiment.study.short_citation","animal_group.experiment.study.full_citation",
                  "animal_group.experiment.study.url","animal_group.experiment.name","animal_group.experiment.type",
                  "animal_group.experiment.chemical","animal_group.experiment.cas","animal_group.experiment.chemical_source",
                  "animal_group.experiment.vehicle","animal_group.experiment.guideline_compliance",
                  "animal_group.dosing_regime.id","animal_group.dosing_regime.route_of_exposure",
                  "animal_group.dosing_regime.duration_exposure","animal_group.dosing_regime.duration_exposure_text",
                  "animal_group.species","animal_group.strain" ,"animal_group.sex","animal_group.name","animal_group.generation",
                  "noel_names.noel","noel_names.loel")
  hawc_pfas <- res_pfas3[,names.list]

  #####################################################################
  cat("map noel,loel, fel values and units from dose dictionary\n")
  #####################################################################
  dose_dict <- res_dose3 %>%
    dplyr::left_join(res_groups3,
              by="dose_group_id") %>%
    dplyr::arrange(dose_regime, dose_units.name, dose)
  dose_dict_orig = dose_dict
  # Get counts of dose entries per dose_regime - units pairs
  dose_dict = dose_dict %>%
    dplyr::select(dose_regime, dose_units.name, dose) %>%
    dplyr::distinct() %>%
    #group_by() %>%
    dplyr::count(dose_regime, dose_units.name) %>%
    dplyr::mutate(dose_units.name_n = paste0("(", n, ") ", dose_units.name)) %>%
    dplyr::select(-n) %>%
    dplyr::left_join(dose_dict, by=c("dose_regime", "dose_units.name")) %>%
    # Combine doses by regime unit groups
    tidyr::pivot_wider(id_cols = c("dose_regime", "dose_units.name", "dose_units.name_n"),
                       names_from = "dose_group_id",
                       values_from = "dose") %>%
    tidyr::unite("dose", -dose_regime, -dose_units.name, -dose_units.name_n, sep=", ") %>%
    dplyr::mutate(dose = gsub(", NA", "", dose))

  # Helper function to manage the multiple dosing weirdness but maintain value-unit pairs
  split_dose_dose_units <- function(r, before_r = TRUE){
    # Split the input strings
    r = r %>%
      strsplit(split = ";") %>%
      unlist() %>%
      stringr::str_squish()
    # Value vs. units are separated by "||", select which is needed, before/after "||"
    if(before_r){
      lapply(r, function(x){ sub('\\|\\|.*', '', x) }) %>%
        paste(collapse = "; ") %>%
        return()
    } else {
      lapply(r, function(x){ sub('.*\\|\\|', '', x) }) %>%
        paste(collapse = "; ") %>%
        return()
    }
  }

  # Sort out dose value - unit pairs for regimes with multiples
  doses = dose_dict %>%
    dplyr::select(dose_regime, dose, dose_units.name) %>%
    tidyr::unite("dose_dose_units", -dose_regime, sep = "||") %>%
    tidyr::pivot_wider(id_cols = dose_regime,
                       names_from = "dose_dose_units",
                       values_from = "dose_dose_units") %>%
    tidyr::unite("dose_dose_units", -dose_regime, sep="; ") %>%
    dplyr::mutate(dose_dose_units = gsub("; NA|NA; ", "", dose_dose_units)) %>%
    dplyr::mutate(dose = purrr::map2_chr(dose_dose_units, TRUE, split_dose_dose_units),
           dose_units.name = purrr::map2_chr(dose_dose_units, FALSE, split_dose_dose_units)) %>%
    dplyr::select(-dose_dose_units)

  # Join/fill in dose and dose_units
  hawc_pfas$doses <- doses$dose[match(hawc_pfas$animal_group.dosing_regime.id,doses$dose_regime)]
  hawc_pfas$doses_units <- doses$dose_units.name[match(hawc_pfas$animal_group.dosing_regime.id,doses$dose_regime)]

  # Fix NOEL dict
  noel_dict = dose_dict_orig %>%
    dplyr::left_join(hawc_pfas %>%
                dplyr::select(animal_group.dosing_regime.id, NOEL),
              by = c("dose_regime"="animal_group.dosing_regime.id")) %>%
    dplyr::filter(NOEL == dose_group_id) %>%
    dplyr::select(-dose_group_id) %>%
    dplyr::distinct() %>%
    dplyr::arrange(dose_regime, dose_units.name, dose)
  # Sort out the NOEL values and units by combining and splitting
  noel_values = noel_dict %>%
    tidyr::unite("dose_dose_units", -dose_regime, -NOEL, sep = "||") %>%
    tidyr::pivot_wider(id_cols = c("dose_regime", "NOEL"),
                       names_from = "dose_dose_units",
                       values_from = "dose_dose_units") %>%
    tidyr::unite("dose_dose_units", -dose_regime, -NOEL, sep="; ") %>%
    dplyr::mutate(dose_dose_units = gsub("; NA|NA; ", "", dose_dose_units)) %>%
    dplyr::mutate(dose = purrr::map2_chr(dose_dose_units, TRUE, split_dose_dose_units),
           dose_units.name = purrr::map2_chr(dose_dose_units, FALSE, split_dose_dose_units)) %>%
    dplyr::select(-dose_dose_units)

  # Fix LOEL dict
  loel_dict = dose_dict_orig %>%
    dplyr::left_join(hawc_pfas %>%
                dplyr::select(animal_group.dosing_regime.id, LOEL),
              by = c("dose_regime"="animal_group.dosing_regime.id")) %>%
    dplyr::filter(LOEL == dose_group_id) %>%
    dplyr::select(-dose_group_id) %>%
    dplyr::distinct() %>%
    dplyr::arrange(dose_regime, dose_units.name, dose)
  # Sort out the LOEL values and units by combining and splitting
  loel_values = loel_dict %>%
    tidyr::unite("dose_dose_units", -dose_regime, -LOEL, sep = "||") %>%
    tidyr::pivot_wider(id_cols = c("dose_regime", "LOEL"),
                       names_from = "dose_dose_units",
                       values_from = "dose_dose_units") %>%
    tidyr::unite("dose_dose_units", -dose_regime, -LOEL, sep="; ") %>%
    dplyr::mutate(dose_dose_units = gsub("; NA|NA; ", "", dose_dose_units)) %>%
    dplyr::mutate(dose = purrr::map2_chr(dose_dose_units, TRUE, split_dose_dose_units),
           dose_units.name = purrr::map2_chr(dose_dose_units, FALSE, split_dose_dose_units)) %>%
    dplyr::select(-dose_dose_units)

  # Fix FEL dict
  fel_dict = dose_dict_orig %>%
    dplyr::left_join(hawc_pfas %>%
                dplyr::select(animal_group.dosing_regime.id, FEL),
              by = c("dose_regime"="animal_group.dosing_regime.id")) %>%
    dplyr::filter(FEL == dose_group_id) %>%
    dplyr::select(-dose_group_id) %>%
    dplyr::distinct() %>%
    dplyr::arrange(dose_regime, dose_units.name, dose)
  # Sort out the FEL values and units by combining and splitting
  fel_values = fel_dict %>%
    tidyr::unite("dose_dose_units", -dose_regime, -FEL, sep = "||") %>%
    tidyr::pivot_wider(id_cols = c("dose_regime", "FEL"),
                       names_from = "dose_dose_units",
                       values_from = "dose_dose_units") %>%
    tidyr::unite("dose_dose_units", -dose_regime, -FEL, sep="; ") %>%
    dplyr::mutate(dose_dose_units = gsub("; NA|NA; ", "", dose_dose_units)) %>%
    dplyr::mutate(dose = purrr::map2_chr(dose_dose_units, TRUE, split_dose_dose_units),
           dose_units.name = purrr::map2_chr(dose_dose_units, FALSE, split_dose_dose_units)) %>%
    dplyr::select(-dose_dose_units)

  # Match NOEL and LOEL values
  hawc_pfas$NOEL_values <- noel_values$dose[match(paste(hawc_pfas$animal_group.dosing_regime.id,hawc_pfas$NOEL),
                                                      paste(noel_values$dose_regime,noel_values$NOEL))]
  hawc_pfas$NOEL_units <-  noel_values$dose_units.name[match(paste(hawc_pfas$animal_group.dosing_regime.id,hawc_pfas$NOEL),
                                                                 paste(noel_values$dose_regime,noel_values$NOEL))]
  hawc_pfas$LOEL_values <- loel_values$dose[match(paste(hawc_pfas$animal_group.dosing_regime.id,hawc_pfas$LOEL),
                                                      paste(loel_values$dose_regime,loel_values$LOEL))]
  hawc_pfas$LOEL_units <-  loel_values$dose_units.name[match(paste(hawc_pfas$animal_group.dosing_regime.id,hawc_pfas$LOEL),
                                                                 paste(loel_values$dose_regime,loel_values$LOEL))]
  hawc_pfas$FEL_values <- fel_values$dose[match(paste(hawc_pfas$animal_group.dosing_regime.id,hawc_pfas$FEL),
                                                    paste(fel_values$dose_regime,fel_values$FEL))]
  hawc_pfas$FEL_units <-  fel_values$dose_units.name[match(paste(hawc_pfas$animal_group.dosing_regime.id,hawc_pfas$FEL),
                                                               paste(fel_values$dose_regime,fel_values$FEL))]

  # hawc_pfas %>% filter(animal_group.dosing_regime.id == 100500837) %>% View()

  #hawc_pfas$study_url <-  paste("https://hawcprd.epa.gov",hawc_pfas$animal_group.experiment.study.url, sep = "")
  hawc_pfas$endpoint_url <-  paste("https://hawcprd.epa.gov",hawc_pfas$url, sep = "")
  hawc_pfas$assessment_url <-  paste("https://hawcprd.epa.gov/assessment/",hawc_pfas$assessment,"/", sep = "")
  fac_cols <- sapply(hawc_pfas, is.factor)                          # Identify all factor columns
  hawc_pfas[fac_cols] <- lapply(hawc_pfas[fac_cols], as.character)  # Convert all factors to characters
  names.list <- c("assessment","critical_effect","target","NOEL_original","LOEL_original",
                  "FEL_original","endpoint_url_original", "data_location",
                  "bmd","study_id","title","authors_short","author","year","journal",
                  "full_text_url","short_ref","long_ref","study_url_original","experiment_name","experiment_type",
                  "name","casrn","chemical_source","media","guideline_compliance",
                  "dosing_regime_id","route_of_exposure","exposure_duration_value",
                  "exposure_duration_text","species","strain","sex","population","generation","noel_names","loel_names",
                  "doses", "doses_units", "NOEL_values","NOEL_units","LOEL_values",
                  "LOEL_units","FEL_values","FEL_units",
                  "record_url","source_url")

  names(hawc_pfas) <- names.list
  # Prep final
  hawc_pfas_final <- hawc_pfas[,names.list]
  hawc_pfas_final <-  hawc_pfas_final[ , !(names(hawc_pfas_final) %in% "assessment")]
  hawc_pfas_final$exposure_route <- hawc_pfas_final$route_of_exposure
  hawc_pfas_final$exposure_method <- hawc_pfas_final$route_of_exposure
  hawc_pfas_final$study_duration_value <- hawc_pfas_final$exposure_duration_value
  hawc_pfas_final$study_duration_units <- hawc_pfas_final$exposure_duration_text
  names(hawc_pfas_final)[match("exposure_duration_value",names(hawc_pfas_final))] <- "exposure_duration_value_original"
  names(hawc_pfas_final)[match("experiment_type",names(hawc_pfas_final))] <- "study_type_original"
  hawc_pfas_final$study_type <- hawc_pfas_final$study_type_original
  hawc_pfas_final$fel_names <- "FEL"
  hawc_pfas_final$source <- paste0("HAWC PFAS ", hawc_num)
  hawc_pfas_final$subsource <- ifelse(hawc_num == 150, "PFAS 150 (2020)",
                                      ifelse(hawc_num == 430, "PFAS 430 (2020)", paste0("PFAS ", hawc_num)))

  # # Base R attempt to pivot longer for NEL, LOEL, FEL columns
  # h1 <- hawc_pfas_final[,c(1:33,35,38:39,40:42,45:49,51:52)]
  # h2 <- hawc_pfas_final[,c(1:33,34,36:37,40:42,45:49,51:52)]
  # h3 <- hawc_pfas_final[,c(1:33,50,43:44,40:42,45:49,51:52)]
  #
  # names(h1)[34] <- "toxval_type"
  # names(h1)[35] <- "toxval_numeric"
  # names(h1)[36] <- "toxval_units"
  # names(h2)[34] <- "toxval_type"
  # names(h2)[35] <- "toxval_numeric"
  # names(h2)[36] <- "toxval_units"
  # names(h3)[34] <- "toxval_type"
  # names(h3)[35] <- "toxval_numeric"
  # names(h3)[36] <- "toxval_units"
  #
  # hawc_pfas_final <- rbind(h1,h2,h3)
  # rownames(hawc_pfas_final) <- c()
  # Pivot the NOEL, LOEL, and FEL fields to long form
  hawc_pfas_final = hawc_pfas_final %>%
    tidyr::unite(noel_names, NOEL_values, NOEL_units,
          col="NOEL", sep="|") %>%
    tidyr::unite(loel_names, LOEL_values, LOEL_units,
          col="LOEL", sep="|") %>%
    tidyr::unite(fel_names, FEL_values, FEL_units,
          col="FEL", sep="|") %>%
    tidyr::pivot_longer(cols = c("NOEL", "LOEL", "FEL"),
                        names_to = NULL,
                        values_to = "toxval_transform") %>%
    tidyr::separate(col = toxval_transform,
                    into = c("toxval_type", "toxval_numeric", "toxval_units"),
                    sep = "\\|") %>%
    # mutate(across(.cols = toxval_type, toxval_numeric, toxval_units,
    #               .fns = ~ gsub("NA", NA, .)))
    dplyr::mutate(toxval_type = gsub("NA", NA, toxval_type),
           toxval_numeric = gsub("NA", NA, toxval_numeric),
           toxval_units = gsub("NA", NA, toxval_units))

  print(dim(hawc_pfas_final))
  hawc_pfas_final <- hawc_pfas_final[which(hawc_pfas_final$toxval_numeric != '-999'),]
  print(dim(hawc_pfas_final))

  #####################################################################
  cat("Collapse duplicated that just differ by critical effect \n")
  #####################################################################
  res = hawc_pfas_final
  res2 = res[,!names(res)%in%c("critical_effect","endpoint_url_original","record_url","target")]

  res2$hashkey = NA
  res$hashkey = NA
  for(i in 1:nrow(res2)) {
    hashkey = digest::digest(paste0(res2[i,],collapse=""), serialize = FALSE)
    res2[i,"hashkey"] = hashkey
    res[i,"hashkey"] = hashkey
  }
  res2 = dplyr::distinct(res2)
  res2$critical_effect = NA
  for(i in 1:nrow(res2)) {
    hashkey = res2[[i,"hashkey"]]
    res3 = res[res$hashkey==hashkey,]
    x = res3$target
    y = res3$critical_effect
    ce = ""
    for(j in 1:length(x)) ce=paste0(ce,x[j],":",y[j],"|")
    ce = substr(ce,1,(nchar(ce)-1))
    res2[i,"critical_effect"] = ce
  }
  res2$endpoint_url_original = NA
  res2$record_url = NA
  res2$target = NA
  res2 = res2[,!names(res2)%in%c("hashkey")]
  res = res2
  #####################################################################
  cat("Prep and load the data\n")
  #####################################################################
  #source_prep_and_load(db,source=paste0("HAWC PFAS ", hawc_num),table=paste0("source_hawc_pfas_", hawc_num),res=res,F,T,T)
  return(res)
}
