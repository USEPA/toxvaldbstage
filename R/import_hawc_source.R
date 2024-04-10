#--------------------------------------------------------------------------------------
#' @title import_hawc_source
#' @description Load HAWC Project data into toxval_source
#' @param db The version of toxval_source into which the source is loaded.
#' @param chem.check.halt If TRUE and there are bad chemical names or casrn,
#' @param do.reset If TRUE, delete data from the database for this source before
#' @param do.insert If TRUE, insert data into the database, default FALSE
#' @return None; data is pushed to toxval_source
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[openxlsx]{getSheetNames}}, \code{\link[openxlsx]{read.xlsx}}
#'  \code{\link[dplyr]{select}}, \code{\link[dplyr]{distinct}}, \code{\link[dplyr]{mutate}}, \code{\link[dplyr]{arrange}}, \code{\link[dplyr]{count}}, \code{\link[dplyr]{mutate-joins}}, \code{\link[dplyr]{bind_rows}}, \code{\link[dplyr]{context}}, \code{\link[dplyr]{reexports}}, \code{\link[dplyr]{across}}, \code{\link[dplyr]{na_if}}
#'  \code{\link[tidyr]{pivot_wider}}, \code{\link[tidyr]{unite}}, \code{\link[tidyr]{reexports}}, \code{\link[tidyr]{drop_na}}
#'  \code{\link[generics]{setops}}
#'  \code{\link[digest]{digest}}
#'  \code{\link[stringr]{str_extract}}, \code{\link[stringr]{str_trim}}
#' @rdname import_hawc_source
#' @export
#' @importFrom openxlsx getSheetNames read.xlsx
#' @importFrom dplyr select distinct mutate arrange count left_join bind_rows n everything across na_if where
#' @importFrom tidyr pivot_wider unite contains drop_na
#' @importFrom generics is.element
#' @importFrom digest digest
#' @importFrom stringr str_extract str_squish
#--------------------------------------------------------------------------------------
import_hawc_source <- function(db, chem.check.halt=FALSE, do.reset=FALSE, do.insert=FALSE) {
  printCurrentFunction(db)
  source = "HAWC Project"
  source_table = "source_hawc"
  # Date provided by the source or the date the data was extracted
  src_version_date = as.Date("2021-12-06")

  #####################################################################
  cat("Build original_hawc table \n")
  #####################################################################
  infile1 = "hawc_original_12_06_21.xlsx"
  infile2 = "dose_dict.xlsx"

  sheets1 <- openxlsx::getSheetNames(paste0(toxval.config()$datapath,"hawc/hawc_files/",infile1))
  hawc_dfs <- lapply(sheets1, openxlsx::read.xlsx, xlsxFile = paste0(toxval.config()$datapath,"hawc/hawc_files/",infile1))
  # subsetting toxval specific variables from 100's of variables
  hawc_cols <- c("assessment","groups","name" ,"organ","NOEL","LOEL",
                 "FEL","url","animal_group.experiment.study.id","animal_group.experiment.study.title" ,"animal_group.experiment.study.authors_short",
                 "animal_group.experiment.study.authors","animal_group.experiment.study.year","animal_group.experiment.study.journal",
                 "animal_group.experiment.study.full_text_url","animal_group.experiment.study.short_citation","animal_group.experiment.study.full_citation",
                 "animal_group.experiment.study.url","animal_group.experiment.name","animal_group.experiment.type",
                 "animal_group.experiment.chemical","animal_group.experiment.cas","animal_group.experiment.chemical_source",
                 "animal_group.experiment.vehicle","animal_group.experiment.guideline_compliance",
                 "animal_group.dosing_regime.id","animal_group.dosing_regime.route_of_exposure",
                 "animal_group.dosing_regime.duration_exposure","animal_group.dosing_regime.duration_exposure_text",
                 "animal_group.species","animal_group.strain" ,"animal_group.sex","animal_group.name","animal_group.generation",
                 "noel_names.noel","noel_names.loel", "animal_group.experiment.url", "animal_group.experiment.id")

  new_hawc <- lapply(hawc_dfs, "[", hawc_cols)
  new_hawc_df <- do.call("rbind", new_hawc)

  #####################################################################
  cat("read in the dose dictionary \n")
  #####################################################################
  s <- openxlsx::read.xlsx(paste0(toxval.config()$datapath,"hawc/hawc_files/",infile2))
  #runInsertTable(s,"hawc_dose_dictionary",db,do.halt=T,verbose=F)
  #print(str(s))

  # Imported dose dictionary logic from import_hawc_pfas_source to address dose_group_id
  res_dose3 <- openxlsx::read.xlsx(paste0(toxval.config()$datapath,"hawc/hawc_files/",infile2)) %>%
    dplyr::select(dose_regime, dose_group_id, dose, name) %>%
    dplyr::distinct()
  res_dose3[] = lapply(res_dose3, as.character)
  dose_dict <- res_dose3 %>%
    dplyr::mutate(dose_group_id = as.numeric(dose_group_id)) %>%
    dplyr::arrange(dose_regime, dose_group_id, name, dose)
  dose_dict_orig = dose_dict
  # Get counts of dose entries per dose_regime - units pairs
  dose_dict = dose_dict %>%
    dplyr::select(dose_regime, name, dose) %>%
    dplyr::distinct() %>%
    #group_by() %>%
    dplyr::count(dose_regime, name) %>%
    dplyr::mutate(name_n = paste0("(", n, ") ", name)) %>%
    dplyr::select(-n) %>%
    dplyr::left_join(dose_dict, by=c("dose_regime", "name")) %>%
    # Combine doses by regime unit groups
    tidyr::pivot_wider(id_cols = c("dose_regime", "name", "name_n"),
                       names_from = "dose_group_id",
                       values_from = "dose") %>%
    tidyr::unite("dose", -dose_regime, -name, -name_n, sep=", ", na.rm = TRUE)


  #####################################################################
  cat("map hawc original with dose dictionary \n")
  #####################################################################
  new_hawc_df$NOEL_values <- s[match(paste(new_hawc_df$animal_group.dosing_regime.id,new_hawc_df$NOEL),paste(s$dose_regime,s$dose_group_id)),"dose"]
  new_hawc_df$NOEL_units <-  s[match(paste(new_hawc_df$animal_group.dosing_regime.id,new_hawc_df$NOEL),paste(s$dose_regime,s$dose_group_id)),"name"]
  new_hawc_df$LOEL_values <- s[match(paste(new_hawc_df$animal_group.dosing_regime.id,new_hawc_df$LOEL),paste(s$dose_regime,s$dose_group_id)),"dose"]
  new_hawc_df$LOEL_units <-  s[match(paste(new_hawc_df$animal_group.dosing_regime.id,new_hawc_df$LOEL),paste(s$dose_regime,s$dose_group_id)),"name"]
  new_hawc_df$FEL_values <- s[match(paste(new_hawc_df$animal_group.dosing_regime.id,new_hawc_df$FEL),paste(s$dose_regime,s$dose_group_id)),"dose"]
  new_hawc_df$FEL_units <-  s[match(paste(new_hawc_df$animal_group.dosing_regime.id,new_hawc_df$FEL),paste(s$dose_regime,s$dose_group_id)),"name"]

  new_hawc_df$doses <-  dose_dict[match(new_hawc_df$animal_group.dosing_regime.id,dose_dict$dose_regime),"dose"]
  # fix nested df in doses column issue
  corrected_column <- new_hawc_df$doses
  new_hawc_df$doses <- corrected_column$dose

  fac_cols <- sapply(new_hawc_df, is.factor)                          # Identify all factor columns
  new_hawc_df[fac_cols] <- lapply(new_hawc_df[fac_cols], as.character)  # Convert all factors to characters
  new_hawc_df$endpoint_url <-  paste("https://hawcproject.org",new_hawc_df$url, sep = "")
  new_hawc_df$study_url <-  paste("https://hawcproject.org",new_hawc_df$animal_group.experiment.study.url, sep = "")
  new_hawc_df$assessment_url <-  paste("https://hawcproject.org/assessment/",new_hawc_df$assessment,"/", sep = "")

  # remove dose dict expressed as groups variable
  new_hawc_df <- new_hawc_df[,names(new_hawc_df)[names(new_hawc_df) != "groups"]]
  names.list <- c("assessment","critical_effect","target","NOEL_original","LOEL_original",
                  "FEL_original","endpoint_url_original","study_id","title","authors_short","author","year","journal",
                  "full_text_url","short_ref","long_ref","study_url_original","experiment_name","experiment_type",
                  "name","casrn","chemical_source","media","guideline_compliance",
                  "dosing_regime_id","route_of_exposure","exposure_duration_value",
                  "exposure_duration_text","species","strain","sex","population","generation","noel_names","loel_names",
                  "experiment_url", "experiment_id",
                  "NOEL_values","NOEL_units","LOEL_values",
                  "LOEL_units","FEL_values","FEL_units","doses","endpoint_url","source_url","assessment_url")

  names(new_hawc_df) <- names.list
  new_hawc_df$fel_names <- "FEL"

  # entire full_text_url field is empty, hence assigning as hyphen to maintain character type
  new_hawc_df[which(is.na(new_hawc_df$full_text_url)),"full_text_url"] <- "-"

  h1 <- new_hawc_df %>%
    dplyr::select(-tidyr::contains("loel"), -tidyr::contains("fel"),
                  toxval_numeric_dose_index=NOEL_original,
                  toxval_type = noel_names,
                  toxval_numeric = NOEL_values,
                  toxval_units = NOEL_units) # [,c(1:33,34,36,37,42:45)]
  h2 <- new_hawc_df %>%
    dplyr::select(-tidyr::contains("noel"), -tidyr::contains("fel"),
                  toxval_numeric_dose_index=LOEL_original,
                  toxval_type = loel_names,
                  toxval_numeric = LOEL_values,
                  toxval_units = LOEL_units) # [,c(1:33,35,38,39,42:45)]

  h3 <- new_hawc_df %>%
    dplyr::select(-tidyr::contains("noel"), -tidyr::contains("loel"),
                  toxval_numeric_dose_index=FEL_original,
                  toxval_type = fel_names,
                  toxval_numeric = FEL_values,
                  toxval_units = FEL_units) # [,c(1:33,46,40,41,42:45)]

  new_hawc_df_final <- dplyr::bind_rows(h1,h2,h3)
  rownames(new_hawc_df_final) <- c()

  new_hawc_df_final = new_hawc_df_final %>%
    dplyr::mutate(study_type = experiment_type,
                  exposure_route = route_of_exposure,
                  exposure_method = route_of_exposure,
                  study_duration_value = exposure_duration_text,
                  study_duration_units = exposure_duration_text) %>%
    tidyr::drop_na(toxval_numeric)

  res = new_hawc_df_final
  res = res[!generics::is.element(res$casrn,"NOCAS"),]
  names(res)[generics::is.element(names(res),"LOEL_original")] = "loel_original"
  names(res)[generics::is.element(names(res),"NOEL_original")] = "noel_original"
  names(res)[generics::is.element(names(res),"FEL_original")] = "fel_original"

  #####################################################################
  cat("Add the code from the original version from Aswani\n")
  #####################################################################
  unique(res$study_type)
  res$study_type <- tolower(res$study_type)
  para_vals <- grep("\\(",res$study_type)
  res[para_vals, "study_type"] <- gsub("(.*)(\\s+\\(.*)","\\1",res[para_vals, "study_type"])
  ##### fix exposure_route
  unique(res$exposure_route)
  res$exposure_route <- tolower(res$exposure_route)
  oral_vals <- grep("oral", res$exposure_route)
  res[oral_vals, "exposure_route"] <- gsub("(oral)(\\s+.*)","\\1",res[oral_vals, "exposure_route"])
  injection_vals <- grep("injection", res$exposure_route)
  res[injection_vals, "exposure_route"] <- gsub("(.*)(\\s+injection)","\\1",res[injection_vals, "exposure_route"])
  ####### fix exposure_method
  unique(res$exposure_method)
  res$exposure_method <- tolower(res$exposure_method)
  oral_vals <- grep("oral", res$exposure_method)
  res[oral_vals, "exposure_method"] <- gsub("(oral\\s+)(.*)","\\2",res[oral_vals, "exposure_method"])
  injection_vals <- grep("injection", res$exposure_method)
  res[injection_vals, "exposure_method"] <- gsub("(.*\\s+)(injection)","\\2",res[injection_vals, "exposure_method"])
  res$exposure_method <- tolower(res$exposure_method)

  ######### fix study duration value and units
  #set all developmental records to NA as not to misrepresent the data
  res <- res %>%
    dplyr::mutate(
      study_duration_value = ifelse(study_type == "developmental", NA, study_duration_value),
      study_duration_units = ifelse(study_type == "developmental", NA, study_duration_units),
      study_duration_units = ifelse(study_duration_units == "GD 0 until GD 0", NA, study_duration_value)
    )
  #hour vals
  hour_vals <- grep("hour", res$study_duration_value, ignore.case = T)
  res[hour_vals,"study_duration_units"] <- "hour"
  res[hour_vals,"study_duration_value"] <- gsub("^([0-9]+)(\\s+)(hours)","\\1",res[hour_vals,"study_duration_value"])
  # day vals
  day_vals <- grep("day", res$study_duration_value, ignore.case = T)
  res[day_vals,"study_duration_units"] <- "day"
  res[day_vals,"study_duration_value"] <- gsub("^([0-9]+)(\\s+)(days)(.*)","\\1",res[day_vals,"study_duration_value"])
  day_vals <- grep("day", res$study_duration_value, ignore.case = T)
  res[day_vals,"study_duration_value"] <- gsub("^([0-9]+\\-)([0-9]+)(\\s+)(days)(.*)","\\2",res[day_vals,"study_duration_value"])
  #week vals
  week_vals <- grep("week", res$study_duration_value, ignore.case = T)
  res[week_vals,"study_duration_units"] <- "week"
  res[week_vals,"study_duration_value"] <- gsub("^([0-9]+)(\\s+)(weeks)(.*)","\\1",res[week_vals,"study_duration_value"])
  week_vals <- grep("week", res$study_duration_value, ignore.case = T)
  res[week_vals,"study_duration_value"] <- gsub("^(.*[^0-9]+)([0-9]+)(\\s+)(weeks)(.*)","\\2",res[week_vals,"study_duration_value"])
  #month vals (without PND)
  month_vals <- grep("months$", res$study_duration_value, ignore.case = T)
  res[month_vals,"study_duration_units"] <- "month"
  res[month_vals,"study_duration_value"] <- gsub("^([0-9]+)(\\s+)(months)","\\1",res[month_vals,"study_duration_value"])
  #one time vals
  one_time_vals <- grep("one time", res$study_duration_value, ignore.case = T)
  res[one_time_vals,"study_duration_units"] <- "one time"
  res[one_time_vals,"study_duration_value"] <- "1"
  # GD range vals
  GD_vals <- grep("GD\\s+.*\\-[^a-zA-Z]+$", res$study_duration_value, ignore.case = T)
  res[GD_vals,"study_duration_units"] <- "GD"
  res[GD_vals,"study_duration_value"] <- gsub("^(GD)(\\s+.*\\-\\s*)(.*)","\\3",res[GD_vals,"study_duration_value"])

  # GD until vals
  GD_until_vals <- grep("GD.*until.*[^0]$", res$study_duration_value, ignore.case = T)
  res[GD_until_vals,"study_duration_units"] <- "GD"
  res[GD_until_vals,"study_duration_value"] <- gsub("^(GD.*GD\\s+)(.*)","\\2",res[GD_until_vals,"study_duration_value"])

  #PND range vals
  PND_vals <- grep(".*PND\\s*.*[^0a-zA-Z]$", res$study_duration_value, ignore.case = T)
  res[PND_vals,"study_duration_units"] <- "PND"
  res[PND_vals,"study_duration_value"] <- gsub("^(.*PND\\s*)(\\d+)","\\2",res[PND_vals,"study_duration_value"])
  res[which(res$study_duration_value == "2-15"),"study_duration_value"] <- gsub("(\\d+\\-)(\\d+)","\\2",res[which(res$study_duration_value == "2-15"),"study_duration_value"])

  # 1 OR 2 years vals
  or_vals <- grep("or", res$study_duration_value, ignore.case = T)
  res[or_vals,"study_duration_units"] <- gsub("(.*or\\s+)(\\d+)(\\s+)(\\w+)","\\4",res[or_vals,"study_duration_value"])
  res[or_vals,"study_duration_value"] <- gsub("(.*or\\s+)(\\d+)(\\s+)(\\w+)","\\2",res[or_vals,"study_duration_value"])

  # #####################################################################
  # cat("Collapse duplicated that just differ by critical effect \n")
  # #####################################################################
  # # critical_effect_map = res %>%
  # #   dplyr::select(critical_effect,source_id,endpoint_url_original,endpoint_url,target) %>%
  # #   tidyr::unite(col="critical_effect", target, critical_effect, sep = ": ", na.rm = TRUE) %>%
  # #   dplyr::group_by(endpoint_url_original, endpoint_url) %>%
  # #   dplyr::summarize(critical_effect = paste0(sort(critical_effect), collapse = "|")) %>%
  # #   dplyr::ungroup()
  #
  # res2 = res[,!names(res)%in%c("critical_effect","source_id","endpoint_url_original","endpoint_url","target")]
  # cat(nrow(res),"\n")
  # res2$hashkey = NA
  # for(i in 1:nrow(res2)) {
  #   hashkey = digest::digest(paste0(res2[i,],collapse=""), serialize = FALSE)
  #   res2[i,"hashkey"] = hashkey
  #   res[i,"hashkey"] = hashkey
  # }
  # res2 = unique(res2)
  # res2$critical_effect = NA
  # for(i in 1:nrow(res2)) {
  #   hashkey = res2[i,"hashkey"]
  #   res3 = res[res$hashkey==hashkey,]
  #   x = res3$target
  #   y = res3$critical_effect
  #   ce = ""
  #   for(j in 1:length(x)) {
  #     # Skip blank entries
  #     if(is.na(x[j]) & is.na(y[j])){
  #       next
  #     } else if (is.na(x[j])){
  #       ce=paste0(ce,y[j],"|")
  #     } else if (is.na(y[j])){
  #       ce=paste0(ce,x[j],"|")
  #     } else {
  #       if(grepl(paste0(x[j], ":"), y[j])) {
  #         ce=paste0(ce,y[j],"|")
  #       } else {
  #         ce=paste0(ce,x[j],":",y[j],"|")
  #       }
  #     }
  #   }
  #   ce = substr(ce,1,(nchar(ce)-1))
  #   res2[i,"critical_effect"] = ce
  # }
  # res2$source_id = NA
  # res2$endpoint_url_original = NA
  # res2$endpoint_url = NA
  # res2$target = NA
  # res2 = res2[,!names(res2)%in%c("hashkey")]
  # res = res2
  # cat(nrow(res),"\n")

  res = res %>%
    tidyr::unite("critical_effect", critical_effect, target,
                 sep = ": ",
                 na.rm=TRUE) %>%
    # Add additional toxval_type details from units
    dplyr::mutate(toxval_type_2 = toxval_units %>%
                    stringr::str_extract("TAD|HED") %>%
                    paste0(")") %>%
                    gsub("NA\\)", NA, .),
                  toxval_units = toxval_units %>%
                    gsub("TAD|HED", "", .) %>%
                    stringr::str_squish(),
                  # Lowercase species
                  species = species %>%
                    tolower(),
                  sex = sex %>%
                    gsub("Combined", "male/female", .) %>%
                    tolower()) %>%
    tidyr::unite("toxval_type", toxval_type, toxval_type_2,
                 sep = " (",
                 na.rm=TRUE) %>%

    # Remove "unknown" values
    dplyr::mutate(dplyr::across(c(species, strain, sex, study_type, exposure_route, exposure_method),
                                # Not using tolower() to simplify cases due to strain field
                                ~dplyr::na_if(., "not reported") %>%
                                  dplyr::na_if("Not reported") %>%
                                  dplyr::na_if("not-reported") %>%
                                  dplyr::na_if("Not Reported") %>%
                                  dplyr::na_if("unspecified") %>%
                                  dplyr::na_if("other") %>%
                                  dplyr::na_if("Other") %>%
                                  dplyr::na_if("unknown")
                  )) %>%
    # Filter out entries with missing experimental information
    tidyr::drop_na(species, study_type, exposure_route) %>%

    dplyr::mutate(
      # Uncomment if splitting toxval_subtype from toxval_type
      # # Extract toxval_subtype from toxal_type
      # toxval_subtype = stringr::str_extract(toxval_type, "HED|TAD"),
      # toxval_type = gsub(" \\(.+", "", toxval_type),

      # Clean toxval_units
      toxval_units = toxval_units %>%
        stringr::str_squish(),

      # Fix exposure_route being used as exposure_method
      exposure_method = dplyr::case_when(
        exposure_method == exposure_route ~ as.character(NA),
        TRUE ~ exposure_method
      ),

      # Get study_duration values with both GD/PND, handle other edge cases
      study_duration = exposure_duration_text %>%
        gsub("\\s?to\\s?|\\s?until\\s?| \\- ", "-", .) %>%
        gsub("D ([0-9\\.])", "D\\1", .) %>%
        gsub("\\s*\\-\\s*", "-", .),
      study_duration_value = dplyr::case_when(
        grepl("GD[0-9]+\\-[0-9]+\\s*weeks", study_duration) ~ stringr::str_extract(study_duration, "GD[0-9]+\\-[0-9]+\\s*weeks") %>% c(),
        grepl("mating\\-PND", study_duration) ~ as.character(NA),
        grepl("GD[0-9\\.]+\\-PND[0-9\\.]+", study_duration) ~ gsub(".*(GD[0-9\\.]+)\\-(PND[0-9\\.]+).*", "\\1-\\2", study_duration),
        grepl("GD[0-9\\.]+\\-GD[0-9\\.]+", study_duration) ~ gsub("GD([0-9\\.]+)\\-GD([0-9\\.]+)", "\\1-\\2", study_duration),
        grepl("PND[0-9\\.]+\\-PND[0-9\\.]+", study_duration) ~ gsub("PND([0-9\\.]+)\\-PND([0-9\\.]+)", "\\1-\\2", study_duration),
        grepl("(?:GD|PND)[0-9\\.]+\\-?[0-9\\.]*", study_duration) ~ stringr::str_extract(study_duration,
                                                                                   "(?:GD|PND)([0-9\\.]+\\-?[0-9\\.]*)",
                                                                                   group=1),
        TRUE ~ study_duration_value
      ),
      study_duration_units = dplyr::case_when(
        grepl("GD", study_duration_value) & grepl("week", study_duration_value) ~ "GD,weeks",
        grepl("mating\\-PND", study_duration) ~ as.character(NA),
        grepl("GD", study_duration) & grepl("PND", study_duration) ~ "GD,PND",
        grepl("GD[0-9\\.]+\\-GD[0-9\\.]+", study_duration) ~ "GD",
        grepl("PND[0-9\\.]+\\-PND[0-9\\.]+", study_duration) ~ "PND",
        grepl("(?:GD|PND)[0-9\\.]+\\-?[0-9\\.]*", study_duration) ~ stringr::str_extract(study_duration,
                                                                                   "(GD|PND)(?:[0-9\\.]+\\-?[0-9\\.]*)",
                                                                                   group=1),
        TRUE ~ study_duration_units
      ),
      study_duration_value = dplyr::case_when(
        study_duration_units != "GD,PND" ~ gsub("\\b0-0\\b", "0", study_duration_value),
        TRUE ~ study_duration_value
      ),

      # Remove excess whitespace and fix unicode
      dplyr::across(dplyr::where(is.character), fix.replace.unicode),
      dplyr::across(dplyr::where(is.character), stringr::str_squish)
    ) %>%
    # Drop unused study_duration field
    dplyr::select(-study_duration)

  # Standardize the names
  names(res) <- names(res) %>%
    stringr::str_squish() %>%
    # Replace whitespace and periods with underscore
    gsub("[[:space:]]|[.]", "_", .) %>%
    tolower()

  # Fill blank hashing cols
  res[, toxval.config()$hashing_cols[!toxval.config()$hashing_cols %in% names(res)]] <- "-"

  # Perform deduping
  # res = toxval.source.import.dedup(res)
  # Use deduping function to improve collapse behavior for critical_effect
  # dedup_fields = c("critical_effect", names(res %>% dplyr::select(-dplyr::any_of(toxval.config()$hashing_cols))))
  hashing_cols = c(toxval.config()$hashing_cols[!(toxval.config()$hashing_cols %in% c("critical_effect"))],
                   "dosing_regime_id", "experiment_url")
  res = toxval.source.import.dedup(res, hashing_cols=hashing_cols) %>%
    # Replace "|::|" in critical_effect with "|" delimiter
    dplyr::mutate(
      critical_effect = critical_effect %>%
        gsub(" \\|::\\| ", "|", .)
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




