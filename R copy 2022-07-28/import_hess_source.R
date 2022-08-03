#--------------------------------------------------------------------------------------
#' Load HESS Source into toxval_source
#' @param db The version of toxval_source into which the source is loaded.
#' @param infile1 The input file ./hess/hess_files/hess_6_16_21.csv, extracted by Risa Sayre(SCDCD)
#' @param infile2 The input file ./hess/hess_files/hess_record_urls_from_clowder.xlsx
#' @param chem.check.halt If TRUE, stop if there are problems with the chemical mapping
#--------------------------------------------------------------------------------------
import_hess_source <- function(db,
                               infile1="hess_6_16_21.xlsx",
                               infile2="hess_record_urls_from_clowder.xlsx",
                               chem.check.halt=T) {
  printCurrentFunction(db)

  infile1 = paste0(toxval.config()$datapath,"hess/hess_files/",infile1)
  infile2 = paste0(toxval.config()$datapath,"hess/hess_files/",infile2)
  #####################################################################
  cat("Build whole_hess_table from source file \n")
  #####################################################################
  res <- openxlsx::read.xlsx(infile1)
  res1 <- res
  # factor cols to character
  fac_cols <- sapply(res1, is.factor)                          # Identify all factor columns
  res1[fac_cols] <- lapply(res1[fac_cols], as.character)  # Convert all factors to characters

  # strip begining and ending quotation marks
  res1 <- as.data.frame(sapply(res1, function(x) gsub("\\'", "", x)))
  # strip leading and trailing spaces
  res1 <- as.data.frame(sapply(res1, function(x) gsub("(.*)(\\s+$)", "\\1", x)))
  res1 <- as.data.frame(sapply(res1, function(x) gsub("(^\\s+)(.*)", "\\2", x)))

  # factor cols to character
  fac_cols <- sapply(res1, is.factor)                          # Identify all factor columns
  res1[fac_cols] <- lapply(res1[fac_cols], as.character)  # Convert all factors to characters

  # apply appropriate data types
  res1 <- lapply(res1, function(x) type.convert(as.character(x), as.is = T))
  res1 <- data.frame(res1, stringsAsFactors = F)

  # all na logi cols to character
  res1 <- data.frame(lapply(res1, function(x) if(is.logical(x)) {
    return(as.character(x))
  } else {
    return(x)
  }), stringsAsFactors=FALSE)

  names(res1) <- tolower(names(res1))
  #print(str(res1))

  ##runInsertTable(res1,"whole_hess_table",db,do.halt=T,verbose=F)
  #####################################################################
  cat("Build original_hess table from res \n")
  #####################################################################
  # names of all critical effect categories
  names.list <- c("deaths","clinical_observation","functional_observation_battery","body_weight_changes","food_consumption",
                  "water_consumption","urinalysis","hematology","blood_chemistry","thyroid_hormone","absolute_organ_weight",
                  "relative_organ_weight","organ_weight","necropsy","macroscopic_finding","histopathology","reproductive_tissue_evaluation",
                  "estrous_cycle_characterization","bone_marrow_cellularity_counts","liver_biochemistry","reproductive_endpoint","other_findings")
  # critical effect N/A Values to hyphen
  res1[ , names(res1)[ names(res1)%in% names.list] ][ res1[ , names(res1)[ names(res1)%in% names.list] ] == "N/A" ] <- "-"
  # combine column names with values
  res1[,names(res1)[ names(res1)%in% names.list]] <- Map(paste, names(res1)[ names(res1)%in% names.list], res1[,names(res1)[ names(res1)%in% names.list]], sep = ';')
  # convert values with just column names and column names attached with NA values to NAs
  res1[, names(res1)[ names(res1)%in% names.list]] <- lapply(res1[, names(res1)[ names(res1)%in% names.list]],
                                                             function(x) replace(x, grepl(paste0("^",names.list,";$", collapse="|"), x), NA))
  res1[, names(res1)[ names(res1)%in% names.list]] <- lapply(res1[, names(res1)[ names(res1)%in% names.list]],
                                                             function(x) replace(x, grepl(paste0("^",names.list,";-$", collapse="|"), x), NA))
  res1[, names(res1)[ names(res1)%in% names.list]] <- lapply(res1[, names(res1)[ names(res1)%in% names.list]],
                                                             function(x) replace(x, grepl(paste0("^",names.list,";NA$", collapse="|"), x), NA))
  # combine all critical effect columns
  res1 <- res1 %>% unite("critical_effect", names(res1)[ names(res1)%in% names.list],sep = "|", na.rm = TRUE, remove = FALSE)
  # remove question marks before critical effect values
  res1$critical_effect <- str_replace_all(res1$critical_effect, pattern = "\\?", replacement = "")
  # keep only death or death none information in critical effect
  res1$critical_effect <- gsub("(^deaths)(\\;[0-9]+)([^|]+)(.*)", "\\1\\4",res1$critical_effect)
  # remove multiple patterns(starting with semicolon, colon, comma) of sex info from critical effect
  res1$critical_effect <- str_replace_all(res1$critical_effect, pattern = "(\\;|\\:)+\\s*[^[:alnum:]]*[0-9]+\\s*(male|female)*\\s*(female|male)*", replacement = "")
  res1$critical_effect <- str_replace_all(res1$critical_effect, pattern = "\\,+\\s*[^[:alnum:]]*[0-9]+\\s*(male|female)*", replacement = "")
  # provide appropriate spacing between words
  res1$critical_effect <- gsub("(\\s*[a-zA-Z]{1}[a-z]+)([A-Z]{1}[a-z]*\\s*)", "\\1 \\2",res1$critical_effect)
  # provide seperaton for values within each critical effect category with commas
  res1$critical_effect <- gsub("(decrease|increase)(\\s+)", "\\1,\\2",res1$critical_effect)
  # remove trailing commas at the end of each critical effect category
  res1$critical_effect <- gsub("(\\,\\s+\\|)", "|",res1$critical_effect)
  # remove multiple patterns(starting with period, multiple sex info within each value, single sex info within each value) of sex info from critical effect
  res1$critical_effect <- gsub("((\\.|\\s+)[0-9]+\\s+(male|female)+)", "",res1$critical_effect)
  res1$critical_effect <- gsub("((male|female)+\\s*(male|female)*\\s*\\*+)", "",res1$critical_effect)
  res1$critical_effect <- gsub("((male|female)+\\s*)", "",res1$critical_effect)
  # convert encoding to utf8
  res1$critical_effect <- enc2utf8(res1$critical_effect)
  # convert encoding of critical effect value columns to utf8
  cols <- names(res1)
  for(i in seq_along(cols)){
    if(!is.character(res1[, cols[[i]]])) next
    Encoding(res1[, cols[[i]]]) <- "UTF-8"
  }
  res1$sex <- "-"

  # columns containing female sex info
  female_cols <- res1[apply(res1, 1, function(i) any(grepl("\\bfemale\\b", i))),]
  res1[rownames(female_cols),"sex"] <- "female"
  # columns containing male sex info
  male_cols <- res1[apply(res1, 1, function(i) any(grepl("\\bmale\\b", i))),]
  res1[rownames(male_cols),"sex"] <- paste("male", res1[rownames(male_cols),"sex"] , sep = '/')
  # remove trailing /- from sex
  res1$sex <- gsub("(.*)(\\/\\-$)","\\1",res1$sex)
  # assign duration value and units original from study type
  res1$study_duration_value <- res1$study_type
  res1$study_duration_units <- res1$study_type
  # assign species and strain original from subject type
  res1$species <- res1$subject_type
  res1$strain <- res1$subject_type
  # assign exposure_route and exposure_method original from route
  res1$exposure_route <- res1$route
  res1$exposure_method <- res1$route
  res1$noel_val <- res1$reported_noel
  res1$noel_units <- res1$reported_noel
  res1$noel_qual <- res1$reported_noel
  res1$noel_type <- "NOEL"
  res1$noael_val <- res1$reported_noael
  res1$noael_units <- res1$reported_noael
  res1$noael_qual <- res1$reported_noael
  res1$noael_type <- "NOAEL"
  res1$loel_val <- res1$reported_loel
  res1$loel_units <- res1$reported_loel
  res1$loel_qual <- res1$reported_loel
  res1$loel_type <- "LOEL"
  res1$loael_val <- res1$reported_loael
  res1$loael_units <- res1$reported_loael
  res1$loael_qual <- res1$reported_loael
  res1$loael_type <- "LOAEL"

  h1 <- res1[,c(1:55,56:59)]
  h2 <- res1[,c(1:55,60:63)]
  h3 <- res1[,c(1:55,64:67)]
  h4 <- res1[,c(1:55,68:71)]
  names(h1)[56] <- "toxval_numeric"
  names(h1)[57] <- "toxval_units"
  names(h1)[58] <- "toxval_numeric_qualifier"
  names(h1)[59] <- "toxval_type"
  names(h2)[56] <- "toxval_numeric"
  names(h2)[57] <- "toxval_units"
  names(h2)[58] <- "toxval_numeric_qualifier"
  names(h2)[59] <- "toxval_type"
  names(h3)[56] <- "toxval_numeric"
  names(h3)[57] <- "toxval_units"
  names(h3)[58] <- "toxval_numeric_qualifier"
  names(h3)[59] <- "toxval_type"
  names(h4)[56] <- "toxval_numeric"
  names(h4)[57] <- "toxval_units"
  names(h4)[58] <- "toxval_numeric_qualifier"
  names(h4)[59] <- "toxval_type"
  res1 <- rbind(h1,h2,h3,h4)
  rownames(res1) <- c()
  names(res1)[names(res1) == "chemical_name"] <- "name"
  res1$toxval_subtype <- res1$study_type
  # res1$document_name <- "-"
  for (i in 1:nrow(res1)){
   #res1$document_name[i] <- gsub("(.*HESS\\\\)(.*)(\\..*)","\\2",res1$document_name[i])
   res1$document_name[i] <- str_replace(res1$document_name[i],".docx","")
  }
  for (i in 1:nrow(res1)){
    res1$document_name[i] <- paste(res1$document_name[i],".pdf", sep = "")
  }
  # assign document url from clowder to corresponding documents
  url_from_clowder <- openxlsx::read.xlsx(infile2)
  for(i in 1:nrow(url_from_clowder)) {
    valold <- url_from_clowder[i,2]
    valnew <- url_from_clowder[i,4]
    res1[is.element(res1$document_name,valold),"record_url"] <- valnew
  }
  #print(str(res1))
  # multiple casrn per chemical, erroneous casrn
  res1[grep("82832\\-73\\-3\\, 87625\\-09\\-0",res1$casrn),"casrn"]  <- "82832-73-3"
  res1[grep("82657\\-04\\-399267\\-18\\-2", res1$casrn),"casrn"] <- "99267-18-2"
  res1 = subset(res1,select=-c(x1,x5,chemical_name_0,casrn_0,input))

  #####################################################################
  cat("Prep and load the data\n")
  #####################################################################
  source_prep_and_load(db,source="HESS",table="source_hess",res=res1,F,T,T)
}
