#--------------------------------------------------------------------------------------
#' code to create ECHA echemportal api dict
#' @param toxval.db The version of toxval into which the dictionary is loaded.
#' @param filepath The path for all the input xlsx files ./echa_echemportal_api/echa_echemportal_api_files
#--------------------------------------------------------------------------------------
build_echa_echemportal_api_dict <- function(toxval.db,filepath) {
  printCurrentFunction(toxval.db)

  #####################################################################
  cat("Read in echa_echemportal_api_original table\n")
  #####################################################################

  files.list <- list.files(path = filepath, pattern = "*.xlsx")
  #print(files.list)
  any_temp_files <- grep("^\\~\\$.*", files.list, value = T)
  files.list <- files.list[! files.list %in% any_temp_files]
  files.list <- paste0(filepath, "/", files.list)
  res <- lapply(files.list,read.xlsx)
  names(res) <- gsub("(.*\\/eChemPortalAPI_)(.*)(_.*)", "\\2", files.list)
  for (i in 1:length(res)) {
    res[[i]] <- lapply(res[[i]], function(x) type.convert(as.character(x), as.is = T))
    res[[i]] <- data.frame(res[[i]], stringsAsFactors = F)
  }
  #res = res[names(res) != "AcuteToxicityOther"]
  for (i in 1:length(res)) {
    res[[i]][sapply(res[[i]], function(x) all(is.na(x) == T))] <- ""
  }


  res1 <- rbindlist(res, fill = TRUE, idcol = 'source_table')
  res1 <- data.frame(res1, stringsAsFactors = F)

  res1$Name <- enc2utf8(res1$Name)
  names(res1) <- tolower(names(res1))
  res1$year <- res1$years

  res1$experimental.value <- enc2utf8(res1$experimental.value)
  res1$toxval_numeric_qualifier <- res1$experimental.value
  res1$toxval_numeric <- res1$experimental.value
  res1$toxval_units <- res1$experimental.value
  res1$generation <- res1$experimental.value
  res1$toxval_type <- res1$value.type
  # study_type from endpoint.type and property.name
  res1$study_type <- res1$endpoint.type
  # exposure_route from property.name and route.of.administration.exposure
  res1$exposure_route <- res1$property.name
  res1$exposure_method <- res1$route.of.administration.exposure
  res1$exposure_form <- res1$inhalation.exposure.type

  # combine data from histopathological.findings..neoplastic to critical effects
  res1$critical_effect <- res1$basis
  res1$study_duration_value <- res1$duration.sampling.time
  res1$study_duration_units <- res1$duration.sampling.time

  #names(res1)[names(res1)%in% "histopathological.findings..neoplastic"] <- "histopathological_findings_neoplastic"
  names(res1) <- gsub("\\.+","_", names(res1))
  res1$sex <- "-"

  # columns containing female sex info

  female_cols <- res1[apply(res1, 1, function(i) any(grepl("\\bfemale\\b", i, ignore.case = T))),]
  res1[rownames(female_cols),"sex"] <- "female"

  # columns containing male sex info

  male_cols <- res1[apply(res1, 1, function(i) any(grepl("\\bmale\\b", i, ignore.case = T))),]
  res1[rownames(male_cols),"sex"] <- paste("male", res1[rownames(male_cols),"sex"] , sep = '/')

  # remove trailing /- from sex
  res1$sex <- gsub("(.*)(\\/\\-$)","\\1",res1$sex)

  #print(str(res1))
  #####################################################################
  cat("Process the data prior to building echa_echemportal_api_dict\n")
  #####################################################################


  res1$experimental_value <- enc2utf8(res1$experimental_value)
  res1$toxval_numeric_qualifier <- enc2utf8(res1$toxval_numeric_qualifier)


  names(res1)[names(res1) == "participant"] <- "subsource"
  names(res1)[names(res1) == "number"] <- "casrn"
  names(res1)[names(res1) == "reliability"] <- "quality"
  names(res1)[names(res1) == "guidelines_qualifiers"] <- "guideline"
  names(res1)[names(res1) == "glp_compliance"] <- "glp"

  #res1 <- res1[ , -which(names(res1) %in% c("Name.Type","Number.Type","Member.of.Category"))]

  res1$source_table <- gsub('(.)([[:upper:]])','\\1 \\2', res1$source_table)
  #res1$source_table <- tolower(res1$source_table)
  res1$source_table <- gsub("(.*)(\\s+Teratogenicity)", "\\1 /\\2", res1$source_table)
  # extract minimum of multiple years
  res1$year <- sapply(
    str_extract_all(res1$years, "[0-9]+"),
    function(x) min(as.integer(x))
  )
  #res1 <- res1[ , -which(names(res1) %in% c("years"))]
  res1[grep("^[^0-9]+", res1$experimental_value),"toxval_numeric_qualifier"] <-  gsub("(^[^0-9]+)(\\s+\\d+.*)","\\1", res1[grep("^[^0-9]+", res1    $experimental_value),"experimental_value"])
  res1[grep("\\:", res1$toxval_numeric_qualifier),"toxval_numeric_qualifier"] <- gsub("(.*\\:)(.*)","\\2",res1[grep("\\:", res1$toxval_numeric_qualifier),"toxval_numeric_qualifier"])
  res1$toxval_numeric_qualifier <- gsub("^\\s+", "", res1$toxval_numeric_qualifier)
  res1[grep("\\d+\\s+.*\\s+\\d+", res1$experimental_value), "toxval_numeric"] <- sapply( str_extract_all(res1[grep("\\d+\\s+.*\\s+\\d+", res1$experimental_value), "experimental_value"], "\\d+\\.*\\d*"),
                                                                                         function(x) min(as.numeric(x))
  )

  #####################################################################
  cat("build echa_echemportal_api_dict table\n")
  #####################################################################

  #starting with non alphanumeric qualifier and has multiple values
  a <- res1[grep("^[^[:alnum:]]+\\s*\\d+\\s+.*\\s+\\d+", res1$experimental_value),"experimental_value" ]

  b <- sapply( str_extract_all(res1[grep("^[^[:alnum:]]+\\s*\\d+\\s+.*\\s+\\d+", res1$experimental_value), "experimental_value"], "\\d+\\.*\\d*"),
               function(x) min(as.numeric(x)))
  c <- data.frame(a,b, stringsAsFactors = F)
  for (i in 1:dim(c)[1]){
    c$d[i] <- gsub(paste("(.*)(",as.character(c$b[i]),")(.*)", sep = ""), "\\1", c$a[i])
    c$d[i] <- gsub("^\\s+|\\s+$","", c$d[i])
  }
  c[grep("\\s+", c$d), "d"] <- gsub("(^[^[:alnum:]]+\\s*[^[:alnum:]]*)(\\s+.*)","\\1",c[grep("\\s+", c$d), "d"])
  c <- unique(c)
  #c$e <- gsub("(.*\\d+\\s+)(\\w+.*)","\\2",c$a)
  c$e <- gsub("(.*?)([a-zA-Z]+.*)","\\2",c$a)
  c[grep("%", c$a), "e"] <- "%"
  c[grep("^[^[:alnum:]]+.*\\d+$", c$e), "e"] <- "-"
  c[grep("^ca\\.", c$e), "e"] <- "mg/kg bw"
  #generation info from dev tox file as g in dictionary
  c$g <- "-"
  c[grep("\\bmin\\b", c$e), "e"] <- "-"
  c[grep("\\bmin\\b", c$e), "b"] <- ""
  c[grep("\\bmin\\b", c$e), "d"] <- "-"
  c[grep("\\bper animal\\b", c$e), "e"] <- gsub("(.*)(\\s+per animal)","\\1",c[grep("\\bper animal\\b", c$e), "e"])
  c[grep("\\begg\\b", c$e), "e"] <- gsub("(.*)(\\/.*)","\\1",c[grep("\\begg\\b", c$e), "e"])
  c[grep("^ppm", c$e), "e"] <- gsub("(ppm)(.*)","\\1",c[grep("^ppm", c$e), "e"])
  c[grep("^ppm", c$e), "e"][grep("\\(nominal",c[grep("^ppm", c$e), "a"])] <- paste(c[grep("^ppm", c$e), "e"][grep("\\(nominal",c[grep("^ppm", c$e), "a"])], "(nominal)", sep = " ")
  c[grep("^ppm", c$e), "e"][grep("\\(v\\/v\\)",c[grep("^ppm", c$e), "a"])] <- paste(c[grep("^ppm", c$e), "e"][grep("\\(v\\/v\\)",c[grep("^ppm", c$e), "a"])], "(v/v)", sep = " ")
  c[grep("percent", c$e), "e"] <- "%"
  c[grep("^[^[:alnum:]]\\s+\\d+\\s+\\w*\\/*\\w+\\s+\\(", c$a),"b"] <- gsub("(^[^[:alnum:]]\\s+)(\\d+)(.*)","\\2",c[grep("^[^[:alnum:]]\\s+\\d+\\s+\\w*\\/*\\w+\\s+\\(", c$a),"a"])
  c[grep("\\(.*\\d+.*\\)",c$e),"e"] <- gsub("(\\w+\\/+\\w+)(\\s+\\(+)(.*)","\\1",c[grep("\\(.*\\d+.*\\)",c$e),"e"])
  c[grep(".*\\d+\\/",c$e),"e"] <- gsub("(.*?)( .*)","\\1",c[grep(".*\\d+\\/",c$e),"e"])
  c[grep("mg\\/m\\d+",c$e),"e"] <- gsub("(.*?)( .*)","mg/m3\\2",c[grep("mg\\/m\\d+",c$e),"e"])
  c[which(c$b == 0),"d"] <- gsub("(.*0\\s+)([^[:alnum:]]+)(\\s+)(\\d+\\.*\\d*)(\\s+.*)","\\2",c[which(c$b == 0),"a"])
  c[which(c$b == 0),"b"] <- gsub("(.*0\\s+[^[:alnum:]]+\\s+)(\\d+\\.*\\d*)(\\s+.*)","\\2",c[which(c$b == 0),"a"])
  #toxval numeric values in range
  a1 <- res1[grep("^\\d+[^a-zA-Z0-9. ]\\d+", res1$experimental_value), "experimental_value"]
  a1 <- gsub("(^\\d+)([^a-zA-Z0-9. ])(\\d+.*)","\\1-\\3", a1)
  b1 <- sapply( str_extract_all(a1, "\\d+\\.*\\d*"),
                function(x) min(as.numeric(x)))
  c1 <- data.frame(a1,b1, stringsAsFactors = F)
  c1$d1 <- "-"
  c1 <- unique(c1)
  c1$e <- gsub("(.*?)([a-zA-Z]+.*)","\\2",c1$a)
  c1[grep("%", c1$a), "e"] <- "%"
  c1[grep("mg/rat", c1$e), "e"] <- "mg"
  #generation info from dev tox file as g in dictionary
  c1$g <- "-"
  names(c1) <- names(c)
  c1[which(c1$b == 0),"b"] <- gsub("(0-)(\\d+\\.*\\d*)(\\s+.*)","\\2",c1[which(c1$b == 0),"a"])
  # without qualifier values at the beginning
  c2 <- data.frame(grep("^\\d+\\.*\\d*\\s+", res1$experimental_value, value = T), stringsAsFactors = F)
  names(c2) <- "a"
  c2$b <- gsub("(^\\d+\\.*\\d*)(\\s+.*)","\\1",c2$a)
  c2$d <- "-"
  c2 <- unique(c2)
  c2$e <- gsub("(.*?)([a-zA-Z]+.*)","\\2",c2$a)
  c2[grep("^\\d+\\.*\\d*\\s+%", c2$a), "e"] <- "%"
  c2[grep("minutes", c2$e), "e"] <- "-"
  c2[grep("minutes", c2$e), "b"] <- ""
  c2[grep("^h$", c2$e), "e"] <- "-"
  c2[grep("^h$", c2$e), "b"] <- ""
  #c2[grep("2.55 \\(10% solution\\)\\/100 g bw", c2$a), "e"] <- gsub("(\\d+\\.\\d*)(\\s+)(\\(.*)", "\\3", c2[grep("2.55 \\(10% solution\\)\\/100 g bw", c2$a), "a"] )
  #[1] "147 <= 167" "10 < 100"
  c2[grep("^\\d+\\s+(<|<=)\\s+\\d+$", c2$a, ignore.case = T), "e"] <- "-"
  c2[grep("deviation from control", c2$e), "e"] <- "-"
  c2[grep("percent", c2$e, ignore.case = T), "e"] <- "%"
  c2[grep("mmol Ni\\/l", c2$e, ignore.case = T), "e"] <- gsub("(.*)(\\s+\\+.*)","\\1",c2[grep("mmol Ni\\/l", c2$e, ignore.case = T), "e"])
  c2[grep("of the filtrate of a saturated solution", c2$e, ignore.case = T), "e"] <- "%"
  c2[grep("v/v Saturated Solution\\)", c2$e, ignore.case = T), "e"] <- gsub("(.*\\(+)(.*)(\\)+)","\\2",c2[grep("v/v Saturated Solution\\)", c2$e, ignore.case = T), "a"])
  c2[grep("Offspring not reported", c2$e, ignore.case = T), "e"] <- "-"
  c2[grep("nominal\\) ppm \\(16774 mg\\/m3\\)", c2$e, ignore.case = T), "e"] <- "ppm (nominal)"
  c2[grep("not relevant according to OECD", c2$e, ignore.case = T), "e"] <- "-"
  c2[grep("^in diet$", c2$e, ignore.case = T), "e"] <- "% in diet"
  c2[grep("^l$", c2$e, ignore.case = T), "e"] <- "ul"
  c2[grep("ca\\.", c2$e, ignore.case = T), "e"] <-  gsub("(.*\\.\\s*\\d+\\s*)(.*)","\\2",c2[grep("ca\\.", c2$e, ignore.case = T), "e"])
  c2[grep("ul\\/l) ppm", c2$e, ignore.case = T), "e"] <- "ppm"
  c2[grep("^g dissolved", c2$e, ignore.case = T), "e"] <- paste("u",c2[grep("^g dissolved", c2$e, ignore.case = T), "e"], sep = "")
  c2[grep("mg/rat|mg/mouse|mg/animal|mg/disk|mg/dish", c2$e, ignore.case = T), "e"] <- gsub("(mg)(\\/\\w+)(\\/*.*)","\\1\\3",c2[grep("mg/rat|mg/mouse|mg/animal|mg/disk|mg/dish", c2$e, ignore.case = T), "e"])

  #generation info from dev tox file as g in dictionary
  c2$g <- "-"
  names(c2) <- names(c)
  # all remaining values , starts with qualifier values and ones that has single value
  #c3 <- unique(res1[which(is.na(res1$toxval_numeric)), "experimental_value"])
  c3 <- unique(res1[grep("^\\d+\\.*\\d*$|^[^[:alnum:]]\\s*\\d+\\.*\\d*\\s*\\w+",res1$toxval_numeric), "toxval_numeric"])
  c3 <- data.frame(c3,stringsAsFactors = F)
  names(c3) <- "a"
  c3$b <- gsub("(.*?\\s+)(\\d+\\.*\\d*)(.*)","\\2",c3$a)
  c3[grep("^[^[:alnum:]].*",c3$a ),"d"] <- gsub("([^[:alnum:]])(\\s+\\d+.*)","\\1",c3[grep("^[^[:alnum:]].*",c3$a ),"a"] )

  c3[grep("^\\d+",c3$a),"b"] <- gsub("(^\\d+\\.*\\d*)(.*)","\\1",c3[grep("^\\d+",c3$a),"a"])
  c3[grep("^\\d+",c3$a),"d"] <- "-"
  c3$e <- gsub("(.*\\s+)(\\d+\\.*\\d*\\s*)(.*)","\\3",c3$a)
  c3[grep("^\\d+",c3$a),"e"] <- "-"
  c3[grep("mg/m.*air.*\\(.*", c3$a), "e"] <- gsub( "(.*)(mg/m)(.*)(\\s*air.*)","\\23 \\4",c3[grep("mg/m.*air.*\\(.*", c3$a), "a"])
  c3[grep("mg/m.*air$", c3$a), "e"] <- gsub( "(.*)(mg/m)(.*)(\\s*air)","\\23 \\4",c3[grep("mg/m.*air$", c3$a), "a"])
  c3[grep("mg/L.*air$", c3$a), "e"] <- gsub( "(.*)(mg/L)(.*)(\\s*air)","\\23 \\4",c3[grep("mg/L.*air$", c3$a), "a"])
  c3[grep("cm.*[^0-9kg]$", c3$a), "e"] <- gsub("(.*\\s+)(.*g/cm)(.*)","\\22",c3[grep("cm.*[^0-9kg]$", c3$a), "a"])
  c3[grep("\\d+[^0-9\\.]+\\d+\\s+mg/L", c3$a), "e"] <- gsub("(.*\\s+?)(mg.*)","\\2",c3[grep("\\d+[^0-9\\.]+\\d+\\s+mg/L", c3$a), "a"])
  c3[grep("mm[^3]/kg", c3$a), "e"] <- gsub("(.*)(mm)(.*)(/kg.*)","\\23\\4",c3[grep("mm[^3]/kg", c3$a), "a"])
  c3[grep("\\d+[^0-9\\.]+\\d+.*[^m|^kg]g/L", c3$a), "e"] <- "ug/L"
  c3[grep("\\d+[^0-9\\.]+\\d+.*[^m|^kg]g.*Ag/L", c3$a, ignore.case = T), "e"] <- "?g Ag/l"
  c3[grep("\\d+[^0-9\\.]+\\d+.*mg/kg", c3$a, ignore.case = T), "e"] <- gsub("(.*\\s+)(mg.*)","\\2",c3[grep("\\d+[^0-9\\.]+\\d+.*mg/kg", c3$a, ignore.case = T), "a"])
  c3[grep("\\d+[^0-9\\.]+\\d+.*mg/L$", c3$a), "e"] <- gsub("(.*\\s+)(mg.*)","\\2",c3[grep("\\d+[^0-9\\.]+\\d+.*mg/L$", c3$a), "a"])
  c3[grep(".*g/m[^3|^l|^L]", c3$e), "e"] <- gsub("(.*m)(.*)","\\13",c3[grep(".*g/m[^3|^l|^L]", c3$e), "e"])
  c3[grep("[^[:alnum:]|^\\(]g", c3$e), "e"] <- gsub("(.*?)(g.*)","u\\2",c3[grep("[^[:alnum:]|^\\(]g", c3$e), "e"])
  c3[grep("[^[:alnum:]]mol", c3$e), "e"] <- gsub("(.*)(mol.*)","u\\2",c3[grep("[^[:alnum:]]mol", c3$e), "e"])
  c3[grep("\\.[4-9]$", c3$e), "e"] <- "-"
  c3[grep("[^[:alnum:]]l/l|[^[:alnum:]]l/kg", c3$e, ignore.case = T), "e"] <- gsub("(.*?)([l|L].*)","u\\2",c3[grep("[^[:alnum:]]l/l|[^[:alnum:]]l/kg", c3$e, ignore.case = T), "e"])
  c3[grep("[^[:alnum:]]/ml", c3$e, ignore.case = T), "e"] <- gsub("(.*?)(/ml.*)","u\\2",c3[grep("[^[:alnum:]]/ml", c3$e, ignore.case = T), "e"])
  c3[grep("^\\(%.*[^\\)]$", c3$e), "e"] <- gsub("(.*)(%.*)","\\2",c3[grep("^\\(%.*[^\\)]$", c3$e), "e"])
  c3[grep("^[^a-zA-Z\\d%\\(\\-]", c3$e), "e"] <- gsub("([^[:alnum:]]\\s+)(.*)","\\2",c3[grep("^[^a-zA-Z\\d%\\(\\-]", c3$e), "e"])
  c3$e[c3$e == ""] <- "-"

  #generation column
  c3$g <- "-"

  # generation info from experimental_value
  c4 <- unique(res1[grep("Maternal|Fetal", res1$experimental_value, ignore.case = T), "experimental_value"])
  c4 <- data.frame(c4, stringsAsFactors = F)
  names(c4) <- "a"
  c4$b <- c4$a
  c4$d <- "-"
  c4$e <- "-"
  c4$g <- "-"
  c4[grep("Fetal|Maternal", c4$a, ignore.case = T), "b"] <- gsub("(\\w+\\:\\s*)(.*)","\\2",c4[grep("Fetal|Maternal", c4$a, ignore.case = T), "a"])
  c4[grep("Fetal|Maternal", c4$a, ignore.case = T), "g"] <- gsub("(\\w+)(\\:\\s*)(.*)","\\1",c4[grep("Fetal|Maternal", c4$a, ignore.case = T), "a"])
  c4[grep(".*Maternal dose.*", c4$g, ignore.case = T), "b"] <- c4[grep(".*Maternal dose.*", c4$g, ignore.case = T), "g"]
  c4[grep(".*Maternal dose.*", c4$g, ignore.case = T), "e"] <- gsub("(.*)(mg.*[^maternal dose])","\\2",c4[grep(".*Maternal dose.*", c4$g, ignore.case = T), "g"])
  c4[grep(".*Maternal dose.*", c4$g, ignore.case = T), "e"] <- gsub("\\s*\\(*maternal dose\\)*|\\)$","",c4[grep(".*Maternal dose.*", c4$g, ignore.case = T), "e"])

  c4[grep("\\(Maternal\\)", c4$g, ignore.case = T), "b"] <- c4[grep("\\(Maternal\\)", c4$g, ignore.case = T), "g"]
  c4[grep("\\(Maternal\\)", c4$g, ignore.case = T), "e"] <-  gsub("(.*)(mg.*)(\\s+\\(.*\\))","\\2",c4[grep("\\(Maternal\\)", c4$g, ignore.case = T), "g"])

  c4$g <- tolower(c4$g)
  c4$g <- unlist(str_extract_all(c4$g,"maternal|fetal"))

  c4[grep(".*Maternal.*bw/day", c4$b, ignore.case = T), "e"] <- gsub("(.*)(mg.*$)","\\2",c4[grep(".*Maternal.*bw/day", c4$b, ignore.case = T), "b"])

  c4[grep("^\\d+\\.*\\d*\\s+\\w+", c4$b), "e"] <- gsub("(^\\d+\\.*\\d*\\s+)(.*)","\\2",c4[grep("^\\d+\\.*\\d*\\s+\\w+", c4$b), "b"])
  c4[grep("/m[^0-9]\\s*air", c4$e), "e"] <- gsub("(.*/m)([^[0-9])(\\s*air)","\\13\\3",c4[grep("/m[^0-9]\\s*air", c4$e), "e"])
  c4[grep("^\\d+\\.*\\d*\\s+\\w+", c4$b), "b"] <- gsub("(^\\d+\\.*\\d*)(\\s+.*)","\\1",c4[grep("^\\d+\\.*\\d*\\s+\\w+", c4$b), "b"])

  c4[grep("^[^[:alnum:]]+\\s+\\d+\\.*\\d*\\s+\\w+", c4$b), "d"] <- gsub("(^[^[:alnum:]]+)(\\s+)(\\d+\\.*\\d*)(\\s+)(.*)","\\1",c4[grep("^[^[:alnum:]]+\\s+\\d+\\.*\\d*\\s+\\w+", c4$b), "b"])
  c4[grep("^[^[:alnum:]]+\\s+\\d+\\.*\\d*\\s+\\w+", c4$b), "e"] <- gsub("(^[^[:alnum:]]+)(\\s+)(\\d+\\.*\\d*)(\\s+)(.*)","\\5",c4[grep("^[^[:alnum:]]+\\s+\\d+\\.*\\d*\\s+\\w+", c4$b), "b"])
  c4[grep("^[^[:alnum:]]+\\s+\\d+\\.*\\d*\\s+\\w+", c4$b), "b"] <- gsub("(^[^[:alnum:]]+)(\\s+)(\\d+\\.*\\d*)(\\s+)(.*)","\\3",c4[grep("^[^[:alnum:]]+\\s+\\d+\\.*\\d*\\s+\\w+", c4$b), "b"])


  c4[grep("^ca", c4$b), "d"] <- gsub("(ca\\.)(\\s*)(\\d+\\.*\\d*)(\\s*)(.*)","\\1",c4[grep("^ca", c4$b), "b"])
  c4[grep("^ca", c4$b), "e"] <- gsub("(ca\\.)(\\s*)(\\d+\\.*\\d*)(\\s*)(.*)","\\5",c4[grep("^ca", c4$b), "b"])
  c4[grep("^ca", c4$b), "b"] <- gsub("(ca\\.)(\\s*)(\\d+\\.*\\d*)(\\s*)(.*)","\\3",c4[grep("^ca", c4$b), "b"])


  c4[grep("^[^[:alnum:]]", c4$b), "d"] <- gsub("(^[^[:alnum:]]+)(\\s*)(\\d+\\.*\\d*)(.*)","\\1",c4[grep("^[^[:alnum:]]", c4$b), "b"])
  c4[grep("^[^[:alnum:]]", c4$b), "e"] <- gsub("(^[^[:alnum:]]+)(\\s*)(\\d+\\.*\\d*\\s*)(.*)","\\4",c4[grep("^[^[:alnum:]]", c4$b), "b"])
  c4[grep("^[^[:alnum:]]", c4$b), "b"] <- gsub("(^[^[:alnum:]]+)(\\s*)(\\d+\\.*\\d*)(.*)","\\3",c4[grep("^[^[:alnum:]]", c4$b), "b"])


  c4[grep("%", c4$b), "e"] <- gsub("(^\\d+\\.*\\d*\\s*)(.*)","\\2",c4[grep("%", c4$b), "b"])
  c4[grep("%", c4$b), "b"] <- gsub("(^\\d+\\.*\\d*)(\\s*.*)","\\1",c4[grep("%", c4$b), "b"])


  c4[grep("[^[:alnum:]|^\\.]", c4$b), "e"] <- gsub("(^\\d+\\.*\\d*)(.*?\\s+)(.*)","\\3",c4[grep("[^[:alnum:]|^\\.]", c4$b), "b"])
  c4[grep("[^[:alnum:]|^\\.]", c4$b), "b"] <- gsub("(^\\d+\\.*\\d*)(.*?\\s+)(.*)","\\1",c4[grep("[^[:alnum:]|^\\.]", c4$b), "b"])

  c4[grep("^[^[:alnum:]|^%]",c4$e),"e"] <- gsub("(^[^[:alnum:]]+\\s*\\d+\\.*\\d*\\s*)(.*)","\\2",c4[grep("^[^[:alnum:]|^%]",c4$e),"e"])


  c4[grep("^\\d+",c4$e),"b"] <- gsub("(\\d+)(.*)","\\1",c4[grep("^\\d+",c4$e),"e"])
  c4[grep("^\\d+",c4$e),"e"] <- gsub("(\\d+)(.*)","\\2",c4[grep("^\\d+",c4$e),"e"])

  c4$e[c4$e == ""] <- "-"
  c4[grep("/m[^0-9]\\s*air", c4$e), "e"] <- gsub("(.*/m)([^[0-9])(\\s*air)","\\13\\3",c4[grep("/m[^0-9]\\s*air", c4$e), "e"])


  dict_effect_levels <- rbind(c,c1,c2,c3,c4)
  names(dict_effect_levels) <- c("original_effect_level", "toxval_numeric", "toxval_numeric_qualifier", "toxval_units", "generation")

  x <- unique(res1$experimental_value)

  x <- x[!is.element(x,dict_effect_levels[,1])]

  cat("   missing values in dictionary:",length(x),"\n")

  c5 <- data.frame(x, stringsAsFactors = F)
  names(c5) <- "a"
  c5$b <- c5$a
  c5$d <- "-"
  c5$e <- "-"
  c5$g <- "-"

  c5[grep("^ca. 0 [ca]*[^[:alnum:]] ", c5$a), "e"] <- gsub("(ca. 0)(.*)(mg.*)","\\3",c5[grep("^ca. 0 [ca]*[^[:alnum:]] ", c5$a), "a"])
  c5[grep("^ca. 0 [ca]*[^[:alnum:]] ", c5$a), "d"] <- gsub("(ca. 0\\s+)(.*?)(\\d+.*)","\\2",c5[grep("^ca. 0 [ca]*[^[:alnum:]] ", c5$a), "a"])
  c5[grep("^ca. 0 [ca]*[^[:alnum:]] ", c5$a), "b"] <- gsub("(ca. 0\\s+)(.*?)(\\d+\\.*\\d*)(.*)","\\3",c5[grep("^ca. 0 [ca]*[^[:alnum:]] ", c5$a), "a"])

  c5[grep("^ca\\. \\d+\\.*\\d* \\w+.*", c5$b), "d"] <- gsub("(^ca\\.)(\\s+)(\\d+\\.*\\d*)(\\s+)(\\w+.*)","\\1",c5[grep("^ca\\. \\d+\\.*\\d* \\w+.*", c5$b), "b"])
  c5[grep("^ca\\.\\s+\\d+\\.*\\d*\\s+\\w+.*", c5$b), "e"] <- gsub("(^ca\\.)(\\s+)(\\d+\\.*\\d*)(\\s+)(\\w+.*)","\\5",c5[grep("^ca\\.\\s+\\d+\\.*\\d*\\s+\\w+.*", c5$b), "b"])
  c5[grep("^ca\\.\\s+\\d+\\.*\\d*\\s+\\w+.*", c5$e), "e"] <- gsub("(^ca\\.)(\\s+)(\\d+\\.*\\d*)(\\s+)(\\w+.*)","\\5",c5[grep("^ca\\.\\s+\\d+\\.*\\d*\\s+\\w+.*", c5$e), "e"])
  c5[grep("^ca\\.", c5$e), "e"] <- gsub("(ca\\.\\s*[0-9]*\\s*)(.*)","\\2",c5[grep("^ca\\.", c5$e), "e"])
  c5[grep("^ca\\.\\s+\\d+\\.*\\d*\\s+\\w+.*", c5$b), "b"] <- gsub("(^ca\\.)(\\s+)(\\d+\\.*\\d*)(\\s+)(\\w+.*)","\\3",c5[grep("^ca\\.\\s+\\d+\\.*\\d*\\s+\\w+.*", c5$b), "b"])


  c5[grep("^[^[:alnum:]]+\\s+\\d+\\.*\\d*\\s+", c5$b), "e"] <- gsub("(^[^[:alnum:]]+)(\\s+)(\\d+\\.*\\d*)(\\s+)(.*)","\\5",c5[grep("^[^[:alnum:]]+\\s+\\d+\\.*\\d*\\s+", c5$b), "b"])
  c5[grep("^[^[:alnum:]]+\\s+\\d+\\.*\\d*\\s+", c5$b), "d"] <- gsub("(^[^[:alnum:]]+)(\\s+)(\\d+\\.*\\d*)(\\s+)(.*)","\\1",c5[grep("^[^[:alnum:]]+\\s+\\d+\\.*\\d*\\s+", c5$b), "b"])
  c5[grep("^[^[:alnum:]]+\\s+\\d+.*", c5$e), "e"] <- gsub("(^[^[:alnum:]]+\\s+\\d+\\.*\\d*\\s*)(.*)","\\2",c5[grep("^[^[:alnum:]]+\\s+\\d+.*", c5$e), "e"])
  c5[grep("^[^[:alnum:]%\\-]+", c5$e), "e"] <- gsub("(^[^[:alnum:]]+\\s+)(.*)","\\2",c5[grep("^[^[:alnum:]%\\-]+", c5$e), "e"])
  c5[grep("^[^[:alnum:]]+\\s+\\d+\\.*\\d*\\s+", c5$b), "b"] <- gsub("(^[^[:alnum:]]+)(\\s+)(\\d+\\.*\\d*)(\\s+)(.*)","\\3",c5[grep("^[^[:alnum:]]+\\s+\\d+\\.*\\d*\\s+", c5$b), "b"])

  c5[grep("^ca\\.", c5$b), "d"] <- gsub("(^ca\\.)(\\s*)(\\d+\\.*\\d*)(.*)","\\1",c5[grep("^ca\\.", c5$b), "b"])
  c5[grep("^ca\\.", c5$b), "e"] <- gsub("(^ca\\.)(\\s*)(\\d+\\.*\\d*)(\\s*)(.*)","\\5",c5[grep("^ca\\.", c5$b), "b"])
  c5[grep("^[^[:alnum:]%\\-]", c5$e), "e"] <- gsub("(^[^[:alnum:]%]+)(\\s*)(\\d+\\.*\\d*)(\\s*)(.*)","\\5",c5[grep("^[^[:alnum:]%\\-]", c5$e), "e"])
  c5[grep("^[^[:alnum:]%\\(|\\-]", c5$e), "e"] <- gsub("(.*\\s+)(.*)","\\2",c5[grep("^[^[:alnum:]%\\(|\\-]", c5$e), "e"])


  c5[grep("^ca\\.", c5$b), "e"] <- gsub("(^ca\\.)(\\s+)(\\d+\\.*\\d*)(\\s*)(.*)","\\5",c5[grep("^ca\\.", c5$b), "b"])
  c5[grep("^[^[:alnum:]]g/cm.*", c5$e), "e"] <- gsub("(^[^[:alnum:]])(g/cm)(.*)","u\\22",c5[grep("^[^[:alnum:]]g/cm.*", c5$e), "e"])
  c5[grep("^[^[:alnum:]%\\(|\\-]", c5$e), "e"] <- gsub("(^[^[:alnum:]]+\\s*\\d*\\.*\\d*\\s*)(\\w+.*)","\\2",c5[grep("^[^[:alnum:]%\\(|\\-]", c5$e), "e"])
  c5[grep("^ca\\.", c5$b), "b"] <- gsub("(^ca\\.)(\\s*)(\\d+\\.*\\d*)(.*)","\\3",c5[grep("^ca\\.", c5$b), "b"])

  c5[grep("^[^[:alnum:]+\\.]", c5$b), "d"] <- gsub("(^[^[:alnum:]]+)(\\s*)(\\d+\\.*\\d*)(.*)","\\1",c5[grep("^[^[:alnum:]+\\.]", c5$b), "b"])
  c5[grep("^[^[:alnum:]+\\.]", c5$b), "e"] <- gsub("(^[^[:alnum:]]+)(\\s*)(\\d+\\.*\\d*)(.*)","\\4",c5[grep("^[^[:alnum:]+\\.]", c5$b), "b"])
  c5[grep("^[^[:alnum:]%\\(|\\-]", c5$e), "e"] <- gsub("(^[^[:alnum:]]+\\s*\\d*\\.*\\d*\\s*)(\\w+.*)","\\2",c5[grep("^[^[:alnum:]%\\(|\\-]", c5$e), "e"])
  c5[grep("^[^[:alnum:]+\\.]", c5$b), "b"] <- gsub("(^[^[:alnum:]]+)(\\s*)(\\d+\\.*\\d*)(.*)","\\3",c5[grep("^[^[:alnum:]+\\.]", c5$b), "b"])

  c5[grep("^\\d+\\.*\\d*[^[:alnum:]\\.]\\d+\\.*\\d*.*", c5$b), "e"] <- gsub("(^\\d+\\.*\\d*)([^[:alnum:]\\.]\\d+\\.*\\d*)(\\s*)(.*)","\\4",c5[grep("^\\d+\\.*\\d*[^[:alnum:]\\.]\\d+\\.*\\d*.*", c5$b), "b"])
  c5[grep("^\\d+\\.*\\d*[^[:alnum:]\\.]\\d+\\.*\\d*.*", c5$b), "b"] <- gsub("(^\\d+\\.*\\d*)([^[:alnum:]\\.]\\d+\\.*\\d*)(\\s*)(.*)","\\1",c5[grep("^\\d+\\.*\\d*[^[:alnum:]\\.]\\d+\\.*\\d*.*", c5$b), "b"])

  c5$d <- gsub("^\\s+|\\s+$","", c5$d)
  c5[grep("mm[^0-9a-z]",c5$e),"e"] <- gsub("(mm)(.*)(/.*)","\\13\\3",c5[grep("mm[^0-9a-z]",c5$e),"e"])
  c5[grep("cm[^0-9\\-]",c5$e),"e"] <- gsub("(.*cm)([^0-9a-z])(.*)","\\12\\3",c5[grep("cm[^0-9\\-]",c5$e),"e"])
  c5[grep("/m[^3].*air",c5$e),"e"] <- gsub("(.*/m)([^0-9a-z])(\\s*air.*)","\\13\\3",c5[grep("/m[^3].*air",c5$e),"e"])
  c5[grep("^[^m|u|k|n]g",c5$e),"e"] <- str_replace(c5[grep("^[^m|u|k|n]g",c5$e),"e"],".*?g","ug")
  c5$e[c5$e == ""] <- "-"


  names(c5) <- c("original_effect_level", "toxval_numeric", "toxval_numeric_qualifier", "toxval_units", "generation")
  dict_effect_levels_2 <- rbind(dict_effect_levels,c5)

  x <- unique(res1$experimental_value)

  x <- x[!is.element(x,dict_effect_levels_2[,1])]

  cat("   missing values in new dictionary:",length(x),"\n")

  #runInsertTable(dict_effect_levels_2, "echa_echemportal_api_dict", toxval.db, do.halt=T,verbose=F)
  #####################################################################
  cat("Dict, while reading in has altered copies of values due to some system level encoding changes\n")
  #####################################################################

  query <- "select * from echa_echemportal_api_dict"

  dict_effect_levels_2 <- runQuery(query,toxval.db,T,F)

  x <- unique(res1$experimental_value)

  x <- x[!is.element(x,dict_effect_levels_2[,1])]

  cat("   missing values in new dictionary:",length(x),"\n")

  #####################################################################
  cat("fix - altered copies of values in dictionary due to multiple encoding issues\n")
  #####################################################################

  x <- data.frame(x,rep("",length(x)), rep("-",length(x)),rep("-",length(x)),rep("-",length(x)),stringsAsFactors = F)
  names(x) <- c("original_effect_level","toxval_numeric","toxval_numeric_qualifier","toxval_units","generation")
  dict_effect_levels_3 <- rbind(dict_effect_levels_2, x)
  dict_effect_levels_3$original_effect_level_1 <- dict_effect_levels_3$original_effect_level

  dict_effect_levels_3[,-1] <- fix.non_ascii(dict_effect_levels_3[,-1])

  dict_effect_levels_3[grep(".*(<c.*\\?).*",dict_effect_levels_3$original_effect_level_1),"original_effect_level_1"] <- gsub("(<c[0-9b]>\\?)+","XXX",dict_effect_levels_3[grep(".*(<c.*\\?).*",dict_effect_levels_3$original_effect_level_1),"original_effect_level_1"])
  dict_effect_levels_3[grep("XXX",dict_effect_levels_3$original_effect_level_1),"original_effect_level_1"] <- gsub("(XXX)+","XXX",dict_effect_levels_3[grep("XXX",dict_effect_levels_3$original_effect_level_1),"original_effect_level_1"])


  print(dim(dict_effect_levels_3))
  dict_effect_levels_4 <- dict_effect_levels_3[1:36097,]
  dict_effect_levels_4[grep("XXX",dict_effect_levels_4$original_effect_level_1),"original_effect_level_1"] <- gsub("(XXX\\?)+","XXX?",dict_effect_levels_4[grep("XXX",dict_effect_levels_4$original_effect_level_1),"original_effect_level_1"])
  dict_effect_levels_4[grep("(XXX\\?XXX)",dict_effect_levels_4$original_effect_level_1),"original_effect_level_1"] <- gsub("(XXX\\?XXX)","XXX?",dict_effect_levels_4[grep("(XXX\\?XXX)",dict_effect_levels_4$original_effect_level_1),"original_effect_level_1"])

  # file <- paste0("./dictionary/echa_echemportal_api_dict4_",Sys.Date(),".xlsx")
  # write.xlsx(dict_effect_levels_4, file)


  #dict_effect_levels_5 <- dict_effect_levels_3[36098:37987,]
  dict_effect_levels_5 <- dict_effect_levels_3[36098:41512,]
  dict_effect_levels_5[grep("XXX",dict_effect_levels_5$original_effect_level_1),"original_effect_level_1"] <- gsub("(XXX)+","XXX?",dict_effect_levels_5[grep("XXX",dict_effect_levels_5$original_effect_level_1),"original_effect_level_1"])
  # file <- paste0("./dictionary/echa_echemportal_api_dict5_",Sys.Date(),".xlsx")
  # write.xlsx(dict_effect_levels_5, file)


  dict_effect_levels_5[,2] <- dict_effect_levels_4[match(dict_effect_levels_5$original_effect_level_1, dict_effect_levels_4$original_effect_level_1), 2]
  dict_effect_levels_5[,3] <- dict_effect_levels_4[match(dict_effect_levels_5$original_effect_level_1, dict_effect_levels_4$original_effect_level_1), 3]
  dict_effect_levels_5[,4] <- dict_effect_levels_4[match(dict_effect_levels_5$original_effect_level_1, dict_effect_levels_4$original_effect_level_1), 4]
  dict_effect_levels_5[,5] <- dict_effect_levels_4[match(dict_effect_levels_5$original_effect_level_1, dict_effect_levels_4$original_effect_level_1), 5]

  dict_effect_levels_6 <- rbind(dict_effect_levels_4,dict_effect_levels_5)
  names(dict_effect_levels_6) <- names(dict_effect_levels_5)

  file <- paste0("./dictionary/echa_echemportal_api_dict6_",Sys.Date(),".xlsx")
  write.xlsx(dict_effect_levels_6, file)

  dict2 <- read.xlsx('./dictionary/echa_echemportal_api_dict6_2021-08-18.xlsx')
  dict2 <- dict2[,-1]
  dict2 <- dict2[,c(5,1,2,3,4)]
  names(dict2)[names(dict2) == "original_effect_level_1"] <- "original_effect_level"

  dict2$toxval_numeric_qualifier[grep("XXX", dict2$toxval_numeric_qualifier)] <- gsub("(XXX\\?)+","XXX?",dict2$toxval_numeric_qualifier[grep("XXX", dict2$toxval_numeric_qualifier)])
  dict2$toxval_units[grep("XXX", dict2$toxval_units)] <- gsub("(XXX\\?)+","XXX?",dict2$toxval_units[grep("XXX", dict2$toxval_units)])
  dict2[grep("[0-9]\\s*XXX\\?g/L$", dict2$original_effect_level),"toxval_units"] <- "XXX?g/L"
  dict2[grep("(XXX\\?XXX)", dict2$toxval_units),"toxval_units"] <- gsub("(XXX\\?XXX)","XXX?",dict2[grep("(XXX\\?XXX)", dict2$toxval_units),"toxval_units"])
  dict2[grep("[0-9]\\s*XXX\\?[a-zA-Z]+", dict2$original_effect_level),"toxval_units"] <- gsub("(.*[0-9]\\s*)(XXX\\?[a-zA-Z]+.*)","\\2",dict2[grep("[0-9]\\s*XXX\\?[a-zA-Z]+", dict2$original_effect_level),"original_effect_level"])
  dict2[grep("[0-9]\\s*\\(XXX\\?[a-zA-Z]+", dict2$original_effect_level),"toxval_units"] <- gsub("(.*[0-9]\\s*)(\\(XXX\\?[a-zA-Z]+.*)","\\2",dict2[grep("[0-9]\\s*\\(XXX\\?[a-zA-Z]+", dict2$original_effect_level),"original_effect_level"])

  #
  print(dim(dict2))
  dict2 <- unique(dict2)
  print(dim(dict2))

  file <- paste0("./dictionary/echa_echemportal_api_dict_",Sys.Date(),".xlsx")
  write.xlsx(dict2, file)

  runInsertTable(dict2, "echa_echemportal_api_dict", toxval.db, do.halt=T,verbose=F)

}
