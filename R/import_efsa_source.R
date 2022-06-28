#--------------------------------------------------------------------------------------
#' Process the raw excel files downloaded from EFSA version3(March 27 2020)
#' To get the files, go to the web site
#' https://zenodo.org/record/3693783#.XrsBMmhKjIU. At the bottom are links to
#' a set of Excel files - download all of them into the next version V3,
#' convert the xlsx files to csv since the xlsx files when read in R converts all symbols/special characters
#' to certain unicode values (space to '_x0020_'). while reading the original xlsx files into R
#' it was unsuccessful to convert enconding to UTF-8, also tried converting using stringi. Only workaround was
#' by converting the downloaded files to csv and using the csv files as input source.
#'
#' modify the field names at the beginning of this script
#' @param db The version of toxval into which the source is loaded.
#' @return Merged tidy excel file that details the data in EFSA
#' @export
#--------------------------------------------------------------------------------------
import_efsa_source <- function(db,
                               chem.check.halt=F){
  printCurrentFunction(db)

  #####################################################################
  cat("Build new_efsa from input csv files \n")
  #####################################################################
  #points <- openxlsx::read.xlsx("../efsa/efsa_files/ReferencePoints_KJ_2020.xlsx")
  #chems <- openxlsx::read.xlsx("../efsa/efsa_files/SubstanceCharacterisation_KJ_2020.xlsx")
  #values <- openxlsx::read.xlsx("../efsa/efsa_files/ReferenceValues_KJ_2020.xlsx")
  #details <- openxlsx::read.xlsx("../efsa/efsa_files/EFSAOutputs_KJ_2020.xlsx")

  points <- read.csv("../efsa/efsa_files/ReferencePoints_KJ_2020.csv",stringsAsFactors=F)
  chems <- read.csv("../efsa/efsa_files/SubstanceCharacterisation_KJ_2020.csv",stringsAsFactors=F)
  values <- read.csv("../efsa/efsa_files/ReferenceValues_KJ_2020.csv",stringsAsFactors=F)
  details <- read.csv("../efsa/efsa_files/EFSAOutputs_KJ_2020.csv",stringsAsFactors=F)

  names(values) <- c("name","subsource","year","output_id","toxval_type","toxval_numeric_qualifier","toxval_numeric","toxval_units","population")
  names(points) <- c("name","subsource","year","output_id","human_eco","study_type","species_original",
                     "route","study_duration_value","toxval_type","toxval_numeric_qualifier","toxval_numeric","toxval_units","critical_effect","system")
  names(details) <- c("name","output_id","legal_basis","subsource","publication_date","title","output_type","doi","record_url")
  names(chems) <- c("name","substance_type","component","casrn","ec_number","mol_formula","smiles")
  # this process will expand the number of rows becasue one substance can map to multiple components,
  # which is the source of the CASRN
  chems <- chems[,c("casrn","name")]
  cat("dim(points) ",dim(points),"\n")
  cat("dim(values) ",dim(values),"\n")
  points <- merge(chems,points,by="name")
  cat("dim(points) ",dim(points),"\n")
  values <- merge(chems,values,by="name")
  cat("dim(values) ",dim(values),"\n")
  for (i in names(points)[!(names(points) %in% names(values))]){
    values[,length(values)+1]="-"
    names(values)[length(values)] = i
  }
  for (i in names(values)[!(names(values) %in% names(points))]){
    points[,length(points)+1]="-"
    names(points)[length(points)] = i
  }
  points[,order(names(points))] -> points
  values[,order(names(values))] -> values
  rbind(points,values)->total

  separate(total, route, c("exposure_route","exposure_method"), sep=": ", fill="right") ->total
  total[is.na(total$exposure_method),"exposure_method"] = "-"
  total$study_duration_units <- "-"
  total[!is.na(total[,"study_duration_value"]),"study_duration_units"] <- "days"
  details$year <- gsub("(.*[a-zA-Z]+)(\\d{4})","\\2",details$publication_date)
  total$title <- "-"
  total$long_ref <- "-"
  total$source <- "EFSA"
  total$subsource <- "OpenFoodTox"
  total$source_url <- "https://zenodo.org/record/3693783#.XrsBMmhKjIU"
  total$record_url <- "-"
  #total$url <- "-"
  #total$journal <- "-"
  i <- sapply(total, is.factor)
  total[i] <- lapply(total[i], as.character)

  for(i in 1:nrow(total)) {
    cname <- total[i,"name"]
    opid <- total[i,"output_id"]
    x <- details[is.element(details[,"name"],cname),]
    j <- sapply(x, is.factor)
    x[j] <- lapply(x[j], as.character)
    y <- x[is.element(x[,"output_id"],opid),]
    k <- sapply(y, is.factor)
    y[k] <- lapply(y[k], as.character)
    total[i,"title"] <- x[1,"title"]
    total[i,"long_ref"] <- x[1,"title"]
    total[i,"record_url"] <- x[1,"record_url"]
    total[i,"record_source_type"] <- x[1,"output_type"]
    total[i,"year"] <- x[1,"year"]
    total[i,"subsource"] <- x[1,"subsource"]
    if(i%%1000==0) cat("processed",i,"out of",nrow(total),"\n")
  }
  x <- total[,"study_type"]
  x[is.element(x,"acute toxicity")] <- "acute"
  x[is.element(x,"reproduction toxicity")] <- "reproductive"
  x[is.element(x,"short-term toxicity")] <- "short-term"
  x[is.element(x,"study with volunteers")] <- "human"
  total[,"study_type"] <- x

  x <- total[,"human_eco"]
  x[is.element(x,"Animal (non-target species) health")] <- "human health"
  x[is.element(x,"Animal (target species) health")] <- "human health"
  x[is.element(x,"Ecotox (soil compartment)")] <- "eco"
  x[is.element(x,"Ecotox (water compartment)")] <- "eco"
  x[is.element(x,"Human health")] <- "human health"
  total[,"human_eco"] <- x

  x <- total[,"toxval_units"]
  x[is.element(x,"mg/m³")] <- "mg/m3"
  x[is.element(x,"µg/day")] <- "ug/day"
  x[is.element(x,"µg/piece")] <- "ug/piece"
  x[is.element(x,"µg/kg")] <- "ug/kg"
  x[is.element(x,"µg/kg bw")] <- "ug/kg bw"
  x[is.element(x,"µg/kg bw/day")] <- "ug/kg bw/day"
  x[is.element(x,"µg/kg bw/week")] <- "ug/kg bw/week"
  total[,"toxval_units"] <- x

  name.list <- names(total)
  name.list <- name.list[!is.element(name.list,"system")]
  total <- total[,name.list]
  name.list <- c("casrn","name","source","subsource","source_url","record_url","toxval_type","toxval_numeric","toxval_numeric_qualifier","toxval_units",
                 "critical_effect","population","exposure_route","exposure_method","study_type","study_duration_value","study_duration_units",
                 "species_original","human_eco","year","long_ref","title","output_id", "record_source_type")

  total <- total[,name.list]
  total$study_duration_value <- gsub("-","", total$study_duration_value)
  total <- lapply(total, function(x) type.convert(as.character(x), as.is = T))
  total <- data.frame(total, stringsAsFactors = F)
  total["efsa_id"] <- c(1:length(total[,1]))
  #openxlsx::write.xlsx(total, "../efsa/efsa_files/new_EFSA_combined.xlsx")

  res = as.data.frame(total)
  res = res[!is.na(res$casrn),]
  res = res[res$casrn!="",]
  badcas = c("601-803-4","2106-46-6","999999-91-4","53383-2-7")
  res = res[!is.element(res$casrn,badcas),]
  res$url = "-"
  res$source_study_id = -1

  #####################################################################
  cat("Prep and load the data\n")
  #####################################################################
  source_prep_and_load(db,source="EFSA",table="source_efsa",res=res,F,T,T)
}
