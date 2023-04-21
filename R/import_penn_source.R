#--------------------------------------------------------------------------------------
#' @description Load Penn Source into toxval_source
#' @param db The version of toxval_source into which the source is loaded.
#' @param infile The input file ./penn/penn_files/Penn DEP Table 5a.xlsx
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
#'  \code{\link[stringr]{str_trim}}
#'  \code{\link[utils]{type.convert}}
#' @rdname import_penn_source
#' @export 
#' @importFrom openxlsx read.xlsx
#' @importFrom stringr str_trim
#' @importFrom utils type.convert
#--------------------------------------------------------------------------------------
import_penn_source <- function(db,
                               infile="Penn DEP Table 5a.xlsx",
                               chem.check.halt=T) {
  printCurrentFunction(db)

  infile = paste0(toxval.config()$datapath,"penn/penn_files/",infile)
  #####################################################################
  cat("Build original_penn_table \n")
  #####################################################################
  res <- openxlsx::read.xlsx(infile)
  res[3,9] <- gsub("\\(.*?\\)","(ug/m3)", res[3,9])

  res_header <- res[c(3,4,5,6),]
  res_header <- unlist(res_header)
  res_header <- unname(res_header[!is.na(res_header)])
  res_header <- gsub("\\s+", " ", stringr::str_trim(res_header))
  h1 <- paste(res_header[c(11:14)], collapse = " ")
  h2 <- paste(res_header[c(15:18)], collapse = " ")
  h3 <- paste(res_header[c(19:20)], collapse = " ")
  h4 <- paste(res_header[c(21:23)], collapse = " ")
  h5 <- paste(res_header[c(24:26)], collapse = " ")
  k1 <- "RFDo_key"
  k2 <- "CSFo_key"
  k3 <- "RfCi_key"
  k4 <- "IUR_key"
  header_val_1 <- c(k1,k2,k3,k4,h3,h4,h5)
  header_val_2 <- c(h1,h2)
  no_header_res <- which(is.na(res[3,]))
  new_header <- as.character(res[3,])
  new_header[1:length(new_header) %in% no_header_res] <- header_val_1
  header_in_row4 <- which(!is.na(res[4,]))
  header_4 <- header_in_row4[!header_in_row4 %in% no_header_res]
  new_header[1:length(new_header) %in% header_4] <- header_val_2
  new_header <- gsub("\\s+", " ", stringr::str_trim(new_header))
  names(res) <- new_header
  rm(h1,h2,h3,h4,h5,k1,k2,k3,k4)
  #####################################################################
  cat("Build penn_key_descriptions \n")
  #####################################################################
  key_desc_info <- res[372:376, c(1,2,7)]
  key_desc_info <- data.frame(unlist(key_desc_info))
  key_desc_info <- as.character(key_desc_info[,1])
  key_desc_info <- key_desc_info[!is.na(key_desc_info)]
  key_desc_info <- unlist( strsplit( key_desc_info , "=" ) )
  key_description <- key_desc_info[seq(2,length(key_desc_info),2)]
  key_description <- gsub("^\\s+|\\s+$","", key_description)
  key_value <- key_desc_info[seq(1,length(key_desc_info),2)]
  key_value <- gsub("^\\s+|\\s+$","", key_value)
  key_value = key_value[2:length(key_value)]
  # penn_key <- data.frame(key_value,key_description, stringsAsFactors = F)
  # runInsertTable(penn_key,"penn_key_descriptions",db,do.halt=T,verbose=F)

  #####################################################################
  cat("Build whole_penn_table and create dataframe res1 \n")
  #####################################################################
  res1 <- res[7:370,]
  names(res1) <- names(res)
  res1["penn_id"] <- c(1:dim(res1)[1])
  res1 <- res1[c("penn_id",names(res1[-20]))]
  colnames(res1) <- c("penn_id","name","casrn","RfDo (mg/kg-d)",
                      "RFDo_key","CSFo (mg/kg-d)-1","CSFo_key",
                      "RfCi (mg/m3)","RfCi_key","IUR (ug/m3)-1",
                      "IUR_key","Koc","VOC","Aqueous Solubility (mg/L)",
                      "Aqueous Solubility Reference1","TF Vol from Surface Soil",
                      "TF Vol from SubSurface Soil","Organic Liquid",
                      "Boiling Point (degrees C)","Degradation Coefficient (K)(yr-1)")
  toxval_types_units <- grep("\\(.*", names(res1), value = T)
  toxval_types <- gsub("\\(.*?\\)|\\-\\d+$", "", toxval_types_units)
  toxval_types <- gsub("\\s+$","", toxval_types)
  toxval_units <- gsub(".*\\s+\\(", "", toxval_types_units)
  toxval_units <- gsub("\\)|\\(", "", toxval_units)
  voc_X <- grep('X', res1$VOC)
  org_liq_X <- grep('X', res1$`Organic Liquid`)
  res1$VOC[1:nrow(res1) %in% voc_X] <- rep('Y', length(voc_X))
  res1$`Organic Liquid`[1:nrow(res1) %in% org_liq_X] <- rep('Y', length(org_liq_X))
  replace_keys <- grep("key", names(res1))
  for (u in replace_keys){
    for (v in 1:dim(res1)[1]){
      for(k in 1:length(key_description)){
        res1[v,u] <- gsub(paste("\\b",key_value[k],"\\b", sep = ""), key_description[k], res1[v,u])
      }
    }
  }
  res1 <- lapply(res1, function(x) utils::type.convert(as.character(x), as.is = T))
  res1 <- data.frame(res1, stringsAsFactors = F)
  colnames(res1) <- c("penn_id","name","casrn","RfDo (mg/kg-d)",
                      "RFDo_key","CSFo (mg/kg-d)-1","CSFo_key",
                      "RfCi (mg/m3)","RfCi_key","IUR (ug/m3)-1",
                      "IUR_key","Koc","VOC","Aqueous Solubility (mg/L)",
                      "Aqueous Solubility Reference1","TF Vol from Surface Soil",
                      "TF Vol from SubSurface Soil","Organic Liquid",
                      "Boiling Point (degrees C)","Degradation Coefficient (K)(yr-1)")
  res1 = res1[,1:20]

  #####################################################################
  cat("Build new_penn_table from res1\n")
  #####################################################################
  t1 <- res1[,c(2,4,5,3)]
  colnames(t1)[2] <- c("toxval_numeric")
  colnames(t1)[3] <- c("toxval_source")
  t1["toxval_type"] <- c(rep(toxval_types[1], nrow(t1)))
  t1["toxval_units"] <- c(rep(toxval_units[1], nrow(t1)))
  t2 <- res1[,c(2,6,7,3)]
  colnames(t2)[2] <- c("toxval_numeric")
  colnames(t2)[3] <- c("toxval_source")
  t2["toxval_type"] <- c(rep(toxval_types[2], nrow(t2)))
  t2["toxval_units"] <- c(rep(toxval_units[2], nrow(t2)))
  t3 <- res1[,c(2,8,9,3)]
  colnames(t3)[2] <- c("toxval_numeric")
  colnames(t3)[3] <- c("toxval_source")
  t3["toxval_type"] <- c(rep(toxval_types[3], nrow(t3)))
  t3["toxval_units"] <- c(rep(toxval_units[3], nrow(t3)))
  t4 <- res1[,c(2,10,11,3)]
  colnames(t4)[2] <- c("toxval_numeric")
  colnames(t4)[3] <- c("toxval_source")
  t4["toxval_type"] <- c(rep(toxval_types[4], nrow(t4)))
  t4["toxval_units"] <- c(rep(toxval_units[4], nrow(t4)))
  penn_types <- rbind(t1,t2,t3,t4)
  penn_types <- subset(penn_types,penn_types[,2]!="")
  penn_types <- subset(penn_types,penn_types[,2]!="0")
  penn_types["penn_id"] <- c(1:dim(penn_types)[1])
  penn_types <- penn_types[c("penn_id",names(penn_types[-7]))]

  nlist = c("casrn","name","toxval_numeric","toxval_source","toxval_type","toxval_units")
  res = penn_types[,nlist]
  #####################################################################
  cat("Prep and load the data\n")
  #####################################################################
  source_prep_and_load(db,source="Pennsylvania DEP ToxValues",table="source_penn",res=res,F,T,T)
}
