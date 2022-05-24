#-------------------------------------------------------------------------------------
#' Get chemical ids for many given CASRN/Chemical name pairs
#'
#' @param toxval.db The version of toxval that the chemical id is pulled from.
#' @param chemical.list A 2-column dataframe of CAS Registry Numbers and chemical names.
#' @param source The source of the chemical data
#' @param verbose If TRUE, print out extra diagnostic messages
#' @return A 3-column dataframe of CAS Registry Numbers, chemical names, and associated chemical IDs.
#' @export
#-------------------------------------------------------------------------------------
get.cid.list.toxval <- function(toxval.db,chemical.list,source,verbose=F) {
  #chemical.list should be a 2-column data frame: CASRN and cname
  #chemical.list <- unique(chemical.list)
  names(chemical.list) <- c("casrn","name")

  name.list <- chemical.list[,"name"]
  name.list.utf8 <- enc2utf8(name.list)
  delta.1 <- name.list[name.list!=name.list.utf8]
  delta.2 <- name.list.utf8[name.list.utf8!=name.list]
  if(length(delta.1)>0) {
    cat("=======================================================\nnon-UTF characters\n")
    print(delta.1)
    cat("=======================================================\ntranslated values\n")
    print(delta.2)
  }
  chemical.list[,"name"] <- name.list.utf8
  for(i in 1:nrow(chemical.list)) chemical.list[i,"casrn"] <- fix.casrn(chemical.list[i,"casrn"])

  #Deal with empty casrns/names
  chemical.list[is.na(chemical.list[,1]),1] <- ""
  chemical.list[is.null(chemical.list[,1]),1] <- ""
  chemical.list[chemical.list[,1] == " ",1] <- ""
  chemical.list[nchar(chemical.list[,1])<2,1] <- paste("NOCAS_",chemical.list[nchar(chemical.list[,1])<2,2],sep="")

  chemical.list[is.na(chemical.list[,2]),2] <- ""
  chemical.list[is.null(chemical.list[,2]),2]<- ""
  chemical.list[chemical.list[,2] == " ",2] <- ""

  #Deal with long casrns/names

  chemical.list[nchar(chemical.list[,1])>45,1] <- substr(chemical.list[nchar(chemical.list[,1])>45,1],1,45)
  chemical.list[nchar(chemical.list[,2])>255,2] <- substr(chemical.list[nchar(chemical.list[,2])>255,2],1,255)
  #names(chemical.list) <- c("casrn","name")

  chemical.list.source <- chemical.list
  chemical.list.source$source <- source
  chemical.list.source[,2] <- iconv(chemical.list.source[,2],from="latin1",to="UTF-8")
  chemical.list.source[,2] <- iconv(chemical.list.source[,2],from="LATIN1",to="UTF-8")
  chemical.list.source[,2] <- iconv(chemical.list.source[,2],from="LATIN2",to="UTF-8")
  chemical.list.source[,2] <- iconv(chemical.list.source[,2],from="latin-9",to="UTF-8")
  runQuery(paste0("delete from source_chemical where source='",source,"'"),toxval.db)
  names(chemical.list.source) <- c("source_casrn","source_name","source")
  runInsertTable(chemical.list.source,"source_chemical",toxval.db,verbose=verbose)
  chemical.list <- runQuery(paste0("select chemical_id,source_casrn,source_name from source_chemical where source='",source,"'"),toxval.db)
  names(chemical.list) <- c("chemical_id","casrn","name")
  return(chemical.list)
}
