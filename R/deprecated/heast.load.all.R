#-------------------------------------------------------------------------------------
#' Load HEAST to toxval_source. The data to be loaded is in the file
#' ./heast/heast_files/EPA_HEAST_Table1_ORNL for loading.xlsx
#' @param source.db The version of toxval_source into which the tables are loaded.
#--------------------------------------------------------------------------------------
heast.load.all <- function(source.db) {
  printCurrentFunction(source.db)
  #heast.load.initial(source.db)
  file <- "./heast/heast_files/EPA_HEAST_Table1_ORNL for loading.xlsx"
  cat(file,"\n")
  mat0 <- read.xlsx(file)
  mat <- NULL
  nrow <- dim(mat0)[1]
  last.row <- 0
  for(i in 1:nrow) {
    casrn <- mat0[i,"casrn"]
    if(!is.na(casrn)) {
      mat <- rbind(mat,mat0[i,])
      last.row <- i
    }
    else {
      mat[last.row,"target"] <- paste(mat[last.row,"target"],":",mat0[i,"target"])
      mat[last.row,"critical_effect"] <- paste(mat[last.row,"critical_effect"],":",mat0[i,"critical_effect"])
    }
  }

  nrow <- dim(mat)[1]
  last.row <- 0
  for(i in 1:nrow) {
    casrn <- mat[i,"casrn"]
    cname <- mat[i,"name"]

    if(!is.na(casrn)) {
      cid <- get.cid.toxval.source(source.db,casrn,cname,verbose=F)

      toxval_type <- mat[i,"toxval_type"]
      toxval_subtype <- mat[i,"toxval_subtype"]
      toxval_numeric <- mat[i,"toxval_numeric"]
      toxval_units <- mat[i,"toxval_units"]
      species <- mat[i,"species"]
      route <- mat[i,"exposure_route"]
      study_duration_value <- mat[i,"study_duration_value"]
      study_duration_units <- mat[i,"study_duration_units"]
      study_duration_class <- mat[i,"study_duration_class"]

      exposure_route <- "-"
      exposure_method <- "-"
      if(!is.na(route)) {
        x <- str_split(route,":")[[1]]
        exposure_route <- x[1]
        if(length(x)>1) exposure_method <- x[2]
        exposure_route <- str_trim(exposure_route)
        exposure_method <- str_trim(exposure_method)
      }

      target <- mat[i,"target"]
      critical_effect <- mat[i,"critical_effect"]
      comment <- mat[i,"comment"]
      ornl_table <- mat[i,"ornl_table"]
      # if(!is.na(toxval_numeric)) {
      #   query <- paste0("insert into heast (chemical_id,toxval_type,toxval_numeric,toxval_units,species,exposure_route,exposure_method,study_duration_value,study_duration_units,study_duration_class,target,critical_effect,comment,ornl_table)
      #                   values (",cid,",'",toxval_type,"',",toxval_numeric,",'",toxval_units,"','",species,"','",exposure_route,"','",exposure_method,"','",study_duration_value,"','",study_duration_units,"','",study_duration_class,"','",target,"','",critical_effect,"','",comment,"','",ornl_table,"')")
      #   runInsert(query,source.db,T,F,T) -> heast_id
      # }

      value <- mat[i,"rfc_subchronic"]
      units <- mat[i,"rfc_subchronic_units"]
      uf <- mat[i,"rfc_Subchronic_uf"]

      if(is.na(uf)) uf <- -999
      if(uf=="N/A") uf <- -999
      # if(!is.na(value)) {
      #   toxval_type <- "RfC"
      #   toxval_subtype <- "subchronic"
      #   query <- paste0("insert into heast_rfd_rfc (chemical_id,heast_id,toxval_type,toxval_subtype,toxval_numeric,toxval_units,toxval_uf)
      #                   values (",cid,",",heast_id,",'",toxval_type,"','",toxval_subtype,"',",value,",'",units,"',",uf,")")
      #   runInsert(query,source.db,T)
      # }

      value <- mat[i,"rfd_subchronic"]
      units <- mat[i,"rfd_subchronic_units"]
      uf <- mat[i,"rfd_subchronic_uf"]

      if(is.na(uf)) uf <- -999
      if(uf=="N/A") uf <- -999
      # if(!is.na(value)) {
      #   toxval_type <- "RfD"
      #   toxval_subtype <- "subchronic"
      #   query <- paste0("insert into heast_rfd_rfc (chemical_id,heast_id,toxval_type,toxval_subtype,toxval_numeric,toxval_units,toxval_uf)
      #                   values (",cid,",",heast_id,",'",toxval_type,"','",toxval_subtype,"',",value,",'",units,"',",uf,")")
      #   runInsert(query,source.db,T)
      # }

      value <- mat[i,"rfc_chronic"]
      units <- mat[i,"rfc_chronic_units"]
      uf <- mat[i,"rfc_chronic_uf"]

      if(is.na(uf)) uf <- -999
      if(uf=="N/A") uf <- -999
      # if(!is.na(value)) {
      #   toxval_type <- "RfC"
      #   toxval_subtype <- "chronic"
      #   query <- paste0("insert into heast_rfd_rfc (chemical_id,heast_id,toxval_type,toxval_subtype,toxval_numeric,toxval_units,toxval_uf)
      #                   values (",cid,",",heast_id,",'",toxval_type,"','",toxval_subtype,"',",value,",'",units,"',",uf,")")
      #   runInsert(query,source.db,T)
      # }

      value <- mat[i,"rfd_chronic"]
      units <- mat[i,"rfd_chronic_units"]
      uf <- mat[i,"rfd_chronic_uf"]
      print(View(mat))
      if(is.na(uf)) uf <- -999
      if(uf=="N/A") uf <- -999
      # if(!is.na(value)) {
      #   toxval_type <- "RfD"
      #   toxval_subtype <- "chronic"
      #   query <- paste0("insert into heast_rfd_rfc (chemical_id,heast_id,toxval_type,toxval_subtype,toxval_numeric,toxval_units,toxval_uf)
      #                   values (",cid,",",heast_id,",'",toxval_type,"','",toxval_subtype,"',",value,",'",units,"',",uf,")")
      #   runInsert(query,source.db,T)
      # }
    }
  }
}
