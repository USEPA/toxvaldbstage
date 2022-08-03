#--------------------------------------------------------------------------------------
#' Organize the clowder_id and document_name information
#' @param db The version of toxval into which the source is loaded.
#--------------------------------------------------------------------------------------
clowder_id_prep.v2 <- function(db="dev_toxval_v9") {
  printCurrentFunction(db)

  indir = paste0(toxval.config()$datapath,"clowder_v2/")

  file = paste0(indir,"toxval_v8_record_source_hash_to_clowder_id.xlsx")
  file = paste0(indir,"record_source_hash clowder_id file_name.xlsx")

  file = paste0(indir,"new_qa_set_with_ClowderID.xlsx")
  cid = openxlsx::read.xlsx(file)
  cid = cid[!is.na(cid$clowder_id),]
  cid2 = unique(cid[,c("record_source_hash","clowder_id")])
  rownames(cid2) = cid2$record_source_hash
  dir = paste0(indir,"icf_files")
  flist = list.files(dir)
  nlist = c("document_name","record_source_hash","db","toxval_id","clowder_id")
  mat = as.data.frame(matrix(nrow=length(flist),ncol=length(nlist)))
  names(mat) = nlist
  mat$document_name = flist
  for(i in 1:length(flist)) {
    dname = flist[i]
    dname = str_replace(dname,".pdf","")
    temp = str_split(dname,"-")[[1]]
    temp = temp[1]
    temp = str_split(temp,"_")[[1]]
    temp = temp[1]
    mat[i,"record_source_hash"] = temp
  }

  temp8 = unique(runQuery("select toxval_id,record_source_hash,long_ref from record_source","dev_toxval_v8"))
  temp9 = unique(runQuery("select toxval_id,record_source_hash,long_ref from record_source","dev_toxval_v9"))

  mat = mat[1:500,]
  for(i in 1:nrow(mat)) {
    rsh = mat[i,"record_source_hash"]
    if(is.element(rsh,temp8$record_source_hash)) {
      mat[i,"db"] = "dev_toxval_v8"
      x = temp8[is.element(temp8$record_source_hash,rsh),]
      mat[i,"toxval_id"] = x[1,"toxval_id"]
      mat[i,"long_ref"] = x[1,"long_ref"]
    }
    else {
      if(is.element(rsh,temp9$record_source_hash)) {
        mat[i,"db"] = "dev_toxval_v9"
        x = temp9[is.element(temp9$record_source_hash,rsh),]
        mat[i,"toxval_id"] = temp9[1,"toxval_id"]
        mat[i,"long_ref"] = temp9[1,"long_ref"]
      }
      else {
        cat("missing rsh",i,"\n")
      }
    }
  }

  for(i in 1:nrow(mat)) {
    rsh = mat[i,"record_source_hash"]
    if(is.element(rsh,cid2$record_source_hash)) {
      mat[i,"clowder_id"] = cid2[rsh,"clowder_id"]
    }
    else {
      cat("missing link:",i,"\n")
    }
  }
}

