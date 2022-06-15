#--------------------------------------------------------------------------------------
#' Get a listing of all of the documents in clowder and link back to information in
#' dev_toxval_v8
#' @param db The version of toxval into which the source is loaded.
#' @param dir The directory where the files live
#'

#--------------------------------------------------------------------------------------
clowder_document_list <- function(db="dev_toxval_v8",
                                indir="../clowder_v3/") {
  printCurrentFunction(db)

  cat("load the databases\n")
  if(!exists("TEMP8")) TEMP8 <<- unique(runQuery("select c.casrn,b.dtxsid,a.toxval_id,a.record_source_hash,a.source,a.long_ref
                                                 from record_source a, toxval b, source_chemical c
                                                 where a.toxval_id=b.toxval_id
                                                 and b.chemical_id=c.chemical_id","dev_toxval_v8"))
  if(!exists("TEMP9")) TEMP9 <<- unique(runQuery("select c.casrn,b.dtxsid,a.toxval_id,a.record_source_hash,a.source,a.long_ref
                                                 from record_source a, toxval b, source_chemical c
                                                 where a.toxval_id=b.toxval_id
                                                 and b.chemical_id=c.chemical_id","dev_toxval_v9"))
  if(!exists("DSSTOX")) {
    dsstox.db <- toxval.config()$dsstox.db
    DSSTOX <<- runQuery("select * from generic_substances",dsstox.db)
  }

  cat("make a list of the files from clowder\n")
  dir0 = paste0(indir,"clowder_files")
  dlist = list.dirs(path=dir0)
  dlist.all = dlist
  row = as.data.frame(matrix(nrow=1,ncol=2))
  allfiles = NULL
  names(row) = c("dir","document_name")
  flist.all = NULL
  for(dir in dlist) {
    dlist2 = list.dirs(path=dir)
    if(length(dlist2)>0) dlist.all = c(dlist.all,dlist2)
  }
  dlist.all = unique(dlist.all)
  for(dir in dlist.all) {
    flist = list.files(path=dir,pattern=".pdf")
    if(length(flist)>0) {
      cat(dir,"\n")
      stub = str_split(dir,"\\/")[[1]]
      stub = stub[length(stub)]
      for(i in 1:length(flist)) {
        row[1,"dir"] = stub
        row[1,"document_name"] = flist[i]
        allfiles = rbind(allfiles,row)
      }
    }
  }
  allfiles$duplicated = 0
  x = allfiles$document_name
  dups = x[duplicated(x)]
  allfiles[is.element(allfiles$document_name,dups),"duplicated"] = 1

  flist = list.files(dir)
  nlist = c("document_name","record_source_hash","source","dtxsid","casrn","name","long_ref","db","toxval_id","clowder_id")
  mat = allfiles
  mat$record_source_hash = NA
  mat$source = NA
  mat$dtxsid = NA
  mat$casrn = NA
  mat$name = NA
  mat$long_ref = NA
  mat$db = NA
  mat$toxval_id = NA
  mat$clowder_id = NA

  #mat = as.data.frame(matrix(nrow=length(flist),ncol=length(nlist)))
  #names(mat) = nlist
  #mat$document_name = flist
  for(i in 1:nrow(mat)) {
    dname = mat[i,"document_name"]
    dname = str_replace(dname,".pdf","")
    temp = str_split(dname,"-")[[1]]
    temp = temp[1]
    temp = str_split(temp,"_")[[1]]
    temp = temp[1]
    if(nchar(temp)>15) mat[i,"record_source_hash"] = temp
  }

  cat("add the clowder_ids\n")
  file = paste0(indir,"clowder_doc_maps_20220608.xlsx")
  map = openxlsx::read.xlsx(file)
  rownames(map) = map$filename
  missing = map[!is.element(map$filename,mat$document_name),"filename"]
  nlist = names(mat)
  mat2 = as.data.frame(matrix(nrow=length(missing),ncol=length(nlist)))
  names(mat2) = nlist
  mat2$document_name = missing
  mat = rbind(mat,mat2)
  nmat = nrow(mat)
 # nmat = 1000
  for(i in 1:nmat) {
    dname = mat[i,"document_name"]
    if(is.na(mat[i,"record_source_hash"])) {
      dname2 = str_replace(dname,".pdf","")
      temp = str_split(dname2,"-")[[1]]
      temp = temp[1]
      temp = str_split(temp,"_")[[1]]
      temp = temp[1]
      if(nchar(temp)>15) mat[i,"record_source_hash"] = temp
    }

    if(is.element(dname,map$filename)) mat[i,"clowder_id"] = map[dname,"id"]
    if(i%%100==0) cat("{b} finished",i,"out of ",nmat,"\n")
  }

  temp8 = TEMP8[is.element(TEMP8$record_source_hash,mat$record_source_hash),]
  temp9 = TEMP9[is.element(TEMP9$record_source_hash,mat$record_source_hash),]

  cat("map to toxval\n")
  clist = unique(c(TEMP8$casrn,TEMP9$casrn))
  dsstox = DSSTOX[is.element(DSSTOX$casrn,clist),]
  for(i in 1:nmat) {
    rsh = mat[i,"record_source_hash"]
    if(!is.na(rsh)) {
      if(is.element(rsh,temp8$record_source_hash)) {
        mat[i,"db"] = "dev_toxval_v8"
        x = temp8[is.element(temp8$record_source_hash,rsh),]
        mat[i,"toxval_id"] = paste(sort(unique(x[,"toxval_id"])),collapse="|")
        mat[i,"long_ref"] = paste(sort(unique(x[,"long_ref"])),collapse="|")
        mat[i,"source"] = paste(sort(unique(x[,"source"])),collapse="|")
        mat[i,"casrn"] = paste(sort(unique(x[,"casrn"])),collapse="|")
        casrn = mat[i,"casrn"]
        if(is.element(casrn,dsstox$casrn)) {
          mat[i,"name"] = dsstox[is.element(dsstox$casrn,casrn),"preferred_name"][1]
          mat[i,"dtxsid"] = dsstox[is.element(dsstox$casrn,casrn),"dsstox_substance_id"][1]
        }
      }
      else {
        if(is.element(rsh,temp9$record_source_hash)) {
          mat[i,"db"] = "dev_toxval_v9"
          x = temp9[is.element(temp9$record_source_hash,rsh),]
          mat[i,"toxval_id"] = paste(sort(unique(x[,"toxval_id"])),collapse="|")
          mat[i,"long_ref"] = paste(sort(unique(x[,"long_ref"])),collapse="|")
          mat[i,"source"] = paste(sort(unique(x[,"source"])),collapse="|")
          mat[i,"casrn"] = paste(sort(unique(x[,"casrn"])),collapse="|")
          casrn = mat[i,"casrn"]
          if(is.element(casrn,dsstox$casrn)) {
            mat[i,"name"] = dsstox[is.element(dsstox$casrn,casrn),"preferred_name"][1]
            mat[i,"dtxsid"] = dsstox[is.element(dsstox$casrn,casrn),"dsstox_substance_id"][1]
          }
        }
        else {
          cat("missing rsh",i,"\n")
        }
      }
    }
    if(i%%100==0) cat("{b} finished",i,"out of ",nmat,"\n")
  }

  x = substr(mat$document_name,1,6)
  mat2 = mat[x!="ToxVal",]
  file = paste0(indir,"toxval_document_map_icf.xlsx")
  openxlsx::write.xlsx(mat2,file)

  cat("deal with the CCTE documents\n")
  file = paste0(indir,"clowder_doc_maps_20220608.xlsx")
  map = openxlsx::read.xlsx(file,sheet=2)
  names(map) = c("clowder_id","document_name","url","doi","hero_id","source",
                 "subsource","author","year","study_name","dtxsid","md5",
                 "sha1","sha224","sha256","sha384","sha512","source_name",
                 "subsource_name")
  map$casrn = NA
  map$name = NA
  dlist = unique(map$dtxsid)
  dlsit = dlist[!is.na(dlist)]
  for (dtxsid in dlist) {
    if(is.element(dtxsid,DSSTOX$dsstox_substance_id)) {
      casrn = DSSTOX[is.element(DSSTOX$dsstox_substance_id,dtxsid),"casrn"]
      name = DSSTOX[is.element(DSSTOX$dsstox_substance_id,dtxsid),"preferred_name"]
      #cat(dtxsid,casrn,name,"\n")
      map[is.element(map$dtxsid,dtxsid),"casrn"] = casrn
      map[is.element(map$dtxsid,dtxsid),"name"] = name
    }
  }
  file = paste0(indir,"toxval_document_map_ccte.xlsx")
  openxlsx::write.xlsx(map,file)

}

