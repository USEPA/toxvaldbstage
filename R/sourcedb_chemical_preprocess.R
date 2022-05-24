# Rough script for pre processing files for chemical mapping

# read the files generated from sourcedb
files.list <- list.files(pattern = "*.xlsx")

res <- lapply(files.list,read.xlsx)

names(res) <- gsub("(.*)(\\.xlsx)","\\1",files.list)

# paste name of the file with id
for (i in 1:length(res)){
  res[[i]][,"chemical_id"] <-paste(names(res)[i],res[[i]][,"chemical_id"], sep = "_")
}

# for (i in 1:length(res)){
#   write.xlsx(res[[i]],files.list[i])
# }

# create new fields for synonym and acronym
res1 <- mapply(cbind, res, raw_name_synonym = "-", SIMPLIFY=F)
res1 <- mapply(cbind, res1, raw_name_acronym = "-" , SIMPLIFY=F)

# extract synonym and acronym from name
res1 <- lapply(res1, function(w)
  transform(w, raw_name_synonym = gsub("(.*)(\\s+\\(([^()]*)\\))","\\1", raw_name)))

res1 <- lapply(res1, function(w)
  transform(w, raw_name_synonym = gsub("\\s+$","", raw_name_synonym)))

res1 <- lapply(res1, function(df) {df["raw_name_acronym"] <- lapply(df["raw_name_acronym"], as.character); df})


for (i in 1:length(res1)){
  if (dim(res1[[i]][grep("\\s+\\(([^()]*)\\)$", res1[[i]]$raw_name),])[1] != 0) {
    res1[[i]][grep("\\s+\\(([^()]*)\\)$", res1[[i]]$raw_name),"raw_name_acronym"] <- gsub("(.*)(\\s+\\()(([^()]*))(\\))","\\3", res1[[i]][grep("\\s+\\(([^()]*)\\)$", res1[[i]]$raw_name),"raw_name"])
  }
}

# assign field for casrn validity status
res1 <- mapply(cbind, res1, casrn_validity_status = "-" , SIMPLIFY=F)
res1 <- lapply(res1, function(df) {df["casrn_validity_status"] <- lapply(df["casrn_validity_status"], as.character); df})


# for (i in 1:length(res1)){
#   write.xlsx(res1[[i]],files.list[i])
# }


##### for cases with casrn validity output
res2 <- res1
# pull the casrn validity output files for mapping
out.files.list <- list.files("./cas_output_files")
out.files.list <- mixedsort(sort(out.files.list)) 
out.files <- lapply(paste0("./cas_output_files/",out.files.list),read.xlsx)

names(out.files) <- gsub("(.*)(\\.xlsx)","\\1",out.files.list)

out_file_inval_form <- list()
out_file_check_sum <- list()

out.file.id <- gsub("(.*_)(\\d+)(\\.xlsx)","\\2",out.files.list)
res2_subset <- list()

for (i in which(gsub("(ToxVal000[0]*)(.*)","\\2",names(res2)) %in% out.file.id)){
  res2_subset[[i]] <- res2[[i]]
} 

res2_subset <- Filter(NROW, res2_subset)

for (i in 1:length(out.file.id)){
  out_file_inval_form[[i]] <- out.files[[i]][which(out.files[[i]]$X3 == "Format Invalid"),]
  res2_subset[[i]][which(res2_subset[[i]]$raw_casrn %in% out_file_inval_form[[i]]$CAS),"casrn_validity_status"] <- "Format Invalid"
  out_file_check_sum[[i]] <- out.files[[i]][which(out.files[[i]]$X3 == "Check Sum Failed"),]
  res2_subset[[i]][which(res2_subset[[i]]$raw_casrn %in% out_file_check_sum[[i]]$CAS),"casrn_validity_status"] <- "Check Sum Failed"
  if (all(out_file_check_sum[[i]]$CAS %in% out_file_inval_form[[i]]$CAS) == T){
    res2_subset[[i]][which(res2_subset[[i]]$raw_casrn %in% out_file_check_sum[[i]]$CAS),"casrn_validity_status"] <- "Format Invalid/Check Sum Failed"
  }
  
}



res2_subset <- mapply(cbind, res2_subset, new_casrn = "-" , SIMPLIFY=F)
res2_subset <- lapply(res2_subset, function(df) {df["new_casrn"] <- lapply(df["new_casrn"], as.character); df})

#1
res2_subset[[1]][grep("-", res2_subset[[1]]$casrn_validity_status),"new_casrn"] <- res2_subset[[1]][grep("-", res2_subset[[1]]$casrn_validity_status),"raw_casrn"]
for (i in 1:nrow(res2_subset[[1]])){
  res2_subset[[1]][i,"new_casrn"] <- fix.casrn(res2_subset[[1]][i,"new_casrn"])
}
colname <- c("chemical_id","original_name","original_casrn","raw_name","acronym","casrn_validity_status","raw_casrn")
colnames(res2_subset[[1]]) <- colname
write.xlsx(res2_subset[[1]],"ToxVal00001_full.xlsx")
res2_subset_new <- list()
res2_subset_new[[1]] <- res2_subset[[1]][,c("chemical_id","raw_name","raw_casrn")]
write.xlsx(res2_subset_new[[1]],"ToxVal00001_new.xlsx")




#2
res2_subset[[2]][grep("-", res2_subset[[2]]$casrn_validity_status),"new_casrn"] <- res2_subset[[2]][grep("-", res2_subset[[2]]$casrn_validity_status),"raw_casrn"]

for (i in 1:nrow(res2_subset[[2]])){
  res2_subset[[2]][i,"new_casrn"] <- fix.casrn(res2_subset[[2]][i,"new_casrn"])
}
colname <- c("chemical_id","original_name","original_casrn","raw_name","acronym","casrn_validity_status","raw_casrn")
colnames(res2_subset[[2]]) <- colname

res2_subset[[2]][grep(".*\\?",res2_subset[[2]]$original_name),"raw_name"] <- gsub("(.*\\-[a-zA-Z]+)([^[:alnum:]]+)(.*)","\\1\\3",res2_subset[[2]][grep(".*\\?",res2_subset[[2]]$original_name),"original_name"])

write.xlsx(res2_subset[[2]],"ToxVal00002_full.xlsx")
res2_subset_new[[2]] <- res2_subset[[2]][,c("chemical_id","raw_name","raw_casrn")]
write.xlsx(res2_subset_new[[2]],"ToxVal00002.xlsx")


#5
res2_subset[[3]][grep("-", res2_subset[[3]]$casrn_validity_status),"new_casrn"] <- res2_subset[[3]][grep("-", res2_subset[[3]]$casrn_validity_status),"raw_casrn"]

for (i in 1:nrow(res2_subset[[3]])){
  res2_subset[[3]][i,"new_casrn"] <- fix.casrn(res2_subset[[3]][i,"new_casrn"])
}

colname <- c("chemical_id","original_name","original_casrn","raw_name","acronym","casrn_validity_status","raw_casrn")
colnames(res2_subset[[3]]) <- colname

res2_subset[[3]][grep(".*\\?",res2_subset[[3]]$original_name),"raw_name"] <-  gsub("(.*\\-[a-zA-Z]+)([^[:alnum:]]+)","\\1",res2_subset[[3]][grep(".*\\?",res2_subset[[3]]$original_name),"original_name"])

write.xlsx(res2_subset[[3]],"ToxVal00005_full.xlsx")
res2_subset_new[[5]] <- res2_subset[[3]][,c("chemical_id","raw_name","raw_casrn")]
write.xlsx(res2_subset_new[[5]],"ToxVal00005.xlsx")

#6
res2_subset[[4]][,"new_casrn"] <- res2_subset[[4]][,"raw_casrn"]
res2_subset[[4]][which(is.na(res2_subset[[4]]$new_casrn)),"new_casrn"] <- "-"

for (i in 1:nrow(res2_subset[[4]])){
  res2_subset[[4]][i,"new_casrn"] <- fix.casrn(res2_subset[[4]][i,"new_casrn"])
}

colname <- c("chemical_id","original_name","original_casrn","raw_name","acronym","casrn_validity_status","raw_casrn")
colnames(res2_subset[[4]]) <- colname

res2_subset[[4]][grep(".*\\?",res2_subset[[4]]$original_name),"raw_name"] <-  gsub("(.*)(\\s+.*\\?\\s+.*\\?\\s+.*)","\\1",res2_subset[[4]][grep(".*\\?\\s+",res2_subset[[4]]$original_name),"original_name"])

res2_subset[[4]][grep("ISO",res2_subset[[4]]$original_name),"raw_name"] <- gsub("\\s+\\[.*\\]","",res2_subset[[4]][grep("ISO",res2_subset[[4]]$original_name),"original_name"])

write.xlsx(res2_subset[[4]],"ToxVal00006_full.xlsx")
res2_subset_new[[6]] <- res2_subset[[4]][,c("chemical_id","raw_name","raw_casrn")]
write.xlsx(res2_subset_new[[6]],"ToxVal00006.xlsx")

#7
res2_subset[[5]][grep("-", res2_subset[[5]]$casrn_validity_status),"new_casrn"] <- res2_subset[[5]][grep("-", res2_subset[[5]]$casrn_validity_status),"raw_casrn"]

for (i in 1:nrow(res2_subset[[5]])){
  res2_subset[[5]][i,"new_casrn"] <- fix.casrn(res2_subset[[5]][i,"new_casrn"])
}
colname <- c("chemical_id","original_name","original_casrn","raw_name","acronym","casrn_validity_status","raw_casrn")
colnames(res2_subset[[5]]) <- colname
write.xlsx(res2_subset[[5]],"ToxVal00007_full.xlsx")
res2_subset_new[[7]] <- res2_subset[[5]][,c("chemical_id","raw_name","raw_casrn")]
write.xlsx(res2_subset_new[[7]],"ToxVal00007.xlsx")


#8
res2_subset[[6]][grep("-", res2_subset[[6]]$casrn_validity_status),"new_casrn"] <- res2_subset[[6]][grep("-", res2_subset[[6]]$casrn_validity_status),"raw_casrn"]
res2_subset[[6]][which(res2_subset[[6]]$new_casrn == "N/A"),"new_casrn"] <- "-"


for (i in 1:nrow(res2_subset[[6]])){
  res2_subset[[6]][i,"new_casrn"] <- fix.casrn(res2_subset[[6]][i,"new_casrn"])
}

colname <- c("chemical_id","original_name","original_casrn","raw_name","acronym","casrn_validity_status","raw_casrn")
colnames(res2_subset[[6]]) <- colname
write.xlsx(res2_subset[[6]],"ToxVal00008_full.xlsx")
res2_subset_new[[8]] <- res2_subset[[6]][,c("chemical_id","raw_name","raw_casrn")]
write.xlsx(res2_subset_new[[8]],"ToxVal00008.xlsx")



#9
res2_subset[[7]][,"new_casrn"] <- res2_subset[[7]][,"raw_casrn"]
res2_subset[[7]][grep("^[a-zA-Z]+",res2_subset[[7]]$new_casrn),"new_casrn"] <- "-"
res2_subset[[7]][grep("\\(V|I\\)$",res2_subset[[7]]$raw_name),"raw_name_synonym"] <- res2_subset[[7]][grep("\\(V|I\\)$",res2_subset[[7]]$raw_name),"raw_name"]
res2_subset[[7]][grep("\\(V|I\\)$",res2_subset[[7]]$raw_name),"raw_name_acronym"] <- "-"


for (i in 1:nrow(res2_subset[[7]])){
  res2_subset[[7]][i,"new_casrn"] <- fix.casrn(res2_subset[[7]][i,"new_casrn"])
}

colname <- c("chemical_id","original_name","original_casrn","raw_name","acronym","casrn_validity_status","raw_casrn")
colnames(res2_subset[[7]]) <- colname

res2_subset[[7]][grep(".*\\?",res2_subset[[7]]$original_name),"raw_name"] <- gsub("(.*)(\\s+[^[:alnum:]]+)(.*)","\\1",res2_subset[[7]][grep(".*\\?",res2_subset[[7]]$original_name),"original_name"])

write.xlsx(res2_subset[[7]],"ToxVal00009_full.xlsx")
res2_subset_new[[9]] <- res2_subset[[7]][,c("chemical_id","raw_name","raw_casrn")]
write.xlsx(res2_subset_new[[9]],"ToxVal00009.xlsx")



#11
res2_subset[[8]][,"new_casrn"] <- res2_subset[[8]][,"raw_casrn"]
res2_subset[[8]][grep("\\-.*\\-.*\\-",res2_subset[[8]]$raw_casrn),"new_casrn"] <- "-"
res2_subset[[8]][grep("\\-.*\\-.*\\-",res2_subset[[8]]$raw_casrn),"casrn_validity_status"] <- "Format Invalid/Check Sum Failed"
res2_subset[[8]][grep("^[a-zA-Z]+",res2_subset[[8]]$raw_casrn),"new_casrn"] <- "-"

#"676-97-1 ", extra space causing invalid format
res2_subset[[8]][grep("\\s+$",res2_subset[[8]]$raw_casrn),"new_casrn"] <- gsub("(.*)(\\s+$)","\\1",res2_subset[[8]][grep("\\s+$",res2_subset[[8]]$raw_casrn),"new_casrn"])
# Ammonium hexachlorohydrate (III)
res2_subset[[8]][grep("\\w+\\s+\\(I+\\)$",res2_subset[[8]]$raw_name),"raw_name_synonym"] <- res2_subset[[8]][grep("\\w+\\s+\\(I+\\)$",res2_subset[[8]]$raw_name),"raw_name"]
res2_subset[[8]][grep("\\w+\\s+\\(I+\\)$",res2_subset[[8]]$raw_name),"raw_name_acronym"]<- "-"


for (i in 1:nrow(res2_subset[[8]])){
  res2_subset[[8]][i,"new_casrn"] <- fix.casrn(res2_subset[[8]][i,"new_casrn"])
}


colname <- c("chemical_id","original_name","original_casrn","raw_name","acronym","casrn_validity_status","raw_casrn")
colnames(res2_subset[[8]]) <- colname



write.xlsx(res2_subset[[8]],"ToxVal00011_full.xlsx")
res2_subset_new[[11]] <- res2_subset[[8]][,c("chemical_id","raw_name","raw_casrn")]
write.xlsx(res2_subset_new[[11]],"ToxVal00011.xlsx")



#14
res2_subset[[9]][grep("-", res2_subset[[9]]$casrn_validity_status),"new_casrn"] <- res2_subset[[9]][grep("-", res2_subset[[9]]$casrn_validity_status),"raw_casrn"]

for (i in 1:nrow(res2_subset[[9]])){
  res2_subset[[9]][i,"new_casrn"] <- fix.casrn(res2_subset[[9]][i,"new_casrn"])
}

colname <- c("chemical_id","original_name","original_casrn","raw_name","acronym","casrn_validity_status","raw_casrn")
colnames(res2_subset[[9]]) <- colname
write.xlsx(res2_subset[[9]],"ToxVal00014_full.xlsx")
res2_subset_new[[14]] <- res2_subset[[9]][,c("chemical_id","raw_name","raw_casrn")]
write.xlsx(res2_subset_new[[14]],"ToxVal00014.xlsx")


#15
res2_subset[[10]][,"new_casrn"] <- res2_subset[[10]][,"raw_casrn"]
res2_subset[[10]][grep("Check Sum Failed",res2_subset[[10]]$casrn_validity_status),"new_casrn"] <- "-"

for (i in 1:nrow(res2_subset[[10]])){
  res2_subset[[10]][i,"new_casrn"] <- fix.casrn(res2_subset[[10]][i,"new_casrn"])
}

colname <- c("chemical_id","original_name","original_casrn","raw_name","acronym","casrn_validity_status","raw_casrn")
colnames(res2_subset[[10]]) <- colname
write.xlsx(res2_subset[[10]],"ToxVal00015_full.xlsx")
res2_subset_new[[15]] <- res2_subset[[10]][,c("chemical_id","raw_name","raw_casrn")]
write.xlsx(res2_subset_new[[15]],"ToxVal00015.xlsx")

#18
res2_subset[[11]][,"new_casrn"] <- res2_subset[[11]][,"raw_casrn"]
res2_subset[[11]][grep("Check Sum Failed",res2_subset[[11]]$casrn_validity_status),"new_casrn"] <- "-"

res2_subset[[11]][grep("\\;$",res2_subset[[11]]$raw_casrn),"new_casrn"] <- gsub("(.*)(\\;$)","\\1",res2_subset[[11]][grep("\\;$",res2_subset[[11]]$raw_casrn),"raw_casrn"])

for (i in 1:nrow(res2_subset[[11]])){
  res2_subset[[11]][i,"new_casrn"] <- fix.casrn(res2_subset[[11]][i,"new_casrn"])
}

colname <- c("chemical_id","original_name","original_casrn","raw_name","acronym","casrn_validity_status","raw_casrn")
colnames(res2_subset[[11]]) <- colname

res2_subset[[11]][grep("Di-sec octyl phthalate\\s+\\(",res2_subset[[11]]$raw_name),"acronym"] <- gsub("(.*\\s+\\()(.*)(\\)$)","\\2",res2_subset[[11]][grep("Di-sec octyl phthalate\\s+\\(",res2_subset[[11]]$raw_name),"raw_name"])
res2_subset[[11]][grep("Di-sec octyl phthalate\\s+\\(",res2_subset[[11]]$raw_name),"raw_name"] <- gsub("(.*)(\\s+\\(.*)","\\1",res2_subset[[11]][grep("Di-sec octyl phthalate\\s+\\(",res2_subset[[11]]$raw_name),"raw_name"])


write.xlsx(res2_subset[[11]],"ToxVal00018_full.xlsx")
res2_subset_new[[18]] <- res2_subset[[11]][,c("chemical_id","raw_name","raw_casrn")]
write.xlsx(res2_subset_new[[18]],"ToxVal00018.xlsx")

#19

res2_subset[[12]][,"new_casrn"] <- res2_subset[[12]][,"raw_casrn"]
res2_subset[[12]][grep("Check Sum Failed",res2_subset[[12]]$casrn_validity_status),"new_casrn"] <- "-"

res2_subset[[12]][grep("[^\\-]\\-$",res2_subset[[12]]$acronym),"raw_name"] <- res2_subset[[12]][grep("[^\\-]\\-$",res2_subset[[12]]$acronym),"original_name"]
res2_subset[[12]][grep("[^\\-]\\-$",res2_subset[[12]]$acronym),"acronym"] <- "-"


for (i in 1:nrow(res2_subset[[12]])){
  res2_subset[[12]][i,"new_casrn"] <- fix.casrn(res2_subset[[12]][i,"new_casrn"])
}

colname <- c("chemical_id","original_name","original_casrn","raw_name","acronym","casrn_validity_status","raw_casrn")
colnames(res2_subset[[12]]) <- colname



write.xlsx(res2_subset[[12]],"ToxVal00019_full.xlsx")
res2_subset_new[[19]] <- res2_subset[[12]][,c("chemical_id","raw_name","raw_casrn")]
write.xlsx(res2_subset_new[[19]],"ToxVal00019.xlsx")


#23
#VARIOUS

res2_subset[[13]][,"new_casrn"] <- res2_subset[[13]][,"raw_casrn"]
res2_subset[[13]][grep("Check Sum Failed",res2_subset[[13]]$casrn_validity_status),"new_casrn"] <- "-"
res2_subset[[13]][grep("Format Invalid",res2_subset[[13]]$casrn_validity_status),"new_casrn"] <- "-"

res2_subset[[13]][grep("VARIOUS",res2_subset[[13]]$raw_casrn),"new_casrn"] <- "-"


for (i in 1:nrow(res2_subset[[13]])){
  res2_subset[[13]][i,"new_casrn"] <- fix.casrn(res2_subset[[13]][i,"new_casrn"])
}

colname <- c("chemical_id","original_name","original_casrn","raw_name","acronym","casrn_validity_status","raw_casrn")
colnames(res2_subset[[13]]) <- colname

write.xlsx(res2_subset[[13]],"ToxVal00023_full.xlsx")
res2_subset_new[[23]] <- res2_subset[[13]][,c("chemical_id","raw_name","raw_casrn")]
write.xlsx(res2_subset_new[[23]],"ToxVal00023.xlsx")



#24
res2_subset[[14]][,"new_casrn"] <- res2_subset[[14]][,"raw_casrn"]
res2_subset[[14]][grep("Check Sum Failed",res2_subset[[14]]$casrn_validity_status),"new_casrn"] <- "-"

for (i in 1:nrow(res2_subset[[14]])){
  res2_subset[[14]][i,"new_casrn"] <- fix.casrn(res2_subset[[14]][i,"new_casrn"])
}

colname <- c("chemical_id","original_name","original_casrn","raw_name","acronym","casrn_validity_status","raw_casrn")
colnames(res2_subset[[14]]) <- colname

write.xlsx(res2_subset[[14]],"ToxVal00024_full.xlsx")
res2_subset_new[[24]] <- res2_subset[[14]][,c("chemical_id","raw_name","raw_casrn")]
write.xlsx(res2_subset_new[[24]],"ToxVal00024.xlsx")


#25
res2_subset[[15]][,"new_casrn"] <- res2_subset[[15]][,"raw_casrn"]
res2_subset[[15]][grep("Check Sum Failed",res2_subset[[15]]$casrn_validity_status),"new_casrn"] <- "-"

for (i in 1:nrow(res2_subset[[15]])){
  res2_subset[[15]][i,"new_casrn"] <- fix.casrn(res2_subset[[15]][i,"new_casrn"])
}

colname <- c("chemical_id","original_name","original_casrn","raw_name","acronym","casrn_validity_status","raw_casrn")
colnames(res2_subset[[15]]) <- colname

write.xlsx(res2_subset[[15]],"ToxVal00025_full.xlsx")
res2_subset_new[[25]] <- res2_subset[[15]][,c("chemical_id","raw_name","raw_casrn")]
write.xlsx(res2_subset_new[[25]],"ToxVal00025.xlsx")


#26
res2_subset[[16]][,"new_casrn"] <- res2_subset[[16]][,"raw_casrn"]

for (i in 1:nrow(res2_subset[[16]])){
  res2_subset[[16]][i,"new_casrn"] <- fix.casrn(res2_subset[[16]][i,"new_casrn"])
}


colname <- c("chemical_id","original_name","original_casrn","raw_name","acronym","casrn_validity_status","raw_casrn")
colnames(res2_subset[[16]]) <- colname

res2_subset[[16]][grep(".*\\?\\s+$",res2_subset[[16]]$original_name),"raw_name"] <- gsub("(.*)(\\s+.*\\?\\s+.*\\?\\s+.*)","\\1",res2_subset[[16]][grep(".*\\?\\s+$",res2_subset[[16]]$original_name),"original_name"])


write.xlsx(res2_subset[[16]],"ToxVal00026_full.xlsx")
res2_subset_new[[26]] <- res2_subset[[16]][,c("chemical_id","raw_name","raw_casrn")]
write.xlsx(res2_subset_new[[26]],"ToxVal00026.xlsx")



# some source have no invalid casrn formats so no output files generated by tool

res2 <- res1


colname <- c("chemical_id","original_name","original_casrn","raw_name","acronym","casrn_validity_status")

res2 <- lapply(res2, setNames, colname)

res2 <- mapply(cbind, res2, raw_casrn = "-" , SIMPLIFY=F)
res2 <- lapply(res2, function(df) {df["raw_casrn"] <- lapply(df["raw_casrn"], as.character); df})

#3
res2[[3]]$raw_casrn <- res2[[3]]$original_casrn

for (i in 1:nrow(res2[[3]])){
  res2[[3]][i,"raw_casrn"] <- fix.casrn(res2[[3]][i,"raw_casrn"])
}



write.xlsx(res2[[3]],"ToxVal00003_full.xlsx")
res2_subset_new[[3]] <- res2[[3]][,c("chemical_id","raw_name","raw_casrn")]
write.xlsx(res2_subset_new[[3]],"ToxVal00003.xlsx")


#4
res2[[4]]$raw_casrn <- res2[[4]]$original_casrn

for (i in 1:nrow(res2[[4]])){
  res2[[4]][i,"raw_casrn"] <- fix.casrn(res2[[4]][i,"raw_casrn"])
}

write.xlsx(res2[[4]],"ToxVal00004_full.xlsx")
res2_subset_new[[4]] <- res2[[4]][,c("chemical_id","raw_name","raw_casrn")]
write.xlsx(res2_subset_new[[4]],"ToxVal00004.xlsx")

#10
res2[[10]]$raw_casrn <- res2[[10]]$original_casrn
res2[[10]][grep("^[a-zA-Z]+",res2[[10]]$raw_casrn),"raw_casrn"] <- "-"


for (i in 1:nrow(res2[[10]])){
  res2[[10]][i,"raw_casrn"] <- fix.casrn(res2[[10]][i,"raw_casrn"])
}

write.xlsx(res2[[10]],"ToxVal00010_full.xlsx")
res2_subset_new[[10]] <- res2[[10]][,c("chemical_id","raw_name","raw_casrn")]
write.xlsx(res2_subset_new[[10]],"ToxVal00010.xlsx")

#12
res2[[12]]$raw_casrn <- res2[[12]]$original_casrn

for (i in 1:nrow(res2[[12]])){
  res2[[12]][i,"raw_casrn"] <- fix.casrn(res2[[12]][i,"raw_casrn"])
}
write.xlsx(res2[[12]],"ToxVal00012_full.xlsx")
res2_subset_new[[12]] <- res2[[12]][,c("chemical_id","raw_name","raw_casrn")]
write.xlsx(res2_subset_new[[12]],"ToxVal00012.xlsx")


#13
res2[[13]]$raw_casrn <- res2[[13]]$original_casrn

for (i in 1:nrow(res2[[13]])){
  res2[[13]][i,"raw_casrn"] <- fix.casrn(res2[[13]][i,"raw_casrn"])
}

write.xlsx(res2[[13]],"ToxVal00013_full.xlsx")
res2_subset_new[[13]] <- res2[[13]][,c("chemical_id","raw_name","raw_casrn")]
write.xlsx(res2_subset_new[[13]],"ToxVal00013.xlsx")

#16

res2[[16]]$raw_casrn <- res2[[16]]$original_casrn
res2[[16]][grep("\\)\\,",res2[[16]]$original_name),"acronym"] <- gsub("(.*\\s+)(\\(.*\\)\\,.*)","\\2",res2[[16]][grep("\\)\\,",res2[[16]]$original_name),"original_name"])
res2[[16]][grep("\\)\\,",res2[[16]]$original_name),"raw_name"] <- gsub("(.*)(\\s+\\(.*\\)\\,.*)","\\1",res2[[16]][grep("\\)\\,",res2[[16]]$original_name),"original_name"])
for (i in 1:nrow(res2[[16]])){
  res2[[16]][i,"raw_casrn"] <- fix.casrn(res2[[16]][i,"raw_casrn"])
}
write.xlsx(res2[[16]],"ToxVal00016_full.xlsx")
res2_subset_new[[16]] <- res2[[16]][,c("chemical_id","raw_name","raw_casrn")]
write.xlsx(res2_subset_new[[16]],"ToxVal00016.xlsx")

#17

res2[[17]]$raw_casrn <- res2[[17]]$original_casrn

for (i in 1:nrow(res2[[17]])){
  res2[[17]][i,"raw_casrn"] <- fix.casrn(res2[[17]][i,"raw_casrn"])
}
write.xlsx(res2[[17]],"ToxVal00017_full.xlsx")
res2_subset_new[[17]] <- res2[[17]][,c("chemical_id","raw_name","raw_casrn")]
write.xlsx(res2_subset_new[[17]],"ToxVal00017.xlsx")


#20

res2[[20]]$raw_casrn <- res2[[20]]$original_casrn

for (i in 1:nrow(res2[[20]])){
  res2[[20]][i,"raw_casrn"] <- fix.casrn(res2[[20]][i,"raw_casrn"])
}

res2[[20]][grep("\\s+\\(", res2[[20]]$raw_name), "acronym"] <- paste(gsub("(.*[^ ]\\s+\\()(.*)(\\)$)","\\2",res2[[20]][grep("\\s+\\(", res2[[20]]$raw_name), "raw_name"]),"(",res2[[20]][grep("\\s+\\(", res2[[20]]$raw_name), "acronym"],")", sep = "")
res2[[20]][grep("\\s+\\(", res2[[20]]$raw_name), "raw_name"] <- gsub("(.*[^ ])(\\s+\\(.*)","\\1",res2[[20]][grep("\\s+\\(", res2[[20]]$raw_name), "raw_name"])

write.xlsx(res2[[20]],"ToxVal00020_full.xlsx")
res2_subset_new[[20]] <- res2[[20]][,c("chemical_id","raw_name","raw_casrn")]
write.xlsx(res2_subset_new[[20]],"ToxVal00020.xlsx")


#21

res2[[21]]$raw_casrn <- res2[[21]]$original_casrn

for (i in 1:nrow(res2[[21]])){
  res2[[21]][i,"raw_casrn"] <- fix.casrn(res2[[21]][i,"raw_casrn"])
}
write.xlsx(res2[[21]],"ToxVal00021_full.xlsx")
res2_subset_new[[21]] <- res2[[21]][,c("chemical_id","raw_name","raw_casrn")]
write.xlsx(res2_subset_new[[21]],"ToxVal00021.xlsx")

#22

res2[[22]]$raw_casrn <- res2[[22]]$original_casrn

for (i in 1:nrow(res2[[22]])){
  res2[[22]][i,"raw_casrn"] <- fix.casrn(res2[[22]][i,"raw_casrn"])
}
write.xlsx(res2[[22]],"ToxVal00022_full.xlsx")
res2_subset_new[[22]] <- res2[[22]][,c("chemical_id","raw_name","raw_casrn")]
write.xlsx(res2_subset_new[[22]],"ToxVal00022.xlsx")