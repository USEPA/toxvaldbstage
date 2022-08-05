#--------------------------------------------------------------------------------------
#' Process the ECHA eChemPortal data from 2020 - step 2
#'
#' @param do.load If TRUE, laod all of the in vivo data
#' @export
#--------------------------------------------------------------------------------------
echemportal.prep.v2.step2 <- function(do.load=F) {
  printCurrentFunction()
  if(do.load) {
    file <- "../echa/echa_files/eChemPortal mammalian data 2020 step 1.xlsx"
    MAT1 <<- read.xlsx(file)
  }
  mat <- MAT1
  tcount <- sum(mat$toxval_count)
  name.list <- names(mat)
  row <- as.data.frame(matrix(nrow=1,ncol=length(name.list)))
  names(row) <- name.list
  res <- as.data.frame(matrix(nrow=tcount,ncol=length(name.list)))
  names(res) <- name.list
  irow <- 0
  for(i in 1:nrow(mat)) {
    temp <- mat[i,]
    count <- temp[1,"toxval_count"]
    tt0 <- temp[1,"toxval_type"]
    tv0 <- temp[1,"toxval_numeric"]
    tu0 <- temp[1,"toxval_units"]
    tq0 <- temp[1,"toxval_numeric_qualifier"]

    tt0 <- substr(tt0,2,nchar(tt0))
    tv0 <- substr(tv0,2,nchar(tv0))
    tu0 <- substr(tu0,2,nchar(tu0))
    tq0 <- substr(tq0,2,nchar(tq0))

    tt <- str_split(tt0,"\\|")[[1]]
    tv <- str_split(tv0,"\\|")[[1]]
    tu <- str_split(tu0,"\\|")[[1]]
    tq <- str_split(tq0,"\\|")[[1]]

    for(j in 1:count) {
      row <- temp
      row[1,"toxval_numeric"] <- tv[j]
      row[1,"toxval_numeric_qualifier"] <- tq[j]
      row[1,"toxval_units"] <- tu[j]
      row[1,"toxval_type"] <- tt[j]
      irow <- irow+1
      res[irow,] <- row
    }
    if(i%%1000==0) cat("finished ",i," out of ",nrow(mat),"\n")
  }
  file <- "../echa/echa_files/eChemPortal mammalian data 2020 step 2.xlsx"
  write.xlsx(res,file)
}
