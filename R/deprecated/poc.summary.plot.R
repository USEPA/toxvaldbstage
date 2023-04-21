#--------------------------------------------------------------------------------------
#' Make a scatter plot of the relationship between info availability and Scientific Domain Metric
#' @param to.file If TRUE, print the graph to a file, else write to the screen
#' @param sys.date The date of the bin data Excel fileto read
#' @param do.read if TRUE, read in the source file and store in a global
#--------------------------------------------------------------------------------------
poc.summary.plot <- function(to.file=F,sys.date,do.read=F) {
  printCurrentFunction()

  if(do.read) {
      file <-   file <- paste0(toxval.config()$datapath,"tsca_binning/tsca_bin_output_step_2_",sys.date,".xlsx")
      MAT.ALL <<- read.xlsx(file)
  }
  do.circles <- F

  if(to.file) {
    fname <- paste("../tsca_binning/POC Summary ",sys.date,".pdf",sep="")
     pdf(file=fname,width=6,height=10,pointsize=12,bg="white",paper="letter",pagecentre=T)
  }
  par(mfrow=c(2,1),mar=c(4,8,2,2))

  name.list <- c("list_name","risk","risk.min","risk.max","info","info.min","info.max")
  row <- as.data.frame(matrix(nrow=1,ncol=length(name.list)))
  names(row) <- name.list
  mat <- NULL
  plot(c(0,0),type="n",xlim=c(0,100),ylim=c(0,100),cex.lab=1.2,cex.axis=1.2,xlab="Information Availability Metric",ylab="Scientific Domain Metric")

  x0 <- 0
  y0 <- 100
  delta <- 5

  lname <- "TSCA High"
  temp <- MAT.ALL[!is.na(MAT.ALL[,"TSCA.High"]),]
  x <- temp[,"information_availability_with_modifier"]
  qx <- quantile(x,probs=seq(0,1,0.05))
  y <- temp[,"scaled_score_overall_s3med"]
  qy <- quantile(y,probs=seq(0,1,0.05))
  lines(c(qx[11],qx[11]),c(qy[2],qy[20]))
  lines(c(qx[2],qx[20]),c(qy[11],qy[11]))
  points(qx[11],qy[11],pch=25,cex=1.5,bg="red")
  points(x0,y0,pch=25,cex=1.5,bg="red")
  text(x0,y0,lname,pos=4,cex=0.8)
  y0 <- y0-delta

  lname <- "TSCA 90"
  temp <- MAT.ALL[!is.na(MAT.ALL[,"TSCA_90"]),]
  x <- temp[,"information_availability_with_modifier"]
  qx <- quantile(x,probs=seq(0,1,0.05))
  y <- temp[,"scaled_score_overall_s3med"]
  qy <- quantile(y,probs=seq(0,1,0.05))
  lines(c(qx[11],qx[11]),c(qy[2],qy[20]))
  lines(c(qx[2],qx[20]),c(qy[11],qy[11]))
  points(qx[11],qy[11],pch=21,cex=1.5,bg="cyan")
  points(x0,y0,pch=21,cex=1.5,bg="cyan")
  text(x0,y0,lname,pos=4,cex=0.8)
  y0 <- y0-delta

  lname <- "TSCA POC"
  temp <- MAT.ALL[!is.na(MAT.ALL[,"TSCA_POC"]),]
  x <- temp[,"information_availability_with_modifier"]
  x <- x+1
  qx <- quantile(x,probs=seq(0,1,0.05))
  y <- temp[,"scaled_score_overall_s3med"]
  qy <- quantile(y,probs=seq(0,1,0.05))
  lines(c(qx[11],qx[11]),c(qy[2],qy[20]))
  lines(c(qx[2],qx[20]),c(qy[11],qy[11]))
  points(qx[11],qy[11],pch=21,cex=1.5,bg="black")
  points(x0,y0,pch=21,cex=1.5,bg="black")
  text(x0,y0,lname,pos=4,cex=0.8)
  y0 <- y0-delta

  lname <- "TSCA Low"
  temp <- MAT.ALL[!is.na(MAT.ALL[,"TSCA.Low"]),]
  x <- temp[,"information_availability_with_modifier"]
  qx <- quantile(x,probs=seq(0,1,0.05))
  y <- temp[,"scaled_score_overall_s3med"]
  y <- y+1
  qy <- quantile(y,probs=seq(0,1,0.05))
  lines(c(qx[11],qx[11]),c(qy[2],qy[20]))
  lines(c(qx[2],qx[20]),c(qy[11],qy[11]))
  points(qx[11],qy[11],pch=24,cex=1.5,bg="yellow")
  points(x0,y0,pch=24,cex=1.5,bg="yellow")
  text(x0,y0,lname,pos=4,cex=0.8)
  y0 <- y0-delta

  lname <- "Food Ingredients"
  temp <- MAT.ALL[!is.na(MAT.ALL[,"Intentional_Food_Ingredient"]),]
  x <- temp[,"information_availability_with_modifier"]
  qx <- quantile(x,probs=seq(0,1,0.05))
  y <- temp[,"scaled_score_overall_s3med"]
  qy <- quantile(y,probs=seq(0,1,0.05))
  lines(c(qx[11],qx[11]),c(qy[2],qy[20]))
  lines(c(qx[2],qx[20]),c(qy[11],qy[11]))
  points(qx[11],qy[11],pch=21,cex=1.5,bg="orange")
  points(x0,y0,pch=21,cex=1.5,bg="orange")
  text(x0,y0,lname,pos=4,cex=0.8)
  y0 <- y0-delta


  lname <- "SCIL"
  temp <- MAT.ALL[!is.na(MAT.ALL[,"SCIL"]),]
  x <- temp[,"information_availability_with_modifier"]
  qx <- quantile(x,probs=seq(0,1,0.05))
  y <- temp[,"scaled_score_overall_s3med"]
  qy <- quantile(y,probs=seq(0,1,0.05))
  lines(c(qx[11],qx[11]),c(qy[2],qy[20]))
  lines(c(qx[2],qx[20]),c(qy[11],qy[11]))
  points(qx[11],qy[11],pch=21,cex=1.5,bg="blue")
  points(x0,y0,pch=21,cex=1.5,bg="blue")
  text(x0,y0,lname,pos=4,cex=0.8)
  y0 <- y0-delta

  lname <- "SCIL Full Green"
  temp <- MAT.ALL[!is.na(MAT.ALL[,"SCIL.Green.Circle"]),]
  x <- temp[,"information_availability_with_modifier"]
  qx <- quantile(x,probs=seq(0,1,0.05))
  y <- temp[,"scaled_score_overall_s3med"]
  qy <- quantile(y,probs=seq(0,1,0.05))
  lines(c(qx[11],qx[11]),c(qy[2],qy[20]))
  lines(c(qx[2],qx[20]),c(qy[11],qy[11]))
  points(qx[11],qy[11],pch=21,cex=1.5,bg="green")
  points(x0,y0,pch=21,cex=1.5,bg="green")
  text(x0,y0,lname,pos=4,cex=0.8)
  y0 <- y0-delta

  lname <- "TSCA Active"
  x <- MAT.ALL[,"information_availability_with_modifier"]
  qx <- quantile(x,probs=seq(0,1,0.05))
  y <- MAT.ALL[,"scaled_score_overall_s3med"]
  qy <- quantile(y,probs=seq(0,1,0.05))
  lines(c(qx[11],qx[11]),c(qy[2],qy[20]))
  lines(c(qx[2],qx[20]),c(qy[11],qy[11]))
  points(qx[11],qy[11],pch=21,cex=1.5,bg="gray")
  points(x0,y0,pch=21,cex=1.5,bg="gray")
  text(x0,y0,lname,pos=4,cex=0.8)
  y0 <- y0-delta

  if(to.file) dev.off()
  else browser()
}
