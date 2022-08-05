#--------------------------------------------------------------------------------------
#' Make a plot summarizing the IG flag informaiton
#' @param to.file If TRUE, print the graph to a file, else write to the screen
#' @param sys.date The date of the bin data Excel fileto read
#' @param do.read if TRUE, read in the source file and store in a global
#--------------------------------------------------------------------------------------
ig.summary.plot <- function(to.file=F,sys.date,do.read=F) {
  printCurrentFunction()

  if(to.file) {
    fname <- paste("../tsca_binning/IG Summary ",sys.date,".pdf",sep="")
    pdf(file=fname,width=8,height=6,pointsize=12,bg="white",paper="letter",pagecentre=T)
  }
  par(mfrow=c(1,1),mar=c(4,15,2,2))
  if(do.read) {
    file <-   file <- paste0(toxval.config()$datapath,"tsca_binning/tsca_bin_output_step_2_",sys.date,".xlsx")
    MAT.ALL <<- read.xlsx(file)
    file <-   file <- paste0(toxval.config()$datapath,"POC selection/POC Selection rationale.xlsx")
    MAT.POC <<- read.xlsx(file)
  }
  mat <- MAT.ALL[!is.na(MAT.ALL$TSCA_POC),]
  domain.list <- NULL
  name.list <- NULL
  ig.list <- NULL
  domains <- c("baf","children","sensitization_irritation","cancer","genetox","eco_hazard","human_hazard")
  titles <- c("Bioaccumulation","Susceptible Population","Sensitization/Irritation","Cancer","Genotoxicity","Ecological Hazard","Human Hazard")

  for(i in 1:length(domains)) {
    domain <- domains[i]
    title <- titles[i]
    print(domain)
    if(domain=="human_hazard") {
      col.list <- c("mammalian_neurotoxicity",
                    "mammalian_repeat_dose",
                    "mammalian_developmental",
                    "mammalian_reproductive",
                    "mammalian_chronic",
                    "mammalian_subchronic",
                    "mammalian_acute"
      )
      for(col in col.list) {
        domain.list <- c(domain.list,title)
        name.list <- c(name.list,str_replace_all(str_replace(col,"mammalian_",""),"_"," "))
        x <- mat[,col]
        missing.ratio <- (length(x)-sum(x))/length(x)
        ig.list <- c(ig.list,missing.ratio)
      }
    }
    if(domain=="eco_hazard") {
      col.list <- c("eco_acute_plant","eco_acute_invertebrate","eco_acute_vertebrate","eco_repeat_dose_plant","eco_repeat_dose_invertebrate","eco_repeat_dose_vertebrate")
       for(col in col.list) {
        domain.list <- c(domain.list,title)
        name.list <- c(name.list,str_replace_all(str_replace(col,"eco_",""),"_"," "))
        x <- mat[,col]
        missing.ratio <- (length(x)-sum(x))/length(x)
        ig.list <- c(ig.list,missing.ratio)
      }
    }
    if(domain=="cancer") {
      col.list <- c("ig_flag_cancer")
      for(col in col.list) {
        domain.list <- c(domain.list,title)
        name.list <- c(name.list,"No cancer data")
        x <- mat[,col]
        y <- x[!is.na(x)]
        missing.ratio <- length(y)/length(x)
        ig.list <- c(ig.list,missing.ratio)
      }
    }
    if(domain=="genetox") {
      col <- c("ig_flag_genetox")
      x <- mat[,col]
      bot <- length(x)
      y <- x[!is.na(x)]
      top <- length(y[is.element(y,"No genetox data available")])
      domain.list <- c(domain.list,title)
      name.list <- c(name.list,"No genetox data or predictions")
      ig.list <- c(ig.list,top/bot)

      top <- length(y[is.element(y,"Only predicted genetox information available")])
      domain.list <- c(domain.list,title)
      name.list <- c(name.list,"Only predicted genetox data")
      ig.list <- c(ig.list,top/bot)
    }
    if(domain=="children") {
      col <- c("ig_flag_children")
      x <- mat[,col]
      bot <- length(x)
      y <- x[!is.na(x)]
      top <- length(y)
      domain.list <- c(domain.list,title)
      name.list <- c(name.list,"No exposure predictions")
      ig.list <- c(ig.list,top/bot)
     }
    if(domain=="baf") {
      col <- c("ig_flag_baf")
      x <- mat[,col]
      bot <- length(x)
      y <- x[!is.na(x)]
      top <- length(y[is.element(y,"BAF low confidence (OPERA BCF out of AD)")])
      domain.list <- c(domain.list,title)
      name.list <- c(name.list,"BAF low confidence")
      ig.list <- c(ig.list,top/bot)

      top <- length(y[is.element(y,"BAF medium confidence (modeled value)")])
      domain.list <- c(domain.list,title)
      name.list <- c(name.list,"BAF medium confidence")
      ig.list <- c(ig.list,top/bot)

      top <- length(y[is.element(y,"no BAF data or models")])
      domain.list <- c(domain.list,title)
      name.list <- c(name.list,"No BAF data or models")
      ig.list <- c(ig.list,top/bot)
    }
    if(domain=="sensitization_irritation") {
      col.list <- c("skin_sensitization_rank","eye_irritation_rank","skin_irritation_rank")
      for(col in col.list) {
        domain.list <- c(domain.list,title)
        name.list <- c(name.list,str_replace_all(str_replace(col,"_rank",""),"_"," "))
        x <- mat[,col]
        y <- x[is.na(x)]
        missing.ratio <- length(y)/length(x)
        ig.list <- c(ig.list,missing.ratio)
      }
    }
  }
  x <- paste(domain.list,":",name.list)
  barplot(ig.list,names=x,horiz=T,las=1,cex.axis=1,cex.lab=1,cex.names=0.7,xlab="Fraction of POC with IG Flag",xlim=c(0,1),main="POC")
  if(!to.file) browser()

  if(to.file) dev.off()
  else browser()
}
