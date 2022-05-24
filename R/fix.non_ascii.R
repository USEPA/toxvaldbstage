#-------------------------------------------------------------------------------------
#' Flag non ascii characters in the database  
#' @return The dataframe with non ascii characters replaced with XXX
#' @export
#-------------------------------------------------------------------------------------

fix.non_ascii <- function(df){
  non_ascii_check <- ""
  #check for non ascii characters
  for (i in 1:ncol(df)){
    non_ascii_check[i] <- any(grepl("NON_ASCII", iconv(df[,names(df)[i]], "UTF-8", "ASCII", sub="NON_ASCII")))
    #cat("Dataframe Column having non_ascii characters :", i, "\n")
    #print( non_ascii_check[i] )
  }
  
  non_ascii_find <- ""
  if(any(non_ascii_check==TRUE)){
    #find the non ascii characters
    for (i in 1:length(which(non_ascii_check==TRUE))){
      non_ascii_find[i] <- list(grep("NON_ASCII", iconv(df[,names(df)[which(non_ascii_check==TRUE)[i]]], "UTF-8", "ASCII", sub="NON_ASCII")))
      #cat("value with non_ascii characters:", i, "\n")
      #print( non_ascii_find[i] )
      
    }
    #flags non ascii characters to "XXX", as well as flags multiple simultaneously occuring non ascii characters to individual flag
    for (i in 1:length(non_ascii_find)){
      df[non_ascii_find[[i]],names(df)[which(non_ascii_check==TRUE)[i]]] <- iconv(df[non_ascii_find[[i]],names(df)[which(non_ascii_check==TRUE)[i]]],"UTF-8", "ASCII", sub = "XXX")
      df[non_ascii_find[[i]],names(df)[which(non_ascii_check==TRUE)[i]]] <- gsub("(XXX\\?)+", "XXX",df[non_ascii_find[[i]],names(df)[which(non_ascii_check==TRUE)[i]]])
      df[non_ascii_find[[i]],names(df)[which(non_ascii_check==TRUE)[i]]] <- gsub("(XXX)+", "XXX",df[non_ascii_find[[i]],names(df)[which(non_ascii_check==TRUE)[i]]])
      #cat("value with non_ascii characters fixed:", i, "\n")
      #print( df[non_ascii_find[[i]],names(df)[which(non_ascii_check==TRUE)[i]]] )
    }
  }
    return(df)
  
}