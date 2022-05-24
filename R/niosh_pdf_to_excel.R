library('openxlsx')
library('tidyverse')
library('tabulizer')
library('shiny')
library('miniUI')

#--------------------------------------------------------------------------------------
# code to create niosh_IDLH_2020.xlsx from CDC - Index of Chemicals - NIOSH Publications and Products.pdf produced 
# from saving the webpage(https://www.cdc.gov/niosh/idlh/intridl4.html) as pdf in ../niosh/niosh_files
#' @param infile The input file ../niosh/niosh_files/CDC - Index of Chemicals - NIOSH Publications and Products.pdf


#--------------------------------------------------------------------------------------
niosh.pdf.to.excel = function(infile){
  printCurrentFunction()
  
  # table_coordinates provides area dimensions to create niosh table
  table_coordinates <- locate_areas(infile)

  niosh_table <- extract_tables(infile,
                                output = "data.frame",
                                pages = c(1:16),
                                area = table_coordinates[-17] ,
                                guess = FALSE)


  # remove header row from all tables
  niosh_table <- lapply(niosh_table, function(x) x[-1,])

  #remove empty field from the last table
  niosh_table[[16]] <- niosh_table[[16]][, colSums(is.na(niosh_table[[16]])) != nrow(niosh_table[[16]])]

  # set new column names for the tables
  names_niosh_table <- c("name", "casrn","IDLH_value_1994","new_updated_values_2016-present")
  niosh_table <- lapply(niosh_table, setNames, names_niosh_table)

  # combine all the tables
  clean_niosh_table <- reduce(niosh_table, bind_rows) 
  write.xlsx(clean_niosh_table, "raw_niosh_table1.xlsx")

  # fix casrn metadata 
  casrn_details_cols <- grep("[a-zA-Z]", clean_niosh_table$casrn)
  casrn_details_not_na <- casrn_details_cols[clean_niosh_table$casrn[casrn_details_cols] != "n/a"]

  # create a copy of clean_niosh_table
  df1 <- clean_niosh_table


  for (i in casrn_details_not_na){
    df1[i-1,] <- unname(mapply(paste, sep = " ", df1[i-1,], df1[i,]))
  
  }

  # fix new_updated_values_2016-present metadata
  new_values_details_cols <- grep("^\\d{4}\\-\\d{3}\\)$|^[a-zA-Z]+.*", df1$`new_updated_values_2016-present`)

  for (i in new_values_details_cols){
    df1[i-1,] <- unname(mapply(paste, sep = " ", df1[i-1,], df1[i,]))
  
  }

  # remove extra rows of metadata after pasting it with the original value
  df1 <- df1[!(rownames(df1) %in% casrn_details_not_na),]
  df1 <- df1[!(rownames(df1) %in% new_values_details_cols),]

  # fix rownames
  row.names(df1) <- 1:nrow(df1)

  # create new field casrn details to house casrn metadata
  casrn_metadata <- grep("\\(.*\\)", df1$casrn)
  df1[casrn_metadata,'casrn_details']<- gsub("^.*\\s+(\\(.*\\))", "\\1", df1[casrn_metadata,'casrn'])
  df1[casrn_metadata,'casrn'] <- gsub("\\s+\\(.*\\)", "", df1[casrn_metadata,'casrn'])

  df1$casrn <- gsub("\\s+", "", df1$casrn)


  # fix IDLH_value_1994 metadata
  old_value_details_cols <- grep("^[a-zA-Z]+.*|^\\[.*", df1$IDLH_value_1994)
  old_value_details <- old_value_details_cols[df1$IDLH_value_1994[old_value_details_cols] != "Unknown"]
  for (i in old_value_details){
    df1[i-1,] <- unname(mapply(paste, sep = " ", df1[i-1,], df1[i,]))
  
  }
  # remove extra rows of metadata after pasting it with the original value
  df1 <- df1[!(rownames(df1) %in% old_value_details),]
  # fix rownames
  row.names(df1) <- 1:nrow(df1)

  # create field toxval numeric with IDLH_value_1994 and replacing the old IDLH_value_1994 with the new_updated_values_2016-present
  df1$toxval_numeric <- ifelse((df1$`new_updated_values_2016-present` == ""), df1$IDLH_value_1994, df1$`new_updated_values_2016-present`)
  num_details <- grep("\\(.*\\)",  df1$toxval_numeric)
  # create column toxval numeric details to house the metadata associated with toxval numeric
  df1[num_details,"toxval_numeric_details"] <- gsub("^.*[[:alnum:]]\\s*(\\(.*\\))", "\\1", df1[num_details,"toxval_numeric"])
  df1[num_details,"toxval_numeric"] <- gsub("(^.*[[:alnum:]]\\s*)\\(.*\\)$", "\\1", df1[num_details,"toxval_numeric"])

  #create field toxval units 
  df1$toxval_units <- gsub("^\\d+|^\\d+\\.\\d+|^\\d+\\,\\d+","",df1$toxval_numeric)
  df1$toxval_units <- gsub("^\\s+|\\s+$", "", df1$toxval_units)

  # remove toxval numeric metadata from toxval numeric values
  df1$toxval_numeric <-  gsub("(^\\d+|^\\d+\\.\\d+|^\\d+\\,\\d+).*", "\\1", df1$toxval_numeric)

  df1$toxval_numeric[df1$toxval_numeric == "Unknown"] <- ""
  df1$toxval_numeric <- gsub("\\s+","", df1$toxval_numeric)
  df1$toxval_numeric <- gsub("\\,","", df1$toxval_numeric)
  df1$toxval_numeric <- as.numeric(df1$toxval_numeric)
  # create field for source_url
  df1$source_url <- "https://www.cdc.gov/niosh/idlh/intridl4.html"
  df1$name <- gsub("^\\s+|\\s+$","", df1$name)
  write.xlsx(df1, "raw_niosh_table2.xlsx")
  new_niosh <- df1[,!(colnames(df1) %in% c("IDLH_value_1994","new_updated_values_2016-present"))]
  new_niosh$casrn_details <- gsub("NA", "", new_niosh$casrn_details)
  new_niosh$casrn_details <- gsub("\\s+", "", new_niosh$casrn_details)

  new_niosh$casrn <- gsub("n/a", "-", new_niosh$casrn)
  new_niosh[new_niosh$casrn == "","casrn"] <- "-"
  write.xlsx(new_niosh, "niosh_IDLH_2020.xlsx")
}