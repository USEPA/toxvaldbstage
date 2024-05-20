# library('dplyr')
# library('tidyr')
#-------------------------------------------------------------------------------------
#' @description Extract ACToR1 data to toxval source
#' @param toxval.db The version of toxval source into which the tables are loaded.
#' @param infile The input file ./ACToR replacements/ACToR_2021/assay_table_hazard prioritized for use.xlsx
#' @param filepath The path for all the input xlsx files ./ACToR replacements/ACToR_2021
#' @param verbose Whether the loaded rows should be printed to the console.
#' @param do.init if TRUE, read the data in from the res_actor_2021q4 database and set up the matrix
#' @export 
#' @title FUNCTION_TITLE
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso 
#'  \code{\link[openxlsx]{read.xlsx}}, \code{\link[openxlsx]{write.xlsx}}
#'  \code{\link[stringr]{str_replace}}
#'  \code{\link[dplyr]{group_by}}, \code{\link[dplyr]{select}}, \code{\link[dplyr]{mutate-joins}}, \code{\link[dplyr]{mutate_all}}
#'  \code{\link[tidyr]{spread}}
#'  \code{\link[stats]{aggregate}}, \code{\link[stats]{na.fail}}
#' @rdname import_actor_source
#' @importFrom openxlsx read.xlsx write.xlsx
#' @importFrom stringr str_replace_all
#' @importFrom dplyr group_by select left_join mutate_at
#' @importFrom tidyr spread
#' @importFrom stats aggregate na.omit
#--------------------------------------------------------------------------------------
import_actor_source <- function(toxval.db,infile,filepath, verbose=F) {
  printCurrentFunction(toxval.db)

  #####################################################################
  cat("Extract ACToR data based on the assays listed in the input file as useme 1 \n")
  #####################################################################

  assay_dict <- openxlsx::read.xlsx(infile)
  assay_dict_to_use <- assay_dict[which(assay_dict$useme == 1),]

  assay_test <- unique(assay_dict_to_use$assay_id)
  # testing with a subset of data
  assay_test <- assay_test[225:230]


  actor_tables <- list()
  for(i in 1:length(assay_test)) {
    query <- paste0("SELECT DISTINCT f.casrn,h.name ,a.source, i.url,a.source_name_aid ,g.source_name_sid, g.source_name_cid,a.assay_id,a.description as assay_description, b.assay_component_id, a.name as assay_name,
                  b.name as assay_component_name,
                  e.description as assay_category,
                  c.value_numerical,
                  c.value_string,
                  b.units,
                  c.substance_id,
                  c.result_group
                  from assay a,
                  assay_component b,
                  assay_result c,
                  assay_category d,
                  assay_category_cv e,
                  substance_casrn f,
                  substance g,
                  substance_name h,
                  substance_url i
                  where
                  b.assay_id=a.assay_id
                  and c.assay_component_id=b.assay_component_id
                  and d.assay_id=a.assay_id
                  and e.assay_category_cv_id = d.assay_category_cv_id
                  and f.substance_id=c.substance_id
                  and g.substance_id=c.substance_id
                  and h.substance_id=c.substance_id
                  and i.substance_id=c.substance_id
                  and e.description like '%hazard%' and a.assay_id like '",assay_test[i],"'")

    db <- "res_actor_2021q4"
    mat.in <- runQuery(query,db)

    mat <- unique(mat.in)
    MAT1 <<- mat

    res1 <- unique(MAT1)
    actor_tables[[i]] <- res1

  }


  # name each table in the list of df's based on the names represented in source_name_sid column
  table_names <- unique(unlist(lapply(actor_tables, '[[', "source_name_aid")))
  table_names <- as.character(table_names)
  names(actor_tables) <- table_names

  # create new columns for each value represented in assay_component_name field
  col_names <- list()
  for (i in 1:length(table_names)){
    col_names[[i]] <- unique(unlist(lapply(actor_tables[i], '[[', "assay_component_name")))
    actor_tables[[i]][col_names[[i]]] <- NA
  }

  # changing dot in assay_component_name column values to underscore

  # multiple spaces and dots to one
  actor_tables <- lapply(actor_tables, function(x) {
    x[,"assay_component_name"] <- gsub("([\\.\\s])\\1+", "\\1", x[,"assay_component_name"], perl=TRUE)
    return(x)
  })
  # replace with underscore
  actor_tables <- lapply(actor_tables, function(x) {
    x[,"assay_component_name"] <- stringr::str_replace_all(x[,"assay_component_name"],"\\s+|\\.", "_")
    return(x)
  })

  actor_tables <- lapply(actor_tables, function(x) {
    colnames(x) <- gsub("([\\.\\s])\\1+", "\\1", colnames(x), perl=TRUE)
    return(x)
  })

  actor_tables <- lapply(actor_tables, function(x) {
    colnames(x) <- stringr::str_replace_all(colnames(x),"\\s+|\\.+", "_")
    return(x)
  })

  print(names(actor_tables))
  #print(View(actor_tables[[1]]))

  # # write each df to individual excel files
  #
  # for(i in seq_along(actor_tables)) {
  #   write.xlsx(actor_tables[[i]], paste0(toxval.config()$datapath,"ACToR replacements/ACToR_2021/",names(actor_tables)[i], ".xlsx"))
  # }

  # processing the extracted ACToR data and output as individual excel files
  for (i in 1:length(actor_tables)){
    res1 <- actor_tables[[i]]
    res1_name <- names(actor_tables[i])
    #print(res1_name)
    #print(names(res1))
    # list the components in assay_component_name field
    components <- unique(res1$assay_component_name)
    #print(components)
    #seperate out the numerical and string values as seperate df's
    df_a <- res1[which(!is.na(res1$value_string)),]
    df_b <- res1[which(!is.na(res1$value_numerical)),]

    # assign values represented in value_string based on substance_id and result_group to components represented in assay_component_name field
    df1 <- df_a %>%
      dplyr::group_by(substance_id,result_group) %>%
      tidyr::spread(assay_component_name, value_string) %>%
      dplyr::select(names(df_a)[!names(df_a) %in% c("assay_component_name","value_string")])
    df1 <- data.frame(df1,stringsAsFactors = F)
    df1 <- df1[,names(df1)[!names(df1) %in% c("assay_component_id","value_numerical")]]

    # remove all na columns
    df1 <- df1[,colSums(is.na(df1))<nrow(df1)]
    #print(names(df1))
    #print(dim(df1))
    # assign values represented in value_numerical based on substance_id and result_group to components represented in assay_component_name field
    df2 <- df_b %>%
      dplyr::group_by(substance_id,result_group) %>%
      tidyr::spread(assay_component_name, value_numerical) %>%
      dplyr::select(names(df_b)[!names(df_b) %in% c("assay_component_name","value_numerical")])
    df2 <- data.frame(df2,stringsAsFactors = F)
    df2 <- df2[,names(df2)[!names(df2) %in% c("assay_component_id","value_string")]]
    df2 <- df2[,colSums(is.na(df2))<nrow(df2)]

    # Common columns
    common <- names(df2)[names(df2) %in% names(df1)]

    # assign same data type for common columns, so it eradicates issues arising due to type mismatch.
    df2[common] <- lapply(common, function(x) {
      match.fun(paste0("as.", class(df1[[x]])))(df2[[x]])
    })
    #print(names(df2))
    #print(dim(df2))
    if (all(names(df2)[names(df2)%in% components] %in% names(df2)[names(df2) %in% names(df1)]) == T ){
      if (length(names(df2)[names(df2)%in% components]) > 0 & length(names(df2)) != 0){

        #combine both dfs containing value string and value numerical
        df3 <- dplyr::left_join(df1,df2, by = names(df2)[names(df2)[names(df2) %in% names(df1)] != names(df2)[names(df2)%in% components]] )
        # aggregate based on substance_id and result_group
        df4 <- stats::aggregate(df3[names(df3)[!names(df3)%in% c("substance_id","result_group")]],df3[names(df3)[names(df3)%in% c("substance_id","result_group")]], function(x) paste0(unique(stats::na.omit(x))))
        # convert list datatype columns to character columns
        df4 <- df4 %>%
          dplyr::mutate_at(names(df4)[which(sapply(df4, class) == "list")], as.character)

        openxlsx::write.xlsx(df4, paste0(toxval.config()$datapath,"ACToR replacements/ACToR_2021/new_ACToR_2021/new_",res1_name, ".xlsx"))



      } else if (length(names(df2)[names(df2)%in% components]) == 0  & length(names(df2)) == 0) {
        #print(paste0("Source with no matching components in numerical table: ",names(res)[i]))
        # aggregate based on substance_id and result_group
        df4 <- stats::aggregate(df1[names(df1)[!names(df1)%in% c("substance_id","result_group")]],df1[names(df1)[names(df1)%in% c("substance_id","result_group")]], function(x) paste0(unique(stats::na.omit(x))))
        # convert list datatype columns to character columns
        df4 <- df4 %>%
          dplyr::mutate_at(names(df4)[which(sapply(df4, class) == "list")], as.character)
        openxlsx::write.xlsx(df4, paste0(toxval.config()$datapath,"ACToR replacements/ACToR_2021/new_ACToR_2021/new_",res1_name, ".xlsx"))

      } else if (length(names(df2)[names(df2)%in% components]) == 0  & length(names(df2)) != 0){
        # aggregate based on substance_id and result_group
        df4 <- stats::aggregate(df2[names(df2)[!names(df2)%in% c("substance_id","result_group")]],df2[names(df2)[names(df2)%in% c("substance_id","result_group")]], function(x) paste0(unique(stats::na.omit(x))))
        # convert list datatype columns to character columns
        df4 <- df4 %>%
          dplyr::mutate_at(names(df4)[which(sapply(df4, class) == "list")], as.character)
        openxlsx::write.xlsx(df4, paste0(toxval.config()$datapath,"ACToR replacements/ACToR_2021/new_ACToR_2021/new_",res1_name, ".xlsx"))

      } else if (length(names(df2)[names(df2)%in% components]) > 0  & length(names(df1)) == 0){
        # aggregate based on substance_id and result_group
        df4 <- stats::aggregate(df2[names(df2)[!names(df2)%in% c("substance_id","result_group")]],df2[names(df2)[names(df2)%in% c("substance_id","result_group")]], function(x) paste0(unique(stats::na.omit(x))))
        # convert list datatype columns to character columns
        df4 <- df4 %>%
          dplyr::mutate_at(names(df4)[which(sapply(df4, class) == "list")], as.character)
        openxlsx::write.xlsx(df4, paste0(toxval.config()$datapath,"ACToR replacements/ACToR_2021/new_ACToR_2021/new_",res1_name, ".xlsx"))

      }

    } else if (all(names(df2)[names(df2)%in% components] %in% names(df2)[names(df2) %in% names(df1)]) == F) {

      #print(paste0("Source with some mismatched components in string and numerical tables: ",names(res)[i]))
      # create columns in string df which are present in numerical df


      if (length(names(df1)) != 0 & length(names(df2)) != 0){
        # if both string df and numerical df have non zero number of fields, but doesn't match with all the components
        missing_components <- names(df2)[!names(df2) %in% names(df1)]
        for(i in 1:length(missing_components)){
          if (length(missing_components) != 0){
            if (length(names(df1)[names(df1) == missing_components[i]]) == 0 & length(names(df2)[names(df2) == missing_components[i]]) != 0){
              df1[missing_components[i]] <- NA
              df1[missing_components[i]] <- lapply(missing_components[i], function(x) {
                match.fun(paste0("as.", class(df2[[x]])))(df1[[x]])
              })
            }
          }
        }
        #combine both dfs containing value string and value numerical
        df3 <- dplyr::left_join(df1,df2, by = names(df2)[!names(df2)[names(df2) %in% names(df1)] %in% names(df2)[names(df2)%in% components]] )
        # aggregate based on substance_id and result_group
        df4 <- stats::aggregate(df3[names(df3)[!names(df3)%in% c("substance_id","result_group")]],df3[names(df3)[names(df3)%in% c("substance_id","result_group")]], function(x) paste0(unique(stats::na.omit(x))))
        # convert list datatype columns to character columns
        df4 <- df4 %>%
          dplyr::mutate_at(names(df4)[which(sapply(df4, class) == "list")], as.character)

        common_names <- split(names(df4)[grep(".*\\.x$|.*\\.y$", names(df4))], gsub('(.*)(\\.[x|y]$)', '\\1', names(df4)[grep(".*\\.x$|.*\\.y$", names(df4))], perl=TRUE))


        for (i in 1:length(common_names)){
          if (all(df4[,names(df4)[names(df4) %in% common_names[[i]][1]]] == df4[,names(df4)[names(df4) %in% common_names[[i]][2]]]) == TRUE){
            names(df4)[names(df4) %in% common_names[[i]][1]] <- gsub("(.*)(\\..*$)","\\1",names(df4)[names(df4) %in% common_names[[i]][1]])
            df4 <- df4[,names(df4)[!names(df4) %in% common_names[[i]][2]]]
          }
        }

        openxlsx::write.xlsx(df4, paste0(toxval.config()$datapath,"ACToR replacements/ACToR_2021/new_ACToR_2021/new_",res1_name, ".xlsx"))

      } else if (length(names(df1)) == 0 & length(names(df2)) != 0){
        # if there are no fields in string df, aggregate based on numerical df
        df4 <- stats::aggregate(df2[names(df2)[!names(df2)%in% c("substance_id","result_group")]],df2[names(df2)[names(df2)%in% c("substance_id","result_group")]], function(x) paste0(unique(stats::na.omit(x))))
        # convert list datatype columns to character columns
        df4 <- df4 %>%
          dplyr::mutate_at(names(df4)[which(sapply(df4, class) == "list")], as.character)
        openxlsx::write.xlsx(df4, paste0(toxval.config()$datapath,"ACToR replacements/ACToR_2021/new_ACToR_2021/new_",res1_name, ".xlsx"))

      } else if (length(names(df1)) != 0 & length(names(df2)) == 0){
        # if there are no fields in numerical df, aggregate based on string df
        df4 <- stats::aggregate(df1[names(df1)[!names(df1)%in% c("substance_id","result_group")]],df1[names(df1)[names(df1)%in% c("substance_id","result_group")]], function(x) paste0(unique(stats::na.omit(x))))
        # convert list datatype columns to character columns
        df4 <- df4 %>%
          dplyr::mutate_at(names(df4)[which(sapply(df4, class) == "list")], as.character)
        openxlsx::write.xlsx(df4, paste0(toxval.config()$datapath,"ACToR replacements/ACToR_2021/new_ACToR_2021/new_",res1_name, ".xlsx"))
      }

    }
  }

  # read in the newly created actor files from ACToR_2021 folder in ACToR_Replacements

  files.list <- list.files(path = filepath, pattern = "*.xlsx")
  any_temp_files <- grep("^\\~\\$.*", files.list, value = T)
  files.list <- files.list[! files.list %in% any_temp_files]
  files.list <- paste0( filepath, '/',files.list)
  res <- lapply(files.list,read.xlsx)

  names.list <- gsub("(.*)(\\.xlsx)","\\1",files.list)

  # insert the files in source db

  stop = FALSE
  for( i in 1:length(res)){
    for (j in 1:length(names.list)){

      runInsertTable(res[[i]],names.list[j],toxval.db,do.halt=T,verbose=F)
      i <- i+1
      if (i == length(res)+1){
        stop = TRUE
        break
      }
    }
    if (stop){break}
  }



}
