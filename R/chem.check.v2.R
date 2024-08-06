#--------------------------------------------------------------------------------------
#' @param res0 The data frame in which chemicals names and CASRN will be replaced
#' @param source The source to be processed. If source=NULL, process all sources
#' @param verbose If TRUE, print diagnostic messages
#' @return Return a list with fixed CASRN and name and flags indicating if fixes were made:
#' res0=res0,name.OK=name.OK,casrn.OK=casrn.OK,checksum.OK=checksum.OK
#'
#' @title chem.check.v2
#' @description Check the chemicals from a file
#' Names with special characters are cleaned and trimmed
#' CASRN are fixed (dashes put in, trimmed) and check sums are calculated
#' The output is sent to a file called chemcheck.xlsx in the source data file
#' One option for using this is to edit the source file until no errors are found
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  [stri_escape_unicode][stringi::stri_escape_unicode]
#'  [str_replace_all][stringr::str_replace_all], [str_squish][stringr::str_squish]
#'  [rowwise][dplyr::rowwise], [mutate][dplyr::mutate], [ungroup][dplyr::ungroup], [filter][dplyr::filter], [select][dplyr::select], [rename][dplyr::rename], [distinct][dplyr::distinct]
#'  [separate][tidyr::separate]
#'  [write_xlsx][writexl::write_xlsx]
#' @rdname chem.check.v2
#' @export
#' @importFrom stringi stri_escape_unicode
#' @importFrom stringr str_replace_all str_squish
#' @importFrom dplyr rowwise mutate ungroup filter select rename distinct
#' @importFrom tidyr separate
#' @importFrom writexl write_xlsx
#--------------------------------------------------------------------------------------
chem.check.v2 <- function(res0, source=NULL, verbose=FALSE) {
  printCurrentFunction(source)
  name.OK = TRUE
  casrn.OK = TRUE
  checksum.OK = TRUE

  cat(">>> Deal with name\n")
  chem.check.name <- function(in_name, source, verbose){
    n0 = in_name %>%
      # Replace zero width space unicode
      gsub("\u200b", "", .)

    if(is.na(n0)) {
      return(paste(n0, n0, n0, sep="||"))
    }
    # Handle translation to ASCII
    n1 = n0 %>%
      iconv(.,from="UTF-8",to="ASCII//TRANSLIT")
    n2 <- n1 %>%
      stringi::stri_escape_unicode() %>%
      stringr::str_replace_all("\\\\'","\'") %>%
      stringr::str_squish() %>%
      stringr::str_replace_all("\\.\\.\\.|\\(registered trademark\\)|#|\\*", "") %>%
      # Remove IUCLID ending bracket note
      gsub("\\[.*\\.\\]$", "", .)

    if(!is.null(source) && source %in% c("Alaska DEC",
                                         "EPA AEGL",
                                         "Mass. Drinking Water Standards",
                                         "OSHA Air contaminants",
                                         "OW Drinking Water Standards",
                                         "Pennsylvania DEP MSCs",
                                         "USGS HBSL",
                                         "WHO IPCS",
                                         "ATSDR MRLs",
                                         "Cal OEHHA",
                                         "COSMOS",
                                         "DOD ERED",
                                         "DOE Wildlife Benchmarks",
                                         "DOE Protective Action Criteria",
                                         "IRIS",
                                         "EPA OPP",
                                         "Pennsylvania DEP ToxValues",
                                         "EnviroTox_v2",
                                         "HEAST")) {
      # Only take first name stem before ";"
      if(grepl(";", n2)) {
        n2 = sub(';.*', '', n2)
      }
      # Remove trailing abbreviation (ex. "DI(2-ETHYLHEXYL)PHTHALATE (DEHP)" to "DI(2-ETHYLHEXYL)PHTHALATE")
      if(grepl(" \\(", n2)) {
        n2 = sub(' \\(.*', '', n2)
      }
    }
    n2 = clean.last.character(n2)
    if(verbose) cat("1>>> ",n0,n1,n2,"\n")
    return(paste(n0, n1, n2, sep="||"))
  }

  # Correct Formula
  correct_formula <- function(df,col='name',comment='name_comment'){
    df_copy <- df
    df_copy$name_is_formula <- sapply(df_copy[[col]], find_formula)
    idx <- df_copy$name_is_formula
    df_copy[idx, comment] <- apply(df_copy[idx, c(comment, col)], 1, function(x){
      append_col(x=x[comment], s=x[col], comment="Name only formula")
    })
    df_copy[idx, col] <- NA
    df_copy <- df_copy[, !(names(df_copy) %in% 'name_is_formula')]
    return(df_copy)
  }

  find_formula <- function(x) {
    if (is.character(x)) {
      regex <- '([A-Z][a-z]?)(\\d*(?:(?:[\\.|\\,])\\d+(?:\\%)?)?)|(?:[\\(|\\[])([^()]*(?:(?:[\\(|\\[]).*(?:[\\)|\\]]))?[^()]*)(?:[\\)|\\]])(\\d*(?:(?:[\\.|\\,]?)\\d+(?:\\%)?))'
      s <- regmatches(x, gregexpr(regex, x))
      s <- unlist(s)
      if (length(s) < 1) {
        s <- ''
      } else {
        s <- paste(s, collapse = "")
        c <- regmatches(s, gregexpr('\\d', s))
        c <- unlist(c)
        if (length(c) < 1) {

          if (s != "NaCl") {
            s <- ''
          }
        }
      }
    } else {
      s <- ''
    }
    if (is.na(x)) {
      x <- 'empty'
    }
    return(identical(s, x))
  }

  append_col <- function(x, s, comment, sep="|") {
    if (!is.null(x) && !is.na(x)) {
      if (!is.null(s) && is.na(s)) {
        s <- paste(comment, ": ", s, sep=": ")
        y <- paste(x, s, sep=sep)
      } else {
        y <- x
      }
    } else if (!is.null(s) && !is.na(s)) {
      s <- paste(comment, s, sep=": ")
      y <- s
    } else {
      y <- NA
    }
    return(y)
  }

  # Drop foods
  foods <- function() {
    food <- c('yeast culture', 'food starch', 'sweet whey',
              'salted fish', 'beverage')
    return(paste(food, collapse = "|"))
  }

  block_list <- function() {
    block <- c('alcohol', 'alcohol', 'Bly', 'Bly', 'Polyester', 'Polyester',
               'Alkanes', 'Alkanes', 'alkanes', 'alkanes', 'red 4, 33',
               'red 4, 33', 'rose', 'rose',
               'Organic electrolyte principally involves ester carbonate',
               'Organic electrolyte principally involves ester carbonate',
               'PP', 'PP', 'Amine soap', 'Amine soap', 'Free Amines',
               'Free Amines', 'Acrylic Polymer', 'Acrylic Polymers',
               'Urethane Polymer', 'Acrylic Polymer', 'Acrylic Polymers',
               'Urethane Polymer', 'Caustic Salt', 'Caustic Salt', '', '',
               'Aflatoxins', 'Aflatoxins', 'Aminoglycosides', 'Anabolic steroids',
               'Analgesic mixtures containing Phenacetin', 'Aminoglycosides',
               'Anabolic steroids', 'Analgesic mixtures containing Phenacetin',
               'Aristolochic acids', 'Aristolochic acids', 'Barbiturates',
               'Barbiturates', 'Benzodiazepines', 'Benzodiazepines',
               'Conjugated estrogens', 'Conjugated estrogens',
               'Dibenzanthracenes', 'Dibenzanthracenes', 'Estrogens, steroidal',
               'Estrogen-progestogen (combined) used as menopausal therapy',
               'Estrogens, steroidal',
               'Estrogen-progestogen (combined) used as menopausal therapy',
               'Etoposide in combination with cisplatin and bleomycin',
               'Etoposide in combination with cisplatin and bleomycin',
               'Cyanide salts that readily dissociate in solution (expressed as cyanide)f',
               'Cyanide salts that readily dissociate in solution (expressed as cyanide)f')
    return(unique(block))
  }

  drop_foods <- function(df, col='name', comment='name_comment'){
    df_copy <- df
    foods <- foods()
    idx <- grepl(paste(foods, collapse="|"), tolower(df_copy[[col]]))
    if(any(idx)){
      df_copy[[comment]][idx] <- mapply(append_col, df_copy[[comment]][idx], s=df_copy[[col]][idx], comment="Name is food")
      df_copy[[col]][idx] <- NA
    }
    return(df_copy)
  }

  drop_blocks <- function(df, col='name', comment='name_comment'){
    blocks <- block_list()
    df_copy <- df
    idx <- df_copy[[col]] %in% blocks
    if(any(idx)){
      df_copy[[comment]][idx] <- sapply(1:nrow(df_copy[idx,]), function(i){
        append_col(df_copy[idx,][i,][[comment]], df_copy[idx,][i,][[col]], comment="Name is on block list")
      })
      df_copy[[col]][idx] <- NA
    }
    return(df_copy)
  }

  # Drop stoppers
  stops <- function(){
    stop_words <- c('proprietary', 'ingredient', 'hazard', 'blend', 'inert', 'stain',
                    'other', 'withheld', 'cas |cas-|casrn',
                    'secret', "herbal",
                    'confidential', 'bacteri', 'treatment', 'contracept', 'emission',
                    "agent", "eye", "resin", "citron", 'bio', 'smoke', 'fiber', 'adult',
                    'boy', 'girl', 'infant', 'child', 'other organosilane', 'material')
    return(stop_words)
  }

  drop_stoppers <- function(df, col='name', comment='name_comment'){
    df_copy <- df
    idx <- grepl(paste(stops(), collapse="|"), tolower(df_copy[[col]])) &
      !grepl("yl", tolower(df_copy[[col]]))
    if(any(idx)){
      df_copy[[comment]][idx] <- mapply(append_col, df_copy[[comment]][idx], df_copy[[col]][idx], "Ambiguous name")
      df_copy[[col]][idx] <- NA
    }

    specific_terms <- c('polymer', 'polymers', 'wax', 'mixture', 'citron', 'compound')
    idx <- tolower(df[[col]]) %in% specific_terms
    if(any(idx)){
      df_copy[[comment]][idx] <- mapply(append_col, df_copy[[comment]][idx], s=df_copy[[col]][idx], "Ambiguous name")
      df_copy[[col]][idx] <- NA
    }

    return(df_copy)
  }

  # Drop text
  drop_text <- function(df, col='name', comment='name_comment') {
    df_copy <- df
    idx <- grepl("^part [a-z]:", tolower(df_copy[[col]]))
    if(any(idx)){
      df_copy[[col]][idx] <- sapply(strsplit(as.character(df_copy[[col]][idx]), ":"), function(x) x[2])
      df_copy[[comment]][idx] <- mapply(append_col, df_copy[[comment]][idx], s=sapply(strsplit(as.character(df_copy[[col]][idx]), ":"), function(x) x[1]), comment="Removed text")
    }

    idx <- grepl('modif', tolower(df_copy[[col]]))
    if(any(idx)){
      df_copy[[comment]][idx] <- mapply(append_col, df_copy[[comment]][idx], s=df_copy[[col]][idx], comment="Unknown modification")
      df_copy[idx, col] <- NA
    }

    quality <- c('pure', 'purif', 'techni', 'grade', 'chemical')
    pat <- paste0("(-?)\\b(\\w*", quality, "\\w*)\\b(-?)", collapse="|")
    idx <- grepl(pat, tolower(df_copy[[col]]))
    if(any(idx)){
      df_copy[[comment]][idx] <- mapply(append_col, df_copy[[comment]][idx], s=gsub(pat, "", df_copy[[col]][idx], ignore.case=TRUE), comment="Unneeded adjective")
      df_copy[[col]][idx] <- gsub(pat, "", df_copy[[col]][idx], ignore.case = TRUE)
    }

    idx <- grepl("\\d+\\%$", tolower(df_copy[[col]]))
    if (any(idx)){
      matches <- regmatches(df_copy[[col]][idx], regexec("\\d+\\%$", df_copy[[col]][idx]))
      matches <- matches[lengths(matches) > 0]
      if(length(matches) > 0){
        df_copy[[comment]][idx] <- mapply(append_col, df_copy[[comment]][idx], s=matches[[1]], comment="Removed text")
        df_copy[[col]][idx] <- gsub("\\d+\\%$", "", df_copy[[col]][idx])
      }
    }

    df_copy[[col]] <- stringr::str_squish(df_copy[[col]])
    #df_copy[[col]] <- gsub("^,|-|,$", "", df_copy[[col]])
    df_copy[[comment]] <- stringr::str_squish(df_copy[[comment]])
    return(df_copy)
  }

  # Drop salts
  drop_salts <- function(df, col = 'name', comment = 'name_comment') {
    df_copy <- df
    pat <- 'and its .* salts|and its salts'
    idx <- grepl(pat, tolower(df_copy[[col]]), ignore.case = TRUE)
    df_copy[[comment]][idx] <- mapply(function(x, col_val){
      s <- regmatches(col_val, regexec(pat, col_val, ignore.case = TRUE))[[1]][1]
      append_col(x, s, comment="Ambiguous salt reference")
    }, df_copy[[comment]][idx], df_copy[[col]][idx])

    df_copy[[col]][idx] <- mapply(function(col_val){
      strsplit(col_val, pat)[[1]][1]
    }, df_copy[[col]][idx])

    return(df_copy)
  }


  res0 = res0 %>%
    dplyr::rowwise() %>%
    dplyr::mutate(name_check = chem.check.name(in_name=name,
                                               source=source,
                                               verbose=verbose)) %>%
    dplyr::ungroup() %>%
    tidyr::separate(name_check,
                    into=c("n0", "n1", "n2"),
                    sep="\\|\\|")

  res0$name_comment <- NA
  res0 <- res0 %>%
    correct_formula(df = ., col='n2', comment='name_comment') %>%
    drop_blocks(df = ., col='n2', comment='name_comment') %>%
    drop_foods(df = ., col='n2', comment='name_comment') %>%
    drop_stoppers(df = ., col='n2', comment='name_comment') %>%
    drop_text(df = ., col='n2', comment='name_comment') %>%
    drop_salts(df = ., col='n2', comment='name_comment') %>%
    mutate(n2 = str_remove(n2, ",$"),
           n2 = str_replace_all(n2, "\\( ?\\)|\\[ ?\\]", ""),
           n2 = gsub(";\\s*$", "", n2),
           n2 = gsub("\\[\\s*$", "", n2),
           n2 = gsub(",\\s*;(?!\\S)",";", n2, perl = TRUE))

  ccheck_name = res0 %>%
    dplyr::filter(n2 != n0) %>%
    dplyr::select(n0, n1, n2, name_comment) %>%
    dplyr::mutate(cs = NA)

  if(nrow(ccheck_name)) {
    name.OK = FALSE
  }

  # Set name as cleaned n2, remove intermediates
  res0 = res0 %>%
    dplyr::select(-name, -n0, -n1) %>%
    dplyr::rename(name = n2)

  cat("\n>>> Deal with CASRN\n")
  chem.check.casrn <- function(in_cas, verbose){
    n0 = in_cas
    if(!is.na(n0)) {
      n0 = n0 %>%
        # Replace NO-BREAK SPACE unicode
        gsub("\u00a0", "", .)
      # Translate to ASCII and clean CASRN formatting
      n1 = iconv(n0,from="UTF-8",to="ASCII//TRANSLIT")
      n2 = stringi::stri_escape_unicode(n1) %>%
        fix.casrn()
      cs = cas_checkSum(n2)
      if(is.na(cs)) cs = 0
      if(verbose) cat("2>>> ",n0,n1,n2,cs,"\n")
      if(!cs & cs!=0) n2 = NA
      return(paste(n0, n1, n2, cs, sep="||"))
    } else {
      return(paste(NA, NA, NA, NA, sep="||"))
    }
  }

  res0 <- res0 %>%
    dplyr::rowwise() %>%
    dplyr::mutate(cas_check = chem.check.casrn(in_cas=casrn,
                                               verbose=verbose)) %>%
    dplyr::ungroup() %>%
    tidyr::separate(cas_check,
                    into=c("n0", "n1", "n2", "cs"),
                    sep="\\|\\|")

  ccheck_cas = res0 %>%
    dplyr::filter(n2 != n0) %>%
    dplyr::select(n0, n1, n2, cs, name_comment)

  if(nrow(ccheck_cas)) {
    casrn.OK = FALSE
    if(any(ccheck_cas$cs == 0)){
      checksum.OK = FALSE
      cat("bad checksum present\n")
    }
  }

  # Set name as cleaned n2, remove intermediates
  res0 = res0 %>%
    dplyr::select(-casrn, -n0, -n1, -cs) %>%
    dplyr::rename(casrn = n2)

  # Prep check export
  ccheck = rbind(ccheck_name,
                 ccheck_cas) %>%
    dplyr::rename(original=n0,
                  escaped=n1,
                  cleaned=n2,
                  checksum=cs,
                  comment=name_comment) %>%
    distinct()
  ccheck$cleaned <- as.character(ccheck$cleaned)
  ccheck$comment <- as.character(ccheck$comment)
  ccheck$checksum <- as.character(ccheck$checksum)

  indir = paste0(toxval.config()$datapath,"chemcheck/")
  if(is.null(source)) {
    file = paste0(indir,"chemcheck no source.xlsx")
  } else {
    file = paste0(indir,"chemcheck ",source,".xlsx")
  }
  if(!is.null(ccheck)) if(nrow(ccheck)>0) writexl::write_xlsx(ccheck,file)

  if(!name.OK) { cat("Some names fixed\n") } else { cat("All names OK\n") }
  if(!casrn.OK) { cat("Some casrn fixed\n") } else { cat("All casrn OK\n") }
  if(!checksum.OK) { cat("Some casrn have bad checksums\n") } else { cat("All checksums OK\n") }
  return(list(res0=res0,name.OK=name.OK,casrn.OK=casrn.OK,checksum.OK=checksum.OK))
}
