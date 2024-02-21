#--------------------------------------------------------------------------------------
#' Check the chemicals from a file
#' Names with special characters are cleaned and trimmed
#' CASRN are fixed (dashes put in, trimmed) and check sums are calculated
#' The output is sent to a file called chemcheck.xlsx in the source data file
#' One option for using this is to edit the source file until no errors are found
#'
#' @param res0  The data frame in which chemicals names and CASRN will be replaced
#' @param source The source to be processed. If source=NULL, process all sources
#' @param verbose If TRUE, print diagnostic messages
#' @return Return a list with fixed CASRN and name and flags indicating if fixes were made:
#' res0=res0,name.OK=name.OK,casrn.OK=casrn.OK,checksum.OK=checksum.OK
#'
#--------------------------------------------------------------------------------------
chem.check.v2 <- function(res0,source=NULL,verbose=FALSE) {
  printCurrentFunction(source)
  name.OK = TRUE
  casrn.OK = TRUE
  checksum.OK = TRUE

  query = "select casrn, name from source_fda_cedi"
  res = runQuery(query,source.db,TRUE,FALSE)
  res0 = res
  res0 = res0 %>%
    tidyr::unite(col="chemical_index", all_of(c("casrn", "name")), sep=" ", remove=FALSE)


  cat(">>> Deal with name\n")
  chem.check.name <- function(in_name, source, verbose){
    n0 = in_name %>%
      # Replace zero width space unicode
      gsub("\u200b", "", .)

    if(is.na(n0)) {
      cat("NA name found...\n")
      browser()
    }
    n1 = n0 %>%
      iconv(.,from="UTF-8",to="ASCII//TRANSLIT")
    n2 <- n1 %>%
      stringi::stri_escape_unicode() %>%

      stringr::str_replace_all("\\\\'","\'") %>%
      stringr::str_squish()

    if(source %in% c("Alaska DEC",
                     "California DPH",
                     "EPA AEGL",
                     "Mass. Drinking Water Standards",
                     "OSHA Air contaminants",
                     "OW Drinking Water Standards",
                     "Pennsylvania DEP MCLs",
                     "USGS HBSL",
                     "WHO IPCS",
                     "ATSDR MRLs",
                     "Cal OEHHA",
                     "Chiu",
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

  res0 = res0 %>%
    dplyr::rowwise() %>%
    dplyr::mutate(name_check = chem.check.name(in_name=name,
                                               source=source,
                                               verbose=verbose)) %>%
    dplyr::ungroup() %>%
    tidyr::separate(name_check,
                    into=c("n0", "n1", "n2"),
                    sep="\\|\\|")

  ccheck_name = res0 %>%
    dplyr::filter(n2 != n0) %>%
    dplyr::select(n0, n1, n2) %>%
    dplyr::mutate(cs = NA)

  if(nrow(ccheck_name)) {
    name.OK = FALSE
  }

  # Set name as cleaned n2, remove intermediates
  res0 = res0 %>%
    dplyr::select(-name, -n0, -n1) %>%
    dplyr::rename(name = n2)
  res0$name_comment <- NA
  res2 = res0 %>%
    mutate(corrected = correct_formula(df=res0,col='name',comment='name_comment'))

  # Correct Formula
  correct_formula <- function(df, col='chemical_name',comment='name_comment'){
    df$name_is_formula <- sapply(df[[col]], find_formula)
    idx <- df$name_is_formula
    df[idx, comment] <- apply(df[idx, c(comment, col)], 1, function(x){
      append_col(x=x[comment], s=x[col], comment="Name only formula")
    })
    df[idx, col] <- NA
    df <- df[, !(names(df) %in% 'name_is_formula')]
    return(df)
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
    if (is.character(x)) {
      if (is.character(s)) {
        s <- paste(comment, s, sep=": ")
        y <- paste(trimws(x), trimws(s), sep=sep)
      } else {
        y <- x
      }
    } else if (is.na(x)) {
      if (!is.na(s)) {
        s <- paste(comment, trimws(s), sep=": ")
      }
      y <- s
    } else {
      y <- NA
    }
    return(y)
  }

  # Drop terminal phrases

  # Drop foods
  foods <- function() {
    food <- c('yeast culture', 'food starch', 'sweet whey',
              'salted fish', 'beverage')
    return(paste(food, collapse = "|"))
  }

  drop_foods <- function(df, col='chemical_name', comment='name_comment'){
    df <- df %>%
      mutate(contains_food = str_detect(tolower(!!sym(col)), foods())) %>%
      mutate(!!sym(comment) := ifelse(contains_food, paste0(!!sym(comment), "Name is food"), !!sym(comment))) %>%
      mutate(!!sym(col) := ifelse(contains_food, NA_character_, !!sym(col))) %>%
      select(-contains_food)
    return(df)
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

  drop_stoppers <- function(df, col='chemical_name', comment='name_comment'){
    df <- df %>%
      mutate(contains_stop_word = str_detect(tolower(!!sym(col)), paste(stops(), collapse="|"))) %>%
      mutate(is_ambiguous = contains_stop_word & !str_detect(tolower(!!sym(col)), "yl")) %>%
      mutate(is_polymer = tolower(!!sym(col)) %in% c("polymer", "plymers", "wax", "mixture")) %>%
      mutate(is_citron = str_detect(tolower(!!sym(col)), "citron")) %>%
      mutate(is_compound = str_detect(tolower(!!sym(col)), "compound")) %>%
      mutate(!!sym(comment) := case_when(
        is_ambiguous ~ append_col(!!sym(comment), !!sym(col), "Ambiguous name"),
        is_polymer | is_citron | is_compound ~ append_col(!!sym(comment), !!sym(col), "Ambiguous name"),
        TRUE ~ !!sym(comment))) %>%
      mutate(!!sym(col) := case_when(
        is_ambiguous | is_polymer | is_citron | is_compound ~ NA_character_, TRUE ~ !!sym(col))) %>%
      select(-contains_stop_word, -is_ambiguous, -is_polymer, - is_citron, -is_compound)
    return(df)
  }

  # Drop text

  # Drop salts

  # Drop terminal phrases

  # String cleaning

  cat("\n>>> Deal with CASRN\n")
  chem.check.casrn <- function(in_cas, verbose){
    n0 = in_cas
    if(!is.na(n0)) {
      n1 = iconv(n0,from="UTF-8",to="ASCII//TRANSLIT")
      n2 = stri_escape_unicode(n1) %>%
        fix.casrn()
      cs = cas_checkSum(n2)
      if(is.na(cs)) cs = 0
      if(verbose) cat("2>>> ",n0,n1,n2,cs,"\n")
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
    dplyr::select(n0, n1, n2, cs)

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
                  checksum=cs) %>%
    distinct()

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
