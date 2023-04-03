#' @title fix_numeric_units_split
#' @description Generic function to split an input column into value and units columns.
#' @param df Input DataFrame
#' @param to_split String of the field containing the values to split, Default: ''
#' @param value_to String of the name of the field for the split values, Default: 'value'
#' @param units_to String of the name of the field for the split units, Default: 'units'
#' @return DataFrame with the split columns based on the input parameters
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[dplyr]{mutate}}, \code{\link[dplyr]{context}}, \code{\link[dplyr]{filter}}, \code{\link[dplyr]{select}}, \code{\link[dplyr]{across}}, \code{\link[dplyr]{arrange}}
#'  \code{\link[stringr]{str_trim}}, \code{\link[stringr]{str_extract}}, \code{\link[stringr]{str_replace}}
#'  \code{\link[rlang]{sym}}
#'  \code{\link[tidyr]{separate}}
#' @rdname fix_numeric_unit_split
#' @export
#' @importFrom dplyr mutate n filter select across arrange
#' @importFrom stringr str_squish str_extract str_replace_all
#' @importFrom rlang sym
#' @importFrom tidyr separate
fix_numeric_units_split <- function(df, to_split="", value_to="value", units_to="units"){
  split_check = nrow(df)
  # Quick normalization
  df = df %>%
    ungroup() %>%
    dplyr::mutate(temp_id = 1:n(),
                  raw_in = stringr::str_squish(tolower(!!rlang::sym(to_split))))

  # Filter to rows without a duration
  out = df %>%
    dplyr::filter(is.na(!!rlang::sym(to_split))) %>%
    dplyr::mutate(!!value_to := NA,
                  !!units_to := NA) %>%
    # dplyr::select(!all_of(to_split)) %>%
    select(-raw_in)

  # Filter out matches
  df = df %>% dplyr::filter(!temp_id %in% out$temp_id)
  # Case of integers with units, optional end in "." (e.g. 13 weeks.)
  out = df %>%
    # https://stackoverflow.com/questions/12117024/decimal-number-regular-expression-where-digit-after-decimal-is-optional
    dplyr::filter(grepl("^[0-9]+\\s+[A-Za-z|.]+$", raw_in)) %>%
    tidyr::separate(raw_in, c(value_to, units_to), sep="\\s", extra="merge") %>%
    dplyr::mutate(dplyr::across(c(value_to, units_to), ~stringr::str_squish(tolower(.)))) %>%
    rbind(out, .)
  # Filter out matches
  df = df %>% dplyr::filter(!temp_id %in% out$temp_id)
  # Case of decimal numbers (ex. 7.5 years)
  out = df %>%
    # https://stackoverflow.com/questions/12117024/decimal-number-regular-expression-where-digit-after-decimal-is-optional
    dplyr::filter(grepl("^[0-9]+\\.?[0-9]+\\s+[A-Za-z|.]+$", raw_in)) %>%
    tidyr::separate(raw_in, c(value_to, units_to), sep="\\s", extra="merge") %>%
    dplyr::mutate(dplyr::across(c(value_to, units_to), ~stringr::str_squish(tolower(.)))) %>%
    rbind(out, .)
  # Filter out matches
  df = df %>% dplyr::filter(!temp_id %in% out$temp_id)
  # Case like 14-days
  out = df %>%
    dplyr::filter(grepl("^[0-9]+-[A-Za-z]+$", raw_in)) %>%
    tidyr::separate(raw_in, c(value_to, units_to), sep="-", extra="merge") %>%
    dplyr::mutate(dplyr::across(c(value_to, units_to), ~stringr::str_squish(.))) %>%
    rbind(out, .)
  # Filter out matches
  df = df %>% dplyr::filter(!temp_id %in% out$temp_id)
  # Case like 90 to 94 days
  out = df %>%
    dplyr::filter(grepl("^[0-9]+\\s+to\\s+[0-9]+\\s+[A-Za-z]+$", raw_in)) %>%
    dplyr::mutate(!!value_to := stringr::str_extract(raw_in, "^[0-9]+\\s+to\\s+[0-9]+\\s+") %>%
                    gsub("to", "-", .),
                  !!units_to := gsub("to", "", raw_in) %>%
                    stringr::str_replace_all("[:digit:]+", "")) %>%
    dplyr::mutate(dplyr::across(c(value_to, units_to), ~stringr::str_squish(tolower(.)))) %>%
    dplyr::select(-raw_in) %>%
    rbind(out, .)
  # Filter out matches
  df = df %>% dplyr::filter(!temp_id %in% out$temp_id)
  # TODO Case like 7 days or 28 days; 40 days to 55 days
  out = df %>%
    dplyr::filter(grepl("^[0-9]+\\s[A-Za-z]+\\s[or|to]+\\s[0-9]+\\s[A-Za-z]+$", raw_in)) %>%
    # Uncertain how to handle, setting aside for now
    dplyr::mutate(!!value_to := raw_in,
           !!units_to := raw_in) %>%
    dplyr::select(-raw_in) %>%
    rbind(out, .)
  # Filter out matches
  df = df %>% dplyr::filter(!temp_id %in% out$temp_id)
  # Case like 90 or 91 days
  out = df %>%
    dplyr::filter(grepl("^[0-9]+\\s[or]+\\s[0-9]+\\s[A-Za-z]+$", raw_in)) %>%
    # Uncertain how to handle, setting aside for now
    dplyr::mutate(!!value_to := stringr::str_extract(raw_in, "^[0-9]+\\s[or]+\\s[0-9]+"),
                  !!units_to := gsub("or", "", raw_in) %>%
                    stringr::str_replace_all("[:digit:]+", "")) %>%
    dplyr::mutate(dplyr::across(c(value_to, units_to), ~stringr::str_squish(tolower(.)))) %>%
    dplyr::select(-raw_in) %>%
    rbind(out, .)
  # Filter out matches
  df = df %>% dplyr::filter(!temp_id %in% out$temp_id)
  # Case like At least 90 days
  out = df %>%
    dplyr::filter(grepl("^[Aa]t least [0-9]+ [A-Za-z]+$", raw_in)) %>%
    dplyr::mutate(raw_in = gsub("at least", "", raw_in, ignore.case=TRUE) %>%
             stringr::str_squish(.)) %>%
    tidyr::separate(raw_in, c(value_to, units_to), sep="\\s", extra="merge") %>%
    dplyr::mutate(dplyr::across(c(value_to, units_to), ~stringr::str_squish(tolower(.)))) %>%
    rbind(out, .)
  # Filter out matches
  df = df %>% dplyr::filter(!temp_id %in% out$temp_id)
  # Case like - 12 weeks
  out = df %>%
    dplyr::filter(grepl("^- [0-9]+ [A-Za-z]+$", raw_in)) %>%
    dplyr::mutate(raw_in = gsub("-", "", raw_in, ignore.case=TRUE) %>%
             stringr::str_squish(.)) %>%
    tidyr::separate(raw_in, c(value_to, units_to), sep="\\s", extra="merge") %>%
    dplyr::mutate(dplyr::across(c(value_to, units_to), ~stringr::str_squish(tolower(.)))) %>%
    rbind(out, .)
  # Filter out matches
  df = df %>% dplyr::filter(!temp_id %in% out$temp_id)
  # TODO Case - Males: 14 days ... - Females: 14 days
  out = df %>%
    dplyr::filter(grepl("^- Males: [0-9]+ [A-Za-z]+|- Females: [0-9]+ [A-Za-z]+", raw_in)) %>%
    dplyr::mutate(!!value_to := raw_in,
           !!units_to := raw_in) %>%
    dplyr::select(-raw_in) %>%
    rbind(out, .)
  # Filter out matches
  df = df %>% dplyr::filter(!temp_id %in% out$temp_id)
  # Case like "Up to 10 months"
  out = df %>%
    dplyr::filter(grepl("^[Uu]p to [0-9]+\\.?[0-9]* [A-Za-z|.]+$", raw_in)) %>%
    dplyr::mutate(raw_in = gsub("up to", "", raw_in, ignore.case=TRUE) %>%
             stringr::str_squish(.)) %>%
    tidyr::separate(raw_in, c(value_to, units_to), sep="\\s", extra="merge") %>%
    dplyr::mutate(dplyr::across(c(value_to, units_to), ~stringr::str_squish(tolower(.)))) %>%
    rbind(out, .)
  # Filter out matches
  df = df %>% dplyr::filter(!temp_id %in% out$temp_id)
  # TODO Case 10-11 days; 104-105 weeks; 100 - 110 days
  out = df %>%
    dplyr::filter(grepl("^[0-9]+-[0-9]+ [A-Za-z|.]+$|^[0-9]+ - [0-9]+ [A-Za-z|.]+$", raw_in)) %>%
    dplyr::mutate(raw_in = gsub(" - ", "-", raw_in)) %>%
    tidyr::separate(raw_in, c(value_to, units_to), sep="\\s", extra="merge") %>%
    dplyr::mutate(dplyr::across(c(value_to, units_to), ~stringr::str_squish(.))) %>%
    rbind(out, .)
  # Filter out matches
  df = df %>% dplyr::filter(!temp_id %in% out$temp_id)
  # Case > 600 d
  out = df %>%
    dplyr::filter(grepl("^>[0-9]+|^> [0-9]+", raw_in)) %>%
    dplyr::mutate(raw_in = gsub(">", "", raw_in) %>%
             stringr::str_squish(.)) %>%
    tidyr::separate(raw_in, c(value_to, units_to), sep="\\s", extra="merge") %>%
    dplyr::mutate(dplyr::across(c(value_to, units_to), ~stringr::str_squish(.))) %>%
    rbind(out, .)
  # Filter out matches
  df = df %>% dplyr::filter(!temp_id %in% out$temp_id)
  # Case 102 weeks (2 years)
  out = df %>%
    dplyr::filter(grepl("^[0-9]+ [A-Za-z|.]+ \\([0-9]+ [A-Za-z]+\\)$", raw_in)) %>%
    dplyr::mutate(raw_in = stringr::str_extract(raw_in, "^[0-9]+ [A-Za-z|.]+ \\([0-9]+ [A-Za-z]+\\)") %>%
             sub('\\(.*', '', .)) %>%
    tidyr::separate(raw_in, c(value_to, units_to), sep="\\s", extra="merge") %>%
    dplyr::mutate(dplyr::across(c(value_to, units_to), ~stringr::str_squish(.))) %>%
    rbind(out, .)
  # Filter out matches
  df = df %>% dplyr::filter(!temp_id %in% out$temp_id)
  # TODO Case of spelled out numbers (e.g., twenty weeks)
  # Generate spelled out number lists to check
  numbers = list(base = c("one", "two", "three", "four", "five", "six", "seven", "eight", "nine"),
                 teens = c("ten", "eleven", "twelve", "thirteen", "fourteen", "fifteen", "sixteen", "seventeen", "eighteen", "nineteen"),
                 tens = c("twenty", "thirty", "forty", "fifty", "sixty", "seventy", "eighty", "ninety"))

  numbers = c(numbers %>% c(),
              sapply(numbers$tens, function(n){ paste0(n, "", numbers$base)}, USE.NAMES = FALSE) %>% c(),
              sapply(numbers$tens, function(n){ paste0(n, "-", numbers$base)}, USE.NAMES = FALSE) %>% c(),
              sapply(numbers$tens, function(n){ paste0(n, " ", numbers$base)}, USE.NAMES = FALSE) %>% c()
  ) %>% unlist()

  # Case "10weeks"
  out = df %>%
    dplyr::filter(grepl("^[0-9]+weeks$", raw_in)) %>%
    dplyr::mutate(raw_in = gsub("w", " weeks", raw_in)) %>%
    tidyr::separate(raw_in, c(value_to, units_to), sep="\\s", extra="merge") %>%
    dplyr::mutate(dplyr::across(c(value_to, units_to), ~stringr::str_squish(.))) %>%
    rbind(out, .)
  # Filter out matches
  df = df %>% dplyr::filter(!temp_id %in% out$temp_id)

  # Case "6wk"
  out = df %>%
    dplyr::filter(grepl("^[0-9]+wk$", raw_in)) %>%
    dplyr::mutate(raw_in = gsub("wk", " weeks", raw_in)) %>%
    tidyr::separate(raw_in, c(value_to, units_to), sep="\\s", extra="merge") %>%
    dplyr::mutate(dplyr::across(c(value_to, units_to), ~stringr::str_squish(.))) %>%
    rbind(out, .)
  # Filter out matches
  df = df %>% dplyr::filter(!temp_id %in% out$temp_id)

  # Case "21d)" (too specific?)
  out = df %>%
    dplyr::filter(grepl("^[0-9]+d\\)$", raw_in)) %>%
    dplyr::mutate(raw_in = gsub("d\\)", " days", raw_in)) %>%
    tidyr::separate(raw_in, c(value_to, units_to), sep="\\s", extra="merge") %>%
    dplyr::mutate(dplyr::across(c(value_to, units_to), ~stringr::str_squish(.))) %>%
    rbind(out, .)
  # Filter out matches
  df = df %>% dplyr::filter(!temp_id %in% out$temp_id)

  # Append units to cases like "gd(s)|gestation day(s) 1(-21)"
  out = df %>%
    dplyr::filter(grepl("^((reproductive: )?gds? ?|gestation days?) ?[0-9]+-?[0-9]*$",
                        raw_in, ignore.case = TRUE)) %>%
    dplyr::mutate(raw_in = gsub("$", "_gestational days", raw_in)) %>%
    tidyr::separate(raw_in, c(value_to, units_to), sep="_", extra="merge") %>%
    dplyr::mutate(dplyr::across(c(value_to, units_to), ~stringr::str_squish(.))) %>%
    rbind(out, .)
  # Filter out matches
  df = df %>% dplyr::filter(!temp_id %in% out$temp_id)

  # Append units to cases like "pnds 5-11"
  out = df %>%
    dplyr::filter(grepl("^pnds? [0-9]+ ?- ?[0-9]*$", raw_in)) %>%
    dplyr::mutate(raw_in = gsub("$", "_postnatal days", raw_in)) %>%
    tidyr::separate(raw_in, c(value_to, units_to), sep="_", extra="merge") %>%
    dplyr::mutate(dplyr::across(c(value_to, units_to), ~stringr::str_squish(.))) %>%
    rbind(out, .)
  # Filter out matches
  df = df %>% dplyr::filter(!temp_id %in% out$temp_id)

  # Case "X, Y, or Z weeks"
  out = df %>%
    dplyr::filter(grepl("^[0-9]-[0-9]+, [0-9]+, or [0-9]+ weeks", raw_in)) %>%
    dplyr::mutate(raw_in = gsub(" weeks", "_weeks", raw_in)) %>%
    tidyr::separate(raw_in, c(value_to, units_to), sep="_", extra="merge") %>%
    dplyr::mutate(dplyr::across(c(value_to, units_to), ~stringr::str_squish(.))) %>%
    rbind(out, .)
  # Filter out matches
  df = df %>% dplyr::filter(!temp_id %in% out$temp_id)
  # Need to refine the splitting of the column
  tmp = df %>%
    dplyr::filter(grepl(paste0("^", numbers, " [A-Za-z|.]+$", collapse="|"),
                 raw_in)) %>%
    dplyr::mutate(!!value_to := raw_in,
           !!units_to := stringr::str_extract(raw_in, "[A-Za-z|.]+$"))
  # Remove units from values (order by character count so smaller abbreviations don't mess up other replacements)
  tmp_reg = unique(tmp[[units_to]])
  tmp[[value_to]] = gsub(paste0(tmp_reg[order(nchar(tmp_reg), tmp_reg, decreasing = TRUE)], collapse="|"),
                                  "",
                                  tmp[[value_to]]) %>%
    stringr::str_squish()
  out = tmp %>%
    # https://rdrr.io/cran/doseminer/man/words2number.html
    # TODO Convert spelled out numbers to numerics
    dplyr::mutate(dplyr::across(c(value_to, units_to), ~stringr::str_squish(.))) %>%
    dplyr::select(-raw_in) %>%
    rbind(out, .)
  # Filter out matches
  df = df %>% dplyr::filter(!temp_id %in% out$temp_id)

  # Helper to see what's left to fix
  # df %>% select(raw_in) %>% unique() %>% View()

  # Percent coverage
  # round(nrow(out) / (nrow(out) + nrow(df)) * 100, 3)

  # Return processed fields
  out = df %>%
    dplyr::mutate(!!value_to :=raw_in,
           !!units_to :=raw_in) %>%
    dplyr::select(-raw_in) %>%
    # Remove extraneous whitespace
    dplyr::mutate(dplyr::across(c(value_to, units_to), ~stringr::str_squish(.))) %>%
    rbind(out, .) %>%
    dplyr::arrange(temp_id) %>%
    dplyr::select(-temp_id)

  if(nrow(out) != split_check){
    stop("Error...extra rows introduced...")
  }

  return(out)
}
