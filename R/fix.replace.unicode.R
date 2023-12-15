#--------------------------------------------------------------------------------------
#' @title fix.replace.unicode
#' @description A function to check all character fields and handle unicode symbols,
#' either by removing them or replacing them with alphabetic equivalents.
#' @return Returns a modified version of the input vector with unicode replacements.
#' @export
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname fix.replace.unicode
#' @param df Character vector to check/replace unicode symbols.
#--------------------------------------------------------------------------------------
fix.replace.unicode <- function(df) {
  if(!is.character(df)){
    cat("fix.replace.unicode input must be character vector...\n")
    return()
  }

  # Generate unicode lists
  # TODO Add more cases to handle
  # USE stringi::stri_escape_unicode("") to assist with identifying new encoding

  ################################################################################
  ############################# HANDLE GREEK SYMBOLS #############################
  ################################################################################

  # https://en.wikipedia.org/wiki/Greek_script_in_Unicode
  alpha = c("<U+1D6C2>", "<U+1D6FC>", "<U+1D736>", "<U+1D770>", "<U+1D7AA>",
            "\u03B1", "\u1D6C2", "\u1D6FC", "\u1D736", "\u1D770", "\u1D7AA", "<U+03B1>")

  beta = c("<U+03B2>", "<U+03D0>", "<U+1D6C3>", "<U+1D6FD>", "<U+1D737>", "<U+1D771>", "<U+1D7AB>",
           "<U+0392>", "<U+1D6A9>", "<U+1D6E3>", "<U+1D71D>", "<U+1D757>", "<U+1D791>", "<U+1D5D>",
           "<U+1D66>", "<U+00df?",
           "\u03B2", "\u03D0", "\u1D6C3", "\u1D6FD", "\u1D737", "\u1D771", "\u1D7AB",
           "\u0392", "\u1D6A9", "\u1D6E3", "\u1D71D", "\u1D757", "\u1D791", "\u1D5D",
           "\u1D66", "\u00df")

  gamma = c("<U+03B3>", "<U+213D>", "<U+1D6C4>", "<U+1D6FE>", "<U+1D738>", "<U+1D772>", "<U+1D7AC>",
            "<U+0393>", "<U+213E>", "<U+1D6AA>", "<U+1D6E4>", "<U+1D71E>", "<U+1D758>", "<U+1D792>",
            "<U+1D5E>", "<U+1D67>", "<U+1D26>", "<U+0263>",
            "\u03B3", "\u213D", "\u1D6C4", "\u1D6FE", "\u1D738", "\u1D772", "\u1D7AC",
            "\u0393", "\u213E", "\u1D6AA", "\u1D6E4", "\u1D71E", "\u1D758", "\u1D792",
            "\u1D5E", "\u1D67", "\u1D26", "\u0263")

  mu = c("<U+03BC>", "<U+00B5>", "<U+1D6CD>", "<U+1D707>", "<U+1D741>", "<U+1D77B>", "<U+1D7B5>",
         "\u03BC", "\u00B5", "\u1D6CD", "\u1D707", "\u1D741", "\u1D77B", "\u1D7B5")

  epsilon = c("<U+03B5>", "<U+03AD>", "<U+03F5>", "<U+1F10>", "<U+1F11>", "<U+1F72>",
              "<U+1D6C6>", "<U+1D700>", "<U+1D73A>", "<U+1D774>", "<U+1D7AE>", "<U+0395>",
              "<U+03F5>", "<U+1D6DC>", "<U+1D716>", "<U+1D750>", "<U+1D78A>", "<U+1D7C4>",
              "<U+1D6AC>", "<U+1D6E6>", "<U+1D720>", "<U+1D75A>", "<U+1D794>",
              "\u03b5", "\u03ad", "\u03f5", "\u1f10", "\u1f11", "\u1f72",
              "\u1d6c6", "\u1d700", "\u1d73a", "\u1d774", "\u1d7ae", "\u0395",
              "\u03f5", "\u1d6dc", "\u1d716", "\u1d750", "\u1d78a", "\u1d7c4",
              "\u1d6ac", "\u1d6e6", "\u1d720", "\u1d75a", "\u1d794")

  apostrophe_epsilon = c("<U+0388>", "<U+1F18>", "<U+1F19>", "<U+1FC8>",
                         "\u0388", "\u1f18", "\u1f19", "\u1fc8")

  lambda = c("<U+03BB>", "<U+1D6CC>", "<U+1D706>", "<U+1D740>", "<U+1D77A>", "<U+1D7B4>",
             "<U+039B>", "<U+1D6B2>", "<U+1D6EC>", "<U+1D726>", "<U+1D760>", "<U+1D79A>",
             "\u03bb", "\u1d6cc", "\u1d706", "\u1d740", "\u1d77a", "\u1d7b4",
             "\u039b", "\u1d6b2", "\u1d6ec", "\u1d726", "\u1d760", "\u1d79a")

  kappa = c("<U+03BA>", "<U+03F0>", "<U+1D6CB>", "<U+1D705>", "<U+1D73F>", "<U+1D779>", "<U+1D7B3>",
            "<U+039A>", "<U+1D6B1>", "<U+1D6EB>", "<U+1D725>", "<U+1D75F>", "<U+1D799>",
            "\u03ba", "\u03f0", "\u1d6cb", "\u1d705", "\u1d73f", "\u1d779", "\u1d7b3",
            "\u039a", "\u1d6b1", "\u1d6eb", "\u1d725", "\u1d75f", "\u1d799")

  omega = c("<U+03C9>", "<U+03CE>", "<U+1F60>", "<U+1F61>", "<U+1F7C>", "<U+1FF3>", "<U+1FF6>",
            "<U+1D6DA>", "<U+1D714>", "<U+1D788>", "<U+1D7C2>", "<U+03A9>", "<U+1FFC>",
            "<U+2126>", "<U+1D6C0>", "<U+1D6FA>", "<U+1D734>", "<U+1D76E>", "<U+1D7A8>",
            "\u03c9", "\u03ce", "\u1f60", "\u1f61", "\u1f7c", "\u1ff3", "\u1ff6",
            "\u1d6da", "\u1d714", "\u1d788", "\u1d7c2", "\u03a9", "\u1ffc",
            "\u2126", "\u1d6co", "\u1d6fa", "\u1d734", "\u1d76e", "\u1d7a8")

  apostrophe_omega = c("<U+038F>", "<U+1F68>", "<U+1F69>", "<U+1FFA>",
                       "\u038f", "\u1f68", "\u1f69", "\u1ffa")

  eta = c("<U+03B7>", "<U+03AE>", "<U+1F20>", "<U+1F21>", "<U+1F74>", "<U+1FC3>", "<U+1FC6>",
          "<U+1D6C8>", "<U+1D702>", "<U+1D73C>", "<U+1D776>", "<U+1D7B0>", "<U+0397>",
          "<U+1FCC>", "<U+1D6AE>", "<U+1D6E8>", "<U+1D722>", "<U+1D75C>", "<U+1D796>",
          "\u03b7", "\u03ae", "\u1f20", "\u1f21", "\u1f74", "\u1fc3", "\u1fc6",
          "\u1d6c8", "\u1d702", "\u1d73c", "\u1d776", "\u1d7bo", "\u0397",
          "\u1fcc", "\u1d6ae", "\u1d6e8", "\u1d722", "\u1d75c", "\u1d796")

  apostrophe_eta = c("<U+0389>", "<U+1F28>", "<U+1F29>", "<U+1FCA>",
                     "\u0389", "\u1f28", "\u1f29", "\u1fca")

  rho = c("<U+03C1>", "<U+03F1>", "<U+1D68>", "<U+1FE4>", "<U+1FE5>", "<U+1D6D2>",
          "<U+1D70C>", "<U+1D746>", "<U+1D780>", "<U+1D7BA>", "<U+03A1>", "<U+1D6B8>",
          "<U+1D6F2>", "<U+1D72C>", "<U+1D766>", "<U+1D7A0>",
          "\u03c1", "\u03f1", "\u1d68", "\u1fe4", "\u1fe5", "\u1d6d2",
          "\1d70", "\1d746", "\u1d780", "\u1d7ba", "\u03a1", "\u1d6b8",
          "\u1d6f2", "\u1d72c", "\u1d766", "\u1d7ao")

  apostrophe_rho = c("<U+1FEC>", "\u1fec")

  sigma = c("<U+03C3>", "<U+1D6D4>", "<U+1D70E>", "<U+1D748>", "<U+1D782>", "<U+1D7BC>", "<U+03A3>",
            "<U+03F9>", "<U+1D6BA>", "<U+1D6F4>", "<U+1D72E>", "<U+1D768>", "<U+1D7A2>",
            "\u03c3", "\u1d6d4", "\u1d70e", "\u1d748", "\u1d782", "\u1d7bc", "\u03a3",
            "\u03f9", "\u1d6ba", "\u1d6f4", "\u1d72e", "\u1d768", "\u1d7a2")

  delta = c("<U+03B4>", "<U+0394>", "<U+1D6AB>", "<U+1D6E5>", "<U+1D71F>", "<U+1D759>", "<U+1D793>",
            "<U+1D5F>", "<U+1D6C5>", "<U+1D6FF>", "<U+1D739>", "<U+1D773>", "<U+1D7AD>",
            "\u03b4", "\u0394", "\u1d6ab", "\u1d6e5", "\u1d71f", "\u1d759", "\u1d793",
            "\u1d5f", "\u1d6c5", "\u1d6ff", "\u1d739", "\u1d773", "\u1d7ad")

  df = df %>%
    # Replacements from: https://www.rapidtables.com/math/symbols/greek_alphabet.html
    gsub(paste0(alpha, collapse="|"), "a", ., ignore.case=TRUE) %>%
    gsub(paste0(beta, collapse="|"), "b", ., ignore.case=TRUE) %>%
    gsub(paste0(gamma, collapse="|"), "g", ., ignore.case=TRUE) %>%
    gsub(paste0(epsilon, collapse="|"), "e", ., ignore.case=TRUE) %>%
    gsub(paste0(apostrophe_epsilon, collapse="|"), "'e", ., ignore.case=TRUE) %>%
    gsub(paste0(lambda, collapse="|"), "l", ., ignore.case=TRUE) %>%
    gsub(paste0(kappa, collapse="|"), "k", ., ignore.case=TRUE) %>%
    gsub(paste0(omega, collapse="|"), "o", ., ignore.case=TRUE) %>%
    gsub(paste0(apostrophe_omega, collapse="|"), "'o", ., ignore.case=TRUE) %>%
    gsub(paste0(eta, collapse="|"), "h", ., ignore.case=TRUE) %>%
    gsub(paste0(apostrophe_eta, collapse="|"), "'h", ., ignore.case=TRUE) %>%
    gsub(paste0(delta, collapse="|"), "'d", ., ignore.case=TRUE) %>%

    # Fix omega with preceding letter
    gsub("<U+33C0>|\u33c0", "KO", ., ignore.case=TRUE) %>%
    gsub("<U+33C1>|\u33c1", "MO", ., ignore.case=TRUE) %>%

    # For "micro" units like micrograms (ug)
    gsub(paste0(mu, collapse="|"), "u", ., ignore.case=TRUE) %>%
    gsub(paste0(rho, collapse="|"), "r", ., ignore.case=TRUE) %>%
    gsub(paste0(apostrophe_rho, collapse="|"), "'r", ., ignore.case=TRUE) %>%
    gsub(paste0(sigma, collapse="|"), "s", ., ignore.case=TRUE) %>%

    ##############################################################################
    ############################ HANDLE OTHER SYMBOLS ############################
    ##############################################################################

    # Remove trademark/copyright symbols
    gsub("\u00ae|<U+00ae>|\u00a9|\u2122", "", ., ignore.case=TRUE) %>%

    # Fix whitespace
    gsub("[\r\n][\r\n]", " ", ., ignore.case=TRUE) %>%
    gsub("\u00a0|<U+00A0>", " ", ., ignore.case=TRUE) %>%

    # Fix escaped quotation marks
    gsub("\\\\{1,}'", "'", ., ignore.case=TRUE) %>%
    gsub('\\\\{1,}"', '"', ., ignore.case=TRUE) %>%

    # Remove double-dagger symbols
    gsub("\u2021|<U+2021>", "", ., ignore.case=TRUE) %>%

    # Replace prime symbols
    gsub("\u00b4|<U+00B4>|\u2018|<U+2018>|\u0092|<U+0092>|\u2019|<U+2019>|\u2032", "'", .,
         ignore.case=TRUE) %>%

    # Handle special case for micro sign
    gsub("\u00c2\u00b5|\u00c2u", "u", ., ignore.case=TRUE) %>%

    # Remove euro/pound currency symbol unicode
    gsub("\u20ac|u00a3", "", ., ignore.case=TRUE) %>%
    # Replace LATIN CAPITAL LETTER Z WITH CARON
    gsub("\u017d|<U+017D>", "Z", ., ignore.case = TRUE) %>%
    # Replace Latin small letter a with circumflex
    gsub("\u00e2|<U+00E2>", "a", ., ignore.case = TRUE) %>%

    # Handle special case for Alpha
    gsub("\u00ce\u00b1", "a", ., ignore.case=TRUE) %>%

    # Handle dashes
    gsub("\u2013|\u2014|\u2212|\u2010", "-", ., ignore.case=TRUE) %>%

    # Fix quotations and apostrophes
    gsub("\u201c|<U+201C>|\u201d|<U+201D>", '"', ., ignore.case=TRUE) %>%
    gsub("\u2018|<U+2018>|\u0092|<U+0092>|\u2019|<U+2019>", "'", ., ignore.case=TRUE) %>%

    # Fix superscript/subscript
    gsub("\u00b3|<U+00B3>", "3", ., ignore.case=TRUE) %>%
    gsub("\u00b9|<U+00B9>", "1", ., ignore.case=TRUE) %>%
    gsub("\u2070|<U+2070>", "0", ., ignore.case=TRUE) %>%
    gsub("\u00b2|<U+00B2>", "2", ., ignore.case=TRUE) %>%
    gsub("\u2079|<U+2079>", "9", ., ignore.case=TRUE) %>%
    gsub("\u2078|<U+2078>", "8", ., ignore.case=TRUE) %>%
    gsub("\u2074|<U+2074>", "4", ., ignore.case=TRUE) %>%
    gsub("\u2077|<U+2077>", "7", ., ignore.case=TRUE) %>%
    gsub("\u2076|<U+2076>", "6", ., ignore.case=TRUE) %>%

    # Fix general punctuation
    gsub("\u00b4|<U+00B4>", "'", ., ignore.case=TRUE) %>%
    gsub("\u00bf|<U+00BF>", "?", ., ignore.case=TRUE) %>%
    gsub("\u2026|<U+2026>", "...", ., ignore.case=TRUE) %>%
    gsub("\u02c6", "^", ., ignore.case=TRUE) %>%

    # Fix math symbols
    gsub("\u2265|<U+2265>", ">=", ., ignore.case=TRUE) %>%
    gsub("\u2264|<U+2264>", "<=", ., ignore.case=TRUE) %>%
    gsub("\u00b1|<U+00B1>", "+/-", ., ignore.case=TRUE) %>%
    gsub("\u00b0|<U+00B0>", "", ., ignore.case=TRUE) %>%
    gsub("\u00b0|<U+00B0>", "", ., ignore.case=TRUE) %>%
    gsub("\u2032|<U+2032>", "", ., ignore.case=TRUE) %>%
    gsub("\u00b7|<U+00B7>|\u00d7|<U+00D7>", "*", ., ignore.case=TRUE) %>%
    gsub("\u2030|<U+2030>", "%o", ., ignore.case=TRUE) %>%

    # Fix parentheses and brackets
    gsub("\uff08", "(", ., ignore.case=TRUE) %>%
    gsub("\uff09", ")", ., ignore.case=TRUE) %>%
    gsub("\uff3d", "]", ., ignore.case=TRUE) %>%
    gsub("\uff3b", "[", ., ignore.case=TRUE) %>%

    # Fix roman numeral 2
    gsub("\u2161","II", ., ignore.case=TRUE) %>%

    # Fix fullwidth latin small letter o
    gsub("\uff4f", "o", ., ignore.case=TRUE) %>%

    # Fix fullwidth digit 1
    gsub("\uff11", 1, ., ignore.case=TRUE) %>%

    # Fix fullwidth digit 3
    gsub("\uff13", 3, ., ignore.case=TRUE) %>%

    # Fix fullwidth digit 7
    gsub("\uff17", 7, ., ignore.case=TRUE) %>%

    # Fix miscellaneous letters
    gsub("\u0067", "g", ., ignore.case=TRUE) %>%
    gsub("\u00fc", "u", ., ignore.case=TRUE) %>%
    gsub("\u00a5", "y", ., ignore.case=TRUE) %>%
    gsub("\u00c2|\u00c3", "A", ., ignore.case=TRUE) %>%
    gsub("\u00ba|\u00f6|\u00f8", "o", ., ignore.case=TRUE) %>%
    gsub("\u00e9", "e", ., ignore.case=TRUE) %>%
    gsub("\u00ce", "I", ., ignore.case=TRUE) %>%
    gsub("\u00e7", "c", ., ignore.case=TRUE) %>%
    gsub("\u00e2|\u00e4", "a", ., ignore.case=TRUE) %>%
    gsub("\ufb02", "fl", ., ignore.case=TRUE) %>%

    # Remove unidentified characters
    gsub("\ufffd", "", ., ignore.case=TRUE)

  # Identify and print unicode symbols that were not handled
  not_handled = df %>%
    # Remove NA values
    na.omit() %>%

    # Escape all text
    stringi::stri_escape_unicode() %>%

    # Search for unicode symbols
    stringr::str_extract_all(., "\\\\[uU][a-zA-Z0-9]{4,8}") %>%

    # Remove nested lists
    purrr::list_c() %>%

    # Make characters lowercase
    tolower() %>%

    # Keep only unique symbols
    unique() %>%

    # Drop empty match and NA
    .[. != "character(0)"] %>%
    .[. != "NA"]

  # Print output related to unicode symbols that were not handled
  if (length(not_handled) > 0) {
    cat(paste0("\n", length(not_handled), " unicode symbols were not handled:\n    - "))
    cat(paste0(not_handled, collapse="\n    - "))
    cat("\n\nUpdate fix.replace.unicode to handle these cases.\n\n")
    # browser()
  }

  return(df)
}
