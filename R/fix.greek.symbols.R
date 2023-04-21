#--------------------------------------------------------------------------------------
#' @title fix.greek.symbols
#' @description A function to check all character fields and replace Greek symbols
#' with alphabetic equivalents.
#' @return Returns a dataframe with all character fields edited to replace Greek symbols
#' with alphabetic equivalents.
#' @export 
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname fix.greek.symbols
#' @param df PARAM_DESCRIPTION
#--------------------------------------------------------------------------------------
fix.greek.symbols <- function(df) {

  if(!is.character(df)){
    cat("fix.greek.symbols input must be character vector...\n")
    return()
  }

  # Generate unicode lists
  # TODO Add more cases to handle
  # https://en.wikipedia.org/wiki/Greek_script_in_Unicode
  alpha = c("<U+1D6C2>", "<U+1D6FC>", "<U+1D736>", "<U+1D770>", "<U+1D7AA>",
            "\u03B1", "\u1D6C2", "\u1D6FC", "\u1D736", "\u1D770", "\u1D7AA", "<U+03B1>")

  beta = c("<U+03B2>", "<U+03D0>", "<U+1D6C3>", "<U+1D6FD>", "<U+1D737>", "<U+1D771>", "<U+1D7AB>",
           "<U+0392>", "<U+1D6A9>", "<U+1D6E3>", "<U+1D71D>", "<U+1D757>", "<U+1D791>", "<U+1D5D>",
           "<U+1D66>",
           "\u03B2", "\u03D0", "\u1D6C3", "\u1D6FD", "\u1D737", "\u1D771", "\u1D7AB",
           "\u0392", "\u1D6A9", "\u1D6E3", "\u1D71D", "\u1D757", "\u1D791", "\u1D5D",
           "\u1D66")

  gamma = c("<U+03B3>", "<U+213D>", "<U+1D6C4>", "<U+1D6FE>", "<U+1D738>", "<U+1D772>", "<U+1D7AC>",
            "<U+0393>", "<U+213E>", "<U+1D6AA>", "<U+1D6E4>", "<U+1D71E>", "<U+1D758>", "<U+1D792>",
            "<U+1D5E>", "<U+1D67>", "<U+1D26>",
            "\u03B3", "\u213D", "\u1D6C4", "\u1D6FE", "\u1D738", "\u1D772", "\u1D7AC",
            "\u0393", "\u213E", "\u1D6AA", "\u1D6E4", "\u1D71E", "\u1D758", "\u1D792",
            "\u1D5E", "\u1D67", "\u1D26")

  mu = c("<U+03BC>", "<U+00B5>", "<U+1D6CD>", "<U+1D707>", "<U+1D741>", "<U+1D77B>", "<U+1D7B5>",
         "\u03BC", "\u00B5", "\u1D6CD", "\u1D707", "\u1D741", "\u1D77B", "\u1D7B5")

  df %>%
    # Replacements from: https://www.rapidtables.com/math/symbols/greek_alphabet.html
    gsub(paste0(alpha, collapse="|"), "a", .) %>%
    gsub(paste0(beta, collapse="|"), "b", .) %>%
    gsub(paste0(gamma, collapse="|"), "g", .) %>%
    # For "micro" units like micrograms (ug)
    gsub(paste0(mu, collapse="|"), "u", .) %>%
    # gsub(paste0(rho, collapse="|"), "r", ., fixed=TRUE) %>%
    # gsub(paste0(sigma, collapse="|"), "s", ., fixed=TRUE) %>%
    return()
}
