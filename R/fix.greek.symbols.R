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
  # USE stringi::stri_escape_unicode("") to assist with identifying new encoding
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
  
  df %>%
    # Replacements from: https://www.rapidtables.com/math/symbols/greek_alphabet.html
    gsub(paste0(alpha, collapse="|"), "a", .) %>%
    gsub(paste0(beta, collapse="|"), "b", .) %>%
    gsub(paste0(gamma, collapse="|"), "g", .) %>%
    gsub(paste0(epsilon, collapse="|"), "e", .) %>%
    gsub(paste0(apostrophe_epsilon, collapse="|"), "'e", .) %>%
    gsub(paste0(lambda, collapse="|"), "l", .) %>%
    gsub(paste0(kappa, collapse="|"), "k", .) %>%
    gsub(paste0(omega, collapse="|"), "o", .) %>%
    gsub(paste0(apostrophe_omega, collapse="|"), "'o", .) %>%
    gsub(paste0(eta, collapse="|"), "h", .) %>%
    gsub(paste0(apostrophe_eta, collapse="|"), "'h", .) %>%
  
    # Fix omega with preceding letter
    gsub("<U+33C0>|\u33c0", "KO", .) %>%
    gsub("<U+33C1>|\u33c1", "MO", .) %>%
  
    # For "micro" units like micrograms (ug)
    gsub(paste0(mu, collapse="|"), "u", .) %>%
    # gsub(paste0(rho, collapse="|"), "r", ., fixed=TRUE) %>%
    # gsub(paste0(sigma, collapse="|"), "s", ., fixed=TRUE) %>%
    return()
}
