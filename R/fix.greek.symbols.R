#--------------------------------------------------------------------------------------
#' @#'
#'
#' @return Returns a dataframe with all character fields edited to replace Greek symbols
#' with alphabetic equivalents.
#' @export
#' @title fix.greek.symbols
#' @description A function to check all character fields and replace Greek symbols
#' with alphabetic equivalents.

#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname fix.greek.symbols
#--------------------------------------------------------------------------------------
fix.greek.symbols <- function(df) {

  if(!is.character(df)){
    cat("fix.greek.symbols input must be character vector...\n")
    return()
  }

  df %>%
    # Replacements from: https://www.rapidtables.com/math/symbols/greek_alphabet.html
    gsub("α", "a", ., fixed=TRUE) %>%
    gsub("β", "b", ., fixed=TRUE) %>%
    gsub("γ", "g", ., fixed=TRUE) %>%
    gsub("δ", "d", ., fixed=TRUE) %>%
    gsub("ε", "e", ., fixed=TRUE) %>%
    gsub("ζ", "z", ., fixed=TRUE) %>%
    gsub("η", "h", ., fixed=TRUE) %>%
    gsub("θ", "th", ., fixed=TRUE) %>%
    gsub("ι", "i", ., fixed=TRUE) %>%
    gsub("κ", "k", ., fixed=TRUE) %>%
    gsub("λ", "l", ., fixed=TRUE) %>%
    # For "micro" units like micrograms (ug)
    gsub("μ", "u", ., fixed=TRUE) %>%
    gsub("ν", "n", ., fixed=TRUE) %>%
    gsub("ξ", "x", ., fixed=TRUE) %>%
    gsub("ο", "o", ., fixed=TRUE) %>%
    gsub("π", "p", ., fixed=TRUE) %>%
    gsub("ρ", "r", ., fixed=TRUE) %>%
    gsub("σ", "s", ., fixed=TRUE) %>%
    gsub("ς", "s", ., fixed=TRUE) %>%
    gsub("τ", "t", ., fixed=TRUE) %>%
    gsub("υ", "u", ., fixed=TRUE) %>%
    gsub("φ", "ph", ., fixed=TRUE) %>%
    gsub("χ", "ch", ., fixed=TRUE) %>%
    gsub("ψ", "ps", ., fixed=TRUE) %>%
    gsub("ω", "o", ., fixed=TRUE) %>%
    return()
}
