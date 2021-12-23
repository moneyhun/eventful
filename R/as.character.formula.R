#' As character formula -- based on as.character.formula from formula.tools
#' @author Levi Moneyhun
#' @param x formula object
#' @param ... further arguments pass to or from other methods
#' @return A list of two items:  left hand side of formula, right hand side of formula
#' @export
#'
formula_to_character <- function (x, ...) {
  form <- paste(deparse(x), collapse = " ")
  form <- gsub("\\s+", " ", form, perl = FALSE)
  split <- strsplit(form, " ~ ")[[1]]
  lhs <- split[1]
  rhs <- strsplit(split[2], " \\+ ")[[1]]
  return(list('lhs'=lhs, 'rhs'=rhs))
}
