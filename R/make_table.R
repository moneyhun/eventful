#' Rolling Event Study
#' @author Levi Moneyhun
#' @param event_study The output from rolling_event_study (with simple=FALSE).
#' @param labels A named list of labels corresponding to the variables in the regression (e.g., list("AAA Inc." = aaa_ret, "B Co." = bbb_ret)).
#' @return If simple is FALSE, an list containing a table of event study results, which the regressions themselves as a column, and many other variables of interest relating to the event study.
#' @export
#'
