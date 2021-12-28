#' Residual Plot
#' @author Levi Moneyhun
#' @import ggplot2
#' @import scales
#' @importFrom plyr round_any
#' @param event_study The output from rolling_event_study (with simple=FALSE).
#' @param labels A named character vector of labels corresponding to the variables in the regression (e.g., c("aaa_ret" = "AAA Inc.","bbb_ret" =  "B Co.")).
#' @param y_axis_unit unit to which y-axis limits should be expanded.
#' @param y_axis_breaks number of y-axis breaks to be attempted
#' @param x_axis_breaks number of x-axis breaks
#' @param source
#' @param include_labs
#' @return ggplot object
#' @export

make_resid_plot_notes <- function(
  event_study,
  labels = NULL,
  source = ''
) {
  if (is.null(labels)) {
    labels <- c(event_study$company, event_study$controls)
    names(labels) <- labels
  }

  names <- list(
    'company' = labels[[event_study$company]],
    'controls' = map(
      event_study$controls,
      ~ labels[[.x]]
    )
  )

  source_str <- ''
  if (source != '') {source_str <- paste0("Source:  ", source, '\n\n')}

  names$controls_string <- names$controls %>%
    paste(collapse = "# ") %>%
    {sub('#([^#]*)$', ', and\\1', .)} %>%
    {gsub('#', ',', .)}

  notes <- paste0(
    source_str,
    "(1)  ",
    names$company,
    " returns are predicted via linear regression on the previous ",
    event_study$rolling_window,
    " days of returns for ",
    names$controls_string,
    ".\n",
    "(2)  Grey shaded area represents the ",
    100*(1 - event_study$p.val_thresh),
    "% confidence interval of the predicted returns."
  ) %>%
    {gsub("\\.\\.", ".", .)}

  return(notes)
}
