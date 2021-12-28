#' Rolling Event Study
#' @author Levi Moneyhun
#' @import ggplot2
#' @import scales
#' @importFrom dplyr select
#' @importFrom dplyr mutate
#' @importFrom dplyr filter
#' @importFrom dplyr rename
#' @importFrom dplyr if_else
#' @importFrom tidyr pivot_longer
#' @importFrom broom glance
#' @importFrom broom tidy
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

make_coef_plot_notes <- function(
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

  orth_str <- ''
  if (!is.null(event_study$orth)) {
    orth_lhs <- formula_to_character(event_study$orth)[['lhs']] %>% {labels[.]}
    orth_rhs <- formula_to_character(event_study$orth)[['rhs']] %>% {labels[.]}

    orth_rhs_str <- if_else(
      length(orth_rhs) > 2,
      orth_rhs %>%
        paste(collapse = "# ") %>%
        {sub('#([^#]*)$', ', and\\1', .)} %>%
        {gsub('#', ',', .)},
      paste(orth_rhs, collapse = ' and ')
    )

    orth_str <- paste0(
      "\n",
      "(3)  ",
      orth_lhs,
      ' returns are orthogonalized against ',
      orth_rhs_str,
      ' returns.  '
    )
  }

  names$controls_string <-
    if_else(
      length(names$controls) > 2,
      names$controls %>%
        paste(collapse = "# ") %>%
        {sub('#([^#]*)$', ', and\\1', .)} %>%
        {gsub('#', ',', .)},
      paste(names$controls, collapse = ' and ')
    )

  notes <- paste0(
    source_str,
    "(1)  ",
    names$company,
    " returns are predicted via linear regression on the previous ",
    event_study$rolling_window,
    " days of returns for ",
    names$controls_string,
    ".\n",
    "(2)  Shaded areas represents the ",
    100*(1 - event_study$p.val_thresh),
    "% confidence intervals of the coefficent estimates.  ",
    orth_str
  ) %>%
    {gsub("\\.\\.", ".", .)}

  return(notes)
}
