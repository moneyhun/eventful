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

make_coef_plot <- function(
  event_study,
  labels = NULL,
  y_axis_unit = .1,
  y_axis_breaks = 10,
  x_axis_breaks = 10,
  source = '',
  include_labs = TRUE,
  confidence_intervals = TRUE
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

  table_long <- event_study$table %>%
    dplyr::select(
      date,
      regression,
      df.residual,
      stdf
    ) %>%
    dplyr::mutate(
      coefficients = purrr::map(
        .x = regression,
        .f = broom::tidy
      )
    ) %>%
    tidyr::unnest(coefficients) %>%
    dplyr::mutate(
      critical_t = qt(1-event_study$p.val_thresh/2, df.residual),
      low = estimate - std.error*critical_t,
      high = estimate + std.error*critical_t
    )

  plot <- ggplot2::ggplot(
    data = table_long
  ) +
    ggplot2::geom_line(
      ggplot2::aes(
        x = date,
        y = estimate,
        color = term
      ),
      size = 1.2
    ) +
    ggplot2::geom_hline(
      ggplot2::aes(yintercept = 0),
      linetype = 'dashed'
    ) +
    ggplot2::scale_y_continuous(
      limits = function(l) {
        l[1] <- plyr::round_any(l[1] - .01, y_axis_unit, floor)
        l[2] <- plyr::round_any(l[2] + .01, y_axis_unit, ceiling)

        breaks <- scales::breaks_extended(y_axis_breaks, only.loose = T)(l)
        range(breaks)
      },
      breaks = scales::breaks_extended(y_axis_breaks, only.loose = T),
      labels = scales::label_number(accuracy = .01),
      expand = ggplot2::expansion(add = .25 * y_axis_unit)
    ) +
    ggplot2::scale_x_date(
      limits = function(l) {
        l[1] <- l[1] - lubridate::days((as.integer(round(l[2] - l[1]) * .02)))
        l[2] <- l[2] + lubridate::days((as.integer(round(l[2] - l[1]) * .02)))
        l
      },
      breaks = function(l) {
        seq.Date(min(event_study$pred_dates),
                 max(event_study$pred_dates),
                 length.out = x_axis_breaks)
      },
      labels = function(x) {
        x_format  <- format(x, '%m/%d/%y')
        x_split   <- strsplit(x_format, '/')
        x_trimmed <- lapply(x_split,
                            function(i) {
                              i[1:2] <- gsub('^0', '', i[1:2])
                              i
                            })
        x_final   <- unlist(lapply(x_trimmed,
                                   function(x) {paste(x, collapse = "/")}))
      },
      expand = c(0,0)
    ) +
    ggplot2::labs(
      y = "Coefficient Estimate"
    ) +
    ggplot2::theme_classic() +
    ggplot2::theme(
      legend.position = 'bottom',
      text = element_text(color = 'black'),
      axis.title.x   = ggplot2::element_blank(),
      legend.margin  = ggplot2::margin(0,0,2,0, 'mm'),
      axis.title.y   = ggplot2::element_text(size = 9),
      plot.title     = ggplot2::element_text(face = 'bold'),
      plot.margin    = ggplot2::margin(.25, .25, .25, .25, 'in'),
      plot.caption   = ggplot2::element_text(hjust = 0)
    )

  if (confidence_intervals) {
    plot <- plot +
      ggplot2::geom_ribbon(
        ggplot2::aes(
          x = date,
          fill = term,
          ymin = low,
          ymax = high
        ),
        alpha = .05,
        show.legend = FALSE
      ) +
      ggplot2::geom_ribbon(
        ggplot2::aes(
          x = date,
          color = term,
          ymin = low,
          ymax = high
        ),
        fill = NA,
        alpha = .3,
        linetype = 'dashed',
        show.legend = FALSE
      ) +
      scale_color_brewer(
        palette = 'Set1',
        guide = guide_legend(title = NULL,
                             override.aes = list(fill = NA))
      ) +
      scale_fill_brewer(
        palette = 'Set1',
        guide = 'none'
      )
  } else {
    plot <- plot  +
      scale_color_brewer(
        palette = 'Set1',
        guide = guide_legend(title = NULL)
      )
  }

  if (include_labs) {
    plot <- plot +
      ggplot2::labs(
        title = paste0(names$company, " Event Study Coefficient Estimates"),
        subtitle = paste0(event_study$rolling_window, "-day Rolling Window Event Study"),
        caption = paste0(
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
      )
  }

  return(plot)
}
