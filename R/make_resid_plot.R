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

make_resid_plot <- function(
  event_study,
  labels = NULL,
  y_axis_unit = .01,
  y_axis_breaks = 10,
  x_axis_breaks = 10,
  source = '',
  include_labs = TRUE
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

  event_study$table <- event_study$table %>%
    mutate(
      critical_t = qt(1-event_study$p.val_thresh/2, df.residual),
      low = -1*stdf*critical_t,
      high = stdf*critical_t,
    )

  plot <- ggplot2::ggplot(
    data = event_study$table
  ) +
    ggplot2::geom_ribbon(
      ggplot2::aes(
        x = date,
        ymin = low,
        ymax = high
      ),
      fill = 'grey96',
      color = 'grey80'
    ) +
    ggplot2::geom_hline(
      ggplot2::aes(yintercept = 0),
      linetype = 'dashed'
    ) +
    ggplot2::geom_point(
      ggplot2::aes(
        x = date,
        y = resid,
        color = ifelse(resid.signif,
                       paste0("Significant at ",
                              scales::percent(event_study$p.val_thresh,
                                              accuracy = 1)),
                       paste0("Not Significant at ",
                              scales::percent(event_study$p.val_thresh,
                                              accuracy = 1)))
      ),
      size = 1.5
    ) +
    ggplot2::geom_point(
      data = event_study$table %>% filter(misrep==1),
      ggplot2::aes(
        x = date,
        y = resid,
        shape = 'Alleged Misrepresentation'
      ),
      size = 3
    ) +
    ggplot2::geom_point(
      data = event_study$table %>% filter(disc==1),
      ggplot2::aes(
        x = date,
        y = resid,
        shape = 'Alleged Disclosure'
      ),
      size = 3
    ) +
    ggplot2::scale_y_continuous(
      limits = function(l) {
        l[1] <- plyr::round_any(l[1] - .01, y_axis_unit, floor)
        l[2] <- plyr::round_any(l[2] + .01, y_axis_unit, ceiling)

        breaks <- scales::breaks_extended(y_axis_breaks, only.loose = T)(l)
        range(breaks)
      },
      breaks = scales::breaks_extended(y_axis_breaks, only.loose = T),
      labels = scales::label_percent(accuracy = 1),
      expand = ggplot2::expansion(add = .0025)
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
    ggplot2::scale_color_manual(
      breaks = c(
        paste0("Significant at ",
               scales::percent(event_study$p.val_thresh,
                               accuracy = 1)),
        paste0("Not Significant at ",
               scales::percent(event_study$p.val_thresh,
                               accuracy = 1))
      ),
      values = c(
        '#E41A1C',
        '#377EB8'
      ),
      guide = ggplot2::guide_legend(title = NULL)
    ) +
    ggplot2::scale_shape_manual(
      breaks = c(
        'Alleged Misrepresentation',
        'Alleged Disclosure'
      ),
      values = c(2, 6),
      guide = ggplot2::guide_legend(title = NULL)
    ) +
    ggplot2::labs(
      y = "Residual Return"
    ) +
    ggplot2::theme_classic() +
    ggplot2::theme(
      legend.position = 'bottom',
      axis.title.x   = ggplot2::element_blank(),
      legend.margin  = ggplot2::margin(0,0,2,0, 'mm'),
      axis.title.y   = ggplot2::element_text(size = 9),
      plot.title     = ggplot2::element_text(face = 'bold'),
      plot.margin    = ggplot2::margin(.25, .25, .25, .25, 'in'),
      plot.caption   = ggplot2::element_text(hjust = 0)
    )

  if(include_labs) {
    plot <- plot +
      ggplot2::labs(
        title = paste0(names$company, " Residual Returns"),
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
          "(2)  Grey shaded area represents the ",
          100*(1 - event_study$p.val_thresh),
          "% confidence interval of the predicted returns."
        ) %>%
          {gsub("\\.\\.", ".", .)}
      )
  }

  return(plot)
}
