#' Rolling Event Study
#' @author Levi Moneyhun
#' @importFrom magrittr %>%
#' @importFrom lubridate ymd
#' @importFrom dplyr select
#' @importFrom dplyr mutate
#' @importFrom dplyr filter
#' @importFrom dplyr rename
#' @importFrom dplyr if_else
#' @importFrom purrr map
#' @importFrom purrr map2
#' @importFrom tidyr unnest
#' @importFrom tidyr pivot_wider
#' @importFrom broom glance
#' @importFrom broom tidy
#' @importFrom stats update
#' @importFrom stats predict
#' @importFrom stats pt
#' @param formula A formula object describing the regression to be run.  If dummy dates are specified in 'id_dates', they will be added to this formula.
#' @param data A data.frame object containing the data for the regression and a 'date' variable.
#' @param pred_date_range A vector of the min and max date for the prediction range.
#' @param exclude_dates A vector of dates which should be excluded from the fitted regression models.
#' @param id_dates A vector of dates which should be 'dummied out' via indicator variables.
#' @param misrep_dates A vector of dates which should be flagged as misrepresentation dates in the output.  Does not affect the regression model.
#' @param disc_dates A vector of dates which should be flagged as disclosure dates in the output.  Does not affect the regression models.
#' @param rolling_window A number specifying the width of the rolling window.  E.g., a window of 50 means that the regression used to predict returns on day i will be fitted on data spanning from day i-50 to day i-1.
#' @param roll_fixed Either 'observations' or 'window'.  Where excluded dates fall within the rolling window for a given date, this specifies whether the number of observations should remain fixed--meaning the window must be extended--or whether the window should remain fixed--meaning the number of observations will be lower.
#' @param orth A formula object describing the orthogonalziation which should be applied (e.g., for y ~ x1 + x2, y in the final regression would be replaced by the residual from this regression.)
#' @param p.val_thresh Threshold for flagging residuals as significant.  5% by default.
#' @param simple TRUE/FALSE indicating whether full output or just 'table' should be returned.  FALSE by default.
#' @return If simple is FALSE, an list containing a table of event study results, which the regressions themselves as a column, and many other variables of interest relating to the event study.
#' @export
#'
rolling_event_study <- function(
  formula,
  data,
  pred_date_range,
  exclude_dates = NULL,
  id_dates = NULL,
  misrep_dates = NULL,
  disc_dates = NULL,
  rolling_window = NULL,
  roll_fixed = NULL,
  orth = NULL,
  p.val_thresh = .05,
  simple = FALSE
) {
  event_study <- list()
  event_study$formula        <- formula
  event_study$data           <- data
  event_study$exclude_dates  <- exclude_dates
  event_study$id_dates       <- id_dates
  event_study$misrep_dates   <- misrep_dates
  event_study$disc_dates     <- disc_dates
  event_study$rolling_window <- rolling_window
  event_study$roll_fixed     <- roll_fixed
  event_study$orth           <- orth
  event_study$p.val_thresh   <- p.val_thresh
  event_study$est_dates      <- data$date
  event_study$pred_dates     <- data$date[data$date >= pred_date_range[1] & data$date <= pred_date_range[2]]
  event_study$company        <- formula_to_character(formula)[['lhs']]
  event_study$controls       <- formula_to_character(formula)[['rhs']]

  if(!is.null(event_study$orth)) {
    orth_var <- paste0(as.character(event_study$orth[[2]]), '_orth')
    event_study$orth_reg <- lm(formula = event_study$orth,
                                data = event_study$data)
    event_study$data[[orth_var]] <- predict(event_study$orth_reg)
    formula <- stats::update(formula,
                             paste("~ . -", as.character(event_study$orth[[2]]), "+", orth_var))
  }

  if (!is.null(id_dates)) {
    id_dates_format <- format(id_dates, '%Y.%m.%d')
    for (i in 1:length(id_dates)) {
      event_study$data[[paste0("i",id_dates_format[i])]] <-
        as.numeric(event_study$data[['date']] == id_dates[i])

      formula <- stats::update(formula,
                               paste('~ . +', paste0("i",id_dates_format[i])))
    }
  }

  event_study$table <- event_study$data %>%
    dplyr::filter(date %in% event_study$pred_dates) %>%
    dplyr::mutate(
      window_dates = purrr::map(
        .x = date,
        .f = function(
          date,
          event_study
        ) {
          if (event_study$roll_fixed == 'observations') {
            est_dates <- event_study$est_dates[!event_study$est_dates %in% event_study$exclude_dates]
          } else {
            est_dates <- event_study$est_dates
          }

          window_end   <- max(which(est_dates < date))
          window_start <- window_end - (event_study$rolling_window - 1)
          window_dates <- est_dates[window_start:window_end]
          if (event_study$roll_fixed == 'window') {
            window_dates <- window_dates[!window_dates %in% event_study$exclude_dates]
          }
          return(window_dates)
        },
        event_study = event_study)) %>%
    dplyr::mutate(
      regression = purrr::map2(
        .x = date,
        .y = window_dates,
        .f = ~ lm(formula = event_study$formula,
                  data = event_study$data %>%
                    dplyr::filter(date %in% .y))
      ),
      pred = purrr::map2(
        .x = regression,
        .y = date,
        .f = ~ stats::predict(regression[[1]],
                              newdata = event_study$data %>%
                                dplyr::filter(date == .y),
                              se.fit = TRUE) %>%
                data.frame() %>%
                dplyr::select(fit, se.fit)
      ),
      reg_stats = purrr::map(
        .x = regression,
        .f = broom::glance
      ),
      reg_coefs = purrr::map(
        .x = regression,
        .f =
          ~ .x %>%
          broom::tidy() %>%
          dplyr::select(term, estimate) %>%
          tidyr::pivot_wider(names_from = 'term',
                             values_from = 'estimate',
                             names_prefix = 'coef_') %>%
          dplyr::rename(
            coef_intercept = `coef_(Intercept)`
          )
      )
    ) %>%
    tidyr::unnest(
      cols = c(pred, reg_stats, reg_coefs)
    ) %>%
    dplyr::select(
      -statistic,
      -p.value,
      -logLik,
      -AIC,
      -BIC,
      -deviance
    ) %>%
    dplyr::mutate(
      actual       = !! formula[[2]],
      resid        = actual - fit,
      stdf         = sqrt(sigma^2 + se.fit^2),
      resid.tstat  = resid / stdf,
      resid.p.val  = 2 * stats::pt(-abs(resid.tstat), df = df.residual),
      resid.signif = resid.p.val < event_study$p.val_thresh,
      misrep       = dplyr::if_else(date %in% event_study$misrep_dates, 1, 0),
      disc         = dplyr::if_else(date %in% event_study$disc_dates, 1, 0),
    )

  if (simple) {
    return(event_study$table)
  } else {
    return(event_study)
  }
}
