cbc_lm <- function(data, formula, .case, n_bootstrap = 1000){

  # List of unique cases
  cases <- unique(data[[.case]])

  # Run regression for each case
  models <- lapply(cases, function(case) {
    mod_df <- data[data[[.case]] == case,]
    #stats::lm(formula, data = data, subset = (data[[.case]] == case))
    stats::lm(formula, data = mod_df)
  }) %>% stats::setNames(cases)

  # Summarize each model
  summaries <- purrr::map(models, broom::tidy)

  # Flatten summaries into one data frame and add case ID
  flat_summaries <- purrr::map2_df(summaries, names(summaries), ~ transform(.x, case = .y))

  # Calculate means of the slope and intercept across models
  mean_slope <- mean(flat_summaries$estimate[flat_summaries$term == "vals"])
  mean_intercept <- mean(flat_summaries$estimate[flat_summaries$term == "(Intercept)"])

  # Bootstrap standard errors and 95% confidence intervals
  boot_slope <- boot::boot(data = flat_summaries, statistic = function(data, indices) {
    mean(data$estimate[data$term == "vals"][indices])
  }, R = n_bootstrap)

  boot_intercept <- boot::boot(data = flat_summaries, statistic = function(data, indices) {
    mean(data$estimate[data$term == "(Intercept)"][indices])
  }, R = n_bootstrap)

  se_slope <- stats::sd(boot_slope$t)
  se_intercept <- stats::sd(boot_intercept$t)

  ci_slope <- boot::boot.ci(boot_slope, type = "bca")$bca[4:5]
  ci_intercept <- boot::boot.ci(boot_intercept, type = "bca")$bca[4:5]

  # Return results
  list(
    summary_estimates = list(
      mean_slope = mean_slope,
      mean_intercept = mean_intercept,
      se_slope = se_slope,
      se_intercept = se_intercept,
      ci_slope = ci_slope,
      ci_intercept = ci_intercept
    ),
    models = models
  )
}
