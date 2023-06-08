cbc_lm <- function(data, formula, .case, n_bootstrap = 1000){

  # Extract the variables from the formula
  ind_vars <- all.vars(stats::as.formula(formula))[-1]

  # List of unique cases
  cases <- unique(data[[.case]])

  # Run regression for each case
  models <- purrr::map(cases, function(case) {
    mod_df <- data[data[[.case]] == case,]
    stats::lm(formula, data = mod_df)
  }) |> purrr::set_names(cases)

  # Summarize each model
  summaries <- purrr::map(models, broom::tidy)

  # Flatten summaries into one data frame and add case ID
  flat_summaries <- purrr::map2_df(summaries, names(summaries), ~ transform(.x, case = .y))

  #Include intercept in ind_vars
  ind_vars <- unique(flat_summaries$term)

  summary_stats <- purrr::map(ind_vars, ~{
    var <- .x
    mean_coef <- mean(flat_summaries$estimate[flat_summaries$term == var])

    boot_coef <- boot::boot(data = flat_summaries[flat_summaries$term == var,] , statistic = function(data, indices) {
      mean(data$estimate[data$term == var][indices])
    }, R = n_bootstrap)

    se_coef <- stats::sd(boot_coef$t)
    ci_coef <- boot::boot.ci(boot_coef, type = "bca")$bca[4:5]

    list(mean_coef = mean_coef, se_coef = se_coef, ci_coef = ci_coef)
  }) |> purrr::set_names(ind_vars)

  # Extract separate lists for each type of summary statistic
  mean_coef <- purrr::map(summary_stats, "mean_coef")
  se_coef <- purrr::map(summary_stats, "se_coef")
  ci_coef <- purrr::map(summary_stats, "ci_coef")

  # Return results
  out <- list(
    summary_estimates = list(
      mean_coef = mean_coef,
      se_coef = se_coef,
      ci_coef = ci_coef
    ),
    models = models
  )

  class(out) <- "cbc_lm"

  return(out)
}
