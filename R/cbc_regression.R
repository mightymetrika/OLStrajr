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
  #
  # # Initialize lists to store the summary estimates
  # mean_coef <- list()
  # se_coef <- list()
  # ci_coef <- list()

  #Include intercept in ind_vars
  ind_vars <- unique(flat_summaries$term)

  # for (var in ind_vars) {
  #   # Calculate means of the coefficients across models
  #   mean_coef[[var]] <- mean(flat_summaries$estimate[flat_summaries$term == var])
  #
  #   # Bootstrap standard errors and 95% confidence intervals
  #   boot_coef <- boot::boot(data = flat_summaries[flat_summaries$term == var,] , statistic = function(data, indices) {
  #     mean(data$estimate[data$term == var][indices])
  #   }, R = n_bootstrap)
  #
  #   se_coef[[var]] <- stats::sd(boot_coef$t)
  #   ci_coef[[var]] <- boot::boot.ci(boot_coef, type = "bca")$bca[4:5]
  # }

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
  list(
    summary_estimates = list(
      mean_coef = mean_coef,
      se_coef = se_coef,
      ci_coef = ci_coef
    ),
    models = models
  )
}


# cbc_lm <- function(data, formula, .case, n_bootstrap = 1000){
#
#   # List of unique cases
#   cases <- unique(data[[.case]])
#
#   # Run regression for each case
#   models <- purrr::map(cases, function(case) {
#     mod_df <- data[data[[.case]] == case,]
#     stats::lm(formula, data = mod_df)
#   }) |> stats::setNames(cases)
#
#   # Summarize each model
#   summaries <- purrr::map(models, broom::tidy)
#
#   # Flatten summaries into one data frame and add case ID
#   flat_summaries <- purrr::map2_df(summaries, names(summaries), ~ transform(.x, case = .y))
#
#   # Calculate means of the slope and intercept across models
#   mean_slope <- mean(flat_summaries$estimate[flat_summaries$term == "vals"])
#   mean_intercept <- mean(flat_summaries$estimate[flat_summaries$term == "(Intercept)"])
#
#   # Bootstrap standard errors and 95% confidence intervals
#   boot_slope <- boot::boot(data = flat_summaries[flat_summaries$term == "vals",] , statistic = function(data, indices) {
#     mean(data$estimate[data$term == "vals"][indices])
#   }, R = n_bootstrap)
#
#
#   boot_intercept <- boot::boot(data = flat_summaries[flat_summaries$term == "(Intercept)",], statistic = function(data, indices) {
#     mean(data$estimate[data$term == "(Intercept)"][indices])
#   }, R = n_bootstrap)
#
#   se_slope <- stats::sd(boot_slope$t)
#   se_intercept <- stats::sd(boot_intercept$t)
#
#   ci_slope <- boot::boot.ci(boot_slope, type = "bca")$bca[4:5]
#   ci_intercept <- boot::boot.ci(boot_intercept, type = "bca")$bca[4:5]
#
#   # Return results
#   list(
#     summary_estimates = list(
#       mean_slope = mean_slope,
#       mean_intercept = mean_intercept,
#       se_slope = se_slope,
#       se_intercept = se_intercept,
#       ci_slope = ci_slope,
#       ci_intercept = ci_intercept
#     ),
#     models = models
#   )
# }
