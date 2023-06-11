#' Case-by-Case Linear Regresssion
#'
#' @param data A data frame containing the variables in the model
#' @param formula An object of class formula or a string coercible to an object
#' of class formula
#' @param .case A quoted variable name used to subset data into cases
#' @param n_bootstrap The number of bootstrap replicates for standard errors and
#' confidence intervals of mean coefficients
#' @param lm_options Pass additional arguments to lm
#' @param boot_options Pass additional arguments to boot
#' @param boot.ci_options Pass additional arguments to boot.ci
#' @param na.rm Pass na.rm to: the mean function used to obtain mean_coef and bm_coef;
#' the sd function used to obtain se_coef; the mean function used in the statistic
#' parameter of boot
#'
#' @return An object of class cbc_lm
#' @export
#'
#' @importFrom stats lm
#' @examples
#'   df <- data.frame(ids = rep(1:5, 5),
#'                    vals = stats::rnorm(25),
#'                    outs = stats::rnorm(25, 10, 25))
#'
#'  cbc_lm(data = df, formula = outs ~ vals, .case = "ids")
cbc_lm <- function(data, formula, .case, n_bootstrap = 4000,
                   lm_options = list(), boot_options = list(),
                   boot.ci_options = list(), na.rm = FALSE){

  # Extract the variables from the formula
  ind_vars <- all.vars(stats::as.formula(formula))[-1]

  # List of unique cases
  cases <- unique(data[[.case]])

  # Run regression for each case
  models <- purrr::map(cases, function(case) {
    mod_df <- data[data[[.case]] == case,]
    do.call("lm", c(list(formula = formula, data = quote(mod_df)), lm_options))
  }) |> purrr::set_names(cases)

  # Summarize each model
  summaries <- purrr::map(models, broom::tidy)

  # Flatten summaries into one data frame and add case ID
  flat_summaries <- purrr::map2_df(summaries, names(summaries), ~ transform(.x, case = .y))

  #Include intercept in ind_vars
  ind_vars <- unique(flat_summaries$term)

  summary_stats <- purrr::map(ind_vars, ~{
    var <- .x
    mean_coef <- mean(flat_summaries$estimate[flat_summaries$term == var], na.rm = na.rm)

    boot_coef <- do.call(boot::boot, c(list(
      data = flat_summaries[flat_summaries$term == var,],
      statistic = function(data, indices) {
        mean(data$estimate[data$term == var][indices], na.rm = na.rm)
      },
      R = n_bootstrap
    ), boot_options))

    bm_coef <- mean(boot_coef$t, na.rm = na.rm)
    se_coef <- stats::sd(boot_coef$t, na.rm = na.rm)
    ci_coef <- do.call(boot::boot.ci, c(list(boot.out = boot_coef, type = "bca"), boot.ci_options))$bca[4:5]

    list(mean_coef = mean_coef, bm_coef = bm_coef, se_coef = se_coef, ci_coef = ci_coef)
  }) |> purrr::set_names(ind_vars)

  # Extract separate lists for each type of summary statistic
  mean_coef <- purrr::map(summary_stats, "mean_coef")
  bm_coef <- purrr::map(summary_stats, "bm_coef")
  se_coef <- purrr::map(summary_stats, "se_coef")
  ci_coef <- purrr::map(summary_stats, "ci_coef")

  # Return results
  out <- list(
    summary_estimates = list(
      mean_coef = mean_coef,
      bm_coef = bm_coef,
      se_coef = se_coef,
      ci_coef = ci_coef
    ),
    models = models
  )

  class(out) <- "cbc_lm"

  return(out)
}

#' @export
print.cbc_lm <- function(x, digits = max(3L, getOption("digits") - 3L),
                         boot = FALSE, ...) {

  # Extract call information
  cat("Call:\n")
  print(x$models[[1]]$call, digits = digits)

  # Print Mean Coefficients
  cat("\nMean Coefficients:\n")
  print(x$summary_estimates$mean_coef, digits = digits)

  if (boot == TRUE){
    # Print Bootstrap Mean Coefficients
    cat("\nBootstrap Mean Coefficients:\n")
    print(x$summary_estimates$bm_coef, digits = digits)
  }

  # Print coefficients for each model stored in models
  cat("\nCoefficients for each model:\n")
  purrr::iwalk(x$models, function(model, id) {
    cat("\nModel", id, "coefficients:\n")
    coef <- coef(model)
    names(coef) <- sub("^", "   ", names(coef)) # Indent the names of the coefficients
    print(coef, digits = digits)
  })

  invisible(x)
}

#' @export
summary.cbc_lm <- function(object, digits = max(3L, getOption("digits") - 3L),
                           boot = FALSE, n_models = length(object$models), ...) {

  # Extract call information
  call <- object$models[[1]]$call

  # Extract mean and bootstrap mean coefficients, standard errors, and confidence intervals
  mean_coef <- object$summary_estimates$mean_coef
  bm_coef <- object$summary_estimates$bm_coef
  se_coef <- object$summary_estimates$se_coef
  ci_coef <- object$summary_estimates$ci_coef

  # Subset the models based on n_models
  models <- object$models[1:min(n_models, length(object$models))]

  # Get summary.lm results for each model (without call info)
  model_tidy <- purrr::map(models, ~broom::tidy(summary(.x)))
  model_glance <- purrr::map(models, ~broom::glance(summary(.x)))

  # Combine into a summary list
  summary_list <- list(
    call = call,
    mean_coef = mean_coef,
    bm_coef = if (boot) bm_coef else NULL,
    se_coef = se_coef,
    ci_coef = ci_coef,
    model_summaries = list(model_tidy = model_tidy,
                           model_glance = model_glance)
  )

  # Add class
  class(summary_list) <- "summary.cbc_lm"

  return(summary_list)
}


#' @export
print.summary.cbc_lm <- function(x, digits = max(3L, getOption("digits") - 3L), ...) {

  # Extract call information
  cat("Call:\n")
  print(x$call, digits = digits)

  # Print Mean Coefficients
  cat("\nMean Coefficients:\n")
  print(x$mean_coef, digits = digits)

  # Conditionally print Bootstrap Mean Coefficients
  if(!is.null(x$bm_coef)){
    cat("\nBootstrap Mean Coefficients:\n")
    print(x$bm_coef, digits = digits)
  }

  # Print Bootstrap Standard Errors
  cat("\nBootstrap Standard Errors:\n")
  print(x$se_coef, digits = digits)

  # Print Bootstrap Confidence Intervals
  cat("\nBootstrap Confidence Intervals:\n")
  print(x$ci_coef, digits = digits)

  # Print summaries for each model
  cat("\nSummaries for each model:\n")
  for(id in seq_along(x$model_summaries$model_tidy)) {
    cat("\nModel", id, "tidy summary:\n")
    print(x$model_summaries$model_tidy[[id]], digits = digits)
    cat("\nModel", id, "glance summary:\n")
    print(x$model_summaries$model_glance[[id]], digits = digits)
  }

  invisible(x)
}


#' @export
plot.cbc_lm <- function(x, n_models = length(x$models), ask = interactive() && n_models > 1, ...) {

  # Check if n_models is within the number of models we have
  if(n_models > length(x$models)){
    warning("n_models is more than the number of models available. Plotting all models instead.")
    n_models <- length(x$models)
  }

  # Subset the models based on n_models
  models <- x$models[1:n_models]

  # If multiple plots will be drawn, set up the graphics device to ask before
  # drawing each one (if interactive)
  if (ask) {
    oldpar <- graphics::par(mfrow = c(2, 2), oma = c(0, 0, 2, 0))
    on.exit(graphics::par(oldpar))
  }

  # Loop through each model and generate 4 base R plot.lm plots
  purrr::iwalk(models, function(model, id) {

    # Plot title
    title <- paste("Model", id, "Diagnostic Plots")

    # Generate 4 plot.lm() plots for each model
    plot(model, main = title, ...)

    # If ask = TRUE and it's interactive, ask before drawing next plot
    if(ask && interactive()) {
      utils::menu(c("Next"),
                  title = "Select 1 and press Enter to go to the next plot; press Esc to quit")
    }
  })

  invisible(x)
}