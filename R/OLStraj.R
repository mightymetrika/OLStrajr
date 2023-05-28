OLStraj <- function(data, idvarname = "id", predvarname = "time",
                    outvarname = "score",
                    varlist = c("anti1", "anti2", "anti3", "anti4"),
                    timepts = c(0, 1, 2, 3), inclmiss = "n", level = "both", regtype = "lin",
                    numplot = NULL, hist = "y", box = "y", outds = TRUE) {

  if(length(timepts) != length(varlist)){
    stop("ERROR: NUMBER OF TIME POINTS DOES NOT EQUAL NUMBER OF REPEATED MEASURES")
  }

  # Create subsample
  if (!is.null(numplot)) {
    data <- data[1:numplot, ]
  }

  # Save a copy of data in original format
  data_orig <- data

  # Listwise deletion
  if (inclmiss == "n") {
    data <- data[stats::complete.cases(data), ]
  }

  # OLS case-by-case regressions
  estimated_values <- data.frame()

  #Lengthen data frame
  data <- tidyr::pivot_longer(data, cols = tidyselect::all_of(varlist),
                              names_to = predvarname,
                              values_to = outvarname)

  data[[predvarname]] <- timepts[match(data[[predvarname]], varlist)]

  # Get quadratic term
  if (regtype != "lin"){
    data[[paste0(predvarname, "_sq")]] <- data[[predvarname]]^2
  }

  for (id in unique(data[[idvarname]])) {

    # Fit a linear regression model
    mod_df <- data[data[[idvarname]] == id, ]

    if (regtype == "lin"){
      cbc_form <- stats::as.formula(paste(outvarname, "~", predvarname))
    } else {
      cbc_form <- stats::as.formula(paste(outvarname, "~", predvarname, "+",
                                          paste0(predvarname, "_sq")))
    }
    model <- stats::lm(cbc_form, data = mod_df)

    # Add the estimated values to the data frame
    if (regtype == "lin"){
      ols_dat <- stats::setNames(data.frame(id,
                                            stats::coef(model)[1],
                                            stats::coef(model)[2],
                                            summary(model)[[8]]),
                                 c(eval(idvarname), "intercept", "linear", "rsquared"))
    } else {
      ols_dat <- stats::setNames(data.frame(id,
                                            stats::coef(model)[1],
                                            stats::coef(model)[2],
                                            stats::coef(model)[3],
                                            summary(model)[[8]]),
                                 c(eval(idvarname), "intercept", "linear", "quad", "rsquared"))

    }


    estimated_values <- rbind(estimated_values, ols_dat)
    }

  # Write output to a data frame
  if (outds == TRUE) {
    out_data <- merge(data_orig, estimated_values, by = eval(idvarname))
  }

  # Plotting section
  group_plots <- list()
  individual_plots <- list()
  histogram_plots <- list()

  # Prepare data for plotting
  data <- merge(data, estimated_values, by = eval(idvarname))

  if (level == "both" | level == "grp") {
    # Group-level plots

    # Simple-joined (noninterpolated) trajectories
    group_plots[["simple_joined"]] <- ggplot2::ggplot(data,
                                                      ggplot2::aes(x = .data[[predvarname]],
                                                                   y = .data[[outvarname]],
                                                                   group = .data[[idvarname]])) +
      ggplot2::geom_line() +
      ggplot2::ggtitle("Simple-Joined Trajectories")

    # OLS trajectories
    group_plots[["ols"]] <- ggplot2::ggplot(data, ggplot2::aes(x = .data[[predvarname]], y = .data[[outvarname]],
                                                               group = .data[[idvarname]])) +
      ggplot2::ggtitle("OLS Trajectories")

    if (regtype == "lin"){
      group_plots[["ols"]] <- group_plots[["ols"]] +
        ggplot2::geom_smooth(se = FALSE, method = lm)

    } else {
      group_plots[["ols"]] <- group_plots[["ols"]] +
        ggplot2::stat_smooth(method = "lm", formula = y ~ x + I(x^2),
                             se = FALSE)
    }

  }

  if (level == "both" | level == "ind") {
    # Individual-level plots
    for (id in unique(data[[idvarname]])) {
      ind_data <- data[data[[idvarname]] == id, ]

      # OLS trajectories
      individual_plots[[paste("ols", id)]] <- ggplot2::ggplot(ind_data,
                                                              ggplot2::aes(x = .data[[predvarname]],
                                                                           y = .data[[outvarname]])) +
        ggplot2::ggtitle(paste("OLS Trajectory for", id))

      if (regtype == "lin"){
        individual_plots[[paste("ols", id)]] <- individual_plots[[paste("ols", id)]] +
          ggplot2::geom_point() +
          ggplot2::geom_smooth(se = FALSE, method = lm)


      } else if (regtype == "quad"){
        individual_plots[[paste("ols", id)]] <- individual_plots[[paste("ols", id)]] +
          ggplot2::geom_point() +
          ggplot2::geom_smooth(method = "lm", formula = y ~ x + I(x^2), se = FALSE)

      } else {
        individual_plots[[paste("ols", id)]] <- individual_plots[[paste("ols", id)]] +
          ggplot2::geom_point() +
          ggplot2::geom_smooth(se = FALSE, method = lm) +
          ggplot2::geom_smooth(method = "lm", formula = y ~ x + I(x^2), se = FALSE,
                               linetype = "dashed")
      }
    }
  }

  if (hist == "y") {
    # Histogram
    intercepts <- ggplot2::ggplot(data, ggplot2::aes(x = intercept)) +
      ggplot2::geom_histogram() +
      ggplot2::ggtitle("Histogram of OLS Estimated Intercepts")

    slopes <- ggplot2::ggplot(data, ggplot2::aes(x = linear)) +
      ggplot2::geom_histogram() +
      ggplot2::ggtitle("Histogram of OLS Estimated Slopes")

    if (regtype != "lin"){
      quads <- slopes <- ggplot2::ggplot(data, ggplot2::aes(x = quad)) +
        ggplot2::geom_histogram() +
        ggplot2::ggtitle("Histogram of OLS Estimated Quadratic Terms")

      histogram_plots = list("intercepts" = intercepts,
                             "slopes" = slopes,
                             "quads" = quads)
    } else {
      histogram_plots = list("intercepts" = intercepts,
                             "slopes" = slopes)
    }
  }

  if (box == "y") {
    #Add mean to boxplot and label outliers:
    if (regtype == "lin"){
      data_box <- data |> tidyr::pivot_longer(cols = c(intercept, linear),
                                              names_to = "param",
                                              values_to = "est")
    } else {
      data_box <- data |> tidyr::pivot_longer(cols = c(intercept, linear, quad),
                                              names_to = "param",
                                              values_to = "est")
    }

    # calculate the upper and lower bounds for outliers
    boxplot_stats <- tapply(data_box$est, data_box$param, grDevices::boxplot.stats)
    outlier_vals <- lapply(boxplot_stats, `[[`, "out")

    # Initialize outlier column
    data_box$outlier <- NA

    # Loop over the parameters and identify outliers
    for(i in unique(data_box$param)) {
      pick_outliers <- data_box$param == i & data_box$est %in% outlier_vals[[i]] &
        is.na(data_box$outlier)

      data_box$outlier[pick_outliers] <- data_box[[idvarname]][pick_outliers]
    }

    # Convert outlier column to character and change missing to ""
    data_box$outlier <- as.character(data_box$outlier)
    data_box$outlier <- ifelse(is.na(data_box$outlier), "", data_box$outlier)

    #Create box plot with mean plotted and outliers labeled by idvarname
    data_box <- data_box |>
      ggplot2::ggplot(ggplot2::aes(x = param, y = est)) +
      ggplot2::geom_boxplot() +
      ggplot2::stat_summary(fun = mean, geom = "point", shape = 20, size = 3, color = "blue") + # Add point for mean
      ggplot2::geom_text(ggplot2::aes(label = outlier), hjust = -0.1) +
      ggplot2::ylab("OLS Estimates") +
      ggplot2::xlab("") +
      ggplot2::ggtitle("Box Plots of OLS Estimates")
  }


  # Return a list containing estimated values and the plots
  result <- list(out_data = out_data,
                 group_plots = group_plots,
                 individual_plots = individual_plots,
                 histogram_plots = histogram_plots,
                 box_plot = data_box)

  return(result)
}
