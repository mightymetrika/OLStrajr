OLStraj <- function(data, idvarname = "id", varlist = c("anti1", "anti2", "anti3", "anti4"),
                    timepts = c(0, 1, 2, 3), inclmiss = "n", level = "both", regtype = "lin",
                    numplot = NULL, hist = "y", box = "y", outds = TRUE) {

  if(length(timepts) != length(varlist)){
    stop("ERROR: NUMBER OF TIME POINTS DOES NOT EQUAL NUMBER OF REPEATED MEASURES")
  }

  # Save a copy of data in original format
  data_orig <- data

  # Listwise deletion
  if (inclmiss == "n") {
    data <- data[stats::complete.cases(data), ]
  }

  # Create a subsample
  if (!is.null(numplot)) {
    data <- data[1:numplot, ]
  }

  # OLS case-by-case regressions
  estimated_values <- data.frame()

  #Lengthen data frame
  data <- tidyr::pivot_longer(data, cols = tidyselect::all_of(varlist),
                              names_to = "time",
                              values_to = "score")

  data$time <- timepts[match(data$time, varlist)]

  for (id in unique(data[[idvarname]])) {

    # Fit a linear regression model
    mod_df <- data[data[[idvarname]] == id, ]
    model <- stats::lm(score ~ time, data = mod_df)

    # Add the estimated values to the data frame
    ols_dat <- stats::setNames(data.frame(id, stats::coef(model)[1], stats::coef(model)[2]),
                               c(eval(idvarname), "intercept", "linear"))
    estimated_values <- rbind(estimated_values, ols_dat)
    }

  # Write output to a data frame
  if (outds == TRUE) {
    out_data <- merge(data_orig, estimated_values, by = "id")
  }

  # Plotting section
  group_plots <- list()
  individual_plots <- list()
  histogram_plots <- list()

  if (level == "both" | level == "grp") {
    # Group-level plots

    # Simple-joined (noninterpolated) trajectories
    group_plots[["simple_joined"]] <- ggplot2::ggplot(data, ggplot2::aes(x = time, y = score,
                                                                         group = .data[[idvarname]])) +
      ggplot2::geom_line() +
      ggplot2::ggtitle("Simple-Joined Trajectories")

    # OLS trajectories
    data <- merge(data, estimated_values, by = "id")
    data$predicted_score <- data$intercept + data$linear * data$time

    group_plots[["ols"]] <- ggplot2::ggplot(data, ggplot2::aes(x = time, y = score,
                                                               group = .data[[idvarname]])) +
      ggplot2::geom_smooth(se = FALSE, method = lm) +
      ggplot2::ggtitle("OLS Trajectories")
  }

  if (level == "both" | level == "ind") {
    # Individual-level plots
    for (id in unique(data[[idvarname]])) {
      ind_data <- data[data[[idvarname]] == id, ]

      # # Simple-joined (noninterpolated) trajectories
      # individual_plots[[paste("simple_joined", id)]] <- ggplot2::ggplot(ind_data, ggplot2::aes(x = time, y = score)) +
      #   ggplot2::geom_point() +
      #   ggplot2::geom_line() +
      #   ggplot2::ggtitle(paste("Simple-Joined Trajectory for", id))

      # OLS trajectories
      individual_plots[[paste("ols", id)]] <- ggplot2::ggplot(ind_data, ggplot2::aes(x = time, y = score)) +
        ggplot2::geom_point() +
        #ggplot2::geom_line(ggplot2::aes(y = predicted_score)) +
        ggplot2::geom_smooth(se = FALSE, method = lm) +
        ggplot2::ggtitle(paste("OLS Trajectory for", id))
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
  }

  if (box == "y") {
    #Add mean to boxplot:
    #https://stackoverflow.com/questions/19876505/boxplot-show-the-value-of-mean
    data_box <- data |> tidyr::pivot_longer(cols = c(intercept, linear),
                                            names_to = "param",
                                            values_to = "est") |>
      ggplot2::ggplot(ggplot2::aes(x = param, y = est)) +
      ggplot2::geom_boxplot() +
      ggplot2::ylab("OLS Estimates") +
      ggplot2::xlab("") +
      ggplot2::ggtitle("Box Plots of OLS Estimates")
  }

  # Return a list containing estimated values and the plots
  result <- list(out_data = out_data,
                 group_plots = group_plots,
                 individual_plots = individual_plots,
                 histogram_plots = list("intercepts" = intercepts,
                                        "slopes" = slopes),
                 box_plot = data_box)

  return(result)
}

