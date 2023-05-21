OLStraj <- function(data, idvarname = "id", varlist = c("anti1", "anti2", "anti3", "anti4"),
                    timepts = c(0, 1, 2, 3), inclmiss = "n", level = "both", regtype = "lin",
                    numplot = NULL, hist = "y"){#}, outds = NULL) {

  if(length(timepts) != length(varlist)){
    stop("ERROR: NUMBER OF TIME POINTS DOES NOT EQUAL NUMBER OF REPEATED MEASURES")
  }

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
    estimated_values <- rbind(estimated_values, data.frame(id = id, intercept = stats::coef(model)[1], linear = stats::coef(model)[2]))
  }

  # Write output to a data frame
  # if (!is.null(outds)) {
  #   assign(outds, estimated_values, envir = .GlobalEnv)
  # }

  # Plotting section
  group_plots <- list()
  individual_plots <- list()
  histogram_plot <- NULL

  if (level == "both" | level == "grp") {
    # Group-level plots

    # Simple-joined (noninterpolated) trajectories
    group_plots[["simple_joined"]] <- ggplot2::ggplot(data, ggplot2::aes(x = time, y = score,
                                                                         group = .data[[idvarname]])) +
      ggplot2::geom_point() +
      ggplot2::geom_line() +
      ggplot2::facet_wrap(~ .data[[idvarname]]) +
      ggplot2::ggtitle("Simple-Joined Trajectories")

    # OLS trajectories
    data <- merge(data, estimated_values, by = "id")
    data$predicted_score <- data$intercept + data$linear * data$time

    group_plots[["ols"]] <- ggplot2::ggplot(data, ggplot2::aes(x = time, y = predicted_score,
                                                               group = id)) +
      ggplot2::geom_point() +
      ggplot2::geom_line() +
      ggplot2::facet_wrap(~ id) +
      ggplot2::ggtitle("OLS Trajectories")
  }

  if (level == "both" | level == "ind") {
    # Individual-level plots
    for (id in unique(data[[idvarname]])) {
      ind_data <- data[data[[idvarname]] == id, ]

      # Simple-joined (noninterpolated) trajectories
      individual_plots[[paste("simple_joined", id)]] <- ggplot2::ggplot(ind_data, ggplot2::aes(x = time, y = score)) +
        ggplot2::geom_point() +
        ggplot2::geom_line() +
        ggplot2::ggtitle(paste("Simple-Joined Trajectory for", id))

      # OLS trajectories
      individual_plots[[paste("ols", id)]] <- ggplot2::ggplot(ind_data, ggplot2::aes(x = time, y = predicted_score)) +
        ggplot2::geom_point() +
        ggplot2::geom_line() +
        ggplot2::ggtitle(paste("OLS Trajectory for", id))
    }
  }

  if (hist == "y") {
    # Histogram
    histogram_plot <- ggplot2::ggplot(data, ggplot2::aes(x = predicted_score)) +
      ggplot2::geom_histogram() +
      ggplot2::ggtitle("Histogram of OLS Predicted Scores")
  }

  # Return a list containing estimated values and the plots
  result <- list(estimated_values = estimated_values,
                 group_plots = group_plots,
                 individual_plots = individual_plots,
                 histogram_plot = histogram_plot)

  return(result)
}

