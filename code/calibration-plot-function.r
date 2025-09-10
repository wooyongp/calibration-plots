
# Function to create calibration plots for predictions
create_calibration_plot <- function(data, outcome, n_best_models = NULL, bins = NULL, by = NULL) {

  '
  data: data.frame with predictions and actual values
  outcome: outcome variable name
  n_best_models: number of best models to plot
  bins: number of bins to use(if NULL, no binning. if not NUll, observations are grouped into bins for better readability)
  by: columns to group by
  '

  pred_cols <- names(data)[grepl(paste0("^", outcome, "_predicted"), names(data))]
  
  if(!is.null(by) && length(by) == 1 && !(by %in% c("strata", "country_coded", "payment_condition_dollar", "intervention_start_week_commencing_date_num"))) {
    warning("Invalid 'by' parameter, using no grouping")
    by <- NULL
  }

  if(is.null(by)) {
  # Prepare data for plotting
  plot_data <- data |>
    select(all_of(c(outcome, pred_cols)))}
    else {
      plot_data <- data |>
        select(all_of(c(outcome, pred_cols)), all_of(by)) |> 
        mutate(across(all_of(by), as.factor))
    }
    
  
  plot_data <- plot_data |>
  pivot_longer(cols = all_of(pred_cols), 
                names_to = "method", 
                values_to = "predicted") |>
  mutate(method = gsub(paste0(outcome, "_predicted_?"), "", method),
          actual = .data[[outcome]]) |> 
  filter(!is.na(predicted) & !is.na(actual))

           
  if (!is.null(by) && by == "country_coded") {
    plot_data <- plot_data |> 
      group_by(method, country_coded) |> 
      mutate(mse = mean((predicted - actual)^2)) |> 
      ungroup()
  } else if (!is.null(by) && by == "payment_condition_dollar") {
    plot_data <- plot_data |> 
      group_by(method, payment_condition_dollar) |> 
      mutate(mse = mean((predicted - actual)^2)) |> 
      ungroup()
  } else if (!is.null(by) && by == "intervention_start_week_commencing_date_num") {
    plot_data <- plot_data |> 
      group_by(method, intervention_start_week_commencing_date_num) |> 
      mutate(mse = mean((predicted - actual)^2)) |> 
      ungroup()
  } else {
    plot_data <- plot_data |>
      group_by(method) |> 
      mutate(mse = mean((predicted - actual)^2)) |> 
      ungroup()
  }

  if (!is.null(n_best_models)) {
    temp <- plot_data |> select(method, mse) |> distinct(method, .keep_all = T) |> slice_min(mse, n=n_best_models)
    plot_data <- plot_data |> filter(method %in% temp$method)
  }
  
  
  # Create calibration plot
  p <- ggplot(plot_data) 
  
  
  if (!is.null(by)) {
    p <- p + 
      geom_point(aes(predicted, actual, color = .data[[by]], group = .data[[by]]), alpha = 0.4) +
      geom_smooth(aes(predicted, actual, color = .data[[by]], group = .data[[by]]), linetype = "dashed", method = "lm") +
      facet_wrap(~method, scales = "free")
  } else {
    p <- p + 
      geom_point(aes(predicted, actual), alpha = 0.4) + 
      geom_smooth(aes(predicted, actual, color = "fitted_line"), linetype = "dashed", method = "lm") +
      facet_wrap(~method, scales = "free")
  }

  p <- p +
      labs(title = "Calibration Plot for Prediction vs Actual",
          subtitle = outcome,
          x = "Predictions",
          y = "Actual Values",
          caption = paste0("n observations: ", nrow(plot_data), "\n", "Overall MSE: ", round(mean((plot_data$predicted - plot_data$actual)^2), 2))) +
          theme_minimal() +
          theme(legend.position = "bottom")

  if(!is.null(n_best_models)) {
    p <- p + labs(title = paste0("Calibration Plot for Prediction vs Actual (best ", n_best_models, " models)"))
  }

  return(p)
}

# Function to create Q-Q plots for prediction residuals
create_qq_plot <- function(data, outcome, n_best_models = NULL, by = NULL) {

  pred_cols <- names(data)[grepl(paste0("^", outcome, "_predicted"), names(data))]
  
  if(!is.null(by) && length(by) == 1 && !(by %in% c("country_coded", "payment_condition_dollar", "intervention_start_week_commencing_date_num"))) {
    warning("Invalid 'by' parameter, using no grouping")
    by <- NULL
  }
  
  
  if(is.null(by)) {
    # Prepare data for plotting
    plot_data <- data |>
      select(all_of(c(outcome, pred_cols))) 
  } else {
    plot_data <- data |>
      select(all_of(c(outcome, pred_cols)), all_of(by)) |> 
      mutate(across(all_of(by), as.factor))
  }
  
  plot_data <- plot_data |>
    pivot_longer(cols = all_of(pred_cols), 
                 names_to = "method", 
                 values_to = "predicted") |>
    mutate(method = gsub(paste0(outcome, "_predicted_?"), "", method),
           actual = .data[[outcome]],
           residual = actual - predicted) |>
    filter(!is.na(predicted) & !is.na(actual)) 

  if (!is.null(by) && by == "country_coded") {
    plot_data <- plot_data |>
      group_by(method, country_coded) |>
      mutate(mse = mean((predicted - actual)^2)) |>
      ungroup()
  } else if (!is.null(by) && by == "payment_condition_dollar") {
    plot_data <- plot_data |>
      group_by(method, payment_condition_dollar) |>
      mutate(mse = mean((predicted - actual)^2)) |>
      ungroup()
  } else if (!is.null(by) && by == "intervention_start_week_commencing_date_num") {
    plot_data <- plot_data |>
      group_by(method, intervention_start_week_commencing_date_num) |>
      mutate(mse = mean((predicted - actual)^2)) |>
      ungroup()
  } else {
    plot_data <- plot_data |>
      group_by(method) |>
      mutate(mse = mean((predicted - actual)^2)) |>
      ungroup()
  }

  if (!is.null(n_best_models)) {
    temp <- plot_data |> select(method, mse) |> distinct(method, .keep_all = T) |> slice_min(mse, n=n_best_models)
    plot_data <- plot_data |> filter(method %in% temp$method)
  }
  
  # Create Q-Q plot
  p <- ggplot(plot_data)
  
  if (!is.null(by)) {
    p <- p +
      stat_qq(aes(sample = residual, color = .data[[by]]), alpha = 0.4) +
      stat_qq_line(aes(sample = residual, color = .data[[by]]), linetype = "dashed") +
      facet_wrap(~ method, scales = "free")
  } else {
    p <- p +
      stat_qq(aes(sample = residual), color = "#F8766D", alpha = 0.4) +
      stat_qq_line(aes(sample = residual), color = "red", linetype = "dashed") +
      facet_wrap(~method, scales = "free")
  }

  p <- p +
    labs(title = "Q-Q Plot for Prediction Residuals",
         subtitle = outcome,
         x = "Theoretical Quantiles",
         y = "Sample Quantiles",
         caption = paste0("n observations: ", nrow(plot_data), "\n", "Overall MSE: ", round(mean((plot_data$predicted - plot_data$actual)^2), 2))) +
    theme_minimal() +
    theme(legend.position = "bottom")
  
  if(!is.null(n_best_models)) {
    p <- p + labs(title = paste0("Q-Q Plot for Prediction Residuals (best ", n_best_models, " models)"))
  }
  
  return(p)
}