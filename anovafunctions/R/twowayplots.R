#' Plots for Two-Way Anova 
#' 
#' Generates 4 Plots, Interaction Plot, Main Effect Factor1, Main Effect Factor2, Grouped Box Plot
#' 
#' @param data A data frame.
#' @param response response variable
#' @param factor1 factor/grouping variable 1
#' @param factor1 factor/grouping variable 2
#' @return 4 Plots, Interaction Plot, Main Effect Factor1, Main Effect Factor2, Grouped Box Plot
#' @importFrom rlang enquo as_name
#' @importFrom dplyr mutate
#' @importFrom ggplot2 ggplot aes stat_summary mean_cl_normal geom_boxplot position_dodge labs theme_bw
#' @importFrom magrittr %>%
#' @export
#' 
twoway_anova_plots <- function(data, response, factor1, factor2) {
  
  # Capture unquoted names
  resp <- enquo(response)
  f1   <- enquo(factor1)
  f2   <- enquo(factor2)
  
  # Convert factors if needed
  data <- data %>%
    mutate(
      !!f1 := as.factor(!!f1),
      !!f2 := as.factor(!!f2)
    )
  
  
  # 1. Interaction Plot
  
  interaction_plot <- ggplot(data, aes(x = !!f1, y = !!resp, 
                                       color = !!f2, group = !!f2)) +
    stat_summary(fun = mean, geom = "point", size = 3) +
    stat_summary(fun = mean, geom = "line") +
    stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.1) +
    labs(title = "Interaction Plot") +
    theme_bw()
  
  
  # 2. Main Effect Plot - Factor 1
  
  main_f1 <- ggplot(data, aes(x = !!f1, y = !!resp)) +
    stat_summary(fun = mean, geom = "point", size = 3) +
    stat_summary(fun = mean, geom = "line") +
    stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.1) +
    labs(title = paste("Main Effect of", as_name(f1))) +
    theme_bw()
  
  
  # 3. Main Effect Plot - Factor 2
  
  main_f2 <- ggplot(data, aes(x = !!f2, y = !!resp)) +
    stat_summary(fun = mean, geom = "point", size = 3) +
    stat_summary(fun = mean, geom = "line") +
    stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.1) +
    labs(title = paste("Main Effect of", as_name(f2))) +
    theme_bw()
  
  
  # 4. Grouped Boxplot
  
  grouped_box <- ggplot(data, aes(x = !!f1, y = !!resp, fill = !!f2)) +
    geom_boxplot(position = position_dodge(width = 0.7)) +
    labs(title = "Grouped Boxplot") +
    theme_bw()
  
  list(
    interaction_plot = interaction_plot,
    main_effect_factor1 = main_f1,
    main_effect_factor2 = main_f2,
    grouped_boxplot = grouped_box
  )
}