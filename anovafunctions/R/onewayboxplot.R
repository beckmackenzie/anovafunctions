#'Box plot Plot One-way ANOVA with CI
#'
#'Generates box plot with confidence interval for mean.
#'
#'@param formula A formula like y ~ group (group variable must be categorical).
#' @param data A data frame.
#' @param alpha Significance level (default 0.05).
#' @return A box plot with confidence interval for mean.
#' @importFrom stats qt
#' @importFrom dplyr group_by summarise n
#' @importFrom rlang .data sym
#' @importFrom ggplot2 ggplot aes geom_boxplot geom_jitter geom_point geom_errorbar labs theme_minimal
#' @importFrom magrittr %>%
#' @export
#use with categorical x-variable/factor
boxplot_oneway_anova_ci <- function(formula, data,
                                    xlab = NULL, ylab = NULL,
                                    show_means = TRUE, alpha = 0.05) {
  
  # extract variable names
  vars <- all.vars(formula)
  y_var <- vars[1]
  x_var <- vars[2]
  
  # convert x to factor
  data[[x_var]] <- as.factor(data[[x_var]])
  
  # default axis labels
  if (is.null(xlab)) xlab <- x_var
  if (is.null(ylab)) ylab <- y_var
  
  # compute group stats
  group_stats <- data %>%
    group_by(.data[[x_var]]) %>%
    summarise(
      mean = mean(.data[[y_var]], na.rm = TRUE),
      n = n(),
      sd = sd(.data[[y_var]], na.rm = TRUE),
      se = sd / sqrt(n),
      tcrit = qt(1 - alpha/2, df = n - 1),
      ci_lwr = mean - tcrit * se,
      ci_upr = mean + tcrit * se,
      .groups = "drop"
    )
  
  # plot
  p <- ggplot(data, aes(x = !!sym(x_var), y = !!sym(y_var))) +
    geom_boxplot(fill = "#D8E6FF", alpha = 0.6, color = "#3366FF") +
    geom_jitter(width = 0.15, alpha = 0.5, color = "#272EF5") +
    labs(
      title = "One-Way ANOVA Plot",
      x = xlab,
      y = ylab
    ) +
    theme_minimal(base_size = 14)
  
  # add means + CI bars
  if (show_means) {
    p <- p +
      geom_point(
        data = group_stats,
        inherit.aes = FALSE,
        aes(x = !!sym(x_var), y = mean),
        color = "#F527BE", size = 3
      ) +
      geom_errorbar(
        data = group_stats,
        inherit.aes = FALSE,
        aes(x = !!sym(x_var), ymin = ci_lwr, ymax = ci_upr),
        width = 0.15,
        color = "#F527BE",
        linewidth = 1
      )
  }
  
  p
}