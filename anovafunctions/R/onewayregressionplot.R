#'Regression Plot One-way ANOVA with CI and PI
#'
#'Fits a linear regression based on ANOVA model and returns scatter plot with regression line, CI band, and PI band. 
#'
#'@param formula A formula like y ~ group (group variable must be numeric).
#' @param data A data frame.
#' @param alpha Significance level (default 0.05).
#' @return A scatter plot with regression line, confidence interval band, and prediction interval band
#' @importFrom stats aov predict
#' @importFrom ggplot2 ggplot geom_point geom_ribbon geom_line labs theme_minimal
#' @export
#use with numerical x-variable/factor
plot_anova_regression_ci_pi <- function(formula, data, alpha = 0.05,
                                        xlab = NULL, ylab = NULL) {
  
  # extract variable names
  vars <- all.vars(formula)
  y_var <- vars[1]
  x_var <- vars[2]
  
  # ff no custom labels supplied, use variable names
  if (is.null(xlab)) xlab <- x_var
  if (is.null(ylab)) ylab <- y_var
  
  # check x is numeric
  if (!is.numeric(data[[x_var]])) {
    stop("The x-variable must be numeric for this scatterplot regression.")
  }
  
  #fit anova model
  model <- aov(formula, data = data)
  
  # newdata for smooth line
  newdf <- data.frame(x = seq(min(data[[x_var]], na.rm = TRUE), max(data[[x_var]], na.rm = TRUE), length.out = 200))
  names(newdf) <- x_var
  
  # predictions
  ci <- predict(model, newdata = newdf, interval = "confidence", level = 1 - alpha)
  pi <- predict(model, newdata = newdf, interval = "prediction", level = 1 - alpha)
  
  newdf$fit <- ci[, "fit"]
  newdf$ci_lwr <- ci[, "lwr"]
  newdf$ci_upr <- ci[, "upr"]
  newdf$pi_lwr <- pi[, "lwr"]
  newdf$pi_upr <- pi[, "upr"]
  
  # plot
  ggplot() +
    geom_point(data = data, aes_string(x = x_var, y = y_var),
               color = "#272EF5") +
    
    ## prediction interval band
    geom_ribbon(
      data = newdf,
      aes_string(x = x_var, ymin = "pi_lwr", ymax = "pi_upr"),
      alpha = 0.15, fill = "#f57327"
    ) +
    
    ## confidence interval band
    geom_ribbon(
      data = newdf,
      aes_string(x = x_var, ymin = "ci_lwr", ymax = "ci_upr"),
      alpha = 0.4, fill = "#27F5C5"
    ) +
    
    ## fitted regression line
    geom_line(
      data = newdf,
      aes_string(x = x_var, y = "fit"),
      color = "#F527BE", linewidth = 1.2
    ) +
    
    labs(
      title = "Regression Fit from ANOVA with CI & PI Bands",
      x = xlab,
      y = ylab
    ) +
    theme_minimal(base_size = 14)
}