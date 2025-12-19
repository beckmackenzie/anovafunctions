#' Confidence Interval and Prediction Interval from One-way ANOVA Model
#' 
#' When given a specific x value, function generates confidence interval and prediction for that value basis on a one-way ANOVA Model
#' 
#' @param formula A formula like y ~ group (group variable must be numeric).
#' @param data A data frame.
#' @param x_value A value of x-variable (must be numeric).
#' @param alpha Significance level (default 0.05).
#' @return Printed List of CI, Interpretation of CI, PI, and Interpretation of PI.
#' @importFrom stats aov
#' @export
interpret_one_anova_intervals <- function(formula, data, x_value, alpha = 0.05) {
  
  # extract variable names
  vars <- all.vars(formula)
  y_var <- vars[1]
  x_var <- vars[2]
  
  # check numeric x
  if (!is.numeric(data[[x_var]])) {
    stop("x-variable must be numeric to compute CI/PI.")
  }
  
  # fit ANOVA model
  model <- aov(formula, data = data)
  
  # build newdata for prediction
  newdata <- data.frame(x_value)
  names(newdata) <- x_var
  
  # confidence level label (e.g. 95)
  conf_level <- (1 - alpha) * 100
  
  # get CI and PI
  ci <- predict(model, newdata = newdata, interval = "confidence", level = 1 - alpha)
  pi <- predict(model, newdata = newdata, interval = "prediction", level = 1 - alpha)
  
  fit  <- ci[1]
  ci_l <- ci[2]
  ci_u <- ci[3]
  pi_l <- pi[2]
  pi_u <- pi[3]
  
  # print interpretation
  cat("Prediction for", x_var, "=", x_value, "\n")
  cat("--------------------------------------\n")
  cat("Estimated mean", y_var, "=", round(fit, 4), "\n\n")
  
  cat(conf_level, "% Confidence Interval (mean response):\n", sep = "")
  cat("  CI =", round(ci_l, 4), "to", round(ci_u, 4), "\n")
  cat("Interpretation: We are ", conf_level, "% confident that the TRUE MEAN ", 
      y_var, " for x = ", x_value, " lies within this interval.\n\n", sep = "")
  
  cat(conf_level, "% Prediction Interval (individual response):\n", sep = "")
  cat("  PI =", round(pi_l, 4), "to", round(pi_u, 4), "\n")
  cat("Interpretation: We are ", conf_level, "% confident that a SINGLE FUTURE observation of ",
      y_var, " at x = ", x_value, " will fall within this interval.\n", sep = "")
  
}