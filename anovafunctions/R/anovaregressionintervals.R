#' Confidence Interval and Prediction Interval from Two-way ANOVA Model
#' 
#' When given a specific values for each factor, function generates confidence interval and prediction for that value based on a two-way ANOVA model
#' 
#' @param formula A formula like y ~ group1*group2 (with interaction) or y ~ group1 + group2 (without interaction). (One group variable must be numeric).
#' @param data A data frame.
#' @param newdata a new data frame with desired factor 1 value and factor 2 value.
#' @param alpha Significance level (default 0.05).
#' @return Printed List of CI, Interpretation of CI, PI, and Interpretation of PI.
#' @importFrom stats aov predict
#' @export
interpret_anova_regression_intervals <- function(formula, data, newdata, alpha = 0.05) {
  
  # Fit ANOVA model
  model <- aov(formula, data = data)
  
  # Ensure model is aov
  if (!inherits(model, "aov")) {
    stop("Model must be an aov() object.")
  }
  
  # Ensure newdata is provided
  if (missing(newdata)) {
    stop("You must supply newdata with the predictor values.")
  }
  
  # Make predictions
  ci <- predict(model, newdata = newdata,
                interval = "confidence", level = 1 - alpha)
  
  pi <- predict(model, newdata = newdata,
                interval = "prediction", level = 1 - alpha)
  
  fit  <- ci[1]
  ci_l <- ci[2]
  ci_u <- ci[3]
  pi_l <- pi[2]
  pi_u <- pi[3]
  
  level_percent <- (1 - alpha) * 100
  
  # Print interpretation
  cat("Predictions for newdata:\n")
  print(newdata)
  cat("\n------------------------------\n")
  
  cat("Estimated mean =", round(fit, 4), "\n\n")
  
  cat(level_percent, "% Confidence Interval:\n", sep = "")
  cat("  CI =", round(ci_l, 4), "to", round(ci_u, 4), "\n")
  cat("Interpretation: We are ", level_percent,
      "% confident that the TRUE MEAN response lies in this interval.\n\n", sep = "")
  
  cat(level_percent, "% Prediction Interval:\n", sep = "")
  cat("  PI =", round(pi_l, 4), "to", round(pi_u, 4), "\n")
  cat("Interpretation: A SINGLE FUTURE observation is expected to fall in this interval.\n")
  
  invisible(NULL)
}