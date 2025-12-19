#' Two-way ANOVA: Report R-squared and Adjusted R-squared
#'
#' Computes overall model R-squared and adjusted R-squared from a two-way ANOVA
#' (works with two categorical factors, with or without interaction).
#'
#' @param formula A formula like y ~ A + B or y ~ A * B.
#' @param data A data frame.
#' @return Invisibly returns a list with r2 and adj_r2; prints a report.
#' @export
report_twoway_anova_r2 <- function(formula, data) {
  
  model <- aov(formula, data = data)
  anova_table <- summary(model)[[1]]
  
  vars <- all.vars(formula)
  y_var <- vars[1]
  
  # model term labels on RHS (e.g., "A", "B", "A:B")
  rhs_terms <- attr(terms(formula), "term.labels")
  
  # identify model rows (everything except residuals)
  factor_row <- rownames(anova_table) != "Residuals"
  
  ss_model  <- sum(anova_table[factor_row, "Sum Sq"])
  df_model  <- sum(anova_table[factor_row, "Df"])
  
  ss_error  <- anova_table["Residuals", "Sum Sq"]
  df_error  <- anova_table["Residuals", "Df"]
  
  ss_total <- ss_model + ss_error
  
  # sample size from df relationship: df_total = n - 1 = df_model + df_error
  n <- df_model + df_error + 1
  
  r2 <- ss_model / ss_total
  
  # adjusted R^2 uses number of predictors (df_model) for ANOVA-style models
  adj_r2 <- 1 - (1 - r2) * ((n - 1) / (n - df_model - 1))
  
  cat("ANOVA R-squared Report\n")
  cat("------------------------\n")
  cat("Response variable:", y_var, "\n")
  cat("Model terms:", paste(rhs_terms, collapse = ", "), "\n\n")
  
  cat("R-squared =", round(r2, 4), "\n")
  cat("Adjusted R-squared =", round(adj_r2, 4), "\n\n")
  
  cat("Interpretation:\n")
  cat(round(r2 * 100, 1), "% of the variation in ", y_var,
      " is explained by the model terms: ",
      paste(rhs_terms, collapse = ", "), ".\n", sep = "")
  
  invisible(list(r2 = r2, adj_r2 = adj_r2))
}
