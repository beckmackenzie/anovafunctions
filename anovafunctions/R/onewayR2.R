#' One_way ANOVA extract and Interpret R-Squared
#' 
#' Extracts R-squared value and prints a sentences detailing % of variation explaining anova model and % of variation due to variability within groups
#' @param formula A formula like y ~ group.
#' @param data A data frame.
#' @return A report containing response variable, grouping variable, R_squared, and interpretation. 
#' @importFrom stats aov
#' @export
report_oneway_anova_r2 <- function(formula, data) {
  
  # fit anova
  model <- aov(formula, data = data)
  anova_table <- summary(model)[[1]]
  

  # extract variable names
  vars <- all.vars(formula)
  y_var <- vars[1]
  group_var <- vars[2]
  
  # identify the correct row name for the factor
  ss_between <- anova_table[group_var, "Sum Sq"]
  ss_within  <- anova_table["Residuals", "Sum Sq"]
  ss_total   <- ss_between + ss_within
  
  # compute R²
  r2 <- ss_between / ss_total
  
  # print report
  cat("One-Way ANOVA R-squared\n")
  cat("------------------------\n")
  cat(paste("Response variable:", y_var, "\n"))
  cat(paste("Grouping variable:", group_var, "\n\n"))
  
  cat(paste("R-squared =", round(r2, 4), "\n\n"))
  
  cat("Interpretation:\n")
  cat(
    paste0(
      "-Approximately ", round(r2 * 100, 1), "% of the variation in ", y_var,
      " is explained by differences between the levels of ", group_var, ".\n",
      "- The remaining ", round((1 - r2) * 100, 1),
      "% of the variation is due to variability within groups.\n"
    )
  )
}