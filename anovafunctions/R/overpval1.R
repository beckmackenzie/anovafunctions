#' Report Anova P-value
#'
#' Reports and determines the significance 
#'
#' @param formula A formula like y ~ group.
#' @param data A data frame.
#' @param alpha Significance level (default 0.05).
#' @return  A caption including model, p-value (<0.0001 printed if value is less than 0.0001), significance level (alpha), and if the effect of the variable is significant at the chosen significance level.
#' @importFrom stats aov
#' @export
report_overall_pval_anova <- function(formula, data, alpha = 0.05) {
  # fit ANOVA
  model <- aov(formula, data = data)
  
  # extract ANOVA table
  anova_table <- summary(model)[[1]]
  
  # extract p-value
  pval <- anova_table[["Pr(>F)"]][1]
  
  # format p-value
  pval_print <- ifelse(pval < 0.0001, "<0.0001", format(pval, digits = 4))
  
  # determine significance
  sig <- ifelse(pval < alpha, "SIGNIFICANT", "NOT SIGNIFICANT")
  
  # print results
  cat("ANOVA Report\n")
  cat("Model: ", deparse(formula), "\n")
  cat("P-value: ", pval_print, "\n")
  cat("Alpha: ", alpha, "\n")
  cat("Conclusion: The effect is", sig, "at the", alpha, "level.\n")
}