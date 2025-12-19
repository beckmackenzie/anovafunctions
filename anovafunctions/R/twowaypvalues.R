#' Report Anova P-values for each factor and interaction
#'
#' Reports and determines the significance. 
#'
#' @param formula A formula like y ~ group1*group2 (with interaction) or y ~ group1 + group2 (without interaction).
#' @param data A data frame.
#' @param alpha Significance level (default 0.05).
#' @return A list of p-values and interpretation for each factor (also p-value and interpretation for interaction if model includes interaction)
#' @importFrom stats aov
#' @export
report_pvals_anova <- function(formula, data, alpha = 0.05) {
  # fit ANOVA
  model <- aov(formula, data = data)
  
  # extract ANOVA table
  anova_table <- summary(model)[[1]]
  
  # extract p-value
  pval <- anova_table[["Pr(>F)"]][1]
  pval_2 <- anova_table[["Pr(>F)"]][2]
  pval_interact <-anova_table[["Pr(>F)"]][3]
  
  # format p-value
  pval_print <- ifelse(pval < 0.0001, "<0.0001", format(pval, digits = 4))
  pval_print_2 <- ifelse(pval_2 < 0.0001, "<0.0001", format(pval_2, digits = 4))
  pval_print_interact <- ifelse(pval_interact < 0.0001, "<0.0001", format(pval_interact, digits = 4))
  # determine significance
  sig <- ifelse(pval < alpha, "SIGNIFICANT", "NOT SIGNIFICANT")
  sig_2 <- ifelse(pval_2 < alpha, "SIGNIFICANT", "NOT SIGNIFICANT")
  sig_interact <- ifelse(pval_interact < alpha, "SIGNIFICANT", "NOT SIGNIFICANT")
  
  # print results
  cat("\nANOVA Report\n")
  cat("Model:", deparse(formula), "\n\n")
  
  cat("Factor 1 p-value:       ", pval_print, " --> ", sig, "\n")
  cat("Factor 2 p-value:       ", pval_print_2, " --> ", sig_2, "\n")
  cat("Interaction p-value:    ", pval_print_interact, " --> ", sig_interact, "\n")
  
  cat("\nAlpha =", alpha, "\n")
}





