#' One-way ANOVA tables (flextable)
#'
#' Fits a one-way ANOVA model and returns three flextables:
#' ANOVA table, model summary, and parameter estimates.
#'
#' @param formula A formula like y ~ group.
#' @param data A data frame.
#' @param alpha Significance level (default 0.05).
#' @return A list of flextable objects.
#' @importFrom stats aov
#' @importFrom flextable flextable bold autofit
#' @importFrom magrittr %>%
#' @export
oneway_anova_flextable <- function(formula, data, alpha = 0.05) {
  model <- aov(formula, data = data)
  anova_tbl <- as.data.frame(summary(model)[[1]])
  
  anova_tbl$Term <- rownames(anova_tbl)
  rownames(anova_tbl) <- NULL
  
  anova_tbl <- anova_tbl[, c("Term", "Df", "Sum Sq", "Mean Sq", "F value", "Pr(>F)")]
  
  anova_tbl$`Pr(>F)` <- ifelse(anova_tbl$`Pr(>F)` < 0.0001,
                               "<0.0001",
                               sprintf("%.4f", anova_tbl$`Pr(>F)`))
  
  ft <- flextable(anova_tbl) %>% bold(part = "header") %>% autofit()
  
  s <- summary(model)[[1]]
  ss_model <- s[1, "Sum Sq"]
  ss_total <- sum(s[, "Sum Sq"])
  ss_error <- s["Residuals", "Sum Sq"]
  df_error <- s["Residuals", "Df"]
  
  r_sq     <- ss_model / ss_total
  mse      <- ss_error / df_error
  root_mse <- sqrt(mse)
  y_mean   <- mean(model$model[[1]])
  cv       <- 100 * root_mse / y_mean
  
  result_df <- data.frame(
    `R-Square` = r_sq,
    `Coeff Var` = cv,
    `Root MSE` = root_mse,
    `Response Mean` = y_mean
  )
  
  ft2 <- flextable(result_df) %>% bold(part = "header") %>% autofit()
  
  coef_mat <- summary.lm(model)$coefficients
  coef_tbl <- as.data.frame(coef_mat)
  coef_tbl$Term <- rownames(coef_tbl)
  rownames(coef_tbl) <- NULL
  
  coef_tbl <- coef_tbl[, c("Term", "Estimate", "Std. Error", "t value", "Pr(>|t|)")]
  
  coef_tbl$`Pr(>|t|)` <- ifelse(coef_tbl$`Pr(>|t|)` < 0.0001,
                                "<0.0001",
                                sprintf("%.4f", coef_tbl$`Pr(>|t|)`))
  
  ft3 <- flextable(coef_tbl) %>% bold(part = "header") %>% autofit()
  
  list(
    anova_table = ft,
    summary_table = ft2,
    parameter_estimates = ft3
  )
}



