#' Two-Way ANOVA Flextable 
#' Fits a two-way ANOVA model and returns three flextables, ANOVA table, model summary, and parameter estimates, for formulas that have 2 categorical or 1 categorical/1 numeric.
#' 
#'  @param formula A formula like y ~ group1*group2 (with interaction) or y ~ group1 + group2 (without interaction).
#' @param data A data frame.
#' @param alpha Significance level (default 0.05).
#' @return A list of flextable objects.
#' @importFrom stats aov
#' @importFrom flextable flextable bold autofit
#' @importFrom magrittr %>%
#' @export
#works for 2 categorical factors and 1 categorical/1 numerical factors

twoway_anova_flextable <- function(formula, data, alpha = 0.05) {
  
  # fit 2-way ANOVA
  model <- aov(formula, data = data)
  anova_tbl <- as.data.frame(summary(model)[[1]])
  
  # add row names as a column
  anova_tbl$Term <- rownames(anova_tbl)
  rownames(anova_tbl) <- NULL
  
  # reorder columns
  anova_tbl <- anova_tbl[, c("Term", "Df", "Sum Sq", "Mean Sq", "F value", "Pr(>F)")]
  
  # format p-values (<0.0001 printed as "<0.0001")
  anova_tbl$`Pr(>F)` <- ifelse(
    anova_tbl$`Pr(>F)` < 0.0001,
    "<0.0001",
    sprintf("%.4f", anova_tbl$`Pr(>F)`)
  )
  
  # flextable 1: ANOVA Table
  ft1 <- flextable(anova_tbl) %>%
    bold(part = "header") %>%
    autofit()
  
  
  
  
  s <- summary(model)[[1]]
  
  # extract SS components
  ss_total  <- sum(s$`Sum Sq`)
  ss_error  <- s["Residuals", "Sum Sq"]
  df_error  <- s["Residuals", "Df"]
  mse       <- ss_error / df_error
  root_mse  <- sqrt(mse)
  
  # extract individual factor SS (auto-detected)
  ss_terms <- s$`Sum Sq`
  names_terms <- rownames(s)
  ss_factors <- ss_terms[names_terms != "Residuals"]
  
  # model SS = sum of all factor + interaction SS
  ss_model <- sum(ss_factors)
  
  # r-squared and adjusted R-squared
  r_sq <- ss_model / ss_total
  
  # number of parameters including intercept
  p <- length(ss_factors)
  n <- nrow(model$model)
  
  adj_r_sq <- 1 - ((1 - r_sq) * (n - 1) / (n - p - 1))
  
  # response mean
  y_mean <- mean(model$model[[1]])
  
  # coefficient of Variation
  cv <- 100 * root_mse / y_mean
  
  
  ### Build Model Summary Table ###
  result_df <- data.frame(
    `R-Square`          = r_sq,
    `Adj R-Square`      = adj_r_sq,
    `Model SS`          = ss_model,
    `Error SS`          = ss_error,
    `Total SS`          = ss_total,
    `Root MSE`          = root_mse,
    `Coeff Var (%)`     = cv,
    `Response Mean`     = y_mean
  )
  
  ft2 <- flextable(result_df) %>%
    bold(part = "header") %>%
    autofit()
  
  # parameter Estimates Table
  coef_mat <- summary.lm(model)$coefficients     # matrix
  coef_tbl <- as.data.frame(coef_mat)            # convert to data frame
  coef_tbl$Term <- rownames(coef_tbl)            # add term names
  rownames(coef_tbl) <- NULL
  
  # the columns
  
  coef_tbl <- coef_tbl[, c("Term", "Estimate", "Std. Error", "t value", "Pr(>|t|)")]
  
  # format p-values
  coef_tbl$`Pr(>|t|)` <- ifelse(coef_tbl$`Pr(>|t|)` < 0.0001,
                                "<0.0001",
                                sprintf("%.4f", coef_tbl$`Pr(>|t|)`))
  
  ft3 <- flextable(coef_tbl) %>% bold(part = "header") %>% autofit()
  
  # return all three tables
  return(list(
    anova_table = ft1,
    summary_table = ft2,
    parameter_estimates = ft3
  ))
}