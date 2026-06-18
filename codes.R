required_packages <- c("betareg", "margins")
for (pkg in required_packages) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
}

df <- read.csv("./data_2025.csv", header = TRUE, encoding = "UTF-8")

df$consumption_rate <- (df$consumption_per_capita * df$pop_total) / (df$gdp * 10000)
df$gdp_per_capita <- (df$gdp / df$pop_total) * 10000
df$income_share <- df$disposable_income / df$gdp_per_capita
df$public_budget_per_capita <- (df$public_budget / df$pop_total) * 10000
df$secondary_share <- df$secondary_industry / df$gdp
df$tertiary_share <- df$tertiary_industry / df$gdp

covariates <- c("gdp_per_capita", "income_share", "public_budget_per_capita", "secondary_share", "tertiary_share")
df[covariates] <- lapply(df[covariates], function(x) as.numeric(scale(x)))

var_names <- c(
  "gdp_per_capita" = "人均GDP (Z值)",
  "income_share" = "居民收入占GDP比重 (Z值)",
  "public_budget_per_capita" = "人均一般公共预算支出 (Z值)",
  "secondary_share" = "第二产业占比 (Z值)",
  "tertiary_share" = "第三产业占比 (Z值)"
)

calculate_vif <- function(model) {
  X <- model.matrix(model)[, -1]
  if (is.null(dim(X))) return(c("单一变量" = 1))
  n <- ncol(X)
  vif_values <- numeric(n)
  names(vif_values) <- colnames(X)
  for (i in 1:n) {
    y <- X[, i]
    x <- X[, -i]
    r_squared <- summary(lm(y ~ x))$r.squared
    vif_values[i] <- 1 / (1 - r_squared)
  }
  return(vif_values)
}

report_variable_importance <- function(model) {
  coef_values <- coef(model)[-1]
  names(coef_values) <- var_names[names(coef_values)]
  coef_sorted <- sort(abs(coef_values), decreasing = TRUE)
  print(round(coef_sorted, 4))
}

model1 <- lm(consumption_rate ~ gdp_per_capita + income_share + public_budget_per_capita + secondary_share + tertiary_share, data = df)
print(summary(model1))
vif1 <- calculate_vif(model1)
names(vif1) <- var_names[names(vif1)]
print(round(vif1, 3))
report_variable_importance(model1)

model2 <- lm(consumption_rate ~ gdp_per_capita + income_share + public_budget_per_capita + secondary_share, data = df)
print(summary(model2))
vif2 <- calculate_vif(model2)
names(vif2) <- var_names[names(vif2)]
print(round(vif2, 3))
report_variable_importance(model2)

model3 <- lm(consumption_rate ~ income_share + public_budget_per_capita + secondary_share, data = df)
print(summary(model3))
vif3 <- calculate_vif(model3)
names(vif3) <- var_names[names(vif3)]
print(round(vif3, 3))
report_variable_importance(model3)

model4 <- lm(consumption_rate ~ gdp_per_capita + income_share + public_budget_per_capita + tertiary_share, data = df)
print(summary(model4))
vif4 <- calculate_vif(model4)
names(vif4) <- var_names[names(vif4)]
print(round(vif4, 3))
report_variable_importance(model4)

model_beta <- betareg(
  consumption_rate ~ gdp_per_capita + income_share + public_budget_per_capita + secondary_share,
  data = df
)
print(summary(model_beta))

beta_margins <- margins(model_beta)
margin_summary <- summary(beta_margins)
print(margin_summary, digits = 4)

ols_coef <- coef(model2)[-1]
beta_ame <- margin_summary$AME
names(beta_ame) <- margin_summary$factor
common_vars <- intersect(names(ols_coef), names(beta_ame))

comparison <- data.frame(
  变量名称 = var_names[common_vars],
  OLS标准化系数 = round(ols_coef[common_vars], 6),
  Beta回归标准化AME = round(beta_ame[common_vars], 6),
  绝对差异 = round(abs(beta_ame[common_vars] - ols_coef[common_vars]), 6),
  差异百分比 = round(abs((beta_ame[common_vars] - ols_coef[common_vars]) / ols_coef[common_vars] * 100), 3)
)
rownames(comparison) <- NULL
print(comparison)
