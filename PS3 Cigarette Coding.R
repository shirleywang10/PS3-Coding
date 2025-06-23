#a)
# Install required packages
install.packages("sandwich")
install.packages("lmtest")
install.packages("stargazer")

# Load libraries
library(sandwich)
library(lmtest)
library(stargazer)

# Load dataset from Desktop
data <- read.csv("~/Desktop/cigarettes1995.csv")

# Create log variables
data$log_packs <- log(data$packs)
data$log_price <- log(data$price)

# Run regression
model <- lm(log_packs ~ log_price, data = data)

# Robust standard errors
robust_se <- sqrt(diag(vcovHC(model, type = "HC1")))

# Output regression using stargazer
stargazer(model,
          type = "text",
          se = list(robust_se),
          title = "Regression of log(packs) on log(price)",
          dep.var.labels = "log(packs)",
          covariate.labels = "log(price)",
          omit.stat = c("f", "ser"))

#d)

# If not already done, install required packages
install.packages("sandwich")
install.packages("lmtest")
install.packages("stargazer")

# Load libraries
library(sandwich)
library(lmtest)
library(stargazer)

# Recreate log(price) if needed
data$log_price <- log(data$price)

# First-stage regression: log(price) ~ salestax
first_stage <- lm(log_price ~ salestax, data = data)

# Compute robust standard errors
robust_se_first <- sqrt(diag(vcovHC(first_stage, type = "HC1")))

# Output using stargazer with all statistics
stargazer(first_stage,
          type = "text",
          se = list(robust_se_first),
          title = "First-Stage Regression: log(price) on salestax",
          dep.var.labels = "log(price)",
          covariate.labels = "salestax")


#d)
# Generate fitted values from the first-stage regression
data$fitted_logprice <- fitted(first_stage)

# Optional: Check the first few values to confirm
head(data$fitted_logprice)

#e)
# Load packages
library(sandwich)
library(lmtest)
library(stargazer)

# Re-create log variables
data$log_packs <- log(data$packs)
data$log_price <- log(data$price)

# (a) OLS: log(packs) ~ log(price)
model_a <- lm(log_packs ~ log_price, data = data)
se_a <- sqrt(diag(vcovHC(model_a, type = "HC1")))

# (d) First stage: log(price) ~ salestax
model_d <- lm(log_price ~ salestax, data = data)
se_d <- sqrt(diag(vcovHC(model_d, type = "HC1")))

# (e) Second stage: log(packs) ~ fitted log(price)
data$fitted_logprice <- fitted(model_d)
model_e <- lm(log_packs ~ fitted_logprice, data = data)
se_e <- sqrt(diag(vcovHC(model_e, type = "HC1")))

# Stargazer output with robust SEs, R², Adj R², Residual Std. Error, F-statistics
stargazer(model_a, model_d, model_e,
          type = "text",
          se = list(se_a, se_d, se_e),
          column.labels = c("(a) OLS", "(d) 1st Stage", "(e) 2nd Stage"),
          title = "Regression Results with Robust SEs and Full Statistics",
          dep.var.labels = c("log(packs)", "log(price)", "log(packs)"),
          covariate.labels = c("log(price)", "salestax", "fitted log(price)"),
          keep.stat = c("n", "rsq", "adj.rsq", "ser", "f"))  # Keep all key stats



          
#f)
# Install package if not already installed
install.packages("AER")

# Load all required packages
library(AER)
library(sandwich)
library(lmtest)
library(stargazer)

# Create log variables if not already done
data$log_packs <- log(data$packs)
data$log_price <- log(data$price)

# (a) OLS: log(packs) ~ log(price)
model_a <- lm(log_packs ~ log_price, data = data)
se_a <- sqrt(diag(vcovHC(model_a, type = "HC1")))

# (e) Manual 2SLS: log(packs) ~ fitted logprice
model_d <- lm(log_price ~ salestax, data = data)
data$fitted_logprice <- fitted(model_d)
model_e <- lm(log_packs ~ fitted_logprice, data = data)
se_e <- sqrt(diag(vcovHC(model_e, type = "HC1")))

# (f) IV regression using ivreg
model_f <- ivreg(log_packs ~ log_price | salestax, data = data)
se_f <- sqrt(diag(vcovHC(model_f, type = "HC1")))

# Stargazer table with all 3 models and robust SE
stargazer(model_a, model_e, model_f,
          type = "text",
          se = list(se_a, se_e, se_f),
          column.labels = c("(a) OLS", "(e) Manual 2SLS", "(f) ivreg"),
          title = "OLS vs Manual 2SLS vs IV Regression using ivreg",
          dep.var.labels = "log(packs)",
          covariate.labels = c("log(price)", "fitted log(price)"),
          omit.stat = c("ser", "rsq", "adj.rsq"))


# Install package if not already installed
install.packages("AER")

# Load all required packages
library(AER)
library(sandwich)
library(lmtest)
library(stargazer)

# Create log variables if not already done
data$log_packs <- log(data$packs)
data$log_price <- log(data$price)

# (a) OLS: log(packs) ~ log(price)
model_a <- lm(log_packs ~ log_price, data = data)
se_a <- sqrt(diag(vcovHC(model_a, type = "HC1")))

# (e) Manual 2SLS: log(packs) ~ fitted logprice
model_d <- lm(log_price ~ salestax, data = data)
data$fitted_logprice <- fitted(model_d)
model_e <- lm(log_packs ~ fitted_logprice, data = data)
se_e <- sqrt(diag(vcovHC(model_e, type = "HC1")))

# (f) IV regression using ivreg
model_f <- ivreg(log_packs ~ log_price | salestax, data = data)
se_f <- sqrt(diag(vcovHC(model_f, type = "HC1")))

# Stargazer table with all 3 models and robust SEs and full stats
stargazer(model_a, model_e, model_f,
          type = "text",
          se = list(se_a, se_e, se_f),
          column.labels = c("(a) OLS", "(e) Manual 2SLS", "(f) ivreg"),
          title = "OLS vs Manual 2SLS vs IV Regression using ivreg",
          dep.var.labels = "log(packs)",
          covariate.labels = c("log(price)", "fitted log(price)"),
          keep.stat = c("n", "rsq", "adj.rsq", "ser", "f"))

#g)
# Create log(income) if not already created
data$log_income <- log(data$income)

# Run OLS regression with income as a control
model_income_control <- lm(log(packs) ~ log(price) + log(income), data = data)

# Robust standard errors
se_income_control <- sqrt(diag(vcovHC(model_income_control, type = "HC1")))

# Show result in Stargazer
library(stargazer)
stargazer(model_income_control,
          type = "text",
          se = list(se_income_control),
          title = "Regression of log(packs) on log(price) with log(income) as Control",
          dep.var.labels = "log(packs)",
          covariate.labels = c("log(price)", "log(income)"),
          omit.stat = c("ser"))


#h)
# Make sure log(income) and log(price) exist
data$log_income <- log(data$income)
data$log_price <- log(data$price)

# Run the first-stage regression
first_stage_control <- lm(log_price ~ salestax + log_income, data = data)

# Compute heteroskedasticity-robust SE
se_first_stage_control <- sqrt(diag(vcovHC(first_stage_control, type = "HC1")))

# Display results with Stargazer
library(stargazer)
stargazer(first_stage_control,
          type = "text",
          se = list(se_first_stage_control),
          title = "First-Stage Regression: log(price) on salestax with log(income) as Control",
          dep.var.labels = "log(price)",
          covariate.labels = c("salestax", "log(income)"),
          omit.stat = c("ser"))
# Save fitted values into new variable in dataset
data$fitted_logprice_income <- fitted(first_stage_control)

# Optional: Check the first few values
head(data$fitted_logprice_income)

#i)
# Make sure log(packs), fitted values, and log(income) exist
data$log_packs <- log(data$packs)

# Run second-stage regression
second_stage_control <- lm(log_packs ~ fitted_logprice_income + log_income, data = data)

# Heteroskedasticity-robust standard errors
se_second_stage_control <- sqrt(diag(vcovHC(second_stage_control, type = "HC1")))

# Report results with stargazer
library(stargazer)
stargazer(second_stage_control,
          type = "text",
          se = list(se_second_stage_control),
          title = "Second-Stage Regression: log(packs) on fitted log(price) and log(income)",
          dep.var.labels = "log(packs)",
          covariate.labels = c("fitted log(price)", "log(income)"),
          omit.stat = c("ser"))

# Make sure necessary variables exist
data$log_packs <- log(data$packs)
data$log_price <- log(data$price)
data$log_income <- log(data$income)

# Load AER package
library(AER)

# IV regression: log(packs) ~ log(price) + log(income), instrumenting log(price) with salestax
iv_model <- ivreg(log_packs ~ log_price + log_income | salestax + log_income, data = data)

# Heteroskedasticity-robust standard errors
library(sandwich)
robust_se_iv <- sqrt(diag(vcovHC(iv_model, type = "HC1")))

# Output with stargazer
library(stargazer)
stargazer(iv_model,
          type = "text",
          se = list(robust_se_iv),
          title = "IV Regression: log(packs) on log(price) and log(income)",
          dep.var.labels = "log(packs)",
          covariate.labels = c("log(price)", "log(income)"),
          omit.stat = c("ser"))

# Ensure variables are defined
data$log_packs <- log(data$packs)
data$log_price <- log(data$price)
data$log_income <- log(data$income)

# (h) First-stage regression: log(price) ~ salestax + log(income)
model_h <- lm(log_price ~ salestax + log_income, data = data)
data$fitted_logprice_income <- fitted(model_h)
se_h <- sqrt(diag(vcovHC(model_h, type = "HC1")))

# (i1) Manual 2SLS: log(packs) ~ fitted log(price) + log(income)
model_i1 <- lm(log_packs ~ fitted_logprice_income + log_income, data = data)
se_i1 <- sqrt(diag(vcovHC(model_i1, type = "HC1")))

# (i2) IV regression: log(packs) ~ log(price) + log(income), instrumented by salestax + log(income)
library(AER)
model_i2 <- ivreg(log_packs ~ log_price + log_income | salestax + log_income, data = data)
se_i2 <- sqrt(diag(vcovHC(model_i2, type = "HC1")))

# Report using stargazer
library(stargazer)
stargazer(model_h, model_i1, model_i2,
          type = "text",
          se = list(se_h, se_i1, se_i2),
          column.labels = c("(h) 1st Stage", "(i1) Manual 2SLS", "(i2) IV via ivreg"),
          title = "Models for Price Instrumentation and Demand Estimation",
          dep.var.labels = c("log(price)", "log(packs)", "log(packs)"),
          covariate.labels = c("salestax", "log(income)", "fitted log(price)", "log(price)"),
          omit.stat = c("ser"))





# Load required packages
install.packages("AER")
install.packages("sandwich")
install.packages("lmtest")
install.packages("stargazer")

library(AER)
library(sandwich)
library(lmtest)
library(stargazer)

# Create necessary variables
data$log_packs <- log(data$packs)
data$log_price <- log(data$price)
data$log_income <- log(data$income)

# (h) First-stage regression: log(price) ~ salestax + log(income)
model_h <- lm(log_price ~ salestax + log_income, data = data)
data$fitted_logprice_income <- fitted(model_h)
se_h <- sqrt(diag(vcovHC(model_h, type = "HC1")))

# (i1) Manual 2SLS: log(packs) ~ fitted log(price) + log(income)
model_i1 <- lm(log_packs ~ fitted_logprice_income + log_income, data = data)
se_i1 <- sqrt(diag(vcovHC(model_i1, type = "HC1")))

# (i2) IV regression using ivreg: log(price) instrumented by salestax
model_i2 <- ivreg(log_packs ~ log_price + log_income | salestax + log_income, data = data)
se_i2 <- sqrt(diag(vcovHC(model_i2, type = "HC1")))

# Report all 3 models in Stargazer table
stargazer(model_h, model_i1, model_i2,
          type = "text",
          se = list(se_h, se_i1, se_i2),
          column.labels = c("(h) 1st Stage", "(i1) Manual 2SLS", "(i2) IV via ivreg"),
          title = "Models for Price Instrumentation and Demand Estimation",
          dep.var.labels = c("log(price)", "log(packs)", "log(packs)"),
          covariate.labels = c("salestax", "log(income)", "fitted log(price)", "log(price)"),
          omit.stat = c("ser"))


# Install packages if needed
install.packages("AER")
install.packages("sandwich")
install.packages("lmtest")
install.packages("stargazer")

# Load libraries
library(AER)
library(sandwich)
library(lmtest)
library(stargazer)

# Create necessary variables
data$log_packs <- log(data$packs)
data$log_price <- log(data$price)
data$log_income <- log(data$income)

# First-stage regression (h)
model_h <- lm(log_price ~ salestax + log_income, data = data)
data$fitted_logprice_income <- fitted(model_h)
se_h <- sqrt(diag(vcovHC(model_h, type = "HC1")))

# Manual 2SLS (i1)
model_i1 <- lm(log_packs ~ fitted_logprice_income + log_income, data = data)
se_i1 <- sqrt(diag(vcovHC(model_i1, type = "HC1")))

# IV via ivreg (i2)
model_i2 <- ivreg(log_packs ~ log_price + log_income | salestax + log_income, data = data)
se_i2 <- sqrt(diag(vcovHC(model_i2, type = "HC1")))

# Final Stargazer output with **correct labels auto-detected**
stargazer(model_h, model_i1, model_i2,
          type = "text",
          se = list(se_h, se_i1, se_i2),
          column.labels = c("1st Stage (h)", "2SLS Manual (i)", "2SLS ivreg (i)"),
          title = "Instrumental Variable Estimation with log(income) Control",
          omit.stat = c("ser"))

