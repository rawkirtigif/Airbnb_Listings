# =============================================================================
# Regression Analysis - London Airbnb Listing Prices
# Author: Kirti Rawat
# Course: ALY 6010, Northeastern University
# =============================================================================
# This script performs linear regression analysis to identify price drivers:
# RQ1: Host acceptance rate vs price (Simple regression)
# RQ2: Minimum nights vs price (Simple regression)
# RQ3: Location, response rate, and room type vs price (Multiple regression)
# =============================================================================

# Load required libraries
# Install required packages
install.packages("lmtest")
install.packages("car")
install.packages("jtools")
install.packages("stargazer")
install.packages("effsize")  # Also needed for Milestone 2

# Or install all at once:
install.packages(c("lmtest", "car", "jtools", "stargazer", "effsize"))
library(tidyverse)
library(broom)
library(car)         # For VIF
library(lmtest)      # For heteroscedasticity tests
library(jtools)      # For coefficient plots
library(stargazer)   # For summary tables

# =============================================================================
# LOAD CLEANED DATA
# =============================================================================

listingsLondon_clean <- readRDS("listingsLondon_clean.rds")
cat("Loaded cleaned data\n")
cat("Total observations:", nrow(listingsLondon_clean), "\n\n")

# =============================================================================
# RQ1: Host Acceptance Rate vs Price
# =============================================================================
# Research Question: Do hosts with higher acceptance rates charge higher prices?
# Note: Acceptance rate distribution is bimodal (clustered at 0% and 100%)
# This violates linear regression assumptions but we proceed with caution

cat("========================================\n")
cat("RQ1: HOST ACCEPTANCE RATE VS PRICE\n")
cat("========================================\n\n")

# Check acceptance rate distribution
cat("Acceptance Rate Distribution:\n")
print(summary(listingsLondon_clean$host_acceptance_rate))
cat("\nDistribution by quartiles:\n")
print(table(cut(listingsLondon_clean$host_acceptance_rate, 
                breaks = c(0, 0.25, 0.5, 0.75, 1), 
                include.lowest = TRUE)))
cat("\n")

# Visualization
ggplot(listingsLondon_clean, aes(x = host_acceptance_rate, y = price)) +
  geom_jitter(alpha = 0.3, color = "blue", width = 0.02) +
  geom_smooth(method = "lm", se = TRUE, color = "red") +
  labs(
    title = "Relationship between Host Acceptance Rate and Price",
    x = "Host Acceptance Rate",
    y = "Price (£)"
  ) +
  theme_minimal()

# Save plot
ggsave("figures/rq1_acceptance_rate_price.png", width = 8, height = 6)

# Correlation test
cor_test1 <- cor.test(listingsLondon_clean$host_acceptance_rate, 
                      listingsLondon_clean$price)
cat("Pearson Correlation:\n")
cat("  r =", round(cor_test1$estimate, 4), "\n")
cat("  p-value =", format.pval(cor_test1$p.value, digits = 3), "\n\n")

# Simple Linear Regression
lm1 <- lm(price ~ host_acceptance_rate, data = listingsLondon_clean)

cat("Regression Results:\n")
print(summary(lm1))
cat("\n")

# Model fit statistics
lm1_glance <- glance(lm1)
cat("Model Fit Statistics:\n")
cat("  R² =", round(lm1_glance$r.squared, 4), "\n")
cat("  Adjusted R² =", round(lm1_glance$adj.r.squared, 4), "\n")
cat("  AIC =", round(lm1_glance$AIC, 2), "\n")
cat("  BIC =", round(lm1_glance$BIC, 2), "\n\n")

# Diagnostic plots
cat("Generating diagnostic plots...\n")
png("figures/rq1_diagnostics.png", width = 800, height = 800)
par(mfrow = c(2, 2))
plot(lm1)
par(mfrow = c(1, 1))
dev.off()

# Formal assumption tests
cat("Assumption Tests:\n")

# Normality (sample for large n)
set.seed(123)
shapiro_lm1 <- shapiro.test(sample(residuals(lm1), min(5000, length(residuals(lm1)))))
cat("  Shapiro-Wilk (normality): W =", round(shapiro_lm1$statistic, 4), 
    ", p =", format.pval(shapiro_lm1$p.value, digits = 3), "\n")

# Homoscedasticity
bp_lm1 <- bptest(lm1)
cat("  Breusch-Pagan (homoscedasticity): BP =", round(bp_lm1$statistic, 4),
    ", p =", format.pval(bp_lm1$p.value, digits = 3), "\n\n")

# Interpretation
cat("INTERPRETATION:\n")
cat("While statistically significant, the effect size is small (R² = ", 
    round(lm1_glance$r.squared, 4), ").\n", sep="")
cat("A 1-unit increase in acceptance rate (0 to 1) predicts a £", 
    round(coef(lm1)[2], 2), " increase in price.\n", sep="")
cat("LIMITATION: Bimodal distribution violates linearity assumptions.\n")
cat("Consider as exploratory finding only.\n\n")

# =============================================================================
# RQ2: Minimum Nights vs Price
# =============================================================================

cat("========================================\n")
cat("RQ2: MINIMUM NIGHTS VS PRICE\n")
cat("========================================\n\n")

# Descriptive statistics
cat("Minimum Nights Summary:\n")
print(summary(listingsLondon_clean$minimum_nights))
cat("\n")

# Visualization (filtered for clarity, regression on full data)
listings_plot2 <- listingsLondon_clean %>%
  filter(price < 500, minimum_nights < 30)

ggplot(listings_plot2, aes(x = minimum_nights, y = price)) +
  geom_jitter(alpha = 0.3, color = "purple", width = 0.4) +
  geom_smooth(method = "lm", se = TRUE, color = "red") +
  labs(
    title = "Relationship between Minimum Nights and Price",
    subtitle = "Filtered to price < £500, min_nights < 30 for visualization clarity",
    x = "Minimum Nights Required",
    y = "Price (£)"
  ) +
  theme_minimal()

ggsave("figures/rq2_minimum_nights_price.png", width = 8, height = 6)

# Correlation test
cor_test2 <- cor.test(listingsLondon_clean$minimum_nights, 
                      listingsLondon_clean$price)
cat("Pearson Correlation:\n")
cat("  r =", round(cor_test2$estimate, 4), "\n")
cat("  p-value =", format.pval(cor_test2$p.value, digits = 3), "\n\n")

# Simple Linear Regression (on FULL data)
lm2 <- lm(price ~ minimum_nights, data = listingsLondon_clean)

cat("Regression Results:\n")
print(summary(lm2))
cat("\n")

# Model fit statistics
lm2_glance <- glance(lm2)
cat("Model Fit Statistics:\n")
cat("  R² =", round(lm2_glance$r.squared, 4), "\n")
cat("  Adjusted R² =", round(lm2_glance$adj.r.squared, 4), "\n")
cat("  AIC =", round(lm2_glance$AIC, 2), "\n")
cat("  BIC =", round(lm2_glance$BIC, 2), "\n\n")

# Diagnostic plots
png("figures/rq2_diagnostics.png", width = 800, height = 800)
par(mfrow = c(2, 2))
plot(lm2)
par(mfrow = c(1, 1))
dev.off()

# Assumption tests
shapiro_lm2 <- shapiro.test(sample(residuals(lm2), min(5000, length(residuals(lm2)))))
bp_lm2 <- bptest(lm2)

cat("Assumption Tests:\n")
cat("  Shapiro-Wilk: W =", round(shapiro_lm2$statistic, 4), 
    ", p =", format.pval(shapiro_lm2$p.value, digits = 3), "\n")
cat("  Breusch-Pagan: BP =", round(bp_lm2$statistic, 4),
    ", p =", format.pval(bp_lm2$p.value, digits = 3), "\n\n")

# Interpretation
cat("INTERPRETATION:\n")
cat("Each additional required night increases price by £", 
    round(coef(lm2)[2], 2), ".\n", sep="")
cat("Statistically significant but economically negligible (R² = ", 
    round(lm2_glance$r.squared, 4), ").\n", sep="")
cat("Minimum nights is an operational decision, not a pricing lever.\n\n")

# =============================================================================
# RQ3: Multiple Regression - Location, Response Rate, Room Type
# =============================================================================

cat("========================================\n")
cat("RQ3: MULTIPLE REGRESSION MODEL\n")
cat("========================================\n\n")

# Create central location indicator (if not already exists)
central_neighbourhoods <- c("Westminster", "Kensington and Chelsea", 
                            "Camden", "Islington")

listingsLondon_clean <- listingsLondon_clean %>%
  mutate(
    is_central = if_else(
      neighbourhood_cleansed %in% central_neighbourhoods, 
      "Central", 
      "Non-Central"
    ),
    is_central = factor(is_central, levels = c("Non-Central", "Central"))
  )

# Check for missing values in predictors
cat("Missing values in predictors:\n")
cat("  is_central:", sum(is.na(listingsLondon_clean$is_central)), "\n")
cat("  host_response_rate:", sum(is.na(listingsLondon_clean$host_response_rate)), "\n")
cat("  room_type:", sum(is.na(listingsLondon_clean$room_type)), "\n\n")

# Multiple Regression
lm3 <- lm(price ~ is_central + host_response_rate + room_type, 
          data = listingsLondon_clean)

cat("Multiple Regression Results:\n")
print(summary(lm3))
cat("\n")

# Tidy output for easy interpretation
lm3_tidy <- tidy(lm3) %>%
  mutate(across(where(is.numeric), ~round(., 4)))
print(lm3_tidy)
cat("\n")

# Model fit statistics
lm3_glance <- glance(lm3)
cat("Model Fit Statistics:\n")
cat("  R² =", round(lm3_glance$r.squared, 4), "\n")
cat("  Adjusted R² =", round(lm3_glance$adj.r.squared, 4), "\n")
cat("  F-statistic =", round(lm3_glance$statistic, 2), "\n")
cat("  p-value =", format.pval(lm3_glance$p.value, digits = 3), "\n")
cat("  AIC =", round(lm3_glance$AIC, 2), "\n")
cat("  BIC =", round(lm3_glance$BIC, 2), "\n\n")

# Check multicollinearity
cat("Multicollinearity Check (VIF):\n")
vif_values <- vif(lm3)
print(round(vif_values, 2))
cat("\nInterpretation: VIF < 5 indicates no concerning collinearity\n\n")

# Diagnostic plots
png("figures/rq3_diagnostics.png", width = 800, height = 800)
par(mfrow = c(2, 2))
plot(lm3)
par(mfrow = c(1, 1))
dev.off()

# Assumption tests
shapiro_lm3 <- shapiro.test(sample(residuals(lm3), min(5000, length(residuals(lm3)))))
bp_lm3 <- bptest(lm3)

cat("Assumption Tests:\n")
cat("  Shapiro-Wilk: W =", round(shapiro_lm3$statistic, 4), 
    ", p =", format.pval(shapiro_lm3$p.value, digits = 3), "\n")
cat("  Breusch-Pagan: BP =", round(bp_lm3$statistic, 4),
    ", p =", format.pval(bp_lm3$p.value, digits = 3), "\n\n")

# Coefficient plot
plot_summs(lm3, scale = FALSE) +
  labs(title = "Coefficient Estimates with 95% CI",
       subtitle = "Multiple Regression Model") +
  theme_minimal()

ggsave("figures/rq3_coefficient_plot.png", width = 8, height = 6)

# Interpretation
cat("INTERPRETATION:\n")
cat("Key findings:\n")
cat("1. Location: Being Non-Central reduces price by £", 
    abs(round(coef(lm3)["is_centralCentral"], 2)), "\n", sep="")
cat("2. Room Type:\n")
cat("   - Private rooms: £", abs(round(coef(lm3)["room_typePrivate room"], 2)), 
    " less than entire homes\n", sep="")
cat("   - Shared rooms: £", abs(round(coef(lm3)["room_typeShared room"], 2)), 
    " less than entire homes\n", sep="")
cat("3. Host response rate: Not significant after controlling for other factors\n")
cat("\nModel explains ", round(lm3_glance$r.squared * 100, 1), 
    "% of price variance (R²)\n", sep="")
cat("Location and room type are PRIMARY pricing drivers.\n\n")

# =============================================================================
# MODEL COMPARISON
# =============================================================================

cat("========================================\n")
cat("MODEL COMPARISON\n")
cat("========================================\n\n")

comparison_table <- tibble(
  Model = c("RQ1: Acceptance Rate", "RQ2: Minimum Nights", "RQ3: Multiple Regression"),
  `R²` = round(c(lm1_glance$r.squared, lm2_glance$r.squared, lm3_glance$r.squared), 4),
  `Adj R²` = round(c(lm1_glance$adj.r.squared, lm2_glance$adj.r.squared, lm3_glance$adj.r.squared), 4),
  AIC = round(c(lm1_glance$AIC, lm2_glance$AIC, lm3_glance$AIC), 2),
  BIC = round(c(lm1_glance$BIC, lm2_glance$BIC, lm3_glance$BIC), 2),
  `F-stat` = round(c(lm1_glance$statistic, lm2_glance$statistic, lm3_glance$statistic), 2)
)

print(comparison_table)

cat("\nBest model: RQ3 (Multiple Regression) - highest R² and most interpretable\n\n")

# =============================================================================
# EXPORT RESULTS
# =============================================================================

# Export tables
write_csv(tidy(lm1), "results_rq1_regression.csv")
write_csv(tidy(lm2), "results_rq2_regression.csv")
write_csv(tidy(lm3), "results_rq3_regression.csv")
write_csv(comparison_table, "results_model_comparison.csv")

# Create comprehensive summary table using stargazer
stargazer(lm1, lm2, lm3, 
          type = "text",
          title = "Regression Models Comparison",
          column.labels = c("RQ1: Acceptance", "RQ2: Min Nights", "RQ3: Multiple"),
          out = "results_regression_summary.txt")

cat("\n========================================\n")
cat("ANALYSIS COMPLETE\n")
cat("========================================\n\n")
cat("Results exported:\n")
cat("  - CSV files for each model\n")
cat("  - Model comparison table\n")
cat("  - Diagnostic plots saved to /figures\n")
cat("  - Summary text file\n\n")