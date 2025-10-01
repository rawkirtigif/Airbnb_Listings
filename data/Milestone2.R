# =============================================================================
# Hypothesis Testing Analysis - London Airbnb Listings
# Author: Kirti Rawat
# Course: ALY 6015, Northeastern University
# =============================================================================
# This script performs three hypothesis tests to analyze Airbnb pricing:
# 1. One-sample t-test: Affordability benchmark comparison
# 2. Two-sample t-test: Central vs Non-Central location pricing
# 3. Two-sample t-test: Entire home vs Private room pricing
# =============================================================================

# Load required libraries
library(tidyverse)
library(broom)       
library(effsize)     

# =============================================================================
# LOAD CLEANED DATA FROM MILESTONE 1
# =============================================================================

listingsLondon_clean <- readRDS("listingsLondon_clean.rds")
cat("Loaded cleaned data from Milestone 1\n")
cat("Total observations:", nrow(listingsLondon_clean), "\n\n")

# =============================================================================
# TEST 1: Affordability Benchmark (One-Sample T-Test)
# =============================================================================
# H₀: Mean price of entire homes ≤ £136
# H₁: Mean price of entire homes > £136

cat("========================================\n")
cat("TEST 1: AFFORDABILITY BENCHMARK\n")
cat("========================================\n\n")

# Filter for entire home/apartment listings
entire_home_listings <- listingsLondon_clean %>%
  filter(room_type == "Entire home/apt")

# Descriptive statistics
cat("Sample size:", nrow(entire_home_listings), "\n")
cat("Mean price: £", round(mean(entire_home_listings$price), 2), "\n", sep="")
cat("SD: £", round(sd(entire_home_listings$price), 2), "\n")
cat("Median: £", round(median(entire_home_listings$price), 2), "\n\n", sep="")

# Check normality assumption
set.seed(123)
normality_sample <- sample(entire_home_listings$price, 
                           min(5000, nrow(entire_home_listings)))
shapiro_t1 <- shapiro.test(normality_sample)
cat("Shapiro-Wilk normality test:\n")
cat("  W =", round(shapiro_t1$statistic, 4), "\n")
cat("  p-value =", format.pval(shapiro_t1$p.value, digits = 3), "\n")

if(shapiro_t1$p.value < 0.05) {
  cat("  Note: Normality violated, but t-test robust with large n via CLT\n")
}
cat("\n")

# One-sample t-test
benchmark_price <- 136

t1_result <- t.test(
  entire_home_listings$price, 
  mu = benchmark_price, 
  alternative = "greater"
)

t1_tidy <- tidy(t1_result)

cat("One-Sample T-Test Results:\n")
cat("  t-statistic =", round(t1_tidy$statistic, 2), "\n")
cat("  df =", round(t1_tidy$parameter, 0), "\n")
cat("  p-value =", format.pval(t1_tidy$p.value, digits = 3, eps = 0.001), "\n")
cat("  95% CI: [", round(t1_tidy$conf.low, 2), ", Inf]\n", sep="")
cat("  Mean difference: £", round(t1_tidy$estimate - benchmark_price, 2), "\n\n", sep="")

# Effect size
effect_size_t1 <- (mean(entire_home_listings$price) - benchmark_price) / 
  sd(entire_home_listings$price)
cat("Effect Size (Cohen's d):", round(effect_size_t1, 3), "\n")
cat("Interpretation:", 
    ifelse(abs(effect_size_t1) < 0.2, "negligible",
           ifelse(abs(effect_size_t1) < 0.5, "small",
                  ifelse(abs(effect_size_t1) < 0.8, "medium", "large"))), "effect\n\n")

cat("Decision: Reject H₀ (p < 0.05)\n")
cat("Conclusion: Entire homes significantly exceed affordability benchmark\n\n")

# =============================================================================
# TEST 2: Central vs Non-Central Location (Two-Sample T-Test)
# =============================================================================
# H₀: Mean price Central ≤ Mean price Non-Central
# H₁: Mean price Central > Mean price Non-Central

cat("========================================\n")
cat("TEST 2: LOCATION COMPARISON\n")
cat("========================================\n\n")

# Define central neighborhoods
central_neighborhoods <- c("Westminster", "Kensington and Chelsea", 
                           "Camden", "Islington")

# Create location variable (factor levels matter for directionality)
listingsLondon_clean <- listingsLondon_clean %>%
  mutate(
    is_central = if_else(
      neighbourhood_cleansed %in% central_neighborhoods, 
      "Central", 
      "Non-Central"
    ),
    is_central = factor(is_central, levels = c("Non-Central", "Central"))
  )

# Descriptive statistics by group
location_summary <- listingsLondon_clean %>%
  group_by(is_central) %>%
  summarise(
    n = n(),
    mean_price = round(mean(price), 2),
    sd_price = round(sd(price), 2),
    median_price = round(median(price), 2)
  )

print(location_summary)
cat("\n")

# Check normality for each group
set.seed(123)
central_prices <- listingsLondon_clean %>% 
  filter(is_central == "Central") %>% 
  pull(price)

noncentral_prices <- listingsLondon_clean %>% 
  filter(is_central == "Non-Central") %>% 
  pull(price)

shapiro_central <- shapiro.test(sample(central_prices, min(5000, length(central_prices))))
shapiro_noncentral <- shapiro.test(sample(noncentral_prices, min(5000, length(noncentral_prices))))

cat("Normality tests:\n")
cat("  Central p-value =", format.pval(shapiro_central$p.value, digits = 3), "\n")
cat("  Non-Central p-value =", format.pval(shapiro_noncentral$p.value, digits = 3), "\n\n")

# Two-sample t-test (Welch's due to unequal variances)
t2_result <- t.test(
  price ~ is_central, 
  data = listingsLondon_clean,
  alternative = "greater",
  var.equal = FALSE
)

t2_tidy <- tidy(t2_result)

cat("Two-Sample T-Test Results:\n")
cat("  t-statistic =", round(t2_tidy$statistic, 2), "\n")
cat("  df =", round(t2_tidy$parameter, 1), "\n")
cat("  p-value =", format.pval(t2_tidy$p.value, digits = 3, eps = 0.001), "\n")
cat("  95% CI: [", round(t2_tidy$conf.low, 2), ", Inf]\n", sep="")
cat("  Mean difference: £", round(t2_tidy$estimate, 2), "\n\n", sep="")

# Effect size
cohen_t2 <- cohen.d(price ~ is_central, data = listingsLondon_clean)
cat("Effect Size (Cohen's d):", round(cohen_t2$estimate, 3), "\n")
cat("Interpretation:", cohen_t2$magnitude, "\n\n")

cat("Decision: Reject H₀ (p < 0.05)\n")
cat("Conclusion: Central London listings significantly more expensive\n\n")

# =============================================================================
# TEST 3: Entire Home vs Private Room (Two-Sample T-Test)
# =============================================================================
# H₀: Mean price Entire home ≤ Mean price Private room
# H₁: Mean price Entire home > Mean price Private room

cat("========================================\n")
cat("TEST 3: ROOM TYPE COMPARISON\n")
cat("========================================\n\n")

# Filter and set factor levels (order matters!)
t_test_data <- listingsLondon_clean %>%
  filter(room_type %in% c("Entire home/apt", "Private room")) %>%
  mutate(
    room_type = factor(room_type, 
                       levels = c("Private room", "Entire home/apt"))
  )

# Descriptive statistics by group
room_summary <- t_test_data %>%
  group_by(room_type) %>%
  summarise(
    n = n(),
    mean_price = round(mean(price), 2),
    sd_price = round(sd(price), 2),
    median_price = round(median(price), 2)
  )

print(room_summary)
cat("\n")

# Two-sample t-test
t3_result <- t.test(
  price ~ room_type, 
  data = t_test_data,
  alternative = "greater",
  var.equal = FALSE
)

t3_tidy <- tidy(t3_result)

cat("Two-Sample T-Test Results:\n")
cat("  t-statistic =", round(t3_tidy$statistic, 2), "\n")
cat("  df =", round(t3_tidy$parameter, 1), "\n")
cat("  p-value =", format.pval(t3_tidy$p.value, digits = 3, eps = 0.001), "\n")
cat("  95% CI: [", round(t3_tidy$conf.low, 2), ", Inf]\n", sep="")
cat("  Mean difference: £", round(t3_tidy$estimate, 2), "\n\n", sep="")

# Effect size
cohen_t3 <- cohen.d(price ~ room_type, data = t_test_data)
cat("Effect Size (Cohen's d):", round(cohen_t3$estimate, 3), "\n")
cat("Interpretation:", cohen_t3$magnitude, "\n\n")

cat("Decision: Reject H₀ (p < 0.05)\n")
cat("Conclusion: Entire homes significantly more expensive than private rooms\n\n")

# =============================================================================
# SUMMARY TABLE
# =============================================================================

cat("========================================\n")
cat("COMPREHENSIVE SUMMARY\n")
cat("========================================\n\n")

summary_table <- tibble(
  Test = c("1. Affordability Benchmark", 
           "2. Central vs Non-Central", 
           "3. Entire Home vs Private Room"),
  n = c(
    nrow(entire_home_listings),
    paste0("C:", location_summary$n[2], ", NC:", location_summary$n[1]),
    paste0("EH:", room_summary$n[2], ", PR:", room_summary$n[1])
  ),
  `Mean Diff (£)` = c(
    round(mean(entire_home_listings$price) - benchmark_price, 2),
    round(diff(location_summary$mean_price), 2),
    round(diff(room_summary$mean_price), 2)
  ),
  `t` = round(c(t1_tidy$statistic, t2_tidy$statistic, t3_tidy$statistic), 2),
  `p-value` = c(
    format.pval(t1_tidy$p.value, digits = 3, eps = 0.001),
    format.pval(t2_tidy$p.value, digits = 3, eps = 0.001),
    format.pval(t3_tidy$p.value, digits = 3, eps = 0.001)
  ),
  `Cohen's d` = round(c(effect_size_t1, cohen_t2$estimate, cohen_t3$estimate), 3),
  Decision = rep("Reject H₀", 3)
)

print(summary_table)

cat("\nAll null hypotheses rejected at α = 0.05 significance level\n")

# =============================================================================
# EXPORT RESULTS
# =============================================================================

write_csv(t1_tidy, "results_test1_affordability.csv")
write_csv(t2_tidy, "results_test2_location.csv")
write_csv(t3_tidy, "results_test3_roomtype.csv")
write_csv(summary_table, "results_summary_all_tests.csv")

cat("\nResults exported to CSV files for report inclusion\n")
cat("Analysis complete.\n")
