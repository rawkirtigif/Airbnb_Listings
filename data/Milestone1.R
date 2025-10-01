# =============================================================================
# Exploratory Data Analysis - London Airbnb Listings
# Author: Kirti Rawat
# Course: ALY 6010, Northeastern University
# =============================================================================

# Load libraries
library(tidyverse)
library(ggplot2)
library(skimr)
library(naniar)

# =============================================================================
# 1. DATA LOADING AND INITIAL EXPLORATION
# =============================================================================

# Load the original dataset
# Assumes: listingsLondon is already loaded in environment or:
# listingsLondon <- read_csv("listings.csv")

cat("Original dataset dimensions:", dim(listingsLondon), "\n")

# Initial overview
glimpse(listingsLondon)
skim(listingsLondon)

# =============================================================================
# 2. DATA TYPE CONVERSIONS
# =============================================================================

listingsLondon <- listingsLondon %>%
  mutate(
    # Convert price from character to numeric (remove $ and commas)
    price = as.numeric(gsub("[$,]", "", price)),
    
    # Convert percentage rates to numeric
    host_response_rate = as.numeric(gsub("%", "", host_response_rate)),
    host_acceptance_rate = as.numeric(gsub("%", "", host_acceptance_rate)),
    
    # Convert categorical variables to factors
    property_type = as.factor(property_type),
    room_type = as.factor(room_type),
    instant_bookable = as.factor(instant_bookable),
    host_is_superhost = as.factor(host_is_superhost)
  )

cat("\nAfter type conversions:\n")
summary(listingsLondon$price)
summary(listingsLondon$host_response_rate)
summary(listingsLondon$host_acceptance_rate)

# =============================================================================
# 3. MISSING DATA ANALYSIS
# =============================================================================

# Count missing values
missing_summary <- colSums(is.na(listingsLondon)) %>%
  sort(decreasing = TRUE) %>%
  head(20)

cat("\nTop 20 columns with missing values:\n")
print(missing_summary)

# Visualize missing data
vis_miss(listingsLondon, warn_large_data = FALSE)

# Drop columns with excessive missing data (>50%)
threshold <- nrow(listingsLondon) * 0.5
cols_to_drop <- names(listingsLondon)[colSums(is.na(listingsLondon)) > threshold]

cat("\nDropping columns with >50% missing:", paste(cols_to_drop, collapse = ", "), "\n")

listingsLondon <- listingsLondon %>%
  select(-any_of(c("license", "neighbourhood_group_cleansed", "calendar_updated")))

# =============================================================================
# 4. PRICE CLEANING (SINGLE CONSOLIDATED STEP)
# =============================================================================

cat("\n=== PRICE CLEANING ===\n")
cat("Original price range:", range(listingsLondon$price, na.rm = TRUE), "\n")

# Visualize BEFORE cleaning
ggplot(listingsLondon, aes(x = price)) +
  geom_histogram(binwidth = 50, fill = "steelblue", color = "black") +
  coord_cartesian(xlim = c(0, 1000)) +
  ggtitle("Price Distribution BEFORE Cleaning") +
  theme_minimal()

# Step 1: Remove NA prices
n_na_price <- sum(is.na(listingsLondon$price))
cat("Removing", n_na_price, "rows with NA prices\n")

listingsLondon <- listingsLondon %>%
  filter(!is.na(price))

# Step 2: Remove likely errors (prices < £5)
# Rationale: Prices under £5/night are likely data entry errors or test listings
n_low_price <- sum(listingsLondon$price < 5)
pct_low_price <- round(n_low_price / nrow(listingsLondon) * 100, 2)
cat("Removing", n_low_price, "listings with price < £5 (", pct_low_price, "% of data)\n", sep="")

listingsLondon <- listingsLondon %>%
  filter(price >= 5)

# Step 3: Remove extreme outliers using IQR method
Q1 <- quantile(listingsLondon$price, 0.25)
Q3 <- quantile(listingsLondon$price, 0.75)
IQR_value <- Q3 - Q1
lower_bound <- Q1 - 1.5 * IQR_value
upper_bound <- Q3 + 1.5 * IQR_value

n_outliers <- sum(listingsLondon$price < lower_bound | listingsLondon$price > upper_bound)
pct_outliers <- round(n_outliers / nrow(listingsLondon) * 100, 2)

cat("IQR bounds: [", round(lower_bound, 2), ", ", round(upper_bound, 2), "]\n", sep="")
cat("Removing", n_outliers, "outliers (", pct_outliers, "% of data)\n\n", sep="")

listingsLondon_clean <- listingsLondon %>%
  filter(price >= lower_bound & price <= upper_bound)

# Summary after cleaning
cat("Final cleaned price summary:\n")
print(summary(listingsLondon_clean$price))
cat("\nFinal dataset size:", nrow(listingsLondon_clean), "rows\n\n")

# Visualize AFTER cleaning
ggplot(listingsLondon_clean, aes(x = price)) +
  geom_histogram(binwidth = 20, fill = "steelblue", color = "black") +
  ggtitle("Price Distribution AFTER Cleaning") +
  xlab("Price (£)") + ylab("Count") +
  theme_minimal()

# =============================================================================
# 5. PROPERTY AND ROOM TYPE ANALYSIS
# =============================================================================

cat("=== PROPERTY & ROOM TYPE ANALYSIS ===\n")

# Room type distribution
room_type_counts <- listingsLondon_clean %>%
  count(room_type, sort = TRUE)
print(room_type_counts)

# Boxplot: Price by Room Type
ggplot(listingsLondon_clean, aes(x = room_type, y = price, fill = room_type)) +
  geom_boxplot() +
  ggtitle("Price Distribution by Room Type") +
  xlab("Room Type") + ylab("Price (£)") +
  theme_minimal() +
  theme(legend.position = "none")

# Top 10 property types
top_10_property <- listingsLondon_clean %>%
  count(property_type, sort = TRUE) %>%
  top_n(10, n) %>%
  pull(property_type)

# Boxplot: Price by Property Type (Top 10)
ggplot(listingsLondon_clean %>% filter(property_type %in% top_10_property),
       aes(x = reorder(property_type, price, FUN = median), y = price, fill = property_type)) +
  geom_boxplot() +
  coord_flip() +
  ggtitle("Price Distribution by Property Type (Top 10)") +
  xlab("Property Type") + ylab("Price (£)") +
  theme_minimal() +
  theme(legend.position = "none")

# =============================================================================
# 6. NEIGHBORHOOD ANALYSIS
# =============================================================================

cat("\n=== NEIGHBORHOOD ANALYSIS ===\n")

# Top 10 neighborhoods by listing count
top_10_neighborhoods <- listingsLondon_clean %>%
  count(neighbourhood_cleansed, sort = TRUE) %>%
  top_n(10, n) %>%
  pull(neighbourhood_cleansed)

print(top_10_neighborhoods)

# Boxplot: Price by Neighborhood (Top 10)
ggplot(listingsLondon_clean %>% filter(neighbourhood_cleansed %in% top_10_neighborhoods),
       aes(x = reorder(neighbourhood_cleansed, price, FUN = median), 
           y = price, 
           fill = neighbourhood_cleansed)) +
  geom_boxplot() +
  coord_flip() +
  ggtitle("Price Distribution by Neighborhood (Top 10)") +
  xlab("Neighborhood") + ylab("Price (£)") +
  theme_minimal() +
  theme(legend.position = "none")

# =============================================================================
# 7. REVIEWS ANALYSIS
# =============================================================================

cat("\n=== REVIEWS ANALYSIS ===\n")

# Create subset with reviews
listings_with_reviews <- listingsLondon_clean %>%
  filter(!is.na(number_of_reviews), number_of_reviews > 0)

cat("Listings with reviews:", nrow(listings_with_reviews), "\n")

# Reviews by room type
review_summary <- listings_with_reviews %>%
  group_by(room_type) %>%
  summarise(
    min_reviews = min(number_of_reviews),
    median_reviews = median(number_of_reviews),
    mean_reviews = mean(number_of_reviews),
    max_reviews = max(number_of_reviews),
    count = n()
  ) %>%
  arrange(desc(mean_reviews))

print(review_summary)

# Boxplot: Reviews by Room Type
ggplot(listings_with_reviews, aes(x = room_type, y = number_of_reviews, fill = room_type)) +
  geom_boxplot() +
  coord_cartesian(ylim = c(0, 200)) +
  ggtitle("Number of Reviews by Room Type") +
  xlab("Room Type") + ylab("Number of Reviews") +
  theme_minimal() +
  theme(legend.position = "none")

# Scatterplot: Reviews vs Rating
ggplot(listings_with_reviews %>% filter(!is.na(review_scores_rating)), 
       aes(x = number_of_reviews, y = review_scores_rating)) +
  geom_point(alpha = 0.3, color = "blue") +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  ggtitle("Relationship Between Number of Reviews and Ratings") +
  xlab("Number of Reviews") + 
  ylab("Review Rating") +
  theme_minimal()

# =============================================================================
# 8. HOST ACTIVITY ANALYSIS
# =============================================================================

cat("\n=== HOST ACTIVITY ANALYSIS ===\n")

# Count listings per host
host_listings <- listingsLondon_clean %>%
  count(host_id, name = "listings_count")

# Summary
cat("Host activity summary:\n")
print(summary(host_listings$listings_count))
cat("Hosts with 1 listing:", sum(host_listings$listings_count == 1), 
    "(", round(sum(host_listings$listings_count == 1)/nrow(host_listings)*100, 1), "%)\n", sep="")

# Histogram: Listings per Host
ggplot(host_listings, aes(x = listings_count)) +
  geom_histogram(binwidth = 1, fill = "steelblue", color = "black") +
  coord_cartesian(xlim = c(1, 20)) +
  ggtitle("Distribution of Listings per Host") +
  xlab("Number of Listings per Host") +
  ylab("Count of Hosts") +
  theme_minimal()

# =============================================================================
# 9. SAVE CLEANED DATA FOR MILESTONE 2
# =============================================================================

# Save as RDS (preserves R data types)
saveRDS(listingsLondon_clean, "listingsLondon_clean.rds")

# Optionally save as CSV
write_csv(listingsLondon_clean, "listingsLondon_clean.csv")

cat("\n=== DATA CLEANING COMPLETE ===\n")
cat("Cleaned data saved as:\n")
cat("  - listingsLondon_clean.rds (for Milestone 2)\n")
cat("  - listingsLondon_clean.csv (for backup)\n")
cat("\nFinal dataset: ", nrow(listingsLondon_clean), " rows × ", 
    ncol(listingsLondon_clean), " columns\n", sep="")
