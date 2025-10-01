# Data Source

## Overview
This analysis uses public Airbnb listing data for London, provided by InsideAirbnb.

## Source
- **Provider:** [InsideAirbnb](http://insideairbnb.com/get-the-data/)
- **Location:** London, United Kingdom
- **Date Collected:** January 2025
- **License:** Creative Commons CC0 1.0 Universal (Public Domain)

## Data Files
The raw data is **not included** in this repository due to size constraints (85,000+ rows).

To reproduce this analysis:
1. Download `listings.csv` from [InsideAirbnb - London](http://insideairbnb.com/get-the-data/)
2. Place it in the project root directory
3. Run `code/01_data_cleaning.R` to generate `listingsLondon_clean.rds`

## Dataset Characteristics

### Original Dataset
- **Rows:** ~87,000 listings
- **Columns:** 75 variables
- **Key Variables:**
  - `price`: Nightly listing price (GBP)
  - `neighbourhood_cleansed`: London borough
  - `room_type`: Entire home/apt, Private room, Shared room, Hotel room
  - `host_acceptance_rate`: % of booking requests accepted
  - `host_response_rate`: % of inquiries responded to
  - `minimum_nights`: Minimum stay requirement

### Cleaned Dataset
- **Rows:** 85,432 (after removing outliers and data quality issues)
- **Price Range:** £42 - £382 (IQR method, outliers removed)
- **Processing Steps:**
  1. Removed listings with price < £5 (probable errors)
  2. Applied IQR outlier removal (1.5 × IQR)
  3. Converted percentage strings to numeric
  4. Factorized categorical variables

## Data Citation
If using this analysis or methodology:

```
Murray Cox and Inside Airbnb. (2025). London Airbnb Listings Data. 
Retrieved from http://insideairbnb.com/get-the-data/
```

## Privacy & Ethics
- All data is publicly available
- No personally identifiable information (PII) is analyzed or reported
- Host IDs are anonymized in the original dataset
