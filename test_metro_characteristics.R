# Test script for new CBSA and CSA characteristics support
library(tidycensus)

# Test CBSA characteristics
cat("Testing CBSA characteristics...\n")
cbsa_test <- get_estimates(
  geography = "cbsa",
  product = "characteristics",
  breakdown = c("SEX", "RACE"),
  vintage = 2024,
  year = 2024
)
cat("CBSA test - Rows:", nrow(cbsa_test), "\n")
cat("CBSA test - Sample:\n")
print(head(cbsa_test))

# Test CSA characteristics  
cat("\n\nTesting CSA characteristics...\n")
csa_test <- get_estimates(
  geography = "combined statistical area",
  product = "characteristics", 
  breakdown = c("AGEGROUP"),
  breakdown_labels = TRUE,
  vintage = 2024,
  year = 2024
)
cat("CSA test - Rows:", nrow(csa_test), "\n")
cat("CSA test - Sample:\n")
print(head(csa_test))

# Test time series
cat("\n\nTesting time series for CBSA...\n")
cbsa_ts <- get_estimates(
  geography = "metropolitan statistical area/micropolitan statistical area",
  product = "characteristics",
  breakdown = "SEX",
  vintage = 2024,
  time_series = TRUE
)
cat("Time series test - Years:", unique(cbsa_ts$year), "\n")