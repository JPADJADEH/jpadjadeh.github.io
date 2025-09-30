# EPPS 6302 Methods of Data Collection and Production
# Google Trends with R

# Install and load required packages (uncomment if needed)
# install.packages("gtrendsR")
library(gtrendsR)

# Load additional packages used for CSV import (already in your code)
library(readr)
library(dplyr)
library(stringr)

# Use imported data instead of fetching new data
# Your code for importing and processing the CSV
df_raw <- read_csv(file.choose(), skip = 2, show_col_types = FALSE)

df <- df_raw %>%
  rename(
    week     = Week,
    trump    = `Donald Trump: (United States)`,
    harris   = `Kamala Harris: (United States)`,
    election = `election: (United States)`
  ) %>%
  # Convert "<1" to 0.5, then to numeric
  mutate(across(-week, ~ as.numeric(str_replace(.x, "<1", "0.5")))) %>%
  mutate(week = as.Date(week))

# Display data structure and first few rows (as in your code)
glimpse(df)
head(df)

# Plot the imported data (mirroring professor's plot structure)
plot(df$week, df$trump, type = "l", col = "blue", xlab = "Week", ylab = "Interest", main = "Google Trends: Trump, Harris, Election")
lines(df$week, df$harris, col = "red")
lines(df$week, df$election, col = "green")
legend("topright", legend = c("Trump", "Harris", "Election"), col = c("blue", "red", "green"), lty = 1)

write.csv(df, "multiTimeline_analyzed.csv", row.names = FALSE)
saveRDS(df, "multiTimeline_analyzed.rds")



# Example: Tariff, China military, Taiwan
plot(gtrends(c("tariff"), time = "all"))
# data(countries)  # Uncomment if needed for country codes
plot(gtrends(c("tariff"), geo = "GB", time = "all"))
plot(gtrends(c("tariff"), geo = c("US", "GB", "TW"), time = "all"))
tg_lot = gtrends(c("tariff"), time = "all")
tc = gtrends(c("tariff", "China military", "Taiwan"), time = "all")
tc_df = data.frame(tc$interest_over_time)
plot(gtrends(c("tariff", "China military", "Taiwan"), time = "all"))
