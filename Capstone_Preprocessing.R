library(tidyverse)
library(stringr)
library(lubridate)
library(dplyr)


# Clean Review Data -------------------------------------------------------

review_data <- read.csv("./Outscraper-202403301950517c03.csv")

# Remove rows with missing values in the 'review' column
review_data <- review_data[!is.na(review_data$review_rating),]

# Parse the datetime strings into POSIXct objects
review_data$datetime_parsed <- mdy_hms(review_data$review_datetime_utc)

# Extract weekday, month, and year
review_data$weekday <- wday(review_data$datetime_parsed, label = TRUE)
review_data$month <- month(review_data$datetime_parsed, label = TRUE)
review_data$year <- year(review_data$datetime_parsed)

head(review_data)

# Select only the 'review' and 'review_rating' columns with tidyverse approach
review_data_cleaned <- review_data %>% 
  select(name, google_id, place_id, location_link, reviews, rating, review_rating, review_text,
         datetime_parsed, weekday, month, year, reviews_per_score_1, reviews_per_score_2, reviews_per_score_3, 
         reviews_per_score_4, reviews_per_score_5, review_questions_Price.per.person)

save(review_data_cleaned, file = "review_data_cleaned.RData")


# Clean Restaurant Information File ---------------------------------------

restaurant_information <- read.csv("./Restaurant_information.csv")

# Drop the column using select()
restaurant_information <- select(restaurant_information, -'place_id', -'category', -'query', -'subtypes')

# Check restaurant types
unique(restaurant_information$type)

# Make various column descriptions uniform
restaurant_information$type <- gsub("restaurant", "", restaurant_information$type, ignore.case = TRUE)

restaurant_information$type <- gsub("Pizza delivery", "Italian", restaurant_information$type, ignore.case = TRUE)

restaurant_information$type <- gsub("Pizza", "Italian", restaurant_information$type, ignore.case = TRUE)

restaurant_information$type <- gsub("Tapas bar", "Spanish", restaurant_information$type, ignore.case = TRUE)

restaurant_information$type <- gsub("Tapas", "Spanish", restaurant_information$type, ignore.case = TRUE)

restaurant_information$type <- gsub("Bar & grill", "Grill", restaurant_information$type, ignore.case = TRUE)

restaurant_information$type <- gsub("Cake shop|Chocolate cafe", "Dessert", restaurant_information$type, ignore.case = TRUE)

restaurant_information$type <- gsub("Catalonian", "Spanish", restaurant_information$type, ignore.case = TRUE)

restaurant_information$type <- gsub("Tortilla shop", "Mexican", restaurant_information$type, ignore.case = TRUE)

restaurant_information$type <- gsub("Meat dish", "Meat", restaurant_information$type, ignore.case = TRUE)

restaurant_information$type <- gsub("Convenience store", "Store", restaurant_information$type, ignore.case = TRUE)

restaurant_information$type <- ifelse(restaurant_information$type == "" | is.na(restaurant_information$type), "General", restaurant_information$type)

# Trim whitespace that might be left after removal
restaurant_information$type <- trimws(restaurant_information$type)

# Save file as RDS file
save(restaurant_information, file = "./Restaurant_Information.RData")



# Merge both files --------------------------------------------------------

load('./review_data_cleaned.RData')
load('./Restaurant_information.RData')

# Left outer join on 'Google ID'
merged_data <- merge(review_data_cleaned, restaurant_information, by = "google_id", all.x = TRUE)

# Save the merged data frame as a new RDS file
save(merged_data, file = "./review_data_cleaned.RData")
