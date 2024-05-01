library(tidyverse)
library(stringr)
library(lubridate)

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
