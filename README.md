# Google Reviews Dashboard

This Shiny application provides a comprehensive dashboard for visualizing Google reviews data and analyzing sentiment scores. It includes features for dynamically filtering data, visualizing trends, and analyzing review metrics across multiple dimensions.

## Features

- **Interactive Filters:** Users can dynamically filter data based on restaurant name, review date, and rating.
- **Data Exploration:** Visualize data through various graphs including:
  - Rating distribution for restaurants and reviews.
  - Monthly and yearly review counts.
  - Trends in average ratings over time with trend indicators.
  - Sentiment score of reviews.
  - Rating and sentiment distribution accross cuisines.
  - Map for displaying the locations of the restaurants. 
- **Customizable UI:** Tailored visual experience using external CSS for styling.

## Project Structure

- `styles.css` - External CSS file for styling the application.
- `ui.R` - User interface definition.
- `server.R` - Server logic of the Shiny application.
- `app.R` - Application initialization script. This file must be executed to run the dashboard.
- `Capstone_Preprocessing.R` - Preprocessing script for the project data.
- `Restaurant_Information.RData` - Data file containing detailed information about the restaurants.
- `review_data_cleaned.RData` - Cleaned review data in RData format.
- `review_data_cleaned.csv` - Cleaned review data in CSV format.
- `review_data_with_sentiment.csv` - Review data with sentiment scores in CSV format. This is the csv used for the dashboard.
- `README.md` - Project description and setup instructions.

## Running the Dashboard

To run the Shiny dashboard, execute the `app.R` file. This script initializes the application and launches the dashboard.

## Setup Instructions

1. **Clone the repository:**
```sh
   git clone https://github.com/Humayed93/Dashboard-Google-Reviews.git
```
2. **Navigate to the project directory:**
```sh 
  cd Dashboard-Google-Reviews
```
3. **Install the required packages:**
```sh 
  install.packages(c("shiny", "shinyWidgets", "leaflet", "plotly", "DT", "shinythemes", "tidyverse", "scales", "lattice", "factoextra", "lubridate", "this.path"))
```
