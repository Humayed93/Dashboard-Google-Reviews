suppressPackageStartupMessages({
  library(shiny)
  library(shinyWidgets)
  library(leaflet)
  library(plotly)
  library(DT)
  library(shinythemes)
  library(tidyverse)
  library(scales)
  library(lattice)
  library(factoextra)
  library(lubridate)
  library('this.path')
})

# For data exploration
load('./review_data_cleaned.RData')

# Create variables outside of the server/ui function
# Extract unique values for filters
name <- sort(unique(review_data_cleaned$name))
years <- sort(unique(review_data_cleaned$year))
days <- sort(unique(review_data_cleaned$weekday))
months <- sort(unique(review_data_cleaned$month))

# Minimum and maximum annual income for slider
minReview <- min(review_data_cleaned$rating)
maxReview <- max(review_data_cleaned$rating)

ui <- navbarPage(
  "Dashboard Google Reviews",
  
  theme = shinytheme("yeti"),
  tags$head(
    includeCSS(file.path(this.dir(), "styles.css"))
  ),
  tabPanel("Introduction",
           sidebarLayout(
             sidebarPanel(
               width = 3,
               sliderInput("restaurantRatingTable",
                           "Restaurant Rating:",
                           min = minReview,
                           max = maxReview,
                           value = c(minReview, maxReview)),
               pickerInput("restaurantNameTable",
                           "Name of Restaurant:",
                           choices = name, multiple = TRUE, options = list(`actions-box` = TRUE)), 
               pickerInput("reviewDayTable",
                           "Day of Review:",
                           choices = days, multiple = TRUE, options = list(`actions-box` = TRUE)),
               pickerInput("reviewMonthTable",
                           "Month of Review:",
                           choices = months, multiple = TRUE, options = list(`actions-box` = TRUE)),
               pickerInput("reviewYearTable",
                           "Year of Review:",
                           choices = years, multiple = TRUE, options = list(`actions-box` = TRUE))
             ),
             mainPanel(
               width = 9,
               div(class="data-table-container",
                   dataTableOutput("filteredData_table"))
             )
           )
  ),
  tabPanel("Data Exploration",
           sidebarLayout(
             sidebarPanel(
               width = 3,
               sliderInput("restaurantRating",
                           "Restaurant Rating:",
                           min = minReview,
                           max = maxReview,
                           value = c(minReview, maxReview)),
               pickerInput("restaurantName",
                           "Name of Restaurant:",
                           choices = name, multiple = TRUE, options = list(`actions-box` = TRUE)), 
               pickerInput("reviewDay",
                           "Day of Review:",
                           choices = days, multiple = TRUE, options = list(`actions-box` = TRUE)),
               pickerInput("reviewMonth",
                           "Month of Review:",
                           choices = months, multiple = TRUE, options = list(`actions-box` = TRUE)),
               pickerInput("reviewYear",
                           "Year of Review:",
                           choices = years, multiple = TRUE, options = list(`actions-box` = TRUE))
             ),
             mainPanel(
               width = 9,
               uiOutput("metricsDisplay"),
               tabsetPanel(
                 tabPanel("Rating Distribution (Restaurants)", plotlyOutput("ratingDist")),
                 tabPanel("Rating Distribution (Reviews)", plotlyOutput("reviewRatingDist")), 
                 tabPanel("Reviews over Time", plotlyOutput("reviewsPerYearPlot")),
                 tabPanel("Rating Trends", plotlyOutput("reviewsPerMonthPlot")),
                 tabPanel("Map View", leafletOutput("map"))
               )
             )
           ))
)