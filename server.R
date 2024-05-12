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
})

load('./review_data_cleaned.RData')
load(file.path(this.dir(), "Restaurant_Information.RData"))

# Create variables outside of the server/ui function
# Extract unique values for filters
name <- sort(unique(review_data_cleaned$name))
years <- sort(unique(review_data_cleaned$year))
days <- sort(unique(review_data_cleaned$weekday))
months <- sort(unique(review_data_cleaned$month))

# Minimum and maximum annual income for slider
minReview <- min(review_data_cleaned$rating)
maxReview <- max(review_data_cleaned$rating)

# Define server logic
server <- function(input, output, session) {
  # Reactive expression to filter data
  filteredData <- reactive({
    review_data_cleaned %>%
      filter(if (length(input$restaurantName) > 0) name %in% input$restaurantName else TRUE) %>%
      filter(rating >= input$restaurantRating[1] & rating <= input$restaurantRating[2]) %>%
      filter(if (length(input$reviewDay) > 0) weekday %in% input$reviewDay else TRUE) %>%
      filter(if (length(input$reviewMonth) > 0) month %in% input$reviewMonth else TRUE) %>%
      filter(if (length(input$reviewYear) > 0) year %in% input$reviewYear else TRUE)
  })
  
  filteredData_restaurants <- reactive({
    # Obtain the filtered review data
    filtered_data <- filteredData()
    
    # Filter restaurant data based on names in the filtered review data
    if (length(filtered_data$name) > 0) {
      restaurant_data %>%
        filter(name %in% filtered_data$name)
    } else {
      restaurant_data  # Return all if no names are filtered
    }
  })
  
  
  filteredData_table <- reactive({
    review_data_cleaned %>%
      filter(if (length(input$restaurantNameTable) > 0) name %in% input$restaurantNameTable else TRUE) %>%
      filter(rating >= input$restaurantRatingTable[1] & rating <= input$restaurantRatingTable[2]) %>%
      filter(if (length(input$reviewDayTable) > 0) weekday %in% input$reviewDayTable else TRUE) %>%
      filter(if (length(input$reviewMonthTable) > 0) month %in% input$reviewMonthTable else TRUE) %>%
      filter(if (length(input$reviewYearTable) > 0) year %in% input$reviewYearTable else TRUE)
  })
  
  output$filteredData_table <- renderDataTable({
    filteredData_table()
  })
  
  
  # calculate average ratings per month
  monthlyReviewData <- reactive({
    data <- filteredData()
    
    # Compute average ratings by month
    data <- data %>%
      mutate(month_year = floor_date(datetime_parsed, "month")) %>%
      group_by(month_year) %>%
      summarise(average_rating = mean(review_rating, na.rm = TRUE), .groups = 'drop') %>%
      arrange(month_year)
    
    # Calculate the slope of average ratings over time
    if (nrow(data) > 1) {  # Ensure there are at least two points to fit a line
      time <- as.numeric(as.Date(data$month_year))  # Convert dates to numeric for lm()
      lm_fit <- lm(average_rating ~ time, data = data)
      slope <- coef(lm_fit)[2]  # Extract the slope coefficient
    } else {
      slope <- NA  # Not enough data to calculate a slope
    }
    
    list(data = data, slope = slope)  # Return both data and slope
  })
  
  # Render Plotly output for average monthly ratings
  output$reviewsPerMonthPlot <- renderPlotly({
    req(monthlyReviewData())  # Ensuring that the data is not NULL
    data <- monthlyReviewData()
    data <- data$data  # Extract the data from the reactive list
    
    if (nrow(data) > 0) {
      plot_ly(data, x = ~month_year, y = ~average_rating, type = 'scatter', mode = 'lines+markers',
              line = list(color = '#ADD8E6')) %>%
        layout(title = 'Average Monthly Ratings',
               xaxis = list(title = 'Month-Year', type = 'date'),
               yaxis = list(title = 'Average Rating'),
               hovermode = 'closest')
    } else {
      plotly_empty()  # Return an empty plot if no data is available
    }
  })
  
  # Output for trend display
  output$trendInfo <- renderText({
    trend_data <- monthlyReviewData()  # Get the reactive list with data and slope
    if (is.na(trend_data$slope)) {
      "Insufficient data to determine the trend."
    } else if (trend_data$slope > 0) {
      "The trend is positive."
    } else if (trend_data$slope < 0) {
      "The trend is negative."
    } else {
      "The trend is stable (no change)."
    }
  })
  
  output$metricsDisplay <- renderUI({
    data <- filteredData()
    
    # Mean rating of all restaurants
    rating_restaurants <- data %>%
      group_by(name) %>%
      summarise(rating = unique(rating)) %>%
      arrange(desc(rating)) 
    
    # Calculate metrics
    numRestaurants <- length(unique(data$name))
    numberReviews <- nrow(data)
    avgReviewScore <- mean(rating_restaurants$rating, na.rm = TRUE)
    
    # Create the HTML for the metrics using the styles we defined
    div(class = "info-boxes",
        div(class = "info-box box1",
            div(class = "number", numRestaurants),
            div(class = "descriptor", "# of Restaurants")
        ),
        div(class = "info-box box2",
            div(class = "number", numberReviews),
            div(class = "descriptor", "# of Reviews")
        ),
        div(class = "info-box box3",
            div(class = "number", round(avgReviewScore, 2)),
            div(class = "descriptor", "Avg Review Score")
        ),
        div(class = "info-box box3",
            div(class = "number", verbatimTextOutput("trendInfo")),
            div(class = "descriptor", "Rating Trend")
        )
    )
  })
  
  
  output$ratingDist <- renderPlotly({
    data <- filteredData()  # Assuming this fetches your restaurant data
    # Group by restaurant name and calculate the unique rating for each restaurant
    rating_restaurants <- data %>%
      group_by(name) %>%
      summarise(rating = unique(rating)) %>%
      ungroup()  # ungroup to avoid issues in later steps
    
    if (nrow(rating_restaurants) > 0) {
      ratings <- rating_restaurants$rating
      
      # Calculate the mean and median of unique ratings
      median_rating <- median(ratings, na.rm = TRUE)
      mean_rating <- round(mean(ratings, na.rm = TRUE), 2)
      
      # Create the histogram with customized hovertemplate
      p <- plot_ly(rating_restaurants, x = ~rating, type = 'histogram',
                   marker = list(color = '#ADD8E6', line = list(color = 'rgba(255, 255, 255, 1)', width = 0.2)),
                   hoverinfo = 'x+y',  # Will display both the x value (range) and y value (count)
                   hovertemplate = paste(
                     "Rating: %{x}<br>",
                     "Number of Restaurants: %{y}<extra></extra>"  # The <extra></extra> tag hides the trace name
                   )) %>%
        layout(title = 'Rating Distribution of Restaurants',
               xaxis = list(title = 'Rating'),
               yaxis = list(title = 'Number of Restaurants'),
               annotations = list(
                 list(
                   x = 1, y = 1, xref = 'paper', yref = 'paper',
                   text = paste("Median Rating: ", median_rating),
                   showarrow = FALSE, xanchor = 'right', yanchor = 'bottom',
                   xshift = -10, yshift = -10
                 ),
                 list(
                   x = 1, y = 0.98, xref = 'paper', yref = 'paper',
                   text = paste("Mean Rating: ", mean_rating),
                   showarrow = FALSE, xanchor = 'right', yanchor = 'bottom',
                   xshift = -10, yshift = -20
                 )
               )
        )
      
      p # Render the Plotly plot
    } else {
      plotly_empty() # Provide an empty plotly object if no data
    }
  })
  
  output$reviewRatingDist <- renderPlotly({
    data <- filteredData()  # Assuming this fetches your review data
    review_ratings <- data$review_rating
    
    if (length(review_ratings) > 0) {
      # Calculate the mean and median ratings
      median_rating <- median(review_ratings, na.rm = TRUE)
      mean_rating <- round(mean(review_ratings, na.rm = TRUE), 2)
      
      # Create the histogram with customized hovertemplate
      p <- plot_ly(data, x = ~review_rating, type = 'histogram',
                   marker = list(color = '#ADD8E6', line = list(color = 'rgba(255, 255, 255, 1)', width = 0.2)),
                   hoverinfo = 'x+y',  # Will display both the x value (range) and y value (count)
                   hovertemplate = paste(
                     "Rating: %{x}<br>",
                     "Count: %{y}<extra></extra>"  # The <extra></extra> tag hides the trace name
                   )) %>%
        layout(title = 'Rating Distribution of Reviews',
               xaxis = list(title = 'Rating'),
               yaxis = list(title = 'Count'),
               annotations = list(
                 list(
                   x = 1, y = 1, xref = 'paper', yref = 'paper',
                   text = paste("Median Rating: ", median_rating),
                   showarrow = FALSE, xanchor = 'right', yanchor = 'bottom',
                   xshift = -10, yshift = -10
                 ),
                 list(
                   x = 1, y = 0.98, xref = 'paper', yref = 'paper',
                   text = paste("Mean Rating: ", mean_rating),
                   showarrow = FALSE, xanchor = 'right', yanchor = 'bottom',
                   xshift = -10, yshift = -20
                 )
               )
        )
      
      p # Render the Plotly plot
    } else {
      plotly_empty() # Provide an empty plotly object if no data
    }
  })
  
  output$reviewsPerYearPlot <- renderPlotly({
    data <- filteredData()
    
    # Count the number of reviews per year
    reviews_per_year <- data %>%
      group_by(year) %>%
      summarise(count = n()) %>%
      ungroup()
    
    # Plot
    p <- plot_ly(reviews_per_year, x = ~year, y = ~count, type = 'bar',
                 marker = list(color = '#ADD8E6',
                               line = list(color = '#ADD8E6', width = 1))) %>%
      layout(title = 'Number of Reviews Per Year',
             xaxis = list(title = 'Year'),
             yaxis = list(title = 'Number of Reviews'))
    
    p  # Render the Plotly plot
  })
  
  output$map <- renderLeaflet({
    # Default map centered on a generic location
    leaflet() %>% 
      addTiles() %>% 
      setView(lng = 2.168672, lat = 41.389133, zoom = 12) # Adjust center and zoom level accordingly
  })
  
  observe({
    # Ensure that there's data to show, including when no filters are applied
    req(filteredData_restaurants())
    
    # Get the data from the reactive expression
    restaurants_to_show <- filteredData_restaurants()
    
    # Update the map
    leafletProxy("map", data = restaurants_to_show) %>% 
      clearMarkers() %>%
      addMarkers(
        lng = ~longitude, lat = ~latitude,
        popup = ~paste(name)
      )
  })
}