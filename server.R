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

#--- Load dataset -------------------------------------------------------------
review_data_cleaned <- read_csv(file.path(this.dir(), 'review_data_with_sentiment.csv'))

# Exclude 'about' variable from dataset
review_data_cleaned <- review_data_cleaned %>% select(-about)

#--- Create variables outside of the server/ui function -----------------------
# Extract unique values for filters
name <- sort(unique(review_data_cleaned$name))
years <- sort(unique(review_data_cleaned$year))
days <- sort(unique(review_data_cleaned$weekday))
months <- sort(unique(review_data_cleaned$month))
type <- sort(unique(review_data_cleaned$type))

# Minimum and maximum annual income for slider
minReview <- min(review_data_cleaned$rating)
maxReview <- max(review_data_cleaned$rating)

#--- Define server logic ------------------------------------------------------

server <- function(input, output, session) {
  
  # -- Reactive expressions to filter data --------------------
  filteredData <- reactive({
    review_data_cleaned %>%
      filter(if (length(input$restaurantName) > 0) name %in% input$restaurantName else TRUE) %>%
      filter(rating >= input$restaurantRating[1] & rating <= input$restaurantRating[2]) %>%
      filter(if (length(input$reviewDay) > 0) weekday %in% input$reviewDay else TRUE) %>%
      filter(if (length(input$reviewMonth) > 0) month %in% input$reviewMonth else TRUE) %>%
      filter(if (length(input$reviewYear) > 0) year %in% input$reviewYear else TRUE) %>%
      filter(if (length(input$restaurantType) > 0) type %in% input$restaurantType else TRUE)
  })
  
  filteredData_unique <- reactive({
    req(filteredData())  # ensure the data is ready
    data <- filteredData()
    
    # Group by name and location to ensure each restaurant appears only once
    data %>%
      distinct(name, longitude, latitude, rating, .keep_all = TRUE)
  })
  
  # New reactive expression to calculate average sentiment score and merge with filtered data
  filtered_data_with_sentiment <- reactive({
    sentiment_restaurants <- filteredData()  # Ensure this is a reactive expression or a reactive object
    
    # Calculate the average sentiment score for each restaurant, excluding scores of 0
    avg_sentiment_scores <- sentiment_restaurants %>%
      filter(review_analysis_score != 0) %>%
      group_by(name) %>%
      summarise(avg_sentiment = mean(review_analysis_score, na.rm = TRUE))
    
    # Merge the average sentiment scores with the filtered data
    filtered_data <- filteredData_unique()  # Ensure this is a reactive expression or a reactive object
    filtered_data_with_sentiment <- filtered_data %>%
      left_join(avg_sentiment_scores, by = "name")
    
    return(filtered_data_with_sentiment)
  })
  
  # Filtered data for the table overview on first page
  filteredData_table <- reactive({
    review_data_cleaned %>%
      filter(if (length(input$restaurantNameTable) > 0) name %in% input$restaurantNameTable else TRUE) %>%
      filter(rating >= input$restaurantRatingTable[1] & rating <= input$restaurantRatingTable[2]) %>%
      filter(if (length(input$reviewDayTable) > 0) weekday %in% input$reviewDayTable else TRUE) %>%
      filter(if (length(input$reviewMonthTable) > 0) month %in% input$reviewMonthTable else TRUE) %>%
      filter(if (length(input$reviewYearTable) > 0) year %in% input$reviewYearTable else TRUE) %>%
      filter(if (length(input$restaurantTypeTable) > 0) type %in% input$restaurantTypeTable else TRUE)
  })
  
  output$filteredData_table <- renderDataTable({
    filteredData_table()
  })
  
  # Reactive expression to get unique categories for the selected restaurant
  restaurantCategories <- reactive({
    if (length(input$restaurantName) > 0) {
      selected_restaurant <- input$restaurantName
      unique(review_data_cleaned %>% filter(name %in% selected_restaurant) %>% pull(type))
    } else {
      unique(review_data_cleaned$type)
    }
  })
  
  # Reactive expression to get unique categories for the selected restaurant in the table
  restaurantCategoriesTable <- reactive({
    if (length(input$restaurantNameTable) > 0) {
      selected_restaurant_table <- input$restaurantNameTable
      unique(review_data_cleaned %>% filter(name %in% selected_restaurant_table) %>% pull(type))
    } else {
      unique(review_data_cleaned$type)
    }
  })
  
  # Observe changes in the restaurantName input and update the restaurantType picker input
  observe({
    updatePickerInput(session, "restaurantType", choices = restaurantCategories())
  })
  
  # Observe changes in the restaurantNameTable input and update the restaurantTypeTable picker input
  observe({
    updatePickerInput(session, "restaurantTypeTable", choices = restaurantCategoriesTable())
  })
  
  # Calculate average ratings per month for trend analysis
  monthlyReviewData <- reactive({
    data <- filteredData()
    
    # Compute average ratings by month
    data <- data %>%
      mutate(month_year = floor_date(datetime_parsed, "month")) %>%
      group_by(month_year) %>%
      reframe(average_rating = mean(review_rating, na.rm = TRUE)) %>%
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
  
  # -- Plotly output for average monthly ratings --------------------
  output$reviewsPerMonthPlot <- renderPlotly({
    req(monthlyReviewData())  # Ensuring that the data is not NULL
    data <- monthlyReviewData()
    data <- data$data  # Extract the data from the reactive list
    
    if (nrow(data) > 0) {
      plot_ly(data, x = ~month_year, y = ~average_rating, type = 'scatter', mode = 'lines+markers',
              line = list(color = '#94E2D1'), 
              marker = list(color = '#FFA07A')) %>%
        layout(title = 'Average Monthly Ratings',
               xaxis = list(title = 'Month-Year', type = 'date'),
               yaxis = list(title = 'Average Rating'),
               hovermode = 'closest')
    } else {
      plotly_empty()  # Return an empty plot if no data is available
    }
  })
  
  # -- Output for trend display --------------------------------------
  output$trendInfo <- renderText({
    trend_data <- monthlyReviewData()  # Get the reactive list with data and slope
    if (is.na(trend_data$slope)) {
      "Insufficient data to determine the trend."
    } else if (trend_data$slope > 0) {
      "Positive trend"
    } else if (trend_data$slope < 0) {
      "Negative trend"
    } else {
      "Stable trend (no change)"
    }
  })
  
  # -- Output for metrics display --------------------------------------
  output$metricsDisplay <- renderUI({
    data <- filteredData()
    
    # Mean rating of all restaurants
    rating_restaurants <- data %>%
      group_by(name) %>%
      reframe(rating = unique(rating)) %>%
      arrange(desc(rating)) 
    
    # Calculate metrics
    numRestaurants <- length(unique(data$name))
    numberReviews <- nrow(data)
    avgReviewScore <- mean(rating_restaurants$rating, na.rm = TRUE)
    avgSentimentScore <- mean(data$review_analysis_score[data$review_analysis_score != 0], na.rm = TRUE)
    
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
            div(class = "number", round(avgSentimentScore, 2)),
            div(class = "descriptor", "Avg Sentiment Score")
        ),
        div(class = "info-box box3",
            div(class = "number", verbatimTextOutput("trendInfo")),
            div(class = "descriptor", "Rating Trend")
        )
    )
  })
  
  # -- Plotly output for rating distribution (restaurants) --------------------
  output$ratingDist <- renderPlotly({
    data <- filteredData()  # Assuming this fetches your restaurant data
    # Group by restaurant name and calculate the unique rating for each restaurant
    rating_restaurants <- data %>%
      group_by(name) %>%
      reframe(rating = unique(rating)) %>%
      ungroup()  # ungroup to avoid issues in later steps
    
    if (nrow(rating_restaurants) > 0) {
      ratings <- rating_restaurants$rating
      
      # Calculate the mean and median of unique ratings
      median_rating <- median(ratings, na.rm = TRUE)
      mean_rating <- round(mean(ratings, na.rm = TRUE), 2)
      
      # Create the histogram with customized hovertemplate
      p <- plot_ly(rating_restaurants, x = ~rating, type = 'histogram',
                   marker = list(color = '#94E2D1', line = list(color = 'rgba(255, 255, 255, 1)', width = 0.2)),
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
  
  # -- Plotly output for rating distribution (reviews) --------------------
  output$reviewRatingDist <- renderPlotly({
    data <- filteredData()  # Assuming this fetches your review data, including sentiment scores
    review_ratings <- data$review_rating
    sentiment_scores <- data$review_analysis_score[data$review_analysis_score != 0]  # Exclude sentiment scores with 0
    
    if (length(review_ratings) > 0 && length(sentiment_scores) > 0) {
      # Calculate the mean and median ratings
      median_rating <- median(review_ratings, na.rm = TRUE)
      mean_rating <- round(mean(review_ratings, na.rm = TRUE), 2)
      
      # Calculate the mean and median sentiment scores
      median_sentiment <- median(sentiment_scores, na.rm = TRUE)
      mean_sentiment <- round(mean(sentiment_scores, na.rm = TRUE), 2)
      
      # Create the histogram for review ratings
      p <- plot_ly(data, x = ~review_rating, type = 'histogram',
                   name = 'Review Ratings',
                   marker = list(color = '#94E2D1', line = list(color = 'rgba(255, 255, 255, 1)', width = 0.2)),
                   hoverinfo = 'x+y',  # Will display both the x value (range) and y value (count)
                   hovertemplate = paste(
                     "Rating: %{x}<br>",
                     "Count: %{y}<extra></extra>"  # The <extra></extra> tag hides the trace name
                   )) %>%
        # Add the histogram for sentiment scores
        add_trace(x = sentiment_scores, type = 'histogram',
                  name = 'Sentiment Scores',
                  marker = list(color = '#FFA07A', line = list(color = 'rgba(255, 255, 255, 1)', width = 0.2)),
                  hoverinfo = 'x+y',
                  hovertemplate = paste(
                    "Sentiment Score: %{x}<br>",
                    "Count: %{y}<extra></extra>"
                  )) %>%
        layout(title = 'Rating and Sentiment Score Distribution of Reviews',
               xaxis = list(title = 'Score'),
               yaxis = list(title = 'Count'),
               barmode = 'group', 
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
                 ),
                 list(
                   x = 1, y = 0.96, xref = 'paper', yref = 'paper',
                   text = paste("Median Sentiment: ", median_sentiment),
                   showarrow = FALSE, xanchor = 'right', yanchor = 'bottom',
                   xshift = -10, yshift = -30
                 ),
                 list(
                   x = 1, y = 0.94, xref = 'paper', yref = 'paper',
                   text = paste("Mean Sentiment: ", mean_sentiment),
                   showarrow = FALSE, xanchor = 'right', yanchor = 'bottom',
                   xshift = -10, yshift = -40
                 )
               )
        )
      
      p # Render the Plotly plot
    } else {
      plotly_empty() # Provide an empty plotly object if no data
    }
  })
  
  # -- Plotly output for number of reviews per year (reviews over time) -------
  output$reviewsPerYearPlot <- renderPlotly({
    data <- filteredData()
    
    # Count the number of reviews per year
    reviews_per_year <- data %>%
      group_by(year) %>%
      reframe(count = n()) %>%
      ungroup()
    
    # Plot
    p <- plot_ly(reviews_per_year, x = ~year, y = ~count, type = 'bar',
                 marker = list(color = '#94E2D1',
                               line = list(color = '#94E2D1', width = 1))) %>%
      layout(title = 'Number of Reviews Per Year',
             xaxis = list(title = 'Year'),
             yaxis = list(title = 'Number of Reviews'))
    
    p  # Render the Plotly plot
  })
  
  
  # -- Map setup with popup having a filter button --------------------
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      addMarkers(data = filtered_data_with_sentiment(),
        lng = ~longitude, lat = ~latitude, 
        popup = ~paste(name, "<br>Review Score: ", rating, 
                       "<br>Average Sentiment Score: ", round(avg_sentiment, 2),
                       "<br><button onclick=\"Shiny.setInputValue('restaurantName','", 
                       gsub("'","\\'", name),"', {priority: 'event'})\">Filter</button>")
      )
  })
  
  # Observe the restaurantName input and update pickerInputs when 'Filter' is clicked
  observeEvent(input$restaurantName, {
    trimmedName <- trimws(input$restaurantName)
    updatePickerInput(session, "restaurantName", selected = trimmedName)
    updatePickerInput(session, "restaurantNameTable", selected = trimmedName)
  })
  
  # -- Reactive expressions for diagrams --------------------------------------
  # Reactive expression to calculate the average review score per restaurant category
  averageReviewByCategory <- reactive({
    filteredData() %>%
      group_by(type) %>%
      reframe(average_rating = mean(review_rating, na.rm = TRUE)) %>%
      arrange(desc(average_rating))
  })
  
  # Reactive expression to calculate the average sentiment score per restaurant category
  averageSentimentByCategory <- reactive({
    filteredData() %>%
      filter(review_analysis_score != 0) %>%
      group_by(type) %>%
      summarise(average_sentiment = mean(review_analysis_score, na.rm = TRUE)) %>%
      arrange(desc(average_sentiment))
  })
  
  # Reactive expression to combine both average ratings and sentiments
  combinedAverageByCategory <- reactive({
    review_data <- averageReviewByCategory()
    sentiment_data <- averageSentimentByCategory()
    
    # Merge the data on 'type'
    combined_data <- review_data %>%
      left_join(sentiment_data, by = "type")
    
    return(combined_data)
  })
  
  # -- Plotly output for average review and sentiment scores per restaurant category
  # Render Plotly bar chart for average review score and average sentiment score per restaurant category
  output$averageReviewByCategoryPlot <- renderPlotly({
    avg_data <- combinedAverageByCategory()
    
    plot_ly(avg_data, x = ~reorder(type, -average_rating), y = ~average_rating, type = 'bar',
            name = 'Average Review Score',
            marker = list(color = '#94E2D1', line = list(color = '#94E2D1', width = 1.5)),
            hovertext = ~paste(type, '<br>Average Review Score:', round(average_rating, 2)),
            hoverinfo = 'text') %>%
      add_trace(y = ~average_sentiment, name = 'Average Sentiment Score', type = 'bar',
                marker = list(color = '#FFA07A', line = list(color = '#FFA07A', width = 1.5)),
                hovertext = ~paste(type, '<br>Average Sentiment Score:', round(average_sentiment, 2)),
                hoverinfo = 'text') %>%
      layout(title = 'Average Review and Sentiment Scores per Restaurant Category',
             xaxis = list(title = 'Restaurant Category', tickangle = -45),
             yaxis = list(title = 'Score'),
             barmode = 'group',
             margin = list(b = 150)) # Adjust bottom margin for category names
  })
}