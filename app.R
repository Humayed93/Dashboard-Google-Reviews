# app.R
library(shiny)
library('this.path')

# Source UI and Server scripts
source(file.path(this.dir(), 'ui.R'))
source(file.path(this.dir(), 'server.R'))

# Run the application
shinyApp(ui = ui, server = server)