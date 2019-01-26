#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)
library(pool)

pool <- dbPool(
  drv = RMySQL::MySQL(),
  dbname = "tests",
  host = "127.0.0.1",
  username = "root",
  password = "wgs491982"
)
onStop(function() {
  poolClose(pool)
})

ui <- fluidPage(
  textInput("ID", "Enter your ID:", "5"),
  tableOutput("tbl"),
  numericInput("nrows", "How many cities to show?", 10),
  plotOutput("popPlot")
)

server <- function(input, output, session) {
  output$tbl <- renderTable({
    pool %>% tbl("mtcars") %>% filter(gear == input$ID) %>% collect()
  })
  output$popPlot <- renderPlot({
    df <- pool %>% tbl("test_encoding_copy01") %>% head(input$nrows) %>% collect()
    pop <- df$x
    names(pop) <- df$y
    barplot(pop)
  })
}

shinyApp(ui, server)
