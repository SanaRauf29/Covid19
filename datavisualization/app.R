# Define UI
ui <- fluidPage(
  titlePanel("COVID-19 Dashboard"),
  sidebarLayout(
    sidebarPanel(
      selectInput("Country/Region", "Select Country", choices = unique(full_grouped$Country)),
    ),
    mainPanel(
      plotlyOutput("time_series_plot"),
      plotlyOutput("scatter_plot")
    )
  )
)

# Define server
server <- function(input, output) {
  
  filtered_data <- reactive({
    filter(full_grouped, "Country/Region" == input$"Country/Region")
  })
  
  output$time_series_plot <- renderPlotly({
    plot_ly(data = filtered_data(), x = ~Date, y = ~Confirmed, type = "scatter", mode = "lines",
            text = ~paste("Date:", Date, "<br>Confirmed Cases:", Confirmed)) %>%
      layout(title = "Time Series Line Plot of Confirmed Cases over Time",
             xaxis = list(title = "Date"),
             yaxis = list(title = "Confirmed Cases"))
  })
  
  output$scatter_plot <- renderPlotly({
    plot_ly(data = filtered_data(), x = ~Confirmed, y = ~Deaths, type = "scatter", mode = "markers",
            text = ~paste("Confirmed:", Confirmed, "<br>Deaths:", Deaths)) %>%
      layout(title = "Scatter Plot of Total Deaths vs Confirmed Cases",
             xaxis = list(title = "Confirmed Cases"),
             yaxis = list(title = "Total Deaths"))
  })
}

# Run the Shiny app
shinyApp(ui, server)


