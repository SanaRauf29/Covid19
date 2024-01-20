# Define UI
ui <- fluidPage(
  titlePanel("COVID-19 Dashboard"),
  sidebarLayout(
    sidebarPanel(
      selectInput("country", "Select Country", choices = unique(full_grouped$`Country/Region`)),
    ),
    mainPanel(
      plotlyOutput("scatter_plot")
    )
  )
)

# Define server
server <- function(input, output) {
  
  filtered_data <- reactive({
    subset(full_grouped, `Country/Region` == input$country)
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
