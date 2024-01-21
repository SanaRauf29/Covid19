# Load necessary libraries
library(shiny)
library(plotly)
library(dplyr)

# Read the dataset
full_grouped <- read.csv("full_grouped.csv")

# Define UI
ui <- fluidPage(
  titlePanel("COVID-19 Dashboard"),
  sidebarLayout(
    sidebarPanel(
      selectInput("country", "Select Country", choices = unique(full_grouped$`Country.Region`)),
    ),
    mainPanel(
      plotlyOutput("scatter_plot"),
      plotlyOutput("who_region_plot")
    )
  )
)

# Define server
server <- function(input, output) {
  
  # Filter data based on selected country
  filtered_data <- reactive({
    full_grouped %>%
      filter(`Country.Region` == input$country)
  })
  
  # Render scatter plot
  output$scatter_plot <- renderPlotly({
    plot_ly(
      data = filtered_data(),
      x = ~Confirmed,
      y = ~Deaths,
      type = "scatter",
      mode = "markers",
      text = ~paste("Confirmed:", Confirmed, "<br>Deaths:", Deaths)
    ) %>%
      layout(
        title = "Scatter Plot of Total Deaths vs Confirmed Cases",
        xaxis = list(title = "Confirmed Cases"),
        yaxis = list(title = "Total Deaths")
      )
  })
  
  output$who_region_plot <- renderPlotly({
    # Bar chart of WHO Region against total cases
    total_cases_by_region <- full_grouped %>%
      group_by(`WHO.Region`) %>%
      summarise(TotalCases = sum(Confirmed))
    
    plot_ly(data = total_cases_by_region, x = ~`WHO.Region`, y = ~TotalCases, type = "bar") %>%
      layout(title = "Total Cases by WHO Region",
             xaxis = list(title = "WHO Region"),
             yaxis = list(title = "Total Cases"))
  })
}

# Run the Shiny app
shinyApp(ui, server)
