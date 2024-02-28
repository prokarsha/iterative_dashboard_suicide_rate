library(shiny)
library(ggplot2)
library(dplyr)

data = read.csv(file.choose())
View(data)

# Define UI for application
ui <- fluidPage(
  
  # Application title
  titlePanel("Suicide Data Dashboard"),
  
  # Sidebar layout with input and output definitions
  sidebarLayout(
    sidebarPanel(
      # Input: Select a country
      selectInput("country", "Select Country:",
                  choices = unique(data$country)),
      
      # Input: Select a year
      selectInput("year", "Select Year:",
                  choices = unique(data$year)),
      
      # Input: Select a variable for x-axis
      selectInput("x_var", "Select X Variable:",
                  choices = c("age", "HDI_for_year", "gdp_per_capita", "generation")),
      
      # Input: Select a variable for y-axis
      selectInput("y_var", "Select Y Variable:",
                  choices = c("suicides_no", "population", "suicides_100k_pop"))
    ),
    
    # Show plots
    mainPanel(
      plotOutput("suicide_plot_sex"),
      plotOutput("suicide_plot_generation")
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  # Filter data based on selected country and year
  filtered_data <- reactive({
    filter(data, country == input$country & year == input$year)
  })
  
  # Render the sex plot
  output$suicide_plot_sex <- renderPlot({
    ggplot(filtered_data(), aes_string(x = input$x_var, y = input$y_var, fill = "sex")) +
      geom_bar(stat = "identity") +
      labs(title = paste("Suicide Data for", input$country, "-", input$year, "- Sex"),
           x = input$x_var,
           y = input$y_var,
           fill = "Sex") +
      theme_minimal()
  })
  
  # Render the generation plot
  output$suicide_plot_generation <- renderPlot({
    ggplot(filtered_data(), aes_string(x = input$x_var, y = input$y_var, fill = "generation")) +
      geom_bar(stat = "identity") +
      labs(title = paste("Suicide Data for", input$country, "-", input$year, "- Generation"),
           x = input$x_var,
           y = input$y_var,
           fill = "Generation") +
      theme_minimal()
  })
}

# Run the application
shinyApp(ui = ui, server = server)
