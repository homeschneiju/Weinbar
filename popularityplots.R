library(shiny)

source("./weintree.R")
wein <- readRDS("C:/Users/Juliana Schneider/Documents/dataentry/prod/datafiles/Weinbar.rds")

# Define UI for app that draws a histogram ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Popularity plots"),
  
  # Sidebar layout with input and output definitions ----
#  sidebarLayout(
    
    # Sidebar panel for inputs ----
    # sidebarPanel(
    #   
    #   # Input: Slider for the number of bins ----
    #   sliderInput(inputId = "bins",
    #               label = "Number of bins:",
    #               min = 1,
    #               max = 50,
    #               value = 30)
    #   
    # ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: popular grapes ----
      splitLayout(
      plotOutput(outputId = "grapePlot"),
      plotOutput(outputId = "pricePlot")),
      splitLayout(
      plotOutput(outputId = "countryPlot"),
      plotOutput(outputId = "regionPlot"))
      
    )
#  )
)

# Define server logic required to draw a histogram ----
server <- function(input, output) {
  
  # Histogram of the Old Faithful Geyser Data ----
  # with requested number of bins
  # This expression that generates a histogram is wrapped in a call
  # to renderPlot to indicate that:
  #
  # 1. It is "reactive" and therefore should be automatically
  #    re-executed when inputs (input$bins) change
  # 2. Its output type is a plot
  output$grapePlot <- renderPlot({
    
    barplot(sort(table(wein$grape), decreasing = TRUE)[1:5],
            main = "Popular grapes",
            xlab = "Grape",
            ylab = "Number of wines liked",
            col= "darkred")
    
    
  })
  
  output$pricePlot <- renderPlot({
    
    barplot(sort(table(wein$price), decreasing = TRUE)[1:5],
            main = "Price range",
            xlab = "Price",
            ylab = "Number of wines liked",
            col= "darkred")
    
    
  })
  
  output$countryPlot <- renderPlot({
    
    barplot(sort(table(wein$country), decreasing = TRUE)[1:5],
            main = "Popular countries",
            xlab = "Country",
            ylab = "Number of wines liked",
            col= "darkred")
    
    
  })
  
  output$regionPlot <- renderPlot({
    
    barplot(sort(table(wein$region[wein$country == names(sort(table(wein$country), decreasing = TRUE)[1])]), decreasing = TRUE)[1:5],
            main = "Popular regions in favourite country",
            xlab = "Region",
            ylab = "Number of wines liked",
            col= "darkred")    
    
  })
  
}
shinyApp(ui, server)
