library(shiny)

library(markdown)

ui = navbarPage("Navbar!",
           tabPanel("Plots",
                  
                      
                      # App title ----
                      titlePanel("Popularity plots"),
                    
                      mainPanel(
                        
                        # Output: popular grapes ----
                        splitLayout(
                          plotOutput(outputId = "grapePlot"),
                          plotOutput(outputId = "pricePlot")),
                        splitLayout(
                          plotOutput(outputId = "countryPlot"),
                          plotOutput(outputId = "regionPlot"))
                        
                      )
                    
           ),
           tabPanel("Entry",
                    mainPanel(
                      DT::dataTableOutput("responses", width = 300), tags$hr(),
                      textInput("name", "Name", ""),
                      textInput("grape", "Grape", ""),
                      textInput("colour", "Colour", ""),
                      textInput("country", "Country", ""),
                      textInput("region", "Region", ""),
                      textInput("vineyard", "Vinyard", ""),
                      textInput("description", "Description", ""),
                      textInput("store", "Store", ""),
                      textInput("Alcohol", "Alcohol", ""),
                      textInput("price", "Price", ""),
                      textInput("year", "Year", ""),
                      textInput("liked", "Liked", ""),
                      textInput("tested", "Tested", ""),
                      
                      actionButton("submit", "Submit"),
                      
                      textOutput("text"),
                      textInput("word", "Search entries:", ""),
                      actionButton("search", "Search")
                    )
           )
           
)



server = function(input, output, session) {
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
  
  outputDir <- "C:/Users/Juliana Schneider/Documents/dataentry/prod/datafiles"
  
  saveData <- function(data) {
    #data <- t(data)
    # Create a unique file name
    fileName <- "dataentry.csv" #sprintf("%s_%s.csv", as.integer(Sys.time()), digest::digest(data))
    # Write the file to the local system
    old_data <- loadData()
    write.csv(
      x = rbind(old_data,data),
      file = file.path(outputDir, fileName), 
      row.names = FALSE, quote = TRUE
    )
  }
  
  loadData <- function() {
    # Read all the files into a list
    files <- list.files(outputDir, full.names = TRUE)
    old_data <- lapply(files, fread, stringsAsFactors = FALSE)
    # Concatenate all data together into one data.frame
    if(exists("old_data")){
      old_data <- old_data[sapply(old_data, function(x) all(colnames(x) == fields))]
      old_data <- do.call(rbind, old_data)}
    old_data
  }
  
  # Whenever a field is filled, aggregate all form data
  formData <- reactive({
    data <- sapply(fields, function(x) input[[x]])
    data
  })
  
  
  # When the Submit button is clicked, save the form data
  observeEvent(input$submit, {
    
    saveData(formData())
  })
  
  
  randomVals <- eventReactive(input$search, {
    a <- loadData()
    return(nrow(a[which(a == input$word, arr.ind=TRUE)[1],]))
  })
  
  output$text <- renderText({
    if(input$word != ""){
      paste(sprintf("Number of ocurrences of %s: ", input$word) , randomVals())}
  })
  
  # Show the previous responses
  # (update with current response when Submit is clicked)
  output$responses <- DT::renderDataTable({
    input$submit
    loadData()[1:10,]
  })
  
  
}



shinyApp(ui, server)

