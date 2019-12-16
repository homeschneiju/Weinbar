
library(shiny)
library(data.table)
library(DT)
library(installr)
source("./weintree.R")

# Define the fields we want to save from the form
fields <- colnames(wein)

  ui = fluidPage(
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
  
  
  server = function(input, output, session) {
    
    
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