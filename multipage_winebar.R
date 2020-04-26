
library(shiny)

library(markdown)

source("./weintree.R")
source("./bubble_maps.R")


ui = navbarPage("Navbar!",
                tabPanel("Bubble Map",
                         
                         
                         # App title ----
                         titlePanel("Bubble map"),
                         
                         mainPanel(
                           
                           # Output: popular regions ----
                             plotOutput(outputId = "BubbleMap")
                            
                         )
                         
                ),
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
  output$BubbleMap <- renderPlot({
    
    ggplot(data=world) +
      geom_sf()+
      geom_sf(data =wc, aes(fill=Anzahl)) +
      scale_fill_gradient(low="blue", high="red")
    
    
    
  })
  
  output$grapePlot <- renderPlot({
    
    # barplot(sort(table(wein$grape), decreasing = TRUE)[1:5],
    #         main = "Popular grapes",
    #         xlab = "Grape",
    #         ylab = "Number of wines liked",
    #         col= "darkred")
    grapeplot_df %>%
      ggplot(aes(x = reorder(grape, -n), y = n, fill=colour)) +
      geom_bar(stat = 'identity') +
      theme_dark()+
      xlab("Grape")+ylab("Number of wines liked")+
      scale_fill_manual(values=c("darkred", "cornsilk"))+
      title("Popular grapes")
    
    
    
  })
  
  output$pricePlot <- renderPlot({
    
    # barplot(sort(table(wein$price), decreasing = TRUE)[1:5],
    #         main = "Price range",
    #         xlab = "Price",
    #         ylab = "Number of wines liked",
    #         col= "darkred")
   priceplot_df %>%
      ggplot(aes(x = reorder(price, -n), y = n, fill = "darkblue")) +
      geom_bar(stat = 'identity') +
      theme_dark()+
      xlab("Price")+ylab("Number of wines liked")+
      scale_fill_manual(values=c("darkblue"))+
      title("Favourite price range")
    
    
  })
  
  output$countryPlot <- renderPlot({
    
    # barplot(sort(table(wein$country), decreasing = TRUE)[1:5],
    #         main = "Popular countries",
    #         xlab = "Country",
    #         ylab = "Number of wines liked",
    #         col= "darkred")
 countryplot_df %>%
      ggplot(aes(x = reorder(country, -n), y = n, fill = country)) +
      geom_bar(stat = 'identity') +
      theme_dark()+
      xlab("Price")+ylab("Number of wines liked")+
      scale_fill_manual("Legend", values = country_col.col)+
      title("Favourite countries")
    
    
  })
  
  output$regionPlot <- renderPlot({
    
    # barplot(sort(table(wein$region[wein$country == names(sort(table(wein$country), decreasing = TRUE)[1])]), decreasing = TRUE)[1:5],
    #         main = "Popular regions in favourite country",
    #         xlab = "Region",
    #         ylab = "Number of wines liked",
    #         col= "darkred")  
    regionplot_df %>%
      ggplot(aes(x = reorder(region, -n), y = n, fill=country)) +
      geom_bar(stat = 'identity') +
      theme_dark()+
      xlab("Region")+ylab("Number of wines liked")+
      scale_fill_manual("Legend", values = country_col.col)+
      title("Popular regions")
      
    
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

