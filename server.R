Sys.setlocale("LC_ALL","English")
options(warn = -1)
library(shiny)
library(shinyjs)
library(plotly)
library(zoo)
library(DT)

shinyServer(function(input, output) {
  observe({
    toggle("selectYea1", condition = input$comparison)
    toggle("selectYea2", condition = input$comparison)
    toggle("selectPlot", condition = !input$comparison)
  })
  
  #### Input Select template ####
  createSelectInput <- function(id, type, label){
    
    selectID <- paste0("selectInput_", id)
    
    if(type == 'yea'){
      choice <- unique(format(as.yearmon(time(co2)),"%Y"))
    } else {
      choice <- c("All",unique(format(as.yearmon(time(co2)),"%Y")))
    }
    
    template <- list(
      selectInput(inputId = selectID, label=label,
                  choices = choice, selected = 1, selectize = FALSE)
    )
    
    return(template)
  }
  
  #### Select rendering ####
  output$selectSumm <- renderUI({createSelectInput(1, 'all', "Data range")})
  output$selectPlot <- renderUI({createSelectInput(2, 'all', "Data range")})
  output$selectYea1 <- renderUI({createSelectInput(3, 'yea', "Year 1")})
  output$selectYea2 <- renderUI({createSelectInput(4, 'yea', "Year 2")})
  
  #### Data preparation ####
  data <- data.frame(time = as.Date(as.yearmon(time(co2))),
                     data = coredata(co2)
              )
  
  #### Select reactives ####
  summSelect <- reactive({input$selectInput_1})
  plotSelect <- reactive({input$selectInput_2})
  yea1Select <- reactive({input$selectInput_3})
  yea2Select <- reactive({input$selectInput_4})
  
  #### Output values ####
  output$dataTable <- DT::renderDataTable({
    data.frame(Year = format(data$time,"%Y"),
               Month = format(data$time,"%m"),
               Value = data$data)
  },options = list(paging=FALSE, scrollX = TRUE), 
    rownames=TRUE, 
    filter = "top")
  
  output$summary <- renderPrint({
    if(summSelect() == 'All'){
      temp <- data$data
    } else {
      temp <- data[which(format(data$time,'%Y') == summSelect()),2]      
    }
    
    summary(temp)
  })
  
  output$plot <- renderPlotly({
    if(input$comparison){
      if(is.null(yea1Select()) | is.null(yea2Select())) return(NULL)
      
      temp <- data.frame(time = month.name,
                         year1 = data[which(format(data$time,'%Y') == yea1Select()),2],
                         year2 = data[which(format(data$time,'%Y') == yea2Select()),2])
      
      plot_ly(temp, x = ~factor(time,month.name)) %>%
        add_trace(y = ~year1, name = yea1Select() , type='scatter', mode = 'lines+markers') %>%
        add_trace(y = ~year2, name = yea2Select() , type='scatter', mode = 'lines+markers') %>%
        layout(xaxis = list(title=""), yaxis = list(title=""))
  
    } else {
      
      if(is.null(plotSelect())) return(NULL)
      
      if(plotSelect() == 'All'){
        temp <- data
      } else {
        temp <- data[which(format(data$time,'%Y') == plotSelect()),]
        temp$time <- factor(format(temp$time, "%B"),month.name)
      }
      
      plot_ly(temp, x=~time, y=~data, type = 'scatter', mode = 'lines')
    }
  })
  
  output$histogram <- renderPlotly({
    data_time <- format(data$time, "%b %Y")
    temp <- data[which(data_time == format(input$dateRange[1],"%b %Y")):which(data_time == format(input$dateRange[2],"%b %Y")),]      
    
    bins = list(start = min(temp$data), end = max(temp$data), size = (max(temp$data) - min(temp$data)) / input$histBins)
    
    plot_ly(temp, x=~data, type = 'histogram', autobinx = FALSE, xbins = bins)
  })
})
