library(shiny)
library(plotly)
library(DT)
library(shinyjs)

shinyUI(fluidPage(
  useShinyjs(),
  
  headerPanel("Mauna Loa Atmospheric CO2 Concentration"),

  navbarPage("Mauna Loa CO2 App",
    navbarMenu("Data",
      tabPanel("Data table",
        basicPage(
          DT::dataTableOutput("dataTable")
        )
      ),
      tabPanel("Summary",
        sidebarLayout(
          sidebarPanel(
            uiOutput("selectSumm")
          ),
          mainPanel(          
            verbatimTextOutput("summary")
          )
        )
      )
    ),
    navbarMenu("Plots",
      tabPanel("Plot",
        sidebarLayout(
          sidebarPanel(
            checkboxInput("comparison", label = "Year-year comparison", value = FALSE),
            uiOutput("selectYea1"),
            uiOutput("selectYea2"),
            uiOutput("selectPlot")
          ),
          mainPanel(
            plotlyOutput("plot")
          )
        )
      ),
      tabPanel("Histogram",
        sidebarLayout(
          sidebarPanel(
            sliderInput("histBins",
                        "Number of bins:",
                        min = 5,
                        max = 39,
                        value = 22
            ),
            sliderInput("dateRange",
                        "Dates:",
                        min = as.Date("1959-01-01","%Y-%m-%d"),
                        max = as.Date("1997-12-01","%Y-%m-%d"),
                        value= c(as.Date("1959-01-01"), as.Date("1997-12-01")),
                        timeFormat="%b %Y")
          ),
          mainPanel(
            plotlyOutput("histogram")
          )
      ))
    )
  )
))