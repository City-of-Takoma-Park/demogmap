library(shiny)
library(leaflet)
library(leafletwrappers)
library(tidyverse)
library(acsmapping)
library(shinythemes)

tp_bg <- readRDS("./data/bg_tp.rds")

tp_tract <- readRDS("./data/tract_tp.rds")

mont_bg <- readRDS("./data/mont_bg.rds")

mont_tract <- readRDS("./data/mont_tract.rds")

mont_place <- readRDS("./data/mont_place.rds")

# tp_place <- readRDS("./data/2020/takomapark/place_tp.rds")

ui <- fluidPage(
  theme = shinytheme("lumen"),
  # Application title
  titlePanel("Takoma Park Interactive Demographic Map"),
  
  tabsetPanel(
    tabPanel(
      title = "Demographic map",
      shiny::fluidRow(
        shiny::column(width = 4,
                      shiny::selectInput(
                        inputId = "tractbg", 
                        label = "Display Census block groups, tracts, or places?",
                        choices = c("Block groups", "Tracts", "Places"), 
                        multiple = F, 
                        selected = "Block groups"
                      )),
        shiny::column(
          width = 4,
          shiny::selectInput(
            inputId = "montcompare",
            label = "Show Montgomery County data as well?",
            choices = c("Yes", "No"),
            multiple = F,
            selected = "No"
          )
        )),
      
      fluidRow(
        shiny::column(
          width =4,
          shiny::selectInput(
            "pcttot",
            "Show percentages or totals?",
            choices = c("Percentages", "Totals"),
            multiple = F,
            selected = "Percentages"
          )),
        shiny::column(
          width =4,
          shiny::checkboxInput(
            "showwards",
            "Show Ward boundaries?",
            value = T
          )
        )
        
      ),
      # shinythemes::themeSelector(),
      
      leaflet::leafletOutput(
        "tractmap",
        height = 575
      ),
      
      br(),
      
      fluidRow(
        column(
          width = 4,
          downloadButton(
            "downloaddata",
            label = "Download data in map as shapefile"
          )
        ),
        column(
          width = 4,
          downloadButton(
            "downloadexcel",
            label = "Download data in map as Excel file"
          )
        )
      )
    ),
    tabPanel(
      title = "Background",
      
      p("The Takoma Park Interactive Demographic map visualizes data from the 2016-2020", a(href = "https://www.census.gov/programs-surveys/acs", "American Community Survey"), ". Data in the map reflects surveys over the period of 2016 to 2020, not any one year. The map will be updated each year with new data from the ACS. The code used to produce this map can be found on the City's Github. Please email Senior Policy and Data Analyst Dan Powers at ", tags$a(href="mailto:danielp@takomaparkmd.gov", "danielp@takomparkmd.gov"), " with any comments or questions.", .noWS = "before-end")
    )
    
# Define server logic required to draw a histogram
server <- function(input, output) {
  
  selectdf <- reactive({ 
    outdf <- switch(
      input$tractbg,
      "Block groups" = switch(
        input$montcompare, 
        "Yes" = mont_bg, 
        "No" = tp_bg
      ),
      "Tracts" = switch(
        input$montcompare, 
        "Yes" = mont_tract, 
        "No" = tp_tract
      ),
      "Places" = mont_place
    )
    
    # browser()
    outdf %>%
      dplyr::select(-matches("(\\.x.x)|(\\.y)|(\\.z)$", ignore.case = T))
  })
  
  output$tractmap <- renderLeaflet({
    # req(input$tractbg)
    # req(input$pcttot)
    
    pct_est <- switch(
      input$pcttot,
      "Percentages" = "pct",
      "Totals" = "estimate"
    )
    outmap <- gen_acsmap_overall(baseacsdata = selectdf(), pct_est = pct_est, inccountycontrols = F, .collapsed = T)
    
    if (input$showwards){
      outmap <- outmap %>%
        leafletwrappers::add_wards_new()
    }
    
    else{
      outmap <- outmap %>%
        leafletwrappers::add_city()
    }
    
    outmap
    
  })
  
  tpname <- reactive({
    
    ifelse(input$montcompare == 'Yes', "mont", "tp")
  })
  
  output$downloaddata <- downloadHandler(
    filename = function() {
      
      paste0(input$tractbg, "_", tpname(), ".geojson")
      
    },
    content = function(file) {
      sf::st_write(selectdf(), file)
    }
  )
  
  output$downloadexcel <- downloadHandler(
    filename = function(){
      paste0(input$tractbg, "_", tpname(), ".xlsx")
    },
    
    content = function(file){
      openxlsx::write.xlsx(sf::st_drop_geometry(selectdf()), file, asTable = T)
    }
  )
  
}

# Run the application 
shinyApp(ui = ui, server = server)
