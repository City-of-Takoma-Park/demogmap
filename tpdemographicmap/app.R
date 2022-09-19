# purpose: generates takoma park interactive demographic map
# last run: 09/19/2022


library(shiny)
library(leaflet)
library(tidyverse)
library(shinythemes)
library(openxlsx)
library(sf)
library(htmlwidgets)

library(mapview)

# packages i've created - download with devtools::install_github("dpowerstp/packagename")
library(leafletwrappers)
library(tpfuncts)
library(acsmapping)

# library(webshot)

# uncomment both of these lines of code and run this code to deply the app
# setwd("~/r_proj/demogmap/tpdemographicmap")
# rsconnect::deployApp(appName = "tpdemographicmap", appTitle = "Takoma Park Demographic Map", account = "takomapark")

# read in data

tp_bg <- readRDS("./data/bg_tp.rds")

tp_tract <- readRDS("./data/tract_tp.rds")

mont_bg <- readRDS("./data/mont_bg.rds")

mont_tract <- readRDS("./data/mont_tract.rds")

mont_place <- readRDS("./data/mont_place.rds")

tp_place <- readRDS("./data/place_tp.rds")

fipsmatch <- list(
  "Montgomery County" = "031", 
  "PG County" = "033",
  "DC" = "001"
)

# phantomjs_path <- webshot:::find_phantom()
# if (is.null(phantomjs_path)){
#   webshot::install_phantomjs()
#   FlgJS <- F
# } else{
#   FlgJS <- T
# }

filtfunct <- function(df, filtlist){
  
  filtvals <- unlist(
    fipsmatch[filtlist], 
    use.names = F
  )
  
  df %>%
    dplyr::filter(countyfp %in% filtvals | tp_col)
}

ui <- fluidPage(
  theme = shinytheme("lumen"),
  # Application title
  titlePanel("Takoma Park Interactive Demographic Map"),
  
  
  tabsetPanel(
    type = "pills",
    tabPanel(
      title = "Demographic map",
      br(),
      
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
            label = "Show Montgomery County, PG County, or DC data as well (Note: may take a moment to load)?",
            choices = c("Montgomery County", "PG County", "DC"),
            multiple = T,
            selected = "None"
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
          ),
          shiny::checkboxInput(
            "showlayers",
            "Show layers as collapsed?",
            value = T
          )
        )
        
      ),
      # shinythemes::themeSelector(),
      
      leaflet::leafletOutput(
        "tractmap",
        height = 575,
        width = "95%"
      ),
      
      br(),
      
      fluidRow(
        column(
          width = 3,
          downloadButton(
            "downloaddata",
            label = "Download data in map as shapefile"
          )
        ),
        column(
          width = 3,
          downloadButton(
            "downloadexcel",
            label = "Download data in map as Excel file"
          )
        )
        # column(
        #   width = 3,
        #   downloadButton(
        #     "pngmap",
        #     label = "Download screenshot of map"
        #   )
        # )
      )
      
      # actionButton(
      #   "update",
      #   "Update map"
      # )
    ),
    tabPanel(
      title = "Background",
      
      br(),
      
      p("The Takoma Park Interactive Demographic map visualizes data from the 2016-2020", a(href = "https://www.census.gov/programs-surveys/acs", "American Community Survey"), "for Takoma Park and Montgomery County. You can select or deselect layers on the map by hovering your mouse over the stack on the left side of the map, and adjust the options to change what data the map displays. Hovering your mouse over a shape will show additional data on the geography. You can also download data displayed on the map as a shapefile or Excel file by hitting a button below the map. Data in the map are from surveys conducted by the Census bureau over the period of 2016 to 2020, not any one year. The map will be updated each year with new data from the ACS. Geographies shown on the map are defined by the Census; block groups and tracts are smaller geographies about the size of neighborhoods, while places are towns and cities. Definitions of terms like Census Block can be found on the", a(href = "https://www.census.gov/programs-surveys/geography/about/glossary.html", "Census Glossary"),  "webpage."),
      
      p("The code used to produce this map can be found on the ", tags$a(href = "https://github.com/City-of-Takoma-Park/demogmap", "City's Github."), "Residents may also be interested in the", tags$a(href = "https://r.takomaparkmd.gov/hcd/takomaparkexplorer.html", "City's Data Explorer,"), "which has non-spatial data visualizations of ACS data. Please email Senior Policy and Data Analyst Dan Powers at ", tags$a(href="mailto:danielp@takomaparkmd.gov", "danielp@takomparkmd.gov"), " with any comments or questions.", .noWS = "before-end")
    ))
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  selectdf <- reactive({ 
    
    if (is.null(input$montcompare)){
      outdf <- switch(
        input$tractbg,
        "Block groups" = tp_bg,
        "Tracts" = tp_tract,
        "Places" = tp_place
      )
    }
    
    else if (!is.null(input$montcompare)){
      outdf <- switch(
        input$tractbg,
        "Block groups" = mont_bg,
        "Tracts" = mont_tract,
        "Places" = mont_place
      )
      
      outdf <- outdf %>%
        filtfunct(input$montcompare)
    }
    
    # browser()
    outdf %>%
      dplyr::select(-matches("(\\.x.x)|(\\.y)|(\\.z)$", ignore.case = T))
  })
  
  outmap <- reactive({
    # req(input$tractbg)
    # req(input$pcttot)
    
    # req(input$update)
    
    pct_est <- switch(
      input$pcttot,
      "Percentages" = "pct",
      "Totals" = "estimate"
    )
    
    outmap <- gen_acsmap_overall(baseacsdata = selectdf(), pct_est = pct_est, inccountycontrols = F, .collapsed = input$showlayers)
    
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
  
  output$tractmap <- renderLeaflet({
    outmap()
  })
  
  tpname <- reactive({
    
    ifelse(is.null(input$montcompare), "tp", "mont")
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
  
  # widgsave <- reactive({
  #   saveWidget(widget = outmap(), file = "webmap.html")
  # }) %>%
  #   bindEvent(input$pngmap)
  
  # output$pngmap <- downloadHandler(
  #   filename= function(){
  #     paste0("map_", tpname(), ".png")
  #   },
  #   
  #   content = function(file){
  #     
  #     owd <- setwd(tempdir())
  #     on.exit(setwd(owd))
  # 
  #     saveWidget(widget = outmap(), file = "webmap.html", selfcontained = F)
  #     webshot::webshot("webmap.html", file = file)
  #     
  #     # mapview::mapshot(x = outmap(), file = file, remove_controls = T)
  #   }
  # )
  
}

# Run the application 
shinyApp(ui = ui, server = server)
