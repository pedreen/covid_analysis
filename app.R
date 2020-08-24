suppressMessages({
    library(shiny)
    library(shinydashboard)
    library(shinyWidgets)
    library(rhandsontable)
    # library(tidyverse)
    library(dplyr)
    library(stringr)
    library(tibble)
    library(lubridate)
    library(plotly)
    library(openxlsx)
    library(shinycssloaders)
    library(waiter)
    library(leaflet)
    library(janitor)
    library(sf)
    library(coronabr)
    library(RColorBrewer)
    library(glue)
    library(caret)
    library(DT)
    library(caTools)
    library(e1071)
    library(DataExplorer)
    library(forecast)

    
})

options(warn = -1, scipen = 999)

# Carregando base do Covid-19
source("www/bases.R")

# Load modules
source('modules/mod_overview.R')
files.sources = list.files('modules/model_explorer', full.names = TRUE)
sapply(c(files.sources), source)

# sidebar
sidebar <- dashboardSidebar(
    
    includeCSS("www/style.css"), 
    
    #useShinyjs(),
    
    use_waiter(),
    
    show_waiter_on_load(
        logo = "logo.png",
        tagList(
            spin_fading_circles(), 
            span("Carregando Dashboard ...", style = "color: white; font-size: 24px;")
        )
    ),
    
    sidebarMenu(id = "sidebar",
                menuItem("Overview", tabName = "overview",
                         icon = icon("far fa-chart-bar"), startExpanded = TRUE
                ), 
                pickerInput("ufs",
                            "Selecione a UF:",
                            choices =  state_list_br,
                            options = list(`live-search` = TRUE,`actions-box` = TRUE),
                            multiple = TRUE,
                            selected = state_list_br,
                            width = '98%')
    )
)

# ui
ui <- dashboardPage(
    skin = "yellow",
    title = "Inatel - 4intelligence", 
    dashboardHeader(title = tags$a(tags$img(src = "4i_logo.png", width = "100%", style = "margin-bottom: 5px;"))), 
    sidebar,
    dashboardBody(
        tabItems(
            tabItem(tabName = "overview", overview_ui("overview")),
            
            tabItem(tabName = "ia_module", overview_ui("ia_module_ui")) 
            
            
        )
    )
)

# server
server <- function(input, output, session) {
    
    callModule(overview_server, "overview", uf_selected = reactive(input$ufs))
    
    
    # Loading... ----
    waiter_hide()
    
    
}

shinyApp(ui, server)

