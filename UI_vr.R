


# Load required libraries
library(shiny)
library(shinydashboard)
library(leaflet)
library(sf)
library(dplyr)
library(readxl)
library(shinyWidgets)
library(htmlwidgets)




# ===============================
# Data Wrangling
# ===============================
# Load shapefile (already saved as .rda)
load("ghanashapefile")
ghana_shapefile <- st_make_valid(ghana_shapefile)
ghana_shapefile <- st_transform(ghana_shapefile, 4326)

volta_data <- ghana_shapefile %>% filter(NAME_1 == "Volta")
volta_districts <- sort(unique(volta_data$NAME_2))

pal <- colorFactor("Set3", domain = volta_districts)

# ---- Read VRCC projects (with Project Type) ----
vrcc_projects <- read_excel("vrcc.xlsx", sheet = "Sheet1")
# Clean column names (remove spaces)
names(vrcc_projects) <- make.names(names(vrcc_projects))
# Convert date to character for popup
vrcc_projects$Launched.Date <- as.character(vrcc_projects$Launched.Date)
# Remove rows with missing coordinates
vrcc_projects <- vrcc_projects[!is.na(vrcc_projects$latitude) & !is.na(vrcc_projects$longitude), ]

# Create an sf version for spatial operations (optional, but we'll use for intersection)
vrcc_projects_sf <- st_as_sf(vrcc_projects,
                             coords = c("longitude", "latitude"),
                             crs = 4326,
                             remove = FALSE)

# ---- Get unique Project Types for filter ----
project_types <- sort(unique(vrcc_projects$Project.Type))
project_types <- project_types[!is.na(project_types)]  # remove NA



# ===============================
# UI
# ===============================
ui <- dashboardPage(
  dashboardHeader(
    
    
    
    title = tags$div(
                    tags$strong("VRCC", style = "font-size: 20px; line-height: 1.2;"),
                    tags$div("Volta Regional Coordinating Council",
                    style = "font-size: 12px; font-weight: normal; opacity: 0.8;")
                    )
  ),
  
  dashboardSidebar(
    width = 300,
    selectInput("districts", 
                "Select a District/Municipal",
                choices = volta_districts,
                selected = "Ho Municipal",
                multiple = FALSE
    ),
    br(),
    
    selectInput("type_filter",
                "Project Type",
                choices = project_types,
                selected = "Infrastructure",   # single‑select, initial set
                multiple = FALSE
    ),
    br(),
    
    checkboxGroupInput("status_filter", 
                       "Project Status",
                       choices = c("Completed", "Planned", "On-going"),
                       selected = c("Completed", "Planned", "On-going")  # all checked
    ),
    br(),
    
    downloadButton("download_map", 
                   "Download Map for Selected District", 
                   style = "width:100%;"
    ),
    br(), br(),
    
    tags$div(
      style = "position:absolute; bottom:20px; left:15px; right:15px;",
      tags$small(HTML("&copy; 2026 Volta Regional Coordinating Council<br/>Creator: D.A (Se Nyo) for VRCC"),
                 style = "color:gray; font-size:12px;")
    )
  ),
  
  dashboardBody(
    tags$head(
      tags$style(HTML("
                  .content-wrapper, .right-side {
                    background-color: #bee7c0;
                  }
                  .box {
                    border-top: 3px solid #ace500;
                  }
                "))
    ),
    
    fluidRow(
      box(
        #title = "Government of Ghana Projects' Map in Volta Region",
        
        
        title = tagList(
          
          tags$img(
            src = "ghana_logo.png",
            height = "30px",
            style = "margin-right:10px;"
          ),
          
          "Government of Ghana Projects' Map in Volta Region" ,
          
          tags$img(
            src = "vrcc_logo.png",
            height = "30px",
            style = "margin-left:10px; float:right;"
          )
          
        ),
        
        
        
        status = "primary",
        solidHeader = TRUE,
        width = 12,
        height = "650px",
        leafletOutput("vr_map", height = "600px")
      )
    ), br(), hr()
  ),
  skin = "purple"  #c("green", "blue", "red", "purple", "yellow", "black")
)
