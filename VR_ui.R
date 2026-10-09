# 
# 
# 
# # Load required libraries
# library(shiny)
# library(shinydashboard)
# library(leaflet)
# library(sf)
# library(dplyr)
# library(readxl)
# library(shinyWidgets)
# library(htmlwidgets)
# library(dplyr)
# library(htmltools)
# 
# 
# 
# # ===============================
# # Data Wrangling
# # ===============================
# # Load shapefile (already saved as .rda)
# load("ghanashapefile")
# ghana_shapefile <- st_make_valid(ghana_shapefile)
# ghana_shapefile <- st_transform(ghana_shapefile, 4326)
# 
# volta_data <- ghana_shapefile %>% filter(NAME_1 == "Volta")
# volta_districts <- sort(unique(volta_data$NAME_2))
# 
# pal <- colorFactor("Set3", domain = volta_districts)
# 
# vrcc_projects <- read_excel("vrcc.xlsx", sheet = "Sheet1")
# vrcc_pictures <- read_excel("vrcc.xlsx", sheet = "Sheet2") %>%
#                  filter(!is.na(Picture_URL), nzchar(trimws(Picture_URL)))
# 
# picture_links <- vrcc_pictures %>%
#                 group_by(Project_ID) %>%
#                 summarise(
#                 photo_html = paste0(
#                                 '<a href="', htmlEscape(Picture_URL), '" target="_blank" rel="noopener">',
#                                 htmlEscape(if_else(
#                                   is.na(Caption) | Caption == "",
#                                   paste("View picture", row_number()),
#                                   paste(Caption, row_number())
#                                  )),
#                                  "</a>",
#                                  collapse = "<br>"
#                                                   ),
#                                  .groups = "drop"
#                           )
# 
# vrcc_projects <- vrcc_projects %>%
#                  left_join(picture_links, by = "Project_ID") %>%
#                  mutate(
#                         photo_html = if_else(
#                                         is.na(photo_html), "No pictures available", photo_html
#                                         )
#                    )
# 
# 
# picture_data <- readxl::read_excel("vrcc.xlsx",  sheet = "Sheet2",  col_types = "text")
# 
# 
# # 
# # make_photo_gallery <- function(project_id) {
# #   photos <- picture_data[
# #     !is.na(picture_data$Project_ID) &
# #       picture_data$Project_ID == project_id &
# #       !is.na(picture_data$Picture_URL) &
# #       nzchar(trimws(picture_data$Picture_URL)),
# #     ,
# #     drop = FALSE
# #   ]
# #   
# #   if (nrow(photos) == 0) return("No pictures available")
# #   
# #   captions <- photos$Caption
# #   captions[is.na(captions) | !nzchar(trimws(captions))] <- "Picture"
# #   
# #   items <- vapply(seq_len(nrow(photos)), function(i) {
# #     url <- htmltools::htmlEscape(photos$Picture_URL[i])
# #     caption <- htmltools::htmlEscape(captions[i])
# #     
# #     paste0(
# #       '<div style="margin-bottom:12px;">',
# #       '<a href="', url, '" target="_blank" rel="noopener noreferrer">',
# #       '<img src="', url,
# #       '" style="width:280px;max-width:100%;height:auto;',
# #       'border-radius:6px;" alt="', caption, '">',
# #       '</a>',
# #       '<br><a href="', url, '" target="_blank" rel="noopener noreferrer">',
# #       caption, ' ', i, ' — View full picture</a>',
# #       '</div>'
# #     )
# #   }, character(1))
# #   
# #   visible_pictures <- paste(head(items, 2), collapse = "")
# #   
# #   if (length(items) <= 2) return(visible_pictures)
# #   
# #   number_more <- length(items) - 2
# #   
# #   paste0(
# #     visible_pictures,
# #     '<details class="vr-photo-toggle" style="margin-top:8px;">',
# #     '<summary style="cursor:pointer;color:#1676a2;',
# #     'text-decoration:underline;font-weight:bold;">',
# #     '<span class="show-more">View ', number_more, ' more picture',
# #     if (number_more == 1) '' else 's',
# #     '</span>',
# #     '<span class="show-fewer">Show only 2 pictures</span>',
# #     '</summary>',
# #     '<div style="margin-top:12px;">',
# #     paste(items[-c(1, 2)], collapse = ""),
# #     '</div>',
# #     '</details>'
# #   )
# # }
# #=======================
# make_photo_gallery <- function(project_id) {
#   if (is.na(project_id)) return("")
#   
#   photos <- picture_data[
#     !is.na(picture_data$Project_ID) &
#       picture_data$Project_ID == project_id &
#       !is.na(picture_data$Picture_URL) &
#       nzchar(trimws(picture_data$Picture_URL)),
#     ,
#     drop = FALSE
#   ]
#   
#   if (nrow(photos) == 0) {
#     return("<b>Pictures and videos:</b> None available")
#   }
#   
#   # Recognize videos from Media_Type, if that column exists,
#   # or from a Cloudinary video URL / MP4 URL.
#   media_types <- rep("image", nrow(photos))
#   
#   if ("Media_Type" %in% names(photos)) {
#     entered_types <- tolower(trimws(photos$Media_Type))
#     media_types[!is.na(entered_types) & entered_types == "video"] <- "video"
#   }
#   
#   video_urls <- grepl(
#     "/video/upload/|\\.mp4($|\\?)",
#     photos$Picture_URL,
#     ignore.case = TRUE
#   )
#   media_types[video_urls] <- "video"
#   
#   captions <- photos$Caption
#   missing_caption <- is.na(captions) | !nzchar(trimws(captions))
#   captions[missing_caption] <- ifelse(
#     media_types[missing_caption] == "video",
#     "Video",
#     "Picture"
#   )
#   
#   items <- vapply(seq_len(nrow(photos)), function(i) {
#     url <- htmltools::htmlEscape(photos$Picture_URL[i])
#     caption <- htmltools::htmlEscape(captions[i])
#     
#     if (media_types[i] == "video") {
#       paste0(
#         '<div style="margin:12px 0;">',
#         '<video controls playsinline preload="none" ',
#         'style="width:280px;max-width:100%;border-radius:6px;">',
#         '<source src="', url, '" type="video/mp4">',
#         'Your browser cannot play this video.',
#         '</video>',
#         '<br><a href="', url,
#         '" target="_blank" rel="noopener noreferrer">',
#         caption, ' — Open video</a>',
#         '</div>'
#       )
#     } else {
#       paste0(
#         '<div style="margin:12px 0;">',
#         '<a href="', url,
#         '" target="_blank" rel="noopener noreferrer">',
#         '<img src="', url,
#         '" style="width:280px;max-width:100%;height:auto;',
#         'border-radius:6px;" alt="', caption, '">',
#         '</a>',
#         '<br><a href="', url,
#         '" target="_blank" rel="noopener noreferrer">',
#         caption, ' — View full picture</a>',
#         '</div>'
#       )
#     }
#   }, character(1))
#   
#   media_label <- if (any(media_types == "video") &&
#                      any(media_types == "image")) {
#     "pictures and videos"
#   } else if (any(media_types == "video")) {
#     "videos"
#   } else {
#     "pictures"
#   }
#   
#   paste0(
#     # Include the toggle styling with the popup so it also works
#     # in the downloaded HTML map.
#     '<style>',
#     '.vr-media-toggle .vr-hide {display:none;}',
#     '.vr-media-toggle[open] .vr-view {display:none;}',
#     '.vr-media-toggle[open] .vr-hide {display:inline;}',
#     '</style>',
#     
#     '<b>Project media:</b><br>',
#     '<details class="vr-media-toggle" style="margin-top:6px;">',
#     '<summary style="cursor:pointer;color:#1676a2;',
#     'text-decoration:underline;font-weight:bold;">',
#     '<span class="vr-view">View ', media_label,
#     ' (', nrow(photos), ')</span>',
#     '<span class="vr-hide">Hide ', media_label, '</span>',
#     '</summary>',
#     '<div style="margin-top:12px;">',
#     paste(items, collapse = ""),
#     '</div>',
#     '</details>'
#   )
# }
# 
# #=========================
# 
# # Clean column names (remove spaces)
# names(vrcc_projects) <- make.names(names(vrcc_projects))
# # Convert date to character for popup
# vrcc_projects$Launched.Date <- as.character(vrcc_projects$Launched.Date)
# # Remove rows with missing coordinates
# vrcc_projects <- vrcc_projects[!is.na(vrcc_projects$latitude) & !is.na(vrcc_projects$longitude), ]
# 
# # Create an sf version for spatial operations (optional, but we'll use for intersection)
# vrcc_projects_sf <- st_as_sf(vrcc_projects,
#                              coords = c("longitude", "latitude"),
#                              crs = 4326,
#                              remove = FALSE)
# 
# # ---- Get unique Project Types for filter ----
# project_types <- sort(unique(vrcc_projects$Project.Type))
# project_types <- project_types[!is.na(project_types)]  # remove NA
# 


# ===============================
# UI_VRCC.R
# ===============================

project_choices <- sort(unique(
  as.character(na.omit(vrcc_projects$Project_Type))
))

default_project_type <- if ("Infrastructure" %in% project_choices) {
  "Infrastructure"
} else {
  project_choices[1]
}

ui <- dashboardPage(
  
  dashboardHeader(
    title = tags$div(
      tags$strong(
        "VRCC",
        style = "font-size: 20px; line-height: 1.2;"
      ),
      tags$div(
        "Volta Regional Coordinating Council",
        style = "font-size: 12px; font-weight: normal; opacity: 0.8;"
      )
    )
  ),
  
  dashboardSidebar(
    width = 300,
    
    # Filters for Projects Map
    conditionalPanel(
      condition = "input.main_tabs === 'Projects Map'",
      
      selectInput(
        inputId = "districts",
        label = "Select a District/Municipal",
        choices = volta_districts,
        selected = "Ho Municipal"
      ),
      
      selectInput(
        inputId = "type_filter",
        label = "Project Type",
        choices = project_choices,
        selected = default_project_type
      ),
      
      checkboxGroupInput(
        inputId = "status_filter",
        label = "Project Status",
        choices = c("Completed", "Planned", "On-going"),
        selected = "On-going"
      ),
      
      br(),
      hr(),
      
      downloadButton(
        "download_map",
        "Download VR Map for Selected District"
      )
    ), br(), br(), 
    
    # Filters for Ho Municipal
    conditionalPanel(
      condition = "input.main_tabs === 'Ho Municipal'",
      
      selectInput(
        inputId = "hm_type_filter",
        label = "Project Type",
        choices = project_choices,
        selected = default_project_type
      ),
      
      br(),
      hr(),
      
      downloadButton(
        "download_hm_map",
        "Download Ho Municipal Map"
      )
    ), br(), br(), 
    
    # Filters for Overview
    conditionalPanel(
      condition = "input.main_tabs === 'overview'",
      
      h4(
        "Filters for Overview Tab",
        style = "color: white; padding: 10px 15px 0;"
      ),
      
      div(
        style = "padding: 0 15px 15px;",
        
        selectInput(
          inputId = "overview_region",
          label = "Region",
          choices = c(
            "All", sort(unique(na.omit(sample_df$Region)))
          ),
          selected = "All"
        ),
        
        selectInput(
          inputId = "overview_gender",
          label = "Gender",
          choices = c(
            "All", sort(unique(na.omit(sample_df$Gender)))
          ),
          selected = "All"
        ),
        
        sliderInput(
          inputId = "overview_age",
          label = "Age",
          min = floor(min(sample_df$Age, na.rm = TRUE)),
          max = ceiling(max(sample_df$Age, na.rm = TRUE)),
          value = c(
            floor(min(sample_df$Age, na.rm = TRUE)),
            ceiling(max(sample_df$Age, na.rm = TRUE))
          )
        ),
        
        sliderInput(
          inputId = "overview_purchases",
          label = "Previous Purchases",
          min = floor(min(
            sample_df$Previous_Purchases,
            na.rm = TRUE
          )),
          max = ceiling(max(
            sample_df$Previous_Purchases,
            na.rm = TRUE
          )),
          value = c(
            floor(min(
              sample_df$Previous_Purchases,
              na.rm = TRUE
            )),
            ceiling(max(
              sample_df$Previous_Purchases,
              na.rm = TRUE
            ))
          )
        )
      )
    ),
    
    tags$div(
      style = paste(
        "padding: 20px 15px;",
        "color: #B8C9CC;",
        "font-size: 12px;"
      ),
      HTML(
        "&copy; 2026 Volta Regional Coordinating Council (VRCC)<br>
         Creator: D.A Analytics (senyoackuaku@gmail.com; +233550359499)"
      )
    )
  ),
  
  dashboardBody(
    
    tags$head(
      tags$style(HTML("
        .content-wrapper, .right-side {
          background-color: #F3F7F6 !important;
        }

        .main-header .logo,
        .main-header .navbar {
          background-color: #0B6E75 !important;
        }

        .main-sidebar, .left-side {
          background-color: #123B46 !important;
        }

        .box {
          border-top-color: #0B6E75 !important;
        }
        
        /* Compact status control over the Ho Municipal map */
.hm-status-strip {
  background: rgba(255, 255, 255, 0.96);
  padding: 5px 8px;
  border-radius: 6px;
  box-shadow: 0 1px 5px rgba(0, 0, 0, 0.25);
  max-width: calc(100vw - 20px);
}

.hm-status-strip .shiny-input-container {
  width: auto !important;
  margin-bottom: 0;
}

.hm-status-strip .control-label {
  display: block;
  font-size: 12px;
  margin-bottom: 2px;
}

.hm-status-strip .shiny-options-group {
  display: flex;
  flex-wrap: nowrap;
  gap: 10px;
  overflow-x: auto;
  white-space: nowrap;
}

.hm-status-strip .radio-inline {
  margin-left: 0 !important;
  padding-top: 0;
}

@media (max-width: 767px) {
  .hm-status-strip {
    font-size: 12px;
  }

  .hm-status-strip .shiny-options-group {
    gap: 8px;
  }
}
      "))
    ),
    
    # Existing gallery CSS/JavaScript object
    media_gallery_assets,
    
    fluidRow(
      tabBox(
        id = "main_tabs",
        width = 12,
        
        tabPanel(
          "Projects Map",
          leafletOutput("vr_map", height = "600px")
        ), br(), hr(), hr(), 
        
        tabPanel(
          "Ho Municipal",
          
          div(
            style = "position: relative; height: 600px;",
            
            leafletOutput("hm_map", height = "600px"), 
            
            absolutePanel(
              top = 10,
              right = 10,
              width = 175,
              draggable = FALSE,
              style = paste(
                "z-index: 1100;",
                "background: white;",
                "padding: 10px 12px;",
                "border-radius: 6px;",
                "box-shadow: 0 2px 8px rgba(0,0,0,0.25);"
              ),
              
              # radioButtons(
              #   inputId = "hm_status_filter",
              #   label = "Project Status",
              #   choices = c(
              #     "All",
              #     "On-going",
              #     "Completed",
              #     "Planned"
              #   ),
              #   selected = "On-going"
              # )
              div(
                class = "hm-status-strip",
                
                radioButtons(
                  inputId = "hm_status_filter",
                  label = "Project Status",
                  choices = c("All", "Completed", "Planned", "On-going"),
                  selected = "All",
                  inline = TRUE
                )
              )
              
            )
          ) 
        ),
        
        tabPanel(
          "Overview",
          value = "overview",
          
          fluidRow(
            valueBoxOutput("overview_total_customers", width = 4),
            valueBoxOutput("overview_conversion_rate", width = 4),
            valueBoxOutput("overview_total_spent", width = 4)
          ),
          
          fluidRow(
            column(
              width = 12,
              downloadButton(
                "overview_download_report",
                "Download Report (Overview)"
              ),
              br(),
              br()
            )
          ),
          
          fluidRow(
            box(
              title = "Conversion by Campaign",
              width = 6,
              plotlyOutput(
                "overview_conversion_plot",
                height = "350px"
              )
            ),
            
            box(
              title = "Spend by Region",
              width = 6,
              plotlyOutput(
                "overview_region_spend",
                height = "350px"
              )
            )
          ),
          
          fluidRow(
            box(
              title = "Income vs Amount Spent",
              width = 12,
              plotlyOutput(
                "overview_scatter_plot",
                height = "400px"
              )
            )
          )
        )
      )
    )
  ),
  
  skin = "green"
)


