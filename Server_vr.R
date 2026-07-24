



# ===============================
# SERVER
# ===============================
server <- function(input, output, session) {
  
  observeEvent(input$districts, {
                                 updateCheckboxGroupInput(session, 
                                                          "status_filter",
                                                          selected = "On-going"
                                                         #c("Completed", "Planned", "On-going")
                                                          )
                                 }
               )
  
  # selected district polygon ----
  filtered_data <- reactive({
                             req(input$districts)
                              volta_data %>% filter(NAME_2 == input$districts)
                            })
  
  # projects filtered by district, status, and type ----
  filtered_projects <- reactive({
                                 req(input$districts)
                                 pts <- vrcc_projects_sf
                                 pts <- pts %>% filter(District == input$districts)
    
                                if (!is.null(input$status_filter) && length(input$status_filter) > 0) {
                                  pts <- pts %>% filter(Status %in% input$status_filter)
                                } else {
                                  return(pts[0, ])
                                }
                                
                                if (!is.null(input$type_filter) && length(input$type_filter) > 0) {
                                  pts <- pts %>% filter(Project.Type %in% input$type_filter)
                                } else {
                                  return(pts[0, ])
                                }
                                 
                                pts
                               })
  
  # ---- Color function (valid marker colours) ----
  getColor <- function(status) {
                                case_when(
                                          status == "Completed" ~ "green",
                                          status == "Planned"   ~ "orange",
                                          status == "On-going"  ~ "blue",
                                          TRUE ~ "gray"
                                        )
                               }
  
  # ---- Main Map ----
  output$vr_map <- renderLeaflet({
                                leaflet() %>%
                                  addProviderTiles(providers$OpenStreetMap) %>%
                                  setView(lng = -0.2, lat = 6.5, zoom = 7)
                                })
  
  observe({
    df <- filtered_data()
    pts <- filtered_projects()
    req(nrow(df) > 0)
    
    center <- st_centroid(st_union(df))
    coords <- st_coordinates(center)
    
    leafletProxy("vr_map") %>%
      clearShapes() %>%
      clearMarkers() %>%
      setView(lng = coords[1], lat = coords[2], zoom = 10) %>%
      addPolygons(data = df,
                  fillColor = ~pal(NAME_2),
                  fillOpacity = 0.7,
                  color = "black",
                  weight = 1,
                  label = ~NAME_2) %>%
      { if (nrow(pts) > 0) {
        addAwesomeMarkers(.,
                          data = pts,
                          lng = ~longitude,
                          lat = ~latitude,
                          icon = ~awesomeIcons(icon = "wrench",
                                               iconColor = "white",
                                               library = "fa",
                                               markerColor = getColor(Status)),
                          
                          popup = ~paste(
                                        "<b>Project Type:</b>", Project.Type,
                                        "<br><br>",   
                                        "<b>Project Name:</b>", Project.Name,
                                        "<br><b>Location:</b>", Location,
                                        "<br><b>Summary:</b>", Summary,
                                        "<br><b>Launched Date:</b>", Launched.Date,
                                        "<br><b>Status:</b>", Status,
                                        "<br><br>",
                                        "<br><b>Link to work progress pics/info:</b>", Link
                                         )
                         )
      } else {
        .
      }
      }
  })
  
  #  Downloading
  output$download_map <- downloadHandler(
    filename = function() {
      paste0("vrccproj_", input$districts, "", format(Sys.time(), "%Y-%m-%d%H-%M-%S"), ".html")
    },
    content = function(file) {
      df <- filtered_data()
      pts <- filtered_projects()
      req(nrow(df) > 0)
      
      center <- st_centroid(st_union(df))
      coords <- st_coordinates(center)
      
      map <- leaflet() %>%
        addProviderTiles(providers$OpenStreetMap) %>%
        setView(lng = coords[1], lat = coords[2], zoom = 10) %>%
        addPolygons(data = df, fillColor = ~pal(NAME_2), fillOpacity = 0.7,
                    color = "black", weight = 1, label = ~NAME_2)
      
      if (nrow(pts) > 0) {
        map <- map %>%
          addAwesomeMarkers(data = pts,
                            lng = ~longitude,
                            lat = ~latitude,
                            icon = ~awesomeIcons(icon = "wrench",
                                                 iconColor = "white",
                                                 library = "fa",
                                                 markerColor = getColor(Status)),
                            
                            popup = ~paste(
                                          "<b>Project Type:</b>", Project.Type,
                                          "<br><br>", 
                                          "<b>Project Name:</b>", Project.Name,
                                          "<br><b>Location:</b>", Location,
                                          "<br><b>Summary:</b>", Summary,
                                          "<br><b>Launched Date:</b>", Launched.Date,
                                          "<br><b>Status:</b>", Status,
                                          "<br><br>", 
                                          "<br><b>Link to work progress pics/info:</b>", Link
                                           )
                            
                            )
      }
      
      temp_map_file <- tempfile(fileext = ".html")
      saveWidget(map, temp_map_file, selfcontained = TRUE)
      map_html_lines <- readLines(temp_map_file, warn = FALSE)
      map_html_content <- paste(map_html_lines, collapse = "\n")
      
      body_match <- regexpr("<body[^>]>.?</body>", map_html_content, perl = TRUE, ignore.case = TRUE)
      if (body_match != -1) {
        body_content <- regmatches(map_html_content, body_match)
      } else {
        body_content <- map_html_content
      }
      
      district_name <- input$districts
      status_text <- if (!is.null(input$status_filter) && length(input$status_filter) > 0) {
        paste(input$status_filter, collapse = ", ")
      } else { "No status selected" }
      type_text <- if (!is.null(input$type_filter) && length(input$type_filter) > 0) {
        paste(input$type_filter, collapse = ", ")
      } else { "No type selected" }
      heading_text <- paste0("Projects in Volta Region - ", district_name, " | Status: ", status_text, " | Type: ", type_text)
      
      final_html <- sprintf(
        '<!DOCTYPE html>
        <html>
        <head>
          <meta charset="utf-8">
          <title>VRCC Projects - %s</title>
          <style>
            * { margin:0; padding:0; box-sizing:border-box; }
            body { font-family: Arial, sans-serif; background: #f5f5f5; }
            .header {
              background: #ace500; padding: 15px 20px; text-align: center;
              font-size: 1.4rem; font-weight: bold; color: #1e1e4c;
              border-bottom: 2px solid #1e1e4c; width: 100%%; z-index: 100;
              position: relative;
            }
            .map-wrapper { width: 100%%; height: calc(100vh - 80px); overflow: hidden; }
            .watermark {
              position: fixed; bottom: 10px; right: 10px;
              background: rgba(255,255,255,0.85); padding: 5px 12px;
              border-radius: 6px; font-size: 12px; font-family: monospace;
              color: #333; z-index: 10000; pointer-events: none;
              box-shadow: 0 1px 3px rgba(0,0,0,0.2);
            }
          </style>
        </head>
        <body>
          <div class="header">%s</div>
          <div class="map-wrapper">%s</div>
          <div class="watermark">(c) 2026 Volta Region. Creator: D.S.A (for Regional Minister)</div>
        </body>
        </html>',
        district_name,
        heading_text,
        body_content
      )
      
      writeLines(final_html, file, useBytes = TRUE)
      unlink(temp_map_file)
    }
  )
}







