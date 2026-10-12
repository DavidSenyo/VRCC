
# ===============================
# SERVER_VRCC.R
# ===============================

# Make the standalone HTML map fit below its heading on a phone.
make_download_map_mobile <- function(map) {
  map$width <- "100%"
  map$height <- "100vh"
  
  map <- htmlwidgets::prependContent(
    map,
    htmltools::tags$head(
      htmltools::tags$meta(
        name = "viewport",
        content = "width=device-width, initial-scale=1"
      ),
      htmltools::tags$style(htmltools::HTML("
        html, body {
          width: 100%;
          height: 100%;
          margin: 0;
          padding: 0;
          overflow: hidden;
        }

        .leaflet.html-widget {
          width: 100% !important;
          height: calc(100vh - var(--vr-header-height, 65px)) !important;
          height: calc(100dvh - var(--vr-header-height, 65px)) !important;
          min-height: 0 !important;
        }

        .leaflet-popup-content-wrapper {
          max-width: calc(100vw - 45px);
        }

        .leaflet-popup-content {
          max-width: calc(100vw - 85px);
          overflow-wrap: anywhere;
        }

        .leaflet-popup-content img,
        .leaflet-popup-content video {
          max-width: 100%;
          height: auto;
        }
      "))
    )
  )
  
  map <- htmlwidgets::onRender(
    map,
    "
    function(el, x) {
      var leafletMap = this;

      function resizeMap() {
        var header = document.querySelector('.vr-header');
        var headerHeight = header
          ? Math.ceil(header.getBoundingClientRect().height)
          : 65;

        document.documentElement.style.setProperty(
          '--vr-header-height',
          headerHeight + 'px'
        );

        setTimeout(function() {
          leafletMap.invalidateSize();
        }, 50);
      }

      setTimeout(resizeMap, 300);
      window.addEventListener('resize', resizeMap);
      window.addEventListener('orientationchange', function() {
        setTimeout(resizeMap, 300);
      });
    }
    "
  )
  
  map
}

server <- function(input, output, session) {
  
  # ============================================================
  # SHARED MAP FUNCTIONS
  # ============================================================
  
  project_marker_icons <- function(pts) {
    icon_by_type <- c(
      "Infrastructure"      = "home",
      "The Big Push"        = "wrench",
      "24hr Eco Market"     = "flag",
      "Infrastructure-Road" = "road",
      "New Project Type 5"  = "plus",
      "New Project Type 6"  = "leaf"
    )
    
    
    library_by_type <- c(
      "Infrastructure"      = "glyphicon",
      "The Big Push"        = "glyphicon",
      "24hr Eco Market"     = "fa",
      "Infrastructure-Road" = "glyphicon",
      "New Project Type 5"  = "glyphicon",
      "New Project Type 6"  = "glyphicon"
    )
    
    marker_color_by_type <- c(
      "Infrastructure"      = "blue",
      "The Big Push"        = "orange",
      "24hr Eco Market"     = "green",
      "Infrastructure-Road" = "purple",
      "New Project Type 5"  = "red",
      "New Project Type 6"  = "cadetblue"
    )
    
    project_type <- as.character(pts$Project_Type)
    
    icons <- unname(icon_by_type[project_type])
    libraries <- unname(library_by_type[project_type])
    marker_colors <- unname(marker_color_by_type[project_type])
    
    # Defaults for types not listed above.
    icons[is.na(icons)] <- "home"
    libraries[is.na(libraries)] <- "glyphicon"
    marker_colors[is.na(marker_colors)] <- "gray"
    
    leaflet::awesomeIcons(
      icon = icons,
      library = libraries,
      iconColor = "white",
      markerColor = marker_colors
    )
  }
  
  # Escape spreadsheet text before adding it to popup HTML.
  popup_text <- function(value) {
    if (length(value) == 0 || is.na(value)) {
      return("—")
    }
    
    htmltools::htmlEscape(as.character(value))
  }
  
  project_popups <- function(pts) {
    vapply(
      seq_len(nrow(pts)),
      function(i) {
        paste0(
          "<b>Project Type:</b> ",
          popup_text(pts$Project_Type[i]),
          "<br><br>",
          
          "<b>Project Name:</b> ",
          popup_text(pts$Project_Name[i]),
          
          "<br><b>Location:</b> ",
          popup_text(pts$Location[i]),
          
          "<br><b>Summary:</b> ",
          popup_text(pts$Summary[i]),
          
          "<br><b>Launched Date:</b> ",
          popup_text(pts$Launched.Date[i]),
          
          "<br><b>Status:</b> ",
          popup_text(pts$Status[i]),
          
          "<br><br>",
          make_photo_gallery(pts$Project_ID[i])
        )
      },
      character(1)
    )
  }
  
  # Used by the live maps and both HTML downloads.
  build_project_map <- function(district_polygons, projects) {
    shiny::req(nrow(district_polygons) > 0)
    
    center <- sf::st_centroid(
      sf::st_union(district_polygons)
    )
    coordinates <- sf::st_coordinates(center)
    
    map <- leaflet::leaflet(
      width = "100%",
      height = 600
    ) %>%
      leaflet::addProviderTiles(
        leaflet::providers$OpenStreetMap
      ) %>%
      leaflet::setView(
        lng = coordinates[1],
        lat = coordinates[2],
        zoom = 10
      ) %>%
      leaflet::addPolygons(
        data = district_polygons,
        fillColor = ~pal(NAME_2),
        fillOpacity = 0.7,
        color = "black",
        weight = 1,
        label = ~NAME_2
      )
    
    if (nrow(projects) > 0) {
      map <- map %>%
        leaflet::addAwesomeMarkers(
          data = projects,
          lng = ~longitude,
          lat = ~latitude,
          icon = project_marker_icons(projects),
          popup = project_popups(projects),
          popupOptions = leaflet::popupOptions(
            maxWidth = 340,
            maxHeight = 450
          )
        )
    }
    
    map
  }
  
  # Add the media behavior, heading, and watermark to an export.
  make_downloadable_map <- function(map, heading_text) {
    map <- htmlwidgets::onRender(
      map,
      vr_media_js
    )
    
    download_css <- paste0(
      "
      html, body {
        margin: 0;
        padding: 0;
      }

      body {
        font-family: Arial, sans-serif;
        background: #f5f5f5;
      }

      .vr-header {
        background: #ace500;
        padding: 15px 20px;
        text-align: center;
        font-size: 1.4rem;
        font-weight: bold;
        color: #1e1e4c;
        border-bottom: 2px solid #1e1e4c;
      }

      .vr-watermark {
        position: fixed;
        bottom: 10px;
        right: 10px;
        background: rgba(255,255,255,0.85);
        padding: 5px 12px;
        border-radius: 6px;
        font-size: 12px;
        font-family: monospace;
        color: #333;
        z-index: 10000;
        pointer-events: none;
        box-shadow: 0 1px 3px rgba(0,0,0,0.2);
      }
      ",
      vr_media_css
    )
    
    map <- htmlwidgets::prependContent(
      map,
      htmltools::tags$style(
        htmltools::HTML(download_css)
      ),
      htmltools::tags$div(
        class = "vr-header",
        heading_text
      )
    )
    
    htmlwidgets::appendContent(
      map,
      htmltools::tags$div(
        class = "vr-watermark",
        "(c) 2026 Volta Regional Coordinating Council. By D.A Analytics (senyoackuaku@gmail.com)"
      )
    )
  }
  
  # ============================================================
  # TAB 1: PROJECTS MAP
  # ============================================================
  
  filtered_data <- shiny::reactive({
    shiny::req(input$districts)
    
    volta_data %>%
      dplyr::filter(NAME_2 == input$districts)
  })
  
  filtered_projects <- shiny::reactive({
    shiny::req(input$districts)
    
    pts <- vrcc_projects_sf %>%
      dplyr::filter(District == input$districts)
    
    if (is.null(input$status_filter) ||
        length(input$status_filter) == 0 ||
        is.null(input$type_filter) ||
        length(input$type_filter) == 0) {
      return(pts[0, ])
    }
    
    pts %>%
      dplyr::filter(
        Status %in% input$status_filter,
        Project_Type %in% input$type_filter
      )
  })
  
  output$vr_map <- leaflet::renderLeaflet({
    df <- filtered_data()
    pts <- filtered_projects()
    
    shiny::req(nrow(df) > 0)
    
    build_project_map(
      district_polygons = df,
      projects = pts
    )
  })
  
  output$download_map <- shiny::downloadHandler(
    filename = function() {
      district_file <- paste(input$districts, collapse = "_")
      
      if (!nzchar(district_file)) {
        district_file <- "All_Districts"
      }
      
      district_file <- gsub(
        "[^A-Za-z0-9_-]+",
        "_",
        district_file
      )
      
      paste0(
        "vrccproj_",
        district_file,
        "_",
        format(Sys.time(), "%Y-%m-%d_%H-%M-%S"),
        ".html"
      )
    },
    
    content = function(file) {
      df <- filtered_data()
      pts <- filtered_projects()
      
      shiny::req(nrow(df) > 0)
      
      district_name <- paste(input$districts, collapse = ", ")
      
      status_text <- if (length(input$status_filter) > 0) {
        paste(input$status_filter, collapse = ", ")
      } else {
        "None selected"
      }
      
      type_text <- if (length(input$type_filter) > 0) {
        paste(input$type_filter, collapse = ", ")
      } else {
        "None selected"
      }
      
      heading_text <- paste0(
        "Projects in Volta Region - ", district_name,
        " | Status: ", status_text,
        " | Type: ", type_text
      )
      
      map <- build_project_map(
        district_polygons = df,
        projects = pts
      )
      
      export_map <- make_downloadable_map(
        map,
        heading_text
      )
      
      export_map <- make_download_map_mobile(
        export_map
      )
      
      htmlwidgets::saveWidget(
        widget = export_map,
        file = file,
        selfcontained = TRUE,
        title = paste("VRCC Projects -", district_name)
      )
    },
    
    contentType = "text/html"
  )
  
  # ============================================================
  # TAB 2: HO MUNICIPAL
  # ============================================================
  
  fltd_data <- shiny::reactive({
    volta_data %>%
      dplyr::filter(NAME_2 == "Ho Municipal")
  })
  
  fltd_projects <- shiny::reactive({
    pts2 <- vrcc_projects_sf %>%
      dplyr::filter(District == "Ho Municipal")
    
    if (is.null(input$hm_status_filter) ||
        length(input$hm_status_filter) == 0 ||
        is.null(input$hm_type_filter) ||
        length(input$hm_type_filter) == 0) {
      return(pts2[0, ])
    }
    
    pts2 %>%
      dplyr::filter(
        Status %in% input$hm_status_filter,
        Project_Type %in% input$hm_type_filter
      )
  })
  
  output$hm_map <- leaflet::renderLeaflet({
    df <- fltd_data()
    pts2 <- fltd_projects()
    
    shiny::req(nrow(df) > 0)
    
    build_project_map(
      district_polygons = df,
      projects = pts2
    )
  })
  
  output$download_hm_map <- shiny::downloadHandler(
    filename = function() {
      paste0(
        "vrccproj_Ho_Municipal_",
        format(Sys.time(), "%Y-%m-%d_%H-%M-%S"),
        ".html"
      )
    },
    
    content = function(file) {
      df <- fltd_data()
      pts2 <- fltd_projects()
      
      shiny::req(nrow(df) > 0)
      
      status_text <- if (length(input$hm_status_filter) > 0) {
        paste(input$hm_status_filter, collapse = ", ")
      } else {
        "None selected"
      }
      
      type_text <- if (length(input$hm_type_filter) > 0) {
        paste(input$hm_type_filter, collapse = ", ")
      } else {
        "None selected"
      }
      
      heading_text <- paste0(
        "Projects in Ho Municipal",
        " | Status: ", status_text,
        " | Type: ", type_text
      )
      
      map <- build_project_map(
        district_polygons = df,
        projects = pts2
      )
      
      export_map <- make_downloadable_map(
        map,
        heading_text
      )
      
      export_map <- make_download_map_mobile(
        export_map
      )
      
      htmlwidgets::saveWidget(
        widget = export_map,
        file = file,
        selfcontained = TRUE,
        title = "VRCC Projects - Ho Municipal"
      )
    },
    
    contentType = "text/html"
  )
  
  
  # ==========================================
  # VRCC - PROJECT STATISTICS
  # ==========================================
  
  # 1. Prepare data
  stats_data <- reactive({
    
    df <- vrcc_projects_sf |>
      sf::st_drop_geometry() |>
      dplyr::mutate(
        District = as.character(District),
        Project_Type = as.character(Project_Type),
        Status = trimws(as.character(Status)),
        Project_ID = as.character(Project_ID)
      ) |>
      dplyr::filter(
        !is.na(Project_ID),
        nzchar(trimws(Project_ID))
      ) |>
      dplyr::distinct(Project_ID, .keep_all = TRUE)
    
    if (!is.null(input$stats_district) &&
        input$stats_district != "All") {
      df <- df |>
        dplyr::filter(District == input$stats_district)
    }
    
    if (!is.null(input$stats_type) &&
        input$stats_type != "All") {
      df <- df |>
        dplyr::filter(Project_Type == input$stats_type)
    }
    
    df
  })
  
  
  # 2. Summary cards
  
  output$stats_total_projects <-
    shinydashboard::renderValueBox({
      
      df <- stats_data()
      
      shinydashboard::valueBox(
        value = nrow(df),
        subtitle = "Total Projects",
        icon = icon("folder-open"),
        color = "blue"
      )
    })
  
  
  output$stats_district_count <-
    shinydashboard::renderValueBox({
      
      df <- stats_data()
      
      shinydashboard::valueBox(
        value = dplyr::n_distinct(
          df$District,
          na.rm = TRUE
        ),
        subtitle = "Districts Covered",
        icon = icon("map-marker"),
        color = "aqua"
      )
    })
  
  
  output$stats_completed <-
    shinydashboard::renderValueBox({
      
      df <- stats_data()
      
      completed <- sum(
        df$Status == "Completed",
        na.rm = TRUE
      )
      
      shinydashboard::valueBox(
        value = completed,
        subtitle = "Completed Projects",
        icon = icon("check-circle"),
        color = "green"
      )
    })
  
  
  output$stats_completion_rate <-
    shinydashboard::renderValueBox({
      
      df <- stats_data()
      
      completed <- sum(
        df$Status == "Completed",
        na.rm = TRUE
      )
      
      rate <- if (nrow(df) > 0) {
        round(100 * completed / nrow(df), 1)
      } else {
        0
      }
      
      shinydashboard::valueBox(
        value = paste0(rate, "%"),
        subtitle = "Completion Rate",
        icon = icon("percent"),
        color = "yellow"
      )
    })
  
  
  # 3. Projects by district
  
  district_summary <- reactive({
    
    stats_data() |>
      dplyr::count(
        District,
        name = "Projects",
        sort = TRUE
      )
  })
  
  
  output$stats_district_plot <-
    plotly::renderPlotly({
      
      df <- district_summary()
      
      plotly::plot_ly(
        data = df,
        x = ~Projects,
        y = ~reorder(District, Projects),
        type = "bar",
        orientation = "h",
        text = ~Projects,
        textposition = "auto"
      ) |>
        plotly::layout(
          xaxis = list(title = "Number of Projects"),
          yaxis = list(title = ""),
          margin = list(l = 130)
        )
    })
  
  
  # 4. Projects by status
  
  status_summary <- reactive({
    
    stats_data() |>
      dplyr::count(
        Status,
        name = "Projects"
      )
  })
  
  
  output$stats_status_plot <-
    plotly::renderPlotly({
      
      df <- status_summary()
      
      plotly::plot_ly(
        data = df,
        labels = ~Status,
        values = ~Projects,
        type = "pie",
        textinfo = "label+percent",
        hoverinfo = "label+value+percent"
      )
    })
  
  
  # 5. Projects by type
  
  type_summary <- reactive({
    
    stats_data() |>
      dplyr::count(
        Project_Type,
        name = "Projects",
        sort = TRUE
      )
  })
  
  
  output$stats_type_plot <-
    plotly::renderPlotly({
      
      df <- type_summary()
      
      plotly::plot_ly(
        data = df,
        x = ~Projects,
        y = ~reorder(Project_Type, Projects),
        type = "bar",
        orientation = "h",
        text = ~Projects,
        textposition = "auto"
      ) |>
        plotly::layout(
          xaxis = list(title = "Number of Projects"),
          yaxis = list(title = ""),
          margin = list(l = 130)
        )
    })
  
  
  # 6. District by status
  
  district_status_summary <- reactive({
    
    stats_data() |>
      dplyr::count(
        District,
        Status,
        name = "Projects"
      )
  })
  
  
  output$stats_district_status_plot <-
    plotly::renderPlotly({
      
      df <- district_status_summary()
      
      plotly::plot_ly(
        data = df,
        x = ~District,
        y = ~Projects,
        color = ~Status,
        type = "bar"
      ) |>
        plotly::layout(
          barmode = "stack",
          xaxis = list(
            title = "",
            tickangle = -45
          ),
          yaxis = list(
            title = "Number of Projects"
          )
        )
    })
  
  
  # ==========================================
  # DISTRICT SUMMARY TABLE
  # ==========================================
  
  district_table_data <- reactive({
    
    stats_data() |>
      dplyr::group_by(District) |>
      dplyr::summarise(
        
        Total = dplyr::n(),
        
        Completed = sum(
          Status == "Completed",
          na.rm = TRUE
        ),
        
        "On-going" = sum(
          Status == "On-going",
          na.rm = TRUE
        ),
        
        Planned = sum(
          Status == "Planned",
          na.rm = TRUE
        ),
        
        "Completion Rate (%)" = round(
          100 * Completed / Total,
          1
        ),
        
        .groups = "drop"
        
      ) |>
      dplyr::arrange(
        dplyr::desc(Total)
      )
    
  })
  
  
  output$stats_district_table <- DT::renderDT({
    
    DT::datatable(
      district_table_data(),
      rownames = FALSE,
      options = list(
        pageLength = 10,
        scrollX = TRUE
      ),
      filter = "top"
    )
  })
  
  
  # 8. District by project type table
  
  type_table_data <- reactive({
    
    stats_data() |>
      dplyr::count(
        District,
        Project_Type,
        name = "Projects"
      ) |>
      tidyr::pivot_wider(
        names_from = Project_Type,
        values_from = Projects,
        values_fill = 0
      )
  })
  
  
  output$stats_type_table <- DT::renderDT({
    
    DT::datatable(
      type_table_data(),
      rownames = FALSE,
      options = list(
        pageLength = 10,
        scrollX = TRUE
      )
    )
  })
  
  
  # 9. Detailed project records
  
  output$stats_project_table <- DT::renderDT({
    
    df <- stats_data()
    
    keep <- intersect(
      c(
        "Project_ID",
        "District",
        "Project_Name",
        "Project_Type",
        "Status",
        "Location",
        "Launched.Date"
      ),
      names(df)
    )
    
    DT::datatable(
      df[, keep, drop = FALSE],
      rownames = FALSE,
      filter = "top",
      options = list(
        pageLength = 10,
        scrollX = TRUE
      )
    )
  })
  
  
  # 10. Excel download
  
  output$download_project_statistics <-
    shiny::downloadHandler(
      
      filename = function() {
        paste0(
          "VRCC_Project_Statistics_",
          Sys.Date(),
          ".xlsx"
        )
      },
      
      content = function(file) {
        
        df <- stats_data()
        
        wb <- openxlsx::createWorkbook()
        
        openxlsx::addWorksheet(
          wb, "District Summary"
        )
        openxlsx::writeData(
          wb, "District Summary",
          district_table_data()
        )
        
        openxlsx::addWorksheet(
          wb, "Project Types"
        )
        openxlsx::writeData(
          wb, "Project Types",
          type_table_data()
        )
        
        openxlsx::addWorksheet(
          wb, "Project Status"
        )
        openxlsx::writeData(
          wb, "Project Status",
          status_summary()
        )
        
        openxlsx::addWorksheet(
          wb, "Detailed Projects"
        )
        
        keep <- intersect(
          c(
            "Project_ID",
            "District",
            "Project_Name",
            "Project_Type",
            "Status",
            "Location",
            "Launched.Date"
          ),
          names(df)
        )
        
        openxlsx::writeData(
          wb, "Detailed Projects",
          df[, keep, drop = FALSE]
        )
        
        openxlsx::saveWorkbook(
          wb,
          file,
          overwrite = TRUE
        )
      }
    )
  
  # ==========================================
  # VRCC - DOWNLOAD STATISTICS GRAPHS AS HTML
  # ==========================================
  
  output$download_statistics_html <-
    shiny::downloadHandler(
      
      filename = function() {
        paste0(
          "VRCC_Project_Graphs_",
          format(Sys.Date(), "%Y-%m-%d"),
          ".html"
        )
      },
      
      content = function(file) {
        
        # Current filtered statistics data
        df <- stats_data()
        
        shiny::validate(
          shiny::need(
            nrow(df) > 0,
            "No project data available for download."
          )
        )
        
        # ----------------------------------
        # 1. PROJECTS BY DISTRICT
        # ----------------------------------
        
        district_df <- df |>
          dplyr::count(
            District,
            name = "Projects",
            sort = TRUE
          )
        
        p1 <- plotly::plot_ly(
          data = district_df,
          x = ~Projects,
          y = ~reorder(District, Projects),
          type = "bar",
          orientation = "h",
          text = ~Projects,
          textposition = "auto"
        ) |>
          plotly::layout(
            title = "Projects by District / Municipality",
            xaxis = list(
              title = "Number of Projects"
            ),
            yaxis = list(title = ""),
            margin = list(l = 140)
          )
        
        
        # ----------------------------------
        # 2. PROJECTS BY STATUS
        # ----------------------------------
        
        status_df <- df |>
          dplyr::count(
            Status,
            name = "Projects"
          )
        
        p2 <- plotly::plot_ly(
          data = status_df,
          labels = ~Status,
          values = ~Projects,
          type = "pie",
          textinfo = "label+percent",
          hoverinfo = "label+value+percent"
        ) |>
          plotly::layout(
            title = "Projects by Status"
          )
        
        
        # ----------------------------------
        # 3. PROJECTS BY TYPE
        # ----------------------------------
        
        type_df <- df |>
          dplyr::count(
            Project_Type,
            name = "Projects",
            sort = TRUE
          )
        
        p3 <- plotly::plot_ly(
          data = type_df,
          x = ~Projects,
          y = ~reorder(Project_Type, Projects),
          type = "bar",
          orientation = "h",
          text = ~Projects,
          textposition = "auto"
        ) |>
          plotly::layout(
            title = "Projects by Type",
            xaxis = list(
              title = "Number of Projects"
            ),
            yaxis = list(title = ""),
            margin = list(l = 140)
          )
        
        
        # ----------------------------------
        # 4. PROJECT STATUS BY DISTRICT
        # ----------------------------------
        
        district_status_df <- df |>
          dplyr::count(
            District,
            Status,
            name = "Projects"
          )
        
        p4 <- plotly::plot_ly(
          data = district_status_df,
          x = ~District,
          y = ~Projects,
          color = ~Status,
          type = "bar"
        ) |>
          plotly::layout(
            title = "Project Status by District",
            barmode = "stack",
            xaxis = list(
              title = "",
              tickangle = -45
            ),
            yaxis = list(
              title = "Number of Projects"
            )
          )
        
        
        # ----------------------------------
        # 5. SUMMARY STATISTICS
        # ----------------------------------
        
        total_projects <- nrow(df)
        
        total_districts <- dplyr::n_distinct(
          df$District,
          na.rm = TRUE
        )
        
        completed <- sum(
          df$Status == "Completed",
          na.rm = TRUE
        )
        
        completion_rate <- round(
          100 * completed / total_projects,
          1
        )
        
        
        # ----------------------------------
        # 6. CREATE HTML DASHBOARD
        # ----------------------------------
        
        district_label <- if (
          is.null(input$stats_district)
        ) "All" else input$stats_district
        
        type_label <- if (
          is.null(input$stats_type)
        ) "All" else input$stats_type
        
        header <- htmltools::tags$div(
          style = paste(
            "background:#0B6E75;",
            "color:white;",
            "padding:20px;",
            "text-align:center;"
          ),
          
          htmltools::tags$h2(
            "VRCC - Project Statistics"
          ),
          
          htmltools::tags$p(
            "Volta Regional Coordinating Council"
          )
        )
        
        summary_section <- htmltools::tags$div(
          style = paste(
            "padding:20px;",
            "background:#F3F7F6;"
          ),
          
          htmltools::tags$h3(
            "Project Summary"
          ),
          
          htmltools::tags$p(
            paste(
              "District:",
              district_label,
              "| Project Type:",
              type_label
            )
          ),
          
          htmltools::tags$p(
            paste(
              "Total Projects:",
              total_projects
            )
          ),
          
          htmltools::tags$p(
            paste(
              "Districts Covered:",
              total_districts
            )
          ),
          
          htmltools::tags$p(
            paste(
              "Completed Projects:",
              completed
            )
          ),
          
          htmltools::tags$p(
            paste0(
              "Completion Rate: ",
              completion_rate,
              "%"
            )
          )
        )
        
        
        # ----------------------------------
        # 7. COMBINE INTERACTIVE GRAPHS
        # ----------------------------------
        
        dashboard <- htmltools::tagList(
          
          htmltools::tags$head(
            htmltools::tags$meta(
              name = "viewport",
              content = "width=device-width, initial-scale=1"
            ),
            
            htmltools::tags$style(
              htmltools::HTML("
              body {
                margin: 0;
                font-family: Arial, sans-serif;
                background: #F3F7F6;
              }

              .graph-card {
                background: white;
                margin: 15px;
                padding: 15px;
                border-radius: 8px;
              }

              .graph-card .html-widget {
                max-width: 100%;
              }
            ")
            )
          ),
          
          header,
          summary_section,
          
          htmltools::tags$div(
            class = "graph-card",
            p1
          ),
          
          htmltools::tags$div(
            class = "graph-card",
            p2
          ),
          
          htmltools::tags$div(
            class = "graph-card",
            p3
          ),
          
          htmltools::tags$div(
            class = "graph-card",
            p4
          ),
          
          htmltools::tags$div(
            style = paste(
              "text-align:center;",
              "padding:20px;",
              "font-size:12px;",
              "color:#555;"
            ),
            
            htmltools::HTML(
              "&copy; 2026 Volta Regional Coordinating Council (VRCC)"
            )
          )
        )
        
        
        
        # ----------------------------------
        # 8. SAVE SELF-CONTAINED HTML
        # ----------------------------------
        
        temp_html <- tempfile(fileext = ".html")
        
        htmltools::save_html(
          htmltools::browsable(dashboard),
          file = temp_html
        )
        
        rmarkdown::pandoc_self_contained_html(
          input = temp_html,
          output = file
        )
        
      } # END content = function(file)
      
    ) # END downloadHandler
  
} # END main server function

