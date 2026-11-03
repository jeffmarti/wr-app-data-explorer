# ================================
# APP METADATA
# ================================
# App:      Water Right Application Waiting Times
# Author:   Jeff Marti
# Deployed: https://waterwater-wr-app-data.share.connect.posit.cloud/
# Updated:  2026-11-03
#
#           (= 01a12284-53a7-e128-5384-1df2201047c9.share.connect.posit.cloud,
#            where the old waterwater.shinyapps.io/wr-app-data redirects)
#
# DATA is refreshed weekly by pipeline/run_pipeline.R on GitHub Actions
# (.github/workflows/refresh-data.yml), which commits data/wr_data.rds and
# then redeploys to that same Connect Cloud content ID with
# rsconnect::deployApp(). Users no longer upload anything. See README.md.
#
# Deployed files: app.R, data/wr_data.rds, wria_boundaries.geojson,
#   county_boundaries.geojson, www/wa_water_rights.svg
# # ================================
#

# ================================
# 1. Libraries
# ================================
library(shiny)
library(dplyr)
library(lubridate)
library(ggplot2)
library(rlang)
library(leaflet)
library(sf)
library(DT)
library(tidyr)

# ================================
# 2. Companion app
# ================================
COMPANION_URL <- "https://waterwater-wr-applications.share.connect.posit.cloud/"

# ================================
# 3. Load spatial layers at startup
# ================================
wria_sf <- sf::st_read("wria_boundaries.geojson", quiet = TRUE) %>%
  mutate(WRIA_NR = as.integer(WRIA_NR))

county_sf <- sf::st_read("county_boundaries.geojson", quiet = TRUE) %>%
  mutate(COUNTY_NM = trimws(toupper(gsub(" County$", "", JURISDICT_NM))))

# ================================
# 4. Load the weekly-refreshed dataset at startup
# ================================
# Built from the WRTS export by pipeline/prepare_app_data.R:
# list(data = records, report_date = "Report Generated on" date)
default_result    <- readRDS("data/wr_data.rds")
default_wr_data   <- default_result$data
default_report_date <- default_result$report_date
# ================================
# Load permit timeliness data at startup
# ================================
WA_SOCRATA_TOKEN <- Sys.getenv("WA_SOCRATA_TOKEN")
# NOTE: CARTO raster basemaps (basemaps.cartocdn.com) now require an API key
# and are being retired by CARTO. This app now uses a keyless basemap instead.
# CARTO_API_KEY <- Sys.getenv("CARTO_API_KEY")
timeliness_data <- tryCatch({
  url <- paste0(
    "https://data.wa.gov/resource/yccr-zbpr.csv",
    "?$$app_token=", WA_SOCRATA_TOKEN,
    "&$limit=10000"
  )
  
  raw <- read.csv(url(url), stringsAsFactors = FALSE)
  
  raw %>%
    filter(
      agency    == "Ecology (ECY)",
      permit_name %in% c("Water Right, New", "Water Right Change")
    ) %>%
    mutate(
      # clean up permit name for display
      permit_label = recode(permit_name,
                            "Water Right, New"    = "New Application",
                            "Water Right Change"  = "Change Application"
      ),
      # convert days to years + remainder days
      rtc_avg_yrs  = floor(receipt_to_complete_average  / 365.25),
      rtc_avg_days = round(receipt_to_complete_average  %% 365.25),
      ctd_avg_yrs  = floor(complete_to_decision_average / 365.25),
      ctd_avg_days = round(complete_to_decision_average %% 365.25),
      # readable labels for hover/annotation
      rtc_label = paste0(rtc_avg_yrs, " yrs, ", rtc_avg_days, " days"),
      ctd_label = paste0(ctd_avg_yrs, " yrs, ", ctd_avg_days, " days")
    )
}, error = function(e) {
  message("Could not fetch timeliness data: ", e$message)
  NULL
})
# ================================
# 5. UI
# ================================
ui <- fluidPage(

  # ── Polished banner ───────────────────────────────────────────────────────
  tags$div(
    style = paste0(
      "background: linear-gradient(135deg, #1a3a5c 0%, #2e6da4 100%);",
      "padding: 12px 20px;",
      "margin-bottom: 16px;",
      "border-radius: 6px;",
      "display: flex;",
      "align-items: center;",
      "gap: 16px;"
    ),
    
    # ── Icon in white circle badge ─────────────────────────────────────────
    tags$div(
      style = paste0(
        "background-color: white;",
        "border-radius: 50%;",
        "padding: 8px;",
        "width: 65px;",
        "height: 65px;",
        "display: flex;",
        "align-items: center;",
        "justify-content: center;",
        "flex-shrink: 0;",
        "box-shadow: 0 2px 8px rgba(0,0,0,0.4);"
      ),
      tags$img(
        src   = "wa_water_rights.svg",
        style = "width: 52px; height: 52px; object-fit: contain; display: block;"
      )
    ),
    
    # ── Title text block ───────────────────────────────────────────────────
    tags$div(
      tags$h2(
        "Water Right Application Waiting Times — Washington State",
        style = paste0(
          "color: white; margin: 0;",
          "font-size: 22px; font-weight: bold; line-height: 1.2;"
        )
      ),
      tags$p(
        "WA Dept. of Ecology  |  New & Change Applications  |  Active Status",
        style = paste0(
          "color: rgba(255,255,255,0.78);",
          "margin: 4px 0 0 0; font-size: 13px;"
        )
      )
    )
  ),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      
      # ------------------
      # Data source indicator
      # ------------------
      div(
        style = "background-color: #f0f4f8; border-radius: 6px; padding: 10px; margin-bottom: 12px;",
        uiOutput("data_source_label"),
        div(
          style = "color: #777; font-size: 11px;",
          "Source: Ecology ",
          tags$a(href   = "https://appswr.ecology.wa.gov/waterrighttrackingsystem/WaterRights/WaterRightSearch.aspx",
                 target = "_blank",
                 "Water Right Record Search"),
          " (New & Change Applications, Status = Active). Refreshed weekly."
        )
      ),

      # ------------------
      # Companion app
      # ------------------
      div(
        style = "background-color: #eaf3fb; border-left: 4px solid #2a7aab;
                 border-radius: 4px; padding: 10px; margin-bottom: 12px;
                 font-size: 12px;",
        strong("Companion app: "),
        tags$a(href   = COMPANION_URL,
               target = "_blank",
               "WA Water Right Applications — Active New & Change Applications"),
        br(),
        "Maps each of these pending applications at its approximate location, ",
        "with a time slider, filters, and KMZ export for Google Earth."
      ),
      
      checkboxGroupInput("phase_filter", "Phase:", choices = NULL),
      
      hr(),
      
      conditionalPanel(
        condition = "input.main_tabs == 'Map'",
        selectInput("map_layer", "Map Layer:", c("WRIA", "County")),
        selectInput("map_metric", "Color by:",
                    c("Total New Applications"        = "n_new",
                      "Total Change Applications"     = "n_change",
                      "Median Years Pending (New)"    = "med_yrs_new",
                      "Median Years Pending (Change)" = "med_yrs_change"))
      ),
      conditionalPanel(
        condition = "input.main_tabs == 'Charts'",
        sliderInput("bin_years", "Bin Width (years)", 2, 20, 5),
        selectInput("plot_type", "Chart Type:",
                    c("Histogram", "Population Pyramid")),
        selectInput("group_type", "Group by:",
                    c("WRIA", "County", "Statewide")),
        uiOutput("group_selector")
      ),
      
      hr(),
      div(
        style = paste0(
          "font-size: 11px;",
          "color: #888;",
          "padding: 4px 2px;"
        ),
        "Questions or feedback:",
        br(),
        tags$a(
          href  = "mailto:jeffjmarti@gmail.com",
          style = "color: #2e6da4;",
          "jeffjmarti@gmail.com"
        )
      )
    ),
    
    mainPanel(
      width = 9,
      tabsetPanel(
        id = "main_tabs",
        tabPanel("Map",       br(), leafletOutput("wr_map",    height = "700px")),
        tabPanel("Charts",    br(), plotOutput("hist_plot",    height = "800px")),
        tabPanel("Data Table",br(), DTOutput("data_table")),
        tabPanel("Processing Times",
                 br(),
                 uiOutput("timeliness_header"),
                 br(),
                 plotOutput("timeliness_plot", height = "700px")
        )
      )
    )
  )
)

# ================================
# 6. SERVER
# ================================
server <- function(input, output, session) {
  
  # ------------------
  # Active dataset (refreshed weekly by the pipeline)
  # ------------------
  wr_data            <- reactive(default_wr_data)
  active_report_date <- reactive(default_report_date)

  # ------------------
  # Data source label shown in sidebar
  # ------------------
  output$data_source_label <- renderUI({
    div(style = "color: #555; font-size: 12px; margin-bottom: 6px;",
        paste0("📋 Data current as of ",
               format(active_report_date(), "%B %d, %Y")))
  })
  
  # ------------------
  # Helper: safely compute median years pending
  # ------------------
  safe_median_yrs <- function(dates) {
    dates <- dates[!is.na(dates)]
    if (length(dates) == 0) return(NA_real_)
    median(time_length(interval(dates, Sys.Date()), "years"), na.rm = TRUE)
  }
  
  # ------------------
  # Phase selector
  # ------------------
  observe({
    req(wr_data())
    updateCheckboxGroupInput(
      session, "phase_filter",
      choices  = unique(wr_data()$Phase),
      selected = unique(wr_data()$Phase)
    )
  })
  
  # ------------------
  # Group selector (Charts tab)
  # ------------------
  output$group_selector <- renderUI({
    req(wr_data())
    if (input$group_type == "WRIA") {
      selectInput("groups", "WRIA:",
                  choices = wr_data() %>%
                    distinct(WRIA, WRIA_LABEL) %>%
                    arrange(WRIA) %>%
                    pull(WRIA_LABEL),
                  multiple = TRUE)
    } else if (input$group_type == "County") {
      selectInput("groups", "County:",
                  choices  = sort(unique(wr_data()$County)),
                  multiple = TRUE)
    }
  })
  
  # ------------------
  # Filter data (Charts + Table)
  # ------------------
  filtered_data <- reactive({
    req(wr_data(), input$phase_filter)
    
    df <- wr_data() %>%
      filter(Phase %in% input$phase_filter)
    
    if (input$group_type == "WRIA") {
      req(input$groups)
      df <- df %>% filter(WRIA_LABEL %in% input$groups,
                          WRIA_LABEL != "Unassigned")
    } else if (input$group_type == "County") {
      req(input$groups)
      df <- df %>% filter(County %in% input$groups)
    }
    
    df
  })
  
  # ------------------
  # Map summary stats
  # ------------------
  map_stats <- reactive({
    req(wr_data(), input$phase_filter)
    
    df <- wr_data() %>%
      filter(Phase %in% input$phase_filter,
             !is.na(WRIA))
    
    if (input$map_layer == "WRIA") {
      stats <- df %>%
        group_by(WRIA) %>%
        summarise(
          n_new          = sum(Phase == "New Application"),
          n_change       = sum(Phase == "Change Application"),
          med_yrs_new    = safe_median_yrs(PriorityDate[Phase == "New Application"]),
          med_yrs_change = safe_median_yrs(PriorityDate[Phase == "Change Application"]),
          .groups = "drop"
        )
      
      wria_sf %>%
        left_join(stats, by = c("WRIA_NR" = "WRIA")) %>%
        mutate(
          n_new    = ifelse(is.na(n_new),    0L, n_new),
          n_change = ifelse(is.na(n_change), 0L, n_change)
        )
      
    } else {
      stats <- df %>%
        group_by(County_upper) %>%
        summarise(
          n_new          = sum(Phase == "New Application"),
          n_change       = sum(Phase == "Change Application"),
          med_yrs_new    = safe_median_yrs(PriorityDate[Phase == "New Application"]),
          med_yrs_change = safe_median_yrs(PriorityDate[Phase == "Change Application"]),
          .groups = "drop"
        )
      
      county_sf %>%
        left_join(stats, by = c("COUNTY_NM" = "County_upper")) %>%
        mutate(
          n_new    = ifelse(is.na(n_new),    0L, n_new),
          n_change = ifelse(is.na(n_change), 0L, n_change)
        )
    }
  })
  
  # ------------------
  # Render map
  # ------------------
  output$wr_map <- renderLeaflet({
    req(map_stats())
    
    layer_data   <- map_stats()
    metric       <- input$map_metric
    
    metric_label <- switch(metric,
                           n_new          = "Total New Applications",
                           n_change       = "Total Change Applications",
                           med_yrs_new    = "Median Years Pending (New)",
                           med_yrs_change = "Median Years Pending (Change)"
    )
    
    vals <- layer_data[[metric]]
    
    pal <- colorNumeric(
      palette  = "YlOrRd",
      domain   = vals,
      na.color = "grey80"
    )
    
    if (input$map_layer == "WRIA") {
      name_text <- paste0(layer_data$WRIA_NR, " - ", layer_data$WRIA_NM)
    } else {
      name_text <- layer_data$COUNTY_NM
    }
    
    popup_text <- paste0(
      "<b>", name_text, "</b><br>",
      "New Applications: ",            layer_data$n_new,    "<br>",
      "Change Applications: ",         layer_data$n_change, "<br>",
      "Median Yrs Pending (New): ",
      ifelse(is.na(layer_data$med_yrs_new),    "N/A", round(layer_data$med_yrs_new,    1)), "<br>",
      "Median Yrs Pending (Change): ",
      ifelse(is.na(layer_data$med_yrs_change), "N/A", round(layer_data$med_yrs_change, 1))
    )
    
    leaflet(layer_data,
            options = leafletOptions(
              minZoom = 6,    # can't zoom out past state level
              maxZoom = 10    # can't zoom into street/backyard level
            )
    ) %>%
      # ------------------------------------------------------------------
    # Basemap: Esri Light Gray Canvas -- no API key required.
    # Visually similar to CartoDB.Positron: neutral gray background with
    # muted labels, so the choropleth polygons stay in the foreground.
    #
    # Keyless fallbacks if Esri ever retires this tile service --
    # just swap the provider name on the line below:
    #   providers$OpenStreetMap.Mapnik   (busier / more color)
    #   providers$USGS.USTopo            (USGS topo, US coverage only)
    #   providers$Esri.WorldTopoMap      (Esri topo, more color)
    # ------------------------------------------------------------------
    addProviderTiles(
      providers$Esri.WorldGrayCanvas,
      options = providerTileOptions(
        noWrap = TRUE
      )
    ) %>%
      setMaxBounds(
        lng1 = -125.0,   # western edge (coast)
        lat1 =   45.4,   # southern edge (Columbia River)
        lng2 = -116.5,   # eastern edge (Idaho border)
        lat2 =   49.1    # northern edge (Canadian border)
      ) %>%
      addPolygons(
        fillColor        = pal(vals),
        fillOpacity      = 0.7,
        color            = "white",
        weight           = 1,
        popup            = popup_text,
        label            = name_text,
        highlightOptions = highlightOptions(
          weight       = 2,
          color        = "#444",
          fillOpacity  = 0.9,
          bringToFront = TRUE
        )
      ) %>%
      addLegend(
        pal      = pal,
        values   = vals,
        title    = metric_label,
        position = "bottomright",
        na.label = "No data"
      )
  })
  
  # ------------------
  # Plot builder
  # ------------------
  build_plot <- function(df, facet_var = NULL, binwidth, report_date = Sys.Date()) {
    
    app_theme <- theme_minimal(base_size = 16) +
      theme(
        panel.background   = element_rect(fill = "white", color = NA),
        plot.background    = element_rect(fill = "white", color = NA),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(color = "grey85"),
        panel.grid.minor   = element_blank(),
        strip.text         = element_text(size = 16, face = "bold"),
        axis.title         = element_text(size = 16, face = "bold"),
        axis.text          = element_text(size = 14),
        legend.position    = "top",
        legend.text        = element_text(size = 14),
        legend.title       = element_text(size = 14, face = "bold"),
        axis.ticks         = element_line(color = "grey40"),
        axis.ticks.length  = unit(0.2, "cm")
      )
    
    phase_colors <- c(
      "New Application"    = "#4C78A8",
      "Change Application" = "#F58518"
    )
    
    if (input$plot_type == "Histogram") {
      
      y_pos <- nrow(df) * 0.15
      
      if (!is.null(facet_var)) {
        medians <- df %>%
          group_by(Phase, !!sym(facet_var)) %>%
          summarise(
            median_date = median(PriorityDate, na.rm = TRUE),
            .groups = "drop"
          )
      } else {
        medians <- df %>%
          group_by(Phase) %>%
          summarise(
            median_date = median(PriorityDate, na.rm = TRUE),
            .groups = "drop"
          )
      }
      
      calc_age <- function(x) {
        yrs <- as.integer((Sys.Date() - as.Date(x)) / 365.25)
        ifelse(is.na(yrs), "N/A", paste0(yrs, " yrs"))
      }
      
      if (!is.null(facet_var)) {
        summary_text <- df %>%
          group_by(.data[[facet_var]]) %>%
          summarise(
            new      = median(PriorityDate[Phase == "New Application"],    na.rm = TRUE),
            change   = median(PriorityDate[Phase == "Change Application"], na.rm = TRUE),
            n_new    = sum(Phase == "New Application"),
            n_change = sum(Phase == "Change Application"),
            .groups  = "drop"
          ) %>%
          mutate(label_text =
                   paste0("New: ",    format(new,    "%Y"), " (", calc_age(new),    ")\n",
                          "Change: ", format(change, "%Y"), " (", calc_age(change), ")\n",
                          "Total New Applications: ",    format(n_new,    big.mark = ","), "\n",
                          "Total Change Applications: ", format(n_change, big.mark = ","), "\n",
                          "Data as of: ", format(report_date, "%B %d, %Y")))
      } else {
        new      <- median(df$PriorityDate[df$Phase == "New Application"],    na.rm = TRUE)
        change   <- median(df$PriorityDate[df$Phase == "Change Application"], na.rm = TRUE)
        n_new    <- format(sum(df$Phase == "New Application"),    big.mark = ",")
        n_change <- format(sum(df$Phase == "Change Application"), big.mark = ",")
        summary_text <- tibble::tibble(
          label_text = paste0(
            "New: ",    format(new,    "%Y"), " (", calc_age(new),    ")\n",
            "Change: ", format(change, "%Y"), " (", calc_age(change), ")\n",
            "Total New Applications: ",    n_new,    "\n",
            "Total Change Applications: ", n_change, "\n",
            "Data as of: ", format(report_date, "%B %d, %Y")
          )
        )
      }
      
      p <- ggplot(df, aes(PriorityDate, fill = Phase)) +
        geom_histogram(binwidth = binwidth, alpha = 0.6, color = "black") +
        geom_vline(data = medians,
                   aes(xintercept = median_date, color = Phase)) +
        geom_text(data = medians,
                  aes(x = median_date, y = y_pos,
                      label = format(median_date, "%Y"), color = Phase),
                  angle = 90, vjust = 0.5, inherit.aes = FALSE) +
        geom_text(data = summary_text,
                  aes(x = Inf, y = Inf, label = label_text),
                  inherit.aes = FALSE,
                  hjust = 1.05, vjust = 1.1,
                  size = 4, color = "grey30") +
        scale_x_date() +
        labs(x = "Priority Date", y = "Count", fill = "Phase", color = "Phase")
      
    } else {
      
      df <- df %>%
        mutate(age = floor(time_length(interval(PriorityDate, Sys.Date()), "years")))
      
      max_count <- df %>%
        count(age, Phase) %>%
        summarise(max_n = max(n, na.rm = TRUE)) %>%
        pull(max_n)
      
      p <- ggplot() +
        geom_histogram(
          data = df %>% filter(Phase == "New Application"),
          aes(y = age, x = after_stat(count), fill = Phase),
          binwidth = input$bin_years, alpha = 0.6, color = "black"
        ) +
        geom_histogram(
          data = df %>% filter(Phase == "Change Application"),
          aes(y = age, x = -after_stat(count), fill = Phase),
          binwidth = input$bin_years, alpha = 0.6, color = "black"
        ) +
        geom_vline(xintercept = 0, color = "black", linewidth = 0.6) +
        scale_x_continuous(
          labels = abs,
          limits = c(-max_count, max_count),
          breaks = seq(-max_count, max_count, by = ifelse(max_count > 100, 50, 10))
        ) +
        scale_y_continuous(
          breaks = seq(0, max(df$age, na.rm = TRUE), by = 5)
        ) +
        labs(x = "Number of Applications", y = "Years Pending", fill = "Phase")
    }
    
    if (!is.null(facet_var)) {
      p <- p + facet_wrap(as.formula(paste("~", facet_var)))
    }
    
    p +
      scale_fill_manual(values = phase_colors, drop = FALSE) +
      scale_color_manual(values = phase_colors) +
      app_theme
    
  }  # closes build_plot
  # ------------------
  # Render chart
  # ------------------
  output$hist_plot <- renderPlot({
    df <- filtered_data()
    req(nrow(df) > 0)
    
    binwidth <- 365.25 * input$bin_years
    
    rd <- active_report_date()
    if (input$group_type == "WRIA") {
      build_plot(df, "WRIA_LABEL", binwidth, rd)
    } else if (input$group_type == "County") {
      build_plot(df, "County", binwidth, rd)
    } else {
      build_plot(df, NULL, binwidth, rd)
    }
  })  # closes renderPlot
  
  # ------------------
  # Render data table
  # ------------------
  output$data_table <- renderDT({
    req(wr_data(), input$phase_filter)
    
    wr_data() %>%
      filter(Phase %in% input$phase_filter) %>%
      mutate(
        `Years Pending` = as.integer(
          time_length(interval(PriorityDate, Sys.Date()), "years")
        ),
        PriorityDate = format(PriorityDate, "%Y-%m-%d")
      ) %>%
      select(
        `Record Document Number`,
        `WR Doc ID`,
        `Certificate Number`,
        `Person or Organization`,
        Status,
        Phase,
        Stage,
        PriorityDate,
        `Years Pending`,
        `Purpose of Use`,
        Qi,
        UOM,
        Qa,
        WRIA,
        WRIA_NAME,
        County,
        TRS,
        QQ,
        Q,
        `First Source Name Listed`
      ) %>%
      arrange(WRIA, PriorityDate) %>%
      datatable(
        filter   = "top",
        rownames = FALSE,
        options  = list(
          pageLength = 25,
          scrollX    = TRUE,
          autoWidth  = TRUE,
          columnDefs = list(
            list(width = "200px", targets = c(3)),
            list(width = "150px", targets = c(7)),
            list(width = "120px", targets = c(19))
          )
        )
      )
  })
  # ------------------
  # Render timeliness header
  # ------------------
  output$timeliness_header <- renderUI({
    if (is.null(timeliness_data)) {
      div(
        style = "color: #a04000; padding: 10px;",
        "⚠ Timeliness data could not be loaded. Check your network connection."
      )
    } else {
      yrs <- sort(unique(timeliness_data$year))
      div(
        # source line
        div(
          style = "color: #555; font-size: 13px; padding: 0 10px;",
          paste0(
            "Showing Ecology permit processing times for Water Right applications, ",
            min(yrs), " – ", max(yrs), ". ",
            "Source: WA State Permit Timeliness dataset (data.wa.gov)."
          )
        ),
        br(),
        # callout box
        div(
          style = paste0(
            "background-color: #fff8e1;",        # soft amber background
            "border-left: 4px solid #f0a500;",   # amber left border
            "border-radius: 4px;",
            "padding: 12px 16px;",
            "margin: 0 10px;",
            "font-size: 13px;",
            "color: #555;"
          ),
          tags$b("Note: "),
          paste0(
            "Pending applications have waited significantly longer than the average ",
            "processing time for completed applications, indicating that Ecology ",
            "processes applications based on factors beyond filing date — such as ",
            "applicant preparedness and availability of mitigation."
          )
        )
      )
    }
  })
  
  # ------------------
  # Render timeliness plot
  # ------------------
  output$timeliness_plot <- renderPlot({
    req(!is.null(timeliness_data))
    
    # shared theme — matches the rest of the app
    app_theme <- theme_minimal(base_size = 16) +
      theme(
        panel.background   = element_rect(fill = "white", color = NA),
        plot.background    = element_rect(fill = "white", color = NA),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(color = "grey85"),
        panel.grid.minor   = element_blank(),
        strip.text         = element_text(size = 16, face = "bold"),
        axis.title         = element_text(size = 16, face = "bold"),
        axis.text          = element_text(size = 14),
        legend.position    = "top",
        legend.text        = element_text(size = 14),
        legend.title       = element_text(size = 14, face = "bold"),
        axis.ticks         = element_line(color = "grey40"),
        axis.ticks.length  = unit(0.2, "cm")
      )
    
    phase_colors <- c(
      "New Application"    = "#4C78A8",
      "Change Application" = "#F58518"
    )
    
    # pivot to long format so both segments plot cleanly
    plot_df <- timeliness_data %>%
      select(year, permit_label,
             receipt_to_complete_average,
             complete_to_decision_average) %>%
      tidyr::pivot_longer(
        cols      = c(receipt_to_complete_average,
                      complete_to_decision_average),
        names_to  = "segment",
        values_to = "days"
      ) %>%
      mutate(
        segment_label = recode(segment,
                               "receipt_to_complete_average"  = "Receipt to Complete\n(Applicant burden)",
                               "complete_to_decision_average" = "Complete to Decision\n(Ecology processing)"
        ),
        # years+days label for each point
        yrs  = floor(days / 365.25),
        dys  = round(days %% 365.25),
        tip  = paste0(yrs, " yrs, ", dys, " days")
      )
    
    ggplot(plot_df,
           aes(x     = year,
               y     = days,
               color = permit_label,
               group = permit_label)) +
      
      geom_line(linewidth = 1.2) +
      geom_point(size = 3) +
      
      # years+days label above each point
      geom_text(
        aes(label = tip),
        vjust    = -0.8,
        size     = 3.5,
        show.legend = FALSE
      ) +
      
      # separate panels for each time segment
      facet_wrap(~ segment_label, ncol = 1, scales = "free_y") +
      
      scale_color_manual(values = phase_colors) +
      scale_x_continuous(breaks = unique(plot_df$year)) +
      scale_y_continuous(
        name   = "Average Days",
        labels = function(x) {
          yrs <- floor(x / 365.25)
          dys <- round(x %% 365.25)
          paste0(yrs, " yrs\n", dys, " days")
        },
        expand = expansion(mult = c(0.05, 0.15))
      ) +
      
      labs(
        x     = "Year",
        color = "Application Type",
        title = "Water Right Permit Processing Times — Washington State",
        subtitle = paste0(
          "Average days from receipt to complete (applicant) and ",
          "complete to decision (Ecology)"
        )
      ) +
      
      app_theme +
      theme(
        plot.title    = element_text(size = 18, face = "bold"),
        plot.subtitle = element_text(size = 13, color = "grey40"),
        plot.margin   = margin(0.25, 0.1, 0.1, 0.1, "in")
      )
  })
}  # closes server

# ================================
# 7. Run
# ================================
shinyApp(ui = ui, server = server)