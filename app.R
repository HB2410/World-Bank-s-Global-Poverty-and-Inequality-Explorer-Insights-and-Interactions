library(shiny)
library(DT)
library(dplyr)
library(ggplot2)

my_data <- read.csv("pip.csv")

ui <- fluidPage(
  titlePanel("World Bank's Global Poverty and Inequality Explorer: Insights and Interactions "),
  tabsetPanel(
    # First Tab
    tabPanel("Data: Inequality & Poverty",
             sidebarLayout(
               sidebarPanel(
                 selectInput(inputId = "country", label = "Select Country", choices = NULL),
                 selectInput(inputId = "year", label = "Select Reporting Year", choices = NULL),
                 selectInput(inputId = "level", label = "Select Reporting Level", choices = NULL)
               ),
               mainPanel(
                 dataTableOutput("table")
               )
             )
    ),

    # Second Tab
    tabPanel("Visualizing: Inequality & Poverty Data - Country Wise",
             sidebarLayout(
               sidebarPanel(
                 selectInput(inputId = "y", label = "Y-Axis",
                             choices = c("headcount", "poverty_severity", "gini", "reporting_pop", "reporting_gdp"),
                             selected = "headcount"),
                 checkboxInput("logy", "Add y log values", value = FALSE),
                 selectInput(inputId = "x", label = "X-Axis",
                             choices = c("reporting_year", "headcount", "poverty_severity", "gini", "reporting_pop", "reporting_gdp"),
                             selected = "reporting_year"),
                 selectInput(inputId = "z", label = "Color",
                             choices = c("country_name", "reporting_level"),
                             selected = "country_name"),
                 sliderInput(inputId = "year_range",
                             label = "Year Range",
                             min = min(my_data$reporting_year), max = max(my_data$reporting_year),
                             value = range(my_data$reporting_year), step = 1),
                 selectInput(inputId = "country_filter", label = "Select Country:",
                             choices = unique(my_data$country_name),
                             selected = "India",
                             selectize = FALSE),
                 sliderInput(inputId = "alpha",
                             label = "Alpha:",
                             min = 0, max = 1,
                             value = 0.5)
               ),
               mainPanel(
                 plotOutput(outputId = "plot", brush = "plot_brush"),
                 dataTableOutput("visual_table")
               )
             )
    ),

    # Third Tab
    tabPanel("Visualizing: Inequality & Poverty Data - Region Wise",
             sidebarLayout(
               sidebarPanel(
                 sliderInput(inputId = "grid_year_range",
                             label = "Year Range",
                             min = min(my_data$reporting_year), max = max(my_data$reporting_year),
                             value = range(my_data$reporting_year), step = 1),
                 selectInput(inputId = "grid_x", label = "X-Axis",
                             choices = c("headcount", "poverty_severity", "gini", "reporting_pop", "reporting_gdp"),
                             selected = "headcount"),
                 selectInput(inputId = "grid_y", label = "Y-Axis",
                             choices = c("headcount", "poverty_severity", "gini", "reporting_pop", "reporting_gdp"),
                             selected = "poverty_severity"),
                 selectInput(inputId = "grid_color", label = "Color",
                             choices = c("country_name", "reporting_level", "region_name"),
                             selected = "region_name")
               ),
               mainPanel(
                 plotOutput(outputId = "grid_plot", brush = "grid_brush"),
                 dataTableOutput("grid_table")
               )
             )
    ),

    # Fourth Tab
    tabPanel("Significance b/w Variables",
             sidebarLayout(
               sidebarPanel(
                 selectInput(inputId = "sig_y", label = "Y-Axis",
                             choices = c("headcount", "poverty_severity", "gini", "reporting_pop", "reporting_gdp"),
                             selected = "gini"),
                 checkboxInput("sig_logy", "Add y log values", value = FALSE),
                 selectInput(inputId = "sig_x", label = "X-Axis",
                             choices = c("headcount", "poverty_severity", "gini", "reporting_pop", "reporting_gdp"),
                             selected = "headcount"),
                 selectInput(inputId = "sig_color", label = "Color",
                             choices = c("country_name", "reporting_level", "region_name"),
                             selected = "region_name"),
                 sliderInput(inputId = "sig_year_range",
                             label = "Year Range",
                             min = min(my_data$reporting_year), max = max(my_data$reporting_year),
                             value = range(my_data$reporting_year), step = 1)
               ),
               mainPanel(
                 plotOutput(outputId = "sig_plot", brush = "sig_brush"),
                 verbatimTextOutput("sig_summary")
               )
             )
    )
  )
)

server <- function(input, output, session) {
  # Reactive values for dropdowns in the first tab
  all_years <- sort(unique(my_data$reporting_year))
  all_countries <- sort(unique(my_data$country_name))
  all_levels <- sort(unique(my_data$reporting_level))

  observe({
    updateSelectInput(session, "country", choices = c("All", all_countries), selected = "All")
    updateSelectInput(session, "year", choices = c("All", all_years), selected = "All")
    updateSelectInput(session, "level", choices = c("All", all_levels), selected = "All")
  })

  observeEvent(input$country, {
    if (input$country == "All") {
      updateSelectInput(session, "year", choices = c("All", all_years))
    } else {
      filtered_years <- sort(unique(my_data$reporting_year[my_data$country_name == input$country]))
      current_year <- ifelse(input$year %in% filtered_years, input$year, "All")
      updateSelectInput(session, "year", choices = c("All", filtered_years), selected = current_year)
    }
  }, ignoreInit = TRUE)

  observeEvent(input$year, {
    if (input$year == "All") {
      updateSelectInput(session, "country", choices = c("All", all_countries), selected = input$country)
    } else {
      filtered_countries <- sort(unique(my_data$country_name[my_data$reporting_year == input$year]))
      current_country <- ifelse(input$country %in% filtered_countries, input$country, "All")
      updateSelectInput(session, "country", choices = c("All", filtered_countries), selected = current_country)
    }
  }, ignoreInit = TRUE)

  observeEvent(c(input$country, input$year), {
    if (input$country == "All" && input$year == "All") {
      updateSelectInput(session, "level", choices = c("All", all_levels), selected = input$level)
    } else {
      filtered_levels <- my_data

      if (input$country != "All") {
        filtered_levels <- filtered_levels %>% filter(country_name == input$country)
      }

      if (input$year != "All") {
        filtered_levels <- filtered_levels %>% filter(reporting_year == input$year)
      }

      filtered_levels <- sort(unique(filtered_levels$reporting_level))
      current_level <- ifelse(input$level %in% filtered_levels, input$level, "All")
      updateSelectInput(session, "level", choices = c("All", filtered_levels), selected = current_level)
    }
  }, ignoreInit = TRUE)

  output$table <- renderDataTable({
    filtered_data <- my_data

    if (input$country != "All") {
      filtered_data <- filtered_data %>% filter(country_name == input$country)
    }

    if (input$year != "All") {
      filtered_data <- filtered_data %>% filter(reporting_year == input$year)
    }

    if (input$level != "All") {
      filtered_data <- filtered_data %>% filter(reporting_level == input$level)
    }

    filtered_data %>%
      select(country_name, reporting_year, reporting_level, headcount, poverty_severity, gini, reporting_pop, reporting_gdp)
  })

  output$plot <- renderPlot({
    filtered_data <- my_data %>%
      filter(country_name == input$country_filter) %>%
      filter(reporting_year >= input$year_range[1] & reporting_year <= input$year_range[2])

    p <- filtered_data %>%
      ggplot(aes_string(x = input$x, y = input$y, color = input$z)) +
      geom_point(alpha = input$alpha) +
      theme_minimal()

    if (input$logy) {
      p <- p + scale_y_log10()
    }

    p
  })

  output$visual_table <- renderDataTable({
    filtered_data <- my_data %>%
      filter(country_name == input$country_filter) %>%
      filter(reporting_year >= input$year_range[1] & reporting_year <= input$year_range[2])

    brushedPoints(filtered_data, brush = input$plot_brush)
  })

  output$grid_plot <- renderPlot({
    grid_filtered_data <- my_data %>%
      filter(reporting_year >= input$grid_year_range[1] & reporting_year <= input$grid_year_range[2])

    p <- grid_filtered_data %>%
      ggplot(aes_string(x = input$grid_x, y = input$grid_y, color = input$grid_color)) +
      geom_point() +
      facet_wrap(~ region_name) +
      theme_minimal()

    p
  })

  output$grid_table <- renderDataTable({
    grid_filtered_data <- my_data %>%
      filter(reporting_year >= input$grid_year_range[1] & reporting_year <= input$grid_year_range[2])

    brushedPoints(grid_filtered_data, brush = input$grid_brush)
  })

  output$sig_plot <- renderPlot({
    sig_filtered_data <- my_data %>%
      filter(reporting_year >= input$sig_year_range[1] & reporting_year <= input$sig_year_range[2])

    p <- sig_filtered_data %>%
      ggplot(aes_string(x = input$sig_x, y = input$sig_y, color = input$sig_color)) +
      geom_point() +
      facet_wrap(~ region_name) +
      theme_minimal()

    if (input$sig_logy) {
      p <- p + scale_y_log10()
    }

    p
  })

  output$sig_summary <- renderPrint({
    sig_filtered_data <- my_data %>%
      filter(reporting_year >= input$sig_year_range[1] & reporting_year <= input$sig_year_range[2])

    sig_brushed_data <- brushedPoints(sig_filtered_data, brush = input$sig_brush)

    if (nrow(sig_brushed_data) > 1) {
      summary(lm(as.formula(paste(input$sig_y, "~", input$sig_x)), data = sig_brushed_data))
    } else {
      "No sufficient data points selected for regression."
    }
  })
}

shinyApp(ui = ui, server = server)
