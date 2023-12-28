
install.packages("DT")
install.packages("shinyWidgets")
library(shinythemes)
library(DT)
library(shiny)
library(tidyverse)
library(shinyWidgets)


# NOTE: you need run all code from 'master_df_for_shiny.R' script before you run this script
# Then run this code to get app to appear

ui <- fluidPage(theme = shinytheme("spacelab"),
                setBackgroundColor(
                  color = c("SkyBlue", "ghostwhite"),
                  gradient = c("linear")
                ),
  titlePanel(strong("Your 2023 Joint Destination Planner")),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Choose The Cities You Would Like To Fly From"),
      
      selectInput("var1", "First City of Choice", choices = unique(master_df4$Departure_City)),
      selectInput("var2", "Second City of Choice", choices = unique(master_df4$Departure_City)),
      
      # Section for lowest average delays by airport
      uiOutput("lowest_delays_airport_var1"),
      uiOutput("lowest_delays_airport_var2")
    ),
    
    mainPanel(
      textOutput("selected_var"),
      strong("Destinations Both Parties Can Fly Direct To:"),
      verbatimTextOutput("arrival_cities_list")
    )
  )
)

server <- function(input, output, session) {
  output$selected_var <- renderText({
    paste("You have selected", input$var1, "and", input$var2)
  })
  
  # Reactive expression to filter data based on selected departure cities
  filtered_data <- reactive({
    master_df4 |>
      filter(Departure_City %in% c(input$var1, input$var2))
  })
  
  # Reactive expression to get unique arrival cities in alphabetical order
  arrival_cities <- reactive({
    intersect(
      filtered_data() |>
        filter(Departure_City == input$var1) |>
        pull(Arrival_City),
      filtered_data() |>
        filter(Departure_City == input$var2) |>
        pull(Arrival_City)
    ) |>
      sort()
  })
  
  output$arrival_cities_list <- renderPrint({
    formatted_cities <- sapply(
      split(arrival_cities(), (seq_along(arrival_cities()) - 1) %/% 1),
      paste, collapse = ", "
    )
    cat(paste(formatted_cities, collapse = "\n"))
  })
  
  # Reactive expression to get lowest average delays by AIRPORT for var1
  lowest_delays_airport_var1 <- reactive({
    top_n(average_delay_df, -3, wt = average_delay) |>
      filter(Departure_City == input$var1) |>
      group_by(departure_airport, airline_name) |>
      summarise(average_delay = mean(average_delay)) |>
      arrange(average_delay) |>
      group_by(departure_airport) |>
      slice_head(n = 3) |>
      mutate(average_delay = round(average_delay, 0))
  })
  
  # Reactive expression to get lowest average delays by AIRPORT for var2
  lowest_delays_airport_var2 <- reactive({
    top_n(average_delay_df, -3, wt = average_delay) |>
      filter(Departure_City == input$var2) |>
      group_by(departure_airport, airline_name) |>
      summarise(average_delay = mean(average_delay)) |>
      arrange(average_delay) |>
      group_by(departure_airport) |>
      slice_head(n = 3) |>
      mutate(average_delay = round(average_delay, 0))
  })
  
  # Render DataTable for lowest delays by AIRPORT for var1
  output$lowest_delays_airport_var1 <- renderUI({
    if (!is.null(input$var1)) {
      airport_tables_var1 <- lapply(unique(lowest_delays_airport_var1()$departure_airport), function(airport) {
        fluidRow(
          column(12, h5(paste("Three Airlines with the Lowest Average Delay for", airport))),
          column(12, DTOutput(paste0("lowest_delays_airport_table_var1_", airport)))
        )
      })
      do.call(tagList, airport_tables_var1)
    }
  })
  
  # Render DataTable for lowest delays by AIRPORT for var2
  output$lowest_delays_airport_var2 <- renderUI({
    if (!is.null(input$var2)) {
      airport_tables_var2 <- lapply(unique(lowest_delays_airport_var2()$departure_airport), function(airport) {
        fluidRow(
          column(12, h5(paste("Three Airlines with the Lowest Average Delay for", airport))),
          column(12, DTOutput(paste0("lowest_delays_airport_table_var2_", airport)))
        )
      })
      do.call(tagList, airport_tables_var2)
    }
  })
  
  # Render DataTables for lowest delays by AIRPORT for var1
  observe({
    airports_var1 <- unique(lowest_delays_airport_var1()$departure_airport)
    lapply(airports_var1, function(airport) {
      output[[paste0("lowest_delays_airport_table_var1_", airport)]] <- renderDT({
        datatable(
          lowest_delays_airport_var1() |>
            filter(departure_airport == airport) |>
            select(-departure_airport),
          options = list(paging = FALSE, searching = FALSE, info = FALSE),
          rownames = FALSE,  # Remove row numbers
          width = '100%'
        )
      })
    })
  })
  
  # Render DataTables for lowest delays by AIRPORT for var2
  observe({
    airports_var2 <- unique(lowest_delays_airport_var2()$departure_airport)
    lapply(airports_var2, function(airport) {
      output[[paste0("lowest_delays_airport_table_var2_", airport)]] <- renderDT({
        datatable(
          lowest_delays_airport_var2() |>
            filter(departure_airport == airport) |>
            select(-departure_airport),
          options = list(paging = FALSE, searching = FALSE, info = FALSE),
          rownames = FALSE,  # Remove row numbers
          width = '100%'
        )
      })
    })
  })
}

shinyApp(ui, server)
  


