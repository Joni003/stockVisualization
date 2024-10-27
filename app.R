# Load packages ----
library(shiny)
library(bslib)
library(quantmod)
library(tidyverse)
library(tsibble)
library(extrafont) # in order to get Times New Roman

# Helper Functions
get_tsibble <- function(symbol, startingDate, endingDate) {
  getSymbols(symbol, src = "yahoo", env = NULL) |>
    as.data.frame() |>
    rownames_to_column("Date") |>
    mutate(Date = as.Date(Date)) |>
    as_tsibble(index = Date) |>
    select(Date, paste0(symbol, ".Close"))
}

# Uses ggplot to create the chart visualization for stocks
plot_data <- function(data, symbol, adjust = FALSE, startingDate, endingDate) {
  # Filter data according to date range
  data <- data |>
    # https://stackoverflow.com/questions/49388206/reactive-daterangeinput-for-shiny
    filter(between(Date , startingDate, endingDate))
  
  # Plot the data
  ggplot(data, aes_string(x = "Date", y = paste0(symbol, ".Close"))) +
    geom_line(color = "black") +
    labs(x = "Date", y = "Price (Dollars $)") +
    theme_minimal(base_size = 14) +
    
    # https://stackoverflow.com/questions/23527385/place-y-axis-on-the-right
    # reading over theme() in help page helped with customization
    theme(
      text = element_text(family = "Times New Roman"),
      panel.border = element_rect(color = "black", fill = NA, size = 0.8),
      panel.grid.major = element_line(color = "grey90"),
      panel.grid.minor = element_line(color = "grey95"),
      axis.title = element_text(size = 16, margin = margin(t = 16, r = 16, b = 16, l = 16)),
      axis.text = element_text(size = 14),
      axis.line.y.right = element_line(color = "black"),
      axis.ticks.y.right = element_line(),
      axis.ticks.length = unit(0.25, "cm"),
      plot.margin = margin(t = 24, r = 32, b = 24, l = 24, unit = "pt")
    ) +
    scale_y_continuous(
      position = "right",
      limits = c(0, NA),
      labels = scales::dollar_format(prefix = "$"),
      expand = expansion(mult = c(0.16, 0.16))  # Add padding to axis limits
    )
}

# Calculates the Gain/Loss of data range
gain_loss <- function(data, col_name) {
  start_price = first(data |> pull(col_name))
  end_price = last(data |> pull(col_name))
  gain_loss = ((end_price - start_price) / start_price) * 100
  gain_loss
}

fill_color <- function(gainloss) {
  ifelse(gainloss > 0, "green", "red")
}

# User interface ----
ui <- page_sidebar(
  
  # https://stackoverflow.com/questions/45359552/change-font-family-throughout-entire-r-shiny-app-css-html
  tags$head(tags$style(HTML('* {font-family: "Times New Roman"};'))),
  titlePanel("United States Securities Monitoring"),
  sidebar = sidebar(
    div(
      "This application lets you screen and learn about securities being sold in
      US Stock Exchanges. All data is scraped from Yahoo Finance and only includes
      stock price info starting from 01-01-2007. Key dynamic implementations include
      gain/loss metrics and date selection.",
      style = "color: 'black';"
    ),
    textInput("symb", "Symbol", "SPY"),
    dateRangeInput(
      "dates",
      "Date range",
      start = "2013-01-01",
      end = as.character(Sys.Date())
    ),
    actionButton("button", "Reset Graph"),
    br()
  ),
  card(
    card_header(
      fluidRow(
        column(6, textOutput("ticker_symbol"), align = "left"),
        column(6, textOutput("gain_loss_text"), align = "right")
      )
    ),
    plotOutput("plot", brush = brushOpts("brush", direction = "x", resetOnNew = TRUE))
  )
)

# Server logic
server <- function(input, output) {
  cap_symb <- reactive({
    toupper(input$symb)
  })
  
  # Retrieve the data and convert to tsibble format using custom function
  dataInput <- reactive({
    get_tsibble(cap_symb())
  })
  
  # Observing our brush highlighting over the x-axis, filtering data 
  observeEvent(input$brush, {
    brushed_range <- input$brush
    
    if (!is.null(brushed_range)) {
      # Convert the brush xmin and xmax to Date objects
      x_min <- as.Date(brushed_range$xmin, origin = "1970-01-01")
      x_max <- as.Date(brushed_range$xmax, origin = "1970-01-01")
    }
    
    # Filter from dataset the necessary gain/loss percentage
    brushed_data <- dataInput() |> 
      filter(Date >= x_min & Date <= x_max)
    
    if (nrow(brushed_data > 0)) {
      # Fetch the close_column name
      close_column <- paste0(cap_symb(), ".Close")
      
      #Calculate the gain/loss
      gain_loss <- gain_loss(brushed_data, close_column)
      
      # Outputting gain_loss text
      output$gain_loss_text <- renderText({
        metric <- toString(round(gain_loss, 2))
        ifelse(gain_loss > 0, paste0("+", metric, "%"), paste0("-", metric, "%"))
      })
      
      # Modifying our plot to output
      output$plot <- renderPlot({
        data <- dataInput()
        plot_data(data = data, symbol = cap_symb(), adjust = input$adjust, startingDate = input$dates[1], endingDate = input$dates[2]) +
          geom_ribbon(data = brushed_data, aes_string(ymin = "0", ymax = paste0(cap_symb(), ".Close")), 
                      fill = fill_color(gain_loss), alpha = 0.3)
      })
        
    }
  })
  
  observeEvent(input$button, {
    # Reset metric text
    output$gain_loss_text <- renderText({
      paste0()
    })
    
    # Outputting our standard plot
    output$plot <- renderPlot({
      data <- dataInput()
      plot_data(data = data, symbol = cap_symb(), adjust = input$adjust, startingDate = input$dates[1], endingDate = input$dates[2]) # adjust functionality not added yet
    })
  })
  
  # Outputting texts
  output$ticker_symbol <- renderText({
    cap_symb()
  })
  
  # Outputting our plot
  output$plot <- renderPlot({
    data <- dataInput()
    plot_data(data = data, symbol = cap_symb(), adjust = input$adjust, startingDate = input$dates[1], endingDate = input$dates[2]) # adjust functionality not added yet
  })

}

# Run the app
shinyApp(ui, server)
