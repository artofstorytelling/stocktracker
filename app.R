
############################
#.   Load Packages
############################
##################################
#. Load Packages
##################################
pacman::p_load(shiny, quantmod, xts, tidyverse, tidyquant, TTR, highcharter, lubridate) 

options(scipen=999,dplyr.summarise.inform = FALSE)
gc()

##########################################
# Define a list of available stock symbols
##########################################

tickers <- c("AAPL", "MSFT", "GOOGL", "AMZN", "META", "F", "GM","QQQ")
stock_names <- c('Apple','Microsoft','Alphabet','Amazon','Meta (FB)','Ford', 'GM','Invesco QQQ ETF')
available_stocks <- data.frame(tickers, stock_names) %>% arrange(tickers)

###########################################
#          Build App POC 
###########################################

#################################
#         UI
#################################

ui <- fluidPage(
  tags$head(
    tags$style(
      HTML("body { background-color: #1e1e1e; }"),
      HTML( ".title {background-color: white; color: white; font-weight: bold; font-size: 20px;}")
    )
  ),
  
  
  titlePanel("Stock Charter"),
  sidebarLayout(
    sidebarPanel(
      selectInput("stockSymbol", "Select Stock Symbol:", 
                  choices = available_stocks$tickers),

      dateRangeInput("dateRange", "Select Date Range:", start = Sys.Date() - years(2), 
                     end = Sys.Date()),
      checkboxInput("ma9", "Add 9-day Moving Average"),
      checkboxInput("ma20", "Add 20-day Moving Average"),
      checkboxInput("ma50", "Add 50-day Moving Average"),
      checkboxInput("ma200", "Add 200-day Moving Average"),
      checkboxInput("rsi", "Add RSI"),
      downloadButton("downloadData", "Download Data")
    ),
    mainPanel(
      wellPanel(
        tags$div(
          style = "background-color: #BFD7EA; padding: 5px; border-radius: 5px; color: #333333; font-size: 12px;",
          "Prices are not real-time and are based on the previous close."
        ),
        highchartOutput("stockChart", height = "60vh"),
        highchartOutput("rsiChart", height = "15vh"),
        verbatimTextOutput("errorMsg")
      )
    )
  )
)

####################################
#      Server
####################################

server <- function(input, output) {
  
  output$errorMsg <- renderPrint({
                            tryCatch({
                              stock_data <- selected_stock_data()
                              if (is.null(stock_data) || nrow(stock_data) < 30) {
                                return("Data is insufficient increase date range")
                              }
                            }, error = function(e) {
                              return("Data is insufficient increase date range")
                            })
          })
  
  # Download stock info
  downloadStockData <- function(symbol, start_date, end_date) {
    stock_data <- getSymbols(symbol, from = start_date, to = end_date, auto.assign = FALSE)
    return(stock_data)
  }
  
  # Get stock name linked to ticker 
  selected_stock_name <- reactive({
    symbol <- input$stockSymbol
    available_stocks$stock_names[available_stocks$tickers == symbol]
  })
  
  # Get selected stock data
  selected_stock_data <- reactive({
    symbol <- input$stockSymbol
    start_date <- as.Date(input$dateRange[1])
    end_date <- as.Date(input$dateRange[2])
    downloadStockData(symbol, start_date, end_date)
  })
  
  
  # Download data 
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(input$stockSymbol,"_Stock_Data_", 
            Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(selected_stock_data(), file)
    }
  )
  
  # Custom theme
  custom_theme <- hc_theme_merge(
    hc_theme_darkunica(),
    hc_theme(
      colors = c("#FFA500", "#008000", "#0000FF"),  # Orange, Green, Blue
      chart = list(
        backgroundColor = "#1e1e1e",  # Dark background color
        style = list(
          fontFamily = "Arial, sans-serif"
        )
      ),
      title = list(
        style = list(
          color = "#ffffff",  # White
          fontFamily = "Arial, sans-serif",
          fontSize = "20px"
        )
      ),
      subtitle = list(
        style = list(
          color = "#d3d3d3",  # Light gray
          fontFamily = "Arial, sans-serif",
          fontSize = "16px"
        )
      )
    )
  )
  
  ####################################
  # Plot Time Series Chart
  ####################################
  
  # RSI plot
  output$rsiChart <- renderHighchart({
    if (input$rsi) {
      
      tryCatch({
        stock_data <- selected_stock_data()
        if (is.null(stock_data) || nrow(stock_data) < 30) {
          return(NULL)  # Return NULL if data is insufficient
        }
        rsi <- round(RSI(Cl(stock_data)),2)
        
        # Median RSI
        median_rsi <- median(rsi, na.rm = TRUE)
        
        hc <- highchart() %>%
          hc_add_series(rsi, name = "RSI", type = "line" , color = "white" , showInLegend = FALSE) %>% # color = "#1FA67A"
          hc_add_yAxis(nid = 0L, title = list(text = "RSI"),  opposite = TRUE

 
                       
          ) |>
          # tooltip 
          hc_tooltip(
            valueDecimals = 2,
            useHTML = TRUE,
            formatter = JS(
              "function() {",
              "  var date = Highcharts.dateFormat('%Y-%m-%d', this.x);",
              "  var rsi = this.y;",
              "  var tooltip = '<b>' + date + '</b><br/>RSI: ' + rsi;",
              "  var median_rsi = ", median_rsi, ";",
              "  tooltip += '<br>Median RSI: ' + median_rsi;",
              "  if (rsi >= 70) {",
              "    tooltip += '<br/>RSI is Overbought';",
              "  } else if (rsi <= 30) {",
              "    tooltip += '<br/>RSI is Oversold';",
              "  }",
              "  return tooltip;",
              "}"
            )
          ) |>

          
          hc_yAxis(
            opposite = TRUE, 
            title = list(text = 'RSI'), 
            plotLines = list(

              list(
                from = 30,
                to = 70,
                color = 'darkgreen', 
                opacity = 0.2          # Adjust the opacity to control shading intensity
              ), 
              list(
                value = median_rsi, 
                color = "yellow", 
                width = 0.75, 
                zIndex = 2, 
                dashStyle = "dot"
              )

            )
          ) |>
          hc_add_theme(custom_theme)
        
      hc
      
      }, error = function(e) {
        return(NULL)  # Return NULL if there's an error
      })
      
    }

     
  })
  
  output$stockChart <- renderHighchart({
    
    # Pull ticker from Yahoo  
    tryCatch({
      stock <- input$stockSymbol
      stock_data <- quantmod::getSymbols(stock, src = "yahoo", 
                                         from = Sys.Date() - years(2),end=Sys.Date(), auto.assign = FALSE)
      if (is.null(stock_data) || nrow(stock_data) < 30) {
        return(NULL)  # Return NULL if data is insufficient
      }
      hc <-highchart(type = "stock") %>%
        hc_title(text = paste(selected_stock_name())) %>%
        hc_add_series(selected_stock_data(), yAxis = 0, showInLegend = FALSE, height = 100) %>%
        hc_add_yAxis(nid = 1L, title = list(text = "Prices"),  relative = 15,height = 1000) %>%
        hc_add_series(selected_stock_data()[, paste0(stock, ".Volume")], yAxis = 1, 
                      type = "column", showInLegend = FALSE,lineWidth = 1  
        ) %>%
        hc_add_yAxis(nid = 2L, title = list(text = "Volume"), relative = 2) |>
        hc_tooltip(valueDecimals = 2) |> # tooltip 
        hc_add_theme(custom_theme) 
      
      # Add 9-day SMA
      if (input$ma9) {
        ma9 <- SMA(Cl(selected_stock_data()), n = 9)
        hc <- hc %>%
          hc_add_series(data = ma9, name = "9-day MA", type = "line")
      }
      
      # Add 20-day SMA
      if (input$ma20) {
        ma20 <- SMA(Cl(selected_stock_data()), n = 20)
        hc <- hc %>%
          hc_add_series(data = ma20, name = "20-day MA", type = "line")
      }
      
      # Add 20-day SMA
      if (input$ma50) {
        ma50 <- SMA(Cl(selected_stock_data()), n = 50)
        hc <- hc %>%
          hc_add_series(data = ma50, name = "50-day MA", type = "line")
      }
      
      # Add 50-day SMA
      if (input$ma200) {
        ma200 <- SMA(Cl(selected_stock_data()), n = 200)
        hc <- hc %>%
          hc_add_series(data = ma200, name = "200-day MA", type = "line")
      }
      
      # Add RSI plot above volume plot
      if (input$rsi) {
        hc <- hc %>%
          hc_add_annotation(
            chart = 0,
            labels = list(
              list(
                point = list(xAxis = 0, yAxis = 0, x = 0, y = 0),
                text = "RSI",
                shape = "rect",
                backgroundColor = custom_theme,
                borderWidth = 0,
                style = list(color = "white", fontSize = "12px")
              )
            )
          ) 
        
      }
      
      hc
      
    }, error = function(e) {
      return(NULL)  # Return NULL if there's an error
    })
    
    
  })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)
