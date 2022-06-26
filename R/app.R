library(shiny)
library(shinydashboard)
library(DT)
library(tidyverse)
library(tidyquant)
library(quantmod)
library(crypto2)
library(plotly)
library(gridExtra)


# ------------------------------------------------------
# LOAD DATA
#-------------------------------------------------------
# Get dataframe with historical crypto data
top_10_crypto <- crypto_history(limit=10,
                                start_date='20210601',
                                finalWait = FALSE)

# Convert time_open to date
top_10_crypto$time_open<-as.Date(top_10_crypto$time_open)


# Extract names of top 10 cryptos
top_10_crypto_names <- unique(top_10_crypto$name)
  


# ------------------------------------------------------
# UI
#-------------------------------------------------------
ui <- dashboardPage(
  skin = "red",
  dashboardHeader(title = "Share Prices"),
  dashboardSidebar(
    sidebarMenu(
      # Stocks
      menuItem("Stocks",
        tabName = "stocks", icon = icon("chart-line"),
        ## Table with prices
        menuSubItem("Stocks Tables",
          tabName = "stocks_tables", href = NULL, newtab = TRUE,
          icon = shiny::icon("table"), selected = NULL
        ),
        ## Stock Price Plot
        menuSubItem("Stocks Plots",
          tabName = "stocks_plot", href = NULL, newtab = TRUE,
          icon = shiny::icon("chart-line"), selected = NULL
        )
      ),
      # Crypto
      menuItem("Crypto",
        tabName = "crypto", icon = icon("bitcoin"),

        ## Table with prices
        menuSubItem("Crypto Table",
          tabName = "crypto_table", href = NULL, newtab = TRUE,
          icon = shiny::icon("table"), selected = NULL
        ),
        ## Crypto Price Plot
        menuSubItem("Top 10 Crypto Plots",
                    tabName = "crypto_plots", href = NULL, newtab = TRUE,
                    icon = shiny::icon("chart-line"), selected = NULL
        ),
        ## Crypto hehe
        menuSubItem("Crypto Hehe",
                    tabName = "crypto_wojak", href = NULL, newtab = TRUE,
                    icon = shiny::icon("chart-line"), selected = NULL
        )
      )
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(
        "stocks_tables",
        fluidPage(
          h1(icon("chart-line"), "Stock information by stock index"),
          selectInput("stock_index",
            label = "Stock Index:",
            choices = tq_index_options()
          ),
          dataTableOutput("stocks_index_table")
        ),
        fluidPage(
          h1(icon("chart-line"), "Stock information by stock exchange"),
          selectInput("stock_exchange",
            label = "Stock Exchange:",
            choices = tq_exchange_options()
          ),
          dataTableOutput("stocks_exchange_table")
        )
      ),
      tabItem(
        "stocks_plot",
        box(plotlyOutput("stock_price_plot"), width = 8),
        box(selectInput("stock_name", "Stock name:",
          selectize = FALSE,
          choices = tq_exchange("NASDAQ")[1]
        ), width = 4)
      ),
      tabItem(
        "crypto_table",
        fluidPage(
          h1(icon("bitcoin"), "Crypto Currencies"),
          dataTableOutput("cryptotable")
        )
      ),
      tabItem(
        "crypto_plots",
        fluidPage(
          h1(icon("chart-line"), "Top 10 crypto currencies"),
          fluidRow(
            column(12, 
                   plotlyOutput("crypto1")),
          ),
          br(),
          fluidRow(
            column(4, 
                   plotlyOutput("crypto2")),
            column(4, 
                   plotlyOutput("crypto3")),
            column(4, 
                   plotlyOutput("crypto4"))
          ),
          br(),
          fluidRow(
            column(4, 
                   plotlyOutput("crypto5")),
            column(4, 
                   plotlyOutput("crypto6")),
            column(4, 
                   plotlyOutput("crypto7"))
          ),
          br(),
          fluidRow(
            column(4, 
                   plotlyOutput("crypto8")),
            column(4, 
                   plotlyOutput("crypto9")),
            column(4, 
                   plotlyOutput("crypto10"))
          )
        )
      ),
      tabItem(
        "crypto_wojak",
        fluidPage(
          h1(icon("bitcoin"), "Crypto Wojak"),
          HTML('<iframe width="1120" height="630" src="https://www.youtube.com/embed/f4UoiE84mdA" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>')
        )
      )
    )
  )
)

# ------------------------------------------------------
# SERVER
#-------------------------------------------------------
server <- function(input, output) {
  output$stock_price_plot <- renderPlotly({
    ggplot(tq_get(input$stock_name), aes(x = date, y = close)) +
      geom_line(color = "darkblue") +
      ggtitle("Stock prices series") +
      xlab("Date") +
      ylab("Price") +
      theme(plot.title = element_text(hjust = 0.5)) +
      scale_x_date(date_labels = "%b %y", date_breaks = "6 months") +
      theme_minimal()
  })
  
  output$crypto1 <- renderPlotly({
    ggplot(subset(top_10_crypto, name==top_10_crypto_names[1]), aes(x = time_open, y = close)) +
      geom_line(color = "darkblue") +
      ggtitle(paste(top_10_crypto_names[1], 'price')) +
      xlab("Date") +
      ylab("Price") +
      theme(plot.title = element_text(hjust = 0.5)) +
      scale_x_date(date_labels = "%b %y", date_breaks = "1 months") +
      theme_minimal()
  })
  
  output$crypto2 <- renderPlotly({
    ggplot(subset(top_10_crypto, name==top_10_crypto_names[2]), aes(x = time_open, y = close)) +
      geom_line(color = "darkblue") +
      ggtitle(paste(top_10_crypto_names[2], 'price')) +
      xlab("Date") +
      ylab("Price") +
      theme(plot.title = element_text(hjust = 0.5)) +
      scale_x_date(date_labels = "%b %y", date_breaks = "3 months") +
      theme_minimal()
  })
  
  output$crypto3 <- renderPlotly({
    ggplot(subset(top_10_crypto, name==top_10_crypto_names[3]), aes(x = time_open, y = close)) +
      geom_line(color = "darkblue") +
      ggtitle(paste(top_10_crypto_names[3], 'price')) +
      xlab("Date") +
      ylab("Price") +
      theme(plot.title = element_text(hjust = 0.5)) +
      scale_x_date(date_labels = "%b %y", date_breaks = "3 months") +
      theme_minimal()
  })
  
  output$crypto4 <- renderPlotly({
    ggplot(subset(top_10_crypto, name==top_10_crypto_names[4]), aes(x = time_open, y = close)) +
      geom_line(color = "darkblue") +
      ggtitle(paste(top_10_crypto_names[4], 'price')) +
      xlab("Date") +
      ylab("Price") +
      theme(plot.title = element_text(hjust = 0.5)) +
      scale_x_date(date_labels = "%b %y", date_breaks = "3 months") +
      theme_minimal()
  })
  
  output$crypto5 <- renderPlotly({
    ggplot(subset(top_10_crypto, name==top_10_crypto_names[5]), aes(x = time_open, y = close)) +
      geom_line(color = "darkblue") +
      ggtitle(paste(top_10_crypto_names[5], 'price')) +
      xlab("Date") +
      ylab("Price") +
      theme(plot.title = element_text(hjust = 0.5)) +
      scale_x_date(date_labels = "%b %y", date_breaks = "3 months") +
      theme_minimal()
  })
  output$crypto6 <- renderPlotly({
    ggplot(subset(top_10_crypto, name==top_10_crypto_names[6]), aes(x = time_open, y = close)) +
      geom_line(color = "darkblue") +
      ggtitle(paste(top_10_crypto_names[6], 'price')) +
      xlab("Date") +
      ylab("Price") +
      theme(plot.title = element_text(hjust = 0.5)) +
      scale_x_date(date_labels = "%b %y", date_breaks = "3 months") +
      theme_minimal()
  })
  
  output$crypto7 <- renderPlotly({
    ggplot(subset(top_10_crypto, name==top_10_crypto_names[7]), aes(x = time_open, y = close)) +
      geom_line(color = "darkblue") +
      ggtitle(paste(top_10_crypto_names[7], 'price')) +
      xlab("Date") +
      ylab("Price") +
      theme(plot.title = element_text(hjust = 0.5)) +
      scale_x_date(date_labels = "%b %y", date_breaks = "3 months") +
      theme_minimal()
  })
  
  output$crypto8 <- renderPlotly({
    ggplot(subset(top_10_crypto, name==top_10_crypto_names[8]), aes(x = time_open, y = close)) +
      geom_line(color = "darkblue") +
      ggtitle(paste(top_10_crypto_names[8], 'price')) +
      xlab("Date") +
      ylab("Price") +
      theme(plot.title = element_text(hjust = 0.5)) +
      scale_x_date(date_labels = "%b %y", date_breaks = "3 months") +
      theme_minimal()
  })
  
  output$crypto9 <- renderPlotly({
    ggplot(subset(top_10_crypto, name==top_10_crypto_names[9]), aes(x = time_open, y = close)) +
      geom_line(color = "darkblue") +
      ggtitle(paste(top_10_crypto_names[9], 'price')) +
      xlab("Date") +
      ylab("Price") +
      theme(plot.title = element_text(hjust = 0.5)) +
      scale_x_date(date_labels = "%b %y", date_breaks = "3 months") +
      theme_minimal()
  })
  
  output$crypto10 <- renderPlotly({
    ggplot(subset(top_10_crypto, name==top_10_crypto_names[10]), aes(x = time_open, y = close)) +
      geom_line(color = "darkblue") +
      ggtitle(paste(top_10_crypto_names[10], 'price')) +
      xlab("Date") +
      ylab("Price") +
      theme(plot.title = element_text(hjust = 0.5)) +
      scale_x_date(date_labels = "%b %y", date_breaks = "3 months") +
      theme_minimal()
  })
  
  output$stocks_index_table <- renderDataTable(tq_index(input$stock_index))

  output$stocks_exchange_table <- renderDataTable(tq_exchange(input$stock_exchange))

  output$cryptotable <- renderDataTable(crypto_list())
}

# Run the application
shinyApp(ui, server)
