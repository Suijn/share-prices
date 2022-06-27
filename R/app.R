library(shiny)
library(shinydashboard)
library(DT)
library(tidyverse)
library(dplyr)
library(tidyquant)
library(quantmod)
library(crypto2)
library(plotly)
library(gridExtra)


# ------------------------------------------------------
# LOAD DATA
#-------------------------------------------------------

# Get dataframe with top 10 NASDAQ stocks by market.cap
tidyquant::tq_exchange("NASDAQ") %>% 
dplyr::arrange(desc(market.cap)) %>% 
dplyr::filter(symbol != 'GOOGL') %>% 
head(10) -> top_10_stock_names


# Get dataframe with historical crypto data
top_10_crypto <- crypto2::crypto_history(limit=10,
                                         start_date='20210601',
                                         finalWait = FALSE)

# Convert time_open to date
top_10_crypto$time_open<-as.Date(top_10_crypto$time_open)


# Extract names of top 10 cryptos
top_10_crypto_names <- unique(top_10_crypto$name)
  


# ------------------------------------------------------
# UI
#-------------------------------------------------------
ui <- shinydashboard::dashboardPage(
                      skin = "red",
  shinydashboard::dashboardHeader(title = "Share Prices"),
  shinydashboard::dashboardSidebar(
    shinydashboard::sidebarMenu(
      # Stocks
      shinydashboard::menuItem("Stocks",
        tabName = "stocks", icon = shiny::icon("chart-line"),
        ## Table with prices
        shinydashboard::menuSubItem("Stocks Tables",
                        tabName = "stocks_tables", href = NULL, newtab = TRUE,
                        icon = shiny::icon("table"), selected = NULL
        ),
        ## Stock Price Plot
        shinydashboard::menuSubItem("Stocks Plots",
                          tabName = "stocks_plot", href = NULL, newtab = TRUE,
                          icon = shiny::icon("chart-line"), selected = NULL
        ),
        ## Stock Price Plot
        shinydashboard::menuSubItem("Top 10 Stocks",
                          tabName = "top_10_stocks", href = NULL, newtab = TRUE,
                          icon = shiny::icon("chart-line"), selected = NULL
        )
      ),
      # Crypto
      shinydashboard::menuItem("Crypto",
                     tabName = "crypto", icon =  shiny::icon("bitcoin"),

        ## Table with prices
        shinydashboard::menuSubItem("Crypto Table",
                          tabName = "crypto_table", href = NULL, newtab = TRUE,
                          icon = shiny::icon("table"), selected = NULL
        ),
        ## Crypto Price Plot
        shinydashboard::menuSubItem("Top 10 Crypto Plots",
                          tabName = "crypto_plots", href = NULL, newtab = TRUE,
                          icon = shiny::icon("chart-line"), selected = NULL
        ),
        ## Crypto hehe
        shinydashboard::menuSubItem("Crypto Hehe",
                          tabName = "crypto_wojak", href = NULL, newtab = TRUE,
                          icon = shiny::icon("chart-line"), selected = NULL
        )
      )
    )
  ),
  shinydashboard::dashboardBody(
    shinydashboard::tabItems(
      shinydashboard::tabItem(
        "stocks_tables",
        shiny::fluidPage(
          shiny::h1(shiny::icon("chart-line"), "Stock information by stock index"),
          shiny::selectInput("stock_index",
            label = "Stock Index:",
            choices = tidyquant::tq_index_options()
          ),
          DT::dataTableOutput("stocks_index_table")
        ),
        shiny::fluidPage(
          shiny::h1(shiny::icon("chart-line"), "Stock information by stock exchange"),
          shiny::selectInput("stock_exchange",
            label = "Stock Exchange:",
            choices = tidyquant::tq_exchange_options()
          ),
          DT::dataTableOutput("stocks_exchange_table")
        )
      ),
      shinydashboard::tabItem(
        "stocks_plot",
        shinydashboard::box(plotly::plotlyOutput("stock_price_plot"), width = 8),
        shinydashboard::box(shiny::selectInput("stock_name", "Stock name:",
                          selectize = FALSE,
                          choices = tidyquant::tq_exchange("NASDAQ")[1]
        ), width = 4)
      ),
      shinydashboard::tabItem(
        "top_10_stocks",
        shiny::fluidPage(
          shiny::h1(shiny::icon("chart-line"), "Top 10 NASDAQ stocks by market cap"),
          shiny::fluidRow(
            shiny::column(12, 
                   plotly::plotlyOutput("stock1")),
          ),
          shiny::br(),
          shiny::fluidRow(
            shiny::column(4, 
                    plotly::plotlyOutput("stock2")),
            shiny::column(4, 
                    plotly::plotlyOutput("stock3")),
            shiny::column(4, 
                    plotly::plotlyOutput("stock4"))
          ),
          shiny::br(),
          shiny::fluidRow(
            shiny::column(4, 
                   plotly::plotlyOutput("stock5")),
            shiny::column(4, 
                   plotly::plotlyOutput("stock6")),
            shiny::column(4, 
                   plotly::plotlyOutput("stock7"))
          ),
          shiny::br(),
          shiny::fluidRow(
            shiny::column(4, 
                  plotly::plotlyOutput("stock8")),
            shiny::column(4, 
                  plotly::plotlyOutput("stock9")),
            shiny::column(4, 
                  plotly::plotlyOutput("stock10"))
          )
        )
      ),
      shinydashboard::tabItem(
        "crypto_table",
        shiny::fluidPage(
          shiny::h1(shiny::icon("bitcoin"), "Crypto Currencies"),
          DT::dataTableOutput("cryptotable")
        )
      ),
      shinydashboard::tabItem(
        "crypto_plots",
        shiny::fluidPage(
          shiny::h1(shiny::icon("chart-line"), "Top 10 crypto currencies"),
          shiny::fluidRow(
            shiny::column(12, 
                   plotly::plotlyOutput("crypto1")),
          ),
          shiny::br(),
          shiny::fluidRow(
            shiny::column(4, 
                   plotly::plotlyOutput("crypto2")),
            shiny::column(4, 
                   plotly::plotlyOutput("crypto3")),
            shiny::column(4, 
                   plotly::plotlyOutput("crypto4"))
          ),
          shiny::br(),
          shiny::fluidRow(
            shiny::column(4, 
                   plotly::plotlyOutput("crypto5")),
            shiny::column(4, 
                   plotly::plotlyOutput("crypto6")),
            shiny::column(4, 
                   plotly::plotlyOutput("crypto7"))
          ),
          shiny::br(),
          shiny::fluidRow(
            shiny::column(4, 
                   plotly::plotlyOutput("crypto8")),
            shiny::column(4, 
                   plotly::plotlyOutput("crypto9")),
            shiny::column(4, 
                   plotly::plotlyOutput("crypto10"))
          )
        )
      ),
      shinydashboard::tabItem(
        "crypto_wojak",
        shiny::fluidPage(
          shiny::h1(shiny::icon("bitcoin"), "Crypto Wojak"),
          shiny::HTML('<iframe width="1120" height="630" src="https://www.youtube.com/embed/f4UoiE84mdA" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>')
        )
      )
    )
  )
)




# -----------------------------------------------------_
# SERVER
#-------------------------------------------------------
server <- function(input, output) {
  output$stock_price_plot <- plotly::renderPlotly({
    ggplot2::ggplot(tidyquant::tq_get(input$stock_name), ggplot2::aes(x = date, y = close)) +
      ggplot2::geom_line(color = "darkblue") +
      ggplot2::ggtitle(paste(input$stock_name, "stock prices")) +
      ggplot2::xlab("Date") +
      ggplot2::ylab("Price") +
      ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5)) +
      ggplot2::scale_x_date(date_labels = "%b %y", date_breaks = "6 months") +
      ggplot2::theme_minimal()
  })
  
  output$stock1 <- plotly::renderPlotly({
    ggplot2::ggplot(tidyquant::tq_get(top_10_stock_names$symbol[1]), ggplot2::aes(x = date, y = close)) +
      ggplot2::geom_line(color = "darkblue") +
      ggplot2::ggtitle(top_10_stock_names$company[1]) +
      ggplot2::xlab("Date") +
      ggplot2::ylab("Price") +
      ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5)) +
      ggplot2::scale_x_date(date_labels = "%b %y", date_breaks = "6 months") +
      ggplot2::theme_minimal()
  })
  
  output$stock2 <- plotly::renderPlotly({
    ggplot2::ggplot(tidyquant::tq_get(top_10_stock_names$symbol[2]), ggplot2::aes(x = date, y = close)) +
      ggplot2::geom_line(color = "darkblue") +
      ggplot2::ggtitle(top_10_stock_names$company[2]) +
      ggplot2::xlab("Date") +
      ggplot2::ylab("Price") +
      ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5)) +
      ggplot2::scale_x_date(date_labels = "%b %y", date_breaks = "18 months") +
      ggplot2::theme_minimal()
  })
  
  output$stock3 <- plotly::renderPlotly({
    ggplot2::ggplot(tidyquant::tq_get(top_10_stock_names$symbol[3]), ggplot2::aes(x = date, y = close)) +
      ggplot2::geom_line(color = "darkblue") +
      ggplot2::ggtitle(top_10_stock_names$company[3]) +
      ggplot2::xlab("Date") +
      ggplot2::ylab("Price") +
      ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5)) +
      ggplot2::scale_x_date(date_labels = "%b %y", date_breaks = "18 months") +
      ggplot2::theme_minimal()
  })
  
  output$stock4 <- plotly::renderPlotly({
    ggplot2::ggplot(tidyquant::tq_get(top_10_stock_names$symbol[4]), ggplot2::aes(x = date, y = close)) +
      ggplot2::geom_line(color = "darkblue") +
      ggplot2::ggtitle(top_10_stock_names$company[4]) +
      ggplot2::xlab("Date") +
      ggplot2::ylab("Price") +
      ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5)) +
      ggplot2::scale_x_date(date_labels = "%b %y", date_breaks = "18 months") +
      ggplot2::theme_minimal()
  })
  
  output$stock5 <- plotly::renderPlotly({
    ggplot2::ggplot(tidyquant::tq_get(top_10_stock_names$symbol[5]), ggplot2::aes(x = date, y = close)) +
      ggplot2::geom_line(color = "darkblue") +
      ggplot2::ggtitle(top_10_stock_names$company[5]) +
      ggplot2::xlab("Date") +
      ggplot2::ylab("Price") +
      ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5)) +
      ggplot2::scale_x_date(date_labels = "%b %y", date_breaks = "18 months") +
      ggplot2::theme_minimal()
  })
  
  output$stock6 <- plotly::renderPlotly({
    ggplot2::ggplot(tidyquant::tq_get(top_10_stock_names$symbol[6]), ggplot2::aes(x = date, y = close)) +
      ggplot2::geom_line(color = "darkblue") +
      ggplot2::ggtitle(top_10_stock_names$company[6]) +
      ggplot2::xlab("Date") +
      ggplot2::ylab("Price") +
      ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5)) +
      ggplot2::scale_x_date(date_labels = "%b %y", date_breaks = "18 months") +
      ggplot2::theme_minimal()
  })
  
  output$stock7 <- plotly::renderPlotly({
    ggplot2::ggplot(tidyquant::tq_get(top_10_stock_names$symbol[7]), ggplot2::aes(x = date, y = close)) +
      ggplot2::geom_line(color = "darkblue") +
      ggplot2::ggtitle(top_10_stock_names$company[7]) +
      ggplot2::xlab("Date") +
      ggplot2::ylab("Price") +
      ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5)) +
      ggplot2::scale_x_date(date_labels = "%b %y", date_breaks = "18 months") +
      ggplot2::theme_minimal()
  })
  
  output$stock8 <- plotly::renderPlotly({
    ggplot2::ggplot(tidyquant::tq_get(top_10_stock_names$symbol[8]), ggplot2::aes(x = date, y = close)) +
      ggplot2::geom_line(color = "darkblue") +
      ggplot2::ggtitle(top_10_stock_names$company[8]) +
      ggplot2::xlab("Date") +
      ggplot2::ylab("Price") +
      ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5)) +
      ggplot2::scale_x_date(date_labels = "%b %y", date_breaks = "18 months") +
      ggplot2::theme_minimal()
  })
  
  output$stock9 <- plotly::renderPlotly({
    ggplot2::ggplot(tidyquant::tq_get(top_10_stock_names$symbol[9]), ggplot2::aes(x = date, y = close)) +
      ggplot2::geom_line(color = "darkblue") +
      ggplot2::ggtitle(top_10_stock_names$company[9]) +
      ggplot2::xlab("Date") +
      ggplot2::ylab("Price") +
      ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5)) +
      ggplot2::scale_x_date(date_labels = "%b %y", date_breaks = "18 months") +
      ggplot2::theme_minimal()
  })
  
  output$stock10 <- plotly::renderPlotly({
    ggplot2::ggplot(tidyquant::tq_get(top_10_stock_names$symbol[10]), ggplot2::aes(x = date, y = close)) +
      ggplot2::geom_line(color = "darkblue") +
      ggplot2::ggtitle(top_10_stock_names$company[10]) +
      ggplot2::xlab("Date") +
      ggplot2::ylab("Price") +
      ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5)) +
      ggplot2::scale_x_date(date_labels = "%b %y", date_breaks = "18 months") +
      ggplot2::theme_minimal()
  })
  
  output$crypto1 <- plotly::renderPlotly({
    ggplot2::ggplot(subset(top_10_crypto, name==top_10_crypto_names[1]), ggplot2::aes(x = time_open, y = close)) +
      ggplot2::geom_line(color = "darkblue") +
      ggplot2::ggtitle(paste(top_10_crypto_names[1], 'price')) +
      ggplot2::xlab("Date") +
      ggplot2::ylab("Price") +
      ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5)) +
      ggplot2::scale_x_date(date_labels = "%b %y", date_breaks = "1 months") +
      ggplot2::theme_minimal()
  })
  
  output$crypto2 <- plotly::renderPlotly({
    ggplot2::ggplot(subset(top_10_crypto, name==top_10_crypto_names[2]), ggplot2::aes(x = time_open, y = close)) +
      ggplot2::geom_line(color = "darkblue") +
      ggplot2::ggtitle(paste(top_10_crypto_names[2], 'price')) +
      ggplot2::xlab("Date") +
      ggplot2::ylab("Price") +
      ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5)) +
      ggplot2::scale_x_date(date_labels = "%b %y", date_breaks = "3 months") +
      ggplot2::theme_minimal()
  })
  
  output$crypto3 <- plotly::renderPlotly({
    ggplot2::ggplot(subset(top_10_crypto, name==top_10_crypto_names[3]), ggplot2::aes(x = time_open, y = close)) +
      ggplot2::geom_line(color = "darkblue") +
      ggplot2::ggtitle(paste(top_10_crypto_names[3], 'price')) +
      ggplot2::xlab("Date") +
      ggplot2::ylab("Price") +
      ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5)) +
      ggplot2::scale_x_date(date_labels = "%b %y", date_breaks = "3 months") +
      ggplot2::theme_minimal()
  })
  
  output$crypto4 <- plotly::renderPlotly({
    ggplot2::ggplot(subset(top_10_crypto, name==top_10_crypto_names[4]), ggplot2::aes(x = time_open, y = close)) +
      ggplot2::geom_line(color = "darkblue") +
      ggplot2::ggtitle(paste(top_10_crypto_names[4], 'price')) +
      ggplot2::xlab("Date") +
      ggplot2::ylab("Price") +
      ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5)) +
      ggplot2::scale_x_date(date_labels = "%b %y", date_breaks = "3 months") +
      ggplot2::theme_minimal()
  })
  
  output$crypto5 <- plotly::renderPlotly({
    ggplot2::ggplot(subset(top_10_crypto, name==top_10_crypto_names[5]), ggplot2::aes(x = time_open, y = close)) +
      ggplot2::geom_line(color = "darkblue") +
      ggplot2::ggtitle(paste(top_10_crypto_names[5], 'price')) +
      ggplot2::xlab("Date") +
      ggplot2::ylab("Price") +
      ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5)) +
      ggplot2::scale_x_date(date_labels = "%b %y", date_breaks = "3 months") +
      ggplot2::theme_minimal()
  })
  output$crypto6 <- plotly::renderPlotly({
    ggplot2::ggplot(subset(top_10_crypto, name==top_10_crypto_names[6]), ggplot2::aes(x = time_open, y = close)) +
      ggplot2::geom_line(color = "darkblue") +
      ggplot2::ggtitle(paste(top_10_crypto_names[6], 'price')) +
      ggplot2::xlab("Date") +
      ggplot2::ylab("Price") +
      ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5)) +
      ggplot2::scale_x_date(date_labels = "%b %y", date_breaks = "3 months") +
      ggplot2::theme_minimal()
  })
  
  output$crypto7 <- plotly::renderPlotly({
    ggplot2::ggplot(subset(top_10_crypto, name==top_10_crypto_names[7]), ggplot2::aes(x = time_open, y = close)) +
      ggplot2::geom_line(color = "darkblue") +
      ggplot2::ggtitle(paste(top_10_crypto_names[7], 'price')) +
      ggplot2::xlab("Date") +
      ggplot2::ylab("Price") +
      ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5)) +
      ggplot2::scale_x_date(date_labels = "%b %y", date_breaks = "3 months") +
      ggplot2::theme_minimal()
  })
  
  output$crypto8 <- plotly::renderPlotly({
    ggplot2::ggplot(subset(top_10_crypto, name==top_10_crypto_names[8]), ggplot2::aes(x = time_open, y = close)) +
      ggplot2::geom_line(color = "darkblue") +
      ggplot2::ggtitle(paste(top_10_crypto_names[8], 'price')) +
      ggplot2::xlab("Date") +
      ggplot2::ylab("Price") +
      ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5)) +
      ggplot2::scale_x_date(date_labels = "%b %y", date_breaks = "3 months") +
      ggplot2::theme_minimal()
  })
  
  output$crypto9 <- plotly::renderPlotly({
    ggplot2::ggplot(subset(top_10_crypto, name==top_10_crypto_names[9]), ggplot2::aes(x = time_open, y = close)) +
      ggplot2::geom_line(color = "darkblue") +
      ggplot2::ggtitle(paste(top_10_crypto_names[9], 'price')) +
      ggplot2::xlab("Date") +
      ggplot2::ylab("Price") +
      ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5)) +
      ggplot2::scale_x_date(date_labels = "%b %y", date_breaks = "3 months") +
      ggplot2::theme_minimal()
  })
  
  output$crypto10 <- plotly::renderPlotly({
    ggplot2::ggplot(subset(top_10_crypto, name==top_10_crypto_names[10]), ggplot2::aes(x = time_open, y = close)) +
      ggplot2::geom_line(color = "darkblue") +
      ggplot2::ggtitle(paste(top_10_crypto_names[10], 'price')) +
      ggplot2::xlab("Date") +
      ggplot2::ylab("Price") +
      ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5)) +
      ggplot2::scale_x_date(date_labels = "%b %y", date_breaks = "3 months") +
      ggplot2::theme_minimal()
  })
  
  output$stocks_index_table <- DT::renderDataTable(tidyquant::tq_index(input$stock_index))

  output$stocks_exchange_table <- DT::renderDataTable(tidyquant::tq_exchange(input$stock_exchange))

  output$cryptotable <- DT::renderDataTable(crypto2::crypto_list())
}

# Run the application
shiny::shinyApp(ui, server)
