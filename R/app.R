library(shiny)
library(shinydashboard)
library(DT)
library(tidyverse)
library(tidyquant)
library(quantmod)
library(crypto2)
library(plotly)


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
        ## Plot
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
        box(plotlyOutput("price_plot"), width = 8),
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
      )
    )
  )
)

# ------------------------------------------------------
# SERVER
#-------------------------------------------------------
server <- function(input, output) {
  output$price_plot <- renderPlotly({
    ggplot(tq_get(input$stock_name), aes(x = date, y = close)) +
      geom_line(color = "darkblue") +
      ggtitle("Stock prices series") +
      xlab("Date") +
      ylab("Price") +
      theme(plot.title = element_text(hjust = 0.5)) +
      scale_x_date(date_labels = "%b %y", date_breaks = "6 months") +
      theme_minimal()
  })

  output$stocks_index_table <- renderDataTable(tq_index(input$stock_index))

  output$stocks_exchange_table <- renderDataTable(tq_exchange(input$stock_exchange
  ))

  output$cryptotable <- renderDataTable(crypto_list())
}

# Run the application
shinyApp(ui, server)
