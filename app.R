library(shiny)
library(shinydashboard)
library(plotly)
library(ggplot2)
ui <- dashboardPage(
  dashboardHeader(title="Customer Analytics"),
  dashboardSidebar(
    selectInput(
      "customer", "Select Customer: ", choices = df_customer_data$`Customer Name`)),
  
  dashboardBody(
    fluidRow(
      tags$head(
        tags$style(HTML("
        .box-title {
          font-family: 'Roboto', sans-serif;
          color: #333;
          font-size: 18px;
          font-weight: bold;
          margin-bottom: 10px;
        }
        .box-body {
          font-family: 'Open Sans', sans-serif;
          color: #333;
          font-size: 14px;
          background-color: #f5f5f5;
          font-weight: bold;
          border: 1px solid #008000;
          border-radius: 5px;
          box-shadow: 2px 2px 2px #008000;
          padding: 10px;
          margin-bottom: 20px;
          transition: all 0.3s ease-in-out;
        }
        .box-body:hover {
          transform: scale(1.05);
          box-shadow: 4px 4px 4px #008000;
        }
      "))
      )
    ),
    fluidRow( 
     column(
        width = 12,
  # Total Spent
        box(
          title = "Total Spent",
          status = "primary",
          solidHeader = TRUE,
          width = 3,
          textOutput("spent")
        ),
  
# Total Transactions
  box(
    title = "Total Transactions",
    status = "primary",
    solidHeader = TRUE,
    width = 3,
    textOutput("transactions")
  ),
      box(
        title = "First Transaction Date",
        status = "primary",
        solidHeader = TRUE,
        width = 3,
        textOutput("first_transaction")
         ),
     
      box(
        title = "Last Transaction Date",
        status = "primary",
        solidHeader = TRUE,
        width = 3,
        textOutput("last_transaction")
 
         ),
      box(
        title = "Avg Days btw Transaction",
        status = "primary",
        solidHeader = TRUE,
        width = 4,
        textOutput("median_days_between_orders")
      ),
      box(
        title = "RFM Segment",
        status = "primary",
        width = 4,
        solidHeader = TRUE,
        textOutput("RFM_segment")
      ),
      box(
        title = "Total Purchased Item",
        status = "primary",
        width = 4,
        solidHeader = TRUE,
        textOutput("quantity_bought")
      ),
      box(
        title = "Customer Segment",
        status = "primary",
        solidHeader = TRUE,
        width = 5,
        textOutput("Segment")
      ),
      box(
        title = "Total Discounts",
        status = "primary",
        solidHeader = TRUE,
        width = 5,
        textOutput("total_Discount")
      ),
      box(
        title = "Category of products mostly purchased",
        status = "primary",
        plotlyOutput("pie_chart_category")
      ),
      box(
        title = "Country wise Purchase Frequency",
        status = "primary",
        plotlyOutput("bar_plot_Country")
      ),
   box(
     title = "Most Preferred Shipment mode",
     status = "primary",
     plotlyOutput("pie_chart_shipment")
   ),
    box(
  title = "Customer Quantity Bought in Each Category",
  status = "primary",
  plotlyOutput("quantityByCategoryPlot")
    ),
     )
      )
       )
        )
      
#server code
server <- function(input, output, session) {
  
  sp <- reactive({
    df_customer_data$total_spent[df_customer_data$`Customer Name` == input$customer]
  })
  output$spent <- renderText({
    sp()
  })
#Total number of transaction  
  ts <- reactive({
    df_customer_data$n_transactions[df_customer_data$`Customer Name` == input$customer]
  })
  output$transactions <- renderText({
    ts()
  })
#First transaction
  first_transaction <- reactive({
    df_customer_data$first_transaction[df_customer_data$`Customer Name` == input$customer]
  })
  
  output$first_transaction <- renderText({
    format(first_transaction(), "%Y-%m-%d")
  })
#Last Transaction
  last_transaction <- reactive({
    df_customer_data$last_transaction[df_customer_data$`Customer Name` == input$customer]
  })
  
  output$last_transaction <- renderText({
    format(last_transaction(), "%Y-%m-%d")
  })
#Avg Days btw Transaction
  median_days_between_orders <- reactive({
    df_customer_data$median_days_between_orders[df_customer_data$`Customer Name` == input$customer]
  })
  
  output$median_days_between_orders <- renderText({
    median_days_between_orders()
  })
  #Rfm segments
  RFM_segment <- reactive({
    rfm_data$RFM_segment[rfm_data$`Customer Name` == input$customer]
  })
  
  output$RFM_segment <- renderText({
    RFM_segment()
  })
#Total purchased item
  quantity_bought <- reactive({
    rfm_quantity$quantity_bought[rfm_quantity$`Customer Name` == input$customer]
  })
  
  output$quantity_bought <- renderText({
    quantity_bought()
  }) 
#Total purchased item
  Segment <- reactive({
    df_customer_segment$Segment[df_customer_segment$`Customer Name` == input$customer]
  })
  
  output$Segment <- renderText({
    Segment()
  }) 
  #Total Discounted Items Purchased
  total_Discount <- reactive({
    df_customer_discount$total_Discount[df_customer_discount$`Customer Name` == input$customer]
  })
  
  output$total_Discount <- renderText({
    total_Discount()
  }) 
#pie chart Category wise Purchase
  output$pie_chart_category <- renderPlotly({
    
    cat <- subset(df_customer_category, df_customer_category$`Customer Name` == input$customer)
    print(cat)
    
    plot_ly(
      cat,
      labels = ~Category,  
      values = ~total_quantity,
      type = "pie"
    ) %>% 
      layout(title = "Category wise Purchase",
             xaxis = list(title = "Category Names"))
  })
#bar_plot Country wise Purchase Frequency
  output$bar_plot_Country <- renderPlotly({
    country <- subset(df_customer_country, df_customer_country$`Customer Name` == input$customer)
    print(cat)
    
    # Create the bar plot
    plot_ly(country, x = ~Country, y = ~RepeatedTransactions, 
            type = "bar", marker = list(color = "steelblue")) %>%
      layout(xaxis = list(title = "Country"), 
             yaxis = list(title = "Purchase Frequency"), 
             title = "Country wise Purchase Frequency")
  })
#pie chartMost Preferred Shipment Mode
  output$pie_chart_shipment <- renderPlotly({
    cat <- subset(df_customer_shipping, `Customer Name` == input$customer)
    
    plot_ly(cat, labels = ~`Ship Mode`, 
            values = ~total_shipments, 
            type = "pie") %>% 
      layout(title = "Most Preferred Shipment Mode", 
             xaxis = list(title = "Ship Mode"))
})
  output$quantityByCategoryPlot <- renderPlotly({
    Quan <- subset(df_customer_quantity, `Customer Name` == input$customer)
    plot_ly(Quan, x = ~Category, y = ~quantity_bought, 
            type = "bar", marker = list(color = "skyblue")) %>%
      layout(xaxis = list(title = "Category"), 
             yaxis = list(title = "Quantity Bought"), 
             title = "Customer Quantity Bought in Each Category")
  })
  
  }

shinyApp(ui, server)

      
    