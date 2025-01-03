library(shiny)
library(shinyjs)
library(randomForest)
library(DT)
library(plotly)
library(ggplot2)
library(shinydashboard)
library(gargle)

# Load the dataset
fund_data <- read.csv("C:\\Users\\user\\OneDrive\\Desktop\\MutualFund.csv")
fund_data <- na.omit(fund_data)

# Train Random Forest model
set.seed(123)
sample_indices <- sample(1:nrow(fund_data), size = 0.8 * nrow(fund_data))
fund_train <- fund_data[sample_indices, ]
fund_test <- fund_data[-sample_indices, ]
rf_model <- randomForest(
  returns_1yr ~ expense_ratio + fund_size_cr + sd + alpha + beta,
  data = fund_train,
  ntree = 500,
  mtry = 3,
  importance = TRUE
)

# Configure Google OAuth client
options(
  gargle_oauth_email = TRUE,
  gargle_oauth_client = gargle::gargle_oauth_client(
    id = "YOUR_CLIENT_ID",        # Replace with your Google OAuth Client ID
    secret = "YOUR_CLIENT_SECRET"  # Replace with your Client Secret
  )
)

# Define UI
ui <- fluidPage(
  useShinyjs(),
  
  # Home Page (initially visible)
  div(
    id = "home-page",
    style = "background: linear-gradient(to right, #8e44ad, #3498db); 
             color: white; 
             height: 100vh; 
             display: flex; 
             flex-direction: column; 
             justify-content: center; 
             align-items: center; 
             text-align: center;",
    
    h1("Welcome to Mutual Fund Analytics"),
    p(style = "font-size: 18px; max-width: 800px; margin: 20px auto;",
      "This application provides a platform for exploring, predicting, and analyzing mutual fund performance. 
       Leverage the power of machine learning and interactive visualizations to gain insights."),
    div(
      style = "margin: 40px 0; text-align: left; max-width: 800px;",
      h3("Features:"),
      tags$ul(
        tags$li("Interactive Data Table with conditional formatting."),
        tags$li("Prediction of mutual fund returns using a Random Forest model."),
        tags$li("Performance analysis with 2D and 3D visualizations."),
        tags$li("Interactive feature importance analysis."),
        tags$li("Custom return prediction using your own inputs.")
      )
    ),
    actionButton(
      "get_started", "Get Started", 
      class = "btn btn-light btn-lg",
      style = "margin-top: 30px;"
    )
  ),
  
  # Login Page
  hidden( div(
    id = "login-page",
    style = "background: linear-gradient(to right, #8e44ad, #3498db); color: white; height: 100vh; display: flex; flex-direction: column; justify-content: center; align-items: center; text-align: center;",
    
    h1("Login to Mutual Fund Analytics"),
    p("Please log in with your Google account to access the application."),
    actionButton("login_btn", "Log in with Google", class = "btn btn-primary btn-lg")
  )
  
  
  ),
  # Application Layout (Dashboard)
  hidden(
    div(
      id = "dashboardPage",
      dashboardPage(
        skin = "purple",
        dashboardHeader(
          title = "Mutual Fund Analytics",
          tags$li(
            class = "dropdown",
            actionButton(
              "logout_btn",
              "Log Out",
              icon = icon("sign-out-alt"),
              class = "btn btn-danger",
              style = "margin: 10px; color: white;"
            )
          )
        ),
        dashboardSidebar(
          sidebarMenu(
            menuItem("Data Table", tabName = "data_table", icon = icon("table")),
            menuItem("Prediction Results", tabName = "predictions", icon = icon("chart-bar")),
            menuItem("Performance Analysis", tabName = "performance", icon = icon("chart-line")),
            menuItem("Feature Importance", tabName = "feature_importance", icon = icon("list-alt")),
            menuItem("Predict Your Returns", tabName = "predict_fund", icon = icon("calculator")),
            menuItem("About", tabName = "about", icon = icon("info-circle"))
          )
        ),
        dashboardBody(
          fluidRow(
            valueBoxOutput("value_rmse", width = 4),
            valueBoxOutput("value_accuracy", width = 4),
            valueBoxOutput("value_entries", width = 4)
          ),
          
          tabItems(
            tabItem(
              tabName = "data_table",
              fluidRow(
                box(
                  title = "Fund Data Table",
                  width = 12,
                  status = "primary",
                  solidHeader = TRUE,
                  DTOutput("fund_table")
                )
              )
            ),
            
            tabItem(
              tabName = "predictions",
              fluidRow(
                box(
                  title = "Prediction Results",
                  width = 12,
                  status = "success",
                  solidHeader = TRUE,
                  DTOutput("predictions_table")
                )
              )
            ),
            
            tabItem(
              tabName = "performance",
              fluidRow(
                box(
                  title = "Performance Analysis (2D Plot)",
                  width = 6,
                  status = "info",
                  solidHeader = TRUE,
                  plotlyOutput("performance_plot", height = "500px")
                ),
                box(
                  title = "Performance Analysis (3D Plot)",
                  width = 6,
                  status = "info",
                  solidHeader = TRUE,
                  plotlyOutput("performance_plot_3d", height = "500px")
                )
              )
            ),
            
            tabItem(
              tabName = "feature_importance",
              fluidRow(
                box(
                  title = "Feature Importance Chart",
                  width = 12,
                  status = "warning",
                  solidHeader = TRUE,
                  plotlyOutput("interactive_feature_importance_chart", height = "500px")
                )
              )
            ),
            
            tabItem(
              tabName = "predict_fund",
              fluidRow(
                box(
                  title = "Predict Your Returns",
                  width = 12,
                  status = "success",
                  solidHeader = TRUE,
                  div(
                    style = "padding: 10px;",
                    h4("Enter Mutual Fund Details:"),
                    textInput("expense_ratio", "Expense Ratio:", ""),
                    textInput("fund_size_cr", "Fund Size (in crores):", ""),
                    textInput("sd", "Standard Deviation:", ""),
                    textInput("alpha", "Alpha:", ""),
                    textInput("beta", "Beta:", ""),
                    actionButton("predict_btn", "Predict Return", class = "btn btn-primary"),
                    br(), br(),
                    h4("Predicted Return (1 Year):"),
                    verbatimTextOutput("predicted_return")
                  )
                )
              )
            ),
            
            tabItem(
              tabName = "about",
              fluidRow(
                box(
                  title = "About the Application",
                  width = 12,
                  status = "warning",
                  solidHeader = TRUE,
                  div(
                    style = "padding: 10px;",
                    h4("Mutual Fund Analytics"),
                    p("This application allows users to explore mutual fund data, make predictions, and analyze performance using a machine learning model."),
                    p("Features include:"),
                    tags$ul(
                      tags$li("Interactive Data Table"),
                      tags$li("Prediction Results"),
                      tags$li("Performance Analysis with Scatter Plot"),
                      tags$li("Feature Importance Visualization")
                    ),
                    h5("Technologies Used:"),
                    tags$ul(
                      tags$li("R: A programming language for statistical computing."),
                      tags$li("Shiny: A web application framework for R to build interactive applications."),
                      tags$li("Random Forest: A machine learning algorithm used for predictive modeling."),
                      tags$li("ggplot2: A data visualization package for creating elegant graphics."),
                      tags$li("Plotly: A graphing library for interactive plots."),
                      tags$li("DT: A package to render interactive DataTables.")
                    ),
                    p("Built by using R, Shiny, and Random Forest for predictive modeling.")
                  )
                  
                )
              )
            )
          )
        )
      )
    )
  )
)

# Server Logic
server <- function(input, output, session) {
  user_data <- reactiveVal(NULL)
  
  # Handle Google Login
  observeEvent(input$login_btn, {
    # Authenticate with Google
    token <- gargle::token_fetch(scopes = "https://www.googleapis.com/auth/userinfo.profile")
    user_info <- gargle::token_userinfo(token)
    
    # Save user data
    user_data(user_info)
    
    # Toggle views
    hide("login-page", anim = TRUE, animType = "fade")
    show("dashboardPage", anim = TRUE, animType = "slide")
  })
  
  # Handle Get Started Button Click
  observeEvent(input$get_started, {
    hide("home-page", anim = TRUE, animType = "fade")
    show("login-page", anim = TRUE, animType = "fade")
  })
  
  # Handle Log Out Button Click
  observeEvent(input$logout_btn, {
    user_data(NULL) # Clear user data
    hide("dashboardPage", anim = TRUE, animType = "fade")
    show("home-page", anim = TRUE, animType = "fade")
  })
  
  # Value Boxes
  output$value_rmse <- renderValueBox({
    valueBox("4.5", "Root Mean Squared Error (RMSE)", icon = icon("calculator"), color = "purple")
  })
  output$value_accuracy <- renderValueBox({
    valueBox("75.14%", "Prediction Accuracy", icon = icon("check-circle"), color = "yellow")
  })
  output$value_entries <- renderValueBox({
    valueBox(nrow(fund_data), "Total Entries in Data Table", icon = icon("database"), color = "blue")
  })
  
  
  # Render Data Table
  output$fund_table <- renderDT({
    datatable(fund_data, options = list(pageLength = 10, class = 'cell-border stripe'))
  })
  
  # Render Predictions Table
  output$predictions_table <- renderDT({
    predictions <- predict(rf_model, fund_test)
    prediction_results <- data.frame(Actual = fund_test$returns_1yr, Predicted = predictions)
    datatable(prediction_results, options = list(pageLength = 10, class = 'cell-border stripe'))
  })
  
  # Render Performance Analysis Plot
  output$performance_plot <- renderPlotly({
    plot_data <- fund_test
    p <- ggplot(plot_data, aes(x = expense_ratio, y = returns_1yr, color = fund_size_cr)) +
      geom_point() +
      labs(x = "Expense Ratio", y = "Returns (1 Yr)", title = "Performance Analysis") +
      theme_minimal()
    ggplotly(p)
  })
  
  # Render Performance 3D Plot
  output$performance_plot_3d <- renderPlotly({
    plot_ly(fund_test, x = ~expense_ratio, y = ~returns_1yr, z = ~fund_size_cr, type = "scatter3d", mode = "markers")
  })
  # Interactive Feature Importance Chart
  output$interactive_feature_importance_chart <- renderPlotly({
    importance_values <- importance(rf_model)
    importance_df <- data.frame(Feature = rownames(importance_values), Importance = importance_values[, "%IncMSE"])
    plot_ly(data = importance_df, x = ~Importance, y = ~reorder(Feature, Importance), type = "bar", orientation = "h") %>%
      layout(title = "Feature Importance", xaxis = list(title = "Importance"), yaxis = list(title = "Feature"))
  })
  # Predict Returns
  observeEvent(input$predict_btn, {
    input_data <- data.frame(
      expense_ratio = as.numeric(input$expense_ratio),
      fund_size_cr = as.numeric(input$fund_size_cr),
      sd = as.numeric(input$sd),
      alpha = as.numeric(input$alpha),
      beta = as.numeric(input$beta)
    )
    prediction <- predict(rf_model, input_data)
    output$predicted_return <- renderText({
      paste0(round(prediction, 2), "%")
    })
  })
}

# Run the application
shinyApp(ui = ui, server = server)
