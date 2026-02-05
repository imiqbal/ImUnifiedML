#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(shinydashboard)
library(shinycssloaders)
library(shinyWidgets)
library(randomForest)
library(xgboost)
library(e1071)
library(nnet)
library(caret)
library(ggplot2)
library(plotly)
library(DT)
library(viridis)
library(GGally)
library(corrplot)
library(reshape2)
library(RColorBrewer)
library(kernlab)  # Added for SVM support in caret
library(gbm)      # Added for GBM support in caret
library(rpart)    # Added for CART support in caret

custom_css <- "
  .content-wrapper, .right-side {
    background: linear-gradient(135deg, #f5f7fa 0%, #c3cfe2 100%);
  }
  .main-header .navbar {
    background: linear-gradient(45deg, #667eea 0%, #764ba2 100%) !important;
  }
  .main-header .logo {
    background: linear-gradient(45deg, #2c3e50 0%, #34495e 100%) !important;
    font-weight: bold;
    color: white !important;
  }
  .skin-blue .main-sidebar {
    background: linear-gradient(180deg, #2c3e50 0%, #34495e 100%);
  }
  .box { 
    border-radius: 15px; 
    box-shadow: 0 8px 25px rgba(0,0,0,0.15);
    transition: all 0.3s ease;
  }
  .box:hover {
    transform: translateY(-2px);
    box-shadow: 0 12px 35px rgba(0,0,0,0.2);
  }
  .about-section {
    background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
    color: white;
    border-radius: 15px;
    padding: 25px;
    margin-bottom: 20px;
    box-shadow: 0 8px 25px rgba(102, 126, 234, 0.3);
  }
  .metric-card {
    background: rgba(255,255,255,0.95);
    border-radius: 12px;
    padding: 20px;
    margin: 10px;
    box-shadow: 0 4px 15px rgba(0,0,0,0.1);
    text-align: center;
    transition: all 0.3s ease;
  }
  .metric-card:hover {
    transform: scale(1.05);
    box-shadow: 0 6px 20px rgba(0,0,0,0.15);
  }
  .metric-value {
    font-size: 28px;
    font-weight: bold;
    background: linear-gradient(45deg, #667eea, #764ba2);
    -webkit-background-clip: text;
    -webkit-text-fill-color: transparent;
  }
  .metric-label {
    color: #7f8c8d;
    font-size: 13px;
    text-transform: uppercase;
    letter-spacing: 1px;
  }
  .feature-box {
    background: white;
    border-radius: 10px;
    padding: 20px;
    margin: 10px;
    box-shadow: 0 4px 12px rgba(0,0,0,0.08);
    border-left: 4px solid #3498db;
  }
  .algorithm-box {
    background: linear-gradient(135deg, #74b9ff 0%, #0984e3 100%);
    color: white;
    border-radius: 10px;
    padding: 20px;
    margin: 10px;
    box-shadow: 0 4px 12px rgba(116, 185, 255, 0.3);
  }
  .nav-tabs-custom > .nav-tabs > li.active {
    border-top-color: #667eea;
  }
  .btn-gradient {
    background: linear-gradient(45deg, #667eea, #764ba2);
    border: none;
    color: white;
    transition: all 0.3s ease;
  }
  .btn-gradient:hover {
    background: linear-gradient(45deg, #764ba2, #667eea);
    transform: translateY(-1px);
    box-shadow: 0 4px 12px rgba(102, 126, 234, 0.4);
  }
  .best-model-card {
    background: linear-gradient(135deg, #00b894 0%, #00a085 100%);
    color: white;
    border-radius: 15px;
    padding: 20px;
    margin: 10px;
    box-shadow: 0 8px 25px rgba(0, 184, 148, 0.3);
    text-align: center;
  }
"

ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(
    title = tags$div(
      tags$img(src = "https://img.icons8.com/fluency/48/artificial-intelligence.png", height = "35px"),
      "ImUnifiedML",
      style = "font-family: 'Arial Black', sans-serif; font-size: 20px; color: white;"
    ),
    titleWidth = 300
  ),
  
  dashboardSidebar(
    width = 280,
    sidebarMenu(
      id = "tabs",
      menuItem("🏠 Dashboard", tabName = "dashboard", icon = icon("home")),
      menuItem("📊 Data Management", tabName = "data", icon = icon("database")),
      menuItem("📍 Data Exploration", tabName = "explore", icon = icon("search-plus")),
      menuItem("🎯 Problem Type", tabName = "problem", icon = icon("crosshairs")),
      menuItem("🤖 Model Training", tabName = "training", icon = icon("cogs")),
      menuItem("📊 Results & Analysis", tabName = "results", icon = icon("chart-line")),
      menuItem("🔮 Make Predictions", tabName = "predictions", icon = icon("magic")),
      menuItem("💥 Export Results", tabName = "export", icon = icon("download")),
      menuItem("ℹ️ About", tabName = "about", icon = icon("info-circle"))
    ),
    
    div(
      style = "padding: 15px; background: linear-gradient(135deg, #667eea 0%, #764ba2 100%); margin: 10px; border-radius: 12px; color: white;",
      h4("📂 Data Source", style = "color: white; margin-bottom: 15px;"),
      radioGroupButtons(
        inputId = "dataSource",
        label = NULL,
        choices = c("📊 Demo Data" = "demo", "📤 Upload CSV" = "upload"),
        selected = "demo",
        status = "primary",
        size = "sm",
        width = "100%"
      ),
      
      conditionalPanel(
        condition = "input.dataSource == 'demo'",
        selectInput("demoData", "Select Dataset:",
                    choices = c("Iris (Classification)" = "iris",
                                "Boston Housing (Regression)" = "boston"),
                    selected = "iris")
      ),
      
      conditionalPanel(
        condition = "input.dataSource == 'upload'",
        fileInput("file", 
                  label = NULL,
                  accept = ".csv",
                  buttonLabel = "Browse CSV...",
                  placeholder = "No file selected"),
        checkboxInput("header", "Header", TRUE),
        selectInput("sep", "Separator:",
                    choices = c("Comma" = ",", "Semicolon" = ";", "Tab" = "\t"),
                    selected = ",")
      ),
      
      conditionalPanel(
        condition = "output.dataLoaded",
        selectInput("targetVar", "🎯 Target Variable:", choices = NULL),
        pickerInput("predictors", "📊 Select Predictors:", 
                    choices = NULL, multiple = TRUE,
                    options = list(`actions-box` = TRUE, 
                                   `selected-text-format` = "count > 3")),
        br(),
        actionBttn("loadData", 
                   "🚀 Load Data",
                   style = "gradient",
                   color = "success",
                   size = "sm",
                   block = TRUE)
      )
    )
  ),
  
  dashboardBody(
    tags$head(tags$style(HTML(custom_css))),
    
    tabItems(
      # Dashboard Tab
      tabItem(tabName = "dashboard",
              fluidRow(
                box(
                  title = NULL, status = "primary", solidHeader = FALSE, width = 12,
                  div(class = "about-section",
                      h1("🚀 ImUnifiedML: Unifying classification & regression in one platform", 
                         style = "margin-top: 0; text-align: center; font-size: 28px;"),
                      p("Advanced unified platform for both Classification and Regression problems with state-of-the-art algorithms, comprehensive visualizations, and detailed performance analysis.",
                        style = "text-align: center; font-size: 16px; margin-bottom: 25px;"),
                      div(style = "display: flex; justify-content: space-around; flex-wrap: wrap;",
                          div(class = "metric-card",
                              div(class = "metric-value", "10+"),
                              div(class = "metric-label", "ML Algorithms")
                          ),
                          div(class = "metric-card",
                              div(class = "metric-value", "Both"),
                              div(class = "metric-label", "Classification & Regression")
                          ),
                          div(class = "metric-card",
                              div(class = "metric-value", "K-Fold"),
                              div(class = "metric-label", "Cross Validation")
                          ),
                          div(class = "metric-card",
                              div(class = "metric-value", "Interactive"),
                              div(class = "metric-label", "Visualizations")
                          )
                      )
                  )
                )
              ),
              
              # Best Model Display Section
              conditionalPanel(
                condition = "output.modelsRun",
                fluidRow(
                  box(
                    title = "🏆 Best Model Summary", status = "success", solidHeader = TRUE, width = 12,
                    div(id = "bestModelDashboard", style = "min-height: 100px;"),
                    uiOutput("bestModelDisplay")
                  )
                )
              ),
              
              fluidRow(
                box(
                  title = "🎯 Key Features", status = "info", solidHeader = TRUE, width = 6,
                  div(class = "feature-box",
                      h4("📊 Dual Problem Support", style = "color: #2c3e50; margin-bottom: 10px;"),
                      tags$ul(
                        tags$li("Classification: Predict categories/classes"),
                        tags$li("Regression: Predict continuous values"),
                        tags$li("Automatic problem type detection"),
                        tags$li("Optimized algorithms for each type")
                      )
                  ),
                  div(class = "feature-box",
                      h4("🔧 Advanced Analytics", style = "color: #2c3e50; margin-bottom: 10px;"),
                      tags$ul(
                        tags$li("Interactive hyperparameter tuning"),
                        tags$li("Cross-validation with multiple folds"),
                        tags$li("Feature importance analysis"),
                        tags$li("Model performance comparison")
                      )
                  )
                ),
                box(
                  title = "🤖 Supported Algorithms", status = "success", solidHeader = TRUE, width = 6,
                  div(class = "algorithm-box",
                      h4("📈 Regression Models", style = "margin-bottom: 15px;"),
                      tags$ul(
                        tags$li("🌲 Random Forest Regression"),
                        tags$li("⚡ XGBoost Regression"),
                        tags$li("📘 Support Vector Regression"),
                        tags$li("🧠 Neural Network Regression")
                      )
                  ),
                  div(class = "algorithm-box", 
                      style = "background: linear-gradient(135deg, #fd79a8 0%, #e84393 100%);",
                      h4("🎯 Classification Models", style = "margin-bottom: 15px;"),
                      tags$ul(
                        tags$li("🌳 Classification Trees (CART)"),
                        tags$li("🎯 K-Nearest Neighbors (KNN)"),
                        tags$li("📘 Support Vector Machine (SVM)"),
                        tags$li("🌲 Random Forest Classification"),
                        tags$li("⚡ Gradient Boosting (GBM)")
                      )
                  )
                )
              ),
              
              fluidRow(
                box(
                  title = "🏆 Quick Start Guide", status = "warning", solidHeader = TRUE, width = 12,
                  div(style = "display: grid; grid-template-columns: repeat(auto-fit, minmax(250px, 1fr)); gap: 20px; padding: 10px;",
                      div(style = "text-align: center; padding: 20px; border: 2px dashed #f39c12; border-radius: 10px;",
                          h4("1️⃣ Load Data", style = "color: #f39c12;"),
                          p("Choose demo dataset or upload your CSV file")
                      ),
                      div(style = "text-align: center; padding: 20px; border: 2px dashed #3498db; border-radius: 10px;",
                          h4("2️⃣ Select Variables", style = "color: #3498db;"),
                          p("Choose target variable and predictors")
                      ),
                      div(style = "text-align: center; padding: 20px; border: 2px dashed #27ae60; border-radius: 10px;",
                          h4("3️⃣ Set Problem Type", style = "color: #27ae60;"),
                          p("Classification or Regression")
                      ),
                      div(style = "text-align: center; padding: 20px; border: 2px dashed #e74c3c; border-radius: 10px;",
                          h4("4️⃣ Train Models", style = "color: #e74c3c;"),
                          p("Configure and train multiple algorithms")
                      )
                  )
                )
              )
      ),
      
      # Data Management Tab
      tabItem(tabName = "data",
              fluidRow(
                box(
                  title = "📊 Dataset Overview", status = "primary", solidHeader = TRUE, width = 12,
                  withSpinner(dataTableOutput("dataTable"), type = 4, color = "#667eea")
                )
              ),
              fluidRow(
                box(
                  title = "📈 Data Summary", status = "info", solidHeader = TRUE, width = 6,
                  withSpinner(verbatimTextOutput("dataSummary"), type = 4, color = "#17a2b8")
                ),
                box(
                  title = "🔍 Data Structure", status = "success", solidHeader = TRUE, width = 6,
                  withSpinner(verbatimTextOutput("dataStructure"), type = 4, color = "#28a745")
                )
              )
      ),
      
      # Data Exploration Tab
      tabItem(tabName = "explore",
              fluidRow(
                box(
                  title = "🎨 Visualization Controls", status = "primary", solidHeader = TRUE, width = 3,
                  selectInput("plotType", "📊 Select Plot Type:",
                              choices = c("Correlation Matrix" = "correlation",
                                          "Pairs Plot" = "pairs",
                                          "Distribution Plot" = "distribution",
                                          "Box Plot" = "boxplot"),
                              selected = "correlation"),
                  
                  conditionalPanel(
                    condition = "(input.plotType == 'distribution' || input.plotType == 'boxplot')",
                    selectInput("plotVar", "Select Variable:", choices = NULL)
                  ),
                  actionBttn("generatePlot", "🎨 Generate Plot", 
                             style = "gradient",
                             color = "primary",
                             size = "sm",
                             block = TRUE)
                ),
                box(
                  title = "📈 Interactive Visualization", status = "info", solidHeader = TRUE, width = 9,
                  withSpinner(
                    conditionalPanel(
                      condition = "input.plotType == 'correlation'",
                      plotlyOutput("correlationPlot", height = "600px")
                    ),
                    type = 4, color = "#17a2b8"
                  ),
                  withSpinner(
                    conditionalPanel(
                      condition = "input.plotType == 'pairs'",
                      plotOutput("pairsPlot", height = "600px")
                    ),
                    type = 4, color = "#17a2b8"
                  ),
                  withSpinner(
                    conditionalPanel(
                      condition = "input.plotType == 'distribution'",
                      plotlyOutput("distributionPlot", height = "600px")
                    ),
                    type = 4, color = "#17a2b8"
                  ),
                  withSpinner(
                    conditionalPanel(
                      condition = "input.plotType == 'boxplot'",
                      plotlyOutput("boxPlot", height = "600px")
                    ),
                    type = 4, color = "#17a2b8"
                  )
                )
              )
      ),
      
      # Problem Type Tab
      tabItem(tabName = "problem",
              fluidRow(
                box(
                  title = "🎯 Machine Learning Problem Configuration", status = "primary", solidHeader = TRUE, width = 12,
                  div(style = "text-align: center; padding: 30px;",
                      h3("Select Your Machine Learning Task", style = "color: #2c3e50; margin-bottom: 30px;"),
                      div(style = "display: flex; justify-content: space-around; margin: 30px 0;",
                          div(style = "text-align: center; padding: 30px; border: 3px solid #3498db; border-radius: 15px; width: 40%; background: linear-gradient(135deg, #74b9ff 0%, #0984e3 100%); color: white;",
                              h4("📊 Classification", style = "margin-bottom: 20px;"),
                              p("Predict categories, classes, or discrete labels"),
                              tags$ul(style = "text-align: left;",
                                      tags$li("Species prediction"),
                                      tags$li("Quality ratings"),
                                      tags$li("Binary outcomes")
                              ),
                              br(),
                              actionBttn("selectClassification", "Choose Classification", 
                                         style = "gradient", color = "primary", size = "lg")
                          ),
                          div(style = "text-align: center; padding: 30px; border: 3px solid #27ae60; border-radius: 15px; width: 40%; background: linear-gradient(135deg, #00b894 0%, #00a085 100%); color: white;",
                              h4("📈 Regression", style = "margin-bottom: 20px;"),
                              p("Predict continuous numerical values"),
                              tags$ul(style = "text-align: left;",
                                      tags$li("Price prediction"),
                                      tags$li("Yield forecasting"),
                                      tags$li("Performance scores")
                              ),
                              br(),
                              actionBttn("selectRegression", "Choose Regression", 
                                         style = "gradient", color = "success", size = "lg")
                          )
                      ),
                      conditionalPanel(
                        condition = "output.problemSelected",
                        div(style = "margin-top: 30px; padding: 20px; background: #f8f9fa; border-radius: 10px;",
                            h4("✅ Problem Type Selected"),
                            textOutput("selectedProblemType"),
                            br(),
                            actionBttn("proceedToTraining", "🚀 Proceed to Training", 
                                       style = "gradient", color = "warning", size = "lg")
                        )
                      )
                  )
                )
              )
      ),
      
      # Training Tab
      tabItem(tabName = "training",
              fluidRow(
                box(
                  title = "⚙️ Training Configuration", status = "primary", solidHeader = TRUE, width = 4,
                  conditionalPanel(
                    condition = "output.problemSelected",
                    sliderInput("trainSplit", "Training Data %:", 
                                value = 80, min = 60, max = 90, step = 5),
                    sliderInput("cvFolds", "Cross-Validation Folds:", 
                                value = 10, min = 3, max = 15, step = 1),
                    sliderInput("cvRepeats", "CV Repeats:", 
                                value = 3, min = 1, max = 5, step = 1),
                    hr(),
                    h4("🔧 Hyperparameters"),
                    conditionalPanel(
                      condition = "output.isClassification",
                      h5("🌲 Random Forest"),
                      sliderInput("clf_rf_ntree", "Trees:", min = 100, max = 1000, value = 500, step = 100),
                      sliderInput("clf_rf_mtry", "Variables per Split:", min = 1, max = 10, value = 3),
                      h5("🎯 KNN"),
                      sliderInput("clf_knn_k", "Neighbors:", min = 1, max = 20, value = 5, step = 2),
                      h5("📘 SVM"),
                      selectInput("clf_svm_kernel", "Kernel:", 
                                  choices = c("radial", "polynomial", "linear"), selected = "radial"),
                      sliderInput("clf_svm_cost", "Cost:", min = 0.1, max = 10, value = 1, step = 0.1)
                    ),
                    conditionalPanel(
                      condition = "output.isRegression",
                      h5("🌲 Random Forest"),
                      sliderInput("reg_rf_ntree", "Trees:", min = 100, max = 1000, value = 500, step = 100),
                      sliderInput("reg_rf_mtry", "Variables per Split:", min = 1, max = 10, value = 3),
                      h5("⚡ XGBoost"),
                      sliderInput("reg_xgb_nrounds", "Rounds:", min = 50, max = 500, value = 100, step = 25),
                      sliderInput("reg_xgb_eta", "Learning Rate:", min = 0.01, max = 0.3, value = 0.1, step = 0.01),
                      h5("📘 SVM"),
                      sliderInput("reg_svm_cost", "Cost:", min = 0.1, max = 10, value = 1, step = 0.1),
                      h5("🧠 Neural Network"),
                      textInput("reg_ann_layers", "Hidden Layers:", value = "5,3"),
                      sliderInput("reg_ann_decay", "Decay:", min = 0, max = 0.1, value = 0.01, step = 0.001)
                    ),
                    hr(),
                    actionBttn("trainAllModels", "🚀 Train All Models", 
                               style = "gradient", color = "success", size = "lg", block = TRUE)
                  )
                ),
                box(
                  title = "🏆 Model Performance", status = "info", solidHeader = TRUE, width = 8,
                  conditionalPanel(
                    condition = "output.modelsRun",
                    withSpinner(plotlyOutput("performanceComparison", height = "500px"), 
                                type = 4, color = "#17a2b8")
                  )
                )
              ),
              fluidRow(
                conditionalPanel(
                  condition = "output.modelsRun",
                  box(
                    title = "📊 Detailed Results", status = "success", solidHeader = TRUE, width = 12,
                    withSpinner(dataTableOutput("modelResults"), type = 4, color = "#28a745")
                  )
                )
              )
      ),
      
      # Results Tab
      tabItem(tabName = "results",
              fluidRow(
                conditionalPanel(
                  condition = "output.modelsRun",
                  box(
                    title = "🎯 Best Model Analysis", status = "primary", solidHeader = TRUE, width = 4,
                    withSpinner(verbatimTextOutput("bestModelSummary"), type = 4, color = "#007bff")
                  ),
                  box(
                    title = "📈 Feature Importance", status = "success", solidHeader = TRUE, width = 8,
                    selectInput("featureModel", "Select Model:", choices = NULL),
                    withSpinner(plotlyOutput("featureImportancePlot", height = "400px"), 
                                type = 4, color = "#28a745")
                  )
                )
              ),
              fluidRow(
                conditionalPanel(
                  condition = "output.modelsRun && output.isClassification",
                  box(
                    title = "📉 Confusion Matrix", status = "warning", solidHeader = TRUE, width = 6,
                    selectInput("confusionModel", "Select Model:", choices = NULL),
                    withSpinner(plotlyOutput("confusionMatrix", height = "400px"), 
                                type = 4, color = "#ffc107")
                  )
                ),
                conditionalPanel(
                  condition = "output.isRegression",
                  box(
                    title = "📉 Residual Analysis", status = "warning", solidHeader = TRUE, width = 6,
                    selectInput("predictionModel", "Select Model:", choices = NULL),
                    tabsetPanel(
                      tabPanel("Residual vs Fitted", plotlyOutput("residualVsFittedPlot", height = "400px")),
                      tabPanel("Residual Histogram", plotlyOutput("residualHistogramPlot", height = "400px")),
                      tabPanel("Q-Q Plot", plotlyOutput("qqPlot", height = "400px")),
                      tabPanel("Actual vs Predicted", plotlyOutput("actualVsPredictedPlot", height = "400px"))
                    )
                  )
                ),
                conditionalPanel(
                  condition = "output.modelsRun",
                  box(
                    title = "📊 Model Comparison Radar", status = "info", solidHeader = TRUE, width = 6,
                    withSpinner(plotOutput("radarPlot", height = "400px"), type = 4, color = "#17a2b8")
                  )
                )
              )
      ),
      
      # Predictions Tab
      tabItem(tabName = "predictions",
              fluidRow(
                conditionalPanel(
                  condition = "output.modelsRun",
                  box(
                    title = "🔮 Make Predictions", status = "primary", solidHeader = TRUE, width = 12,
                    selectInput("predModel", "Select Model:", choices = NULL),
                    uiOutput("predInputs"),
                    br(),
                    actionBttn("makePrediction", "🔮 Predict", 
                               style = "gradient", color = "primary", size = "lg", block = TRUE),
                    br(),
                    br(),
                    withSpinner(verbatimTextOutput("predictionOutput"), type = 4, color = "#007bff")
                  )
                )
              )
      ),
      
      # Export Tab
      tabItem(tabName = "export",
              fluidRow(
                box(
                  title = "💥 Export Options", status = "primary", solidHeader = TRUE, width = 12,
                  conditionalPanel(
                    condition = "output.modelsRun",
                    div(style = "display: grid; grid-template-columns: repeat(auto-fit, minmax(300px, 1fr)); gap: 20px; padding: 20px;",
                        div(style = "text-align: center; padding: 25px; border: 3px dashed #3498db; border-radius: 15px; background: linear-gradient(135deg, #74b9ff 0%, #0984e3 100%); color: white;",
                            h4("📊 Model Results"),
                            p("Download comprehensive performance metrics"),
                            downloadBttn("downloadResults", "Download CSV", 
                                         style = "bordered", color = "primary", size = "lg")
                        ),
                        div(style = "text-align: center; padding: 25px; border: 3px dashed #27ae60; border-radius: 15px; background: linear-gradient(135deg, #00b894 0%, #00a085 100%); color: white;",
                            h4("🎯 Feature Importance"),
                            selectInput("exportFeatureModel", "Select Model:", choices = NULL),
                            downloadBttn("downloadFeatures", "Download CSV", 
                                         style = "bordered", color = "success", size = "lg")
                        ),
                        conditionalPanel(
                          condition = "output.modelsRun",
                          div(style = "text-align: center; padding: 25px; border: 3px dashed #f39c12; border-radius: 15px; background: linear-gradient(135deg, #fdcb6e 0%, #e17055 100%); color: white;",
                              h4("📈 Predictions"),
                              selectInput("exportPredModel", "Select Model:", choices = NULL),
                              downloadBttn("downloadPredictions", "Download CSV", 
                                           style = "bordered", color = "warning", size = "lg")
                          )
                        ),
                        div(style = "text-align: center; padding: 25px; border: 3px dashed #e74c3c; border-radius: 15px; background: linear-gradient(135deg, #fd79a8 0%, #e84393 100%); color: white;",
                            h4("📋 Complete Report"),
                            p("Comprehensive analysis report"),
                            downloadBttn("downloadReport", "Download PDF", 
                                         style = "bordered", color = "danger", size = "lg")
                        )
                    )
                  )
                )
              )
      ),
      
      # About Tab
      tabItem(tabName = "about",
              fluidRow(
                box(
                  title = "🌟 About ImUnifiedML", status = "info", solidHeader = TRUE, width = 12,
                  div(style = "padding: 25px; font-size: 16px; line-height: 1.8;",
                      div(class = "about-section",
                          h2("🚀 ImUnifiedML: The Ultimate ML Platform", style = "margin-top: 0; text-align: center;"),
                          p("A comprehensive, interactive machine learning platform that seamlessly handles both Classification and Regression tasks. Built with cutting-edge algorithms and beautiful visualizations to make machine learning accessible and powerful.", style = "text-align: center; font-size: 18px;")
                      ),
                      fluidRow(
                        column(6,
                               h3("🎯 Key Features", style = "color: #27ae60; margin-top: 25px;"),
                               div(class = "feature-box",
                                   tags$ul(
                                     tags$li("💥 Dual Support: Both Classification and Regression"),
                                     tags$li("🤖 10+ State-of-the-Art ML Algorithms"),
                                     tags$li("🎨 Interactive Visualizations with Plotly"),
                                     tags$li("⚙️ Advanced Hyperparameter Tuning"),
                                     tags$li("📊 Cross-Validation with Multiple Folds"),
                                     tags$li("📈 Comprehensive Performance Metrics"),
                                     tags$li("🎯 Feature Importance Analysis"),
                                     tags$li("📱 Responsive Design for All Devices"),
                                     tags$li("💥 Export Results in Multiple Formats"),
                                     tags$li("🏆 Automated Best Model Recommendation")
                                   )
                               )
                        ),
                        column(6,
                               h3("💡 How to Use", style = "color: #e74c3c; margin-top: 25px;"),
                               div(class = "feature-box",
                                   tags$ol(
                                     tags$li("📍 Load your dataset (demo or upload CSV)"),
                                     tags$li("🎯 Select target variable and predictors"),
                                     tags$li("⚙️ Choose Classification or Regression"),
                                     tags$li("🔧 Configure hyperparameters as needed"),
                                     tags$li("🚀 Train multiple models simultaneously"),
                                     tags$li("📊 Compare performance with interactive charts"),
                                     tags$li("🎯 Analyze feature importance and results"),
                                     tags$li("💥 Export results and comprehensive reports")
                                   )
                               )
                        )
                      ),
                      div(
                        style = "background: linear-gradient(135deg, #667eea 0%, #764ba2 100%); padding: 25px; border-radius: 15px; margin-top: 30px; color: white; text-align: center;",
                        h3("👨‍💻 Development Team", style = "color: white; margin-bottom: 20px;"),
                        p(strong("Lead Developers:"), " M.Iqbal Jeelani & Sheikh Mansoor"),
                        p(strong("Contributing Team:"), " Imran Khan, Nageena Nazir, Fehim Jeelani, Immad Shah, T. A.Raja, M.S Pukhta, Mansha Gul, Shreedhar"),
                        p(strong("Primary Contact:"), " jeelani.miqbal@gmail.com"),
                        p(strong("Institution:"), " SKUAST-Kashmir (India)"),
                        p(strong("Version:"), " 3.0.0 (Unified Edition)"),
                        p(strong("Last Updated:"), " January 2026"),
                        div(style = "margin-top: 25px;",
                            actionBttn("contactTeam", "📧 Contact Team", 
                                       style = "bordered", color = "primary", size = "md"),
                            span(style = "margin: 0 15px;"),
                            actionBttn("visitYouTube", "📺 YouTube Channel", 
                                       style = "bordered", color = "primary", size = "md"),
                            span(style = "margin: 0 15px;"),
                            actionBttn("viewSource", "💻 View Source", 
                                       style = "bordered", color = "primary", size = "md")
                        )
                      ),
                      div(
                        style = "text-align: center; margin-top: 30px; padding: 20px; background: #ecf0f1; border-radius: 12px;",
                        p("🌱 Empowering Research & Industry with AI-Driven Analytics", 
                          style = "font-style: italic; color: #7f8c8d; font-size: 18px; margin: 0;")
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
  
  # Reactive Values
  values <- reactiveValues(
    data = NULL,
    problem_type = NULL,
    models = NULL,
    results = NULL,
    target_var = NULL,
    predictors = NULL,
    train_data = NULL,
    test_data = NULL,
    scaled_data = NULL,
    feature_importance_list = NULL,
    train_index = NULL
  )
  
  # Safe data loading with error handling
  loadDemoData <- function(dataset) {
    tryCatch({
      switch(dataset,
             "iris" = {
               data(iris)
               iris
             },
             "boston" = {
               if (requireNamespace("MASS", quietly = TRUE)) {
                 data(Boston, package = "MASS")
                 Boston
               } else {
                 showNotification("MASS package not available. Using synthetic Boston Housing data.", type = "warning")
                 set.seed(123)
                 n <- 506
                 data.frame(
                   medv = rnorm(n, 22, 9),
                   crim = runif(n, 0, 90),
                   zn = sample(c(0, 25, 50, 75, 100), n, replace = TRUE),
                   indus = runif(n, 0, 28),
                   nox = runif(n, 0.3, 0.9),
                   rm = rnorm(n, 6, 1),
                   age = runif(n, 0, 100),
                   dis = runif(n, 1, 12),
                   rad = sample(1:24, n, replace = TRUE),
                   tax = sample(200:800, n, replace = TRUE),
                   ptratio = runif(n, 12, 22),
                   lstat = runif(n, 2, 38)
                 )
               }
             },
             stop("Invalid dataset selected"))
    }, error = function(e) {
      showNotification(paste("Error loading demo data:", e$message), type = "error")
      NULL
    })
  }
  
  # Data Loading Observer with enhanced error handling
  observeEvent(c(input$dataSource, input$demoData, input$file), {
    if (input$dataSource == "demo") {
      values$data <- loadDemoData(input$demoData)
    } else if (input$dataSource == "upload" && !is.null(input$file)) {
      tryCatch({
        # Enhanced CSV reading with better error handling
        temp_data <- read.csv(input$file$datapath, 
                              header = input$header, 
                              sep = input$sep, 
                              check.names = TRUE,
                              stringsAsFactors = FALSE,
                              na.strings = c("", "NA", "N/A", "null", "NULL"))
        
        if (nrow(temp_data) == 0) {
          stop("Uploaded CSV is empty.")
        }
        if (ncol(temp_data) < 2) {
          stop("CSV must have at least 2 columns.")
        }
        
        # Clean column names
        names(temp_data) <- make.names(names(temp_data), unique = TRUE)
        
        # Remove completely empty rows and columns
        temp_data <- temp_data[rowSums(is.na(temp_data)) != ncol(temp_data), ]
        temp_data <- temp_data[, colSums(is.na(temp_data)) != nrow(temp_data)]
        
        if (nrow(temp_data) == 0 || ncol(temp_data) < 2) {
          stop("No valid data remaining after cleaning.")
        }
        
        values$data <- temp_data
        showNotification("Data uploaded successfully!", type = "message")
        
      }, error = function(e) {
        showNotification(paste("Error loading CSV:", e$message), type = "error")
        values$data <- NULL
      })
    }
    
    # Update UI choices after successful data loading
    if (!is.null(values$data) && nrow(values$data) > 0 && ncol(values$data) >= 2) {
      all_cols <- names(values$data)
      numeric_vars <- names(values$data)[sapply(values$data, is.numeric)]
      
      updateSelectInput(session, "targetVar", 
                        choices = all_cols, 
                        selected = all_cols[length(all_cols)])
      updatePickerInput(session, "predictors", 
                        choices = all_cols, 
                        selected = all_cols[-length(all_cols)])
      updateSelectInput(session, "plotVar", 
                        choices = if (length(numeric_vars) > 0) numeric_vars else all_cols, 
                        selected = if (length(numeric_vars) > 0) numeric_vars[1] else all_cols[1])
    } else {
      updateSelectInput(session, "targetVar", choices = character(0))
      updatePickerInput(session, "predictors", choices = character(0))
      updateSelectInput(session, "plotVar", choices = character(0))
    }
  })
  
  # Data Loading Status
  output$dataLoaded <- reactive({
    !is.null(values$data) && nrow(values$data) > 0 && ncol(values$data) > 1
  })
  outputOptions(output, 'dataLoaded', suspendWhenHidden = FALSE)
  
  # Load Data Button Observer
  observeEvent(input$loadData, {
    req(values$data, input$targetVar, input$predictors)
    
    if (!(input$targetVar %in% names(values$data))) {
      showNotification("Invalid target variable selected.", type = "error")
      return()
    }
    if (length(input$predictors) < 1 || !all(input$predictors %in% names(values$data))) {
      showNotification("Invalid predictors selected.", type = "error")
      return()
    }
    
    values$target_var <- input$targetVar
    values$predictors <- input$predictors
    showNotification("Data configured successfully!", type = "message")
  })
  
  # Data Display Outputs
  output$dataTable <- renderDataTable({
    req(values$data)
    datatable(values$data, 
              options = list(
                pageLength = 10,
                scrollX = TRUE,
                dom = 'Bfrtip',
                buttons = c('copy', 'csv', 'excel')
              ),
              class = 'cell-border stripe hover') %>%
      formatStyle(columns = names(values$data), fontSize = '12px')
  })
  
  output$dataSummary <- renderPrint({
    req(values$data)
    summary(values$data)
  })
  
  output$dataStructure <- renderPrint({
    req(values$data)
    str(values$data)
  })
  
  # Problem Type Observers
  observeEvent(input$selectClassification, {
    values$problem_type <- "classification"
    showNotification("Classification problem selected!", type = "message")
  })
  
  observeEvent(input$selectRegression, {
    values$problem_type <- "regression"
    showNotification("Regression problem selected!", type = "message")
  })
  
  # Problem Type Status
  output$problemSelected <- reactive({
    !is.null(values$problem_type)
  })
  outputOptions(output, 'problemSelected', suspendWhenHidden = FALSE)
  
  output$isClassification <- reactive({
    identical(values$problem_type, "classification")
  })
  outputOptions(output, 'isClassification', suspendWhenHidden = FALSE)
  
  output$isRegression <- reactive({
    identical(values$problem_type, "regression")
  })
  outputOptions(output, 'isRegression', suspendWhenHidden = FALSE)
  
  output$selectedProblemType <- renderText({
    req(values$problem_type)
    if (values$problem_type == "classification") {
      "🎯 Classification: Predicting categories or classes"
    } else {
      "📈 Regression: Predicting continuous numerical values"
    }
  })
  
  # FIXED: Correlation Plot (now works for both classification and regression)
  output$correlationPlot <- renderPlotly({
    req(values$data)
    
    # Get only numeric columns
    numeric_data <- values$data[sapply(values$data, function(x) is.numeric(x) && !all(is.na(x)))]
    
    if (ncol(numeric_data) < 2) {
      p <- ggplot() + 
        annotate("text", x = 0.5, y = 0.5, 
                 label = "Not enough numeric variables\nfor correlation analysis", 
                 size = 5, hjust = 0.5, vjust = 0.5) +
        theme_void() + xlim(0, 1) + ylim(0, 1)
      return(ggplotly(p))
    }
    
    # Remove rows with all NAs
    numeric_data <- numeric_data[complete.cases(numeric_data), ]
    
    if (nrow(numeric_data) == 0) {
      p <- ggplot() + 
        annotate("text", x = 0.5, y = 0.5, 
                 label = "No complete cases available\nfor correlation analysis", 
                 size = 5, hjust = 0.5, vjust = 0.5) +
        theme_void() + xlim(0, 1) + ylim(0, 1)
      return(ggplotly(p))
    }
    
    tryCatch({
      cor_matrix <- cor(numeric_data, use = "complete.obs")
      cor_melted <- reshape2::melt(cor_matrix)
      
      p <- ggplot(cor_melted, aes(x = Var1, y = Var2, fill = value)) +
        geom_tile() +
        scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                             midpoint = 0, limit = c(-1,1), space = "Lab", 
                             name="Correlation") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
        labs(title = "Correlation Matrix", x = "", y = "") +
        coord_fixed()
      
      ggplotly(p)
    }, error = function(e) {
      p <- ggplot() + 
        annotate("text", x = 0.5, y = 0.5, 
                 label = paste("Error creating correlation plot:", e$message), 
                 size = 4, hjust = 0.5, vjust = 0.5) +
        theme_void() + xlim(0, 1) + ylim(0, 1)
      ggplotly(p)
    })
  })
  
  output$pairsPlot <- renderPlot({
    req(values$data, values$target_var)
    
    tryCatch({
      # Get numeric columns
      numeric_cols <- sapply(values$data, is.numeric)
      plot_data <- values$data[, numeric_cols, drop = FALSE]
      
      if (ncol(plot_data) < 2) {
        return(ggplot() + 
                 annotate("text", x = 0.5, y = 0.5, 
                          label = "Not enough numeric variables for pairs plot", 
                          size = 5) +
                 theme_void() + xlim(0, 1) + ylim(0, 1))
      }
      
      # Add target variable for coloring if it exists
      if (values$target_var %in% names(values$data)) {
        plot_data[[values$target_var]] <- values$data[[values$target_var]]
      }
      
      # Limit to first 5 variables for performance
      if (ncol(plot_data) > 6) {
        plot_data <- plot_data[, c(1:5, ncol(plot_data))]
      }
      
      # Remove incomplete cases
      plot_data <- plot_data[complete.cases(plot_data), ]
      
      if (nrow(plot_data) == 0) {
        return(ggplot() + 
                 annotate("text", x = 0.5, y = 0.5, 
                          label = "No complete cases for pairs plot", 
                          size = 5) +
                 theme_void() + xlim(0, 1) + ylim(0, 1))
      }
      
      if (values$target_var %in% names(plot_data)) {
        GGally::ggpairs(plot_data, 
                        mapping = aes(colour = .data[[values$target_var]]),
                        lower = list(continuous = wrap("smooth", alpha = 0.3, size = 0.5)),
                        upper = list(continuous = wrap("cor", size = 3)),
                        progress = FALSE) +
          theme_minimal() +
          theme(strip.text = element_text(size = 8),
                axis.text = element_text(size = 6))
      } else {
        GGally::ggpairs(plot_data, 
                        lower = list(continuous = wrap("smooth", alpha = 0.3, size = 0.5)),
                        upper = list(continuous = wrap("cor", size = 3)),
                        progress = FALSE) +
          theme_minimal() +
          theme(strip.text = element_text(size = 8),
                axis.text = element_text(size = 6))
      }
    }, error = function(e) {
      ggplot() + 
        annotate("text", x = 0.5, y = 0.5, 
                 label = paste("Error creating pairs plot:", e$message), 
                 size = 4) +
        theme_void() + xlim(0, 1) + ylim(0, 1)
    })
  })
  
  output$distributionPlot <- renderPlotly({
    req(values$data, input$plotVar, values$target_var)
    
    if (input$plotVar %in% names(values$data) && values$target_var %in% names(values$data)) {
      tryCatch({
        plot_data <- values$data[!is.na(values$data[[input$plotVar]]) & !is.na(values$data[[values$target_var]]), ]
        
        p <- ggplot(plot_data, aes_string(x = input$plotVar, fill = values$target_var)) +
          geom_histogram(alpha = 0.7, position = "identity", bins = 30) +
          scale_fill_viridis_d() +
          theme_minimal() +
          labs(title = paste("Distribution of", input$plotVar), x = input$plotVar, y = "Count")
        
        ggplotly(p)
      }, error = function(e) {
        p <- ggplot() + 
          annotate("text", x = 0.5, y = 0.5, 
                   label = paste("Error creating distribution plot:", e$message), 
                   size = 4) +
          theme_void() + xlim(0, 1) + ylim(0, 1)
        ggplotly(p)
      })
    }
  })
  
  output$boxPlot <- renderPlotly({
    req(values$data, input$plotVar, values$target_var)
    
    if (input$plotVar %in% names(values$data) && values$target_var %in% names(values$data)) {
      tryCatch({
        plot_data <- values$data[!is.na(values$data[[input$plotVar]]) & !is.na(values$data[[values$target_var]]), ]
        
        p <- ggplot(plot_data, aes_string(x = values$target_var, y = input$plotVar, fill = values$target_var)) +
          geom_boxplot(alpha = 0.7) +
          scale_fill_viridis_d() +
          theme_minimal() +
          labs(title = paste("Box Plot of", input$plotVar), x = values$target_var, y = input$plotVar) +
          theme(axis.text.x = element_text(angle = 45, hjust = 1))
        
        ggplotly(p)
      }, error = function(e) {
        p <- ggplot() + 
          annotate("text", x = 0.5, y = 0.5, 
                   label = paste("Error creating box plot:", e$message), 
                   size = 4) +
          theme_void() + xlim(0, 1) + ylim(0, 1)
        ggplotly(p)
      })
    }
  })
  
  # FIXED: Enhanced Feature Importance Function
  getFeatureImportance <- function(model, model_name, predictors, problem_type, train_data = NULL, test_data = NULL) {
    imp_df <- NULL
    
    tryCatch({
      if (model_name == "rf") {
        if (problem_type == "classification") {
          # For classification RF from caret
          if ("finalModel" %in% names(model) && !is.null(model$finalModel)) {
            imp_data <- randomForest::importance(model$finalModel)
            if ("MeanDecreaseGini" %in% colnames(imp_data)) {
              imp_df <- data.frame(Feature = rownames(imp_data), 
                                   Importance = imp_data[, "MeanDecreaseGini"])
            } else if (ncol(imp_data) >= 1) {
              imp_df <- data.frame(Feature = rownames(imp_data), 
                                   Importance = imp_data[, 1])
            }
          }
        } else {
          # For regression RF (direct randomForest object)
          if (!is.null(model$importance)) {
            imp_data <- model$importance
            if ("%IncMSE" %in% colnames(imp_data)) {
              imp_df <- data.frame(Feature = rownames(imp_data), 
                                   Importance = imp_data[, "%IncMSE"])
            } else if (ncol(imp_data) >= 1) {
              imp_df <- data.frame(Feature = rownames(imp_data), 
                                   Importance = imp_data[, 1])
            }
          }
        }
        
      } else if (model_name == "gbm" && problem_type == "classification") {
        if ("finalModel" %in% names(model) && !is.null(model$finalModel)) {
          imp <- summary(model$finalModel, plotit = FALSE)
          imp_df <- data.frame(Feature = imp$var, Importance = imp$rel.inf)
        }
        
      } else if (model_name == "xgb" && problem_type == "regression") {
        if (!is.null(model)) {
          imp <- xgb.importance(feature_names = predictors, model = model)
          if (nrow(imp) > 0) {
            imp_df <- data.frame(Feature = imp$Feature, Importance = imp$Gain)
          }
        }
        
      } else if (model_name == "cart" && problem_type == "classification") {
        if ("finalModel" %in% names(model) && !is.null(model$finalModel$variable.importance)) {
          imp_data <- model$finalModel$variable.importance
          imp_df <- data.frame(Feature = names(imp_data), Importance = imp_data)
        }
        
      } else if (model_name == "svm") {
        # FIXED: SVM Feature Importance using permutation method
        if (!is.null(train_data) && !is.null(test_data) && !is.null(model)) {
          if (problem_type == "classification") {
            base_pred <- predict(model, test_data[, predictors, drop = FALSE])
            if (is.factor(test_data[[names(test_data)[1]]])) {
              base_accuracy <- mean(base_pred == test_data[[names(test_data)[1]]], na.rm = TRUE)
              
              importance_scores <- sapply(predictors, function(var) {
                temp_test <- test_data
                temp_test[[var]] <- sample(temp_test[[var]])
                perm_pred <- predict(model, temp_test[, predictors, drop = FALSE])
                perm_accuracy <- mean(perm_pred == test_data[[names(test_data)[1]]], na.rm = TRUE)
                base_accuracy - perm_accuracy
              })
              
              imp_df <- data.frame(Feature = predictors, 
                                   Importance = pmax(importance_scores, 0))
            }
          } else {
            # Regression SVM
            base_pred <- predict(model, test_data[, predictors, drop = FALSE])
            if (is.numeric(test_data[[names(test_data)[1]]])) {
              base_rmse <- sqrt(mean((base_pred - test_data[[names(test_data)[1]]])^2, na.rm = TRUE))
              
              importance_scores <- sapply(predictors, function(var) {
                temp_test <- test_data
                temp_test[[var]] <- sample(temp_test[[var]])
                perm_pred <- predict(model, temp_test[, predictors, drop = FALSE])
                perm_rmse <- sqrt(mean((perm_pred - test_data[[names(test_data)[1]]])^2, na.rm = TRUE))
                perm_rmse - base_rmse
              })
              
              imp_df <- data.frame(Feature = predictors, 
                                   Importance = pmax(importance_scores, 0))
            }
          }
        }
        
      } else if (model_name == "knn" && problem_type == "classification") {
        # KNN feature importance using permutation approach
        if (!is.null(train_data) && !is.null(test_data) && !is.null(model)) {
          base_pred <- predict(model, test_data[, predictors, drop = FALSE])
          if (is.factor(test_data[[names(test_data)[1]]])) {
            base_accuracy <- mean(base_pred == test_data[[names(test_data)[1]]], na.rm = TRUE)
            
            importance_scores <- sapply(predictors, function(var) {
              temp_test <- test_data
              temp_test[[var]] <- sample(temp_test[[var]])
              perm_pred <- predict(model, temp_test[, predictors, drop = FALSE])
              perm_accuracy <- mean(perm_pred == test_data[[names(test_data)[1]]], na.rm = TRUE)
              base_accuracy - perm_accuracy
            })
            
            imp_df <- data.frame(Feature = predictors, 
                                 Importance = pmax(importance_scores, 0))
          }
        }
        
      } else if (model_name == "ann" && problem_type == "regression") {
        # FIXED: Neural Network Feature Importance
        if (!is.null(model) && !is.null(model$wts) && length(model$wts) > 0) {
          n_inputs <- length(predictors)
          if (!is.null(model$n) && length(model$n) >= 2) {
            n_hidden <- model$n[2]  # Number of hidden units
            
            if (length(model$wts) >= n_inputs * n_hidden) {
              # Extract input-to-hidden weights
              input_weights <- matrix(model$wts[1:(n_inputs * n_hidden)], 
                                      nrow = n_inputs, ncol = n_hidden)
              
              # Calculate feature importance as sum of absolute weights
              feature_importance <- rowSums(abs(input_weights))
              
              imp_df <- data.frame(Feature = predictors, 
                                   Importance = feature_importance)
            }
          }
        }
        
        # If direct method fails, use permutation importance
        if (is.null(imp_df) && !is.null(train_data) && !is.null(test_data)) {
          scaled_test <- scale(test_data[, predictors, drop = FALSE], 
                               center = attr(values$scaled_data, "scaled:center"),
                               scale = attr(values$scaled_data, "scaled:scale"))
          
          base_pred <- predict(model, scaled_test)
          base_rmse <- sqrt(mean((base_pred - test_data[[names(test_data)[1]]])^2, na.rm = TRUE))
          
          importance_scores <- sapply(predictors, function(var) {
            temp_test <- scaled_test
            temp_test[, var] <- sample(temp_test[, var])
            perm_pred <- predict(model, temp_test)
            perm_rmse <- sqrt(mean((perm_pred - test_data[[names(test_data)[1]]])^2, na.rm = TRUE))
            perm_rmse - base_rmse
          })
          
          imp_df <- data.frame(Feature = predictors, 
                               Importance = pmax(importance_scores, 0))
        }
      }
      
      # Clean and sort results
      if (!is.null(imp_df) && nrow(imp_df) > 0) {
        # Remove NAs and infinite values
        imp_df <- imp_df[is.finite(imp_df$Importance) & !is.na(imp_df$Importance), ]
        
        if (nrow(imp_df) > 0) {
          # Sort by importance
          imp_df <- imp_df[order(imp_df$Importance, decreasing = TRUE), ]
          
          # Limit to top 15 features
          if (nrow(imp_df) > 15) imp_df <- imp_df[1:15, ]
          
          # Ensure positive values
          imp_df$Importance <- pmax(imp_df$Importance, 0)
        }
      }
      
      return(imp_df)
      
    }, error = function(e) {
      return(NULL)
    })
  }
  
  # FIXED: Model Training Observer with better error handling
  observeEvent(input$trainAllModels, {
    req(values$data, values$target_var, values$predictors, values$problem_type)
    
    withProgress(message = 'Training Models...', value = 0, {
      
      # FIXED: Better data preparation with error handling
      tryCatch({
        # Prepare model data
        selected_cols <- c(values$target_var, values$predictors)
        model_data <- values$data[, selected_cols, drop = FALSE]
        
        # Remove rows with missing values in selected columns
        model_data <- model_data[complete.cases(model_data), ]
        
        if (nrow(model_data) < 10) {
          showNotification("Not enough complete cases (minimum 10 required)!", type = "error")
          return()
        }
        
        # Ensure correct data types
        if (values$problem_type == "classification") {
          model_data[[values$target_var]] <- as.factor(as.character(model_data[[values$target_var]]))
          
          # Check if we have at least 2 classes
          if (nlevels(model_data[[values$target_var]]) < 2) {
            showNotification("Target variable must have at least 2 levels for classification!", type = "error")
            return()
          }
          
          # Check if classes have enough samples
          class_counts <- table(model_data[[values$target_var]])
          if (any(class_counts < 2)) {
            showNotification("Each class must have at least 2 samples!", type = "error")
            return()
          }
        } else {
          model_data[[values$target_var]] <- as.numeric(as.character(model_data[[values$target_var]]))
          
          if (any(is.na(model_data[[values$target_var]]))) {
            showNotification("Target variable contains non-numeric values for regression!", type = "error")
            return()
          }
        }
        
        # Ensure predictors are numeric where needed
        for (pred in values$predictors) {
          if (!is.numeric(model_data[[pred]]) && !is.factor(model_data[[pred]])) {
            model_data[[pred]] <- as.numeric(as.character(model_data[[pred]]))
          }
        }
        
      }, error = function(e) {
        showNotification(paste("Error preparing data:", e$message), type = "error")
        return()
      })
      
      # FIXED: Create stratified train/test split
      set.seed(123)
      tryCatch({
        if (values$problem_type == "classification") {
          train_index <- createDataPartition(model_data[[values$target_var]], 
                                             p = input$trainSplit/100, list = FALSE)
        } else {
          # For regression, use simple random sampling
          train_index <- sample(nrow(model_data), 
                                size = floor(input$trainSplit/100 * nrow(model_data)))
        }
        
        train_data <- model_data[train_index, , drop = FALSE]
        test_data <- model_data[-train_index, , drop = FALSE]
        
        # Store for later use
        values$train_data <- train_data
        values$test_data <- test_data
        values$train_index <- train_index
        
        # Check minimum sample sizes
        if (nrow(train_data) < 5 || nrow(test_data) < 2) {
          showNotification("Insufficient data for train/test split!", type = "error")
          return()
        }
        
      }, error = function(e) {
        showNotification(paste("Error creating train/test split:", e$message), type = "error")
        return()
      })
      
      # Prepare scaled data for neural networks (regression only)
      if (values$problem_type == "regression") {
        tryCatch({
          predictor_data <- model_data[, values$predictors, drop = FALSE]
          # Only scale numeric predictors
          numeric_predictors <- sapply(predictor_data, is.numeric)
          if (any(numeric_predictors)) {
            scaled_predictors <- scale(predictor_data[, numeric_predictors, drop = FALSE])
            if (all(numeric_predictors)) {
              values$scaled_data <- scaled_predictors
            } else {
              values$scaled_data <- predictor_data
              values$scaled_data[, numeric_predictors] <- scaled_predictors
              values$scaled_data <- as.matrix(values$scaled_data)
            }
          } else {
            values$scaled_data <- as.matrix(predictor_data)
          }
        }, error = function(e) {
          showNotification(paste("Error scaling data:", e$message), type = "warning")
          values$scaled_data <- as.matrix(model_data[, values$predictors, drop = FALSE])
        })
      }
      
      # Cross-validation control
      ctrl <- trainControl(method = "repeatedcv", 
                           number = input$cvFolds, 
                           repeats = input$cvRepeats,
                           verboseIter = FALSE,
                           allowParallel = FALSE)
      
      formula_obj <- as.formula(paste(values$target_var, "~ ."))
      models <- list()
      results_df <- data.frame()
      
      if (values$problem_type == "classification") {
        
        # CART
        incProgress(0.18, detail = "Training CART...")
        tryCatch({
          models$cart <- train(formula_obj, data = train_data, method = "rpart", 
                               trControl = ctrl, metric = "Accuracy")
        }, error = function(e) {
          showNotification(paste("Error training CART:", e$message), type = "warning")
        })
        
        # KNN
        incProgress(0.18, detail = "Training KNN...")
        tryCatch({
          models$knn <- train(formula_obj, data = train_data, method = "knn", 
                              trControl = ctrl, metric = "Accuracy",
                              tuneGrid = expand.grid(k = input$clf_knn_k),
                              preProcess = c("center", "scale"))
        }, error = function(e) {
          showNotification(paste("Error training KNN:", e$message), type = "warning")
        })
        
        # SVM
        incProgress(0.18, detail = "Training SVM...")
        tryCatch({
          if (input$clf_svm_kernel == "linear") {
            models$svm <- train(formula_obj, data = train_data, method = "svmLinear", 
                                trControl = ctrl, metric = "Accuracy",
                                tuneGrid = expand.grid(C = input$clf_svm_cost),
                                preProcess = c("center", "scale"))
          } else {
            models$svm <- train(formula_obj, data = train_data, method = "svmRadial", 
                                trControl = ctrl, metric = "Accuracy",
                                tuneGrid = expand.grid(sigma = 0.1, C = input$clf_svm_cost),
                                preProcess = c("center", "scale"))
          }
        }, error = function(e) {
          showNotification(paste("Error training SVM:", e$message), type = "warning")
        })
        
        # Random Forest
        incProgress(0.18, detail = "Training Random Forest...")
        tryCatch({
          models$rf <- train(formula_obj, data = train_data, method = "rf", 
                             trControl = ctrl, metric = "Accuracy",
                             tuneGrid = expand.grid(mtry = min(input$clf_rf_mtry, length(values$predictors))),
                             ntree = input$clf_rf_ntree,
                             importance = TRUE)
        }, error = function(e) {
          showNotification(paste("Error training Random Forest:", e$message), type = "warning")
        })
        
        # GBM
        incProgress(0.18, detail = "Training GBM...")
        tryCatch({
          models$gbm <- train(formula_obj, data = train_data, method = "gbm", 
                              trControl = ctrl, metric = "Accuracy", verbose = FALSE)
        }, error = function(e) {
          showNotification(paste("Error training GBM:", e$message), type = "warning")
        })
        
        # FIXED: Evaluate Classification Models with proper error handling
        for (model_name in names(models)) {
          if (!is.null(models[[model_name]])) {
            tryCatch({
              pred <- predict(models[[model_name]], test_data)
              actual <- test_data[[values$target_var]]
              
              # Ensure both pred and actual are factors with same levels
              all_levels <- union(levels(actual), levels(as.factor(pred)))
              pred <- factor(pred, levels = all_levels)
              actual <- factor(actual, levels = all_levels)
              
              cm <- confusionMatrix(pred, actual)
              
              # Handle multiclass metrics
              if (length(levels(actual)) > 2) {
                sensitivity_mean <- mean(cm$byClass[, "Sensitivity"], na.rm = TRUE)
                specificity_mean <- mean(cm$byClass[, "Specificity"], na.rm = TRUE)
              } else {
                sensitivity_mean <- cm$byClass["Sensitivity"]
                specificity_mean <- cm$byClass["Specificity"]
              }
              
              results_df <- rbind(results_df, data.frame(
                Model = toupper(model_name),
                Accuracy = as.numeric(cm$overall["Accuracy"]),
                Kappa = as.numeric(cm$overall["Kappa"]),
                Sensitivity = as.numeric(sensitivity_mean),
                Specificity = as.numeric(specificity_mean)
              ))
              
            }, error = function(e) {
              showNotification(paste("Error evaluating", model_name, ":", e$message), type = "warning")
            })
          }
        }
        
      } else { # Regression
        
        # Random Forest
        incProgress(0.2, detail = "Training Random Forest...")
        tryCatch({
          models$rf <- randomForest(formula_obj, data = train_data, 
                                    ntree = input$reg_rf_ntree,
                                    mtry = min(input$reg_rf_mtry, length(values$predictors)),
                                    importance = TRUE)
        }, error = function(e) {
          showNotification(paste("Error training Random Forest:", e$message), type = "warning")
        })
        
        # XGBoost
        incProgress(0.2, detail = "Training XGBoost...")
        tryCatch({
          # Prepare data for xgboost
          train_matrix <- model.matrix(formula_obj, data = train_data)[, -1]
          dtrain <- xgb.DMatrix(data = train_matrix, 
                                label = train_data[[values$target_var]])
          
          models$xgb <- xgboost(data = dtrain, 
                                nrounds = input$reg_xgb_nrounds,
                                eta = input$reg_xgb_eta,
                                objective = "reg:squarederror", 
                                verbose = 0)
        }, error = function(e) {
          showNotification(paste("Error training XGBoost:", e$message), type = "warning")
        })
        
        # SVM
        incProgress(0.2, detail = "Training SVM...")
        tryCatch({
          models$svm <- svm(formula_obj, data = train_data, 
                            cost = input$reg_svm_cost,
                            scale = TRUE)
        }, error = function(e) {
          showNotification(paste("Error training SVM:", e$message), type = "warning")
        })
        
        # Neural Network
        incProgress(0.2, detail = "Training Neural Network...")
        tryCatch({
          layers <- as.numeric(unlist(strsplit(input$reg_ann_layers, ",")))
          layers <- layers[!is.na(layers)]
          if (length(layers) == 0) layers <- c(5)
          
          # Prepare scaled training data
          train_scaled <- values$scaled_data[values$train_index, , drop = FALSE]
          
          models$ann <- nnet(train_scaled, train_data[[values$target_var]], 
                             size = layers[1], decay = input$reg_ann_decay,
                             linout = TRUE, trace = FALSE, maxit = 500)
        }, error = function(e) {
          showNotification(paste("Error training Neural Network:", e$message), type = "warning")
        })
        
        # FIXED: Evaluate Regression Models with proper error handling
        for (model_name in names(models)) {
          if (!is.null(models[[model_name]])) {
            tryCatch({
              if (model_name == "xgb") {
                test_matrix <- model.matrix(formula_obj, data = test_data)[, -1]
                dtest <- xgb.DMatrix(data = test_matrix)
                pred <- predict(models[[model_name]], dtest)
              } else if (model_name == "ann") {
                test_scaled <- values$scaled_data[-values$train_index, , drop = FALSE]
                pred <- predict(models[[model_name]], test_scaled)
                if (is.matrix(pred)) pred <- pred[, 1]
              } else {
                pred <- predict(models[[model_name]], test_data)
              }
              
              actual <- test_data[[values$target_var]]
              
              # Calculate metrics
              rmse <- sqrt(mean((pred - actual)^2, na.rm = TRUE))
              r2 <- 1 - sum((pred - actual)^2, na.rm = TRUE) / sum((actual - mean(actual))^2, na.rm = TRUE)
              mae <- mean(abs(pred - actual), na.rm = TRUE)
              
              # Handle edge cases
              if (!is.finite(rmse)) rmse <- NA
              if (!is.finite(r2)) r2 <- NA
              if (!is.finite(mae)) mae <- NA
              
              results_df <- rbind(results_df, data.frame(
                Model = toupper(model_name),
                RMSE = rmse,
                R2 = r2,
                MAE = mae
              ))
              
            }, error = function(e) {
              showNotification(paste("Error evaluating", model_name, ":", e$message), type = "warning")
            })
          }
        }
      }
      
      incProgress(0.1, detail = "Finalizing...")
      
      if (nrow(results_df) == 0) {
        showNotification("No models were successfully trained!", type = "error")
        return()
      }
      
      # Store results
      values$models <- models
      values$results <- results_df
      
      # Update UI choices
      successful_models <- names(models)[!sapply(models, is.null)]
      model_choices <- setNames(successful_models, toupper(successful_models))
      
      updateSelectInput(session, "featureModel", choices = model_choices, 
                        selected = if(length(model_choices) > 0) model_choices[1] else NULL)
      updateSelectInput(session, "confusionModel", choices = model_choices,
                        selected = if(length(model_choices) > 0) model_choices[1] else NULL)
      updateSelectInput(session, "predictionModel", choices = model_choices,
                        selected = if(length(model_choices) > 0) model_choices[1] else NULL)
      updateSelectInput(session, "exportFeatureModel", choices = model_choices,
                        selected = if(length(model_choices) > 0) model_choices[1] else NULL)
      updateSelectInput(session, "exportPredModel", choices = model_choices,
                        selected = if(length(model_choices) > 0) model_choices[1] else NULL)
      updateSelectInput(session, "predModel", choices = model_choices,
                        selected = if(length(model_choices) > 0) model_choices[1] else NULL)
      
      showNotification(paste("Successfully trained", length(successful_models), "models!"), type = "message")
    })
  })
  
  # Models Run Status
  output$modelsRun <- reactive({
    !is.null(values$models) && !is.null(values$results) && nrow(values$results) > 0
  })
  outputOptions(output, 'modelsRun', suspendWhenHidden = FALSE)
  
  # Best Model Display for Dashboard
  output$bestModelDisplay <- renderUI({
    req(values$results, values$problem_type)
    
    if (values$problem_type == "classification") {
      best_model <- values$results[which.max(values$results$Accuracy), ]
      best_metric <- "Accuracy"
      best_value <- round(best_model$Accuracy, 4)
    } else {
      best_model <- values$results[which.min(values$results$RMSE), ]
      best_metric <- "RMSE"
      best_value <- round(best_model$RMSE, 4)
    }
    
    div(class = "best-model-card",
        h3("🏆 Best Performing Model", style = "margin-top: 0; color: white;"),
        div(style = "display: flex; justify-content: space-around; margin: 20px 0;",
            div(style = "text-align: center;",
                h4(best_model$Model, style = "color: white; margin: 0;"),
                p("Model", style = "color: rgba(255,255,255,0.8); font-size: 12px; margin: 0;")
            ),
            div(style = "text-align: center;",
                h4(best_value, style = "color: white; margin: 0;"),
                p(best_metric, style = "color: rgba(255,255,255,0.8); font-size: 12px; margin: 0;")
            ),
            div(style = "text-align: center;",
                h4(if(values$problem_type == "classification") "📊" else "📈", style = "color: white; margin: 0; font-size: 2em;"),
                p(stringr::str_to_title(values$problem_type), style = "color: rgba(255,255,255,0.8); font-size: 12px; margin: 0;")
            )
        )
    )
  })
  
  # Performance Comparison Plot
  output$performanceComparison <- renderPlotly({
    req(values$results)
    
    results_long <- reshape2::melt(values$results, id.vars = "Model", variable.name = "Metric", value.name = "Value")
    
    if (values$problem_type == "classification") {
      title_text <- "🏆 Classification Model Performance"
    } else {
      title_text <- "🏆 Regression Model Performance"
    }
    
    p <- ggplot(results_long, aes(x = Model, y = Value, fill = Model)) +
      geom_col(alpha = 0.8) +
      facet_wrap(~Metric, scales = "free_y") +
      scale_fill_viridis_d() +
      theme_minimal() +
      labs(title = title_text, x = "Models", y = "Metric Value") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1), 
            legend.position = "none",
            plot.title = element_text(hjust = 0.5))
    
    ggplotly(p)
  })
  
  # Model Results Table
  output$modelResults <- renderDataTable({
    req(values$results)
    
    results_display <- values$results
    numeric_cols <- sapply(results_display, is.numeric)
    results_display[numeric_cols] <- lapply(results_display[numeric_cols], function(x) round(x, 4))
    
    datatable(results_display, 
              options = list(pageLength = 10, scrollX = TRUE),
              class = 'cell-border stripe hover') %>%
      formatStyle(columns = names(results_display)[-1], 
                  backgroundColor = styleColorBar(range(results_display[numeric_cols], na.rm = TRUE), 'lightblue'),
                  backgroundSize = '100% 90%',
                  backgroundRepeat = 'no-repeat',
                  backgroundPosition = 'center')
  })
  
  # Best Model Summary
  output$bestModelSummary <- renderPrint({
    req(values$results, values$problem_type)
    
    if (values$problem_type == "classification") {
      best_model <- values$results[which.max(values$results$Accuracy), ]
      cat("🏆 Best Model (Accuracy):", best_model$Model, "\n")
      cat("Accuracy:", round(best_model$Accuracy, 4), "\n")
      cat("Kappa:", round(best_model$Kappa, 4), "\n")
      if (!is.na(best_model$Sensitivity)) cat("Sensitivity:", round(best_model$Sensitivity, 4), "\n")
      if (!is.na(best_model$Specificity)) cat("Specificity:", round(best_model$Specificity, 4), "\n")
    } else {
      best_model <- values$results[which.min(values$results$RMSE), ]
      cat("🏆 Best Model (RMSE):", best_model$Model, "\n")
      cat("RMSE:", round(best_model$RMSE, 4), "\n")
      cat("R²:", round(best_model$R2, 4), "\n")
      cat("MAE:", round(best_model$MAE, 4), "\n")
    }
  })
  
  # FIXED: Feature Importance Plot
  output$featureImportancePlot <- renderPlotly({
    req(values$models, input$featureModel, values$predictors)
    
    model <- values$models[[input$featureModel]]
    
    if (!is.null(model)) {
      imp_df <- getFeatureImportance(model, input$featureModel, values$predictors, 
                                     values$problem_type, values$train_data, values$test_data)
      
      if (!is.null(imp_df) && nrow(imp_df) > 0) {
        # Sort and limit to top 10
        imp_df <- imp_df[order(imp_df$Importance, decreasing = TRUE), ]
        if (nrow(imp_df) > 10) imp_df <- imp_df[1:10, ]
        
        p <- ggplot(imp_df, aes(x = reorder(Feature, Importance), y = Importance)) +
          geom_col(fill = "steelblue", alpha = 0.8) +
          coord_flip() +
          theme_minimal() +
          labs(title = paste("Feature Importance -", toupper(input$featureModel)),
               x = "Features", y = "Importance Score") +
          theme(plot.title = element_text(hjust = 0.5))
        
        ggplotly(p)
      } else {
        # Create empty plot with message
        p <- ggplot() + 
          annotate("text", x = 0.5, y = 0.5, 
                   label = "Feature importance not available\nfor this model or failed to calculate", 
                   size = 5, hjust = 0.5, vjust = 0.5) +
          theme_void() +
          xlim(0, 1) + ylim(0, 1)
        
        ggplotly(p)
      }
    } else {
      p <- ggplot() + 
        annotate("text", x = 0.5, y = 0.5, 
                 label = "Model not available", 
                 size = 5, hjust = 0.5, vjust = 0.5) +
        theme_void() +
        xlim(0, 1) + ylim(0, 1)
      
      ggplotly(p)
    }
  })
  
  # Confusion Matrix
  output$confusionMatrix <- renderPlotly({
    req(values$models, input$confusionModel, values$problem_type == "classification")
    
    model <- values$models[[input$confusionModel]]
    
    if (!is.null(model)) {
      tryCatch({
        pred <- predict(model, values$test_data)
        actual <- values$test_data[[values$target_var]]
        
        # Ensure both have same levels
        all_levels <- union(levels(actual), levels(as.factor(pred)))
        pred <- factor(pred, levels = all_levels)
        actual <- factor(actual, levels = all_levels)
        
        cm <- table(Predicted = pred, Actual = actual)
        cm_df <- as.data.frame.matrix(cm)
        cm_df$Predicted <- rownames(cm_df)
        cm_melt <- reshape2::melt(cm_df, id.vars = "Predicted", variable.name = "Actual", value.name = "Count")
        
        p <- plot_ly(data = cm_melt, x = ~Actual, y = ~Predicted, z = ~Count, 
                     type = "heatmap", colorscale = "Blues",
                     text = ~Count, texttemplate = "%{text}", textfont = list(color = "white")) %>%
          layout(title = paste("Confusion Matrix -", toupper(input$confusionModel)))
        
        p
      }, error = function(e) {
        plot_ly() %>% 
          add_annotations(text = paste("Error generating confusion matrix:", e$message),
                          x = 0.5, y = 0.5, showarrow = FALSE)
      })
    }
  })
  
  # Residual vs Fitted Plot
  output$residualVsFittedPlot <- renderPlotly({
    req(values$models, input$predictionModel, values$problem_type == "regression")
    
    model <- values$models[[input$predictionModel]]
    
    if (!is.null(model)) {
      tryCatch({
        if (input$predictionModel == "xgb") {
          test_matrix <- model.matrix(as.formula(paste(values$target_var, "~ .")), data = values$test_data)[, -1]
          dtest <- xgb.DMatrix(data = test_matrix)
          pred <- predict(model, dtest)
        } else if (input$predictionModel == "ann") {
          test_scaled <- values$scaled_data[-values$train_index, , drop = FALSE]
          pred <- predict(model, test_scaled)
          if (is.matrix(pred)) pred <- pred[, 1]
        } else {
          pred <- predict(model, values$test_data)
        }
        
        actual <- values$test_data[[values$target_var]]
        
        plot_data <- data.frame(Fitted = pred, Residual = actual - pred)
        
        p <- ggplot(plot_data, aes(x = Fitted, y = Residual)) +
          geom_point(alpha = 0.6, color = "blue") +
          geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
          theme_minimal() +
          labs(title = "Residual vs Fitted", x = "Fitted Values", y = "Residuals")
        
        ggplotly(p)
      }, error = function(e) {
        ggplotly(ggplot() + annotate("text", x = 0.5, y = 0.5, label = paste("Error:", e$message)) + theme_void())
      })
    }
  })
  
  # Residual Histogram Plot
  output$residualHistogramPlot <- renderPlotly({
    req(values$models, input$predictionModel, values$problem_type == "regression")
    
    model <- values$models[[input$predictionModel]]
    
    if (!is.null(model)) {
      tryCatch({
        if (input$predictionModel == "xgb") {
          test_matrix <- model.matrix(as.formula(paste(values$target_var, "~ .")), data = values$test_data)[, -1]
          dtest <- xgb.DMatrix(data = test_matrix)
          pred <- predict(model, dtest)
        } else if (input$predictionModel == "ann") {
          test_scaled <- values$scaled_data[-values$train_index, , drop = FALSE]
          pred <- predict(model, test_scaled)
          if (is.matrix(pred)) pred <- pred[, 1]
        } else {
          pred <- predict(model, values$test_data)
        }
        
        actual <- values$test_data[[values$target_var]]
        
        residual <- actual - pred
        
        p <- ggplot(data.frame(Residual = residual), aes(x = Residual)) +
          geom_histogram(bins = 30, fill = "blue", alpha = 0.6) +
          theme_minimal() +
          labs(title = "Residual Histogram", x = "Residuals", y = "Frequency")
        
        ggplotly(p)
      }, error = function(e) {
        ggplotly(ggplot() + annotate("text", x = 0.5, y = 0.5, label = paste("Error:", e$message)) + theme_void())
      })
    }
  })
  
  # Q-Q Plot
  output$qqPlot <- renderPlotly({
    req(values$models, input$predictionModel, values$problem_type == "regression")
    
    model <- values$models[[input$predictionModel]]
    
    if (!is.null(model)) {
      tryCatch({
        if (input$predictionModel == "xgb") {
          test_matrix <- model.matrix(as.formula(paste(values$target_var, "~ .")), data = values$test_data)[, -1]
          dtest <- xgb.DMatrix(data = test_matrix)
          pred <- predict(model, dtest)
        } else if (input$predictionModel == "ann") {
          test_scaled <- values$scaled_data[-values$train_index, , drop = FALSE]
          pred <- predict(model, test_scaled)
          if (is.matrix(pred)) pred <- pred[, 1]
        } else {
          pred <- predict(model, values$test_data)
        }
        
        actual <- values$test_data[[values$target_var]]
        
        residual <- actual - pred
        
        p <- ggplot(data.frame(sample = residual), aes(sample = sample)) +
          stat_qq() +
          stat_qq_line(color = "red") +
          theme_minimal() +
          labs(title = "Q-Q Plot of Residuals")
        
        ggplotly(p)
      }, error = function(e) {
        ggplotly(ggplot() + annotate("text", x = 0.5, y = 0.5, label = paste("Error:", e$message)) + theme_void())
      })
    }
  })
  
  # Actual vs Predicted Plot
  output$actualVsPredictedPlot <- renderPlotly({
    req(values$models, input$predictionModel, values$problem_type == "regression")
    
    model <- values$models[[input$predictionModel]]
    
    if (!is.null(model)) {
      tryCatch({
        if (input$predictionModel == "xgb") {
          test_matrix <- model.matrix(as.formula(paste(values$target_var, "~ .")), data = values$test_data)[, -1]
          dtest <- xgb.DMatrix(data = test_matrix)
          pred <- predict(model, dtest)
        } else if (input$predictionModel == "ann") {
          test_scaled <- values$scaled_data[-values$train_index, , drop = FALSE]
          pred <- predict(model, test_scaled)
          if (is.matrix(pred)) pred <- pred[, 1]
        } else {
          pred <- predict(model, values$test_data)
        }
        
        actual <- values$test_data[[values$target_var]]
        
        plot_data <- data.frame(Actual = actual, Predicted = pred)
        
        p <- ggplot(plot_data, aes(x = Actual, y = Predicted)) +
          geom_point(alpha = 0.6, color = "blue") +
          geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
          theme_minimal() +
          labs(title = "Actual vs Predicted", x = "Actual Values", y = "Predicted Values")
        
        ggplotly(p)
      }, error = function(e) {
        ggplotly(ggplot() + annotate("text", x = 0.5, y = 0.5, label = paste("Error:", e$message)) + theme_void())
      })
    }
  })
  
  # Prediction Inputs UI
  output$predInputs <- renderUI({
    req(values$predictors)
    
    lapply(values$predictors, function(pred) {
      numericInput(inputId = paste0("pred_", pred), label = pred, value = 0)
    })
  })
  
  # Make Prediction
  observeEvent(input$makePrediction, {
    req(values$models, input$predModel, values$problem_type)
    
    model <- values$models[[input$predModel]]
    
    if (!is.null(model)) {
      tryCatch({
        new_data <- data.frame(t(sapply(values$predictors, function(pred) {
          input[[paste0("pred_", pred)]]
        })))
        names(new_data) <- values$predictors
        
        if (values$problem_type == "regression" && input$predModel == "ann") {
          new_data_scaled <- scale(new_data, 
                                   center = attr(values$scaled_data, "scaled:center"),
                                   scale = attr(values$scaled_data, "scaled:scale"))
          pred <- predict(model, new_data_scaled)
        } else {
          pred <- predict(model, new_data)
        }
        
        if (values$problem_type == "classification") {
          prob <- predict(model, new_data, type = "prob")
          output$predictionOutput <- renderPrint({
            cat("Predicted Class:", as.character(pred), "\n")
            cat("Probabilities:\n")
            print(prob)
          })
        } else {
          output$predictionOutput <- renderPrint({
            cat("Predicted Value:", pred, "\n")
          })
        }
      }, error = function(e) {
        output$predictionOutput <- renderPrint({
          cat("Error making prediction:", e$message)
        })
      })
    }
  })
  
  # Radar Plot
  output$radarPlot <- renderPlot({
    req(values$results)
    
    if (values$problem_type == "classification") {
      # Normalize metrics for radar plot
      results_norm <- values$results
      numeric_cols <- sapply(results_norm, is.numeric)
      results_norm[numeric_cols] <- lapply(results_norm[numeric_cols], function(x) {
        if (all(is.na(x))) return(x)
        range_x <- range(x, na.rm = TRUE)
        if (range_x[2] == range_x[1]) return(rep(0.5, length(x)))
        (x - range_x[1]) / (range_x[2] - range_x[1])
      })
      
      radar_data <- reshape2::melt(results_norm, id.vars = "Model", variable.name = "Metric", value.name = "Score")
      
      ggplot(radar_data, aes(x = Metric, y = Score, group = Model, color = Model)) +
        geom_polygon(aes(fill = Model), alpha = 0.2) +
        geom_point(size = 3) +
        geom_line(size = 1) +
        coord_polar() +
        scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
        scale_color_viridis_d() +
        scale_fill_viridis_d() +
        theme_minimal() +
        labs(title = "Classification Model Performance Radar") +
        theme(legend.position = "bottom",
              plot.title = element_text(hjust = 0.5))
      
    } else {
      # For regression, invert RMSE and MAE for radar plot
      results_norm <- values$results
      
      # Normalize RMSE (lower is better, so invert)
      rmse_range <- range(results_norm$RMSE, na.rm = TRUE)
      if (rmse_range[2] > rmse_range[1]) {
        results_norm$RMSE <- 1 - (results_norm$RMSE - rmse_range[1]) / (rmse_range[2] - rmse_range[1])
      } else {
        results_norm$RMSE <- rep(0.5, nrow(results_norm))
      }
      
      # Normalize MAE (lower is better, so invert)
      mae_range <- range(results_norm$MAE, na.rm = TRUE)
      if (mae_range[2] > mae_range[1]) {
        results_norm$MAE <- 1 - (results_norm$MAE - mae_range[1]) / (mae_range[2] - mae_range[1])
      } else {
        results_norm$MAE <- rep(0.5, nrow(results_norm))
      }
      
      # Normalize R2 (higher is better)
      r2_range <- range(results_norm$R2, na.rm = TRUE)
      if (r2_range[2] > r2_range[1]) {
        results_norm$R2 <- (results_norm$R2 - r2_range[1]) / (r2_range[2] - r2_range[1])
      } else {
        results_norm$R2 <- rep(0.5, nrow(results_norm))
      }
      
      radar_data <- reshape2::melt(results_norm, id.vars = "Model", variable.name = "Metric", value.name = "Score")
      
      ggplot(radar_data, aes(x = Metric, y = Score, group = Model, color = Model)) +
        geom_polygon(aes(fill = Model), alpha = 0.2) +
        geom_point(size = 3) +
        geom_line(size = 1) +
        coord_polar() +
        scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
        scale_color_viridis_d() +
        scale_fill_viridis_d() +
        theme_minimal() +
        labs(title = "Regression Model Performance Radar") +
        theme(legend.position = "bottom",
              plot.title = element_text(hjust = 0.5))
    }
  })
  
  # Download Handlers
  output$downloadResults <- downloadHandler(
    filename = function() {
      paste("model_results_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(values$results, file, row.names = FALSE)
    }
  )
  
  output$downloadFeatures <- downloadHandler(
    filename = function() {
      paste("feature_importance_", input$exportFeatureModel, "_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      req(values$models, input$exportFeatureModel)
      
      model <- values$models[[input$exportFeatureModel]]
      imp_df <- getFeatureImportance(model, input$exportFeatureModel, values$predictors, 
                                     values$problem_type, values$train_data, values$test_data)
      
      if (!is.null(imp_df) && nrow(imp_df) > 0) {
        write.csv(imp_df, file, row.names = FALSE)
      } else {
        # Create empty file with message
        write.csv(data.frame(Message = "Feature importance not available for this model"), file, row.names = FALSE)
      }
    }
  )
  
  output$downloadPredictions <- downloadHandler(
    filename = function() {
      paste("predictions_", input$exportPredModel, "_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      req(values$models, input$exportPredModel, values$problem_type == "regression")
      
      model <- values$models[[input$exportPredModel]]
      
      tryCatch({
        if (input$exportPredModel == "xgb") {
          test_matrix <- model.matrix(as.formula(paste(values$target_var, "~ .")), data = values$test_data)[, -1]
          dtest <- xgb.DMatrix(data = test_matrix)
          pred <- predict(model, dtest)
        } else if (input$exportPredModel == "ann") {
          test_scaled <- values$scaled_data[-values$train_index, , drop = FALSE]
          pred <- predict(model, test_scaled)
          if (is.matrix(pred)) pred <- pred[, 1]
        } else {
          pred <- predict(model, values$test_data)
        }
        
        actual <- values$test_data[[values$target_var]]
        pred_df <- data.frame(
          Actual = actual, 
          Predicted = pred,
          Residual = actual - pred,
          Model = toupper(input$exportPredModel)
        )
        write.csv(pred_df, file, row.names = FALSE)
      }, error = function(e) {
        write.csv(data.frame(Error = paste("Error generating predictions:", e$message)), file, row.names = FALSE)
      })
    }
  )
  
  output$downloadReport <- downloadHandler(
    filename = function() {
      paste("ml_analysis_report_", Sys.Date(), ".html", sep = "")
    },
    content = function(file) {
      req(values$results, values$problem_type)
      
      # Create HTML report
      report_content <- paste0('
<!DOCTYPE html>
<html>
<head>
    <title>ImMLUnified Analysis Report</title>
    <style>
        body { font-family: Arial, sans-serif; margin: 40px; line-height: 1.6; }
        .header { background: linear-gradient(135deg, #667eea 0%, #764ba2 100%); color: white; padding: 30px; border-radius: 10px; text-align: center; }
        .section { margin: 30px 0; padding: 20px; border: 1px solid #ddd; border-radius: 8px; }
        table { width: 100%; border-collapse: collapse; margin: 20px 0; }
        th, td { padding: 12px; text-align: left; border-bottom: 1px solid #ddd; }
        th { background-color: #f2f2f2; }
        .best-model { background: linear-gradient(135deg, #00b894 0%, #00a085 100%); color: white; padding: 20px; border-radius: 10px; text-align: center; }
        .footer { text-align: center; margin-top: 50px; color: #666; font-size: 12px; }
    </style>
</head>
<body>
    <div class="header">
        <h1>🤖 ImMLUnified Analysis Report</h1>
        <h2>', if(values$problem_type == "classification") "Classification" else "Regression", ' Analysis</h2>
        <p>Generated on: ', Sys.Date(), '</p>
    </div>
    
    <div class="section">
        <h2>📊 Executive Summary</h2>
        <p>This report presents the results of a <strong>', if(values$problem_type == "classification") "Classification" else "Regression", '</strong> analysis using multiple machine learning algorithms. The analysis was performed using the ImMLUnified platform.</p>
        
        <h3>Dataset Information</h3>
        <ul>
            <li><strong>Target Variable:</strong> ', values$target_var, '</li>
            <li><strong>Number of Predictors:</strong> ', length(values$predictors), '</li>
            <li><strong>Training Split:</strong> ', input$trainSplit, '%</li>
            <li><strong>Cross-Validation:</strong> ', input$cvFolds, '-fold with ', input$cvRepeats, ' repeats</li>
        </ul>
    </div>')
      
      if (values$problem_type == "classification") {
        best_model <- values$results[which.max(values$results$Accuracy), ]
        report_content <- paste0(report_content, '
    <div class="best-model">
        <h2>🏆 Best Performing Model</h2>
        <h3>', best_model$Model, '</h3>
        <p>Accuracy: ', round(best_model$Accuracy, 4), ' | Kappa: ', round(best_model$Kappa, 4), '</p>
    </div>')
      } else {
        best_model <- values$results[which.min(values$results$RMSE), ]
        report_content <- paste0(report_content, '
    <div class="best-model">
        <h2>🏆 Best Performing Model</h2>
        <h3>', best_model$Model, '</h3>
        <p>RMSE: ', round(best_model$RMSE, 4), ' | R²: ', round(best_model$R2, 4), ' | MAE: ', round(best_model$MAE, 4), '</p>
    </div>')
      }
      
      # Add results table
      report_content <- paste0(report_content, '
    <div class="section">
        <h2>📈 Model Performance Results</h2>
        <table>')
      
      # Table header
      report_content <- paste0(report_content, '<tr>')
      for (col in names(values$results)) {
        report_content <- paste0(report_content, '<th>', col, '</th>')
      }
      report_content <- paste0(report_content, '</tr>')
      
      # Table rows
      for (i in 1:nrow(values$results)) {
        report_content <- paste0(report_content, '<tr>')
        for (j in 1:ncol(values$results)) {
          val <- values$results[i, j]
          if (is.numeric(val)) val <- round(val, 4)
          report_content <- paste0(report_content, '<td>', val, '</td>')
        }
        report_content <- paste0(report_content, '</tr>')
      }
      
      report_content <- paste0(report_content, '
        </table>
    </div>
    
    <div class="section">
        <h2>🎯 Conclusions</h2>
        <p>', if(values$problem_type == "classification") {
          'This classification analysis compared multiple machine learning algorithms to predict categorical outcomes. Models were evaluated using accuracy, kappa coefficient, sensitivity, and specificity metrics.'
        } else {
          'This regression analysis compared multiple machine learning algorithms to predict continuous values. Models were evaluated using RMSE, R², and MAE metrics.'
        }, '</p>
        
        <h3>Key Findings:</h3>
        <ul>
            <li>Total models trained: ', nrow(values$results), '</li>
            <li>Best performing algorithm: ', if(values$problem_type == "classification") {
              values$results[which.max(values$results$Accuracy), "Model"]
            } else {
              values$results[which.min(values$results$RMSE), "Model"]
            }, '</li>
            <li>Validation method: ', input$cvFolds, '-fold cross-validation with ', input$cvRepeats, ' repeats</li>
        </ul>
    </div>
    
    <div class="footer">
        <hr>
        <p><strong>Report generated by ImUnifiedML Platform v3.0.0</strong></p>
        <p>Developed by: M.Iqbal Jeelani & Sheikh Mansoor</p>
        <p>SKUAST-Kashmir (India) </p>
        <p>Contact: jeelani.miqbal@gmail.com</p>
    </div>
</body>
</html>')
      
      writeLines(report_content, file)
    }
  )
  
  # Contact Team Modal
  observeEvent(input$contactTeam, {
    showModal(modalDialog(
      title = "📧 Contact Development Team",
      div(style = "text-align: center; padding: 20px;",
          h4("Get in Touch with Our Team!", style = "color: #2c3e50; margin-bottom: 20px;"),
          p("For questions, collaborations, or technical support:"),
          br(),
          div(style = "background: #f8f9fa; padding: 15px; border-radius: 10px; margin: 10px;",
              h5("📧 Primary Contact"),
              p(strong("M.Iqbal Jeelani"), br(),
                "Email: jeelani.miqbal@gmail.com", br(),
                "SKUAST-Kashmir, India")
          ),
          div(style = "background: #f8f9fa; padding: 15px; border-radius: 10px; margin: 10px;",
              h5("🤝 Secondary Contact"),
              p(strong("Sheikh Mansoor"), br(),
                "Recep Tayyip Erdoğan University, Turkey")
          ),
          br(),
          p("We welcome feedback, suggestions, and collaboration opportunities!", 
            style = "font-style: italic; color: #7f8c8d;")
      ),
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
  })
  
  # YouTube Channel Modal
  observeEvent(input$visitYouTube, {
    showModal(modalDialog(
      title = "📺 YouTube Channel",
      div(style = "text-align: center; padding: 20px;",
          h4("Visit Our Educational Channel", style = "color: #2c3e50; margin-bottom: 20px;"),
          p("Subscribe to our channel for:"),
          tags$ul(style = "text-align: left; display: inline-block;",
                  tags$li("Machine Learning Tutorials"),
                  tags$li("R Programming Guides"),
                  tags$li("Data Science Applications"),
                  tags$li("Statistical Analysis Methods")
          ),
          br(), br(),
          div(style = "background: linear-gradient(45deg, #FF0000, #CC0000); color: white; padding: 15px; border-radius: 10px;",
              h5("🔗 Channel Information"),
              p("YouTube: https://www.youtube.com/@Iqbalstat", br(),
                "Subscribe for latest content!")
          ),
          br(),
          p("📺 New tutorials every week!", style = "font-style: italic; color: #7f8c8d;")
      ),
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
  })
  
  # View Source Modal
  observeEvent(input$viewSource, {
    showModal(modalDialog(
      title = "💻 Source Code Information",
      div(style = "text-align: center; padding: 20px;",
          h4("Open Source Project", style = "color: #2c3e50; margin-bottom: 20px;"),
          p("ImUnifiedML is developed as an educational and research tool."),
          br(),
          div(style = "background: #f8f9fa; padding: 15px; border-radius: 10px;",
              h5("📋 Project Details"),
              p("Platform: R Shiny", br(),
                "Source: Available upon request", br(),
                "Contact: jeelani.miqbal@gmail.com")
          ),
          br(),
          p("Contribute to our open-source initiative!", 
            style = "font-style: italic; color: #7f8c8d;")
      ),
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
  })
  
  # Navigate to Training Tab
  observeEvent(input$proceedToTraining, {
    updateTabItems(session, "tabs", "training")
  })
}

# Run the Application
shinyApp(ui, server)