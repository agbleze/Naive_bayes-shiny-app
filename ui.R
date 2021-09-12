#####  Naive bayers classification
## This is User interface of the app for presenting Naive Bayes classifier analysis

### load libraries
library(shiny)
shinyUI(fluidPage(

    # Application title
    titlePanel("Naive Bayer classification of German firms that update product line"),

    # Sidebar with a dynamically generated input controls
    sidebarLayout(
        sidebarPanel(
            uiOutput("select_predictor"),
            uiOutput("response_variable")
        ),

        # Main display with tabs to show results of analysis
        mainPanel(
            tabsetPanel(id = "tabs",
                tabPanel(title = "Dataset used", icon = icon("database"), 
                       #  fluidRow(
                       h4("German Enterprise data 2005"),
                            dataTableOutput("dataset") 
                         #)
                         ),
                
                #### Tabset to show splitting of response variable into training and test datasets
                tabPanel( title = "Response variable", icon = icon("chart-bar"),
                           fluidPage(title = "Upgrade existing product line",
                                     h4("Proportion of binary class in response variable (Upgrade existing product line)"),
                           fluidRow(
                               
                          plotlyOutput("upgradeproduct")
                          ),
                          
                          fluidRow(
                              plotlyOutput("training_dataset")
                          ),
                          
                          fluidRow(
                              plotlyOutput("test_dataset")
                          )
                          )
                          
                ),
                
                ### Tabpanel to display performance metrics for selected Naive model
                tabPanel(title = "Naive classifer best model", value = "model_predictors", icon = icon("chart-line"),
                    fluidPage(
                        h1("Model validation with testing dataset"),
                        h2("Performance Metrics"),
                        fluidRow(
                            valueBoxOutput("rmse"),
                            valueBoxOutput("gini")
                        ),
                        fluidRow(
                            h3("Confusion Matrix"),
                            tableOutput("confusionmatrix")
                        ),
                        fluidRow(
                            plotlyOutput("auc_perform")
                        )
                    )
                ),
                
                ## tabpanel for model prediction
                tabPanel(title = "Predict activity of firms with model", value = "pred", icon = icon("wave-square"),
                         fluidPage(
                             h3("Predict whether or not a firm has upgraded its existing product line"),
                             fluidRow(
                                 valueBoxOutput("actual"),
                                 valueBoxOutput("predicted"),
                                 valueBoxOutput("model_precision")
                             )
                         ))
            )   
            
        )
    )
))
