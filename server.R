## load libraries
library(shiny)

# Define server logic 

# predictors_selected <- reactive({
#     val <- input$variab
#     
#     transform_data%>%
#         select(val)
# })
shinyServer(function(input, output, session) {

output$dataset <- renderDataTable(
    transform_data
)

################## Plot binary classes of response variable in the dataset  #####################
output$upgradeproduct <- renderPlotly({
    
    levels(transform_data$upgrade_existingproduct_line)[1] <- "Upgraded existing product line"
    levels(transform_data$upgrade_existingproduct_line)[2] <- "No upgrade of existing product line"
    ggplot(data = transform_data, aes(upgrade_existingproduct_line)) + geom_bar() + 
        ggtitle("Data on firms activity of upgrading existing product line") +
        coord_flip() + xlab("")
}
)

###### Plot proportion of binary classes in training dataset ############
output$training_dataset <- renderPlotly({
    levels(train_german_ent_data$upgrade_existingproduct_line)[1] <- "Upgraded existing product line"
    levels(train_german_ent_data$upgrade_existingproduct_line)[2] <- "No upgrade of existing product line"
    ggplot(data = train_german_ent_data, aes(upgrade_existingproduct_line)) + geom_bar() + xlab("") +
        ggtitle("Random split of Training dataset (Proportion of Binary class") + coord_flip() 
        
})

###### Plot proportion of binary classes in testing dataset ############
output$test_dataset <- renderPlotly({
    levels(test_german_ent_data$upgrade_existingproduct_line)[1] <- "Upgraded existing product line"
    levels(test_german_ent_data$upgrade_existingproduct_line)[2] <- "No upgrade of existing product line"
    ggplot(data = test_german_ent_data, aes(upgrade_existingproduct_line)) + geom_bar() + xlab("") +
        ggtitle("Random split of Testing dataset (Proportion of Binary class)") + coord_flip()
})

###################### Display of various performance metrics  ###############################
## AUC 
output$auc_perform <- renderPlotly(
    data.frame(fpr = fpr_tested_model, tpr = tpr_tested_model) %>%
        ggplot(aes(fpr, tpr)) + geom_line() + ggtitle(sprintf("AUC: %f", h2o.auc(test_model)))
)

## Root Mean Squared Error (RMSE)
output$rmse <- renderValueBox(
    valueBox(
        value = paste0(comma(test_rmse, digits = 2)),
        subtitle = "Root Mean Squared Error (RMSE)"
    )
)

## Gini
output$gini <- renderValueBox(
    valueBox(
        value = paste0(comma(test_gini, digits = 2)),
        subtitle = "Gini"
    )
)

## Confusion matrix table 
output$confusionmatrix <- renderTable(rownames = TRUE, hover = TRUE,
    test_confusionMatrix
)

####### Different Dynamically generate input controls for different tabs
output$select_predictor <- renderUI({
    ## selectInput control for selecting company ID for prediction
    if(input$tabs == "pred"){
        selectInput("predic", label = "Select company id", choices = test_predict_join$rowid,
                    multiple = FALSE, selected = 1)
        ## Input control showing predictors used in the model
    } else if (input$tabs == "model_predictors"  ){
        varSelectizeInput("variab", "Predictors used for modelling", predictor_data, multiple = TRUE,
                        selected = c(x_h2o))
    }
})

## Input control showing response variable used
output$response_variable <- renderUI({
    if(input$tabs == "model_predictors"){
        selectInput(inputId = "resp", label= "Response variable used", choices = "Upgrade of existing productline",
                    multiple = FALSE, selected = "Upgrade of existing productline")
    }
})

########################## Display actual class value of a firm from the testing dataset
output$actual <- renderValueBox({
    val <- input$predic
    
    levels(test_predict_join$upgrade_existingproduct_line)[1] <- "Upgraded product line"
    levels(test_predict_join$upgrade_existingproduct_line)[2] <- "No upgrade of product line"
    levels(test_predict_join$predict)[1]<- "Upgraded product line"
    levels(test_predict_join$predict)[2]<- "No upgrade of product line"
    actul <- test_predict_join %>%
        filter(rowid == val) %>%
        dplyr::select(upgrade_existingproduct_line)

    valueBox(value = h4(actul), subtitle = "Actual value", color = "yellow")
})

############# Display predicted class of a firm in the test dataset with the naive bayers model
output$predicted <- renderValueBox({
    val <- input$predic
    
    predict_value <- test_predict_join %>%
        filter(rowid == val) %>%
        dplyr::select(predict)
    
    valueBox(value = h4(predict_value), subtitle = "Prediction", color = "red")
})

################# Indicates whether the model prediction is correct or wrong
output$model_precision <- renderValueBox({
    val <- input$predic
    
   condition_eval <-  test_predict_join %>%
        filter(rowid == val)
    
   model_status <-  if(condition_eval$upgrade_existingproduct_line == condition_eval$predict){
        print("Correct prediction")
    } else {
        print("Wrong prediction")
    }
   
   ## create different icons for correcting and wrong prediction
   model_icon<- if(model_status == "Correct prediction"){
       correct = icon("check-circle")
   } else if (model_status == "Wrong prediction"){
       wrong = icon("times-circle")
   } 
   
   valueBox(value = h4(model_status), subtitle = "Accuracy of Model", icon = model_icon)
})
})
