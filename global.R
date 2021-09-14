library(haven)
library(caret)
library(h2o)
library(tidyverse)
library(dplyr)
library(rsample)
library(DT)
library(formattable)
library(fontawesome)
library(shiny)
library(shinydashboard)
library(devtools)
library(ggplot2)
library(magrittr)
library(plotly)
library(purrr)
library(readr)
library(shinyWidgets)
library(tibble)
library(utils)

Germany_2005_full_data_3_6_dta <- read_stata("Germany-2005--full data-3,6.dta.zip")
german_enterprise_data <- data.frame(Germany_2005_full_data_3_6_dta)

# Q.60 Has your company undertaken any of the following initiatives over the last 36 months? 
# Upgraded an existing product line Q60a2 
# table(german_enterprise_data$q60a2)
# table(german_enterprise_data$q60b2) ## level of importance of Q60a2 to firms

select_enterprise_data <- german_enterprise_data %>%
  dplyr::select(s7, q36a, q54a, q54b, q54c, q54d, q54e, q54f, q54g, q54h, q54i, q54j, q54k, q54l, q54m, q54n,
                q54o, q54p, q54q, q54r, q54s, q54t, q56a, q56b, q56c, q60a1, q60a2, q60a3, q60a4, q60a5, q60a6,
                q60a7, q60a8, q61a, q61b, q65a, q70a, q70b, q70c, q70d, q70e, q71a1, q71a2, q71a3) %>%
  dplyr::mutate(market_type = factor(s7),
                business_assoc_member = factor(q36a),
                finance_access = factor(q54a, ordered = TRUE),
                finance_cost = factor(q54b, ordered = TRUE),
                telecommunications = factor(q54c, ordered = TRUE),
                electricity = factor(q54d, ordered = TRUE),
                transportation = factor(q54e, ordered = TRUE),
                land_access = factor(q54f, ordered = TRUE),
                land_lease = factor(q54g, ordered = TRUE),
                tax_rate = factor(q54h, ordered = TRUE),
                tax_adminstration = factor(q54i, ordered = TRUE),
                custom_trade_regul = factor(q54j, ordered = TRUE),
                busi_lincense_permit = factor(q54k, ordered = TRUE),
                labour_regul = factor(q54l, ordered = TRUE),
                skills_edu_workers = factor(q54m, ordered = TRUE),
                uncertainty_regul_policies = factor(q54n, ordered = TRUE),
                macroeconomic_stability = factor(q54o, ordered = TRUE),
                judiciary_functioning = factor(q54p, ordered = TRUE),
                corruption = factor(q54q, ordered = TRUE),
                streetcrime_theft_disorder = factor(q54r, ordered = TRUE),
                organized_crime = factor(q54s, ordered = TRUE),
                anti_competitive = factor(q54t, ordered = TRUE),
                merged_firm = factor(q56a),
                acquired_firm = factor(q56b),
                sold_firm_establishment = factor(q56c),
                new_productline = factor(q60a1),
                upgrade_existingproduct_line = factor(q60a2),
                discontinued_productline = factor(q60a3),
                joint_venture = factor(q60a4),
                new_product_lincense = factor(q60a5),
                outsourced_major_productionacivity = factor(q60a6),
                brought_inhouse_productionactivity = factor(q60a7),
                new_accreditation = factor(q60a8),
                way_acquired_technology = factor(q61b),
                current_capacity_utilization = q65a,
                vacancy_fill_time_manager = q70a,
                vacancy_fill_time_professional = q70b,
                vacancy_fill_time_skilledworker = q70c,
                vacancy_fill_time_unskilledworker = q70d,
                vacancy_fill_time_nonproduction = q70e,
                training_skilledworker = factor(q71a1),
                training_unskilledworker = factor(q71a2),
                training_nonproduction_worker = factor(q71a3)) 
transform_data <- data.frame(select_enterprise_data[, 45:87])


transform_data <- dplyr::select(transform_data, c(,1:34)) %>%
  dplyr::select(-way_acquired_technology, -judiciary_functioning, -corruption, -uncertainty_regul_policies,
                -macroeconomic_stability, -streetcrime_theft_disorder, -organized_crime, -merged_firm, 
                -acquired_firm, -sold_firm_establishment) %>%
  na.omit()

predictor_data <- transform_data %>%
  dplyr::select(-upgrade_existingproduct_line)
# View(transform_data)
# summary(transform_data)

## splitting data
set.seed(123)
german_enterprise_split_data <- initial_split(transform_data, prop = 0.7, strata = "upgrade_existingproduct_line")
train_german_ent_data <- training(german_enterprise_split_data)
test_german_ent_data <- testing(german_enterprise_split_data)

## ----------------------------------------------------------
# ## create response variable
#  y <- train_german_ent_data[, "upgrade_existingproduct_line"]
# ## create feature / predictor variables
#  feature_names <- setdiff(names(train_german_ent_data), "upgrade_existingproduct_line")
#  x <- train_german_ent_data[, feature_names]
# ##View(x)
# 
# ## setup 10 fold cross-validation
# train_control = trainControl(
#   method = "cv",
#   number = 10
# )
# 
# ### train naive bayes model
# upgradeproduct_naiveBayes <- train(
#   x = x, y = y, trControl = train_control, method = "nb"
# )
# 
# confusionMatrix(upgradeproduct_naiveBayes)
# plot(upgradeproduct_naiveBayes)
# 
# ## testing the model developed
# pred.test <- predict(upgradeproduct_naiveBayes, newdata = test_german_ent_data)
# confusionMatrix(pred.test, test_german_ent_data$upgrade_existingproduct_line)
# 
# 
# ## tuning parameters to improve model
# search_grid <- expand.grid(
#   usekernel = c(TRUE, FALSE),
#   fL = 0:15,
#   adjust = seq(0, 15, by = 1)
# )
# 
# upgradeproduct_naiveBayes_2 <- train(
#   x = x, 
#   y = y, 
#   method = "nb",
#   trControl = train_control, 
#   tuneGrid = search_grid, 
#   preProc = c("BoxCox", "center", "scale", "pca")
# )
# 
# upgradeproduct_naiveBayes_2$results%>%
#   top_n(5, wt = Accuracy) %>%
#   arrange(desc(Accuracy))
# 
# plot(upgradeproduct_naiveBayes_2)
# confusionMatrix(upgradeproduct_naiveBayes_2)
# 
# upgrade_predict_2 <- predict(upgradeproduct_naiveBayes_2, newdata = test_german_ent_data)
# confusionMatrix(upgrade_predict_2, test_german_ent_data$upgrade_existingproduct_line)


###### h2o for naive bayes
h2o.init()

## h2o does not accepted ordered factors
h2o_train_upgradeproduct <- train_german_ent_data %>%
  mutate_if(is.factor, factor, ordered = FALSE) %>%
  as.h2o()

h2o_test_upgradeproduct <- test_german_ent_data %>%
  mutate_if(is.factor, factor, ordered = FALSE) %>%
  as.h2o()

x_h2o = setdiff(names(train_german_ent_data), "upgrade_existingproduct_line")

y_h2o <- "upgrade_existingproduct_line"

## h2o naive model
h2o_naive_bayes <- h2o.naiveBayes(
  x = x_h2o,
  y = y_h2o,
  training_frame = h2o_train_upgradeproduct,
  nfolds = 10,
  laplace = 0,
)

## confusionmatrix to assess result
h2o.confusionMatrix(h2o_naive_bayes)

###### parameter tuning 
 preprocess_h2o <- preProcess(train_german_ent_data, method = c("BoxCox", "center", "scale", "pca"))
tune_train <-  predict(preprocess_h2o, train_german_ent_data) %>%
  mutate_if(is.factor, factor, ordered = F) %>%
  as.h2o()

tune_test <- predict(preprocess_h2o, test_german_ent_data) %>%
  mutate_if(is.factor, factor, ordered = F) %>%
  as.h2o()

h2o_hyper_params <- list(
  laplace = seq(0, 15, by = 0.5)
)

  h2o_grid <- h2o.grid(
    algorithm = "naivebayes",
   # grid_id = "nb_id",
    hyper_params = h2o_hyper_params,
    training_frame = tune_train,
    nfolds = 10,
    x = x_h2o,
    y = y_h2o
  )
  
 # retrieve grid_id 
  grid_id <- h2o_grid@grid_id
  
  
### sort model by accuracy
(h2o_sorted_grid <- h2o.getGrid(grid_id = grid_id, sort_by = "accuracy", decreasing = TRUE))
(h2o_best_model_retrive <- h2o_sorted_grid@model_ids[[1]])
(h20_best_model <- h2o.getModel(h2o_best_model_retrive))

#h2o.performance(h20_best_model, newdata = tune_test)

## confusinmatrx 
h2o.confusionMatrix(h20_best_model)

# ROC 
h2o_auc <- h2o.auc(h20_best_model, xval = TRUE)

## fpr retrieve
fpr <- h2o.performance(h20_best_model, xval = TRUE) %>%
  h2o.fpr() %>%
  .[["fpr"]]

## tpr retrieve
tpr <- h2o.performance(h20_best_model, xval = TRUE) %>%
  h2o.tpr() %>%
  .[["tpr"]]

data.frame(fpr = fpr, tpr = tpr) %>%
  ggplot(aes(fpr, tpr)) + geom_line() + ggtitle(sprintf("AUC: %f", h2o_auc))

# ## evaluate model with training data
# h2o.performance(h20_best_model, h2o_train_upgradeproduct)

#############################################################
## evaluate model with test data
test_model <- h2o.performance(h20_best_model, tune_test, xval = TRUE)

tpr_tested_model <- h2o.tpr(test_model) %>%
  .[["tpr"]]

fpr_tested_model <- h2o.fpr(test_model) %>%
  .[["fpr"]]

data.frame(fpr = fpr_tested_model, tpr = tpr_tested_model) %>%
  ggplot(aes(fpr, tpr)) + geom_line() + ggtitle(sprintf("AUC: %f", h2o.auc(test_model)))

## use model to predict 
h2o.predict(h20_best_model, newdata = tune_test)

h2o.confusionMatrix(h20_best_model, newdata = tune_test)
test_confusionMatrix <- h2o.confusionMatrix(h20_best_model, newdata = tune_test)

(test_rmse <- h2o.rmse(test_model))
(test_gini <- h2o.giniCoef(test_model))
row.names(test_confusionMatrix) <- c("Upgraded existing product line", "No upgrade of existing product line", "Totals")
row.names(test_confusionMatrix)

test_confusionMatrix <- test_confusionMatrix %>%
  dplyr::select("Upgraded existing product line" = 1, "No upgrade of existing product line" = 2, Error, Rate)
#View(test_confusionMatrix)
h2o.precision(test_model)
test_prediction <- h2o.predict(h20_best_model, newdata = tune_test)

test_prediction_with_rowid <- rowid_to_column(as.data.frame(test_prediction))
test_data_with_rowid <- rowid_to_column(as.data.frame(tune_test))

#View(test_prediction_with_rowid)
test_predict_join <- full_join(test_data_with_rowid, test_prediction_with_rowid) %>%
    select(rowid, upgrade_existingproduct_line, predict)
#test_predict_join$upgrade_existingproduct_line[levels]

levels(test_predict_join$upgrade_existingproduct_line)[1] <- "Upgraded product line"
levels(test_predict_join$upgrade_existingproduct_line)[2] <- "No upgrade of product line"
levels(test_predict_join$predict)[1]<- "Upgraded product line"
levels(test_predict_join$predict)[2]<- "No upgrade of product line"
#test_predict_join
  
  
