#
# This is a Plumber API. You can run the API by clicking
# the 'Run API' button above. 
#
# Find out more about building APIs with Plumber here:
#
#    https://www.rplumber.io/
#

# Load needed packages
library(plumber)
library(tidyverse)
library(tidymodels)
library(ranger)
library(ggplot2)
library(parsnip)

# Read in data set
diabetes_data <- read_csv("diabetes_binary_health_indicators_BRFSS2015.csv")

# Mutating to the correct column types
diabetes_data <- diabetes_data %>%
  mutate(
    Diabetes_binary = factor(Diabetes_binary, levels = c(0, 1), labels = c("No", "Yes")),
    HighBP = factor(HighBP, levels = c(0, 1), labels = c("No", "Yes")),
    HighChol = factor(HighChol, levels = c(0, 1), labels = c("No", "Yes")),
    CholCheck = factor(CholCheck, levels = c(0, 1), labels = c("No", "Yes")),
    BMI = as.numeric(BMI),
    Smoker = factor(Smoker, levels = c(0, 1), labels = c("No (Smoker)", "Yes (Smoker)")),
    Stroke = factor(Stroke, levels = c(0, 1), labels = c("No", "Yes")),
    HeartDiseaseorAttack = factor(HeartDiseaseorAttack, levels = c(0, 1), labels = c("No", "Yes")),
    PhysActivity = factor(PhysActivity, levels = c(0, 1), labels = c("No", "Yes")),
    Fruits = factor(Fruits, levels = c(0, 1), labels = c("No", "Yes")),
    Veggies = factor(Veggies, levels = c(0, 1), labels = c("No", "Yes")),
    HvyAlcoholConsump = factor(HvyAlcoholConsump, levels = c(0, 1), labels = c("No", "Yes")),
    AnyHealthcare = factor(AnyHealthcare, levels = c(0, 1), labels = c("No", "Yes")),
    NoDocbcCost = factor(NoDocbcCost, levels = c(0, 1), labels = c("No", "Yes")),
    GenHlth = factor(GenHlth, levels = c(1,2,3,4,5), labels = c("excellent", "very good", "good", "fair", "poor"), ordered = T),
    MentHlth = as.numeric(MentHlth),
    PhysHlth = as.numeric(PhysHlth),
    DiffWalk = factor(DiffWalk, levels = c(0, 1), labels = c("No", "Yes")),
    Sex = factor(Sex, levels = c(0, 1), labels = c("female", "male")),
    Age = as.numeric(Age),
    Education = factor(Education, levels = c(1,2,3,4,5,6), labels = c("no school or only kindergarten", "elementary", "some high school", "high school or GED", "some college or technical school", "college grad")),
    Income = factor(Income, levels = c(1,2,3,4,5,6,7,8), labels = c("less than $10K", "$10K to $15K", "$15K to $20K", "$20K to $25K", "$25K to $35K", "$35K to $50K", "$50K to $75K", "more than $75K"), ordered = T)
  )

# Renaming for ease of reference
diabetes_data <- diabetes_data %>%
  rename('bp' = 'HighBP',
         'chol' = 'HighChol',
         'cholcheck' = 'CholCheck',
         'bmi' = 'BMI',
         'smoke' = 'Smoker',
         'stroke' = 'Stroke',
         'heartdiseaseorattack' = 'HeartDiseaseorAttack',
         'physical' = 'PhysActivity',
         'fruit' = 'Fruits',
         'veg' = 'Veggies',
         'alc' = 'HvyAlcoholConsump',
         'healthcare' = 'AnyHealthcare',
         'doc' = 'NoDocbcCost',
         'genhlth' = 'GenHlth',
         'menthlth' = 'MentHlth',
         'physhlth' = 'PhysHlth',
         'diffwalk' = 'DiffWalk',
         'sex' = 'Sex',
         'age' = 'Age',
         'education' = 'Education',
         'income' = 'Income')

# Adding in best overall model from our modeling page
diabetes_split <- initial_split(diabetes_data, prop = 0.70, strata = Diabetes_binary)
diabetes_train <- training(diabetes_split)
diabetes_test <- testing(diabetes_split)

# Creating the recipe
diabetes_recipe <- recipe(Diabetes_binary ~ chol + smoke + sex + physical + education, data = diabetes_train) %>%
  step_dummy(chol, smoke, sex, physical, education) %>%
  step_normalize(all_numeric())

# Setting up our random forest tree model based on the best overall values from modeling
rf_spec <- rand_forest(trees = 500, mtry = 7, min_n = 10) %>%
  set_engine("ranger", importance = "impurity", splitrule = "gini") %>%
  set_mode("classification")

# Creating the workflow
rf_wkf <- workflow() %>%
  add_recipe(diabetes_recipe) %>%
  add_model(rf_spec)

# Fitting data to our model
best_rf_model <- rf_wkf %>%
  fit(diabetes_data)

#* @apiTitle API for Diabetes Data Health Indicators
#* @apiDescription Here we have our API. We have 4 endpoints: the predictions endpoints allow us to make predictions using the interested variables, examples allows you to obtain the 3 examples to use, info allows you to obtain the info, and confusion allows you to plot a confusion matrix. Thanks for you dropping by!

#* Predictions
#* @param chol  High cholesterol status: No, Yes -- default: No
#* @param smoke Smoker status: No, Yes -- default: No
#* @param sex  Sex of the individual: Female, Male -- default: Female
#* @param physical  Physical activity status: No, Yes -- default: Yes
#* @param education Education level: 1 = No schooling, 2 = elementary, 3 = some HS,  4 = HS/GED, 5 = some college, 6 = College graduate -- default: 6
#* @get /pred

function(chol = "No", smoke = "No", sex = "Female", physical = "Yes", education = 6) {
  # Creating a new data set for prediction with most common
  default_inputs <- data.frame(
    chol,
    smoke,
    sex,
    physical,
    education
  )
  rf_predict <- predict(best_rf_model, new_data = default_inputs, type = "prob")
  
  return(list(prediction = rf_predict))
}


#* Obtaining 3 examples to use
#* @get /examples

function() {
  list(
    example1 = "/pred?chol=Yes&smoke=No&sex=Female&physical=Yes&education=6",
    example2 = "/pred?chol=Yes&smoke=No&sex=Male&physical=No&education=4",
    example3 = "/pred?chol=No&smoke=Yes&sex=Male&physical=Yes&education=5"
  )
}


#* Obtaining info
#* @get /info

function() {
  list(
    author = "John Tuong",
    github_url = "https://github.com/statstuff274/FinalProjectTuong.git"
  )
}



#* Plot a confusion matrix
#* @serializer png
#* @get /confusion
function() {
  # This'll be used to take the predictions from the model of the data set
  rf_predictions <- best_rf_model %>%
    predict(new_data = diabetes_data) %>%
    bind_cols(diabetes_data)
  
  # Create a confusion matrix 
  con_matrix <- rf_predictions %>%
    conf_mat(truth = Diabetes_binary, estimate = .pred_class)
  
  # Plot the confusion matrix
  con_plot <- autoplot(con_matrix, type = "heatmap") +
    ggtitle("Confusion Matrix")
  print(con_plot)
}






