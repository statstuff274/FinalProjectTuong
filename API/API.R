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

#* @apiTitle API for Diabetes Data Health Indicators
#* @apiDescription Here we have our API. 

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
set.seed(11)
diabetes_split <- initial_split(diabetes_data, prop = 0.70, strata = Diabetes_binary)
diabetes_train <- training(diabetes_split)
diabetes_test <- testing(diabetes_split)
diabetes_5_fold <- vfold_cv(diabetes_train, 5)

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


#* Predictions
#* @param chol (default: 0) - High cholesterol status (0 = No, 1 = Yes)
#* @param smoke (default: 0) - Smoker status (0 = No, 1 = Yes)
#* @param sex (default: 0) - Sex of the individual (0 = Female, 1 = Male)
#* @param physical (default: 1) - Physical activity status (0 = No, 1 = Yes)
#* @param education (default: 6) - Education level (1 = No schooling to 6 = College graduate)
#* @get /pred

function(chol = 0, smoke = 0, sex = 0, physical = 1, education = 6) {
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

# Here I will provide 3 examples for you to copy and paste to see if they work!
# Example 1: /pred?chol=1&smoke=0&sex=0&physical=1&education=6
# Example 2: /pred?chol=1&smoke=0&sex=1&physical=0&education=4
# Example 3: /pred?chol=0&smoke=1&sex=0&physical=1&education=5


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



#* Echo back the input
#* @param msg The message to echo
#* @get /echo
function(msg = "") {
  list(msg = paste0("The message is: '", msg, "'"))
}

#* Plot a histogram
#* @serializer png
#* @get /plot
function() {
  rand <- rnorm(100)
  hist(rand)
}

#* Return the sum of two numbers
#* @param a The first number to add
#* @param b The second number to add
#* @post /sum
function(a, b) {
  as.numeric(a) + as.numeric(b)
}

# Programmatically alter your API
#* @plumber
function(pr) {
  pr %>%
    # Overwrite the default serializer to return unboxed JSON
    pr_set_serializer(serializer_unboxed_json())
}



