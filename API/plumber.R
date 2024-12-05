#

# Load needed packages
library(plumber)
library(tidyverse)
library(tidymodels)
library(ranger)
library(parsnip)

# Read in data set and
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


#* @apiTitle Plumber Example API
#* @apiDescription Plumber example description.

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
