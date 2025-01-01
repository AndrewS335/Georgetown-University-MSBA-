### Andrew Singh
### Machine Learning Project II
### November 22, 2024

### Set up ----

# load libraries
library(tidyverse)

library(caret)

library(GGally)

library(broom)

library(car) # Variance inflation factor

library(readxl) # read excel files

library(pROC) #Sampling-over and under, ROC and AUC curve

library(margins) # for marginal effects


## import the dataset 

employee = read_excel("Employee_Data_Project.xlsx")

## explore the dataset for null values

anyNA(employee) # find if there are any null values 

apply(employee,2,anyNA) # find if there are any null values for each column 

sum(is.na(employee)) # count the total null values in the dataset

## analysis: this is a very clean dataset being used for this project. 

# explore the dataset analysis (cont.)

summary(employee) # gain an overall sense of the dataset

str(employee) # see the structure of the dataset

dim(employee) # get the shape of the dataset

head(employee) # see the first five rows of the dataset

tail(employee) # see the last five rows of the dataset

names(employee) # see the names of the columns 

table(employee$BusinessTravel)

table(employee$Education)

## Model Parameter Interpretation

##Fit a logistic regression model

set.seed(90120) # see a seed for reproducibility 

#Age (in years)
#Attrition: Whether the employee left the previous year (Yes, No)
#BusinessTravel: How frequently the employees travelled for business purposes last year
#DistanceFromHome: Distance from home to location of work (in miles)
#Education: Education (1 ='Below College', 2= 'College', 3= 'Bachelor', 4= 'Master', 5= 'Doctor')
#EmployeeID
#Gender (Female, Male)
#JobLevel: Job level at company (scale of 1 to 5, level 1 is lowest and 5 is highest)
#MaritalStatus: Marital status of the employee (Single, Married, Divorced)
#Income: Annual Income (in $)
#NumCompaniesWorked: Number of companies they worked at previously
#StandardHours: Standard hours of work for the employee
#TotalWorkingYears: Total number of years the employee has worked so far
#TrainingTimesLastYear: Number of times training was conducted for this employee last year
#YearsAtCompany: Total number of years spent at the company by the employee
#YearsWithCurrManager: Number of years under current manager
#EnvironmentSatisfaction: Satisfaction with Work Environment (1= 'Low', 2= 'Medium', 3= 'High', 4= 'Very High')
#JobSatisfaction: Job Satisfaction (1= 'Low', 2= 'Medium', 3= 'High', 4= 'Very High')

# any obvious pre-formatting, do here (e.g., numeric to categorical etc.)
employee_df = employee %>% 
        mutate(
          # convert attrition to a categorical variable
          Attrition = recode_factor(Attrition, 
                                    "No" = "No",
                                    "Yes" = "Yes"), 
          
          # convert business travel to a categorical variable 
          BusinessTravel = recode_factor(BusinessTravel, 
                                         "Non-Travel" = "Non-Travel", 
                                         "Travel_Frequently" = "Travel_Frequently",
                                         "Travel_Rarely" = "Travel_Rarely"), 
          
          # convert educaiton to a categorical variable
          Education = recode_factor(Education, 
                                    "1" = "Below College", 
                                    "2" = "College",
                                    "3" = "Bachelor",
                                    "4" = "Master",
                                    "5" = "Doctor"),
          
          #convert gender to a categorical variable
          Gender = recode_factor(Gender,
                                 "Female" = "Female",
                                 "Male" = "Male"),
          
          #convert martial status to a categorical variable 
          MaritalStatus = recode_factor(MaritalStatus, 
                                 "Divorced" = "Divorced", 
                                  "Married" = "Married",
                                 "Single" = "Single"), 
          
          #convert environment satisfaction to a categorical variable 
          EnvironmentSatisfaction = recode_factor(EnvironmentSatisfaction, 
                                  "1" = "Low", 
                                  "2" = "Medium",
                                  "3" = "High",
                                  "4" = "Very High"),
           
          #convert job satisfaction to a categorical variable
          JobSatisfaction = recode_factor(JobSatisfaction, 
                                  "1" = "Low",
                                  "2" = "Medium",
                                  "3" = "High",
                                  "4" = "Very High")
        )

#Verify the counts for the new dataframe: employee_df

names(employee_df)


table(employee_df$Attrition) 
table(employee_df$Gender)
table(employee_df$MaritalStatus)
table(employee_df$EnvironmentSatisfaction) # there are 25 NA values
table(employee_df$JobSatisfaction) # there are 20 NA values
table(employee_df$BusinessTravel)
table(employee_df$Education)

##Summary of the model
##After conducting the logistic regression analysis, interpret the parameter estimates obtained from the model with a focus on their significance in predicting employee attrition.

# set a random seed for reproducibility
set.seed(385)

### Load Data ----
# check data structure
str(employee_df)

# check missing values
employee_df |>
  summarize(
    across(everything(), function(x) sum(is.na(x)))
  )

# any obvious pre-formatting, do here (e.g., numeric to categorical etc.)
employee_df = employee %>% 
  mutate(
    # convert attrition to a categorical variable
    Attrition = recode_factor(Attrition, 
                              "No" = "No",
                              "Yes" = "Yes"), 
    
    # convert business travel to a categorical variable 
    BusinessTravel = recode_factor(BusinessTravel, 
                                   "Non-Travel" = "Non-Travel", 
                                   "Travel_Frequently" = "Travel_Frequently",
                                   "Travel_Rarely" = "Travel_Rarely"), 
    
    # convert educaiton to a categorical variable
    Education = recode_factor(Education, 
                              "1" = "Below College", 
                              "2" = "College",
                              "3" = "Bachelor",
                              "4" = "Master",
                              "5" = "Doctor"),
    
    #convert gender to a categorical variable
    Gender = recode_factor(Gender,
                           "Female" = "Female",
                           "Male" = "Male"),
    
    #convert martial status to a categorical variable 
    MaritalStatus = recode_factor(MaritalStatus, 
                                  "Divorced" = "Divorced", 
                                  "Married" = "Married",
                                  "Single" = "Single"), 
    
    #convert environment satisfaction to a categorical variable 
    EnvironmentSatisfaction = recode_factor(EnvironmentSatisfaction, 
                                            "1" = "Low", 
                                            "2" = "Medium",
                                            "3" = "High",
                                            "4" = "Very High"),
    
    #convert job satisfaction to a categorical variable
    JobSatisfaction = recode_factor(JobSatisfaction, 
                                    "1" = "Low",
                                    "2" = "Medium",
                                    "3" = "High",
                                    "4" = "Very High")
  )

### Step 1: Create a train/test split ----

test_employee_df <- createDataPartition(
  employee_df$Attrition,
  p = 0.3
)

employee_df_test <- employee_df[test_employee_df[[1]], ]

employee_df_train <- employee_df[-test_employee_df[[1]], ]

employee_df_validation <- createDataPartition(
  employee_df_train$Attrition,
  p = 0.3
)

employee_df_validation <- loans[validation_idx[[1]], ]

employee_df_train <- employee_df_train[-validation_idx[[1]], ]

### Step 2: Data Exploration ----

summary(loans_train)

loans_train[sample(1:nrow(loans_train), 5000), ] |> # sampling to save my computer
  ggpairs(aes(color = SeriousDlqin2yrs, alpha = 0.4))


### Step 3: Data pre-processing ----

# we have lots of data so we can do a complete case analysis
loans_train <- 
  loans_train |>
  drop_na()

# we have imbalanced classes. Let's downsample the negative class
# create vector of indices to keep
keep_idx <- c(
  which(loans_train$SeriousDlqin2yrs == "1"), # indices of positive class
  sample(which(loans_train$SeriousDlqin2yrs != "1"), 5000)
)

# let's see what that did
loans_train[keep_idx, ] |>
  ggpairs(aes(color = SeriousDlqin2yrs, alpha = 0.4))

### Step 4: Feature Engineering ----

### Step 5: Feature & Model Selection ----

f1 <- glm(
  SeriousDlqin2yrs ~ . +
    I(Age ^ 2) +
    I(Num_loans ^ 2) + 
    I(Num_dependents ^ 2) + 
    I(MonthlyIncome ^ 2 ) +
    I(Num_Savings_Accts ^ 2) + 
    I(DebtRatio ^ 2),
  data = loans_train[keep_idx, ] |> # 
    select(
      -Cust_id
    ),
  family = binomial("logit")
)

summary(f1)

vif(f1)

roc1 <- roc(
  data = tibble(
    actual = loans_train |> # not using balanced data for evaluation
      select(SeriousDlqin2yrs) |>
      unlist(),
    predicted = predict(f1, loans_train)
  ),
  "actual",
  "predicted"
)

plot(roc1)

roc1$auc

# Let's use stepwise regression to  help pare down some variables
f_step <- step(
  object = f1,
  direction = "both"
)

summary(f_step)

vif(f_step)

roc_step <- roc(
  data = tibble(
    actual = loans_train |> # not using balanced data for evaluation 
      select(SeriousDlqin2yrs) |>
      unlist(),
    predicted = predict(f_step, loans_train)
  ),
  "actual",
  "predicted"
)

plot(roc_step)

roc_step$auc # didn't do much for in-sample prediction

# let's pick a final model 
# Using my judgment: Drop Age ^2, DebtRatio, and DebtRatio ^ 2
f_final <- 
  glm(
    SeriousDlqin2yrs ~  +
      Age +
      Num_loans +
      I(Num_loans ^ 2) + 
      Num_dependents +
      MonthlyIncome +
      I(MonthlyIncome ^ 2 ) +
      Num_Savings_Accts +
      I(Num_Savings_Accts ^ 2) ,
    data = loans_train[keep_idx, ] |>
      select(
        -Cust_id
      ),
    family = binomial("logit")
  )

summary(f_final)

roc_final <- roc(
  data = tibble(
    actual = loans_train |> # not using balanced data for evaluation
      select(SeriousDlqin2yrs) |>
      unlist(),
    predicted = predict(f_final, loans_train)
  ),
  "actual",
  "predicted"
)

plot(roc_final)

roc_final$auc # little decrease in AUC, I will accept it

### Step 6: Model Validation ----
preds_validation <- predict(f_final, loans_validation)

roc_validation <- roc(
  data = tibble(
    actual = loans_validation |> 
      select(SeriousDlqin2yrs) |>
      unlist(),
    predicted = preds_validation
  ),
  "actual",
  "predicted"
)

plot(roc_validation)

roc_final$auc # not much difference --> ship it!


### Step 7: Predictions and Conclusions ----

# See how it performs on held out data
preds_test <- predict(f_final, loans_test)

roc_test <- roc(
  data = tibble(
    actual = loans_test |> 
      select(SeriousDlqin2yrs) |>
      unlist(),
    predicted = preds_test
  ),
  "actual",
  "predicted"
)

plot(roc_test)

roc_test$auc # not much difference --> woo!

# Re-train the model on the whole data set for marginal effects/production
# why? Because we want as much data as possible in our final model.
# We verified that the model specification generalizes well enough with a 
# train/validate/test split

# first, balance classes before training
sum(loans$SeriousDlqin2yrs == "1") # put in quotes because it's a factor now


keep_idx_whole <- c(
  which(loans$SeriousDlqin2yrs == "1"), # indices of positive class
  sample(which(loans$SeriousDlqin2yrs != "1"), 10026)
)

f_prod <- 
  glm(
    SeriousDlqin2yrs ~  +
      Age +
      Num_loans +
      I(Num_loans ^ 2) + 
      Num_dependents +
      MonthlyIncome +
      I(MonthlyIncome ^ 2 ) +
      Num_Savings_Accts +
      I(Num_Savings_Accts ^ 2) ,
    data = loans[keep_idx_whole, ] |> 
      select(
        -Cust_id
      ),
    family = binomial("logit")
  )

summary(f_prod)

roc_prod <- roc( # unbalancing classes for the evaluation
  data = tibble(
    actual = loans |> 
      select(SeriousDlqin2yrs) |>
      unlist(),
    predicted = predict(f_prod, loans, type = "response")
  ),
  "actual",
  "predicted"
)

plot(roc_prod)

roc_prod$auc 

# marginal effects
coefs <- tidy(f_prod) |>
  mutate(
    odds = exp(estimate),
    odds_mfx = odds - 1
  )

coefs


mfx <- margins(f_prod)

str(mfx) # notice this is a big object

# instead of getting into the details of messing with it, feel free to 
# print to screen and grab the mfx that way
mfx



# Remember: if you use a quadratic and see the coefficients are opposite signs
# you have a critical point.
# plot marginal effect on terms with critical points

summary(f_prod)

loans |>
  ggplot(aes(
    x = Num_loans, 
    y = 1 / (1 + exp(-1 * (coefs$estimate[1] + coefs$estimate[3] * Num_loans + coefs$estimate[4] * Num_loans ^ 2)))
  )) + 
  geom_line() + 
  xlab("Number of Loans") +
  ylab("P(default|number of loans, excluding other variables)")

loans |>
  ggplot(aes(
    x = MonthlyIncome / 1000, 
    y = 1 / (1 + exp(-1 * (coefs$estimate[1] + coefs$estimate[6] * MonthlyIncome + coefs$estimate[7] * MonthlyIncome ^ 2)))
  )) + 
  geom_line() + 
  xlab("Monthly income, $'000") +
  ylab("P(default|monthly income, excluding other variables)")


loans |>
  ggplot(aes(
    x = Num_Savings_Accts, 
    y = 1 / (1 + exp(-1 * (coefs$estimate[1] + coefs$estimate[8] * Num_Savings_Accts + coefs$estimate[9] * Num_Savings_Accts ^ 2)))
  )) + 
  geom_line() + 
  xlab("Number of Savings Accounts") + 
  ylab("P(default|number of savings accts., excluding other variables)")


