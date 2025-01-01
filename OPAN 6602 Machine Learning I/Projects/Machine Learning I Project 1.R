# install the necessary packages 

install.packages("tidyverse") 
install.packages("dplyr")
install.packages("ggplot2")
install.packages("GGally")
install.packages("caret")
install.packages("corrplot")
install.packages("scales")
install.packages("Hmisc")
install.packages("broom")
install.packages("mvrsquared")
install.packages("ggrid")
install.packages("lmtest")
install.packages("sandwich")
install.packages("car")

# use the library function

library(tidyverse)
library(dplyr)
library(ggplot2)
library(GGally)
library(caret)
library(corrplot)
library(scales) # Make sure to load this for the comma and label formatting functions
library(Hmisc)
library(broom)
library(mvrsquared)
library(ggridges)
library(lmtest)
library(sandwich)
library(car)

# import the dataset and set the working directory
setwd("~/Documents/Machine Learning I/Machine Learning OPAN 6602")
bikeshare = read.csv("Data-Raw/Capital Bike Sharing data by hour.csv")

# explore the dataset and check the differences between the old and new dataframe

View(bikeshare) #view the entire the dataset
View(bikesharenew) #view the new dataset with recoded variables

str(bikeshare) #explore the datatypes of the dataset
str(bikesharenew) #explore the datatypes of the new dataset

dim(bikeshare) #explore the dimensions of the dataset
dim(bikesharenew) #explore the dimensions of the dataset

head(bikeshare) #explore the first five rows of the dataset
head(bikesharenew) #explore the first five rows of the new dataset

tail(bikeshare) #explore the last five rows of the dataset
tail(bikesharenew) #explore the last five rows of the new dataset

names(bikeshare) #get the name of the columns in the dataset
names(bikesharenew) #get the name of the columns in the new dataset

# perform basic data exploration of the dataset and check for null values in both 
# the old and new dataframe

anyNA(bikeshare) #check if there are any NA values in the dataset
anyNA(bikesharenew) #check if there are any NA values in the dataset

apply(bikeshare, 2, anyNA) #confirm that are no NA values in the dataset with a similar function
apply(bikesharenew, 2, anyNA) #confirm that are no NA values in the dataset with a similar function

sum(is.na(bikeshare)) #there are no null values in the dataset
sum(is.na(bikesharenew)) #there are no null values in the new dataset

# explore the dataset for any insights 

summary(bikeshare) #explore summary statistics for the dataset
summary(bikesharenew) #explore summary statistics for the new dataset


# Provide summary statistics (mean, median, standard deviation, etc.).
# Include in the report univariate summaries of the whole data set, not just the training set. Do this
# only after you have completed your analysis. Do not look at the test set and let it
# influence your analysis.


# Important findings

#Based on the findings, there are 2,672,662 (81.16%) regular users, 620,017 (18.83%), and 3,292,679  total users. 
#Based on the findings, spring (4,409) and summer (4,496) receive higher ride shares than fall (4,232) and winter (4,242).
#2012 had more riders (8,734) than 2011 (8645).
#Most days the weather was "clear, few clouds, partly cloudly, and cloudy (11,413) following by "mist/cloudy) (4544).
#On average, the ride share per day was primarily consistent hovering around 14.1%.
#Majority of the days, bikeshares were used wre not on a holiday. (16,879) while only 500 bikeshares were on a holiday. 
#A total of 11,865 were used if the day is neither a weekend or a holiday. 5514 were not. 

#Recode categorical variables 

#Are there variables that R has read in as numeric but should be categorical? (i.e., 
#Variables that should be character or factor variables.) Could this affect your analysis? Re-code such variables as necessary. •

str(bikeshare) #check the datatypes of the original dataframe
str(bikesharenew) #check the datatypes of the new dataframe

# When using the structure command for the bikeshare dataset, we see that there
# several variables that will need to be converted from integer to a factor due 
# to its categorical nature, for example: 1. season will be need to be converted 
# because it is a fixed number of seasons in the year. 

#Based on the initial re-factoring of the data, the counts per categorical variable is the same, as the
#original variables in the original dataframe. The analysis is not impacted for the total counts. When finding the 
#univariate descriptive statistics such as standard deviation, mean, and median the output is changed to reflect the exact 
#integer values, instead of reflecting a exponential value. 


#Convert the fields from integer to categorical and create a new dataframe to store results. 


str(bikesharenew)

#recode weather variable 
bikesharenew = bikeshare %>%  
  mutate(weatherrecode = recode(weathersit, 
                                "1" = "Clear",
                                "2" = "Mist",
                                "3" = "Light snow and rain",
                                "4" = "Heavy rain"))


weatherorg = table(bikeshare$weathersit) #check the total number of responses for the original weather variable
print(weatherorg) #print the response for the original weather variable 
prop.table(weatherorg) #check the frequency for each category in the weather variable

weathernew = table(bikeshare$weatherrecode) #check the total number of responses in the recoded weather variable 
print(weathernew) #print the response for the new weather variable
prop.table(weathernew) #check the frequency for each category in the new weather variable

#weekday variable
weekdayfreq = table(bikeshare$weekday) #check the frequency for each weekday in the weekday variable
print(weekdayfreq) #print the response for the weekday variable
prop.table(weekdayfreq) #print the frequency for the weekday variable

#recode the holiday variable
bikeshare = bikeshare %>% #recode the holiday variable
  mutate(holidayrecode = recode(holiday, 
                                "1" = "True",
                                "0" = "False"))

table(bikeshare$holiday) #check the totals in the original holiday variable
table(bikeshare$holidayrecode) #check the totals in the new holiday variable 

#workingday variable 
table(bikeshare$workingday) #check the totals in the original workday variable

bikeshare = bikeshare %>% #recode the workingday variable
  mutate(workingdayrecode = recode(workingday,
                                   "1" = "if the day is neither a weekend nor holiday",
                                   "0" = "not"))


workingdayfreq = table(bikeshare$workingdayrecode) #check the total in the new working day variable 
print(workingdayfreq) #print the frequency in the new workingday variable
prop.table(workingdayfreq) #check the frequency in the new workingday variable

# re-factor the temperature variable in the dataframe 
bikesharenew = bikesharenew %>% 
  mutate(tempnew = temp * 100)

mean(bikesharenew$tempnew) #verify if the temperature is in the right format

# re-factor the humidity variable in the dataframe
bikesharenew = bikesharenew %>%  
  mutate(humidity = hum * 100)

mean(bikesharenew$humidity) #verify if the temperature is in the right format

# recode the month variable 

table(bikesharenew$mnth) #verify the current count of the old mnth variable 

bikesharenew = bikesharenew %>%  #recode the new month variable 
      mutate(month = recode(mnth, 
                            "1" = "January", 
                            "2" = "February",
                            "3" = "March",
                            "4" = "April",
                            "5" = "May", 
                            "6" = "June",
                            "7" = "July",
                            "8" = "August",
                            "9" = "September", 
                            "10" = "October", 
                            "11" = "November",
                            "12" = "December"))

table(bikesharenew$month) #verify the count of the new month variable

#recode the weekday variable

table(bikesharenew$weekday) #verify the counts for the old weekday variable

bikesharenew = bikesharenew %>%  #recode the old weekday variable 
  mutate(weekdaynew = recode(weekday,
                             "0" = "Sunday",
                             "1" = "Monday",
                             "2" = "Tuesday",
                             "3" = "Wednesday",
                             "4" = "Thursday",
                             "5" = "Friday",
                             "6" = "Saturday"))

table(bikesharenew$weekdaynew) #verify the counts are consistent with the old variable

bikesharenew = bikesharenew %>% 
  mutate(seasonnew = recode(season, 
                            "1" = "Winter",
                            "2" = "Spring",
                            "3" = "Summer",
                            "4" = "Fall"))

head(bikesharenew)


bikesharenew = bikeshare %>% 
  mutate(dteday = as.Date(bikeshare$dteday),
         season = as.factor(bikeshare$season), 
         yr = as.factor(bikeshare$yr),
         mnth = as.factor(bikeshare$mnth),
         hr = as.factor(bikeshare$hr),
         holiday = as.factor(bikeshare$holiday),
         weekday = as.factor(bikeshare$weekday),
         workingday = as.factor(bikeshare$workingday),
         weathersit = as.factor(bikeshare$weathersit),
         weatherrecode = as.factor(bikeshare$weathersit),
         holidayrecode = as.factor(bikeshare$holiday),
         workingdayrecode = as.factor(bikeshare$workingday),
         seasonrecode = as.factor(bikeshare$season))

str(bikesharenew)

# Double-check the counts for the re-coded variables 
# Using sapply to count non-missing values for each column in the original bikeshare dataframe
count_records <- sapply(bikeshare, function(x) sum(!is.na(x)))

# Print the counts
count_records

#Using sapply to count the non-missing values for each column in the new bikeshare dataframe 
count_records <- sapply(bikesharenew, function(x) sum(!is.na(x)))

# Print the counts for each individual column between the old dataframe and new dataframe to ensure accuracy. 
count_records

#Check the individual counts for the 
for (col in names(bikesharenew)) {
  # Count unique values (excluding NA) using the table function
  value_counts <- table(bikesharenew[[col]], useNA = "ifany")
  
  # Print the counts
  print(paste("Counts for variable:", col))
  print(value_counts)
  
  # Compare the total count with the original number of rows (including NA)
  total_counts <- sum(value_counts)
  original_count <- length(bikeshare[[col]])
  
  # Check if counts match
  if (total_counts == original_count) {
    print(paste("The counts for", col, "match the original variable count:", original_count))
  } else {
    print(paste("Mismatch in counts for", col, "- Table count:", total_counts, "Original count:", original_count))
  }
}


# Univariate analysis for holiday, weekday, workingday, weathersit, temp, 
# humidity, windspeed, registered, casual, and count.

#Include in the report univariate summaries of the whole data set
# Pre-allocate vectors
stddev <- numeric(ncol(bikesharenew))
median_vals <- numeric(ncol(bikesharenew))
avg <- numeric(ncol(bikesharenew))

# Loop through each column
for (i in 1:ncol(bikesharenew)) {
  if (is.numeric(bikesharenew[, i])) {  # Check if the column is numeric
    stddev[i] <- sd(bikesharenew[, i], na.rm = TRUE)  # Calculate standard deviation
    median_vals[i] <- median(bikesharenew[, i], na.rm = TRUE)  # Calculate median
    avg[i] <- mean(bikesharenew[, i], na.rm = TRUE)  # Calculate mean
  } else {
    stddev[i] <- NA  # Assign NA for non-numeric columns
    median_vals[i] <- NA
    avg[i] <- NA
  }
}

# Combine results into a data frame (optional)
results <- data.frame(
  Standard_Deviation = stddev,
  Median = median_vals,
  Average = avg
)

# If you want to name the rows with the column names
rownames(results) <- colnames(bikesharenew)

# View the results
print(results)


# check the total number and percentage composition of regular vs casual bikeshare users 
bikesharenew %>%
  select(registered, casual, cnt) %>% 
  summarize(
    totalreg = sum(registered),
    totalregper = sum(registered) / (sum(cnt)),
    totalcasual = sum(casual),
    totalcasper = sum(casual) / (sum(cnt)),
    totalcount = sum(cnt)
  )

names(bikesharenew)

#bikesharenew %>% 
#  group_by() %>% 
#  select(registered, casual, cnt) %>% 
#  summarize(
#    totalreg = sum(registered),
#    totalregper = sum(registered) / (sum(cnt)),
#    totalcasual = sum(casual),
#    totalcasper = sum(casual) / (sum(cnt)),
#    totalcount = sum(cnt)
#  )


#workingdaycasual = bikesharenew %>% 
#  select(registered, casual, cnt, workingdayrecode) %>% 
#  group_by(workingdayrecode) %>% 
#  summarize(
#    totalreg = sum(registered),
#    totalregper = sum(registered) / (sum(cnt)),
#    totalcasual = sum(casual),
#    totalcasper = sum(casual) / (sum(cnt)),
#    totalcount = sum(cnt)
#  )





# Check the frequency for the new dataframe to make sure the values are consistent with the 
# old dataframe 

# Check the frequency distribution of the season variable in the new dataframe
seasonfreq = table(bikesharenew$season) # season: four categories > 1=Winter, 2=Spring, 3=Summer, 4=Fall (independent variable)
print(seasonfreq)
prop.table(seasonfreq) #count the percentage distribution with the season variable

# Check the frequency distribution of the year variable in the new dataframe
yrfreq = table(bikesharenew$yr) #count the frequency for the year, 0 = 2011, 1 = 2012
print(yrfreq) #print year frequency
prop.table(yrfreq) #print the year frequency percentage

# Check the frequency distribution of the month variable in the new dataframe
mnthfreq = table(bikesharenew$mnth) #count the frequency for each month
print(mnthfreq) #print the frequency for each month
prop.table(mnthfreq) #print the percentage frequency for each month

# Find the summary for numerical variables. 
summary(bikesharenew$temp) #provide the summary for temp
summary(bikesharenew$windspeed) #provide the summary for windspeed
summary(bikesharenew$atemp) #provide the summary for atemp
summary(bikesharenew$hum) #provide the summary for humidity 

#Which variables are outcome variables? Be sure to exclude any outcomes from the right hand side of the regression model. 

# check for general seasonal differences in the time variable 

total_cnt <- sum(bikesharenew$cnt, na.rm = TRUE)
total_registered <- sum(bikesharenew$registered, na.rm = TRUE)
total_casual<- sum(bikesharenew$casual, na.rm = TRUE)

# Labels and sizes for the pie chart
labels <- c("Total Registered Users", "Total Casual Users")
sizes <- c(total_registered, total_casual)

# Calculate percentages for each section
percentages <- round(sizes / sum(sizes) * 100, 1)
annotated_labels <- paste(labels, "\n", sizes, " (", percentages, "%)", sep="")

# Plotting the pie chart with annotations
pie(sizes, labels = annotated_labels, col = c("lightblue", "lightcoral"), main = "Capital Bikeshare Users")


#Are there seasonal differences? #Spring and Summer experience higher bikeshare users


# check for bikeshare distribution amongst registered and casual users in the holiday variable
bikesharenew %>% 
  group_by(holiday) %>% 
  summarize(total = sum(cnt), casualusers = sum(casual), registeredusers = sum(registered))


# check for bikeshare distributtotal_counts# check for bikeshare distribution amongst registered and casual users in the month variable
bikesharenew %>% 
  group_by(mnth) %>% 
  summarize(totalcnt = sum(cnt))

names(bikesharenew)
            

# visualize users for the dataset
barplot(bikesharenew$registered)

##### REVISE ALL CODE 


# Reshape the data to long format
bikeshare_long <- bikesharenew %>%
  pivot_longer(cols = c(registered, casual), 
               names_to = "user_type", 
               values_to = "count")


bikeshare_long = bikesharenew %>% 
  mutate(dteday = as.Date(bikeshare$dteday),
         season = as.factor(bikeshare$season), 
         yr = as.factor(bikeshare$yr),
         mnth = as.factor(bikeshare$mnth),
         hr = as.factor(bikeshare$hr),
         holiday = as.factor(bikeshare$holiday),
         weekday = as.factor(bikeshare$weekday),
         workingday = as.factor(bikeshare$workingday),
         weathersit = as.factor(bikeshare$weathersit),
         weatherrecode = as.factor(bikeshare$weathersit),
         holidayrecode = as.factor(bikeshare$holiday),
         workingdayrecode = as.factor(bikeshare$workingday),
         seasonrecode = as.factor(bikeshare$season))

str(bikeshare_long)

head(bikeshare_long)

#recode weather variable 
bikeshare_long = bikesharenew %>%  
  mutate(weatherrecode = recode(weathersit, 
                                "1" = "Clear, cloudy",
                                "2" = "Mist + cloudy, mist",
                                "3" = "Light snow and rain",
                                "4" = "Heavy rain + ice pallets"))


bikeshare_long = bikesharenew %>% #recode the holiday variable
  mutate(holidayrecode = recode(holiday, 
                                "1" = "True",
                                "0" = "False"))


bikeshare_long = bikesharenew %>% #recode the workingday variable
  mutate(workingdayrecode = recode(workingday,
                                   "1" = "if the day is neither a weekend nor holiday",
                                   "0" = "not"))

bikeshare_long = bikesharenew %>%  #recode the new month variable 
  mutate(month = recode(mnth, 
                        "1" = "January", 
                        "2" = "February",
                        "3" = "March",
                        "4" = "April",
                        "5" = "May", 
                        "6" = "June",
                        "7" = "July",
                        "8" = "August",
                        "9" = "September", 
                        "10" = "October", 
                        "11" = "November",
                        "12" = "December"))

bikeshare_long = bikesharenew %>%  #recode the old weekday variable 
  mutate(weekdaynew = recode(weekday,
                             "0" = "Sunday",
                             "1" = "Monday",
                             "2" = "Tuesday",
                             "3" = "Wednesday",
                             "4" = "Thursday",
                             "5" = "Friday",
                             "6" = "Saturday"))


bikeshare_long = bikesharenew %>% 
  mutate(dteday = as.Date(bikesharenew$dteday),
         season = as.factor(bikesharenew$season), 
         yr = as.factor(bikesharenew$yr),
         mnth = as.factor(bikesharenew$mnth),
         hr = as.factor(bikesharenew$hr),
         holiday = as.factor(bikesharenew$holiday),
         weekday = as.factor(bikesharenew$weekday),
         workingday = as.factor(bikesharenew$workingday),
         weathersit = as.factor(bikesharenew$weathersit),
         weatherrecode = as.factor(bikesharenew$weathersit),
         holidayrecode = as.factor(bikesharenew$holiday),
         workingdayrecode = as.factor(bikesharenew$workingday),
         seasonrecode = as.factor(bikesharenew$season))


class(bikeshare_long$cnt)

aggregate(cnt~mnth, bikeshare_long, sum)

# Create the grouped bar chart
ggplot(bikeshare_long, aes(x = hum, y = count, fill = user_type)) + 
  geom_col(position = "dodge", alpha = 0.8) +
  theme_minimal() +
  labs(
    x = "Humidity", 
    y = "Number of Riders", 
    title = "The Total Number of Casual and Registered Riders Across Humidity Levels",
    subtitle = "The highest number of bikeshares is seen approximately when the humidity is around 55.",
    caption = "Data provided by the Bike Sharing Systems dataset",
    fill = "User Type"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 20, color = "darkblue"),
    plot.subtitle = element_text(hjust = 0.5)
  )


## Month analysis
# visualize ridership across months for total count ** generally ridership ebbs and flows across the month

ggplot(bikesharenew, aes(x = month, y = total_cnt)) + 
  geom_col(fill = "skyblue", alpha = 0.8) +
  theme_minimal() +
  labs(
    x = "Month",
    y = "Total Rider Count",
    title = "Total Rider Counts by Month",
    subtitle = "Across the year, generally bikeshare ridership is consistently high between May through October.",
    caption = "Data provided by the Bike Sharing Systems dataset"
  ) +
  scale_y_continuous(
    labels = scales::label_number(scale = 1e-3, suffix = "K"),
    expand = expansion(mult = c(0, 0.05))
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 90, hjust = 1, margin = margin(t = 10), size = 10)
  )

# visualize ridership across months for registered ** registered uses are consistently high around May to October.
ggplot(bikesharenew, aes(x = mnth, y = cnt)) +
  geom_col(fill = "skyblue", alpha = 0.8) +
  theme_minimal() +
  labs(
    x = "Month",
    y = "Total Registered Rider Count",
    title = "Total Registered Rider Counts by Month",
    subtitle = "Across the year, generally bikeshare ridership is consistently high between May through October.",
    caption = "Data provided by the Bike Sharing Systems dataset"
  ) +
  scale_y_continuous(
    labels = scales::label_number(scale = 1e-3, suffix = "K"),
    expand = expansion(mult = c(0, 0.05))
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 90, hjust = 1, margin = margin(t = 10), size = 10)
  )


# visualize ridership across months for casual *** may - september is mostly the best usage for casual users. 
ggplot(bikesharenew, aes(x = mnth, y = casual)) +
  geom_col(fill = "skyblue", alpha = 0.8) +
  theme_minimal() +
  labs(
    x = "Month",
    y = "Total Casual Rider Count",
    title = "Total Casual Rider Counts by Month",
    subtitle = "Across the year, generally bikeshare ridership is consistently high between May through September",
    caption = "Data provided by the Bike Sharing Systems dataset"
  ) +
  scale_y_continuous(
    labels = scales::label_number(scale = 1e-3, suffix = "K"),
    expand = expansion(mult = c(0, 0.05))
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 90, hjust = 1, margin = margin(t = 10), size = 10)
  )



# Check the distribution of user type across month
ggplot(bikeshare_long, aes(x = mnth, y = count, fill = user_type)) + 
  geom_col(position = "dodge", alpha = 0.8) +
  theme_minimal() +
  labs(
    x = "Month", 
    y = "Count", 
    title = "Comparison of Registered vs Casual Riders Across Months",
    subtitle = "Bikeshare usage is predominately high in September and October for registered users.",
    caption = "Data provided by the Bike Sharing Systems dataset",
    fill = "User Type"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 12, color = "darkblue"), 
    plot.subtitle = element_text(hjust = 0.5)
  )



## Hour variable 
# check for bikeshare distribution amongst registered and casual users in the hour variable
bikesharenew %>% 
  group_by(hr) %>% 
  summarize(totalcnt = sum(cnt), casualusers = sum(casual), registeredusers = sum(registered)) %>% 
  print(n = 30)





# check the distribution overall for all bikeshare users 
ggplot(bikesharenew, aes(x = hr, y = cnt)) +
  geom_col(fill = "skyblue", alpha = 0.8) +
  theme_minimal() +
  labs(
    x = "Hour",
    y = "Total Bikeshare Riders",
    title = "Total Rider Counts by Hour",
    subtitle = "Across the day, bikeshare ridership is at its peak around 8am and 5pm - 6pm.",
    caption = "Data provided by the Bike Sharing Systems dataset"
  ) +
  scale_y_continuous(
    labels = scales::label_number(scale = 1e-3, suffix = "K"),
    expand = expansion(mult = c(0, 0.05))) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# visualize the distribution of the registered bikeshare users **bikeshare is mostly used during rush hour 8am and 5-6pm. 
ggplot(bikesharenew, aes(x = hr, y = registered)) +
  geom_col(fill = "skyblue", alpha = 0.8) +
  theme_minimal() +
  labs(
    x = "Hour",
    y = "Total Registered Riders",
    title = "Total Registered Rider Counts by Hour",
    subtitle = "Across the day, registered bikeshare ridership is at its peak around 8am and 5pm - 6pm.",
    caption = "Data provided by the Bike Sharing Systems dataset"
  ) +
  scale_y_continuous(
    labels = scales::label_number(scale = 1e-3, suffix = "K"),
    expand = expansion(mult = c(0, 0.05))) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# visualize the distribution of the casual bikeshare users  ** casual bikeshare users have a farther distributon of utilized bikeshare usage between 8am - 7 pm. 
ggplot(bikesharenew, aes(x = hr, y = casual)) +
  geom_col(fill = "skyblue", alpha = 0.8) +
  theme_minimal() +
  labs(
    x = "Hour",
    y = "Total Casual Riders",
    title = "Total Casual Rider Counts by Hour",
    subtitle = "Across the day, casual bikeshare ridership is incrementally increasing between 7am to 12pm.",
    caption = "Data provided by the Bike Sharing Systems dataset"
  ) +
  scale_y_continuous(
    labels = scales::label_number(scale = 1e-3, suffix = "K"),
    expand = expansion(mult = c(0, 0.05))) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# Create the bar plot with comparison between registered and casual users
ggplot(bikeshare_long, aes(x = hr, y = count, fill = user_type)) + 
  geom_col(position = "dodge", alpha = 0.8) +
  theme_minimal() +
  labs(
    x = "Hour", 
    y = "Count", 
    title = "Comparison of Registered vs Casual Riders Across Hours",
    subtitle = "The bar plot shows the count of registered and casual riders across different the day.",
    caption = "Data provided by the Bike Sharing Systems dataset",
    fill = "User Type"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 15, color = "darkblue"), 
    plot.subtitle = element_text(hjust = 0.5)
  )

## Weekday variable
# check for bikeshare distribution amongst registered and casual users in the weekday variable
bikesharenew %>% 
  group_by(weekday) %>% 
  summarize(totalcnt = sum(cnt), casualusers = sum(casual), registeredusers = sum(registered))

# visualize the distribution of all bikeshare users per day *** so far riders are consistently being used with friday being the highest. 
ggplot(bikesharenew, aes(x = weekdaynew, y = cnt)) +
  geom_col(fill = "skyblue", alpha = 0.8) +
  theme_minimal() +
  labs(
    x = "Weekday",
    y = "Total Rider",
    title = "Total Riders by Weekday",
    subtitle = "Across the day, bikeshare ridership is at its peak around Thursday and Friday.",
    caption = "Data provided by the Bike Sharing Systems dataset"
  ) +
  scale_y_continuous(
    labels = scales::label_number(scale = 1e-3, suffix = "K"),
    expand = expansion(mult = c(0, 0.05))) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# visualize the registered rideshare users across weekday *** registered users are mostly weekday users. 
ggplot(bikesharenew, aes(x = weekdaynew, y = registered)) +
  geom_col(fill = "skyblue", alpha = 0.8) +
  theme_minimal() +
  labs(
    x = "Weekday",
    y = "Total Registered Riders",
    title = "Total Registered Riders by Weekday",
    subtitle = "Across the day, registered bikeshare ridership is at its peak around Wednesday and Thursday",
    caption = "Data provided by the Bike Sharing Systems dataset"
  ) +
  scale_y_continuous(
    labels = scales::label_number(scale = 1e-3, suffix = "K"),
    expand = expansion(mult = c(0, 0.05))) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# visualize the casual rideshare users across weekday *** most casual users are on the weekends.
ggplot(bikesharenew, aes(x = weekdaynew, y = casual)) +
  geom_col(fill = "skyblue", alpha = 0.8) +
  theme_minimal() +
  labs(
    x = "Weekday",
    y = "Total Casual Riders",
    title = "Total Casual Riders by Weekday",
    subtitle = "Across the day, casual bikeshare ridership is at its peak during the weekends.",
    caption = "Data provided by the Bike Sharing Systems dataset"
  ) +
  scale_y_continuous(
    labels = scales::label_number(scale = 1e-3, suffix = "K"),
    expand = expansion(mult = c(0, 0.05))) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# Comparison between casual and registered
ggplot(bikeshare_long, aes(x = weekday, y = count, fill = user_type)) + 
  geom_col(position = "dodge", alpha = 0.8) +
  theme_minimal() +
  labs(
    x = "Weekday", 
    y = "Count", 
    title = "Comparison of Registered vs Casual Riders Across Weekday",
    subtitle = "Bikeshare usage is high for registered users, perhaps due to the workday.",
    caption = "Data provided by the Bike Sharing Systems dataset",
    fill = "User Type"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 15, color = "darkblue"), 
    plot.subtitle = element_text(hjust = 0.5)
  )


## Workingday variable
# check for bikeshare distribution amongst registered and casual users in the workingday variable
bikesharenew %>% 
  group_by(workingday) %>% 
  summarize(totalcnt = sum(cnt), casualusers = sum(casual), registeredusers = sum(registered)) 

# visualize the distribution of count by workingday
ggplot(bikesharenew, aes(x = workingday, y = cnt)) +
  geom_col(fill = "skyblue", alpha = 0.8) +
  theme_minimal() +
  labs(
    x = "Workingday",
    y = "Total Riders",
    title = "Total Riders by Workingday",
    subtitle = "Across the workingday,  bikeshare ridership is majority used during workingdays",
    caption = "Data provided by the Bike Sharing Systems dataset"
  ) +
  scale_y_continuous(
    labels = scales::label_number(scale = 1e-3, suffix = "K"),
    expand = expansion(mult = c(0, 0.05))) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# visualize the distribution of registered users by workingday *** registered bikeshare users utilize the bikeshare during workingdays mostly. 
ggplot(bikesharenew, aes(x = workingday, y = registered)) +
  geom_col(fill = "skyblue", alpha = 0.8) +
  theme_minimal() +
  labs(
    x = "Workingday",
    y = "Total Registered Riders",
    title = "Total Registered Riders by Workingday",
    subtitle = "Across the workingday, registered bikeshare ridership is majority used during workingdays",
    caption = "Data provided by the Bike Sharing Systems dataset"
  ) +
  scale_y_continuous(
    labels = scales::label_number(scale = 1e-3, suffix = "K"),
    expand = expansion(mult = c(0, 0.05))) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# visualize the distribution of casual users by workingday *** Casual users use the bikeshares irregardless of the day
ggplot(bikesharenew, aes(x = workingday, y = casual)) +
  geom_col(fill = "skyblue", alpha = 0.8) +
  theme_minimal() +
  labs(
    x = "Workingday",
    y = "Total Casual Riders",
    title = "Total Casual Riders by Workingday",
    subtitle = "Across the workingday, casual bikeshare ridership is similar in usage either on a workingday or holiday.",
    caption = "Data provided by the Bike Sharing Systems dataset"
  ) +
  scale_y_continuous(
    labels = scales::label_number(scale = 1e-3, suffix = "K"),
    expand = expansion(mult = c(0, 0.05))) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )


# Create the bar plot with comparison between registered and casual users
ggplot(bikeshare_long, aes(x = workingdayrecode, y = count, fill = user_type)) + 
  geom_col(position = "dodge", alpha = 0.8) +
  theme_minimal() +
  labs(
    x = "Working Day", 
    y = "Count", 
    title = "Comparison of Registered vs Casual Riders Across Working Days",
    subtitle = "The bar plot shows the count of registered and casual riders across working and non-working days.",
    caption = "Data provided by the Bike Sharing Systems dataset",
    fill = "User Type"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 20, color = "darkblue"), 
    plot.subtitle = element_text(hjust = 0.5)
  )


# check for bikeshare distribution amongst registered and casual users in the weather variable
bikesharenew %>% 
  group_by(weathersit) %>% 
  summarize(totalcnt = sum(cnt), casualusers = sum(casual), registeredusers = sum(registered))


#breakdown by weather 
df = aggregate(cnt~weatherrecode, bikesharenew, sum)

bikesharenew %>% 
  group_by(weatherrecode) %>% 
  summarize(totalcnt = sum(cnt), totalregistered = sum(registered), totalregisteredper = (totalregistered/totalcnt)*100, totalcasual = sum(casual), totalcasualper = (totalcasual/totalcnt)*100)


## Weather variable
# visualize the distribution of total users by weather *** Most of the registered users use bikes when the weather is clear and few clouds
ggplot(bikesharenew, aes(x = weatherrecode, y = cnt)) +
  geom_col(fill = "skyblue", alpha = 0.8) +
  theme_minimal() +
  labs(
    x = "Weather",
    y = "Total Riders",
    title = "Total Riders by Weather",
    subtitle = "Across the weather categories, bikeshare ridership is mostly used when the weather is clear with a few clouds.",
    caption = "Data provided by the Bike Sharing Systems dataset"
  ) +
  scale_y_continuous(
    labels = scales::label_number(scale = 1e-3, suffix = "K"),
    expand = expansion(mult = c(0, 0.05))) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# visualize the distribution of casual users by weather
ggplot(bikesharenew, aes(x = weathersit, y = casual)) +
  geom_col(fill = "skyblue", alpha = 0.8) +
  theme_minimal() +
  labs(
    x = "Weather",
    y = "Total Casual Riders",
    title = "Total Casual Riders by Weather",
    subtitle = "Across the weather categories, casual bikeshare ridership is mostly used when the weather is clear with a few clouds.",
    caption = "Data provided by the Bike Sharing Systems dataset"
  ) +
  scale_y_continuous(
    labels = scales::label_number(scale = 1e-3, suffix = "K"),
    expand = expansion(mult = c(0, 0.05))) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )


# visualize the distribution of registered users by weather
ggplot(bikesharenew, aes(x = weathersit, y = registered)) +
  geom_col(fill = "skyblue", alpha = 0.8) +
  theme_minimal() +
  labs(
    x = "Weather",
    y = "Total Registerd Riders",
    title = "Total Registered Riders by Weather",
    subtitle = "Across the weather categories, registered bikeshare ridership is mostly used when the weather is clear with a few clouds.",
    caption = "Data provided by the Bike Sharing Systems dataset"
  ) +
  scale_y_continuous(
    labels = scales::label_number(scale = 1e-3, suffix = "K"),
    expand = expansion(mult = c(0, 0.05))) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

names(bikeshare_long)
# Create the bar plot with comparison between registered and casual users
ggplot(bikesharenew, aes(x = weatherrecode, y = cnt)) + 
  geom_col(position = "dodge", alpha = 0.8) +
  theme_minimal() +
  labs(
    x = "Weather", 
    y = "Count", 
    title = "Comparison of Registered vs Casual Riders Across Weather",
    subtitle = "The bar plot shows the number of bikeshares decreases .",
    caption = "Data provided by the Bike Sharing Systems dataset",
    fill = "User Type"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 12, color = "darkblue"), 
    plot.subtitle = element_text(hjust = 0.5)
  )


# First, reorder the levels of weatherrecode based on the count
bikesharenew$weatherrecode <- reorder(bikesharenew$weatherrecode, bikesharenew$cnt, FUN = sum)

# Then, plot the data with reordered weatherrecode
ggplot(bikesharenew, aes(x = weatherrecode, y = cnt)) + 
  geom_col(position = "dodge", alpha = 0.8) +
  theme_minimal() +
  labs(
    x = "Weather", 
    y = "Count", 
    title = "Comparison of Registered vs Casual Riders Across Weather",
    subtitle = "The bar plot shows bikeshares increase as the weather conditions improve.",
    caption = "Data provided by the Bike Sharing Systems dataset",
    fill = "User Type"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 12, color = "darkblue"), 
    plot.subtitle = element_text(hjust = 0.5)
  ) +
  scale_y_continuous(
    labels = scales::label_number(scale = 1e-3, suffix = "K")
  ) 



str(bikesharenew)

#check for differences in temp, windspeed, relative humidity

bikesharenew %>% 
  group_by(tempnew) %>% 
  summarize(totalcnt = sum(cnt), casualusers = sum(casual), registeredusers = sum(registered)) %>% 
  print(n = 60)


###
#find the number of users by temperature
aggregate(cnt~tempnew, bikesharenew, sum)

## Temperature variable
#find the number of users between 60 to 76 degrees Celsius
bikesharenew %>% 
  filter(tempnew >= 60 & tempnew <=76) %>% 
  select(cnt, registered, casual) %>% 
  summarize(meancnt = mean(cnt), meanregistered = mean(registered), meancasual = mean(casual))

# visualize the distribution of riders by temperature *** as the weather increases bikeshare usage increases
ggplot(bikesharenew, aes(tempnew, cnt)) +
  geom_col(fill = "skyblue", alpha = 0.8) +
  theme_minimal() +
  labs(x = "Temperature in Celsius", 
       y = "Number of Riders", 
       title = "The Total Number of Riders Across Temperature",
       subtitle = "The highest number of bikeshares is seen between approximately 66 through 82 degrees celsius.",
       caption = "Data is provided by the Bike Sharing Systems dataset") +
  theme(
  plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
  plot.subtitle = element_text(hjust = 0.5),
  axis.text.x = element_text(angle = 45, hjust = 1)
  )


# visualize the distribution of registered riders by temperature *** consistent with the top line findings
ggplot(bikesharenew, aes(tempnew, registered)) +
  geom_col(fill = "skyblue", alpha = 0.8) +
  theme_minimal() +
  labs(x = "Temperature in Celsius", 
       y = "Number of Registered Riders", 
       title = "The Total Number of Registered Riders Across Temperature",
       subtitle = "The highest number of registered bikeshares is seen between approximately 62 through 77 degrees celsius.",
       caption = "Data is provided by the Bike Sharing Systems dataset") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )


# visualize the distribution of casual riders by temperature *** consistent with the top line findings
ggplot(bikesharenew, aes(tempnew, casual)) +
  geom_col(fill = "skyblue", alpha = 0.8) +
  theme_minimal() +
  labs(x = "Temperature in Celsius", 
       y = "Number of Casual Riders", 
       title = "The Total Number of Casual Riders Across Temperature",
       subtitle = "The highest number of casual bikeshares is seen between approximately 70 through 77 degrees celsius.",
       caption = "Data is provided by the Bike Sharing Systems dataset") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )


# Create the bar plot with comparison between registered and casual users
ggplot(bikeshare_long, aes(x = tempnew, y = count, fill = user_type)) + 
  geom_col(position = "dodge", alpha = 0.8) +
  theme_minimal() +
  labs(
    x = "Temperature", 
    y = "Count", 
    title = "Comparison of Registered vs Casual Riders Across Temperature",
    subtitle = "The bar plot shows the count of registered and casual riders across temperature",
    caption = "Data provided by the Bike Sharing Systems dataset",
    fill = "User Type"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 15, color = "darkblue"), 
    plot.subtitle = element_text(hjust = 0.5)
  )

# Comparison of humidity for casual and registered users
bikesharenew %>% 
  group_by(hum) %>% 
  summarize(totalcnt = sum(cnt), casualusers = sum(casual), registeredusers = sum(registered)) %>% 
  print(n = 60)

# visualize the distribution of total riders by humidity 
ggplot(bikesharenew, aes(humidity, cnt)) +
  geom_col(fill = "skyblue", alpha = 0.8) +
  theme_minimal() +
  labs(x = "Humidity", 
       y = "Number of Riders", 
       title = "The Total Number of Riders Across Humidity",
       subtitle = "The highest number of bikeshares is seen approximately when the humidity is 80.",
       caption = "Data is provided by the Bike Sharing Systems dataset") +
  theme(plot.title = element_text(hjust = 0.5, 
                                  size = 20, 
                                  color = "darkblue"),
        plot.subtitle = element_text(hjust = .5))

# visualize the distribution of registered riders by humidity 
ggplot(bikesharenew, aes(humidity, registered)) +
  geom_col(fill = "skyblue", alpha = 0.8) +
  theme_minimal() +
  labs(x = "Humidity", 
       y = "Number of Registered Riders", 
       title = "The Total Number of Registered Riders Across Humidity",
       subtitle = "The highest number of bikeshares is seen approximately when the humidity is 80",
       caption = "Data is provided by the Bike Sharing Systems dataset") +
  theme(plot.title = element_text(hjust = 0.5, 
                                  size = 20, 
                                  color = "darkblue"),
        plot.subtitle = element_text(hjust = .5))

# visualize the distribution of casual riders by humidity 
ggplot(bikesharenew, aes(humidity, casual)) + 
  geom_col(fill = "skyblue", alpha = 0.8) +
  theme_minimal() +
  labs(x = "Humidity", 
       y = "Number of Casual Riders", 
       title = "The Total Number of Casual Riders Across Humidity",
       subtitle = "The highest number of bikeshares is seen approximately when the humidity is 55.",
       caption = "Data is provided by the Bike Sharing Systems dataset") +
  theme(plot.title = element_text(hjust = 0.5, 
                                  size = 20, 
                                  color = "darkblue"),
        plot.subtitle = element_text(hjust = .5))


bikesharenew %>% 
  group_by(hr) %>% 
  summarize(totalcnt = sum(cnt), casualusers = sum(casual), registeredusers = sum(registered)) %>% 
  print(n = 60) 

# Create the bar plot with comparison between registered and casual users
ggplot(bikeshare_long, aes(x = humidity, y = count, fill = user_type)) + 
  geom_col(position = "dodge", alpha = 0.8) +
  theme_minimal() +
  labs(
    x = "Humidity", 
    y = "Count", 
    title = "Comparison of Registered vs Casual Riders Across Humidity",
    subtitle = "The bar plot shows the count of registered and casual riders across humidity",
    caption = "Data provided by the Bike Sharing Systems dataset",
    fill = "User Type"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 20, color = "darkblue"), 
    plot.subtitle = element_text(hjust = 0.5)
  )


bikesharenew %>% 
  group_by(weathersit, seasonrecode) %>% 
  filter(weathersit == 4) %>% 
  summarize(
    totalcnt = sum(cnt), 
    casualusers = sum(casual), 
    registeredusers = sum(registered), 
    .groups = "drop"
  )

## Season variable
# Show the distribution of total riders by weather per season  *** there are no bikeshares in heavy rain or ice pallets
ggplot(bikesharenew, aes(seasonrecode, cnt)) + 
  geom_boxplot(fill = "skyblue", alpha = 0.8) +
  theme_minimal() +
  labs(x = "Seasons", 
       y = "Count", 
       title = "The Total Number of Riders Across Season and Weather", 
       subtitle = "According to the boxplot, there is less bikeshare users during the winter.",
       caption = "Data is provided by the Bike Sharing Systems dataset") +
  theme(plot.title = element_text(hjust = 0.5, 
                                  size = 20, 
                                  color = "darkblue"), 
        plot.subtitle = element_text(hjust = .5))



# Create the bar plot with comparison between registered and casual users
ggplot(bikeshare_long, aes(x = weatherrecode, y = count, fill = user_type)) + 
  geom_col(position = "dodge", alpha = 0.8) +
  theme_minimal() +
  labs(
    x = "Seasons", 
    y = "Count", 
    title = "Comparison of Registered vs Casual Riders Across Season and Weather",
    subtitle = "The bar plot shows the count of registered and casual riders across different weather conditions.",
    caption = "Data provided by the Bike Sharing Systems dataset",
    fill = "User Type"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 20, color = "darkblue"), 
    plot.subtitle = element_text(hjust = 0.5)
  )

## Time variable
# Visualize total users over time
ggplot(bikesharenew, aes(x = dteday, y = cnt)) +
  geom_line(color = "skyblue", size = 1) +
  theme_minimal() +
  labs(
    x = "Date",
    y = "Total Rider Count",
    title = "Daily Rider Counts Over Time",
    subtitle = "Showing the trend of bike sharing ridership across dates",
    caption = "Data provided by the Bike Sharing Systems dataset"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5)
  )

# visualize registered bikeshare users across the date
ggplot(bikesharenew, aes(x = dteday, y = registered)) +
  geom_line(color = "skyblue", size = 1) +
  theme_minimal() +
  labs(
    x = "Date",
    y = "Total Rider Count",
    title = "Daily Rider Counts Over Time",
    subtitle = "Showing the trend of bike sharing ridership across dates",
    caption = "Data provided by the Bike Sharing Systems dataset"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5)
  )

# Create the line plot
ggplot(bikeshare_long, aes(x = dteday, y = count, color = user_type)) +
  geom_line(size = 1) +
  theme_minimal() +
  labs(
    x = "Date",
    y = "Total Rider Count",
    title = "Daily Rider Counts for Registered and Casual Users Over Time",
    subtitle = "Comparing trends between registered and casual users",
    caption = "Data provided by the Bike Sharing Systems dataset",
     color = "User Type"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5)
  )





#create a bar chart to show the total users per month
aggregate(cnt~mnth, bikesharenew, sum) #numerical representation of total users per month
ggplot(monthlysum, aes(mnth, totalusers)) + #bar chart of total users per month
  geom_col()

ggplot(bikesharenew, aes(holiday, cnt)) + #bar chart of total users per month
  geom_col()

aggregate(cnt~yr+registered+casual, bikesharenew, sum) #

bikesharenew %>% 
  group_by(yr) %>% 
  summarize(totalcnt = sum(cnt), casualusers = sum(casual), registeredusers = sum(registered))

bikesharenew %>% 
  group_by(hr) %>% 
  summarize(totalcnt = sum(cnt), casualusers = sum(casual), registeredusers = sum(registered)) %>% 
  print(n = 50)




aggregate(registered~mnth, bikesharenew, sum)
aggregate(casual~mnth, bikesharenew, sum)
aggregate(registered~holiday,bikesharenew,sum)



#How do the time variables relate to each other? 
cor(bikesharenew$holiday, bikesharenew$weekday)



#Is it worth reporting summary stats by one or more of those time variables? 


bikesharenew %>% 
  group_by()





#Based on the output 

• Identify any correlations between the independent variables and the dependent variable.

str(bikeshare)

bikesharecorr = bikesharenew %>% select_if(is.numeric)

cor_matrix = cor(bikesharecorr)

corrplot(cor_matrix, method = "circle", type = "upper", tl.col = "black", tl.srt = 45)

cor(bikesharenew$temp, bikesharenew$casual) # there is a moderately weak correlation 0.45 between the temperature and casual users
cor(bikesharenew$temp, bikesharenew$cnt) #there is a moderately weak correlation 0.40 between temp and total users
cor(bikesharenew$workingday,bikesharenew$casual) #there is a negatively weak correlation between workingday and casual users

#There is a possible correlation between temperature, season, and hour of the day with a moderately weak correlation with both casual and registered bikeshare users. 





Set up
• Split your data into test and training sets.

#set seed for reproducibility 
set.seed(90210)

## Partition the data 
# Split your training set into training and validation sets or use k-fold CV for validation.

# train/validate/test splits - simple random sampling

# Create a test set with 20% of the original dataset
test_index_simple = 
  createDataPartition(
    1:nrow(bikesharenew), # This avoids stratification
    times = 1, 
    p = 0.2
  )

# Split the test set 
test_set_simple = 
  bikesharenew[test_index_simple[[1]], ]

training_set_simple = 
  bikesharenew[-test_index_simple[[1]], ]

# Create a validation set 
validation_index_simple = 
  createDataPartition(
    1:nrow(training_set_simple),
    times = 1,
    p = 0.3
  )

# Split the remaining training set into validation and training sets
validation_set_simple = 
  training_set_simple[validation_index_simple[[1]], ]

training_set_simple = 
  training_set_simple[-validation_index_simple[[1]], ]



str(bikesharenew)



• Get univariate summaries of all variables in the data set.
describe(bikesharenew)


• Produce pairs plots of the data.
bikesharenew %>% 
  ggpairs(cardinality_threshold = 25) 

bikesharenew %>% 
  ggpairs()




  
#Basbikeshare#Based on the 


Include visualizations (scatter plots, correlation heat maps, etc.) in the main body only if they support your main point. 

#Visualize the distribution weather and registered users
ggplot(bikesharenew, aes(weathersit, registered)) +
  geom_boxplot() 

ggplot(bikeshare, aes(temp,cnt)) +
  geom_point() +
  facet_wrap(~season)

ggplot(bikeshare, aes(temp, registered)) +
  geom_point() + 
  facet_wrap(~season)

ggplot(bikeshare, aes(weatherrecode, registered)) +
  geom_boxplot() +
  facet_wrap(~mnth) 

ggplot(bikesharenew, aes(, registered)) +
  geom_point()

# we see around May to November that the weather light snow and rain, that the count of registered users increases and remains roughly the same
# as compared to clear partly cloudy days where the usage remains primarily consistent. 

ggplot(bikeshare, aes(weekday,registered )) +
  geom_point()  

str(bikeshare)

names(bikesharenew)

# Build a regression model 

#Build a Multiple Linear Regression model 
#using a combination of your judgment and 
#automated model selection procedures to select your final model. 
#Provide justification of variables and transformations included and excluded. 
#Report the following outputs from the model: 
#Coefficients for each independent variable. 
#R-squared and adjusted R-squared values. 
#RMSE values. 
#Significance (p-values) for each independent variable. 
#Use visuals like residual plots to assess model fit. 


# set seed for reproducibility 
set.seed(90210)

### Step 1: Create a test/train split 

bikesharenew_partition = 
  createDataPartition(
    1:nrow(bikesharenew),
    times = 1, 
    p = .3
  )

bikesharenew_train = 
  bikesharenew[-bikesharenew_partition[[1]], ]


bikesharenew_test = 
  bikesharenew[bikesharenew_partition[[1]], ]


## Step 2: Data Exploration

str(bikesharenew) #numeric variables are temp, atemp, humidity, windspeed

bikesharenew_eda =
  bikesharenew_train %>%  
    select(
      where(is.numeric), 
      instant,
      dteday,
      windspeed,
      yr,
      mnth,
      weekday,
      temp,
      hum, 
      casual,
      registered,
      holidayrecode,
      workingdayrecode,
      weatherrecode,
      seasonrecode
    )

bikesharenew_eda %>%  
  ggpairs(aes(color = seasonrecode, alpha = 0.3))

bikesharenew_eda %>% 
  ggpairs(aes(color = yr, alpha = 0.3))

bikesharenew_eda %>% 
  ggpairs(aes(color = mnth, alpha = 0.3))

bikesharenew_eda %>% 
  ggpairs(aes(color = holidayrecode, alpha = 0.3))

bikesharenew_eda %>% 
  ggpairs(aes(color = weekday, alpha = 0.3))

bikesharenew_eda %>% 
  ggpairs(aes(color = workingdayrecode, alpha = 0.3))

bikesharenew_eda %>% 
  ggpairs(aes(color = weatherrecode, alpha = 0.3))


### Step 3: Data pre-processing ----
# There is no need to perform data pre-processing as the
# dataset has no missing values. 
anyNA(bikesharenew_eda)


### Step 4: Feature Engineering ----
## Regularize the numeric variables using the caret package
## Add quadratic terms for each of the regularized variables

standardizer = # create a procedure to standardize data (store means and sds)
  bikesharenew_train %>%  
  select(
    where(is.numeric), 
    instant,
    dteday,
    yr,
    mnth,
    weekday,
    casual,
    windspeed,
    temp,
    hum, 
    registered,
    holidayrecode,
    workingdayrecode,
    weatherrecode,
    seasonrecode
  ) %>% 
  preProcess(
      method = c("center","scale")
  )

bikesharenew_train_standardized = 
  predict(standardizer, bikesharenew_train)

### Step 5: Feature & Model Selection ----
## build a kitchen sink model: 
## include all regularized and quadratic terms and all categorical variables



#this model keeps humidity and windspeed (even the polynominal term for windspeed)
f1 = lm(
  registered ~
    yr +
    dteday +
    atemp +
    mnth +
    weekday +
    temp +
    hum +
    casual +
    windspeed +
    holidayrecode +
    workingdayrecode +
    weatherrecode +
    seasonrecode +
    I(temp ^ 2) +
    I(casual ^2) +
    I(windspeed ^2),
  data = bikesharenew_train_standardized
)

#model two - removing humidity and windspeed as these are not statistically significant variables in the model
#the polynominal term for windspeed is removed
f2 = lm(
  registered ~
    yr +
    dteday +
    windspeed +
    mnth +
    weekday +
    temp +
    casual +
    holidayrecode +
    workingdayrecode +
    weatherrecode +
    seasonrecode +
    I(temp ^ 2) +
    I(casual ^2),
  data = bikesharenew_train_standardized
)


#model three - removing humidity and windspeed as these are not statistically significant variables in the model
#the polynominal term for windspeed is removed
f3 = lm(
  registered ~
    yr +
    mnth +
    weekday +
    temp +
    casual +
    holidayrecode +
    workingdayrecode +
    weatherrecode +
    seasonrecode +
    I(temp ^ 2) +
    I(casual ^2) +
    casual * temp,
  data = bikesharenew_train_standardized
)

#model 4 removes workingdayrecode as there is no statisical impact to the R2 coefficients 

f4 = lm(
  registered ~
    yr +
    mnth +
    weekday +
    temp +
    casual +
    holidayrecode +
    weatherrecode +
    seasonrecode +
    I(temp ^ 2) +
    I(casual ^2) +
    I(temp ^ 3) +
    I(casual * temp),
  data = bikesharenew_train_standardized
)


#model 5 - this model looks at cnt as the dependent variable

#this model keeps humidity and windspeed (even the polynominal term for windspeed)
f5 = lm(
  casual ~
    yr +
    dteday +
    atemp +
    mnth +
    weekday +
    temp +
    hum +
    cnt +
    windspeed +
    holidayrecode +
    workingdayrecode +
    weatherrecode +
    seasonrecode +
    I(temp ^ 2) +
    I(casual ^2) +
    I(windspeed ^2) +
    I(temp ^3),
  data = bikesharenew_train_standardized
)



## summarize and plot your kitchen sink model
## do you see evidence of model misspecification in the plots?

#test model 1
summary(f1)

plot(f1)

#test model 2
summary(f2)

plot(f2)


#test model 3
summary(f3)

plot(f3)

#test model 4
summary(f4)

plot(f4)

#test model 5

summary(f5)

plot(f5)


# Do backwards selection on your kitchen sink model 
## summarize and plot it for model 1

f_back1 = step(
  object = f1, 
  direction = "back"
)

summary(f_back1)

plot(f_back1)

# backwards selection for model 2 
f_back2 = step(
  object = f2, 
  direction = "back"
)

summary(f_back2)

plot(f_back2)

# backwards selection for model 3
f_back3 = step(
  object = f3, 
  direction = "forward"
)

summary(f_back3)

plot(f_back3)





## Do stepwise selection starting with your kitchen sink model
## summarize and plot it for model 1

f_step1 = step(
  object = f1, 
  direction = "both"
)

summary(f_step1)

plot(f_step1)



## Do stepwise selection for model 2

f_step2 = step(
  object = f2, 
  direction = "both"
)

summary(f_step2)

plot(f_step2)


## Do stepwise selection for model 3

f_step3 = step(
  object = f3, 
  direction = "both"
)

summary(f_step3)

plot(f_step3)


### Step 6: Model Evaluation ----
## Create a 10-fold partition of indices on your training set using caret
## Call that object k_fold_indices

k_fold_indices = createFolds(
  1:nrow(bikesharenew_train_standardized),
  k = 10
)

my_training_function = function(dat){
  f = lm(
    registered ~
      dteday +
      yr +
      mnth +
      weekday +
      temp +
      hum +
      casual +
      windspeed +
      holidayrecode +
      workingdayrecode +
      weatherrecode +
      seasonrecode,
    data = dat
  )
  f
}


# Declare an evaluation function
my_evaluation_function <- function(model, new_data) {
  
  # remove any NA values for simplicity
  new_data <- 
    new_data |>
    na.omit()
  
  preds <- predict(model, new_data)
  
  oos_r2 <- calc_rsquared(
    y = new_data$cnt,
    yhat = preds
  )
  
  oos_rmse <- ((new_data$cnt - preds) ^ 2) |>
    mean() |>
    sqrt()
  
  tibble(
    r2 = oos_r2,
    rmse = oos_rmse
  )
}

## loop over your k-fold indices to get k-fold cross validation of your model
cv_results <- 
  k_fold_indices |>
  map( # lapply() also works.
    function(fold){
      train <- bikesharenew_train_standardized[-fold, ]
      
      test <- bikesharenew_train_standardized[fold, ]
      
      f <- my_training_function(train)
      
      eval <- my_evaluation_function(f, test) 
      
      eval
    }
  ) |>
  bind_rows()


summary(cv_results)
## Plot a histogram or density of your k-fold r-squared and RMSE
## Did your cross-validation results do better or worse than training on the whole set?
## Do you think this model is good enough to move forward, or do we need to do more tweaking?

cv_results |>
  ggplot(aes(x = r2)) + 
  geom_density(fill = "red", alpha = 0.5)

cv_results |>
  ggplot(aes(x = rmse)) + 
  geom_density(fill = "red", alpha = 0.5)

### Step 7: Predictions and Conclusions ----

## Get out-of sample predictions from your model using the test set
## Compare the R-squared and RMSE. Are they worse or better than the CV above?

bikesharenew_test_standardized <- predict(standardizer, bikesharenew_test) 

oos_predictions <- predict(f_step, bikesharenew_test_standardized)

oos_results <- tibble(
  r2 = calc_rsquared(
    y = bikesharenew_test_standardized$cnt,
    yhat = oos_predictions |> na.omit()
  ),
  rmse = ((bikesharenew_test_standardized$cnt - oos_predictions) ^ 2) |> #– RMSE values. 
    mean(na.rm = TRUE) |>
    sqrt()
)


## Train your model on the whole dataset, using the specification from above

f_final <- my_training_function(bikesharenew)

## Use tidy() from the broom package to extract your coefficients

f_final_coeffs <- tidy(f_final)

print(f_final_coeffs) # – Coefficients for each independent variable. 

## Create a barplot with 95% confidence intervals for all coefficients 
## except the intercept
f_final_coeffs |>
  filter(term != "(Intercept)") |>
  ggplot(aes(x = term, y = estimate)) + 
  geom_bar(fill = "skyblue", stat = "identity") + 
  geom_errorbar(
    aes(x = term, ymin = estimate - 1.96 * std.error, ymax = estimate + 1.96 * std.error), 
    width = 0.4,
    color = "orange", 
    alpha = 0.9
  )

# Correcting for linear regression assummptions that were violated

# Marginal effects

## set up some data 
### Fit a model ----
# Note: this example assumes you've already gone through our modeling steps

f3 = lm(
  registered ~
    yr +
    mnth +
    weekday +
    temp +
    casual +
    holidayrecode +
    workingdayrecode +
    weatherrecode +
    seasonrecode +
    I(temp ^ 2) +
    I(casual ^2) +
    casual * temp,
  data = bikesharenew_train_standardized
)


summary(f3)

plot(f3)


### Extract coefficients ----

coefs <- tidy(f3)

# plot our model and points
f3 |>
  ggplot(aes(x = , y = registered)) + 
  geom_point(alpha = 0.3) + 
  geom_line(
    aes(x = x, y = coefs$estimate[1] + coefs$estimate[2] * x + coefs$estimate[3] * x ^ 2),
    linewidth = 1.25,
    color = "red"
  )


dat <- 
  dat |>
  mutate(
    mfx = coefs$estimate[2] + 2 * coefs$estimate[3] * dat$x
  )


### Report summary statistics of the marginal effect ----
summary(dat$mfx)

dat |> # so the mfx changes depending on x
  ggplot(aes(x = x, y = mfx)) + 
  geom_line()


dat2 <- tibble(
  x = dat$x,
  y = 2 - 1.5 * x + 0.7 * x ^ 2 + rnorm(1000, mean = 0, sd = 10)
)

summary(dat2)

pairs(dat2)




# heteroskedasticity 


#multicollinearity

f3 <- tibble(
  x1 = rnorm(n = 1000, mean = 7, sd = 3),
  
  x2 = rnorm(n = 1000, mean = 3, sd = 7),
  
  x3 = x2 + rnorm(n = 1000, mean = 0, sd = 0.5),
  
  y = 3 + 5 * x1 + 3 * x2 + rnorm(n = 1000, mean = 0, sd = 20)
)


# Discuss any limitations of the model, suhc as the potential for omitted variable or biases in the data. 



