#Programming 1 - Assignment 1
#Your name(s):Andrew Harper, Andrew Singh

#The data of over 1000 eBay auctions is provided in the file eBayAcution.csv. 
#Use RStudio to study this marketplace. 
#(Source: The data is adapted from this book: https://www.dataminingbook.com/book/r-2nd-edition-2023)

#1) Load the file: "eBayAcution.csv" and save it as auctionData.
auctionData = read.csv("eBayAuctions.csv")
auctionData
sum(auctionData$Duration==10)


#2) Write a code that checks if the dataset has any missing values, 
#   a code that returns the number of auctions (i.e., rows), 
#   and one to return the number of variables (i.e., columns).

#Checks if the dataset has any missing values - No NAs
anyNA(auctionData)
sum(is.na(auctionData))

#Number of Auctions - 1223 Auctions
nrow(auctionData)
#Number of Variables - 6 variables
ncol(auctionData)

#3) What is the maximum auction duration? How many auctions were open for these many days? 
#   What is the average auction duration? What percentage of the auctions have an above average duration?

#Maximum Auction Duration - 10
max(auctionData$Duration)

#How many auctions were open for these days - 220
nrow(auctionData[auctionData$Duration == 10,])

#Average Auction Duration - 6.421096
mean(auctionData$Duration)

#What Percentage of Auctions have an above average duration - 59.04%
(nrow(auctionData[auctionData$Duration > mean(auctionData$Duration),]) / nrow(auctionData))*100

#4) Create a new variable called Ratio that calculates the ratio of the closing price over the opening price 
#   for each auction and add this variable to the dataset as a new column. 
#   What's the average ratio of all auctions? What's the average ratio of 'Computer' auctions?

# Price Ratio - 
auctionData$Ratio = (auctionData$ClosePrice / auctionData$OpenPrice)

#Average Price Ration for all auctions - 119.6468
mean(auctionData$Ratio)

#Average Ratio for Computer Auctions - 21.82995
computerAuctions = auctionData[auctionData$Category == "Computer",]
mean(computerAuctions$Ratio)

#5) Create an object named "catNames" that contains the names of unique auction categories, 
#   sorted in alphabetical order. Write a code to return the number of categories stored in this object.

#Unique Categories -10 
catNames <- sort(unique(auctionData$Category))
catNames
length(catNames)


#6) Write a loop to go through "catNames" and calculate the number of auctions in each category. 
#   In so doing, save the results in a vector called "numAuctions". 
#   Write a code to return the values stored in this object.


numAuctions <- vector()

for(i in catNames){
  subset_data <- auctionData[auctionData$Category == i,]
  
  # saving value in empty vector in each iteration
  numAuctions[i] <- nrow(subset_data)
  
}

numAuctions
#7) Combine the two objects (catNames and numAuctions) into a new data frame called catInfo. 
# Write two different codes to return the fifth element of the second column in the catInfo dataframe.

catInfo = data.frame(cbind(catNames, numAuctions))
catInfo
# Write two different codes to return the fifth element of the second column in the catInfo dataframe. - 33 
catInfo[5,2]
catInfo$numAuctions[5]

#8) Write a piece of code that prints the name of each category and the number of auctions in that category.
numAuctions


#9) Create a function, called weekendTest, that checks whether a given day is a weekend (endDay of 'Sat' or 'Sun') 
#   or not and returns TRUE or FALSE (logical constants in R).
#   Then use this function to create a new variable (called Weekend) that shows if each auction had an endDay of the weekend or not. 
#   Add this variable to the dataset as a new column. How many auction ended on weekend? (Write a code that returns this value)

weekendTest <- function(day) {
  return(day %in% c('Sat', 'Sun'))
}

# Apply the function to create a new column 'Weekend'
auctionData$Weekend <- sapply(auctionData$endDay, weekendTest)
auctionData

# Count the number of auctions that ended on a weekend
numWeekendAuctions <- sum(auctionData$Weekend)
numWeekendAuctions

