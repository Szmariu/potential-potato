# To-do
#   Finish converting the date (gives errors). Alternative - just substring the year (but that's meh)
#   Plots without outliers
#   More work on the ratingsTable

# Ideas
# Sentiment analisys of the revievs, take the average/partial sums, and put it into mainTable
# Removing outliers in contininous variables?
#


######PACKAGES######
# Install packages
# install.packages("ggplot2")
# install.packages("gridExtra")
# install.packages("dplyr")
# install.packages("tidyr")


# Load them
library(dplyr)
library(tidyr)
library(ggplot2)
library(gridExtra)

options(scipen=999) #avoiding e10 notation


######DATA IMPORT######
# Importing main data table, NaN are ommited (about 10% of all data)
mainTable <- na.omit(read.csv("data/googleplaystore.csv",header=TRUE))

# Import the ratings, NaN are ommited (about 30% of all data)
ratingsTable <- na.omit(read.csv("data/googleplaystore_user_reviews.csv",header=TRUE))

# Some basic summary
str(mainTable)
summary(mainTable)

# Renaming columns
columnsToRename = c('Reviews' = 'Reviews.Count', 'Current.Ver' = 'Current.Software.Version', 'Android.Ver' = 'Android.Version')
mainTable <- mainTable %>% plyr::rename(columnsToRename)




######FIXING DATA######
# All - Setting "Varies with device" as NaN
mainTable[mainTable == "Varies with device"] <- NA

# Rating - Fixing outiliers
mainTable$Rating[mainTable$Rating > 5] <- NA

# Size - Delete M (megabytes)
mainTable[5] <- lapply(mainTable[5], as.character)
mainTable$Size <- substr(mainTable$Size, 1, nchar(mainTable$Size) - 1)

# Type - Fixing the 0
mainTable$Type[mainTable$Type == 0] <- "Free"

# Price - Delete dollars, fix outliers
mainTable$Price <- substring(mainTable$Price,2)
mainTable$Price[mainTable$Price == "" | mainTable$Price == "veryone"] <- "0"

# Content.Rating - Deleting outliers
mainTable$Content.Rating[mainTable$Content.Rating == '' | mainTable$Content.Rating == "Unrated"] <- "Everyone"

# Genres - Taking only the main genre 
mainTable$Genres <- gsub(";.*","",mainTable$Genres)

# Last.Updated - Converting to date NOT WORKING YET!
mainTable <- mainTable[!mainTable$Last.Updated == "1.0.19",]
mainTable$Last.Updated <- gsub(",","",mainTable$Last.Updated)
# placeholder <- as.Date(mainTable$Last.Updated, format = "%B %d %Y", optional = TRUE)

# Android.Version - Take only the main part, eg. 4.3
mainTable$Android.Version <- substr(mainTable$Android.Version, 1, 3)






######DATA TYPES######
# Convert to character
for (i in c(1, 4)){
  mainTable[i] <- lapply(mainTable[i], as.character)
}

# Convert to numeric 
for (i in c(4, 5, 8)){
  mainTable[i] <- lapply(mainTable[i], as.numeric)
}
  
# Convert to factors
for (i in c(10, 13)){
  mainTable[i] <- lapply(mainTable[i], as.factor)
}
remove(i)

# Drop all unused factors (cool function)
mainTable <- droplevels.data.frame(mainTable)

# Drop the Category column
mainTable <- mainTable[-c(2,12)]





######LOOK AT THE DATA######
# Summary
str(mainTable)
summary(mainTable)

# Factors summary
summary(mainTable[c(5,6,8,9,11)])

# Basic plots
# Histograms
p1 <- ggplot(mainTable, aes(x = Rating)) + geom_histogram() 
p2 <- ggplot(mainTable, aes(x = Reviews.Count)) + geom_histogram()
p3 <- ggplot(mainTable, aes(x = Size)) + geom_histogram()
p4 <- ggplot(mainTable, aes(x = Price)) + geom_histogram()
grid.arrange(p1, p2, p3, p4, nrow = 2)

# Box plots 
p1 <- ggplot(mainTable, aes(y = Rating, 1)) + geom_boxplot() 
p2 <- ggplot(mainTable, aes(y = Reviews.Count, 1)) + geom_boxplot()
p3 <- ggplot(mainTable, aes(y = Size, 1)) + geom_boxplot()
p4 <- ggplot(mainTable, aes(y = Price, 1)) + geom_boxplot()
grid.arrange(p1, p2, p3, p4, nrow = 2)

# Scatter plots
ggplot(mainTable, aes(y = Reviews.Count, x = seq(1,length(mainTable$Reviews.Count)))) + geom_point()





######EXPORT CLEAN DATA######
write.csv(mainTable, file = "data/clean_googleplaystore.csv", row.names = FALSE)
write.csv(ratingsTable, file = "data/clean_googleplaystore_user_reviews.csv", row.names = FALSE)




