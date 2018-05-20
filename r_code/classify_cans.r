
library(readr)
library (dplyr)
library (tidyr)
library(caret)

df <- read_csv("~/development/cantcha/data/Cantcha Data Gather (Responses) - Form Responses 1.csv")

colnames(df) <- c("Timestamp", "Photo", "Name", "Colour", "Cartoon", "Fonts", "Informative", "Impression", "class")
#EDA of full dataset

dim (df)
str(df)
summary(df)


#row names
names(df)

#how many classes - and what are they?
num_class <- length(unique(df$class))
uniqueClasses <- as.character(unique(df$class)) 
cat("There are ", num_class, "unique classes, which are", uniqueClasses)

#class distribution
classDis<- as.data.frame(table(df$class))
names(classDis) <- c("label","count") 
classDis

#check that they all add up
sr <- sum(classDis$count) 
nr <- nrow(df)
cat("The total of each class add up to the whole number of rows?",sr==nr )

# list all observations for missing values 
sapply(df, function(x) sum(is.na(x)))
§