
library(readr)
library (dplyr)
library (tidyr)
library(caret)

df <- read_csv("~/development/cantcha/data/Cantcha Data Gather (Responses) - Form Responses 1.csv")

colnames(df) <- c("Timestamp", "Photo", "Name", "Colour", "Cartoon", "Fonts", "Informative", "class", "old_class")
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

# drop redundant colums
drop.cols <- c('Timestamp', 'Photo', 'Name', "old_class") # NB we might want to retain and encode "impression"
reduced_df <- df %>% select(-one_of(drop.cols))

dim (reduced_df)
names(reduced_df)

# divide the dataset into train (70%) and test (30%)

set.seed(13579) #to get repeatable data
dt = sort(sample(nrow(reduced_df), nrow(reduced_df)*.7))
trainA<-reduced_df[dt,]
testA<-reduced_df[-dt,]

#class distribution of TEST
classDis<- as.data.frame(table(testA$class))
names(classDis) <- c("label","count") 
classDis

#class distribution of TRAIN
classDis<- as.data.frame(table(trainA$class))
names(classDis) <- c("label","count") 
classDis

