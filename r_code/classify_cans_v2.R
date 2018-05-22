
library(readr)
library (dplyr)
library (tidyr)
library(caret)

df <- read_csv("~/development/cantcha/data/Cantcha Data Gather (Responses) - Form Responses 1.csv")
str(df)
colnames(df) <- c("Timestamp", "Photo", "Name", "Colour", "Cartoon", "Fonts", "Informative", "class", "old_class")

index <- c("Clearly Alcohol", "Probably Alcohol", "Really can't tell", "Probably non-alcoholic",  "Clearly non-alcoholic")
values <- c("Yes", "Yes", "NA", "No", "No")
df$add_col <- values[match(df$class, index)]

df2<- df[df$add_col != "NA",]

dim(df2)
str(df)
summary(df2)

#col names
names(df2)

colnames(df2) <- c("Timestamp", "Photo", "Name", "Colour", "Cartoon", "Fonts", "Informative", "not_class", "ex_class", "class")
#how many classes - and what are they?
num_class <- length(unique(df2$class))
uniqueClasses <- as.character(unique(df2$class)) 
cat("There are ", num_class, "unique classes, which are", uniqueClasses)

# drop redundant colums
drop.cols <- c('Timestamp', 'Photo', 'Name', "not_class", "ex_class") 
reduced_df <- df2 %>% select(-one_of(drop.cols))

#class distribution
classDis<- as.data.frame(table(reduced_df$class))
names(classDis) <- c("label","count") 
classDis

#check that they all add up
sr <- sum(classDis$count) 
nr <- nrow(reduced_df)
cat("The total of each class add up to the whole number of rows?",sr==nr )

# list all observations for missing values 
sapply(reduced_df, function(x) sum(is.na(x)))



dim (reduced_df)
names(reduced_df)

# divide the dataset into train (70%) and test (30%)

set.seed(135) #to get repeatable data
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


ctrl <- trainControl(method="repeatedcv", number=10, repeats=3)
# tested c5.0, SVM and KNN - with C5.0 performing on traning set best. 
#C5.0Tree
set.seed(1)
mod.c5Tree <- train(class~., data=trainA, method="C5.0Tree", trControl=ctrl)
# svmRadial
set.seed(1)
mod.svm <- train(class~., data=trainA, method="svmRadial", trControl=ctrl)
# kNN
set.seed(1)
mod.knn <- train(class~., data=trainA, method="knn", trControl=ctrl)


# collect resamples
results <- resamples(list(C5.0Tree=mod.c5Tree, svm=mod.svm, kNN=mod.knn))

scales <- list(x=list(relation="free"), y=list(relation= "free"))
dotplot(results, scales=scales, conf.level = 0.95)

#Try to improve C50
#========================================
# now tune the model
library(caret)
library(C50)
library(mlbench)
fitControl <- trainControl(method = "repeatedcv",
                           number = 10,
                           repeats = 10, returnResamp="all")
# Choose the features and classes
data(trainA)
x <- trainA[, 1:4]
y <- trainA$class
grid <- expand.grid( .winnow = c(TRUE,FALSE), .trials=c(1,5,10,15,20), .model="tree" )
mdl<- train(x=x,y=y,tuneGrid=grid,trControl=fitControl,method="C5.0",verbose=FALSE)
mdl
#show which attributes were most used

summary(mdl$finalModel)
summary(mdl$results)

# Tuning parameter 'model' was held constant at a value of tree
# Accuracy was used to select the optimal model using  the largest value.
# The final values used for the model were trials = 20, model = tree and winnow = FALSE.

# visualize the resample distributions
xyplot(mdl,type = c("g", "p", "smooth"))

# Testing the previous model on the testing set	
TestRes <- predict(mdl, newdata = testA, type="raw")
confusionMatrix(TestRes, testA$class)