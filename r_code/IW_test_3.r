
#data source: https://www.openml.org/d/40514 

library(readr)
library (dplyr)
library (tidyr)
library(caret)

BNG_credit_g<- read_csv("data/BNG_credit_g.csv")

#EDA of full dataset
df <- BNG_credit_g
dim (df)
str(df)
summary(df)
# 1 million rows of 21 of observations

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

#change for the sample percentage (tested at 0.1, 1.0, and 0.5)
sample_pc <- 0.1
df_pc <- df[sample(nrow(df), nrow(df) * sample_pc / 100), ]
nrow(df_pc)

#<begin>=================================================
# Adopt a 'one-hot' approach to the factors with V values

for(unique_value in unique(df_pc$checking_status)){
  df_pc[paste("checking_status", unique_value, sep = ".")] <- ifelse(df_pc$checking_status == unique_value, 1, 0)
}
for(unique_value in unique(df_pc$credit_history)){
  df_pc[paste("credit_history", unique_value, sep = ".")] <- ifelse(df_pc$credit_history == unique_value, 1, 0)
}
for(unique_value in unique(df_pc$purpose)){
  df_pc[paste("purpose", unique_value, sep = ".")] <- ifelse(df_pc$purpose == unique_value, 1, 0)
}
for(unique_value in unique(df_pc$savings_status)){
  df_pc[paste("savings_status", unique_value, sep = ".")] <- ifelse(df_pc$savings_status == unique_value, 1, 0)
}
for(unique_value in unique(df_pc$employment)){
  df_pc[paste("employment", unique_value, sep = ".")] <- ifelse(df_pc$employment == unique_value, 1, 0)
}
for(unique_value in unique(df_pc$personal_status)){
  df_pc[paste("personal_status", unique_value, sep = ".")] <- ifelse(df_pc$personal_status == unique_value, 1, 0)
}
for(unique_value in unique(df_pc$other_parties)){
  df_pc[paste("other_parties", unique_value, sep = ".")] <- ifelse(df_pc$other_parties == unique_value, 1, 0)
}
for(unique_value in unique(df_pc$property_magnitude)){
  df_pc[paste("property_magnitude", unique_value, sep = ".")] <- ifelse(df_pc$property_magnitude == unique_value, 1, 0)
}
for(unique_value in unique(df_pc$housing)){
  df_pc[paste("housing", unique_value, sep = ".")] <- ifelse(df_pc$housing == unique_value, 1, 0)
}
for(unique_value in unique(df_pc$job)){
  df_pc[paste("job", unique_value, sep = ".")] <- ifelse(df_pc$job == unique_value, 1, 0)
}

# further processing - replace three column values with 1/0 
df_pc<- mutate(df_pc, 
               own_telephone = if_else(own_telephone == 'yes', 1L, 0L), 
               other_payment_plans = if_else( other_payment_plans =="none", 0L, 1L),
               foreign_worker = if_else(foreign_worker == 'yes', 1L , 0L))
#<end>=====================================

#<begin>=====================================
# Dump redundant columns & shift class to the end column as 'label'
df_pc <- df_pc %>% mutate( new_class = class ) 
drop.cols <- c('checking_status', 'credit_history', 'purpose', 'savings_status', 
               'employment', 'personal_status','other_parties', 'property_magnitude',
               'housing', 'job', 'class')
reduced <- df_pc %>% select(-one_of(drop.cols))

#<end>=====================================

#<begin>=====================================
# divide the dataset into train (70%) and test (30%)

set.seed(13579) #to get repeatable data
dt = sort(sample(nrow(reduced), nrow(reduced)*.7))
trainA<-reduced[dt,]
testA<-reduced[-dt,]
#<end>=====================================
# test three algorithms

ctrl <- trainControl(method="repeatedcv", number=10, repeats=3)
# tested c5.0, SVM and KNN - with C5.0 performing on traning set best. 
#C5.0Tree
set.seed(1)
mod.c5Tree <- train(new_class~., data=trainA, method="C5.0Tree", trControl=ctrl)
# svmRadial
set.seed(1)
mod.svm <- train(new_class~., data=trainA, method="svmRadial", trControl=ctrl)
# kNN
set.seed(1)
mod.knn <- train(new_class~., data=trainA, method="knn", trControl=ctrl)


# collect resamples
results <- resamples(list(C5.0Tree=mod.c5Tree, svm=mod.svm, kNN=mod.knn))

scales <- list(x=list(relation="free"), y=list(relation= "free"))
dotplot(results, scales=scales, conf.level = 0.99)

# outcome 
# C5.0 delivers best performance
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
x <- trainA[, 1:58]
y <- trainA$new_class
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
confusionMatrix(TestRes, testA$new_class)