

library(readr)
library (dplyr)
library (tidyr)
library(caret)

cantcha <- read_csv("data/Cantcha Data Gather (Responses) - Form Responses 1.csv")

#EDA of full dataset
df <- cantcha
names(df)
colnames(df) <- c("Timestamp",  "Photo", "Drink_name", "Colour", "Cartoony", "Fonts", "Informative", "Impression" ,"label")
dim (df)
str(df)
summary(df)


#row names
names(df)

df$label <- as.factor(df$label)

#how many classes - and what are they?
num_class <- length(unique(df$label))
uniqueClasses <- as.factor(unique(df$label)) 
cat("There are ", num_class, "unique classes, which are", uniqueClasses)

#class distribution
classDis<- as.data.frame(table(df$label))
names(classDis) <- c("label","count") 
classDis

#check that they all add up
sr <- sum(classDis$count) 
nr <- nrow(df)
cat("The total of each class add up to the whole number of rows?",sr==nr )

# list all observations for missing values 
sapply(df, function(x) sum(is.na(x)))

#change for the sample percentage (tested at 0.1, 1.0, and 0.5)
sample_pc <- 0.5
df_pc <- df[sample(nrow(df), nrow(df) * sample_pc / 100), ]
nrow(df_pc)



#<begin>=====================================
# Dump redundant columns & shift class to the end column as 'label'

drop.cols <- c('Timestamp', 'Photo', "Drink_name", "Impression")
reduced <- df %>% select(-one_of(drop.cols))

#<end>=====================================


# test three algorithms

ctrl <- trainControl(method="repeatedcv", number=10, repeats=3)
# tested c5.0, SVM and KNN - with C5.0 performing on traning set best. 
#C5.0Tree
set.seed(1)
mod.c5Tree <- train(label~., data=reduced, method="C5.0Tree", trControl=ctrl)
# svmRadial
set.seed(1)
mod.svm <- train(label~., data=reduced, method="svmRadial", trControl=ctrl)
# kNN
set.seed(1)
mod.knn <- train(label~., data=reduced, method="knn", trControl=ctrl)


# collect resamples
results <- resamples(list(C5.0Tree=mod.c5Tree, svm=mod.svm, kNN=mod.knn))

scales <- list(x=list(relation="free"), y=list(relation= "free"))
dotplot(results, scales=scales, conf.level = 0.99)

# outcome 
# SVM delivers best performance
#========================================
# now tune the model
library(caret)
library(kernlab)
library(mlbench)
fitControl <- trainControl(method = "repeatedcv",
                           number = 10,
                           repeats = 10, returnResamp="all")
# Choose the features and classes

grid <- grid <- expand.grid(sigma = c(.01, .015, 0.2),
                            C = c(0.75, 0.9, 1, 1.1, 1.25))
mdl <- train(x=reduced[-5],
             y= reduced$label,
             method = "svmRadial",
             tuneGrid = grid,
             summaryFunction=twoClassSummary,	# Use AUC to pick the best model
             classProbs=TRUE,
             trControl=ctrl)
mdl
#show which attributes were most used

summary(mdl$finalModel)
summary(mdl$results)

# Tuning parameter 'model' was held constant at a value of tree
# Accuracy was used to select the optimal model using  the largest value.
# The final values used for the model were trials = 20, model = tree and winnow = FALSE.


# Testing the previous model on the testing set	
TestRes <- predict(mdl, newdata = reduced[,-5], type="raw")
confusionMatrix(TestRes, reduced$label)


# Set impression as the label

drop.cols <- c('Timestamp', 'Photo', "Drink_name", "label")
reduced.1 <- df %>% select(-one_of(drop.cols))
reduced.1$Impression <- as.factor(reduced.1$Impression)
levels(reduced.1$Impression) <- c( "Clearly Alcohol",  "Clearly non-alcoholic", "Probably Alcohol", "Probably non-alcoholic", "Really can't tell" )
# test three algorithms

ctrl <- trainControl(method="repeatedcv", number=10, repeats=3)
# tested c5.0, SVM and KNN - with C5.0 performing on traning set best. 
#C5.0Tree
set.seed(1)
mod.c5Tree <- train(Impression~., data=reduced.1, method="C5.0Tree", trControl=ctrl)
# svmRadial
set.seed(1)
mod.svm <- train(Impression~., data=reduced.1, method="svmRadial", trControl=ctrl)
# kNN
set.seed(1)
mod.knn <- train(Impression~., data=reduced.1, method="knn", trControl=ctrl)

# collect resamples
results <- resamples(list(C5.0Tree=mod.c5Tree, svm=mod.svm, kNN=mod.knn))

scales <- list(x=list(relation="free"), y=list(relation= "free"))
dotplot(results, scales=scales, conf.level = 0.99)


pairs(reduced)

fitControl <- trainControl(method = "repeatedcv",
                           number = 10,
                           repeats = 10, returnResamp="all")
# Choose the features and classes

grid <- grid <- expand.grid(sigma = c(.01, .015, 0.2),
                            C = c(0.75, 0.9, 1, 1.1, 1.25))
mdl <- train(x=reduced.1[-5],
             y= reduced.1$Impression,
             method = "svmRadial",
             tuneGrid = grid,
             trControl=ctrl)
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
TestRes <- predict(mdl, newdata = reduced.1[,-5], type="raw")
confusionMatrix(TestRes, reduced.1$Impression)
table(TestRes, reduced.1$Impression)
