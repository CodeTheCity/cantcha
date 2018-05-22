
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

# 
reduced_df[["class"]] = factor(reduced_df[["class"]])
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

# The following code is inspired by the following two articles
# http://dataaspirant.com/2017/01/19/support-vector-machine-classifier-implementation-r-caret-package/ 
# and http://blog.revolutionanalytics.com/2015/10/the-5th-tribe-support-vector-machines-and-caret.html 

trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
set.seed(135)

svm_Radial <- train(class ~., data = trainA, method = "svmRadial",
                    trControl=trctrl,
                    preProcess = c("center", "scale"),
                    tuneLength = 10)

svm_Radial

test_pred <- predict(svm_Radial, newdata = testA)
test_pred


confusionMatrix(test_pred, testA$class )

# tune the model
grid <- expand.grid(sigma = c(.1, 0.2, 0.25, 0.3), C = c(0,0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 1, 1.25, 1.5, 1.75, 2,4, 5))

set.seed(135)
svm_Radial_Grid <- train(class ~., data = trainA, method = "svmRadial",
                           trControl=trctrl,
                           preProcess = c("center", "scale"),
                           tuneGrid = grid,
                           tuneLength = 10)

svm_Radial_Grid

plot(svm_Radial_Grid)

#Now test the runed model on test data
test_svm_Radial_Grid <- predict(svm_Radial_Grid, newdata = testA)

confusionMatrix(test_svm_Radial_Grid, testA$class )

