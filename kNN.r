#import library
library(class)
library(ggplot2)
library(caret)

#set the environment location
setwd("SET WORKING DIRECTORY")


#read .csv file
vertebralData <- read.csv("column_2C.csv", header = TRUE)


#shuffle row of dataset
set.seed(161231463) #set seed to matric number
rows <- sample(nrow(vertebralData))
vertebralData <- vertebralData[rows, ]


#split data into training and testing
set.seed(3033)
intrain <- createDataPartition(y = vertebralData$class, p= 0.7, list = FALSE)
training <- vertebralData[intrain,]
testing <- vertebralData[-intrain,]

dim(training); dim(testing)


#check missing data
anyNA(vertebralData)


#summary
summary(vertebralData)


#Training and train control
trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
set.seed(3333)


#train model and find the best value of k
#train function take the class, training dataset, method, trContril for N-fold.
knn_fit <- train(class ~., data = training, method = "knn",
                 trControl=trctrl,  preProcess = c("center","scale"), tuneLength = 20)

knn_fit


plot(knn_fit)

#make prediction
#the predict function take the model, and testing data
#i sure care, so the model already optimize
knnPredict <- predict(knn_fit,newdata = testing )

#chechk accuracy specificity and sensitivity
confusionMatrix(knnPredict, testing$class )
