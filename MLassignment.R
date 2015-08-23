#list all the library that will be used. 
library(caret)
library(kernlab)
library(randomForest)
library(corrplot)

# Avoid duplication and check if a data folder exists; if not then create one
if (!file.exists("data")) {dir.create("data")}

# Downloaded file URL and file destination to be saved. 
fileUrl1 <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
destfile1 <- "./data/pml-training.csv"
fileUrl2 <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"
destfile2 <- "./data/pml-testing.csv"

# download and take note of the time downloaded. 
download.file(fileUrl1, destfile = destfile1)
download.file(fileUrl2, destfile = destfile2)
dateDownloaded <- date()

# read the csv file for training 
training_data <- read.csv("./data/pml-training.csv", na.strings= c("NA",""," "))

#Pre-processing the data
# clean the data by removing columns with NAs etc
training_data_NAs <- apply(training_data , 2, function(x) {sum(is.na(x))})
training_data_clean <- training_data [,which(training_data_NAs == 0)]

# remove identifier columns such as name, timestamps etc
training_data_clean <- training_data_clean[8:length(training_data_clean)]

# Crossvalidation 70/30 split the cleaned testing data into training and cross validation
inTrain <- createDataPartition(y = training_data_clean$classe, p = 0.7, list = FALSE)
training <- training_data_clean[inTrain, ]
crossval <- training_data_clean[-inTrain, ]

# plot a correlation matrix
correlMatrix <- cor(training[, -length(training)])
corrplot(correlMatrix, order = "FPC", method = "circle", type = "lower", tl.cex = 0.8,  tl.col = rgb(0, 0, 0))

# fit a model to predict the classe using everything else as a predictor
model <- randomForest(classe ~ ., data = training)

# crossvalidate the model using the remaining 30% of data
predictCrossVal <- predict(model, crossval)
confusionMatrix(crossval$classe, predictCrossVal)

# apply the same treatment to the final testing data
test_data <- read.csv("./data/pml-testing.csv", na.strings= c("NA",""," "))
test_data_NAs <- apply(test_data, 2, function(x) {sum(is.na(x))})
test_data_clean <- test_data[,which(test_data_NAs == 0)]
test_data_clean <- test_data_clean[8:length(test_data_clean)]

# predict the classes of the test set
predictTest <- predict(model, test_data)