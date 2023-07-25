# ------------------------------------
# Author: Maithreyi Mydur
# ------------------------------------

#
# Install these packages from R-Studio 
#
installed.packages("ggplot2")
install.packages("caret", dependencies = c("Depends", "Suggests"))
installed.packages("rpart")
installed.packages("randomForest")
installed.packages("e1071")

# 
# Execute below command to use the package/library in this file
#
library(ggplot2)
library(caret)
library(rpart)
library(randomForest)
library(e1071)

#
# Load the demographic data from the CSV File Manually via pop-up
#
demoData <- read.csv(file.choose())

#
# Explore the data imported
#
demoData
nrow(demoData) # 195 rows
ncol(demoData) # 5 columns

head(demoData) # top 6 rows
head(demoData, n=10) # top 10 rows

tail(demoData) # bottom 6 rows
tail(demoData, n=10) # bottom 10 rows

str(demoData) # structure of the data
summary(demoData) # Summary of the data type, min/max, class, Mode
factor(demoData$Income.Group) # 4 types/factor of Income Groups

# Display countries with internet users < 10%
demoData[demoData$Internet.users < 10, ]

# Display countries with birth rate > 35%
demoData[demoData$Birth.rate > 35, ]

# Display countries with internet user < 10% AND Birth rate > 35%
demoData[demoData$Internet.users < 10 & demoData$Birth.rate > 35, ]

# Selecting one country data 
demoData[demoData$Country.Name == "Mali", ]

# ------------------------------------
# Exploratory Data Analysis (EDA)
# ------------------------------------
# Birth rate vs. Income Group
qplot(data=demoData, x=Income.Group, y=Birth.rate, size=I(3), colour=I("blue"))

# Internet users vs. Birth Rate
qplot(data=demoData, x=Internet.users, y=Birth.rate, colour=Income.Group, size=I(5))

# ------------------------------------
# Data Processing
# ------------------------------------
# Adding additional data to the Data frame
# import Country region from the Vectors and merge to existing data frame
#
# Open the file "CountryRegionVectors.R" in R-Studio and RUN the commands in the file to create 3 vectors

# Create Data Frame from the Vectors
regionsDF <- data.frame(Country=Countries_2012_Dataset, Code=Codes_2012_Dataset, Region=Regions_2012_Dataset)

# Check the below Data frame we created from Vector
regionsDF
str(regionsDF) # Length, Columns
summary(regionsDF) # 195, Class, Mode

# check demoData
head(demoData)
head(regionsDF)

# merge regionsDF to demoData, matching column Country.code + Code
mergedDemoData <- merge(demoData, regionsDF, by.x="Country.Code", by.y="Code")
head(mergedDemoData)

# Remove duplicate column "Country"
mergedDemoData$Country <- NULL
head(mergedDemoData)

# Plot new Merged Demographic dataset
qplot(data=mergedDemoData, x=Internet.users, y=Birth.rate, 
      colour=Region, size=I(5), shape=I(17),
      main="Birth Rate vs. Internet Users (Regions)")

# facets by Region
w <- ggplot(data = mergedDemoData, aes(x=Internet.users, y=Birth.rate, color=Income.Group))
w + xlab("Internet Users") +
  ylab("Birth Rate") +
  ggtitle("Birth Rate vs. Internet User (Facets: Regions)") +
  geom_point(size=3) +
  facet_grid(.~Region)

#
# Model Training: Linear Regression.
#
# Split the data set into Training & Testing
set.seed(40)
trainIndex = createDataPartition(mergedDemoData$Internet.users, p = 0.8, list = FALSE)
trainData <- mergedDemoData[trainIndex, ]
testData <- mergedDemoData[-trainIndex, ]

head(trainData)
summary(trainData)
sum(is.na(trainData$Internet.users)) # check for any missing values
sum(is.na(trainData$Income.Group)) # check for any missing values
sum(is.na(trainData$Region)) # check for any missing values

#  L.R model
lmModel <- lm(trainData$Internet.users ~ trainData$Income.Group + trainData$Region, data=trainData)

# Check model summary
summary(lmModel)
plot(lmModel) # exploration

# L.R predictions
predictions <- predict(lmModel, newdata = testData)
rmse <- sqrt(mean((predictions - testData$Internet.users)^2))
print(paste("Linear regression Root Mean Squared Error (RMSE):", rmse))

#
# Model Training:  Decision Tree
#
head(trainData)

# D.F Model
dtModel <- train(Internet.users ~ Income.Group + Region, data=trainData, method="rpart")
# D.F Predictions
dtPredictions <- predict(dtModel, newdata = testData)
dtRmse <- sqrt(mean((dtPredictions - testData$Internet.users)^2))

print(paste("Decision Tree Root Mean Squared Error (RMSE):", dtRmse))


#
# Model Training:  Random Forest
#
head(trainData)

# R.F Model
rfModel <- train(Internet.users ~ Income.Group + Region, data=trainData, method="rf")
# R.F Predictions
rfPredictions <- predict(rfModel, newdata = testData)
rfRmse <- sqrt(mean((rfPredictions - testData$Internet.users)^2))
print(paste("Random Forest Root Mean Squared Error (RMSE):", rfRmse))


#
# Model Tuning for Random Forest model
#
rfGrid <- expand.grid(
  mtry = c(2, 3, 4, 5)   # Number of features to consider at each split
)
set.seed(4)
rfModel <- train(
  Internet.users ~ Income.Group + Region, 
  data=trainData,
  method = "rf",
  metric="RMSE", 
  ntree = 50,
  trControl = trainControl(method = "cv", number = 5),
  tuneGrid = rfGrid
)

# Print the best hyperparameters found during tuning
print(rf_model$bestTune)
predictions <- predict(rf_model, newdata = testData)
rmse <- sqrt(mean((predictions - testData$Internet.users)^2))
print(paste("Random Forest Tuning Root Mean Squared Error (RMSE):", rmse))

