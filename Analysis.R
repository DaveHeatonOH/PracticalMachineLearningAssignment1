setwd("C:/Users/dave_/OneDrive/GitHub/Coursera/Practical Machine Learning/PracticalMachineLearningAssignment1")
# Download Training Data
download.file("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv", destfile = "pml-training.csv")

# Download Test Data
download.file("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv", destfile = "pml-testing.csv")


training <- read.csv("pml-training.csv")
testing <- read.csv("pml-testing.csv")

# Create a matrix ready to take input of for loop
combo <- rbind(training[,-160], testing[,-160])

rows <- ncol(combo)

output <- matrix(ncol=2,nrow = rows)

# Loop through the columns to find the columns with >0 NA rows
for (i in 1:ncol(combo)) {
        
        j <- sum(is.na(combo[,i]))
        
        if (sum(is.na(combo[,i]))) {
                
                output[i,] <- c(i,j)
                
        } else {
               output[i,] <- c(0,0)
        }
        
        output <- as.data.frame(output)
        
}

nrow(output[output$V1 >0,])

# Show a summary of the missing data for columsn with at least 1 NA
summary(output[output$V1 >0,]$V2)[3]


training <- training[,-output$V1]

testing <- testing[,-output$V1]

training <- training[,-c(1:6)]
testing <- testing[,-c(1:6)]

library(randomForest)
library(caret)

# Set the seed for reproducible resulta
set.seed(26598)

# Create partition and split traind and test 70 - 30 percent
inTrain <- createDataPartition(training$classe, p = 0.7, list = FALSE)
train1 <- training[inTrain,]
test1 <- training[-inTrain,]

# Fit random forest on all variables in order to aid feature selection
rfFit0 <- randomForest(classe ~., data = train1)

# Use varImp and Importance functions to see the top features, and cross validate these
vimp <- as.data.frame(varImp(rfFit0))
vimp <- data.frame(overall = vimp$Overall,
                  names   = rownames(vimp))
vimp[order(vimp$overall,decreasing = T),]

rfFit0$confusion

rfFit0


rfPred0 <- predict(rfFit0,test1[,-54])

confusionMatrix(rfPred0, test1$classe)

imp <- as.data.frame(importance(rfFit0))
imp <- data.frame(overall = imp$MeanDecreaseGini,
                  names   = rownames(imp))
imp[order(imp$overall,decreasing = T),]

# Plot a Variable Importance Plot
varImpPlot(rfFit0)

library(dplyr)
featuresCor <- train1 %>%
        select(num_window, yaw_belt, roll_belt, magnet_dumbbell_z, pitch_belt, magnet_dumbbell_y, pitch_forearm)
cor(featuresCor)


set.seed(546886)
rfFit1 <- randomForest(classe ~ num_window + roll_belt + pitch_forearm + 
                               magnet_dumbbell_z + pitch_belt + magnet_dumbbell_y +
                               roll_forearm + magnet_dumbbell_x + accel_dumbbell_y,
                       data = train1, 
                       importance = TRUE)


preds <- predict(rfFit1, test1[,-54])

confusionMatrix(rfPred0, test1$classe)
confusionMatrix(preds, test1$classe)
