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

set.seed(546886)
rfFit1 <- randomForest(classe ~ num_window + roll_belt + pitch_forearm + 
                               magnet_dumbbell_z + pitch_belt + magnet_dumbbell_y +
                               roll_forearm + magnet_dumbbell_x + accel_dumbbell_y,
                       data = train1, 
                       importance = TRUE)

rfFit1

preds <- predict(rfFit1, test1[,-54])

confusionMatrix(preds, test1$classe)


# Create multi folds for cross val
cv10Folds <- createMultiFolds(train1$classe, k = 3, times = 10)

# Create Train COntrol
ctrl1 <- trainControl(method = "repeatedcv", number = 3, repeats = 10,
                      index = cv10Folds)

library(doSNOW)

cl <- makeCluster(2, type = "SOCK")

registerDoSNOW(cl)

rfCV1 <- train(x = train1[,-54], train1$classe, method = "rf", tuneLength = 3,
               ntree = 1000, trControl = ctrl1)

stopCluster(cl)

rfCV1$bestTune

finalPred <- predict(rfCV1, test1[,-54])

confusionMatrix(finalPred, test1$classe)
