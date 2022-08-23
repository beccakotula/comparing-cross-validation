library(groupdata2) # fold()
library(dplyr) 

###### Data Loading functions ########################################################
# Load dataset & convert ID column to factor
load_housing_dataset <- function(){
    housing <- read.csv("https://raw.githubusercontent.com/rashida048/Datasets/master/home_data.csv")
    housing$id <- as.factor(housing$id)
    housing <- cbind(df.index = rownames(housing), housing)
    return(housing)
}

##### Cross-validation functions ###############################################
##### CV functions take a dataframe that has a ".fold" column, created by the
##### "fold()" function in the "groupdata2" package
KCV <- function(data, k, model, predict.col) {
    performances <- c()
    
    for (fold in 1:k){
        # Split to test on 1 fold, train on k-1 folds
        training.set <- data[data$.folds !=fold, ]
        testing.set <- data[data$.folds==fold, ]
        
        # Train model
        model <- lm(model, training.set)
        # Test model
        predicted <- predict(model, testing.set)
        
        RMSE <- sqrt(mean((testing.set[[predict.col]]-predicted)^2))
        
        # Add the MSE to the performance list
        performances[fold] <- RMSE
    }
    # Return the mean of the recorded MSEs
    return(c('KCV.RMSE' = mean(performances)))
}

MPCV <- function(data, k, model, predict.col, df.index){
    predictions <- data.frame(df.index = data[[df.index]])

    for (fold in 1:k){
        # Test on the k-1 folds, train on the 1 fold
        testing.set <- data[data$.folds !=fold, ]
        training.set <- data[data$.folds==fold, ]
        
        # Train model
        model <- lm(model, training.set)
        # Test model
        predicted <- predict(model, testing.set)
        # Prediction errors for fold
        #error <- testing.set[[predict.col]]-predicted
        predicted <- cbind(testing.set[[df.index]], predicted)
        colnames(predicted) <- c(df.index, fold)
        predictions <- merge(predictions, predicted, by=df.index, all.x=TRUE)
    }
    
    # Reset index column as the actual index
    rownames(predictions) <- predictions$df.index
    predictions$df.index <- NULL

    # Change dataset to numeric and get avg. errors
    predictions <- mutate_all(predictions, function(x) as.numeric(as.character(x)))
    avg.preds <- rowMeans(predictions, na.rm=TRUE)
    
    RMSE <- sqrt(mean((data[[predict.col]]-avg.preds)^2))
    return(c('MPCV.RMSE' = RMSE))
}

LOOCV <- function(data, model, predict.col){
    performances <- c()
    
    for (observation in 1:length(data)){
        # Split to test on 1 fold, train on k-1 folds
        training.set <- data[-observation, ]
        testing.set <- data[observation, ]
        
        # Train model
        model <- lm(model, training.set)
        # Test model
        predicted <- predict(model, testing.set)
        
        RMSE <- sqrt(mean((testing.set[[predict.col]]-predicted)^2))
        
        # Add the MSE to the performance list
        performances[observation] <- RMSE
    }
    # Return the mean of the recorded MSEs
    return(c('LOOCV.RMSE' = mean(performances)))
}
