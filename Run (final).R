library(caret)
library(e1071)
library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)
library(randomForest)
library(party)
library(RWeka)
library(elmNN)
library(nnet)
library(neuralnet)
library(sampling)

### FUNCTIONS ###
## Print output to results.txt file in current working directory
out <- function(x)
{
    cat(date(), ": ", x, "\n", file="output.txt", append=TRUE)
}

## Provide some basic stats about an attribute
explore <- function(x)
{
    print(summary(x))
    dist <- factor(x, exclude=NULL)
    print(table(dist))
    plot(table(dist))
}

## Run analysis on a dataset and testset
analyze <- function(dataset, testset, model=c("svm", "rforest", "nnet"), seed, formula, na.remove=TRUE)
{
    if (na.remove == TRUE)
    {
        dataset <- dataset[complete.cases(dataset),]
    }
    # Need to set seed again for the model
    set.seed(seed)
    if (model == "svm")
    {
        svm_fit <- svm(formula, data=dataset, na.action=na.omit)
        testset$Predicted <- predict(svm_fit, testset)
    }
    if (model == "rforest")
    {
        rforest_fit <- randomForest(formula, data=dataset, importance=TRUE, ntree=1000)
        #varImpPlot(rforest_fit)
        testset$Predicted <- predict(rforest_fit, testset)
    }
    if (model == "nnet")
    {
        nnet_fit <- avNNet(formula, data=dataset, repeats=3, bag=FALSE, allowParallel=TRUE, decay=0.001, size=5)
        testset$Predicted <- predict(nnet_fit, testset, type="class")
    }
    matrix <- confusionMatrix(testset$TreatmentComplete, testset$Predicted)
    accuracy <- round(matrix$overall[1] * 100, 2)
    cols <- paste (colnames(dataset), collapse=",")
    str <- paste ("Model", model, "Columns", cols, "Accuracy", accuracy)
    accuracy
}

# Get Precision/Recall/Accuracy table
getresults <- function(x, y)
{
    matrix <- confusionMatrix(x, y)
    accuracy <- matrix$overall[1] # Correctness of model
    precision <- matrix$byClass[3] # Positive prediction value
    neg_precision <- matrix$byClass[4] # Negative prediction value
    sensitivity <- matrix$byClass[1] # True positive recognition rate (aka recall)
    specificity <- matrix$byClass[2] # True negative recognition rate
    type1_error <- 0 # FP
    type2_error <- 0 # FN
    results <- c(accuracy, precision, sensitivity, specificity)
    results
}
#################

attributes <- c("PatientID", "TreatmentComplete", "Gender", "AgeGroup", "WeightGroup", "HeightGroup", "MaritalStatus", "Religion", "Caste", 
  "ScreeningYear", "RegistrationYear", "Fever", "Cough", "CoughDuration", "ProductiveCough", "BloodInCough", "NightSweats", "WeightLoss", "TBHistory", "TBInFamily", "SeverityScore", 
  "RegistrationDelay", "SmearTested", "SputumResultDelay", "SmearResult", "SmearPositive", "ScreeningToSmearDelay", "XRayDone", "XRayResultDelay", "XRayResults", "XRayIndicative", "ScreeningToXRayDelay", "GeneXpertTested", "GeneXpertResult", "DrugResistance", "GXPPositive", 
  "ScreeningToGXPDelay", "DiagnosisDone", "ScreeningToDiagnosisDelay", "DiagnosedBy", "DiagnosisAntibiotic", "TBSymptomsDiagnosed", "TBContactDiagnosed", "Diagnosis", "LargeLymphDiagnosed", "LymphBiopsyDiagnosed", "MantouxDiagnosed", "PastTBDiagnosed", "XRaySuggestiveDiagnosed", 
  "ScreeningToBaselineDelay", "SmearToBaselineDelay", "XRayToBaselineDelay", "GXPToBaselineDelay", "DiagnosisToBaselineDelay", "BaselineWeightGroup", "BaselinePatientCategory", "BaselinePatientType", "BaselineRegimen", "BaselineDoseCombination", "BaselineStreptomycin", "ScreeningToBaselineWeightDifference", "BaselineWeightGroup", "HasTreatmentSupporter", "DiseaseCategory", "DiseaseSite", "DoseCombination")

seeds <- c(10, 300, 5000, 700)
setwd("D:/Datasets/Tuberculosis")
limits <- c(0.2, 0.5, 1)
test_cases <- 0.2
prune_columns <- FALSE
sampling <- "stratified" # random or stratified

out(paste("Sampling:", sampling))
for (limit in limits)
{
    out(paste("Limit:", limit * 100))
    for (seed in seeds)
    {
        out(paste("Seed:", seed))
        # Read the dataset fresh
        dt <- read.csv("dataset_clean.csv", stringsAsFactors=TRUE)
        # Remove DIED and limit the dataset only to the columns till baseline treatment 
        dt <- dt[dt$TreatmentOutcome != 'DIED', attributes]
        # Limit to given percentage of data
        set.seed(seed)
        dt <- dt[sample(1:nrow(dt), nrow(dt) * limit, replace=FALSE), ]
        # Separate test data initially
        set.seed(seed)
        if (sampling == "random")
        {
            indices <- sample(1:nrow(dt), nrow(dt)*test_cases, replace=FALSE)
        } else if (sampling == "stratified")
        {
            indices <- strata(data=dt, method="srswor", size=nrow(dt)*test_cases)$ID_unit
        }
        test <- dt[indices,]
        train <- dt[-indices,]
        
        # For every individual column, calculate the accuracy and pick the column with highest accuracy
        if (prune_columns)
        {
            col_length <- length(colnames(dt))
            model="svm"
            accuracies <- data.frame(Col=colnames(dt)[3:col_length], Acc=rep(0,col_length-2))
            for (i in 3:col_length)
            {
                dataset <- dt[,c(1,2,i)]
                formula <- as.factor(TreatmentComplete) ~ .
                accuracy <- analyze (dataset, test, model, seed, formula)
                accuracies$Acc[accuracies$Col == colnames(dt)[i]] <- accuracy
            }
            svm_accuracies <- accuracies
            
            model="rforest"
            accuracies <- data.frame(Col=colnames(dt)[3:col_length], Acc=rep(0,col_length-2))
            for (i in 3:col_length)
            {
                dataset <- dt[,c(1,2,i)]
                formula <- as.factor(TreatmentComplete) ~ .
                accuracy <- analyze (dataset, test, model, seed, formula)
                accuracies$Acc[accuracies$Col == colnames(dt)[i]] <- accuracy
            }
            rforest_accuracies <- accuracies
            
            model="nnet"
            accuracies <- data.frame(Col=colnames(dt)[3:col_length], Acc=rep(0,col_length-2))
            for (i in 3:col_length)
            {
                dataset <- dt[,c(1,2,i)]
                formula <- as.factor(TreatmentComplete) ~ .
                accuracy <- analyze (dataset, test, model, seed, formula)
                accuracies$Acc[accuracies$Col == colnames(dt)[i]] <- accuracy
            }
            nnet_accuracies <- accuracies
            # Feature selection. For each model, keep appending every feature and recalculate accuracy
            svm_accuracies
            rforest_accuracies
            nnet_accuracies
            
            yes_prob <- length(dt$TreatmentComplete[dt$TreatmentComplete == 'YES'])/nrow(dt) * 100
            # The columns with accuracy less than probability of YES on all the models tried are of little use, because hypothetically, they add nothing to prediction
            
            accuracies <- data.frame(attribute=svm_accuracies$Col, svm=svm_accuracies$Acc, rforest=rforest_accuracies$Acc, nnet=nnet_accuracies$Acc)
            # Get the columns indices that are at least as accurate as yes_prob
            attributes <- c()
            for (i in 1:nrow(accuracies))
            {
                # If any one model gives higher accuracy, then keep it
                if (accuracies[i,"svm"] >= yes_prob || accuracies[i,"rforest"] >= yes_prob || accuracies[i,"nnet"] >= yes_prob)
                {
                    attributes <- c(attributes, accuracies$attribute[i])
                }
            }
            print(sort(attributes))
            train <- train[,attributes]
            test <- test[,attributes]
        }
        
        formula <- TreatmentComplete ~ .
        # Run with default parameters
        svm_fit <- svm(formula, data=train, na.action=na.exclude)
        test$Predicted <- predict(svm_fit, test)
        svm_results <- getresults(test$Predicted, test$TreatmentComplete)
        
        rforest_fit <- randomForest(formula, data=train, importance=TRUE)
        test$Predicted <- predict(rforest_fit, test)
        rforest_results <- getresults(test$Predicted, test$TreatmentComplete)
        
        nnet_fit <- avNNet(formula, data=train, allowParallel=TRUE, size=5, repeats=10, decay=0.001, bag=FALSE, MaxNWts=5000, trace=FALSE)
        test$Predicted <- predict(nnet_fit, test, type="class")
        matrix <- confusionMatrix(test$TreatmentComplete, test$Predicted)
        nnet_results <- getresults(test$TreatmentComplete, test$Predicted)
        
        print(svm_results)
        print(rforest_results)
        print(nnet_results)
        out(svm_results)
        out(rforest_results)
        out(nnet_results)
    }
}
