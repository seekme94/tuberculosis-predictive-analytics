#install.packages(c("caret","minqa","e1071","rpart","rattle","rpart.plot","RColorBrewer","randomForest","party","RWeka","elmNN","nnet","neuralnet","sampling"))
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

setwd("D:/Datasets/Tuberculosis")
dt <- read.csv("dataset_clean.csv", stringsAsFactors=TRUE)
limit <- 100
seed <- 300
test_size <- 0.2
prune_columns <- FALSE
tune <- FALSE

# Separate test data initially
set.seed(seed)
test <- dt[sample(1:nrow(dt), nrow(dt) * test_size, replace=FALSE),]
train <- subset(dt, !(dt$PatientID %in% test$PatientID))

# Print output to results.txt file in current working directory
out <- function(x)
{
  cat(date(), ": ", x, "\n", file="output.txt", append=TRUE)
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
    rforest_fit <- randomForest(formula, data=dataset, importance=TRUE, ntree=1000, control=rpart.control(minsplit=10, cp=0,  maxdepth=3))
    #varImpPlot(rforest_fit)
    testset$Predicted <- predict(rforest_fit, testset)
  }
  if (model == "nnet")
  {
    nnet_fit <- avNNet(formula, data=dataset, repeats=3, bag=FALSE, allowParallel=TRUE, decay=0.1, size=5)
    testset$Predicted <- predict(nnet_fit, testset, type="class")
  }
  matrix <- confusionMatrix(testset$TreatmentComplete, testset$Predicted)
  accuracy <- round(matrix$overall[1] * 100, 2)
  cols <- paste (colnames(dataset), collapse=",")
  str <- paste ("Model", model, "Columns", cols, "Accuracy", accuracy)
  out(str)
  accuracy
}

# For every individual column, calculate the accuracy and pick the column with highest accuracy
if (prune_columns)
{
  model="svm"
  accuracies <- data.frame(Col=colnames(dt)[3:67], Acc=rep(0,65))
  for (i in 3:67)
  {
    dataset <- dt[,c(1,2,i)]
    formula <- as.factor(TreatmentComplete) ~ .
    accuracy <- analyze (dataset, test, model, seed, formula)
    accuracies$Acc[accuracies$Col == colnames(dt)[i]] <- accuracy
  }
  svm_accuracies <- accuracies
  
  model="rforest"
  accuracies <- data.frame(Col=colnames(dt)[3:67], Acc=rep(0,65))
  for (i in 3:67)
  {
    dataset <- dt[,c(1,2,i)]
    formula <- as.factor(TreatmentComplete) ~ .
    accuracy <- analyze (dataset, test, model, seed, formula)
    accuracies$Acc[accuracies$Col == colnames(dt)[i]] <- accuracy
  }
  rforest_accuracies <- accuracies
  model="nnet"
  accuracies <- data.frame(Col=colnames(dt)[3:67], Acc=rep(0,65))
  for (i in 3:67)
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
  dt <- dt[,attributes]
}

### APPYING MODELS
formula <- as.factor(TreatmentComplete) ~ .

if (tune)
{
  # Break the dataset further into two portions for tuning
  tune_test <- train[sample(1:nrow(train), nrow(train) * 0.2),]
  tune_train <- subset(train, !(train$PatientID %in% tune_test$PatientID))
  
  # SLR
  glm_fit <- glm(formula, data=tune_train, family=binomial(link="logit"))
  tune_test$Predicted <- predict(glm_fit, type="response", tune_test)
  tune_test$Predicted <- ifelse(tune_test$Predicted >= 0.5, 'YES', 'NO')
  tune_test$Predicted <- as.factor(tune_test$Predicted)
  getresults(tune_test$Predicted, tune_test$TreatmentComplete)

  ## SVM
  # Do some tuning to SVM for parameters gamma, cost and kernel; sampling method is bootstrapping
  set.seed(seed)
  svm_tune <- tune.svm(formula, data=tune_train, tunecontrol=tune.control(sampling = "boot"), gamma=seq(from=0.1, to=0.5, by=0.05), cost=2^(0.1:3))
  plot(svm_tune, main="Tune SVM on gamma and cost")
  print(svm_tune)
  # gamma=0.1, cost=1.072
  
  # Run with custom parameters on full data
  formula <- as.factor(TreatmentComplete) ~ .
  set.seed(seed)
  svm_fit <- svm(formula, data=train, na.action=na.exclude, gamma=0.1, cost=1.072, kernel="radial")
  tune_test$Predicted <- predict(svm_fit, tune_test)
  getresults(tune_test$Predicted, tune_test$TreatmentComplete)
  
  ## RANDOM FOREST
  # Do some tuning to RF to get optimal value of mtry, ntree
  set.seed(seed)
  rf_tune <- tuneRF(tune_train[,-2], tune_train[,2], stepFactor=0.16, improve=0.05, ntreeTry=1000, plot=TRUE)

  
  set.seed(seed)
  paste(names(tune_train), collapse=' + ')
  formula = as.factor(TreatmentComplete) ~ Gender + AgeGroup + Weight + HeightGroup + MaritalStatus + TBHistory + RegistrationDelay + SputumResultDelay + SmearResult + ScreeningToSmearDelay + XRayDone + XRayResultDelay + XRayResults + XRayIndicative + ScreeningToXRayDelay + GeneXpertTested + GeneXpertResult + DrugResistance + GXPPositive + ScreeningToGXPDelay + ScreeningToDiagnosisDelay + DiagnosedBy + DiagnosisAntibiotic + TBSymptomsDiagnosed + TBContactDiagnosed + Diagnosis + LargeLymphDiagnosed + LymphBiopsyDiagnosed + MantouxDiagnosed + PastTBDiagnosed + XRaySuggestiveDiagnosed + ScreeningToBaselineDelay + SmearToBaselineDelay + XRayToBaselineDelay + GXPToBaselineDelay + DiagnosisToBaselineDelay + BaselineWeightGroup + BaselinePatientCategory + BaselinePatientType + BaselineRegimen + BaselineDoseCombination + BaselineStreptomycin + ScreeningToBaselineWeightDifference + DiseaseCategory + DiseaseSite + DoseCombination

  rforest_fit <- randomForest(formula, data=tune_train, importance=TRUE, mtry=10, ntree=500)
  tune_test$Predicted <- predict(rforest_fit, tune_test)
  getresults(tune_test$Predicted, tune_test$TreatmentComplete)
  
  ## NEURAL NETWORK
  # Tune for repeats, decay and size
  set.seed(seed)
  for (i in seq(from=10, to=100, by=10)) {
    for (j in seq(from=0, to=0.1, by=0.01)) {
      for (k in 1:5) {
        nnet_fit <- avNNet(formula, data=tune_train, repeats=i, decay=j, size=k, bag=FALSE, MaxNWts=50000, trace=FALSE)
        tune_test$Predicted <- predict(nnet_fit, tune_test, type="class")
        print(cat("Repeats:", i, ", Decay:", j, ", size:", k))
        print(getresults(tune_test$Predicted, tune_test$TreatmentComplete))
      }
    }
  }
  # Best known parameters where repeats=70; size=3; decay=0.1
} else {
  # Run with default parameters
  svm_fit <- svm(formula, data=train, na.action=na.exclude)
  test$Predicted <- predict(svm_fit, test)
  getresults(test$Predicted, test$TreatmentComplete)
  
  rforest_fit <- randomForest(formula, data=train, importance=TRUE)
  test$Predicted <- predict(rforest_fit, test)
  getresults(test$Predicted, test$TreatmentComplete)
  
  nnet_fit <- avNNet(formula, data=train, allowParallel=TRUE, size=3, repeats=70, decay=0.1, bag=FALSE, bag=FALSE, MaxNWts=50000)
  test$Predicted <- predict(nnet_fit, test, type="class")
  matrix <- confusionMatrix(test$TreatmentComplete, test$Predicted)
  getresults(test$TreatmentComplete, test$Predicted)
}


## Next step: Greedy method
all.cols <- c("AgeGroup","WeightGroup","HeightGroup","MaritalStatus","Religion","Caste","RegistrationYear","Fever","Cough","CoughDuration","ProductiveCough","BloodInCough","NightSweats","WeightLoss","TBHistory","TBInFamily","SeverityScore","RegistrationDelay","SmearTested","SputumResultDelay","SmearResult","SmearPositive","ScreeningToSmearDelay","XRayDone","XRayResultDelay","XRayResults","XRayIndicative","ScreeningToXRayDelay","GeneXpertTested","GeneXpertResult","DrugResistance","GXPPositive","ScreeningToGXPDelay","DiagnosisDone","ScreeningToDiagnosisDelay","DiagnosedBy","DiagnosisAntibiotic","TBSymptomsDiagnosed","TBContactDiagnosed","Diagnosis","LargeLymphDiagnosed","LymphBiopsyDiagnosed","MantouxDiagnosed","PastTBDiagnosed","XRaySuggestiveDiagnosed","ScreeningToBaselineDelay","SmearToBaselineDelay","XRayToBaselineDelay","GXPToBaselineDelay","DiagnosisToBaselineDelay","BaselineWeightGroup","BaselinePatientCategory","BaselinePatientType","BaselineRegimen","BaselineDoseCombination","BaselineStreptomycin","ScreeningToBaselineWeightDifference","BaselineWeightGroup","HasTreatmentSupporter","DiseaseCategory","DiseaseSite","DoseCombination","PatientType")
cols <- c("TreatmentComplete","Gender")
max.accuracy <- 0
while (i < length(all.cols))
{
  ts <- test[,cols]
  tr <- dt[,cols]
  formula <- TreatmentComplete ~ .
  set.seed(seed)
  svm_fit <- svm(formula, data=tr, na.action=na.exclude)
  ts$Predicted <- predict(svm_fit, ts)
  results <- getresults(ts$Predicted, ts$TreatmentComplete)
  accuracy <- results[1]
  if (accuracy >= max.accuracy)
  {
    print(results)
    max.accuracy <- accuracy
    cols <- c(cols, all.cols[i])
    all.cols <- all.cols[-i]
    i <- 1
  } else
  {
    i <- i + 1    
  }
}
