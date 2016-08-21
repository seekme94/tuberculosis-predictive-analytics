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
out <- function(x, print_date=FALSE) {
  if (print_date) {
    cat(date(), ": ", x, "\n", file="output.txt", append=TRUE)
  } else {
    cat(x, "\n", file="output.txt", append=TRUE)
  }
}

## Provide some basic stats about an attribute
explore <- function(x) {
  print(summary(x))
  dist <- factor(x, exclude=NULL)
  print(table(dist))
  plot(table(dist))
}

## Run analysis on a dataset and testset
analyze <- function(dataset, testset, model=c("glm", "svm", "rforest", "nnet"), seed, formula, na.remove=TRUE) {
  if (na.remove == TRUE) {
    dataset <- dataset[complete.cases(dataset),]
  }
  # Need to set seed again for the model
  set.seed(seed)
  if (model == "glm") {
    glm_fit <- glm(formula, data=dataset, family=binomial(link="logit"))
    testset$Predicted <- predict(glm_fit, type="response", testset)
    testset$Predicted <- as.factor(ifelse(testset$Predicted >= 0.5, 'YES', 'NO'))
  }
  if (model == "svm") {
    svm_fit <- svm(formula, data=dataset, na.action=na.omit, kernel="radial")
    testset$Predicted <- predict(svm_fit, testset)
  }
  if (model == "rforest") {
    rforest_fit <- randomForest(formula, data=dataset, importance=TRUE, ntree=1000, mtry=4)
    #varImpPlot(rforest_fit)
    testset$Predicted <- predict(rforest_fit, testset)
  }
  if (model == "nnet") {
    nnet_fit <- avNNet(formula, data=dataset, repeats=70, bag=FALSE, allowParallel=TRUE, decay=0.1, size=3)
    testset$Predicted <- predict(nnet_fit, testset, type="class")
  }
  matrix <- confusionMatrix(testset$TreatmentComplete, testset$Predicted)
  accuracy <- round(matrix$overall[1] * 100, 2)
  cols <- paste (colnames(dataset), collapse=",")
  str <- paste ("Model", model, "Columns", cols, "Accuracy", accuracy)
  accuracy
}

# Get Precision/Recall/Accuracy table
getresults <- function(x, y) {
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


################# START #################
sampling <- "stratified" # random or stratified
file <- "d:/datasets/Tuberculosis/dataset_clean.csv"
seeds <- c(10, 300, 5000, 700)
limits <- c(0.2, 0.5, 1)
train_size <- 0.6
test_size <- 0.4

# Randomize indices
indices <- strata(data=dt, method="srswor", size=nrow(dt) * (1 - train_size) )$ID_unit
# Create training, cross-validation and test sets
train <- dt[-indices,]
test <- dt[indices,]
cv_size <- nrow(test) / 2
cv <- test[1:cv_size,]
test <- test[(cv_size + 1):nrow(test),]

#formula = as.factor(TreatmentComplete) ~ Gender + AgeGroup + Weight + HeightGroup + MaritalStatus + TBHistory + RegistrationDelay + SputumResultDelay + SmearResult + ScreeningToSmearDelay + XRayDone + XRayResultDelay + XRayResults + XRayIndicative + ScreeningToXRayDelay + GeneXpertTested + GeneXpertResult + DrugResistance + GXPPositive + ScreeningToGXPDelay + ScreeningToDiagnosisDelay + DiagnosedBy + DiagnosisAntibiotic + TBSymptomsDiagnosed + TBContactDiagnosed + Diagnosis + LargeLymphDiagnosed + LymphBiopsyDiagnosed + MantouxDiagnosed + PastTBDiagnosed + XRaySuggestiveDiagnosed + ScreeningToBaselineDelay + SmearToBaselineDelay + XRayToBaselineDelay + GXPToBaselineDelay + DiagnosisToBaselineDelay + BaselineWeightGroup + BaselinePatientCategory + BaselinePatientType + BaselineRegimen + BaselineDoseCombination + BaselineStreptomycin + ScreeningToBaselineWeightDifference + DiseaseCategory + DiseaseSite + DoseCombination
formula <- as.factor(TreatmentComplete) ~ .

################# TUNING PARAMETERS #################
## SVM
# Do some tuning to SVM for parameters gamma, cost and kernel; sampling method is bootstrapping
set.seed(seed)
svm_tune <- tune.svm(formula, data=train, tunecontrol=tune.control(sampling = "boot"), gamma=seq(from=0.1, to=0.5, by=0.05), cost=2^(0.1:3))
plot(svm_tune, main="Tune SVM on gamma and cost")
print(svm_tune)
# gamma=0.1, cost=1.072

# Run with custom parameters on full data
set.seed(seed)
svm_fit <- svm(formula, data=train, na.action=na.exclude, gamma=0.1, cost=1.072, kernel="radial")
cv$Predicted <- predict(svm_fit, cv)
getresults(cv$Predicted, cv$TreatmentComplete)
# Accuracy Pos Pred Value    Sensitivity    Specificity 
# 0.7540230      0.7327586      0.5279503      0.8868613 

## RANDOM FOREST
# Do some tuning to RF to get optimal value of mtry, ntree
set.seed(seed)
rf_tune <- tuneRF(train[,-2], train[,2], stepFactor=0.16, improve=0.05, ntreeTry=1000, plot=TRUE)

rforest_fit <- randomForest(formula, data=train, importance=TRUE, mtry=10, ntree=2000)
varImpPlot(rforest_fit)
cv$Predicted <- predict(rforest_fit, cv)
getresults(cv$Predicted, cv$TreatmentComplete)
# Accuracy Pos Pred Value    Sensitivity    Specificity 
# 0.9080460      0.9352518      0.8074534      0.9671533

## NEURAL NETWORK
# Tune for repeats, decay and size
set.seed(seed)
max_acc <- 0
vars <- c("PatientID", "TreatmentComplete", "ScreeningToBaselineWeightDifference", "BaselineStreptomycin", "DiagnosedBy", "DoseCombination", "ScreeningToBaselineDelay", "XRayResultDelay", "ScreeningToSmearDelay", "Weight", "SmearResult", "SmearToBaselineDelay", "SputumResultDelay", "DrugResistance", "XRayToBaselineDelay", "BaselinePatientType", "HeightGroup", "AgeGroup", "GXPToBaselineDelay", "BaselineWeightGroup", "ScreeningToGXPDelay", "TBHistory", "DiseaseSite", "Diagnosis", "XRayIndicative", "BaselineDoseCombination", "XRaySuggestiveDiagnosed")
for (i in seq(from=1, to=50, by=1)) {
  for (j in seq(from=0, to=0.15, by=0.05)) {
    for (k in 3:5) {
      nnet_fit <- avNNet(formula, data=train[,vars], repeats=i, decay=j, size=k, bag=FALSE, MaxNWts=50000, trace=FALSE)
      cv$Predicted <- predict(nnet_fit, cv[,vars], type="class")
      results <- getresults(cv$Predicted, cv$TreatmentComplete)
      if (results[1] > max_acc) {
        max_acc <- results[1]
        print(results)
        print(paste("Repeats:", i, ", Decay:", j, ", size:", k))
      }
    }
  }
}
# Best known parameters where repeats=70; size=3; decay=0.1

################# INDIVIDUAL ACCURACIES #################
head(dt)
dt <- read.csv(file, stringsAsFactors=TRUE)

variables <- setdiff(names(dt), c("X","TreatmentComplete","PatientID"))
for (v in variables) {
  formula=TreatmentComplete ~ .
  print(v)
  indices <- strata(data=dt, method="srswor", size=nrow(dt)*test_cases)$ID_unit
  test <- dt[indices,c("TreatmentComplete", v)]
  train <- dt[-indices,c("TreatmentComplete", v)]
  
  svm_fit <- svm(formula, data=train, na.action=na.exclude, gamma=0.1, cost=1.071773)
  res <- getresults(predict(svm_fit, test), test$TreatmentComplete)
  print(res)
  
  rforest_fit <- randomForest(formula, data=train, importance=TRUE, mtry=10, ntree=2000, na.action=na.exclude)
  res <- getresults(predict(svm_fit, test), test$TreatmentComplete)
  print(res)
  
  nnet_fit <- avNNet(formula, data=train, allowParallel=TRUE, size=3, repeats=70, decay=0.15, bag=FALSE, MaxNWts=5000, trace=FALSE)
  res <- getresults(predict(svm_fit, test), test$TreatmentComplete)
  print(res)
}


################# PREDICTION #################
# Merge cv dataset with training set
train <- rbind(train, cv[,1:length(cv)-1])
out(paste("Sampling:", sampling))
for (limit in limits) {
  out(paste("Limit:", limit * 100))
  for (seed in seeds) {
    out(paste("Seed:", seed))
    set.seed(seed)
    
    svm_fit <- svm(formula, data=train, na.action=na.exclude, gamma=0.1, cost=1.071773)
    test$Predicted <- predict(svm_fit, test)
    svm_results <- getresults(test$Predicted, test$TreatmentComplete)
    
    set.seed(seed)
    rforest_fit <- randomForest(formula, data=train, importance=TRUE, mtry=10, ntree=2000, na.action=na.exclude)
    test$Predicted <- predict(rforest_fit, test)
    rforest_results <- getresults(test$Predicted, test$TreatmentComplete)
    
    set.seed(seed)
    nnet_fit <- avNNet(formula, data=train[,vars], allowParallel=TRUE, size=5, repeats=100, decay=0.1, bag=FALSE, MaxNWts=5000, trace=FALSE)
    test$Predicted <- predict(nnet_fit, test[,vars], type="class")
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


################# TUNING + PREDICTION #################
dataset <- dt
train_size <- 0.6
test_size <- 0.2
cv_size <- 0.2
#formula = as.factor(TreatmentComplete) ~ Gender + AgeGroup + Weight + HeightGroup + MaritalStatus + TBHistory + RegistrationDelay + SputumResultDelay + SmearResult + ScreeningToSmearDelay + XRayDone + XRayResultDelay + XRayResults + XRayIndicative + ScreeningToXRayDelay + GeneXpertTested + GeneXpertResult + DrugResistance + GXPPositive + ScreeningToGXPDelay + ScreeningToDiagnosisDelay + DiagnosedBy + DiagnosisAntibiotic + TBSymptomsDiagnosed + TBContactDiagnosed + Diagnosis + LargeLymphDiagnosed + LymphBiopsyDiagnosed + MantouxDiagnosed + PastTBDiagnosed + XRaySuggestiveDiagnosed + ScreeningToBaselineDelay + SmearToBaselineDelay + XRayToBaselineDelay + GXPToBaselineDelay + DiagnosisToBaselineDelay + BaselineWeightGroup + BaselinePatientCategory + BaselinePatientType + BaselineRegimen + BaselineDoseCombination + BaselineStreptomycin + ScreeningToBaselineWeightDifference + DiseaseCategory + DiseaseSite + DoseCombination
formula <- as.factor(TreatmentComplete) ~ .

## SVM
# Run for each limit
for (limit in limits) {
  dt <- dataset
  set.seed(seed)
  dt <- dt[sample(1:nrow(dt), nrow(dt) * limit),]
  # Run for each seed
  for (seed in seeds) {
    set.seed(seed)
    # Randomize indices
    indices <- strata(data=dt, method="srswor", size=nrow(dt) * (train_size + cv_size) )$ID_unit
    # Create training and test sets. No need to cross-validation set
    train <- dt[indices,]
    test <- dt[-indices,]
    # Tune SVM
    set.seed(seed)
    print("Tuning parameters")
    svm_tune <- tune.svm(formula, data=train, tunecontrol=tune.control(sampling = "boot"), gamma=seq(from=0.01, to=0.16, by=0.03), cost=1:5)
    gamma <- svm_tune$best.parameters[1]
    cost <- svm_tune$best.parameters[2]
    # Automatically put best parameters
    svm_fit <- svm(formula, data=train, na.action=na.exclude, gamma=gamma, cost=cost, kernel="radial")
    test$Predicted <- predict(svm_fit, test)
    svm_results <- getresults(test$Predicted, test$TreatmentComplete)
    print(svm_results)
    out(svm_results)
  }
}

## Random Forests
# Run for each limit
for (limit in limits) {
  dt <- dataset
  set.seed(seed)
  dt <- dt[sample(1:nrow(dt), nrow(dt) * limit),]
  # Run for each seed
  for (seed in seeds) {
    set.seed(seed)
    # Randomize indices
    indices <- strata(data=dt, method="srswor", size=nrow(dt) * (train_size + cv_size) )$ID_unit
    # Create training and test sets. No need to cross-validation set
    train <- dt[indices,]
    test <- dt[-indices,]
    # Tune Random Forests
#     print("Tuning parameters")
#     rf_tune <- tuneRF(train[,-2], train[,2], stepFactor=1.25, improve=0.0005, ntreeTry=100, trace=TRUE)
#     res <- as.data.frame(rf_tune)
#     mtry <- res[res$OOBError == min(res$OOBError),"mtry"]
    mtry <- 25
    set.seed(seed)
    # Automatically put best parameters
    rforest_fit <- randomForest(formula, data=train, importance=TRUE, mtry=mtry, ntree=2000)
    test$Predicted <- predict(rforest_fit, test)
    rforest_results <- getresults(test$Predicted, test$TreatmentComplete)
    print(rforest_results)
    out(rforest_results, FALSE)
  }
}

## Neural Networks
# Run for each limit
for (limit in limits) {
  dt <- dataset
  set.seed(seed)
  dt <- dt[sample(1:nrow(dt), nrow(dt) * limit),]
  # Run for each seed
  for (seed in seeds) {
    set.seed(seed)
    # Randomize indices
    indices <- strata(data=dt, method="srswor", size=nrow(dt) * (train_size + cv_size) )$ID_unit
    # Create training and test sets. No need to cross-validation set
    train <- dt[indices,]
    test <- dt[-indices,]
    set.seed(seed)
    fitControl <- trainControl(method='cv', number=5)
    tuneGrid <- expand.grid(.size=c(1, 5, 10), .decay=c(0, 0.1, 0.01))
#    nnet_fit <- avNNet(formula, data=train, allowParallel=TRUE, size=4, repeats=4, decay=0.001, MaxNWts=5000, trace=FALSE)
    nnet_fit <- train(train[,-3], train$TreatmentComplete, method='nnet', preProcess='range', trControl=fitControl, tuneGrid=tuneGrid, trace=FALSE, maxit=1000)
    test$Predicted <- predict(nnet_fit, test, type="raw")
    nnet_results <- getresults(test$TreatmentComplete, test$Predicted)
    print(nnet_results)
    out(nnet_results, FALSE)
  }
}

