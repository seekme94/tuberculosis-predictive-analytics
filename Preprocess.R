library(rpart)

# Give some description about the data and plot it's distribution
explore <- function(x) {
    print(summary(x))
    dist <- factor(x, exclude=NULL)
    print(table(dist))
    print(length(x[is.na(x)])/length(x))
    plot(table(dist))
}

# Turn data into MD5 hash
hash <- function(x) {
    missing <- is.na(x)
    x[!missing] <- sapply(x[!missing], digest, algo="crc32")
    x
}

# Get the most frequent value (mode) in data
frequent <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# Returns entropy of given vector
entropy <- function(data) {
    dist <- table(data)
    dist <- sapply(dist, function(x) x/sum(dist))
    # Entropy = - p(a)*log(p(a)) - p(b)*log(p(b))
    entropy <- sum(sapply(dist, function(x) x*log2(x))) / length(dist)
    abs(entropy)
}

############################################### Pre-processing ###############################################
## Fill missing values: 
# Cross referencing: check if some other field has this value, e.g. DiagnosedByID is same as GPID in all cases
# Central tendency: use mean/median where P(missing values) < 0.05
# Classification: when the above are not applicable and P(missing values) > 0.5
# No change: when more than 50% of the data is missing
dt <- read.csv("D:/Datasets/Tuberculosis/dataset.csv", stringsAsFactors=FALSE, na.strings='')
head(dt)
colnames(dt)
# Change all dates from string to dates
dt$DOB <- as.Date(dt$DOB)
dt$ScreeningDate <- as.Date(dt$ScreeningDate)
dt$DateRegistered <- as.Date(dt$DateRegistered)
dt$DateSputumSubmitted <- as.Date(dt$DateSputumSubmitted)
dt$DateSputumTested <- as.Date(dt$DateSputumTested)
dt$DateXRayReported <- as.Date(dt$DateXRayReported)
dt$XRayDate <- as.Date(dt$XRayDate)
dt$DateGXPTested <- as.Date(dt$DateGXPTested)
dt$TreatmentEndDate <- as.Date(dt$TreatmentEndDate)
dt$BaselineDate <- as.Date(dt$BaselineDate)
dt$DiagnosisDate <- as.Date(dt$DiagnosisDate)

# Set missing registration year
dt$RegistrationYear[is.na(dt$RegistrationYear)] <- median(dt$RegistrationYear, na.rm=TRUE)

missing <- is.na(dt$GP)
dt$GP[missing] <- frequent(dt$GP[!missing])
# Now cross fill DiagnosedBy using GP
missing <- is.na(dt$DiagnosedBy)
dt$DiagnosedBy[missing] <- 'DON\'T KNOW'

# Discretize DiagnosedBy: all GPs with < 20 patients get merged into a single, 'other' entity
minors <- table(dt$DiagnosedBy)[table(dt$DiagnosedBy) < 20]
dt$DiagnosedBy[dt$DiagnosedBy %in% rownames(minors)] <- 'other'

# Fill missing ScreeningAge from DOB (estimate by taking difference of DOB and RegistrationDate)
missing <- is.na(dt$ScreeningAge)
dt$ScreeningAge[missing] <- as.numeric(format(dt$DateRegistered[missing], '%Y')) - as.numeric(format(dt$DOB[missing], '%Y'))
dt$ScreeningAge <- as.numeric(dt$ScreeningAge)
# Fill missing ScreeningDate from DateRegistered
missing <- dt$ScreeningDate[is.na(dt$ScreeningDate)]
dt$ScreeningDate[missing] <- dt$DateRegistered[missing]
# Similarly, fill missing DOB using ScreeningAge and ScreeningDate
missing <- is.na(dt$DOB)
all_dobs <- paste(as.numeric(format(dt$ScreeningDate, '%Y')) - dt$ScreeningAge, format(dt$ScreeningDate, '%m'), format(dt$ScreeningDate, '%d'), sep="-")
dt$DOB[missing] <- as.Date(all_dobs[missing])
# Extract BirthYear again from DOB
dt$BirthYear <- as.numeric(format(dt$DOB, '%Y'))
# Fix crazily high Ages by matching with BirthYear
crazy <- dt$ScreeningAge > 90
dt$ScreeningAge[crazy] <- dt$RegistrationYear[crazy] - dt$BirthYear[crazy]

# Age discretization
# Applying k-means to get an idea of age distribution
ages <- kmeans(dt$ScreeningAge, 7, iter.max=100)
ages$centers
# Add a new column AgeGroup
dt$AgeGroup <- NA
dt$AgeGroup[dt$ScreeningAge < 5] <- 'infant'
dt$AgeGroup[dt$ScreeningAge >= 5 & dt$ScreeningAge < 13] <- 'child'
dt$AgeGroup[dt$ScreeningAge >= 13 & dt$ScreeningAge < 20] <- 'teen'
dt$AgeGroup[dt$ScreeningAge >= 20 & dt$ScreeningAge < 26] <- 'young'
dt$AgeGroup[dt$ScreeningAge >= 26 & dt$ScreeningAge < 30] <- 'adult1'
dt$AgeGroup[dt$ScreeningAge >= 30 & dt$ScreeningAge < 40] <- 'adult2'
dt$AgeGroup[dt$ScreeningAge >= 40 & dt$ScreeningAge < 50] <- 'old1'
dt$AgeGroup[dt$ScreeningAge >= 50 & dt$ScreeningAge < 67] <- 'old2'
dt$AgeGroup[dt$ScreeningAge >= 67] <- 'ancient'

# Fill missing Religions
# Muslim and Islam are same things
dt$Religion[dt$Religion == 'MUSLIM'] <- 'ISLAM'
# Since 95.9 records are ISLAM, filling in the missing ones
missing <- is.na(dt$Religion)
dt$Religion[missing] <- 'ISLAM'

# Dealing with Caste :-(
# Remove whitespaces and dots
dt$Caste <- gsub("\\s|[.]","", dt$Caste)

# ABAASI = ABBASI.; AFGANI = AFGHANI; BALTI = BALDI; 
dt$Caste[dt$Caste == 'ABAASI'] <- 'ABBASI'
dt$Caste[dt$Caste == 'AFGANI'] <- 'AFGHANI'
dt$Caste[dt$Caste == 'BALTI'] <- 'BALDI'
dt$Caste[dt$Caste == 'BARMI'] <- 'BARMI'
dt$Caste[dt$Caste %in% c('GILGATI','GILGETI','GILGIT','GILGITTI','GILGTI','GILGTY','GILLGITE','')] <- 'GILGITI'
dt$Caste[dt$Caste %in% c('HAZARA','HAZARYWAAL')] <- 'HAZARAWAL'
dt$Caste[dt$Caste %in% c('HINDE','HINDO')] <- 'HINDU'
dt$Caste[dt$Caste %in% c('KACCHI','KATHIAWARI')] <- 'MEMON'
dt$Caste[dt$Caste == 'HINKO'] <- 'HINDKO'
dt$Caste[dt$Caste == 'KASHAMEERI'] <- 'KASHAMIRI'
dt$Caste[dt$Caste == 'KOHISTANE'] <- 'KOHISTANI'
dt$Caste[dt$Caste %in% c('KURESHI','QURAISHI')] <- 'QURESHI'
dt$Caste[dt$Caste %in% c('MARVARI','MARWAARI','MARWADRI')] <- 'MARWARI'
dt$Caste[dt$Caste %in% c('PHATAN')] <- 'PAKHTUN'
dt$Caste[dt$Caste %in% c('RAJPOT','RAJPUT')] <- 'RAJPOOT'
dt$Caste[dt$Caste == 'SERAIQE'] <- 'SARAIKI'
others <- c('ABBASI','BHERMI','BURMI','CHINA','CHITRAL','KHOKAR','MAEO','MAHROHRI','MALAK','MEWATI','RANGARI','SHINA','OTHER')
dt$Caste[dt$Caste %in% others] <- 'OTHERS'
# Fill missing values with most frequent, since p < 0.05
missing <- is.na(dt$Caste)
dt$Caste[missing] <- names(which.max(table(dt$Caste)))

# Fill in Weight using AgeGroup and Gender
missing <- is.na(dt$Weight)
temp <- dt[missing,c("PatientID","AgeGroup","Gender","Weight","Height")]
library(psych) # For harmonic mean
for (group in temp$AgeGroup)
{
    dt[missing & dt$AgeGroup == group & dt$Gender == 'M',"Weight"] <- harmonic.mean(dt$Weight[dt$AgeGroup == group & dt$Gender == 'M'])
    dt[missing & dt$AgeGroup == group & dt$Gender == 'F',"Weight"] <- harmonic.mean(dt$Weight[dt$AgeGroup == group & dt$Gender == 'F'])
}

# Round off and Ddiscretize Weight
dt$Weight <- round(dt$Weight)
weights <- table(dt$Weight)
k <- kmeans(dt$Weight, centers=5, iter.max=100)
# Add new column WeightGroup
dt$WeightGroup <- NA
dt$WeightGroup[dt$Weight < 34.69] <- 'verylow'
dt$WeightGroup[dt$Weight >= 34.69 & dt$Weight < 46.04] <- 'low'
dt$WeightGroup[dt$Weight >= 46.04 & dt$Weight < 54.05] <- 'below_avg'
dt$WeightGroup[dt$Weight >= 54.05 & dt$Weight < 63.78] <- 'average'
dt$WeightGroup[dt$Weight >= 63.78 & dt$Weight < 81.6] <- 'above_avg'
dt$WeightGroup[dt$Weight >= 91.6] <- 'high'
# Fill missing values with most frequent, since p < 0.05
missing <- is.na(dt$WeightGroup)
dt$WeightGroup[missing] <- names(which.max(table(dt$WeightGroup)))
explore(dt$WeightGroup)
# Do the same with baseline weight
dt$BaselineWeightGroup <- NA
dt$BaselineWeightGroup[dt$BaselineWeight < 34.69] <- 'verylow'
dt$BaselineWeightGroup[dt$BaselineWeight >= 34.69 & dt$BaselineWeight < 46.04] <- 'low'
dt$BaselineWeightGroup[dt$BaselineWeight >= 46.04 & dt$BaselineWeight < 54.05] <- 'below_avg'
dt$BaselineWeightGroup[dt$BaselineWeight >= 54.05 & dt$BaselineWeight < 63.78] <- 'average'
dt$BaselineWeightGroup[dt$BaselineWeight >= 63.78 & dt$BaselineWeight < 81.6] <- 'above_avg'
dt$BaselineWeightGroup[dt$BaselineWeight >= 91.6] <- 'high'
dt$BaselineWeightGroup[is.na(dt$BaselineWeightGroup)] <- dt$WeightGroup[is.na(dt$BaselineWeightGroup)]

# Remove crazily high heights
dt$Height[dt$Height > 200] <- NA
# Round off and Ddiscretize Weight
dt$Height <- round(dt$Height)
plot(table(dt$Height)) # Is it normal?
heights <- table(dt$Height)
k <- kmeans(dt$Height[!is.na(dt$Height)], centers=4, iter.max=100)
# Add a column HeightGroup
dt$HeightGroup <- NULL
dt$HeightGroup[dt$Height < 60.25] <- 'short'
dt$HeightGroup[dt$Height >= 60.25 & dt$Height < 143.14] <- 'below_avg'
dt$HeightGroup[dt$Height >= 143.14 & dt$Height < 154.83] <- 'average'
dt$HeightGroup[dt$Height >= 154.83 & dt$Height < 167.78] <- 'above_avg'
dt$HeightGroup[dt$Height >= 167.78] <- 'high'

# Fill in Height using Weight, Caste, AgeGroup and Gender with the help of classification
missing <- is.na(dt$HeightGroup)
train <- dt[!missing,c("PatientID","AgeGroup","Gender","Caste","HeightGroup","WeightGroup")]
test <- dt[missing,c("PatientID","AgeGroup","Gender","Caste","WeightGroup")]

formula <- as.factor(HeightGroup) ~ AgeGroup + Gender + WeightGroup
svm_fit = svm(formula, data=train)
test$HeightGroup <- predict(svm_fit, test)
# Tricky part: must convert from factors into characters before updating the missing values, due to difference in levels
dt$HeightGroup <- as.character(dt$HeightGroup)
test$HeightGroup <- as.character(test$HeightGroup)
dt$HeightGroup[missing] <- test$HeightGroup
# Turn back into factors
dt$HeightGroup <- as.factor(dt$HeightGroup)

# Look for marital status. Fill the NA's with respect to AgeGroup and Gender
table(dt$MaritalStatus, useNA="always")
missing <- is.na(dt$MaritalStatus)
dt$MaritalStatus[missing] <- "MARRIED"

# Copy registration date from Screening date
missing <- is.na(dt$DateRegistered)
dt$DateRegistered[missing] <- dt$ScreeningDate[missing]

# Introduce new column for delay in registration from screening
dt$RegistrationDelay <- dt$DateRegistered - dt$ScreeningDate
# Negatives are just bad data, turn them into zero. Same goes for NA
dt$RegistrationDelay[dt$RegistrationDelay < 0] <- NA
# Similarly, 365 days delay is just year wrongly entered
dt$RegistrationDelay[dt$RegistrationDelay == 365] <- NA
# Fix the Screening year as well
dt$ScreeningYear[dt$ScreeningYear < 2011] <- 2011

# Copy Disease category from Baseline, where possible
dt$DiseaseCategory[is.na(dt$DiseaseCategory) & !is.na(dt$BaselinePatientCategory)] <- dt$BaselinePatientCategory[is.na(dt$DiseaseCategory) & !is.na(dt$BaselinePatientCategory)]

# Copy from TB Diagnosis, where possible
dt$DiseaseSite[is.na(dt$DiseaseSite) & !is.na(dt$Diagnosis)] <- dt$Diagnosis[is.na(dt$DiseaseSite) & !is.na(dt$Diagnosis)]
# Spelling corrections
dt$DiseaseSite[dt$DiseaseSite %in% c('EP-TB','EP -TB','EP TB','EXTRA PULMONARY')] <- 'EXTRAPULMONARY'
dt$DiseaseSite[dt$DiseaseSite == 'OTHER THAN TB'] <- NA

# Regimens: see if Regimens changed during treatment
dt$BaselineRegimen[dt$BaselineRegimen == 'HE'] <- 'EH'
dt$RegimenChanged <- 'NO'
dt$RegimenChanged[dt$LastRegimen != dt$BaselineRegimen] <- 'YES'
# Turn 99's into NA's
dt$DoseCombination[dt$DoseCombination == 99] <- NA
# Turn 0's into NA's (ever saw a 0mg drug?)
dt$OtherDoseDescription[dt$OtherDoseDescription == 0] <- NA

# Treatment phase: according to WHO guidelines, a CAT I patient should be on INTENSIVE phase for 2 months, followed by 4 month CONTINUATION phase. Therefore, we can look at the Tx duration and predict the Phases
dt$TreatmentPhase[dt$TreatmentPhase == 'CONTINUOUS'] <- 'CONTINUATION'
missing <- is.na(dt$TreatmentPhase) & !is.na(dt$TreatmentDuration)
dt$TreatmentDuration[missing] <- ifelse(dt$TreatmentDuration[missing] <= 60, 'INTENSIVE', 'CONTINUATION')

# Turn Patient status into binary - OPEN/CLOSED
dt$PatientStatus[dt$PatientStatus %in% c('SUSPECT','GP_CONF','VERIFIED','PATIENT','SUSPENDED')] <- 'OPEN'

# Note: Patient types are potential critical identifiers
dt$PatientType <- toupper(dt$PatientType) # Like a mosquite, there was a 'New' against all CAPS

dt$TreatedPreviously <- NULL # Useless column
dt$CompletedPreviousTreatment <- NULL # Useless column

## Symptoms
# If there is blood in cough, the cough must be productive
dt$ProductiveCough[dt$ProductiveCough != 'YES' & dt$BloodInCough == 'YES'] <- 'YES'
# Blood in cough should be NA when cough is not productive
dt$BloodInCough[dt$ProductiveCough != 'YES'] <- NA
# Spelling difference
dt$NightSweats[dt$NightSweats == 'REFUSED'] <- 'REFUSE'
dt$WeightLoss[dt$WeightLoss == 'REFUSED'] <- 'REFUSE'
dt$TBHistory[dt$TBHistory == 'REFUSED'] <- 'REFUSE'
dt$TBInFamily[dt$TBInFamily == 'REFUSED'] <- 'REFUSE'
# Fill in missing Symptoms using different methods. Don't Know applies for least predictable
dt$Fever[is.na(dt$Fever)] <- 'YES'
dt$Cough[is.na(dt$Cough)] <- 'YES'
dt$CoughDuration[is.na(dt$CoughDuration)] <- 'DON\'T KNOW'
dt$CoughDuration[dt$Cough == 'NO'] <- 'NO'
dt$ProductiveCough[is.na(dt$ProductiveCough) & dt$Cough == 'YES'] <- 'YES'
dt$ProductiveCough[is.na(dt$ProductiveCough) & dt$Cough == 'NO'] <- 'NO'
dt$ProductiveCough[is.na(dt$ProductiveCough)] <- 'DON\'T KNOW'
dt$ProductiveCough[dt$Cough == 'NO'] <- 'NO'
dt$BloodInCough[dt$Cough == 'NO'] <- 'NO'
dt$BloodInCough[is.na(dt$BloodInCough)] <- 'NO'
dt$NightSweats[is.na(dt$NightSweats)] <- 'YES'
dt$WeightLoss[is.na(dt$WeightLoss)] <- 'YES'
dt$TBHistory[is.na(dt$TBHistory)] <- 'DON\'T KNOW'
dt$TBInFamily[is.na(dt$TBInFamily)] <- 'DON\'T KNOW'
# Calculate Severity of disease based on different 
dt$SeverityScore <- 0
dt$SeverityScore[!is.na(dt$Cough) & dt$Cough == 'YES'] <- dt$SeverityScore[!is.na(dt$Cough) & dt$Cough == 'YES'] + 1
dt$SeverityScore[!is.na(dt$CoughDuration) & dt$CoughDuration == 'LESS THAN 2 WEEKS'] <- dt$SeverityScore[!is.na(dt$CoughDuration) & dt$CoughDuration == 'LESS THAN 2 WEEKS'] + 1
dt$SeverityScore[!is.na(dt$CoughDuration) & dt$CoughDuration == '2 TO 3 WEEKS'] <- dt$SeverityScore[!is.na(dt$CoughDuration) & dt$CoughDuration == '2 TO 3 WEEKS'] + 2
dt$SeverityScore[!is.na(dt$CoughDuration) & dt$CoughDuration == 'MORE THAN 3 WEEKS'] <- dt$SeverityScore[!is.na(dt$CoughDuration) & dt$CoughDuration == 'MORE THAN 3 WEEKS'] + 3
dt$SeverityScore[!is.na(dt$ProductiveCough) & dt$ProductiveCough == 'YES'] <- dt$SeverityScore[!is.na(dt$ProductiveCough) & dt$ProductiveCough == 'YES'] + 1
dt$SeverityScore[!is.na(dt$BloodInCough) & dt$BloodInCough == 'YES'] <- dt$SeverityScore[!is.na(dt$BloodInCough) & dt$BloodInCough == 'YES'] + 1
dt$SeverityScore[!is.na(dt$Fever) & dt$Fever == 'YES'] <- dt$SeverityScore[!is.na(dt$Fever) & dt$Fever == 'YES'] + 1
dt$SeverityScore[!is.na(dt$NightSweats) & dt$NightSweats == 'YES'] <- dt$SeverityScore[!is.na(dt$NightSweats) & dt$NightSweats == 'YES'] + 1
dt$SeverityScore[!is.na(dt$WeightLoss) & dt$WeightLoss == 'YES'] <- dt$SeverityScore[!is.na(dt$WeightLoss) & dt$WeightLoss == 'YES'] + 1
dt$SeverityScore[!is.na(dt$TBHistory) & dt$TBHistory == 'YES'] <- dt$SeverityScore[!is.na(dt$TBHistory) & dt$TBHistory == 'YES'] + 1
dt$SeverityScore[!is.na(dt$TBInFamily) & dt$TBInFamily == 'YES'] <- dt$SeverityScore[!is.na(dt$TBInFamily) & dt$TBInFamily == 'YES'] + 1

## Medical tests
# Smear microscopy
# Add a column to record delay between screening and smear test result
dt$ScreeningToSmearDelay <- dt$DateSputumTested - dt$ScreeningDate
dt$SputumResultDelay <- dt$DateSputumTested - dt$DateSputumSubmitted

# X-Ray test
# Add a column to record delay between screening and XRay
dt$ScreeningToXRayDelay <- dt$XRayDate - dt$ScreeningDate
# Fix spelling mistakes, etc.
dt$XRayResults <- toupper(dt$XRayResults)
dt$XRayResults[dt$XRayResults %in% c('OTHER ABNORMALITY (NON-TB)','OTHER ABNORMALITY ')] <- 'OTHER ABNORMALITY'
dt$XRayResults[dt$XRayResults %in% c('POSSIBILITY OF TB','SUSPICIOUS OF TB')] <- 'SUGGESTIVE OF TB'

# pertpert test
# Add a column to record delay between screening and GXP
dt$ScreeningToGXPDelay <- dt$DateGXPTested - dt$ScreeningDate
dt$GXPDelay <- NULL # Seems to be out of order
dt$GXRemarks <- NULL # Useless

## Clinical Diagnosis
# Add a column to determine whether Diagnosis was done or not
dt$DiagnosisDone <- 'YES'
missing <- is.na(dt$Diagnosis)
dt$DiagnosisDone[missing] <- 'NO'
# Add a column to record delay between screening and diagnosis
dt$ScreeningToDiagnosisDelay <- dt$DiagnosisDate - dt$ScreeningDate
# Spelling corrections
dt$Diagnosis[dt$Diagnosis %in% c('EP-TB','EP -TB','EP TB','EPTB','EXTRA PULMONARY')] <- 'EXTRAPULMONARY'
dt$Diagnosis[dt$Diagnosis == 'OTHER THAN TB'] <- NA
# TB History detected in Dignosis supercedes TBHistory
dt$TBHistory[dt$PastTBDiagnosed == 'YES' & dt$TBHistory == 'NO'] <- 'YES'
# Clean out some Diagnosis notes
dt$OtherTBDiagnosis[dt$OtherTBDiagnosis %in% c('NO T.B','NO TB')] <- 'NOT TB'
dt$OtherTBDiagnosis[dt$OtherTBDiagnosis %in% c('TB BONE','TB SPINE','PLURAL TB')] <- 'EXTRAPULMONARY' # Pleural TB is EPTB! Reference?
dt$OtherTBDiagnosis[dt$OtherTBDiagnosis %in% c('CXR SUGGUSTIVE TB','SUGGESTIVE OF TB','SMEAR NEGATIVE')] <- 'PULMONARY TB'

## Baseline Treatment
# Add a columns to record delay between screening/tests/diagnosis and baseline treatment
dt$ScreeningToBaselineDelay <- dt$BaselineDate - dt$ScreeningDate
dt$SmearToBaselineDelay <- dt$BaselineDate - dt$DateSputumTested
dt$XRayToBaselineDelay <- dt$BaselineDate - dt$DateXRayReported
dt$GXPToBaselineDelay <- dt$BaselineDate - dt$DateGXPTested
dt$DiagnosisToBaselineDelay <- dt$BaselineDate - dt$DiagnosisDate
# Add a column to record absolute difference in weights from baseline to screening
dt$ScreeningToBaselineWeightDifference <- abs(dt$BaselineWeight - dt$Weight)
# Rename some baseline attributes to avoid confusion
colnames(dt)[75] <- 'BaselineDoseCombination'
colnames(dt)[76] <- 'BaselineStreptomycin'

# Some case correction
dt$BaselinePatientType <- toupper(dt$BaselinePatientType)

# The real thing
dt$TreatmentOutcome[dt$TreatmentOutcome %in% c('NOT TB','CLOSED')] <- 'OTHER'
dt$TreatmentComplete <- 'NO'
dt$TreatmentComplete[dt$TreatmentOutcome %in% c('CURED','TRANSFERRED','TREATMENT COMPLETED','TREATMENT FAILURE','')] <- 'YES'

# Discretize all durations into none, 3day, 1week, 2weeks, 1month, 2months, 6months, 1year or more
discretize_delay <- function(x)
{
    missing <- is.na(x)
    y <- NULL
    y[!missing & x < 1] <- 'NONE'
    y[!missing & x > 1 & x <= 3] <- '3D'
    y[!missing & x > 3 & x <= 7] <- '1W'
    y[!missing & x > 7 & x <= 14] <- '2W'
    y[!missing & x > 14 & x <= 30] <- '1M'
    y[!missing & x > 30 & x <= 60] <- '2M'
    y[!missing & x > 60 & x <= 180] <- '6M'
    y[!missing & x > 180] <- '1Y'
    y[missing] <- 'NOT PRESENT'
    y
}
dt$SputumResultDelay <- discretize_delay(dt$SputumResultDelay)
dt$GXPToBaselineDelay <- discretize_delay(dt$GXPToBaselineDelay)
dt$SmearToBaselineDelay <- discretize_delay(dt$SmearToBaselineDelay)
dt$XRayResultDelay <- discretize_delay(dt$XRayResultDelay)
dt$XRayToBaselineDelay <- discretize_delay(dt$XRayToBaselineDelay)
dt$RegistrationDelay <- discretize_delay(dt$RegistrationDelay)
dt$ScreeningToSmearDelay <- discretize_delay(dt$ScreeningToSmearDelay)
dt$ScreeningToXRayDelay <- discretize_delay(dt$ScreeningToXRayDelay)
dt$ScreeningToGXPDelay <- discretize_delay(dt$ScreeningToGXPDelay)
dt$ScreeningToDiagnosisDelay <- discretize_delay(dt$ScreeningToDiagnosisDelay)
dt$ScreeningToBaselineDelay <- discretize_delay(dt$ScreeningToBaselineDelay)
dt$DiagnosisToBaselineDelay <- discretize_delay(dt$DiagnosisToBaselineDelay)

# Finally fill remaining NAs with medians or whatever makes sense
dt$DiagnosedBy[is.na(dt$DiagnosedBy)] <- 'NOT PRESENT'
dt$RegistrationDelay[is.na(dt$RegistrationDelay)] <- 'NOT PRESENT'
dt$SputumResultDelay[is.na(dt$SputumResultDelay)] <- 'NOT PRESENT'
dt$SmearResult[dt$SmearTested == 'NO'] <- 'NOT PRESENT'
dt$ScreeningToSmearDelay[dt$SmearTested == 'NO'] <- 'NOT PRESENT'
dt$ScreeningToSmearDelay[is.na(dt$ScreeningToSmearDelay)] <- 'NOT PRESENT'
dt$XRayResultDelay[is.na(dt$XRayResultDelay)] <- 'NOT PRESENT'
dt$XRayResults[is.na(dt$XRayResults)] <- 'DON\'T KNOW'
dt$ScreeningToXRayDelay[is.na(dt$ScreeningToXRayDelay)] <- 'NOT PRESENT'
dt$GeneXpertResult[is.na(dt$GeneXpertResult)] <- 'DON\'T KNOW'
dt$DrugResistance[is.na(dt$DrugResistance)] <- 'DON\'T KNOW'
dt$ScreeningToGXPDelay[is.na(dt$ScreeningToGXPDelay)] <- 'NOT PRESENT'
dt$ScreeningToDiagnosisDelay[is.na(dt$ScreeningToDiagnosisDelay)] <- 'NOT PRESENT'
dt$DiagnosedBy[is.na(dt$DiagnosedBy)] <- 'DON\'T KNOW'
dt$DiagnosisAntibiotic[is.na(dt$DiagnosisAntibiotic)] <- 'DON\'T KNOW'
dt$TBSymptomsDiagnosed[is.na(dt$TBSymptomsDiagnosed)] <- 'DON\'T KNOW'
dt$TBContactDiagnosed[is.na(dt$TBContactDiagnosed)] <- 'DON\'T KNOW'
dt$Diagnosis[is.na(dt$Diagnosis)] <- 'DON\'T KNOW'
dt$LargeLymphDiagnosed[is.na(dt$LargeLymphDiagnosed)] <- 'DON\'T KNOW'
dt$LymphBiopsyDiagnosed[is.na(dt$LymphBiopsyDiagnosed)] <- 'DON\'T KNOW'
dt$MantouxDiagnosed[is.na(dt$MantouxDiagnosed)] <- 'DON\'T KNOW'
dt$PastTBDiagnosed[is.na(dt$PastTBDiagnosed)] <- 'DON\'T KNOW'
dt$XRaySuggestiveDiagnosed[is.na(dt$XRaySuggestiveDiagnosed)] <- 'DON\'T KNOW'
dt$ScreeningToBaselineDelay[is.na(dt$ScreeningToBaselineDelay)] <- 'NOT PRESENT'
dt$SmearToBaselineDelay[is.na(dt$SmearToBaselineDelay)] <- 'NOT PRESENT'
dt$XRayToBaselineDelay[is.na(dt$XRayToBaselineDelay)] <- 'NOT PRESENT'
dt$GXPToBaselineDelay[is.na(dt$GXPToBaselineDelay)] <- 'NOT PRESENT'
dt$DiagnosisToBaselineDelay[is.na(dt$DiagnosisToBaselineDelay)] <- 'NOT PRESENT'
dt$BaselineWeightGroup[is.na(dt$BaselineWeightGroup)] <- 'DON\'T KNOW'
dt$BaselinePatientCategory[is.na(dt$BaselinePatientCategory)] <- 'CAT I'
dt$BaselinePatientType[is.na(dt$BaselinePatientType)] <- 'NEW'
dt$BaselineRegimen[is.na(dt$BaselineRegimen)] <- 'RHZE'
dt$DiseaseCategory[is.na(dt$DiseaseCategory)] <- 'CAT I'
dt$DoseCombination[is.na(dt$DoseCombination)] <- 3
dt$PatientType[is.na(dt$DoseCombination)] <- dt$BaselinePatientType[is.na(dt$DoseCombination)]
dt$TreatmentOutcome[is.na(dt$TreatmentOutcome)] <- 'NOT PRESENT'

# Fill in missing BaselineDoseCombination using AgeGroup BaselinePatientCategory, BaselinePatientType, BaselineWeightGroup & BaselineRegimen using Random partitions
missing <- is.na(dt$BaselineDoseCombination)
train <- dt[!missing,c("PatientID","BaselineDoseCombination","BaselineRegimen","AgeGroup","BaselineRegimen","BaselinePatientCategory","BaselinePatientType","BaselineWeightGroup")]
test <- dt[missing,c("PatientID","BaselineDoseCombination","BaselineRegimen","AgeGroup","BaselinePatientCategory","BaselinePatientType","BaselineWeightGroup")]
formula <- as.factor(BaselineDoseCombination) ~ BaselineRegimen + AgeGroup + BaselinePatientCategory + BaselinePatientType + BaselineWeightGroup
rpart_fit <- rpart(formula, data=train, method="class")
test$BaselineDoseCombination <- predict(rpart_fit, test, type="class")
dt$BaselineDoseCombination[missing] <- test$BaselineDoseCombination

# Fill in missing BaselineStreptomycin using AgeGroup BaselinePatientCategory, BaselinePatientType, BaselineWeightGroup & BaselineRegimen using Random partitions
missing <- is.na(dt$BaselineStreptomycin)
train <- dt[!missing,c("PatientID","BaselineStreptomycin","BaselineDoseCombination","BaselineRegimen","AgeGroup","BaselinePatientCategory","BaselinePatientType","BaselineWeightGroup")]
test <- dt[missing,c("PatientID","BaselineDoseCombination","BaselineRegimen","AgeGroup","BaselinePatientCategory","BaselinePatientType","BaselineWeightGroup")]
unique(dt$BaselineRegimen)
train$BaselineRegimen <- as.factor(train$BaselineRegimen)
test$BaselineRegimen <- as.factor(test$BaselineRegimen)
levels(train$BaselineRegimen) <- c("EH","RH","RHE","RHZ","RHZE","RHZES")
levels(test$BaselineRegimen) <- c("EH","RH","RHE","RHZ","RHZE","RHZES")
formula <- as.factor(BaselineStreptomycin) ~ BaselineDoseCombination + BaselineRegimen + AgeGroup + BaselinePatientCategory + BaselinePatientType + BaselineWeightGroup
rpart_fit <- rpart(formula, data=train, method="class")
test$BaselineStreptomycin <- predict(rpart_fit, test, type="class")
dt$BaselineStreptomycin[missing] <- test$BaselineStreptomycin

# BaselineWeightDifference needs to be round before prediction, 20+ difference is too much difference to be true. We can use demographics here
bad <- is.na(dt$ScreeningToBaselineWeightDifference) | dt$ScreeningToBaselineWeightDifference > 20
train <- dt[!bad,c("PatientID","ScreeningToBaselineWeightDifference","AgeGroup","Gender","HeightGroup","WeightGroup")]
test <- dt[bad,c("PatientID","AgeGroup","Gender","HeightGroup","WeightGroup")]
formula <- as.factor(ScreeningToBaselineWeightDifference) ~ AgeGroup + Gender + HeightGroup + WeightGroup
rpart_fit <- rpart(formula, data=train, method="class")
test$ScreeningToBaselineWeightDifference <- predict(rpart_fit, test, type="class")
dt$ScreeningToBaselineWeightDifference[bad] <- test$ScreeningToBaselineWeightDifference
dt$ScreeningToBaselineWeightDifference <- round(dt$ScreeningToBaselineWeightDifference)

# DiseaseSite can be predicted on the basis of Category, Dose, XRay results and Smear rsults
missing <- is.na(dt$DiseaseSite)
train <- dt[!missing,c("PatientID","DiseaseSite","BaselineDoseCombination","BaselineRegimen","BaselinePatientCategory","SmearResult","XRayResults")]
test <- dt[missing,c("PatientID","DiseaseSite","BaselineDoseCombination","BaselineRegimen","BaselinePatientCategory","SmearResult","XRayResults")]
formula <- as.factor(DiseaseSite) ~ BaselineDoseCombination + BaselineRegimen + BaselinePatientCategory + SmearResult + XRayResults
rpart_fit <- rpart(formula, data=train, method="class")
PredDiseaseSite <- predict(rpart_fit, test, type="class")
dt$DiseaseSite[missing] <- as.character(PredDiseaseSite)

## De-identifying data using MD5 checksum
library(digest)
dt$GP <- hash(dt$GP)
dt$Screener <- hash(dt$Screener)
dt$DiagnosedBy <- hash(dt$DiagnosedBy)
dt$BaselineCHWID <- hash(dt$BaselineCHWID)

# Turn qualitative variables in numeric form into factors
dt$ScreeningYear <- as.factor(dt$ScreeningYear)

# Check the entropy of target variable
entropy(dt$TreatmentComplete)

# Choose variables to include in final data set. Remove all those with statistically insignificant chisq.test score
# removed <- c("Religion", "Caste", "Fever", "Cough", "CoughDuration", "ProductiveCough", "BloodInCough", "NightSweats", "WeightLoss", "SeverityScore", "SmearTested", "SmearPositive", "DiagnosisDone")
attributes <- c("PatientID", "TreatmentComplete", "Gender", "AgeGroup", "Weight", "HeightGroup", "MaritalStatus", "ScreeningYear", "RegistrationYear", "TBHistory", "TBInFamily", 
  "RegistrationDelay", "SputumResultDelay", "SmearResult", "ScreeningToSmearDelay", "XRayDone", "XRayResultDelay", "XRayResults", "XRayIndicative", "ScreeningToXRayDelay", "GeneXpertTested", "GeneXpertResult", "DrugResistance", "GXPPositive", 
  "ScreeningToGXPDelay", "ScreeningToDiagnosisDelay", "DiagnosedBy", "DiagnosisAntibiotic", "TBSymptomsDiagnosed", "TBContactDiagnosed", "Diagnosis", "LargeLymphDiagnosed", "LymphBiopsyDiagnosed", "MantouxDiagnosed", "PastTBDiagnosed", "XRaySuggestiveDiagnosed", 
  "ScreeningToBaselineDelay", "SmearToBaselineDelay", "XRayToBaselineDelay", "GXPToBaselineDelay", "DiagnosisToBaselineDelay", "BaselineWeightGroup", "BaselinePatientCategory", "BaselinePatientType", "BaselineRegimen", "BaselineDoseCombination", "BaselineStreptomycin", "ScreeningToBaselineWeightDifference", 
  "DiseaseCategory", "DiseaseSite", "DoseCombination")
# Remove DIED and limit the dataset only to the columns till baseline treatment 
dt <- dt[dt$TreatmentOutcome != 'DIED', attributes]
nrow(dt)

# Write the pre-processed data into new file
write.csv(dt, file="dataset_clean.csv", append=FALSE, quote=TRUE)
