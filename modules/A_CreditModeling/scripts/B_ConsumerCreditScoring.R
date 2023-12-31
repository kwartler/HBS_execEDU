#' Author: Ted Kwartler
#' Date: Oct 31, 2022
#' Purpose: Lending Club score notes and visualize
#' 

# Options
options(scipen = 999)

# Libraries
#library(rpart) # you can try any of the methods from our class to improve performance
#library(randomForest)
library(dplyr)
library(caret)
library(e1071)
library(vtreat)
library(readr)
library(MLmetrics)
library(rbokeh)

# I/O
df <- read.csv('https://raw.githubusercontent.com/kwartler/Harvard_DataMining_Business_Student/master/Lessons/H_CreditModeling/data/20K_sampleLoans.csv') 
newNotes <- read.csv('https://raw.githubusercontent.com/kwartler/Harvard_DataMining_Business_Student/master/Lessons/H_CreditModeling/data/OpenNotesJune18_v2.csv')

# Keep the pertinent information
keeps <- c("loan_amnt", "term", "int_rate", "installment", "grade", "sub_grade", "emp_length" , "home_ownership", "annual_inc", "purpose", "title", "zip_code", "addr_state", "dti", "delinq_2yrs", "pub_rec_bankruptcies", "inq_last_6mths", "mths_since_last_delinq", "mths_since_last_record", "open_acc", "pub_rec", "revol_bal", "revol_util", "total_acc", "collections_12_mths_ex_med", "mths_since_last_major_derog","y")
df    <- df[,keeps]

# Make a data change 
df$revol_util <- as.numeric(gsub('%', '', df$revol_util))
df$int_rate   <- as.numeric(gsub('%', '', df$int_rate))
df$term       <- as.numeric(gsub(' months','',df$term))
newNotes$revol_bal <- as.numeric(gsub('%', '', newNotes$revol_bal))
newNotes$int_rate  <- as.numeric(gsub('%', '', newNotes$int_rate))
newNotes$term      <- as.numeric(gsub(' months','',newNotes$term))
newNotes$mths_since_last_major_derog <- NA

## Sample & Segment the prep data
set.seed(1234)
idx         <- sample(1:nrow(df),.1*nrow(df))
prepData    <- df[idx,]
nonPrepData <- df[-idx,]

## Modify
# Design a "C"ategorical variable plan 
dataPlan <- designTreatmentsC(dframe        = prepData, 
                              varlist       = keeps,
                              outcomename   = 'y', 
                              outcometarget = 1)

# Apply to xVars
treatedX <- prepare(dataPlan, nonPrepData)

# Partition to avoid over fitting
set.seed(2022)
idx        <- sample(1:nrow(treatedX),.8*nrow(treatedX))
training   <- treatedX[idx,]
validation <- treatedX[-idx,]

## Explore
head(training)

# Model w/increased CV
crtl <- trainControl(method      = "cv", 
                     number      = 10,
                     verboseIter = TRUE)

# Fit lm model using 10-fold CV: model
finalFit <- train(as.factor(y) ~ ., 
                  data      = training, 
                  method    = "glm", 
                  family    = "binomial",
                  trControl = crtl)

# Make predictions for 3 partitions
trainProbs    <- predict(finalFit, training, type = 'prob')
testProbs     <- predict(finalFit, validation, type = 'prob')
treatedNew    <- prepare(dataPlan, newNotes) #remember the new notes need to be treated for the model!
newNotesProbs <- predict(finalFit, treatedNew, type = 'prob')

# Change the cutoff threshold
cutoff              <- 0.80
cutoffProbsTrain    <- ifelse(trainProbs[,2]>=cutoff, 1, 0) 
cutoffProbsTest     <- ifelse(testProbs[,2]>=cutoff, 1, 0) 
cutoffProbsNewNotes <- ifelse(newNotesProbs[,2]>=cutoff, 1, 0) 

# Accuracy
Accuracy(training$y, cutoffProbsTrain)
Accuracy(validation$y, cutoffProbsTest)

table(training$y, cutoffProbsTrain)
table(validation$y, cutoffProbsTest)

## Suppose you want to review "A" and low risk notes 20% chance of default
# Organize
testSetComparison <- data.frame(y     = validation$y, #actual outcome
                                grade = nonPrepData[-idx,]$grade, #lending club guess
                                risk  = testProbs[,1]) #probability of 0 in model
head(testSetComparison)

# Get their best guess
onlyA <- subset(testSetComparison, testSetComparison$grade == "A")

# Get their best guess with our best risk score
testSetBEST <- subset(testSetComparison, 
                      testSetComparison$grade == "A" & 
                      testSetComparison$risk <= 0.1 )
head(testSetBEST)

# How many were 0 in the ENTIRE test set? Random Selection 
sum(testSetComparison$y==0) / nrow(testSetComparison)

# How many were 0 among "A" (their best guess)? Accept their model
sum(onlyA$y==0)/nrow(onlyA)

# How many were "A" (their guess) &  sub 10% default risk for comparison (our guess)? Combine their expertise & our model
sum(testSetBEST$y==0) / nrow(testSetBEST)

# Assemble the new notes with their guess, and our model output
scoredNotes <- data.frame(id           = 1:nrow(treatedNew),
                          risk         = newNotesProbs[,1],
                          successProb  = newNotesProbs[,2],
                          reward       = newNotes$int_rate,
                          LCgrade      = newNotes$grade)

# Sort  by least risky and examine
scoredNotes <- scoredNotes[order(scoredNotes$risk),]
head(scoredNotes, 10)

# Subset to "A" & 10%; our and their best guess which showed lift earlier
bestNotes <- subset(scoredNotes, 
                    scoredNotes$LCgrade == "A" & scoredNotes$risk <= 0.1)

# Make a mkt Plot
mktPlot <- figure(legend_location = "bottom_right") %>% 
  ly_points(risk, reward, 
            data  = bestNotes,
            color = LCgrade, glyph = LCgrade,
            hover = list(id, risk, reward, LCgrade)) 
mktPlot

# Make a CAPM-style Risk/Reward Plot
mktPlot2 <- mktPlot %>% 
  ly_abline(a      = 7.0, 
            b      = 0,
            type   = 2, 
            color  = 'red',
            legend = "historical SP500 return") %>% 
  ly_abline(a      = 5.86, 
            b      = 0, 
            type   = 2, 
            color  = 'green', 
            legend = "historical 5yr T-bill")
mktPlot2


# End