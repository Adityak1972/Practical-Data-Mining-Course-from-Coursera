# include all required libraries 
library(caret); 
library(lattice);
library(ggplot2);

# Load the dataset for training & testing for the assignment 
setwd("C:/Users/zcoe385.ZAFINLABS0/SkyDrive/Aditya Files/Documents/Learning/Coursera/Practical DataMining Coursera"); 
TrainSet <- read.csv("pml-training_DS.csv", header = TRUE);
# remove the cols which are purely meta deta in the data set 
TrainSet <- TrainSet[,-c(1:7)];
# replace all blanks in the dataset with NA to help in preprocessing 
TrainSet <- sapply(TrainSet, function(f){is.na(f)<-which(f == '');f}); 
TrainSet <- as.data.frame(TrainSet, stringsAsFactors = TRUE);
TrainSet$classe <- as.factor(TrainSet$classe);

# Identify all  variables were "NA" is >50% 
FeatureIndex = NULL; 
for (i in 1:dim(TrainSet)[2]) {
  if(sum(is.na(TrainSet[,i]))/length(TrainSet[,i]) > 0.5)
    { 
     FeatureIndex <- c(FeatureIndex, i); 
    }
}
# Remove the features which >50% imputations of "NA" values, we dont want this in the modelling set 
TrainSet <- TrainSet[,-FeatureIndex]; 

# Create cross valudationset: train set (60%) and validation set (40%) within the TrainSet
set.seed(1234); 
inTrain <- createDataPartition(y=TrainSet$classe, p=0.6, list = FALSE);
ModelSet <- TrainSet[inTrain,];
ValidSet <- TrainSet[-inTrain,];

# Build a GBM boosting based model to predict classe on the ModelSet 
time_ <- proc.time(); 
GBM_Model <- train(classe ~ ., data = ModelSet, method = "gbm", verbose = FALSE)
Timetaken <- proc.time() - time_; 

# find out what are the important variables 
varImp(GBM_Model$finalModel) 

# cross validate on the  ValidSet 
ValidPred <- predict(GBM_Model, ValidSet); 

# Develop confusion Matrix for cross validation 
confusionMatrix(data = ValidPred, reference = ValidSet$classe); 

# Test model on the test set for model out of sample testing 
#first prepare the data of the test set 
TestSet <- read.csv("pml-testing.csv", header = TRUE); 
TestSet <- TestSet[,-c(1:7)]; 
TestSet <- sapply(TestSet, function(f){is.na(f)<-which(f == '');f}); 
TestSet <- as.data.frame(TestSet, stringsAsFactors = TRUE)
FeatureIndex = NULL; 
for (i in 1:dim(TestSet)[2]) {
  if(sum(is.na(TestSet[,i]))/length(TestSet[,i]) > 0.5)
  { 
    FeatureIndex <- c(FeatureIndex, i); 
  }
}
TestSet <- TestSet[,-FeatureIndex]; 


# Predit on the testset using the GBM Model just built 
TestPred <- as.character(predict(GBM_Model, TestSet));

# write the predicted output to a file for the assignment submission 
pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}

pml_write_files(TestPred); 