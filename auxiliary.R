##################################################
#this file does some of the preliminary work
#of the .Rmd file, like setting the directory, 
#defining some helper functions, loading the data,
#loading packages, what have you
##################################################

rm(list=ls())

#set seed
set.seed(23)

#packages 
library(caret)
library(dplyr)

##################################################

#define a couple of functions to assist me

#this function gives me feedback after I've fit a model
#returns a list with important vars, and a confusion matrix
#it does this after first having printed those two objects

feedback<-function(fitobject) { 
  importantvars<-varImp(fitobject)
  predictions<-predict(fitobject,pmltrain_test)
  confusionmatrix<-confusionMatrix(predictions,pmltrain_test$classe)
  accuracy<-confusionmatrix$overall[c(1,3,4)]
  return(list(importantvars,accuracy))
}

#this function takes vars I want to use for prediction, 
#and makes a formula object with 'classe' as the dependent variable

makeformula<-function(vars) {
  return(as.formula(paste("classe ~ ",paste(vars,collapse=" + "),sep="")))
}

#this function is taken from the assignment page
#it outputs my predictions on the test set as a .txt file
pml_write_files = function(x) {
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}

##################################################