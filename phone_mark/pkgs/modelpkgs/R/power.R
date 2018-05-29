
#'The function train_model reads the data from the csv file, apply multiple models and save the models output as .rds files.
#' @param No input Required
#' @export  
train_model<- function()
{
  library("caret")
 
  raw_data <- read.csv(paste(getwd(),'/phone_mark/data/bank.csv',sep=""),sep=';')
  
  index<-createDataPartition(raw_data$y,p=0.5,list=FALSE)
  trainset<- raw_data[index,]
  testset<-raw_data[-index,]
  
  outcomeName<-'y'
  predictors<-names(trainset)[!names(trainset) %in% outcomeName]
  
  model_gbm<-train(trainset[,predictors],trainset[,outcomeName],method='gbm')
  model_rf<-train(trainset[,predictors],trainset[,outcomeName],method='rf')
  model_nnet<-train(trainset[,predictors],trainset[,outcomeName],method='nnet')
  model_glm<-train(trainset[,predictors],trainset[,outcomeName],method='glm')
 
  saveRDS(model_gbm, paste(getwd(),"/phone_mark/pkgs/modelpkgs/data/model_gbm.rds",sep=""))
  saveRDS(model_rf, paste(getwd(),"/phone_mark/pkgs/modelpkgs/data/model_rf.rds",sep=""))
  saveRDS(model_nnet, paste(getwd(),"/phone_mark/pkgs/modelpkgs/data/model_nnet.rds",sep=""))
  saveRDS(model_glm, paste(getwd(),"/phone_mark/pkgs/modelpkgs/data/model_glm.rds",sep=""))
  
}

#'  This function reads the GBM model from the data directory and then predict the outcome based on the test data set.
#' @param No input Required
#' @export 
predictModel_gbm<- function()
{
  library("caret")
  print(getwd())
  model_gbm <- readRDS(paste(getwd(),"/phone_mark/pkgs/modelpkgs/data/model_gbm.rds",sep=""))
  

  predict_gbm<-predict.train(object=model_gbm,testset[,predictors],type="raw")
  table(predict_gbm)
  confusionMatrix(predict_gbm,testset[,outcomeName])
}

#'  This function reads the Random Forest model from the data directory and then predict the outcome based on the test data set.
#' @param No input Required
#' @export 
predictModel_rf<- function()
{
  library("caret")
  model_rf <- readRDS(paste(getwd(),"/phone_mark/pkgs/modelpkgs/data/model_rf.rds",sep=""))
  
  predict_rf<-predict.train(object=model_rf,testset[,predictors],type="raw")
  table(predict_rf)
  confusionMatrix(predict_rf,testset[,outcomeName])
}

#'  This function reads the Neural Network model from the data directory and then predict the outcome based on the test data set.
#' @param No input Required
#' @export 
predictModel_nnet<- function()
{

  library("caret")
  model_nnet<-readRDS(paste(getwd(),"/phone_mark/pkgs/modelpkgs/data/model_nnet.rds",sep=""))
  
  predict_nnet<-predict.train(object=model_nnet,testset[,predictors],type="raw")
  table(predict_nnet)
  confusionMatrix(predict_nnet,testset[,outcomeName])
}

#'  This function reads the GLM model from the data directory and then predict the outcome based on the test data set.
#' @param No input Required
#' @export 
predictModel_glm<- function()
{
  library("caret")
  model_glm <- readRDS(paste(getwd(),"/phone_mark/pkgs/modelpkgs/data/model_glm.rds",sep=""))
  
  predict_glm<-predict.train(object=model_glm,testset[,predictors],type="raw")
  table(predict_glm)
  confusionMatrix(predict_glm,testset[,outcomeName])
}

#'  This function reads the project location from user and return the path to the caller.
#' @param No input Required
#' @export 
readprojectpath<- function()
{ 
  n <- readline(prompt="Enter the project location (CSX415-project): ")
  return(as.character(n))
}
