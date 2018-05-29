
#'The function train_model reads the data from the csv file, apply multiple models and save the models output as .rds files.
#' @param No input Required
#' @export  
train_model<- function()
{
  library("caret")
 
  raw_data <- read.csv('/Projects/R/CSX415-project/phone_mark/data/bank.csv',sep=';')
  
  index<-createDataPartition(raw_data$y,p=0.5,list=FALSE)
  trainset<- raw_data[index,]
  testset<-raw_data[-index,]
  
  outcomeName<-'y'
  predictors<-names(trainset)[!names(trainset) %in% outcomeName]
  
  model_gbm<-train(trainset[,predictors],trainset[,outcomeName],method='gbm')
  model_rf<-train(trainset[,predictors],trainset[,outcomeName],method='rf')
  model_nnet<-train(trainset[,predictors],trainset[,outcomeName],method='nnet')
  model_glm<-train(trainset[,predictors],trainset[,outcomeName],method='glm')
 
  saveRDS(model_gbm, "./data/model_gbm.rds")
  saveRDS(model_rf, "./data/model_rf.rds")
  saveRDS(model_nnet, "./data/model_nnet.rds")
  saveRDS(model_glm, "./data/model_glm.rds")
  
}

#'  This function reads the GBM model from the data directory and then predict the outcome based on the test data set.
#' @param No input Required
#' @export 
predictModel_gbm<- function()
{
  
  print(getwd())
  

  predict_gbm<-predict.train(object=model_gbm,testset[,predictors],type="raw")
  table(predict_gbm)
  confusionMatrix(predict_gbm,testset[,outcomeName])
}

#'  This function reads the Random Forest model from the data directory and then predict the outcome based on the test data set.
#' @param No input Required
#' @export 
predictModel_rf<- function()
{
  
  model_rf <- readRDS("./data/model_rf.rds")
  
  predict_rf<-predict.train(object=model_rf,testset[,predictors],type="raw")
  table(predict_rf)
  confusionMatrix(predict_rf,testset[,outcomeName])
}

#'  This function reads the Neural Network model from the data directory and then predict the outcome based on the test data set.
#' @param No input Required
#' @export 
predictModel_nnet<- function()
{

  model_nnet<-readRDS("./data/model_nnet.rds")
  
  predict_nnet<-predict.train(object=model_nnet,testset[,predictors],type="raw")
  table(predict_nnet)
  confusionMatrix(predict_nnet,testset[,outcomeName])
}

#'  This function reads the GLM model from the data directory and then predict the outcome based on the test data set.
#' @param No input Required
#' @export 
predictModel_glm<- function()
{
  
  model_glm <- readRDS("./data/model_glm.rds")
  
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
