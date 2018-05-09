#' The function returns a square of the given number
#' @param input a numeric value
#' @export
square<-function(x)
{
  return(x^2)
}

#https://gist.github.com/primaryobjects/d02b93f1e539a9dd2c85

#' @param input
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
  
  predict_rf<-predict.train(object=model_rf,testset[,predictors],type="raw")
  table(predict_rf)
  confusionMatrix(predict_rf,testset[,outcomeName])
  
  predict_gbm<-predict.train(object=model_gbm,testset[,predictors],type="raw")
  table(predict_gbm)
  confusionMatrix(predict_gbm,testset[,outcomeName])
  
  predict_nnet<-predict.train(object=model_nnet,testset[,predictors],type="raw")
  table(predict_nnet)
  confusionMatrix(predict_nnet,testset[,outcomeName])
  
  predict_glm<-predict.train(object=model_nnet,testset[,predictors],type="raw")
  table(predict_glm)
  confusionMatrix(predict_glm,testset[,outcomeName])
  
}