{
  ###########
  #Libraries#
  ###########
  if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
  if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
  if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
  if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
  if(!require(gridExtra)) install.packages("gridExtra", repos = "http://cran.us.r-project.org")
  if(!require(ada)) install.packages("ada", repos = "http://cran.us.r-project.org")
  if(!require(plyr)) install.packages("plyr", repos = "http://cran.us.r-project.org")
  if(!require(xgboost)) install.packages("xgboost", repos = "http://cran.us.r-project.org")
  if(!require(party)) install.packages("party", repos = "http://cran.us.r-project.org")
  if(!require(readxl)) install.packages("readxl", repos = "http://cran.us.r-project.org")
  if(!require(writexl)) install.packages("writexl", repos = "http://cran.us.r-project.org")
  
  
  library(tidyverse)
  library(caret)
  library(data.table)
  library(ggplot2)
  library(gridExtra)
  library(ada)
  library(plyr)
  library(xgboost)
  library(party)
  library(readxl)
  library(writexl)
  
  ####################
  #Importing Data Set#
  ####################
  #Downloading data and creating data set
  url <- "https://github.com/RPereira98/Covid-Pre-condition/raw/main/covid.zip"
  dl <- tempfile()
  download.file(url, dl)
  unzip(dl,"covid.csv")
  covid_dat <- read_csv("covid.csv")
  covid_dat<-data.frame(covid_dat)
  #Covid_res=1 means that the patient is covid positive, using only the covid positive
  #patients for the data
  ind<- which(covid_dat$covid_res==1)
  covid_dat<-covid_dat[ind,]
  #Dates, id of patient,intubation,pregnancy,ICU will  not be used for this algorithm
  #Covid results column is not more necessary, all patients are positive in the new data set
  covid_dat<-covid_dat[,-c(1,4,5,6,7,10,21,22,23)]
  #97,98,99 values are NAs, not useful
  ind<- which((covid_dat$pneumonia%in%c(97,98,99))|(covid_dat$diabetes%in%c(97,98,99))|(covid_dat$copd%in%c(97,98,99))|(covid_dat$asthma%in%c(97,98,99))|(covid_dat$inmsupr%in%c(97,98,99))|
                (covid_dat$hypertension%in%c(97,98,99))|(covid_dat$other_disease%in%c(97,98,99))|(covid_dat$cardiovascular%in%c(97,98,99))|(covid_dat$obesity%in%c(97,98,99))|
                (covid_dat$renal_chronic%in%c(97,98,99))|(covid_dat$tobacco%in%c(97,98,99)))
  covid_dat<-covid_dat[-ind,]
  
  ##############################
  #Split train set and test set#
  ##############################
  #The test set is 20% of the original data set
  set.seed(1, sample.kind="Rounding")
  test_index <- createDataPartition(y = covid_dat$patient_type, times = 1, p = 0.2, list = FALSE)
  train_set<-covid_dat[-test_index,]
}
{
  ################################
  #Linear discriminant analysis#
  ################################
  #Using linear discriminant analysis for the first model
  train_lda<-train(as.factor(patient_type)~as.factor(sex)+as.factor(pneumonia)+age+
                     as.factor(diabetes)+as.factor(copd)+as.factor(asthma)+as.factor(inmsupr)+
                     as.factor(hypertension)+as.factor(other_disease)+as.factor(cardiovascular)+
                     as.factor(obesity)+as.factor(renal_chronic)+as.factor(tobacco),
                   data =train_set,method="lda")#training the model
  
  
  
  ##########################
  #Generalized linear model#
  ##########################
  #Using a generalized linear model for the second model
  train_glm<-train(as.factor(patient_type)~as.factor(sex)+as.factor(pneumonia)+age+
                     as.factor(diabetes)+as.factor(copd)+as.factor(asthma)+as.factor(inmsupr)+
                     as.factor(hypertension)+as.factor(other_disease)+as.factor(cardiovascular)+
                     as.factor(obesity)+as.factor(renal_chronic)+as.factor(tobacco),
                   method="glm",data =train_set,family = "binomial")#training the model
  
  
  #################################
  #Quadratic discriminant analysis#
  #################################
  #Using quadratic discriminatory analysis for the third model
  train_qda<-train(as.factor(patient_type)~as.factor(sex)+as.factor(pneumonia)+age+
                     as.factor(diabetes)+as.factor(copd)+as.factor(asthma)+as.factor(inmsupr)+
                     as.factor(hypertension)+as.factor(other_disease)+as.factor(cardiovascular)+
                     as.factor(obesity)+as.factor(renal_chronic)+as.factor(tobacco),
                   data =train_set,method="qda")#training the model
  
  
  #####################
  #Classification tree#
  #####################
  #Using a classification tree for the fourth model
  train_rpart<-train(as.factor(patient_type)~as.factor(sex)+as.factor(pneumonia)+age+
                       as.factor(diabetes)+as.factor(copd)+as.factor(asthma)+as.factor(inmsupr)+
                       as.factor(hypertension)+as.factor(other_disease)+as.factor(cardiovascular)+
                       as.factor(obesity)+as.factor(renal_chronic)+as.factor(tobacco),
                     data = train_set,method="rpart")#training the model
  
  
  #############################
  #Boosted Classification Tree#
  #############################
  #Using Boosted Classification Tree for the fifth model
  train_ada<-train(as.factor(patient_type)~as.factor(sex)+as.factor(pneumonia)+age+
                     as.factor(diabetes)+as.factor(copd)+as.factor(asthma)+as.factor(inmsupr)+
                     as.factor(hypertension)+as.factor(other_disease)+as.factor(cardiovascular)+
                     as.factor(obesity)+as.factor(renal_chronic)+as.factor(tobacco),
                   method="xgbTree", trControl = trainControl("cv", number = 5),
                   data =train_set)#training the model
  train_ada$bestTune
 
  ############################
  #Conditional Inference Tree#
  ############################
  #Using a Conditional Interference Tree for the sixth model
  train_cit<-train(as.factor(patient_type)~as.factor(sex)+as.factor(pneumonia)+age+
                     as.factor(diabetes)+as.factor(copd)+as.factor(asthma)+as.factor(inmsupr)+
                     as.factor(hypertension)+as.factor(other_disease)+as.factor(cardiovascular)+
                     as.factor(obesity)+as.factor(renal_chronic)+as.factor(tobacco),
                   method="ctree", data =train_set)#training the model
  
  
}