{#Lectura de datos
  pacientes<-read_xlsx("DatosPacientes.xlsx")
  test_set<-as.data.frame(pacientes)
  colnames(test_set)<-c("CI","sex","age","pneumonia","diabetes","copd","asthma",
                        "cardiovascular","inmsupr","hypertension","obesity","renal_chronic",
                        "tobacco","other_disease")
  test_set$pneumonia[test_set$pneumonia==0]<-2
  test_set$diabetes[test_set$diabetes==0]<-2
  test_set$copd[test_set$copd==0]<-2
  test_set$asthma[test_set$asthma==0]<-2
  test_set$cardiovascular[test_set$cardiovascular==0]<-2
  test_set$inmsupr[test_set$inmsupr==0]<-2
  test_set$hypertension[test_set$hypertension==0]<-2
  test_set$obesity[test_set$obesity==0]<-2
  test_set$renal_chronic[test_set$renal_chronic==0]<-2
  test_set$tobacco[test_set$tobacco==0]<-2
  test_set$other_disease[test_set$other_disease==0]<-2
}

{#predictores
  lda_pred<-predict(train_lda,test_set)#predicting the results
  glm_pred<-predict(train_glm,test_set)#predicting the results
  qda_pred<-predict(train_qda,test_set)#predicting the results
  rpart_pred<-predict(train_rpart,test_set)#predicting the results
  ada_pred<-predict(train_ada,test_set)#predicting the results
  cit_pred<-predict(train_cit,test_set)#predicting results
  ##########
  #Ensemble#
  ##########
  #Using the other six models to create an ensemble for the seventh model
  #If the majority of the models predict an inpatient, the ensemble will 
  #predict an inpatient. If the majority of models predict an outpatient it 
  #will predict an outpatient. If there is a tie, the ensemble will predict
  #an outpatient, because there are more outpatients in the  training data set.
  ensemble<-data.frame(LDA=as.numeric(lda_pred),
                       QDA=as.numeric(qda_pred),
                       GLM=as.numeric(glm_pred),
                       RPART=as.numeric(rpart_pred),
                       ADA=as.numeric(ada_pred),
                       CIT=as.numeric(cit_pred))
  ensemble_pred<-ifelse(rowMeans(ensemble)<=9/6,1,2)#There are more outpatients
  #than inpatients, so in case of tie, predict outpatient
  
  ############
  #Ensemble 2#
  ############
  #From the plots could be seen that there are considerable more inpatients  with
  #pneumonia,chronic kidney disease, COPD and diabetes than outpatients with the
  #same preconditions, if the patient has one of the pre-conditions it will predict
  #inpatient
  ensemble_pred2<-ifelse(rowMeans(ensemble)<=9/6,1,2)
  ind2<-which(rowMeans(ensemble)==9/6)#for ties in the ensemble
  ensemble_pred2[ind2]<-ifelse(test_set$pneumonia[ind2]==1,2,
                               ifelse(test_set$renal_chronic[ind2]==1,2,
                                      ifelse(test_set$copd[ind2]==1,2,
                                             ifelse(test_set$diabetes[ind2]==1,2,1))))
  
  ############
  #Ensemble 3#
  ############
  #Similar to Ensemble 2 but using all the pre-conditions that have more inpatients
  #than outpatients except age to predict an inpatient
  ensemble_pred3<-ifelse(rowMeans(ensemble)<=9/6,1,2)
  ind2<-which(rowMeans(ensemble)==9/6)#for ties in the ensemble
  ensemble_pred3[ind2]<-ifelse(test_set$pneumonia[ind2]==1,2,
                               ifelse(test_set$renal_chronic[ind2]==1,2,
                                      ifelse(test_set$copd[ind2]==1,2,
                                             ifelse(test_set$diabetes[ind2]==1,2,
                                                    ifelse(test_set$inmsupr[ind2]==1,2,
                                                           ifelse(test_set$hypertension[ind2]==1,2,
                                                                  ifelse(test_set$cardiovascular[ind2]==1,2,1)))))))
}
  

# RESULTADOS: 1 = NO HOSPITALIZAR, 2=HOSPITALIZAR


{
    resultados<-data.frame(row.names = c("C.I.","LDA","GLM","QDA","RPART","BTREE","CTREE","ENSEMBLE","ENSEMBLE2","ENSEMBLE3"),
                         RESULTADOS=c(test_set$CI,lda_pred,glm_pred,qda_pred,rpart_pred,ada_pred,cit_pred,ensemble_pred,ensemble_pred2,ensemble_pred3))
  resultados$LDA[resultados$LDA==1]<-"NO"
  resultados$LDA[resultados$LDA==2]<-"SI"
  resultados$GLM[resultados$GLM==1]<-"NO"
  resultados$GLM[resultados$GLM==2]<-"SI"
  resultados$QDA[resultados$QDA==1]<-"NO"
  resultados$QDA[resultados$QDA==2]<-"SI"
  resultados$RPART[resultados$RPART==1]<-"NO"
  resultados$RPART[resultados$RPART==2]<-"SI"
  resultados$BTREE[resultados$BTREE==1]<-"NO"
  resultados$BTREE[resultados$BTREE==2]<-"SI"
  resultados$CTREE[resultados$CTREE==1]<-"NO"
  resultados$CTREE[resultados$CTREE==2]<-"SI"
  resultados$ENSEMBLE[resultados$ENSEMBLE==1]<-"NO"
  resultados$ENSEMBLE[resultados$ENSEMBLE==2]<-"SI"
  resultados$ENSEMBLE2[resultados$ENSEMBLE2==1]<-"NO"
  resultados$ENSEMBLE2[resultados$ENSEMBLE2==2]<-"SI"
  resultados$ENSEMBLE3[resultados$ENSEMBLE3==1]<-"NO"
  resultados$ENSEMBLE3[resultados$ENSEMBLE3==2]<-"SI"
  
  
  write_xlsx(resultados,"Resultados.xlsx")
  
}