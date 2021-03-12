library("readxl")

pacientes<-read_xlsx("DatosPacientes.xlsx")
test_set<-as.data.frame(pacientes)
colnames(test_set)<-c("C.I.","sex","age","pneumonia","diabetes","copd","asthma",
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

