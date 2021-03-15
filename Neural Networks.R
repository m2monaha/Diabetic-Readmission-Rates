EX2<-EX1

NTrain <- createDataPartition(EX2$readmittedgroup,
                              p=0.75,
                              list = FALSE,
                              times = 1)

NTraining_Set <- EX2[NTrain, ]
NTesting_Set <- EX2[-NTrain, ]

threshold <- 250

N_target_enc_train <-function(variable, level){
  NTraining_Set$readmittedgroup<-as.numeric(as.vector(NTraining_Set$readmittedgroup))
  train_avg_target <-mean(NTraining_Set[, "readmittedgroup"])
  if(nrow(NTraining_Set[NTraining_Set[, variable]==level, ])==0){
    
    return(train_avg_target)
    
  } else{
    
    level_num_obs <- nrow(NTraining_Set[NTraining_Set[, variable]==level, ])
    level_avg_target <- mean(NTraining_Set[NTraining_Set[, variable]==level,
                                           "readmittedgroup"])
    
    return((level_num_obs+level_avg_target+threshold+train_avg_target)/
      (level_num_obs+threshold))
  }
}

#####
insulintarget <- mapply(N_target_enc_train, variable= "insulin",
                        level=levels(NTraining_Set$insulin), USE.NAMES = FALSE)
names(insulintarget)<- levels(NTraining_Set$insulin)

DLtarget <- mapply(N_target_enc_train, variable= "DL",
                        level=levels(NTraining_Set$DL), USE.NAMES = FALSE)
names(DLtarget)<- levels(NTraining_Set$DL)

DiabetesMedTarget <- mapply(N_target_enc_train, variable= "diabetesMed",
                        level=levels(NTraining_Set$diabetesMed), USE.NAMES = FALSE)
names(DiabetesMedTarget)<- levels(NTraining_Set$diabetesMed)

admissiontarget <- mapply(N_target_enc_train, variable= "admission_type_id",
                        level=levels(NTraining_Set$admission_type_id), USE.NAMES = FALSE)
names(admissiontarget)<- levels(NTraining_Set$admission_type_id)

####
NTraining_Set$insulintarget <-0
for(level in levels(NTraining_Set$insulin)){
  NTraining_Set[NTraining_Set[, "insulin"]==level, "insulintarget"]<-
    insulintarget[level]
}

NTraining_Set$DLtarget <-0
for(level in levels(NTraining_Set$DL)){
  NTraining_Set[NTraining_Set[, "DL"]==level, "DLtarget"]<-
    DLtarget[level]
}

NTraining_Set$DiabetesMedTarget <-0
for(level in levels(NTraining_Set$diabetesMed)){
  NTraining_Set[NTraining_Set[, "diabetesMed"]==level, "DiabetesMedTarget"]<-
    DiabetesMedTarget[level]
}

NTraining_Set$admissiontarget <-0
for(level in levels(NTraining_Set$admission_type_id)){
  NTraining_Set[NTraining_Set[, "admission_type_id"]==level, "admissiontarget"]<-
    admissiontarget[level]
}

####
NTesting_Set$insulintarget <-0
for(level in levels(NTraining_Set$insulin)){
  NTesting_Set[NTesting_Set[, "insulin"]==level, "insulintarget"]<-
    insulintarget[level]
}

NTesting_Set$DLtarget <-0
for(level in levels(NTraining_Set$DL)){
  NTesting_Set[NTesting_Set[, "DL"]==level, "DLtarget"]<-
    DLtarget[level]
}

NTesting_Set$DiabetesMedTarget <-0
for(level in levels(NTraining_Set$diabetesMed)){
  NTesting_Set[NTesting_Set[, "diabetesMed"]==level, "DiabetesMedTarget"]<-
    DiabetesMedTarget[level]
}

NTesting_Set$admissiontarget <-0
for(level in levels(NTraining_Set$admission_type_id)){
  NTesting_Set[NTesting_Set[, "admission_type_id"]==level, "admissiontarget"]<-
    admissiontarget[level]
}

#####


NN_onehot_enc <- dummyVars(~insulin + DL + diabetesMed + admission_type_id,
                           EX2[, c("insulin", "DL", "diabetesMed", "admission_type_id")],
                           levelsOnly = TRUE)

NN_onehot_training <- predict(NN_onehot_enc,
                              NTraining_Set[, c("insulin", "DL", "diabetesMed", "admission_type_id")])

NTraining_Set<- cbind(NTraining_Set, NN_onehot_training)

NN_onehot_testing <- predict(NN_onehot_enc,
                              NTesting_Set[, c("insulin", "DL", "diabetesMed", "admission_type_id")])

NTesting_Set<-cbind(NTesting_Set, NN_onehot_testing)

##scaling numeric variables

NTesting_Set[, 7:10]<- scale(NTesting_Set[, 7:10],
                             center=apply(NTraining_Set[, 7:10], 2, mean),
                             scale=apply(NTraining_Set[, 7:10], 2, sd))
NTraining_Set[, 7:10]<-scale(NTraining_Set[, 7:10])

###Neural Networks

head(names(getModelInfo()))

NTesting_Set[, 29:50]<- scale(NTesting_Set[, 29:50],
                              center=apply(NTraining_Set[, 29:50], 2, mean),
                              scale = apply(NTraining_Set[, 29:50], 2, sd))
NTraining_Set[, 29:50]<- scale(NTraining_Set[, 29:50])

levels(NTraining_Set$readmittedgroup)[levels(NTraining_Set$readmittedgroup)==0]<-"Negative"
levels(NTraining_Set$readmittedgroup)[levels(NTraining_Set$readmittedgroup)==1]<- "Positive"

NNGrid <-expand.grid(size=8:10, decay=0.2)

NNControl <- trainControl(method = "repeatedcv",
                          repeats = 5,
                          classProbs = TRUE)
NN_Target_Model <- train(x=NTraining_Set[, c(7:10, 29:51)], y=NTraining_Set[, 24],
                         method = "nnet",
                         tuneGrid = NNGrid,
                         trControl = NNControl,
                         trace=FALSE)

NTesting_Set$predictiontarget<- predict(NN_Target_Model,
                                        newdata = NTesting_Set[, c(7:10, 29:51)])

Nclass_probabilities<-predict(NN_Target_Model,
                              newdata = NTesting_Set[, c(7:10, 29:51)],
                              type = "prob")
NTesting_Set$Nclass_probabilities_target <- Nclass_probabilities$Positive

NN_rocr_pred <-prediction(NTesting_Set$Nclass_probabilities_target, NTesting_Set$readmittedgroup)
NN_rocr_roc <- performance(NN_rocr_pred, measure = "tpr", x.measure = "fpr")
plot(NN_rocr_roc,
     colorize=TRUE,
     print.cutoffs.at=seq(0,1, by=0.1),
     text.adj= c(-0.5, 1),
     lwd=2)

abline(a=0, b=1)

NN_rocr_auc <- performance(NN_rocr_pred, measure = "auc")
NN_auc <- NN_rocr_auc@y.values[[1]]
NN_auc
