install.packages("Boruta")
install.packages("dplyr")
install.packages("tidyverse")
install.packages("ggplot2")
install.packages("GGally")
install.packages("caret")
install.packages("corrplot")
install.packages("randomForest")
install.packages("janitor")
install.packages("data.table")
install.packages("lubridate")
install.packages("bit64")
install.packages("stringr")
install.packages("anytime")
install.packages("broom")
install.packages("dlookr")
install.packages("purrr")
install.packages("psych")
install.packages("ROCR")
install.packages("rpart")
install.packages("nnet")
install.packages("e1071")
install.packages("knitr")
install.packages("cluster")
install.packages("factoextra")

library(ROCR)
library(rpart)
library(nnet)
library(e1071)
library(GGally)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(Boruta)
library(caret)
library(corrplot)
library(randomForest)
library(janitor)
library(data.table)
library(lubridate)
library(bit64)
library(stringr)
library(anytime)
library(broom)
library(dlookr)
library(purrr)
library(psych)
library(knitr)
library(factoextra)
library(cluster)

MedData <- read.csv("~/Desktop/medicaldata.csv")
set.seed(123)
head(MedData)



##Data Cleaning

sapply(MedData, function(x) sum(is.na(x))/length(x))*100

sum(is.na(MedData))


##what attributes have lots of missing variables?

missingvalues <- MedData %>%
  gather(key = "key", value = "val")%>%
  mutate(is.missing = is.na(val))%>%
  group_by(key, is.missing)%>%
  summarise(num.missing= n())%>%
  filter(is.missing==T)%>%
  select(-is.missing)%>%
  arrange(desc(num.missing))

MV <- MedData%>%
  gather(key = "key", value = "val") %>%
  mutate(isna = is.na(val)) %>%
  group_by(key) %>%
  mutate(total = n()) %>%
  group_by(key, total, isna) %>%
  summarise(num.isna = n()) %>%
  mutate(pct = num.isna / total * 100)

levels <-
  (MV  %>% filter(isna == T) %>% arrange(desc(pct)))$key

percentage.plot <- MV %>%
  ggplot() +
  geom_bar(aes(x = reorder(key, desc(pct)), 
               y = pct, fill=isna), 
           stat = 'identity', alpha=0.8) +
  scale_x_discrete(limits = levels) +
  scale_fill_manual(name = "", 
                    values = c('steelblue', 'tomato3'), labels = c("Present", "Missing")) +
  coord_flip() +
  labs(title = "Percentage of missing values", x =
         'Variable', y = "% of missing values")

percentage.plot



###okay so now we know to get rid of Weight, payer code and Medical Speciality

##Lets look at Near Zero Variance

nerzo <- nearZeroVar(MedData, saveMetrics = TRUE)
print(nerzo)
plot(nerzo)

##figured out which ones to get rid of.
##now that we know what variable to get rid of, lets get rid of them

MedData$weight <- NULL
MedData$encounter_id <- NULL
MedData$patient_nbr <- NULL
MedData$payer_code <- NULL
MedData$medical_specialty <- NULL
MedData$citoglipton <- NULL
MedData$examide <- NULL
MedData$max_glu_serum <-NULL
MedData$repaglinide <- NULL
MedData$nateglinide<- NULL
MedData$chlorpropamide<-NULL
MedData$glimepiride<-NULL
MedData$acetohexamide<-NULL
MedData$tolbutamide<-NULL
MedData$acarbose<-NULL
MedData$miglitol<-NULL
MedData$troglitazone <-NULL
MedData$tolazamide<-NULL
MedData$glipizide.metformin<-NULL
MedData$glimepiride.pioglitazone<-NULL
MedData$metformin.rosiglitazone<-NULL
MedData$pioglitazone<-NULL


##now attributes need to be given right levels

MedData$admission_type_id <-factor(MedData$admission_type_id)
MedData$discharge_disposition_id <- factor(MedData$discharge_disposition_id)
MedData$admission_source_id <- factor(MedData$admission_source_id)

##what is missing still

sum(is.na(MedData))

MedData <- MedData %>%
  na.omit()

sum(is.na(MedData))


##lets recode the diagnosises, first I changed them to numeric attributes, from characters,
##so i could name their levels

##Feature Engineering

EX1<-MedData

EX1$diag_1 <- as.numeric(levels(EX1$diag_1)[EX1$diag_1])
EX1$diag_2 <- as.numeric(levels(EX1$diag_2)[EX1$diag_2])
EX1$diag_3 <- as.numeric(levels(EX1$diag_3)[EX1$diag_3])


## new NAs popped up because I did this so i recoded those are well.
EX1 <- EX1 %>%
  na.omit()

sum(is.na(EX1))

 
EX1$DL <- factor(rep("other", nrow(EX1)), ordered = F,
                 levels = c("circulatory", "respiratory", "Digestive", "Diabetes", "Injury",
                            "Musculoskeletal", "Genitourinary", "Neoplasms", "other"))
EX1$DL[EX1$diag_1>=390 & EX1$diag_1 <= 459 | EX1$diag_1==785] <- "circulatory"
EX1$DL[EX1$diag_1>=460 & EX1$diag_1 <= 519 | EX1$diag_1==786] <- "respiratory"
EX1$DL[EX1$diag_1>=520 & EX1$diag_1 <= 579 | EX1$diag_1==787] <- "Digestive"
EX1$DL[EX1$diag_1>=250 & EX1$diag_1 <251] <- "Diabetes"
EX1$DL[EX1$diag_1>800 & EX1$diag_1 <= 999] <- "Injury"
EX1$DL[EX1$diag_1>=710 & EX1$diag_1 <= 739] <- "Musculoskeletal"
EX1$DL[EX1$diag_1>=580 & EX1$diag_1 <= 629 | EX1$diag_1==788] <- "Genitourinary"
EX1$DL[EX1$diag_1>=140 & EX1$diag_1 <= 239 | EX1$diag_1>=790 & 
         EX1$diag_1 <= 799 | EX1$diag_1==780 | EX1$diag_1>=240 & EX1$diag_1 < 250 |
         EX1$diag_1>=251 & EX1$diag_1 <= 279 | EX1$diag_1>=680 & EX1$diag_1 <= 709 |
         EX1$diag_1>=001 & EX1$diag_1 <= 139 | EX1$diag_1==781 |
         EX1$diag_1==782 | EX1$diag_1==784] <- "Neoplasms"

EX1 <- mutate(EX1, DL = case_when(
  DL %in%  "circulatory"       ~ "D1",
  DL %in%  "respiratory"      ~ "D2",
  DL %in%  "Digestive"      ~ "D3",
  DL %in%  "Diabetes"      ~ "D4",
  DL %in%  "Injury"     ~ "D5",
  DL %in%  "Musculoskeletal"    ~ "D6",
  DL%in%      "Genitourinary"        ~ "D7",
  DL%in% "Neoplasms"~ "D8",
  DL%in% "other" ~"D9"
)
) 


##Dia 2
EX1$DL2 <- factor(rep("other", nrow(EX1)), ordered = F,
                  levels = c("circulatory", "respiratory", "Digestive", "Diabetes", "Injury",
                             "Musculoskeletal", "Genitourinary", "Neoplasms", "other"))
EX1$DL2[EX1$diag_2>=390 & EX1$diag_2 <= 459 | EX1$diag_2==785] <- "circulatory"
EX1$DL2[EX1$diag_2>=460 & EX1$diag_2 <= 519 | EX1$diag_2==786] <- "respiratory"
EX1$DL2[EX1$diag_2>=520 & EX1$diag_2 <= 579 | EX1$diag_2==787] <- "Digestive"
EX1$DL2[EX1$diag_2>=250 & EX1$diag_2 <251] <- "Diabetes"
EX1$DL2[EX1$diag_2>800 & EX1$diag_2 <= 999] <- "Injury"
EX1$DL2[EX1$diag_2>=710 & EX1$diag_2 <= 739] <- "Musculoskeletal"
EX1$DL2[EX1$diag_2>=580 & EX1$diag_2 <= 629 | EX1$diag_2==788] <- "Genitourinary"
EX1$DL2[EX1$diag_2>=140 & EX1$diag_2 <= 239 | EX1$diag_2>=790 & 
          EX1$diag_2 <= 799 | EX1$diag_2==780 | EX1$diag_2>=240 & EX1$diag_2 < 250 |
          EX1$diag_2>=251 & EX1$diag_2 <= 279 | EX1$diag_2>=680 & EX1$diag_2 <= 709 |
          EX1$diag_2>=001 & EX1$diag_2 <= 139 | EX1$diag_2==781 |
          EX1$diag_2==782 | EX1$diag_2==784] <- "Neoplasms"


EX1 <- mutate(EX1, DL2 = case_when(
  DL2 %in%  "circulatory"       ~ "D1",
  DL2 %in%  "respiratory"      ~ "D2",
  DL2 %in%  "Digestive"      ~ "D3",
  DL2 %in%  "Diabetes"      ~ "D4",
  DL2 %in%  "Injury"     ~ "D5",
  DL2 %in%  "Musculoskeletal"    ~ "D6",
  DL2%in%      "Genitourinary"        ~ "D7",
  DL2%in% "Neoplasms"~ "D8",
  DL2%in% "other" ~"D9"
)
)  

##diagnoise 3

EX1$DL3 <- factor(rep("other", nrow(EX1)), ordered = F,
                  levels = c("circulatory", "respiratory", "Digestive", "Diabetes", "Injury",
                             "Musculoskeletal", "Genitourinary", "Neoplasms", "other"))
EX1$DL3[EX1$diag_3>=390 & EX1$diag_3 <= 459 | EX1$diag_3==785] <- "circulatory"
EX1$DL3[EX1$diag_3>=460 & EX1$diag_3 <= 519 | EX1$diag_3==786] <- "respiratory"
EX1$DL3[EX1$diag_3>=520 & EX1$diag_3 <= 579 | EX1$diag_3==787] <- "Digestive"
EX1$DL3[EX1$diag_3>=250 & EX1$diag_3 <251] <- "Diabetes"
EX1$DL3[EX1$diag_3>800 & EX1$diag_3 <= 999] <- "Injury"
EX1$DL3[EX1$diag_3>=710 & EX1$diag_3 <= 739] <- "Musculoskeletal"
EX1$DL3[EX1$diag_3>=580 & EX1$diag_3 <= 629 | EX1$diag_3==788] <- "Genitourinary"
EX1$DL3[EX1$diag_3>=140 & EX1$diag_3 <= 239 | EX1$diag_3>=790 & 
          EX1$diag_3 <= 799 | EX1$diag_3==780 | EX1$diag_3>=240 & EX1$diag_3 < 250 |
          EX1$diag_3>=251 & EX1$diag_3 <= 279 | EX1$diag_3>=680 & EX1$diag_3 <= 709 |
          EX1$diag_3>=001 & EX1$diag_3 <= 139 | EX1$diag_3==781 |
          EX1$diag_3==782 | EX1$diag_3==784] <- "Neoplasms"

EX1 <- mutate(EX1, DL3 = case_when(
  DL3 %in%  "circulatory"       ~ "D1",
  DL3 %in%  "respiratory"      ~ "D2",
  DL3 %in%  "Digestive"      ~ "D3",
  DL3 %in%  "Diabetes"      ~ "D4",
  DL3 %in%  "Injury"     ~ "D5",
  DL3 %in%  "Musculoskeletal"    ~ "D6",
  DL3%in%      "Genitourinary"        ~ "D7",
  DL3%in% "Neoplasms"~ "D8",
  DL3%in% "other" ~"D9"
)
)


##recoding variables


EX1 <- mutate(EX1, insulin = case_when(
  insulin %in%  "No"       ~ "Insulin_1",
  insulin %in%  "Down"      ~ "Insulin_2",
  insulin %in%  "Steady"      ~ "Insulin_3",
  insulin %in%  "Up"      ~ "Insulin_4"
)
)

EX1 <- mutate(EX1, diabetesMed = case_when(
  diabetesMed %in%  "No"       ~ "Diabetes_1",
  diabetesMed %in%  "Yes"      ~ "Diabetes_2"
)
)

EX1 <- mutate(EX1, admission_type_id = case_when(
  admission_type_id %in%  1      ~ "ASI_1",
  admission_type_id %in%  2      ~ "ASI_2",
  admission_type_id %in%  3      ~ "ASI_3",
  admission_type_id %in%  4      ~ "ASI_4",
  admission_type_id %in%  5     ~ "ASI_5",
  admission_type_id %in%  6    ~ "ASI_6",
  admission_type_id %in%      7        ~ "ASI_7",
  admission_type_id %in% 8 ~ "ASI_8"
)
) 

##some of the discharge disopsition ids were unnecessary due to them either being expired, in hospice or null       
##get rid of expired values
EX1<-EX1%>%filter(discharge_disposition_id !=11)
EX1<-EX1%>%filter(discharge_disposition_id !=18)
EX1<-EX1%>%filter(discharge_disposition_id !=19)
EX1<-EX1%>%filter(discharge_disposition_id !=20)
EX1<-EX1%>%filter(discharge_disposition_id !=21)
EX1<-EX1%>%filter(discharge_disposition_id !=25)
EX1<-EX1%>%filter(discharge_disposition_id !=26)
EX1<-EX1%>%filter(discharge_disposition_id !=13)
EX1<-EX1%>%filter(discharge_disposition_id !=14)


EX1 <- mutate(EX1, discharge_disposition_id = case_when(
  discharge_disposition_id %in%  1       ~ "Discharge_1",
  discharge_disposition_id %in%  6      ~ "Discharge_1",
  discharge_disposition_id %in%  8     ~ "Discharge_1",
  discharge_disposition_id %in%  7       ~ "Discharge_2",
  discharge_disposition_id %in%  2      ~ "Discharge_3",
  discharge_disposition_id %in%  3     ~ "Discharge_3",
  discharge_disposition_id %in%  4 ~"Discharge_3",
  discharge_disposition_id %in%  5 ~"Discharge_3",
  discharge_disposition_id %in%  9 ~"Discharge_3",
  discharge_disposition_id %in%  10 ~"Discharge_3",
  discharge_disposition_id %in%  16:17 ~"Discharge_4",
  discharge_disposition_id %in%  22:24 ~"Discharge_3",
  discharge_disposition_id %in%  12 ~"Discharge_1",
  discharge_disposition_id %in%  27:30 ~"Discharge_3"
)
)

##encoding into a binary target

EX1$readmittedgroup <- ifelse(EX1$readmitted == "<30", 1, 0)
table(EX1$readmittedgroup)
prop.table(table(EX1$readmittedgroup))

     
##boruta test to see correlations between target variale and others.
EX2 <-EX1
     
set.seed(123)
train.boruta <-Boruta(readmittedgroup~.,
                           data = EX2, doTrace = 2, maxRuns=30)
     
print(train.boruta)
     
plot(train.boruta, las=2,
          cex.axis=0.7)
     
     
##get rid of other unnecessary data     
EX1$readmitted <-NULL
EX1$glipizide <- NULL
EX1$glyburide<- NULL
EX1$glyburide.metformin<-NULL
EX1$rosiglitazone<-NULL
EX1$diag_1<-NULL
EX1$diag_2<-NULL
EX1$diag_3<-NULL
     
EX1$readmittedgroup<- factor(EX1$readmittedgroup)
EX1$admission_type_id<- factor(EX1$admission_type_id)
EX1$DL<- factor(EX1$DL)
EX1$DL2<-factor(EX1$DL2)
EX1$DL3<-factor(EX1$DL3)
##make sure they are factors     
class(EX1$DL)
class(EX1$admission_type_id)
class(EX1$time_in_hospital)
class(EX1$num_lab_procedures)
class(EX1$num_procedures)    
class(EX1$num_medications)
class(EX1$diabetesMed)
class(EX1$insulin)
class(EX1$readmittedgroup)
EX1$insulin<-factor(EX1$insulin)
EX1$diabetesMed<-factor(EX1$diabetesMed)
EX1$time_in_hospital<-as.numeric(EX1$time_in_hospital)
EX1$num_lab_procedures<-as.numeric(EX1$num_lab_procedures)
EX1$num_procedures<-as.numeric(EX1$num_procedures)
EX1$num_medications<-as.numeric(EX1$num_medications)


EX1=EX1 %>% mutate_if(is.character, as.factor)

sum(is.na(EX1))

EX1 <- EX1 %>%
  na.omit()
     
indtrain <- createDataPartition(EX1$readmittedgroup,
                                     p=0.75,
                                     list = FALSE,
                                     times = 1)
     
settrain <-EX1[indtrain, ]
settest <-EX1[-indtrain, ]
     
sum(is.na(settrain))
sum(is.na(settest))
   
     
onehot_enc <-dummyVars(~ diabetesMed + insulin + admission_type_id +DL,
                            EX1[, c("diabetesMed", "insulin", "admission_type_id", "DL")],
                            levelsOnly = TRUE)

onehottraining <- predict(onehot_enc,
                               settrain[, c("diabetesMed", "insulin", "admission_type_id", "DL")])
settrain<- cbind(settrain, onehottraining)

onehottesting <- predict(onehot_enc, settest[, c("diabetesMed", "insulin", "admission_type_id", "DL")])
settest <- cbind(settest, onehottesting)


     
settest[, 7:10]<- scale(settest[, 7:10],
                             center = apply(settrain[, 7:10], 2, mean),
                             scale = apply(settrain[, 7:10], 2, sd))
settrain[, 7:10]<-scale(settrain[, 7:10])

     
levels(settrain$readmittedgroup)[levels(settrain$readmittedgroup)==0]<- "Negative"
levels(settrain$readmittedgroup)[levels(settrain$readmittedgroup)==1]<- "Positive"
     
modelLookup("rf")
     
recc_mtry <- floor(sqrt(ncol(settrain[, -1*c(25:47)])))
rfGrid <- expand.grid(mtry= c(recc_mtry-2, recc_mtry, recc_mtry+2))                   
     
rfControl <- trainControl(method = "oob",
                               classProbs = TRUE)

rfControl <- rfControl %>%
  na.omit() 

rf_onehot <- train(x=settrain[, -1*c(24:47)], y=settrain[, 24],
                        method="rf",
                        tuneGrid= rfGrid,
                        trControl = rfControl,
                        importance = TRUE,
                        trace=FALSE)

settest$prediction_onehot <- predict(rf_onehot,
                                          newdata = settest[, -1*c(25:47)])  
class_probabilities <- predict(rf_onehot,
                                    newdata = settest[, -1*c(24:47)],
                                    type = "prob")
     
settest$class_probabilities_onehot <- class_probabilities$Positive 
     
rocr_pred <- prediction(settest$class_probabilities_onehot, settest$readmittedgroup)  
rocr_roc <- performance(rocr_pred, measure = "tpr", x.measure = "fpr")
     
plot(rocr_roc,
          colorize=TRUE,
          print.cutoffs.at= seq(0, 1, by=0.1),
          text.adj = c(-0.5, 1),
          lwd = 2)
     
abline(a=0, b=1)

     
rocr_auc <- performance(rocr_pred, measure = "auc")
auc <-rocr_auc@y.values[[1]]
auc     

cali_curve <- calibration(readmittedgroup ~ class_probabilities_onehot,
                          data = settest,
                          class = 1)     



plot(cali_curve)



####feature importance

rf_varImp <- varImp(rf_onehot, type=2)
plot(rf_varImp)


###scaling to calibrate predicted probablies 

t1_ind <-createDataPartition(settrain$readmittedgroup,
                             p=0.8,
                             list = FALSE,
                             times = 1)
t1<- settrain[t1_ind, ]
t2<- settrain[-t1_ind, ]

rf_onehot_1t <-train(x=t1[, -1*c(24:47)],
                     y=t1[, 24],
                          method="rf",
                          tuneGrid = rfGrid,
                          trControl = rfControl,
                          trace = FALSE)

rf_onehot_t1_t2_p <- predict(rf_onehot_1t,
                             newdata = t2[, -1*c(24:47)],
                             type = "prob")

rf_onehot_platt<- train(y~x,
                        data=data.frame(x=c(rf_onehot_t1_t2_p[, "Positive"]),
                                        y=t2$readmittedgroup),
                        method="glm",
                        family="binomial",
                        trControl = trainControl(classProbs = TRUE),
                        trace=FALSE)     

rf_onehot_t1_test_p <- predict(rf_onehot_1t,
                               newdata = settest[, -1*c(24:47)],
                               type = "prob")

class_probabilities <- predict(rf_onehot_platt,
                               newdata = data.frame(x=c(rf_onehot_t1_test_p[, "Positive"])),
                               type = "prob")

settest$class_probs_onehot_platt <- class_probabilities$Positive

preds_list <-list(settest$class_probabilities_onehot,
                  settest$class_probs_onehot_platt)

actuals_list <- list(settest$readmittedgroup, settest$readmittedgroup)

m <- length(actuals_list)

rocr_preds <- prediction(preds_list, actuals_list)
rocr_rocs <- performance(rocr_preds,
                         measure = "tpr",
                         x.measure = "fpr")

plot(rocr_rocs,
     col=as.list(1:m),
     lwd=2)
legend(x="bottomright",
       legend = c("One-Hot", "One-Hot Platt"),
       fill = 1:m)
abline(a=0, b=1)

rocr_aucs<- performance(rocr_preds,
                        measure = "auc")
aucs<-rocr_aucs@y.values
aucs

calibration_curve <-calibration(readmittedgroup~class_probabilities_onehot +
                                  class_probs_onehot_platt,
                                data = settest,
                                class = 1)

plot(calibration_curve,
     type= "1",
     auto.key = list(columns = 2, lines=TRUE, points=FALSE))
