########## Bank Deposit Prediction ########## 

bank <- read.csv(file.choose(),header = TRUE, sep = ";")
str(bank)
summary(bank)
sum(is.na(bank))
attach(bank)
boxplot(age)$out


#### Variable siginificancy analysis ####

logit <- glm(bank$y~.,family = "binomial",data = bank)
summary(logit)

# age, pdays and previous show high p-values, therefore can be deleted

bank_new <- bank[,-c(1,14,15)]


#### Splitting ####

bank_yes <- bank_new[bank_new$y=="yes",]
bank_no <- bank_new[bank_new$y=="no",]
bank_train <- rbind(bank_no[1:27945,],bank_yes[1:3702,])
bank_test <- rbind(bank_no[27946:39922,],bank_yes[3703:5289,])


#### Model Building ####

logit1 <- glm(bank_train$y~.,family = "binomial",data = bank_train)
summary(logit1)
logit1_train_pred <- logit1$fitted.values
confusion <- table(logit1_train_pred>0.12,bank_train$y)
logit1_train_accuracy <- sum(diag(confusion))/sum(confusion) # 94.48

logit1_test_pred <- predict.glm(logit1,newdata = bank_test[,-14])
confusion1 <- table(logit1_test_pred>0.12,bank_test$y)
logit1_test_accuracy <- sum(diag(confusion1))/sum(confusion1) # 32.16
table(bank_test$y)

# ROC Curve

library(ROCR)

rocrpred <- prediction(logit_train_pred,bank_train$y)
rocrperf <- performance(rocrpred,'tpr','fpr')
str(rocrperf)

plot(rocrperf,colorize=T,test.adj=c(-0.2,1.7),print.cutoffs.at=seq(0.1,by=0.1))

rocr_cutoff <- data.frame(cut_off = rocrperf@alpha.values[[1]],fpr=rocrperf@x.values,tpr=rocrperf@y.values)
colnames(rocr_cutoff) <- c("cut_off","FPR","TPR")
rocr_cutoff <- round(rocr_cutoff,2)


library(dplyr)
rocr_cutoff <- arrange(rocr_cutoff,desc(TPR))

library(pROC)
auc <- performance(rocrpred,measure = "auc")
auc1 <- auc@y.values[[1]]
auc1