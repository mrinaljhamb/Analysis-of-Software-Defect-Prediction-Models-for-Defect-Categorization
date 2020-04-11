data=read.csv("C:\\Users\\Mrinal\\Desktop\\minor porject\\data\\jedit.csv")
data=data[,-22]
View(data)
data$target=data$target-1


#################Creation of training and testing dataset. So that we can test the created model on both the sets to look at the training and testing accuracy in order to judge that how effective over modelled prediction structure will be on unseen data.
l=sample(1:length(data$target),floor(0.7*length(data$target)))
train_data=data[l,]
test_data=data[-l,]


###################Modelling on train data.
lm_train=glm(target~.,data=train_data,family=binomial)
summary(lm_train)
#Asteriks against every entry in the table genrated by the summary signifies the importance of coresponding variables on the result.

#Training accuracy.
train.prob =predict (lm_train,type ="response")
train.pred=rep("0",length(train_data$target))
train.pred[train.prob>0.5]="1"
table(train.pred,train_data$target,dnn = c('predicted','true'))
accu_train=(sum((train.pred==train_data$target)/length(train_data$target)))*100
accu_train
?predict
#Testing accuracy.
test.prob =predict(lm_train,test_data,type ="response")
test.pred_lr=rep("0",length(test_data$target))
test.pred_lr[test.prob>0.5]="1"
table(test.pred_lr,test_data$target,dnn = c('predicted','true'))
accu_test=(sum((test.pred_lr==test_data$target)/length(test_data$target)))*100
accu_test


##########RF
library(randomForest)
rf.boston =randomForest(target~.,data=train_data ,mtry=5, importance =TRUE)
#yhat.rf = predict (rf.boston ,newdata =Boston [-train ,])
#mean(( yhat.rf -boston .test)^2)

#Training accuracy.
train.prob =predict (rf.boston,type ="response")
train.pred=rep("0",length(train_data$target))
train.pred[train.prob>0.5]="1"
table(train.pred,train_data$target,dnn = c('predicted','true'))
accu_train=(sum((train.pred==train_data$target)/length(train_data$target)))*100
accu_train
?predict
#Testing accuracy.
test.prob =predict(rf.boston,test_data,type ="response")
test.pred_rf=rep("0",length(test_data$target))
test.pred_rf[test.prob>0.5]="1"
table(test.pred_rf,test_data$target,dnn = c('predicted','true'))
accu_test=(sum((test.pred_rf==test_data$target)/length(test_data$target)))*100
accu_test
