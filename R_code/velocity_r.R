data=read.csv("C:\\Users\\Mrinal\\Desktop\\minor porject\\data\\velocity.csv")
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

##########SVM
library (e1071)

tune.out.l=tune(svm , target~., data=train_data, kernel ="polynomial",ranges =list(cost=c(0.1 ,1 ,10 ,100 ,1000),degree=c(1,2,3,4,5) ))
summary (tune.out.l)
table(true=test_data[,"target"], pred=predict(tune.out.r$best.model,newdata =test_data))
bestmod_l =tune.out.l$best.model
bestmod_l


#Training accuracy.
train.prob=predict (bestmod_l,type ="response")
train.pred=rep("0",length(train_data$target))
train.pred[train.prob>0.5]="1"
table(train.pred,train_data$target,dnn = c('predicted','true'))
accu_train=(sum((train.pred==train_data$target)/length(train_data$target)))*100
accu_train
?predict
#Testing accuracy.
test.prob =predict(bestmod_l,test_data,type ="response")
test.pred_svm=rep("0",length(test_data$target))
test.pred_svm[test.prob>0.5]="1"
table(test.pred_svm,test_data$target,dnn = c('predicted','true'))
accu_test=(sum((test.pred_svm==test_data$target)/length(test_data$target)))*100
accu_test

data_velocity=cbind(test_data,test.pred_lr,test.pred_rf,test.pred_svm)
View(data_velocity)
write.csv(data_velocity,"data_velocity.csv")

library(VennDiagram)
# Taking a subset of the data of predicted labels
tempdf = data_velocity[,c(1,22,23,24)]
# Adding a new column for intermediate use
tempdf$names = rownames(tempdf)
# This step is required for drawing venn diagram. Here we are creating a
# unique row value composed of rowname and the predicted output. This will 
# help in drawing venn diagramm. The venn diagram counts how many items 
# are present in both set A and Set b. 
tempdf$test.pred_lr_new = paste0(tempdf$names, "_", tempdf$test.pred_lr)
tempdf$test.pred_rf_new = paste0(tempdf$names, "_", tempdf$test.pred_rf)
tempdf$test.pred_svm_new = paste0(tempdf$names, "_", tempdf$test.pred_svm)

# This code generates black and white venn diagram
# NOTE - Change the file location to get the output. 
venn.diagram(x = list(pred_lr = tempdf_deffective[, "test.pred_lr_new"], pred_rf = tempdf_deffective[,"test.pred_rf_new"],pred_svm = tempdf_deffective[, "test.pred_svm_new"]), filename = "velocity_b&w_deffective.png")

# This plots a colorful venn diagram, however the labels are not placed
# in correct location.
venn.diagram(x = list(pred_lr = tempdf_deffective[, "test.pred_lr_new"],pred_rf = tempdf_deffective[, "test.pred_rf_new"],pred_svm = tempdf_deffective[, "test.pred_svm_new"]), filename = "velocity_coloured_deffective.png",col = "transparent", fill = c("cornflowerblue","green","yellow"),alpha = 0.50, label.col = c("orange", "white", "darkorchid4", "white", "white","darkgreen", "white"),cex = 1.5, fontfamily = "serif", fontface = "bold",cat.col = c("darkblue", "darkgreen", "orange"), cat.cex = 1.5,cat.pos = 0, cat.dist = 0.07, cat.fontfamily = "serif", rotation.degree = 270,margin = 0.2)

tempdf_correct=tempdf[which(tempdf$target==0),]
# This code generates black and white venn diagram
# NOTE - Change the file location to get the output. 
venn.diagram(x = list(pred_lr = tempdf_correct[, "test.pred_lr_new"], pred_rf = tempdf_correct[,"test.pred_rf_new"],pred_svm = tempdf_correct[, "test.pred_svm_new"]), filename = "velocity_b&w_correct.png")

# This plots a colorful venn diagram, however the labels are not placed
# in correct location.
venn.diagram(x = list(pred_lr = tempdf_correct[, "test.pred_lr_new"],pred_rf = tempdf_correct[, "test.pred_rf_new"],pred_svm = tempdf_correct[, "test.pred_svm_new"]), filename = "velocity_coloured_correct.png",col = "transparent", fill = c("cornflowerblue","green","yellow"),alpha = 0.50, label.col = c("orange", "white", "darkorchid4", "white", "white","darkgreen", "white"),cex = 1.5, fontfamily = "serif", fontface = "bold",cat.col = c("darkblue", "darkgreen", "orange"), cat.cex = 1.5,cat.pos = 0, cat.dist = 0.07, cat.fontfamily = "serif", rotation.degree = 270,margin = 0.2)

