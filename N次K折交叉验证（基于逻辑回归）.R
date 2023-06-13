library(caret)#做交叉验证
library(pROC)

rm(list = ls()) 

aa<- read.csv('交叉验证示例.csv')
str(aa)#查看变量性质

#批量数值转因子
for (i in names(aa)[c(4:9)]){aa[,i] <- as.factor(aa[,i])}
#再次检查变量性质
str(aa)
#设置随机种子，使数据分割可重复
set.seed(1)


#多次K折交叉验证
folds <-createMultiFolds(y=aa$status,k=5,times=400)

train <- aa[folds[[1]],]#取fold 1数据，建立测试集和验证集
#str(train)
test <- aa[-folds[[1]],]
#str(test)

#建模
model<-glm(status~age+n+hr+lvi+g+rt,
              family = binomial(link = logit), 
              data=train )#summary(model)
#预测
model_pre<-predict(model,
                   type='response',
                   newdata=test)##生成预测值
#auc值
roc1<-roc((test$status),model_pre)
round(auc(roc1),3)##AUC
round(ci(roc1),3)##95%CI

#画图
plot(roc1, 
     print.auc=T, 
     auc.polygon=T, 
     auc.polygon.col="skyblue",
     grid=c(0.1, 0.2),
     grid.col=c("green", "red"), 
     max.auc.polygon=T,
     print.thres=T)
#--------------------------------
#做成循环
auc_value<-as.numeric()###建立空值
for(i in 1:2000){
  train<- aa[ folds[[i]],] 
  test <- aa[-folds[[i]],] 
  model<- glm(status~age+n+hr+lvi+g+rt,family=binomial(link=logit),data=train)
  model_pre<-predict(model,type='response', newdata=test)
  auc_value<- append(auc_value,as.numeric(auc(as.numeric(test[,1]),model_pre)))
}
summary(auc_value)
mean(auc_value)

#不做交叉验证================================================================
model1<-glm(status~age+n+hr+lvi+g+rt,
            family = binomial(link = logit),
            data=aa)
model_pre1<-predict(model1,type='response')
roc2<-roc((aa$status),model_pre1);auc(roc2)


#10折200次交叉验证============================================================
set.seed(1)
folds <-createMultiFolds(y=aa$status,k=10,times=200)
#建训练集和验证集
train <- aa[folds[[1]],]
test <- aa[-folds[[1]],]
#2000次批量训练与验证
#做成循环
auc_value<-as.numeric()
for(i in 1:2000){
  train<- aa[ folds[[i]],] 
  test <- aa[-folds[[i]],] 
  model<- glm(status~age+n+hr+lvi+g+rt,family=binomial(link=logit),data=train)
  model_pre<-predict(model,type='response', newdata=test)
  auc_value<- append(auc_value,as.numeric(auc(as.numeric(test[,1]),model_pre)))
}
mean(auc_value)


#单纯10折交叉验证===============================================================
set.seed(1)
folds <-createMultiFolds(y=aa$status,k=10,times=1)
#建训练集和验证集
train <- aa[folds[[1]],]
test <- aa[-folds[[1]],]

#10次批量训练与验证
auc_value<-as.numeric()
for(i in 1:10){
  train<- aa[ folds[[i]],] 
  test <- aa[-folds[[i]],] 
  model<- glm(status~age+n+hr+lvi+g+rt,
              family=binomial(link=logit),data=train)
  model_pre<-predict(model,type='response', newdata=test)
  auc_value<- append(auc_value,
                     as.numeric(auc(as.numeric(test[,1]),model_pre)))
}
mean(auc_value)






