#读数据
ABI5 <- read.table("./Classification/ABI5_pos_neg_kmer_shuf.txt",sep= "\t")

#标准化并分训练测试集
library(plyr)
knn.dataset <- cbind(colwise(scale)(ABI5[,-197]),V197 = as.factor(ABI5$V197))
set.seed(123)
n <- dim(knn.dataset)[1]
index <- sample(n,round(0.7*n))
train.knn <- knn.dataset[index,]
test.knn <- knn.dataset[-index,]

library(caret)
library(e1071)
#KNN
###利用交叉验证方法选最优K
knnTune <- train(x = train.knn[,-197], y = train.knn[,197], method = "knn",tuneGrid = data.frame(.k = 1:65), trControl = fitControl)
plot(knnTune,pch = 2)

library(class)
 
#建模
start = Sys.time()
knn_test_pred <- knn(train = train.knn[,-197], test =test.knn[,-197],cl = train.knn[,197], k=56)
end = Sys.time()
print("训练KNN模型花费时间为：")
end -start

#评估模型性能
# 加载 "gmodels" 包
library(gmodels)
 
# 交叉表分析
CrossTable(x = test.knn[,197], y = knn_test_pred,prop.chisq=FALSE)
#准确率
acc <- mean( knn_test_pred == test.knn[,197])

#AUC

library(pROC)
roc<-multiclass.roc (as.ordered(test.knn$V197) ,as.ordered(knn_test_pred))
roc

########SVM
#首先调用tune.svm调整支持向量机：
tuned<-tune.svm(V197~.,data = train.knn,gamma = 10^(-6:-1),cost = 10^(1:2))
##得到最优gramma，cost
summary( tuned)

###训练、测试模型
start = Sys.time()
svmfit <- svm (V197~., data=train.knn, kernel = "radial", cost = 100, gamma=0.00001, scale = FALSE)
svm_test_pred <- predict(svmfit, test.knn[,-197])
end = Sys.time()

end-start
#画图
plot(predict(svmfit), train.knn$V197)



###评估
# 交叉表分析
CrossTable(x = test.knn[,197], y = svm_test_pred,prop.chisq=FALSE)

#acc
acc <- mean( svm_test_pred == test.knn[,197])

#AUC
library(pROC)

roc<-multiclass.roc (as.ordered(test.knn$V197) ,as.ordered(SVM_test_pred))
roc

##############RF

library(randomForest)

#寻找最优mtry
n<-length(names(train.knn))
set.seed(9)
min=100
num=0
for (i in 1:(n-1)){
mtry_fit<- randomForest(V197~.,data = train.knn,mtry=i)
err<-mean(mtry_fit$err.rate)
print(err)
print(i)
if(err<min) {    
min =err     
num=i }
}

print(min)
print(num)

#num即为mtry个数
#mtry = 3
set.seed(100)
#带入mtry，尝试寻找ntree
ntree_fit<-randomForest(V197~.,data = train.knn,mtry=3,ntree=5000)
ntree_fit

###模型训练和预测
start = Sys.time()
train_rf <- randomForest(x=train.knn[,-197], y=train.knn[,197],mtry=3, ntree=1800,important=TRUE,proximity=TRUE)#训练
test_rf_pred <- predict(train_rf, test.knn[,-197])#预测

end = Sys.time()
print("训练+预测RF模型花费时间为：")
end -start

# 交叉表分析
CrossTable(x = test.knn[,197], y = test_rf_pred,prop.chisq=FALSE)

#acc
acc <- mean( test_rf_pred == test.knn[,197])

#AUC
library(pROC)

roc<-multiclass.roc (as.ordered(test.knn$V197) ,as.ordered(test_rf_pred))
roc

#####logistic regression
start = Sys.time()

train_glm<-glm(V197~V1+V2+V3+V4+V5+V6+V7+V8+V9+V10+V11+V12+V13+V14+V15+V16+V17+V18+V19+V20+V21+V22+V23+V24+V25+V26+V27+V28+V29+V30+V31+V32+V33+V34+V35+V36+V37+V38+V39+V40+V41+V42+V43+V44+V45+V46+V47+V48+V49+V50+V51+V52+V53+V54+V55+V56+V57+V58+V59+V60+V61+V62+V63+V64+V65+V66+V67+V68+V69+V70+V71+V72+V73+V74+V75+V76+V77+V78+V79+V80+V81+V82+V83+V84+V85+V86+V87+V88+V89+V90+V91+V92+V93+V94+V95+V96+V97+V98+V99+V100+V101+V102+V103+V104+V105+V106+V107+V108+V109+V110+V111+V112+V113+V114+V115+V116+V117+V118+V119+V120+V121+V122+V123+V124+V125+V126+V127+V128+V129+V130+V131+V132+V133+V134+V135+V136+V137+V138+V139+V140+V141+V142+V143+V144+V145+V146+V147+V148+V149+V150+V151+V152+V153+V154+V155+V156+V157+V158+V159+V160+V161+V162+V163+V164+V165+V166+V167+V168+V169+V170+V171+V172+V173+V174+V175+V176+V177+V178+V179+V180+V181+V182+V183+V184+V185+V186+V187+V188+V189+V190+V191+V192+V193+V194+V195+V196,family = binomial(link = "logit"),data = train.knn)

glm_test_pred <- predict(train_glm, test.knn[,-197])
end = Sys.time()
print("训练+预测SVM模型花费时间为：")
end -start

# 交叉表分析
CrossTable(x = test.knn[,197], y = y.test.pred.glm,prop.chisq=FALSE)

#acc
acc <- mean(y.test.pred.glm == test.knn[,197])

#AUC
library(pROC)
roc<-multiclass.roc (as.ordered(test.knn$V197) ,as.ordered(y.test.pred.glm,))
roc

























