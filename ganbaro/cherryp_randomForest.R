library(randomForest)
set.seed(1)
train <- tr24[,-c(28,29,31,32)]
test <- te24[,-c(28,29,30,31,32)]
bag.tr=randomForest(ra24~.,data = train,importance=TRUE)
yhat.bag = predict(bag.tr,newdata=test)
plot(yhat.bag, te24$ra24)
abline(0,1)
mean((yhat.bag-te24$ra24)^2)
importance(bag.tr)
varImpPlot(bag.tr)

#======
oob.err=double(13)
test.err=double(13)
for(mtry in 1:13){
        fit=randomForest(ra24~.,data = train,mtry=mtry,ntree=400)
        oob.err[mtry]=fit$mse[400]
        pred=predict(fit,newdata=test)
        test.err[mtry]=c(pred,test$te24$ra24)
}
matplot(1:mtry,cbind(test.err,oob.err),pch=19,col=c("red","blue"),type="b",ylab="Mean Squared Error")
legend("topright",legend=c("OOB","Test"),pch=19,col=c("red","blue"))

