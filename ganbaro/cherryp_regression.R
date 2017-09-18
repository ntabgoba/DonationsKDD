#cherry pick for Linear Regression
library(dplyr)
library(glmnet)
j <- read.csv("val.csv",na.strings=c(""," ","NA"))
dl_remov <- j %>% select(matches("ADATE_|RAMNT_|DOB|DATE|RFA|ZIP|TIMELAG|TARGET_B|TARGET_D|HPHONE_D|CONTROLN|X|AGEFLAG"))
dl <- j[,!(colnames(j) %in% colnames(dl_remov))]
#fix levels
levels(dl$GENDER)[match("A",levels(dl$GENDER))] <- NA
levels(dl$GENDER)[match("C",levels(dl$GENDER))] <- NA
levels(dl$GENDER)[match("U",levels(dl$GENDER))] <- NA

#lets assume that NAs of Homeownr, means not owner
dl$HOMEOWNR <- as.character(dl$HOMEOWNR)
dl$HOMEOWNR[is.na(dl$HOMEOWNR)] <- "U"
dl$HOMEOWNR <- as.factor(dl$HOMEOWNR)

#keep variables with less than 1/3 NAs
dl_na <- sapply(dl,function(x) sum(is.na(x)))
dl_kp <- dl_na[dl_na < ((1/3)*dim(dl)[1])]

#keep dl with fewer NAs, i.e AGE has max NA 23k/95k
dk <- dl[,(colnames(dl) %in% names(dl_kp))]

#check categorical variables and their number of levels
dk_cat <- dk[,sapply(dk,is.factor)] %>% sapply(function(x) length(levels(x)))

#REPEATIVE WORK
#Add mail date and amount donated for Promotion 1
d24 <- dk 
d24$RAMNT_24 <- j$RAMNT_24
d24$ADATE_24 <- j$ADATE_24
#Encode donations into p&np,
d24$ra24 <- as.factor(ifelse(is.na(d24$RAMNT_24),"np","p"))
# Encode profit/loss 36973 (0 profit),40701 (-0.68 profit), 17738 (donated)
d24$profit_24 <- ifelse(is.na(d24$RAMNT_24),ifelse(is.na(d24$ADATE_24),0,-0.68),d24$RAMNT_24 - 0.68)
#Encode mailing into m$nm
d24$ad24 <- as.factor(ifelse(is.na(d24$ADATE_24),"nm","m"))
#Encode RAMNT_24 and ADATE_24 NAs as 0
d24$RAMNT_24 <- ifelse(is.na(d24$RAMNT_24),0,d24$RAMNT_24)
d24$ADATE_24 <- ifelse(is.na(d24$ADATE_24),0,d24$ADATE_24)

#OMIT NAs
dtt <- na.omit(d24)  #95,412x325 -> 60,805x325
dtt <- dtt[dtt$ad24 == "m",] # 37986*325, and no nm level
#clear workspace, but d24
dtt1 <- dtt[,-c(1:6,8,20:305)]
rm(list=setdiff(ls(), "dtt1"))
dtt1$INCOME <- as.factor(dtt1$INCOME)

#PARTITION
set.seed(1986)
#sample 10k observations for computation simplicity
#dtt <- dtt[sample(nrow(dtt), 10000), ]
smpl <- sample.int(nrow(dtt1),size = floor(0.7*nrow(dtt1)),replace=FALSE)
tr24 <- dtt1[smpl,]
te24 <- dtt1[-smpl,]

#------------------------------ Skip
#LINEAR REGRESSION
f24 <- lm(RAMNT_24~.,data = tr24[,-c(29:32)])
#keep coef with p-val < 0.05, aka have a *,**,*** ...
data.frame(summary(f24)$coef[summary(f24)$coef[,4] <= .05, 4])

#1.0 LINEAR REGRESSION with Glmnet
x24 <- model.matrix(RAMNT_24~.,data = tr24[,-c(322:324)])
y24 <- tr24$RAMNT_24
f24g <- glmnet(x=x24,y=y24)

cvfit24 = cv.glmnet(x=x24,y=y24, type.measure = "mse", nfolds = 10)
cvfit24_coef <- coef(cvfit24,s="lambda.min")

#PREDICT
xte24 <- model.matrix(RAMNT_24~.,data=te24[,-c(322:324)])
#te_pr_positive <- merge(x=tepositive,y=as.data.frame(prpositive),by="row.names")
#true positives, actual:$37878, predicted:$13063
#------------------------------ Above is skipped due its to poor perfomance

#2.0 LOGISTIC REGRESSION
xl24 <- model.matrix(ra24~.,tr24[,-c(28,29,31,32)])
yl24 <- tr24$ra24
system.time(fitl24 <- glmnet(x=xl24,y=yl24,family = "binomial"))
# plot(fitl24,xvar="dev")
#predict
xtel24 <- model.matrix(ra24~.,te24[,-c(28,29,31,32)])
prl24 <- predict(fitl24, newx = xtel24, type = "class",s=c(0.05,0.01))
xte_pr_l24 <- merge(x=te24[,c(322:325)],y=as.data.frame(prl24),by="row.names")
xte_pr_l24_pos <- xte_pr_l24[((xte_pr_l24$ra24 =="p") & (xte_pr_l24$`1`=="p")),]
#results
#true positive predicted:36, actual:566, from 3000 sample 

#cross validate
cvfitl24 = cv.glmnet(x=xl24, y=yl24, family="binomial", type.measure = "class", nfolds = 10)

cvl24_coef <- coef(cvfitl24,s="lambda.min")

cvl24_pr <- predict(cvfitl24, newx = xtel24, s = "lambda.min", type = "class")
#evaluate
xte_cvpr_l24 <- merge(x=te24[,c(322:325)],y=as.data.frame(cvl24_pr),by="row.names")
xte_cvpr_l24_pos <- xte_cvpr_l24[((xte_cvpr_l24$ra24 =="p") & (xte_cvpr_l24$`1`=="p")),]
#results
#true positive predicted:1062, actual:3388, from 3000 sample 

#OUT OF PREDICTED TO DONATE, HOW MUCH WILL HE/SHE DONATE?

#ASSUMPTIONS
#--Use logistic model 'cvfitl24' to classify,dtt
#--Cut out predicted true positives and finaly compare with dtt

xdtt <- model.matrix(ra24~.,dtt[,-c(321,322,324,325)])
dtt_pr <- predict(cvfitl24, newx = xdtt, s = "lambda.min", type = "class")
dtt_p <- as.data.frame(dtt_pr[dtt_pr=="p",])
colnames(dtt_p) <- "ra24_pr"
#merge dtt and dtt_pr, to keep positive predicted donations only (dim 646x1)
dtt_pr_l24 <- merge(x=dtt,y=dtt_p,by="row.names")

#Remaining actual Zero donations in non zero predicted donations
#length(dtt_pr_l24$RAMNT_24[dtt_pr_l24$RAMNT_24 == 0])  =213

#LINEAR REGRESSION, how much she/he will donate, given we predicted will donate

#sample
set.seed(6891)
smpld <- sample.int(nrow(dtt_pr_l24),size = floor(0.8*nrow(dtt_pr_l24)),replace=FALSE)
trd24 <- dtt_pr_l24[smpld,]
ted24 <- dtt_pr_l24[-smpld,]
#glmnet linear regression
xtrd24 <- model.matrix(RAMNT_24~.,data=trd24[,-c(323:326)])
ytrd24 <- trd24$RAMNT_24
ftdtt24 <- glmnet(x=xtrd24,y=ytrd24)

cvftdtt24 = cv.glmnet(x=xtrd24,y=ytrd24, type.measure = "mse", nfolds = 10)
cvftdtt24_coef <- coef(cvftdtt24,s="lambda.min")

#PREDICT
xted24 <- model.matrix(RAMNT_24~.,data=ted24[,-c(323:327)])
xte24_pr <- predict(cvftdtt24,newx = xted24,s="lambda.min")
#plot predicted and actual
xte24_pr_df <- merge(x=ted24,y=as.data.frame(xte24_pr),by="row.names")
names(xte24_pr_df$`1`) <- c("ra24_am") 
plot(xte24_pr_df$ra24_am,xte24_pr_df$RAMNT_24)

rf_feats <- c(2,8,10:15,301)

