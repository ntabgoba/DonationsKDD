#PROMOTION_23
library(dplyr)
library(glmnet)
library(randomForest)
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
dk <- dk[,-c(1:6,20:305,318)]
#REPEATIVE WORK
#Add mail date and amount donated for Promotion 1
d23 <- dk 
d23$RAMNT_23 <- j$RAMNT_23
d23$ADATE_23 <- j$ADATE_23
#Encode donations into p&np,
d23$ra23 <- as.factor(ifelse(is.na(d23$RAMNT_23),"np","p"))
# Encode profit/loss 36973 (0 profit),40701 (-0.68 profit), 17738 (donated)
d23$profit_23 <- ifelse(is.na(d23$RAMNT_23),ifelse(is.na(d23$ADATE_23),0,-0.68),d23$RAMNT_23 - 0.68)
#Encode mailing into m$nm
d23$ad23 <- as.factor(ifelse(is.na(d23$ADATE_23),"nm","m"))
#Encode RAMNT_23 and ADATE_23 NAs as 0
d23$RAMNT_23 <- ifelse(is.na(d23$RAMNT_23),0,d23$RAMNT_23)
d23$ADATE_23 <- ifelse(is.na(d23$ADATE_23),0,d23$ADATE_23)
#OMIT NAs
dtt <- na.omit(d23)  #95,412x325 -> 60,805x325
dtt <- dtt[dtt$ad23 == "m",] # 37986*325, and no nm level
dtt$INCOME <- as.factor(dtt$INCOME)
#clear workspace, but d23
#rm(list=setdiff(ls(), "dtt"))

#PARTITION
set.seed(1986)
smpl <- sample.int(nrow(dtt),size = floor(0.7*nrow(dtt)),replace=FALSE)
tr23 <- dtt[smpl,-c(28,29,31,32)]
te23 <- dtt[-smpl,-c(28,29,30,31,32)]
bag23=randomForest(ra23~.,data = tr23,importance=TRUE)
yhat23 = predict(bag23,newdata=te23)
# summary(yhat23) and summary(dtt[-smpl,-c(28,29,31,32)]$ra23)
#predict on entire dataset
dtt$pr23 <- predict(bag23, newdata=dtt[,-c(28,29,30,31,32)])

#REGRESSION
rgr23 <- dtt[dtt$pr23 == "p",] 
smpl23 <- sample.int(nrow(rgr23),size = floor(0.7*nrow(rgr23)),replace=FALSE)
fit23 <- lm(RAMNT_23 ~.,data=rgr23[smpl23,-c(29:33)])
pr23 <- predict(fit23,newdata = rgr23[-smpl23,-c(28:33)])
#predict on entire dataset
rgr23$pra23 <- predict(fit23,newdata = rgr23[,-c(28:33)])
write.csv(rgr23,file="results/rgr23.csv",row.names = FALSE)

rm(list=setdiff(ls(), "j"))
