z#PROMOTION_22
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
d22 <- dk 
d22$RAMNT_22 <- j$RAMNT_22
d22$ADATE_22 <- j$ADATE_22
#Encode donations into p&np,
d22$ra22 <- as.factor(ifelse(is.na(d22$RAMNT_22),"np","p"))
# Encode profit/loss 36973 (0 profit),40701 (-0.68 profit), 17738 (donated)
d22$profit_22 <- ifelse(is.na(d22$RAMNT_22),ifelse(is.na(d22$ADATE_22),0,-0.68),d22$RAMNT_22 - 0.68)
#Encode mailing into m$nm
d22$ad22 <- as.factor(ifelse(is.na(d22$ADATE_22),"nm","m"))
#Encode RAMNT_22 and ADATE_22 NAs as 0
d22$RAMNT_22 <- ifelse(is.na(d22$RAMNT_22),0,d22$RAMNT_22)
d22$ADATE_22 <- ifelse(is.na(d22$ADATE_22),0,d22$ADATE_22)
#OMIT NAs
dtt <- na.omit(d22)  #95,412x325 -> 60,805x325
dtt <- dtt[dtt$ad22 == "m",] # 37986*325, and no nm level
dtt$INCOME <- as.factor(dtt$INCOME)
#clear workspace, but d22
#rm(list=setdiff(ls(), "dtt"))

#PARTITION
set.seed(1986)
smpl <- sample.int(nrow(dtt),size = floor(0.7*nrow(dtt)),replace=FALSE)
tr22 <- dtt[smpl,-c(28,29,31,32)]
te22 <- dtt[-smpl,-c(28,29,30,31,32)]
bag22=randomForest(ra22~.,data = tr22,importance=TRUE)
yhat22 = predict(bag22,newdata=te22)
# summary(yhat22) and summary(dtt[-smpl,-c(28,29,31,32)]$ra22)
#predict on entire dataset
dtt$pr22 <- predict(bag22, newdata=dtt[,-c(28,29,30,31,32)])

#REGRESSION
rgr22 <- dtt[dtt$pr22 == "p",] 
smpl22 <- sample.int(nrow(rgr22),size = floor(0.7*nrow(rgr22)),replace=FALSE)
fit22 <- lm(RAMNT_22 ~.,data=rgr22[smpl22,-c(29:33)])
pr22 <- predict(fit22,newdata = rgr22[-smpl22,-c(28:33)])
#predict on entire dataset
rgr22$pra22 <- predict(fit22,newdata = rgr22[,-c(28:33)])
write.csv(rgr22,file="results/rgr22.csv",row.names = FALSE)
rm(list=setdiff(ls(), "j"))