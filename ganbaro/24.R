#PROMOTION_24
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
dtt$INCOME <- as.factor(dtt$INCOME)
#clear workspace, but d24
#rm(list=setdiff(ls(), "dtt"))

#PARTITION
set.seed(1986)
smpl <- sample.int(nrow(dtt),size = floor(0.7*nrow(dtt)),replace=FALSE)
tr24 <- dtt[smpl,-c(28,29,31,32)]
te24 <- dtt[-smpl,-c(28,29,30,31,32)]
bag24=randomForest(ra24~.,data = tr24,importance=TRUE)
yhat24 = predict(bag24,newdata=te24)
# summary(yhat24) and summary(dtt[-smpl,-c(28,29,31,32)]$ra24)
#predict on entire dataset
dtt$pr24 <- predict(bag24, newdata=dtt[,-c(28,29,30,31,32)])

#REGRESSION
rgr24 <- dtt[dtt$pr24 == "p",] 
smpl24 <- sample.int(nrow(rgr24),size = floor(0.7*nrow(rgr24)),replace=FALSE)
fit24 <- lm(RAMNT_24 ~.,data=rgr24[smpl24,-c(29:33)])
pr24 <- predict(fit24,newdata = rgr24[-smpl24,-c(28:33)])
#predict on entire dataset
rgr24$pra24 <- predict(fit24,newdata = rgr24[,-c(28:33)])
write.csv(rgr24,file="results/rgr24.csv",row.names = FALSE)

rm(list=setdiff(ls(), "j"))
