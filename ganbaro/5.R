#PROMOTION_5
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
dk <- dk[,-c(1:6,20:305,306,316,317,318)]
#REPEATIVE WORK
#Add mail date and amount donated for Promotion 1
d5 <- dk 
d5$RAMNT_5 <- j$RAMNT_5
d5$ADATE_5 <- j$ADATE_5
#Encode donations into p&np,
d5$ra5 <- as.factor(ifelse(is.na(d5$RAMNT_5),"np","p"))
# Encode profit/loss 36973 (0 profit),40701 (-0.68 profit), 17738 (donated)
d5$profit_5 <- ifelse(is.na(d5$RAMNT_5),ifelse(is.na(d5$ADATE_5),0,-0.68),d5$RAMNT_5 - 0.68)
#Encode mailing into m$nm
d5$ad5 <- as.factor(ifelse(is.na(d5$ADATE_5),"nm","m"))
#Encode RAMNT_5 and ADATE_5 NAs as 0
d5$RAMNT_5 <- ifelse(is.na(d5$RAMNT_5),0,d5$RAMNT_5)
d5$ADATE_5 <- ifelse(is.na(d5$ADATE_5),0,d5$ADATE_5)
#OMIT NAs
dtt <- na.omit(d5)  #95,412x325 -> 60,805x325
dtt <- dtt[dtt$ad5 == "m",] # 37986*325, and no nm level
dtt$INCOME <- as.factor(dtt$INCOME)
#clear workspace, but d5
#rm(list=setdiff(ls(), "dtt"))

#PARTITION
set.seed(1986)
smpl <- sample.int(nrow(dtt),size = floor(0.7*nrow(dtt)),replace=FALSE)
tr5 <- dtt[smpl,-c(25,26,28,29)]
te5 <- dtt[-smpl,-c(25,26,27,28,29)]
bag5=randomForest(ra5~.,data = tr5,importance=TRUE)
yhat5 = predict(bag5,newdata=te5)
# summary(yhat5) and summary(dtt[-smpl,-c(28,29,31,32)]$ra5)
#predict on entire dataset
dtt$pr5 <- predict(bag5, newdata=dtt[,-c(25,26,27,28,29)])

#REGRESSION
rgr5 <- dtt[dtt$pr5 == "p",] 
smpl5 <- sample.int(nrow(rgr5),size = floor(0.7*nrow(rgr5)),replace=FALSE)
fit5 <- lm(RAMNT_5 ~.,data=rgr5[smpl5,-c(26:30)])
pr5 <- predict.lm(fit5,newdata = rgr5[-smpl5,-c(25:30)])
#predict on entire dataset
rgr5$pra5 <- predict(fit5,newdata = rgr5[,-c(25:30)])
write.csv(rgr5,file="results/rgr5.csv",row.names = FALSE)
rm(list=setdiff(ls(), "j"))
