#PROMOTION_21
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
dk <- dk[,-c(1:6,20:305,306,318)]
#REPEATIVE WORK
#Add mail date and amount donated for Promotion 1
d21 <- dk 
d21$RAMNT_21 <- j$RAMNT_21
d21$ADATE_21 <- j$ADATE_21
#Encode donations into p&np,
d21$ra21 <- as.factor(ifelse(is.na(d21$RAMNT_21),"np","p"))
# Encode profit/loss 36973 (0 profit),40701 (-0.68 profit), 17738 (donated)
d21$profit_21 <- ifelse(is.na(d21$RAMNT_21),ifelse(is.na(d21$ADATE_21),0,-0.68),d21$RAMNT_21 - 0.68)
#Encode mailing into m$nm
d21$ad21 <- as.factor(ifelse(is.na(d21$ADATE_21),"nm","m"))
#Encode RAMNT_21 and ADATE_21 NAs as 0
d21$RAMNT_21 <- ifelse(is.na(d21$RAMNT_21),0,d21$RAMNT_21)
d21$ADATE_21 <- ifelse(is.na(d21$ADATE_21),0,d21$ADATE_21)
#OMIT NAs
dtt <- na.omit(d21)  #95,412x325 -> 60,805x325
dtt <- dtt[dtt$ad21 == "m",] # 37986*325, and no nm level
dtt$INCOME <- as.factor(dtt$INCOME)
#clear workspace, but d21
#rm(list=setdiff(ls(), "dtt"))

#PARTITION
set.seed(1986)
smpl <- sample.int(nrow(dtt),size = floor(0.7*nrow(dtt)),replace=FALSE)
tr21 <- dtt[smpl,-c(27,28,30,31)]
te21 <- dtt[-smpl,-c(27,28,29,30,31)]
bag21=randomForest(ra21~.,data = tr21,importance=TRUE)
yhat21 = predict(bag21,newdata=te21)
# summary(yhat21) and summary(dtt[-smpl,-c(28,29,31,32)]$ra21)
#predict on entire dataset
dtt$pr21 <- predict(bag21, newdata=dtt[,-c(27,28,29,30,31)])

#REGRESSION
rgr21 <- dtt[dtt$pr21 == "p",] 
smpl21 <- sample.int(nrow(rgr21),size = floor(0.7*nrow(rgr21)),replace=FALSE)
fit21 <- lm(RAMNT_21 ~.,data=rgr21[smpl21,-c(28:32)])
pr21 <- predict.lm(fit21,newdata = rgr21[-smpl21,-c(27:32)])
#predict on entire dataset
rgr21$pra21 <- predict(fit21,newdata = rgr21[,-c(27:32)])
write.csv(rgr21,file="results/rgr21.csv",row.names = FALSE)
rm(list=setdiff(ls(), "j"))
