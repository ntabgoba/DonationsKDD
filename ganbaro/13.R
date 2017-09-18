#PROMOTION_13
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
d13 <- dk 
d13$RAMNT_13 <- j$RAMNT_13
d13$ADATE_13 <- j$ADATE_13
#Encode donations into p&np,
d13$ra13 <- as.factor(ifelse(is.na(d13$RAMNT_13),"np","p"))
# Encode profit/loss 36973 (0 profit),40701 (-0.68 profit), 17738 (donated)
d13$profit_13 <- ifelse(is.na(d13$RAMNT_13),ifelse(is.na(d13$ADATE_13),0,-0.68),d13$RAMNT_13 - 0.68)
#Encode mailing into m$nm
d13$ad13 <- as.factor(ifelse(is.na(d13$ADATE_13),"nm","m"))
#Encode RAMNT_13 and ADATE_13 NAs as 0
d13$RAMNT_13 <- ifelse(is.na(d13$RAMNT_13),0,d13$RAMNT_13)
d13$ADATE_13 <- ifelse(is.na(d13$ADATE_13),0,d13$ADATE_13)
#OMIT NAs
dtt <- na.omit(d13)  #95,412x325 -> 60,805x325
dtt <- dtt[dtt$ad13 == "m",] # 37986*325, and no nm level
dtt$INCOME <- as.factor(dtt$INCOME)
#clear workspace, but d13
#rm(list=setdiff(ls(), "dtt"))

#PARTITION
set.seed(1986)
smpl <- sample.int(nrow(dtt),size = floor(0.7*nrow(dtt)),replace=FALSE)
tr13 <- dtt[smpl,-c(25,26,28,29)]
te13 <- dtt[-smpl,-c(25,26,27,28,29)]
bag13=randomForest(ra13~.,data = tr13,importance=TRUE)
yhat13 = predict(bag13,newdata=te13)
# summary(yhat13) and summary(dtt[-smpl,-c(28,29,31,32)]$ra13)
#predict on entire dataset
dtt$pr13 <- predict(bag13, newdata=dtt[,-c(25,26,27,28,29)])

#REGRESSION
rgr13 <- dtt[dtt$pr13 == "p",] 
smpl13 <- sample.int(nrow(rgr13),size = floor(0.7*nrow(rgr13)),replace=FALSE)
fit13 <- lm(RAMNT_13 ~.,data=rgr13[smpl13,-c(26:30)])
pr13 <- predict.lm(fit13,newdata = rgr13[-smpl13,-c(25:30)])
#predict on entire dataset
rgr13$pra13 <- predict(fit13,newdata = rgr13[,-c(25:30)])
write.csv(rgr13,file="results/rgr13.csv",row.names = FALSE)
rm(list=setdiff(ls(), "j"))
