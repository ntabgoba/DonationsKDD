#PROMOTION_7
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
d7 <- dk 
d7$RAMNT_7 <- j$RAMNT_7
d7$ADATE_7 <- j$ADATE_7
#Encode donations into p&np,
d7$ra7 <- as.factor(ifelse(is.na(d7$RAMNT_7),"np","p"))
# Encode profit/loss 36973 (0 profit),40701 (-0.68 profit), 17738 (donated)
d7$profit_7 <- ifelse(is.na(d7$RAMNT_7),ifelse(is.na(d7$ADATE_7),0,-0.68),d7$RAMNT_7 - 0.68)
#Encode mailing into m$nm
d7$ad7 <- as.factor(ifelse(is.na(d7$ADATE_7),"nm","m"))
#Encode RAMNT_7 and ADATE_7 NAs as 0
d7$RAMNT_7 <- ifelse(is.na(d7$RAMNT_7),0,d7$RAMNT_7)
d7$ADATE_7 <- ifelse(is.na(d7$ADATE_7),0,d7$ADATE_7)
#OMIT NAs
dtt <- na.omit(d7)  #95,47x325 -> 60,805x325
dtt <- dtt[dtt$ad7 == "m",] # 37986*325, and no nm level
dtt$INCOME <- as.factor(dtt$INCOME)
#clear workspace, but d7
#rm(list=setdiff(ls(), "dtt"))

#PARTITION
set.seed(1986)
smpl <- sample.int(nrow(dtt),size = floor(0.7*nrow(dtt)),replace=FALSE)
tr7 <- dtt[smpl,-c(28,29,31,32)]
te7 <- dtt[-smpl,-c(28,29,30,31,32)]
bag7=randomForest(ra7~.,data = tr7,importance=TRUE)
yhat7 = predict(bag7,newdata=te7)
# summary(yhat7) and summary(dtt[-smpl,-c(28,29,31,32)]$ra7)
#predict on entire dataset
dtt$pr7 <- predict(bag7, newdata=dtt[,-c(28,29,30,31,32)])

#REGRESSION
rgr7 <- dtt[dtt$pr7 == "p",] 
smpl7 <- sample.int(nrow(rgr7),size = floor(0.7*nrow(rgr7)),replace=FALSE)
fit7 <- lm(RAMNT_7 ~.,data=rgr7[smpl7,-c(29:33)])
pr7 <- predict(fit7,newdata = rgr7[-smpl7,-c(28:33)])
#predict on entire dataset
rgr7$pra7 <- predict(fit7,newdata = rgr7[,-c(28:33)])
write.csv(rgr7,file="results/rgr7.csv",row.names = FALSE)
rm(list=setdiff(ls(), "j"))