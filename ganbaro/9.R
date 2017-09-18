#PROMOTION_9
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
d9 <- dk 
d9$RAMNT_9 <- j$RAMNT_9
d9$ADATE_9 <- j$ADATE_9
#Encode donations into p&np,
d9$ra9 <- as.factor(ifelse(is.na(d9$RAMNT_9),"np","p"))
# Encode profit/loss 36973 (0 profit),40701 (-0.68 profit), 17738 (donated)
d9$profit_9 <- ifelse(is.na(d9$RAMNT_9),ifelse(is.na(d9$ADATE_9),0,-0.68),d9$RAMNT_9 - 0.68)
#Encode mailing into m$nm
d9$ad9 <- as.factor(ifelse(is.na(d9$ADATE_9),"nm","m"))
#Encode RAMNT_9 and ADATE_9 NAs as 0
d9$RAMNT_9 <- ifelse(is.na(d9$RAMNT_9),0,d9$RAMNT_9)
d9$ADATE_9 <- ifelse(is.na(d9$ADATE_9),0,d9$ADATE_9)
#OMIT NAs
dtt <- na.omit(d9)  #95,49x325 -> 60,805x325
dtt <- dtt[dtt$ad9 == "m",] # 37986*325, and no nm level
dtt$INCOME <- as.factor(dtt$INCOME)
#clear workspace, but d9
#rm(list=setdiff(ls(), "dtt"))

#PARTITION
set.seed(1986)
smpl <- sample.int(nrow(dtt),size = floor(0.7*nrow(dtt)),replace=FALSE)
tr9 <- dtt[smpl,-c(28,29,31,32)]
te9 <- dtt[-smpl,-c(28,29,30,31,32)]
bag9=randomForest(ra9~.,data = tr9,importance=TRUE)
yhat9 = predict(bag9,newdata=te9)
# summary(yhat9) and summary(dtt[-smpl,-c(28,29,31,32)]$ra9)
#predict on entire dataset
dtt$pr9 <- predict(bag9, newdata=dtt[,-c(28,29,30,31,32)])

#REGRESSION
rgr9 <- dtt[dtt$pr9 == "p",] 
smpl9 <- sample.int(nrow(rgr9),size = floor(0.7*nrow(rgr9)),replace=FALSE)
fit9 <- lm(RAMNT_9 ~.,data=rgr9[smpl9,-c(29:33)])
pr9 <- predict(fit9,newdata = rgr9[-smpl9,-c(28:33)])
#predict on entire dataset
rgr9$pra9 <- predict(fit9,newdata = rgr9[,-c(28:33)])
write.csv(rgr9,file="results/rgr9.csv",row.names = FALSE)
rm(list=setdiff(ls(), "j"))