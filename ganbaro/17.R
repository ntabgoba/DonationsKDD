#PROMOTION_17
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
d17 <- dk 
d17$RAMNT_17 <- j$RAMNT_17
d17$ADATE_17 <- j$ADATE_17
#Encode donations into p&np,
d17$ra17 <- as.factor(ifelse(is.na(d17$RAMNT_17),"np","p"))
# Encode profit/loss 36973 (0 profit),40701 (-0.68 profit), 17738 (donated)
d17$profit_17 <- ifelse(is.na(d17$RAMNT_17),ifelse(is.na(d17$ADATE_17),0,-0.68),d17$RAMNT_17 - 0.68)
#Encode mailing into m$nm
d17$ad17 <- as.factor(ifelse(is.na(d17$ADATE_17),"nm","m"))
#Encode RAMNT_17 and ADATE_17 NAs as 0
d17$RAMNT_17 <- ifelse(is.na(d17$RAMNT_17),0,d17$RAMNT_17)
d17$ADATE_17 <- ifelse(is.na(d17$ADATE_17),0,d17$ADATE_17)
#OMIT NAs
dtt <- na.omit(d17)  #95,412x325 -> 60,805x325
dtt <- dtt[dtt$ad17 == "m",] # 37986*325, and no nm level
dtt$INCOME <- as.factor(dtt$INCOME)
#clear workspace, but d17
#rm(list=setdiff(ls(), "dtt"))

#PARTITION
set.seed(1986)
smpl <- sample.int(nrow(dtt),size = floor(0.7*nrow(dtt)),replace=FALSE)
tr17 <- dtt[smpl,-c(28,29,31,32)]
te17 <- dtt[-smpl,-c(28,29,30,31,32)]
bag17=randomForest(ra17~.,data = tr17,importance=TRUE)
yhat17 = predict(bag17,newdata=te17)
# summary(yhat17) and summary(dtt[-smpl,-c(28,29,31,32)]$ra17)
#predict on entire dataset
dtt$pr17 <- predict(bag17, newdata=dtt[,-c(28,29,30,31,32)])

#REGRESSION
rgr17 <- dtt[dtt$pr17 == "p",] 
smpl17 <- sample.int(nrow(rgr17),size = floor(0.7*nrow(rgr17)),replace=FALSE)
fit17 <- lm(RAMNT_17 ~.,data=rgr17[smpl17,-c(29:33)])
pr17 <- predict(fit17,newdata = rgr17[-smpl17,-c(28:33)])
#predict on entire dataset
rgr17$pra17 <- predict(fit17,newdata = rgr17[,-c(28:33)])
write.csv(rgr17,file="results/rgr17.csv",row.names = FALSE)
rm(list=setdiff(ls(), "j"))