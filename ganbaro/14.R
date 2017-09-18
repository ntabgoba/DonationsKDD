#PROMOTION_14
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
d14 <- dk 
d14$RAMNT_14 <- j$RAMNT_14
d14$ADATE_14 <- j$ADATE_14
#Encode donations into p&np,
d14$ra14 <- as.factor(ifelse(is.na(d14$RAMNT_14),"np","p"))
# Encode profit/loss 36973 (0 profit),40701 (-0.68 profit), 17738 (donated)
d14$profit_14 <- ifelse(is.na(d14$RAMNT_14),ifelse(is.na(d14$ADATE_14),0,-0.68),d14$RAMNT_14 - 0.68)
#Encode mailing into m$nm
d14$ad14 <- as.factor(ifelse(is.na(d14$ADATE_14),"nm","m"))
#Encode RAMNT_14 and ADATE_14 NAs as 0
d14$RAMNT_14 <- ifelse(is.na(d14$RAMNT_14),0,d14$RAMNT_14)
d14$ADATE_14 <- ifelse(is.na(d14$ADATE_14),0,d14$ADATE_14)
#OMIT NAs
dtt <- na.omit(d14)  #95,412x325 -> 60,805x325
dtt <- dtt[dtt$ad14 == "m",] # 37986*325, and no nm level
dtt$INCOME <- as.factor(dtt$INCOME)
#clear workspace, but d14
#rm(list=setdiff(ls(), "dtt"))

#PARTITION
set.seed(1986)
smpl <- sample.int(nrow(dtt),size = floor(0.7*nrow(dtt)),replace=FALSE)
tr14 <- dtt[smpl,-c(28,29,31,32)]
te14 <- dtt[-smpl,-c(28,29,30,31,32)]
bag14=randomForest(ra14~.,data = tr14,importance=TRUE)
yhat14 = predict(bag14,newdata=te14)
# summary(yhat14) and summary(dtt[-smpl,-c(28,29,31,32)]$ra14)
#predict on entire dataset
dtt$pr14 <- predict(bag14, newdata=dtt[,-c(28,29,30,31,32)])

#REGRESSION
rgr14 <- dtt[dtt$pr14 == "p",] 
smpl14 <- sample.int(nrow(rgr14),size = floor(0.7*nrow(rgr14)),replace=FALSE)
fit14 <- lm(RAMNT_14 ~.,data=rgr14[smpl14,-c(29:33)])
pr14 <- predict(fit14,newdata = rgr14[-smpl14,-c(28:33)])
#predict on entire dataset
rgr14$pra14 <- predict(fit14,newdata = rgr14[,-c(28:33)])
write.csv(rgr14,file="results/rgr14.csv",row.names = FALSE)
rm(list=setdiff(ls(), "j"))