#PROMOTION_6
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
dk <- dk[,-c(1:6,20:305,306,317,318)]
#REPEATIVE WORK
#Add mail date and amount donated for Promotion 1
d6 <- dk 
d6$RAMNT_6 <- j$RAMNT_6
d6$ADATE_6 <- j$ADATE_6
#Encode donations into p&np,
d6$ra6 <- as.factor(ifelse(is.na(d6$RAMNT_6),"np","p"))
# Encode profit/loss 36973 (0 profit),40701 (-0.68 profit), 17738 (donated)
d6$profit_6 <- ifelse(is.na(d6$RAMNT_6),ifelse(is.na(d6$ADATE_6),0,-0.68),d6$RAMNT_6 - 0.68)
#Encode mailing into m$nm
d6$ad6 <- as.factor(ifelse(is.na(d6$ADATE_6),"nm","m"))
#Encode RAMNT_6 and ADATE_6 NAs as 0
d6$RAMNT_6 <- ifelse(is.na(d6$RAMNT_6),0,d6$RAMNT_6)
d6$ADATE_6 <- ifelse(is.na(d6$ADATE_6),0,d6$ADATE_6)
#OMIT NAs
dtt <- na.omit(d6)  #95,412x325 -> 60,805x325
dtt <- dtt[dtt$ad6 == "m",] # 37986*325, and no nm level
dtt$INCOME <- as.factor(dtt$INCOME)
#clear workspace, but d6
#rm(list=setdiff(ls(), "dtt"))

#PARTITION
set.seed(1986)
smpl <- sample.int(nrow(dtt),size = floor(0.7*nrow(dtt)),replace=FALSE)
tr6 <- dtt[smpl,-c(26,27,29,30)]
te6 <- dtt[-smpl,-c(26,27,28,29,30)]
bag6=randomForest(ra6~.,data = tr6,importance=TRUE)
yhat6 = predict(bag6,newdata=te6)
# summary(yhat6) and summary(dtt[-smpl,-c(28,29,31,32)]$ra6)
#predict on entire dataset
dtt$pr6 <- predict(bag6, newdata=dtt[,-c(26,27,28,29,30)])

#REGRESSION
rgr6 <- dtt[dtt$pr6 == "p",] 
smpl6 <- sample.int(nrow(rgr6),size = floor(0.7*nrow(rgr6)),replace=FALSE)
fit6 <- lm(RAMNT_6 ~.,data=rgr6[smpl6,-c(27:31)])
pr6 <- predict.lm(fit6,newdata = rgr6[-smpl6,-c(26:31)])
#predict on entire dataset
rgr6$pra6 <- predict(fit6,newdata = rgr6[,-c(26:31)])
write.csv(rgr6,file="results/rgr6.csv",row.names = FALSE)
rm(list=setdiff(ls(), "j"))
