#PROMOTION_18
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
d18 <- dk 
d18$RAMNT_18 <- j$RAMNT_18
d18$ADATE_18 <- j$ADATE_18
#Encode donations into p&np,
d18$ra18 <- as.factor(ifelse(is.na(d18$RAMNT_18),"np","p"))
# Encode profit/loss 36973 (0 profit),40701 (-0.68 profit), 17738 (donated)
d18$profit_18 <- ifelse(is.na(d18$RAMNT_18),ifelse(is.na(d18$ADATE_18),0,-0.68),d18$RAMNT_18 - 0.68)
#Encode mailing into m$nm
d18$ad18 <- as.factor(ifelse(is.na(d18$ADATE_18),"nm","m"))
#Encode RAMNT_18 and ADATE_18 NAs as 0
d18$RAMNT_18 <- ifelse(is.na(d18$RAMNT_18),0,d18$RAMNT_18)
d18$ADATE_18 <- ifelse(is.na(d18$ADATE_18),0,d18$ADATE_18)
#OMIT NAs
dtt <- na.omit(d18)  #95,412x325 -> 60,805x325
dtt <- dtt[dtt$ad18 == "m",] # 37986*325, and no nm level
dtt$INCOME <- as.factor(dtt$INCOME)
#clear workspace, but d18
#rm(list=setdiff(ls(), "dtt"))

#PARTITION
set.seed(1986)
smpl <- sample.int(nrow(dtt),size = floor(0.7*nrow(dtt)),replace=FALSE)
tr18 <- dtt[smpl,-c(28,29,31,32)]
te18 <- dtt[-smpl,-c(28,29,30,31,32)]
bag18=randomForest(ra18~.,data = tr18,importance=TRUE)
yhat18 = predict(bag18,newdata=te18)
# summary(yhat18) and summary(dtt[-smpl,-c(28,29,31,32)]$ra18)
#predict on entire dataset
dtt$pr18 <- predict(bag18, newdata=dtt[,-c(28,29,30,31,32)])

#REGRESSION
rgr18 <- dtt[dtt$pr18 == "p",] 
smpl18 <- sample.int(nrow(rgr18),size = floor(0.7*nrow(rgr18)),replace=FALSE)
fit18 <- lm(RAMNT_18 ~.,data=rgr18[smpl18,-c(29:33)])
pr18 <- predict(fit18,newdata = rgr18[-smpl18,-c(28:33)])
#predict on entire dataset
rgr18$pra18 <- predict(fit18,newdata = rgr18[,-c(28:33)])
write.csv(rgr18,file="results/rgr18.csv",row.names = FALSE)
rm(list=setdiff(ls(), "j"))