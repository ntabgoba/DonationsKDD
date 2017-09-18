#PROMOTION_20
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
d20 <- dk 
d20$RAMNT_20 <- j$RAMNT_20
d20$ADATE_20 <- j$ADATE_20
#Encode donations into p&np,
d20$ra20 <- as.factor(ifelse(is.na(d20$RAMNT_20),"np","p"))
# Encode profit/loss 36973 (0 profit),40701 (-0.68 profit), 17738 (donated)
d20$profit_20 <- ifelse(is.na(d20$RAMNT_20),ifelse(is.na(d20$ADATE_20),0,-0.68),d20$RAMNT_20 - 0.68)
#Encode mailing into m$nm
d20$ad20 <- as.factor(ifelse(is.na(d20$ADATE_20),"nm","m"))
#Encode RAMNT_20 and ADATE_20 NAs as 0
d20$RAMNT_20 <- ifelse(is.na(d20$RAMNT_20),0,d20$RAMNT_20)
d20$ADATE_20 <- ifelse(is.na(d20$ADATE_20),0,d20$ADATE_20)
#OMIT NAs
dtt <- na.omit(d20)  #95,412x325 -> 60,805x325
dtt <- dtt[dtt$ad20 == "m",] # 37986*325, and no nm level
dtt$INCOME <- as.factor(dtt$INCOME)
#clear workspace, but d20
#rm(list=setdiff(ls(), "dtt"))

#PARTITION
set.seed(1986)
smpl <- sample.int(nrow(dtt),size = floor(0.7*nrow(dtt)),replace=FALSE)
tr20 <- dtt[smpl,-c(27,28,30,31)]
te20 <- dtt[-smpl,-c(27,28,29,30,31)]
bag20=randomForest(ra20~.,data = tr20,importance=TRUE)
yhat20 = predict(bag20,newdata=te20)
# summary(yhat20) and summary(dtt[-smpl,-c(28,29,31,32)]$ra20)
#predict on entire dataset
dtt$pr20 <- predict(bag20, newdata=dtt[,-c(27,28,29,30,31)])

#REGRESSION
rgr20 <- dtt[dtt$pr20 == "p",] 
smpl20 <- sample.int(nrow(rgr20),size = floor(0.7*nrow(rgr20)),replace=FALSE)
fit20 <- lm(RAMNT_20 ~.,data=rgr20[smpl20,-c(28:32)])
pr20 <- predict.lm(fit20,newdata = rgr20[-smpl20,-c(27:32)])
#predict on entire dataset
rgr20$pra20 <- predict(fit20,newdata = rgr20[,-c(27:32)])
write.csv(rgr20,file="results/rgr20.csv",row.names = FALSE)
rm(list=setdiff(ls(), "j"))
