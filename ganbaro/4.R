#PROMOTION_4
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
d4 <- dk 
d4$RAMNT_4 <- j$RAMNT_4
d4$ADATE_4 <- j$ADATE_4
#Encode donations into p&np,
d4$ra4 <- as.factor(ifelse(is.na(d4$RAMNT_4),"np","p"))
# Encode profit/loss 36973 (0 profit),40701 (-0.68 profit), 17738 (donated)
d4$profit_4 <- ifelse(is.na(d4$RAMNT_4),ifelse(is.na(d4$ADATE_4),0,-0.68),d4$RAMNT_4 - 0.68)
#Encode mailing into m$nm
d4$ad4 <- as.factor(ifelse(is.na(d4$ADATE_4),"nm","m"))
#Encode RAMNT_4 and ADATE_4 NAs as 0
d4$RAMNT_4 <- ifelse(is.na(d4$RAMNT_4),0,d4$RAMNT_4)
d4$ADATE_4 <- ifelse(is.na(d4$ADATE_4),0,d4$ADATE_4)
#OMIT NAs
dtt <- na.omit(d4)  #95,44x325 -> 60,805x325
dtt <- dtt[dtt$ad4 == "m",] # 37986*325, and no nm level
dtt$INCOME <- as.factor(dtt$INCOME)
#clear workspace, but d4
#rm(list=setdiff(ls(), "dtt"))

#PARTITION
set.seed(1986)
smpl <- sample.int(nrow(dtt),size = floor(0.7*nrow(dtt)),replace=FALSE)
tr4 <- dtt[smpl,-c(28,29,31,32)]
te4 <- dtt[-smpl,-c(28,29,30,31,32)]
bag4=randomForest(ra4~.,data = tr4,importance=TRUE)
yhat4 = predict(bag4,newdata=te4)
# summary(yhat4) and summary(dtt[-smpl,-c(28,29,31,32)]$ra4)
#predict on entire dataset
dtt$pr4 <- predict(bag4, newdata=dtt[,-c(28,29,30,31,32)])

#REGRESSION
rgr4 <- dtt[dtt$pr4 == "p",] 
smpl4 <- sample.int(nrow(rgr4),size = floor(0.7*nrow(rgr4)),replace=FALSE)
fit4 <- lm(RAMNT_4 ~.,data=rgr4[smpl4,-c(29:33)])
pr4 <- predict(fit4,newdata = rgr4[-smpl4,-c(28:33)])
#predict on entire dataset
rgr4$pra4 <- predict(fit4,newdata = rgr4[,-c(28:33)])
write.csv(rgr4,file="results/rgr4.csv",row.names = FALSE)
rm(list=setdiff(ls(), "j"))