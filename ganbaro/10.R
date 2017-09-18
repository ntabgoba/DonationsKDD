#PROMOTION_10
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
d10 <- dk 
d10$RAMNT_10 <- j$RAMNT_10
d10$ADATE_10 <- j$ADATE_10
#Encode donations into p&np,
d10$ra10 <- as.factor(ifelse(is.na(d10$RAMNT_10),"np","p"))
# Encode profit/loss 36973 (0 profit),40701 (-0.68 profit), 17738 (donated)
d10$profit_10 <- ifelse(is.na(d10$RAMNT_10),ifelse(is.na(d10$ADATE_10),0,-0.68),d10$RAMNT_10 - 0.68)
#Encode mailing into m$nm
d10$ad10 <- as.factor(ifelse(is.na(d10$ADATE_10),"nm","m"))
#Encode RAMNT_10 and ADATE_10 NAs as 0
d10$RAMNT_10 <- ifelse(is.na(d10$RAMNT_10),0,d10$RAMNT_10)
d10$ADATE_10 <- ifelse(is.na(d10$ADATE_10),0,d10$ADATE_10)
#OMIT NAs
dtt <- na.omit(d10)  #95,412x325 -> 60,805x325
dtt <- dtt[dtt$ad10 == "m",] # 37986*325, and no nm level
dtt$INCOME <- as.factor(dtt$INCOME)
#clear workspace, but d10
#rm(list=setdiff(ls(), "dtt"))

#PARTITION
set.seed(1986)
smpl <- sample.int(nrow(dtt),size = floor(0.7*nrow(dtt)),replace=FALSE)
tr10 <- dtt[smpl,-c(28,29,31,32)]
te10 <- dtt[-smpl,-c(28,29,30,31,32)]
bag10=randomForest(ra10~.,data = tr10,importance=TRUE)
yhat10 = predict(bag10,newdata=te10)
# summary(yhat10) and summary(dtt[-smpl,-c(28,29,31,32)]$ra10)
#predict on entire dataset
dtt$pr10 <- predict(bag10, newdata=dtt[,-c(28,29,30,31,32)])

#REGRESSION
rgr10 <- dtt[dtt$pr10 == "p",] 
smpl10 <- sample.int(nrow(rgr10),size = floor(0.7*nrow(rgr10)),replace=FALSE)
fit10 <- lm(RAMNT_10 ~.,data=rgr10[smpl10,-c(29:33)])
pr10 <- predict(fit10,newdata = rgr10[-smpl10,-c(28:33)])
#predict on entire dataset
rgr10$pra10 <- predict(fit10,newdata = rgr10[,-c(28:33)])
write.csv(rgr10,file="results/rgr10.csv",row.names = FALSE)
rm(list=setdiff(ls(), "j"))