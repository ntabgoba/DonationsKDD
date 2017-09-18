#PROMOTION_11
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
d11 <- dk 
d11$RAMNT_11 <- j$RAMNT_11
d11$ADATE_11 <- j$ADATE_11
#Encode donations into p&np,
d11$ra11 <- as.factor(ifelse(is.na(d11$RAMNT_11),"np","p"))
# Encode profit/loss 36973 (0 profit),40701 (-0.68 profit), 17738 (donated)
d11$profit_11 <- ifelse(is.na(d11$RAMNT_11),ifelse(is.na(d11$ADATE_11),0,-0.68),d11$RAMNT_11 - 0.68)
#Encode mailing into m$nm
d11$ad11 <- as.factor(ifelse(is.na(d11$ADATE_11),"nm","m"))
#Encode RAMNT_11 and ADATE_11 NAs as 0
d11$RAMNT_11 <- ifelse(is.na(d11$RAMNT_11),0,d11$RAMNT_11)
d11$ADATE_11 <- ifelse(is.na(d11$ADATE_11),0,d11$ADATE_11)
#OMIT NAs
dtt <- na.omit(d11)  #95,412x325 -> 60,805x325
dtt <- dtt[dtt$ad11 == "m",] # 37986*325, and no nm level
dtt$INCOME <- as.factor(dtt$INCOME)
#clear workspace, but d11
#rm(list=setdiff(ls(), "dtt"))

#PARTITION
set.seed(1986)
smpl <- sample.int(nrow(dtt),size = floor(0.7*nrow(dtt)),replace=FALSE)
tr11 <- dtt[smpl,-c(28,29,31,32)]
te11 <- dtt[-smpl,-c(28,29,30,31,32)]
bag11=randomForest(ra11~.,data = tr11,importance=TRUE)
yhat11 = predict(bag11,newdata=te11)
# summary(yhat11) and summary(dtt[-smpl,-c(28,29,31,32)]$ra11)
#predict on entire dataset
dtt$pr11 <- predict(bag11, newdata=dtt[,-c(28,29,30,31,32)])

#REGRESSION
rgr11 <- dtt[dtt$pr11 == "p",] 
smpl11 <- sample.int(nrow(rgr11),size = floor(0.7*nrow(rgr11)),replace=FALSE)
fit11 <- lm(RAMNT_11 ~.,data=rgr11[smpl11,-c(29:33)])
pr11 <- predict(fit11,newdata = rgr11[-smpl11,-c(28:33)])
#predict on entire dataset
rgr11$pra11 <- predict(fit11,newdata = rgr11[,-c(28:33)])
write.csv(rgr11,file="results/rgr11.csv",row.names = FALSE)
rm(list=setdiff(ls(), "j"))
