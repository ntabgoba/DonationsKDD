#PROMOTION_8
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
d8 <- dk 
d8$RAMNT_8 <- j$RAMNT_8
d8$ADATE_8 <- j$ADATE_8
#Encode donations into p&np,
d8$ra8 <- as.factor(ifelse(is.na(d8$RAMNT_8),"np","p"))
# Encode profit/loss 36973 (0 profit),40701 (-0.68 profit), 17738 (donated)
d8$profit_8 <- ifelse(is.na(d8$RAMNT_8),ifelse(is.na(d8$ADATE_8),0,-0.68),d8$RAMNT_8 - 0.68)
#Encode mailing into m$nm
d8$ad8 <- as.factor(ifelse(is.na(d8$ADATE_8),"nm","m"))
#Encode RAMNT_8 and ADATE_8 NAs as 0
d8$RAMNT_8 <- ifelse(is.na(d8$RAMNT_8),0,d8$RAMNT_8)
d8$ADATE_8 <- ifelse(is.na(d8$ADATE_8),0,d8$ADATE_8)
#OMIT NAs
dtt <- na.omit(d8)  #95,48x325 -> 60,805x325
dtt <- dtt[dtt$ad8 == "m",] # 37986*325, and no nm level
dtt$INCOME <- as.factor(dtt$INCOME)
#clear workspace, but d8
#rm(list=setdiff(ls(), "dtt"))

#PARTITION
set.seed(1986)
smpl <- sample.int(nrow(dtt),size = floor(0.7*nrow(dtt)),replace=FALSE)
tr8 <- dtt[smpl,-c(28,29,31,32)]
te8 <- dtt[-smpl,-c(28,29,30,31,32)]
bag8=randomForest(ra8~.,data = tr8,importance=TRUE)
yhat8 = predict(bag8,newdata=te8)
# summary(yhat8) and summary(dtt[-smpl,-c(28,29,31,32)]$ra8)
#predict on entire dataset
dtt$pr8 <- predict(bag8, newdata=dtt[,-c(28,29,30,31,32)])

#REGRESSION
rgr8 <- dtt[dtt$pr8 == "p",] 
smpl8 <- sample.int(nrow(rgr8),size = floor(0.7*nrow(rgr8)),replace=FALSE)
fit8 <- lm(RAMNT_8 ~.,data=rgr8[smpl8,-c(29:33)])
pr8 <- predict(fit8,newdata = rgr8[-smpl8,-c(28:33)])
#predict on entire dataset
rgr8$pra8 <- predict(fit8,newdata = rgr8[,-c(28:33)])
write.csv(rgr8,file="results/rgr8.csv",row.names = FALSE)
rm(list=setdiff(ls(), "j"))