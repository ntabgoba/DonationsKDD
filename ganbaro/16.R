#PROMOTION_16
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
d16 <- dk 
d16$RAMNT_16 <- j$RAMNT_16
d16$ADATE_16 <- j$ADATE_16
#Encode donations into p&np,
d16$ra16 <- as.factor(ifelse(is.na(d16$RAMNT_16),"np","p"))
# Encode profit/loss 36973 (0 profit),40701 (-0.68 profit), 17738 (donated)
d16$profit_16 <- ifelse(is.na(d16$RAMNT_16),ifelse(is.na(d16$ADATE_16),0,-0.68),d16$RAMNT_16 - 0.68)
#Encode mailing into m$nm
d16$ad16 <- as.factor(ifelse(is.na(d16$ADATE_16),"nm","m"))
#Encode RAMNT_16 and ADATE_16 NAs as 0
d16$RAMNT_16 <- ifelse(is.na(d16$RAMNT_16),0,d16$RAMNT_16)
d16$ADATE_16 <- ifelse(is.na(d16$ADATE_16),0,d16$ADATE_16)
#OMIT NAs
dtt <- na.omit(d16)  #95,412x325 -> 60,805x325
dtt <- dtt[dtt$ad16 == "m",] # 37986*325, and no nm level
dtt$INCOME <- as.factor(dtt$INCOME)
#clear workspace, but d16
#rm(list=setdiff(ls(), "dtt"))

#PARTITION
set.seed(1986)
smpl <- sample.int(nrow(dtt),size = floor(0.7*nrow(dtt)),replace=FALSE)
tr16 <- dtt[smpl,-c(28,29,31,32)]
te16 <- dtt[-smpl,-c(28,29,30,31,32)]
bag16=randomForest(ra16~.,data = tr16,importance=TRUE)
yhat16 = predict(bag16,newdata=te16)
# summary(yhat16) and summary(dtt[-smpl,-c(28,29,31,32)]$ra16)
#predict on entire dataset
dtt$pr16 <- predict(bag16, newdata=dtt[,-c(28,29,30,31,32)])

#REGRESSION
rgr16 <- dtt[dtt$pr16 == "p",] 
smpl16 <- sample.int(nrow(rgr16),size = floor(0.7*nrow(rgr16)),replace=FALSE)
fit16 <- lm(RAMNT_16 ~.,data=rgr16[smpl16,-c(29:33)])
pr16 <- predict(fit16,newdata = rgr16[-smpl16,-c(28:33)])
#predict on entire dataset
rgr16$pra16 <- predict(fit16,newdata = rgr16[,-c(28:33)])
write.csv(rgr16,file="results/rgr16.csv",row.names = FALSE)
rm(list=setdiff(ls(), "j"))