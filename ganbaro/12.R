#PROMOTION_12
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
d12 <- dk 
d12$RAMNT_12 <- j$RAMNT_12
d12$ADATE_12 <- j$ADATE_12
#Encode donations into p&np,
d12$ra12 <- as.factor(ifelse(is.na(d12$RAMNT_12),"np","p"))
# Encode profit/loss 36973 (0 profit),40701 (-0.68 profit), 17738 (donated)
d12$profit_12 <- ifelse(is.na(d12$RAMNT_12),ifelse(is.na(d12$ADATE_12),0,-0.68),d12$RAMNT_12 - 0.68)
#Encode mailing into m$nm
d12$ad12 <- as.factor(ifelse(is.na(d12$ADATE_12),"nm","m"))
#Encode RAMNT_12 and ADATE_12 NAs as 0
d12$RAMNT_12 <- ifelse(is.na(d12$RAMNT_12),0,d12$RAMNT_12)
d12$ADATE_12 <- ifelse(is.na(d12$ADATE_12),0,d12$ADATE_12)
#OMIT NAs
dtt <- na.omit(d12)  #95,412x325 -> 60,805x325
dtt <- dtt[dtt$ad12 == "m",] # 37986*325, and no nm level
dtt$INCOME <- as.factor(dtt$INCOME)
#clear workspace, but d12
#rm(list=setdiff(ls(), "dtt"))

#PARTITION
set.seed(1986)
smpl <- sample.int(nrow(dtt),size = floor(0.7*nrow(dtt)),replace=FALSE)
tr12 <- dtt[smpl,-c(28,29,31,32)]
te12 <- dtt[-smpl,-c(28,29,30,31,32)]
bag12=randomForest(ra12~.,data = tr12,importance=TRUE)
yhat12 = predict(bag12,newdata=te12)
# summary(yhat12) and summary(dtt[-smpl,-c(28,29,31,32)]$ra12)
#predict on entire dataset
dtt$pr12 <- predict(bag12, newdata=dtt[,-c(28,29,30,31,32)])

#REGRESSION
rgr12 <- dtt[dtt$pr12 == "p",] 
smpl12 <- sample.int(nrow(rgr12),size = floor(0.7*nrow(rgr12)),replace=FALSE)
fit12 <- lm(RAMNT_12 ~.,data=rgr12[smpl12,-c(29:33)])
pr12 <- predict(fit12,newdata = rgr12[-smpl12,-c(28:33)])
#predict on entire dataset
rgr12$pra12 <- predict(fit12,newdata = rgr12[,-c(28:33)])
write.csv(rgr12,file="results/rgr12.csv",row.names = FALSE)
rm(list=setdiff(ls(), "j"))