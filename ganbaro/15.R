#PROMOTION_15
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
d15 <- dk 
d15$RAMNT_15 <- j$RAMNT_15
d15$ADATE_15 <- j$ADATE_15
#Encode donations into p&np,
d15$ra15 <- as.factor(ifelse(is.na(d15$RAMNT_15),"np","p"))
# Encode profit/loss 36973 (0 profit),40701 (-0.68 profit), 17738 (donated)
d15$profit_15 <- ifelse(is.na(d15$RAMNT_15),ifelse(is.na(d15$ADATE_15),0,-0.68),d15$RAMNT_15 - 0.68)
#Encode mailing into m$nm
d15$ad15 <- as.factor(ifelse(is.na(d15$ADATE_15),"nm","m"))
#Encode RAMNT_15 and ADATE_15 NAs as 0
d15$RAMNT_15 <- ifelse(is.na(d15$RAMNT_15),0,d15$RAMNT_15)
d15$ADATE_15 <- ifelse(is.na(d15$ADATE_15),0,d15$ADATE_15)
#OMIT NAs
dtt <- na.omit(d15)  #95,412x325 -> 60,805x325
dtt <- dtt[dtt$ad15 == "m",] # 37986*325, and no nm level
dtt$INCOME <- as.factor(dtt$INCOME)
#clear workspace, but d15
#rm(list=setdiff(ls(), "dtt"))

#PARTITION
set.seed(1986)
smpl <- sample.int(nrow(dtt),size = floor(0.7*nrow(dtt)),replace=FALSE)
tr15 <- dtt[smpl,-c(26,27,29,30)]
te15 <- dtt[-smpl,-c(26,27,28,29,30)]
bag15=randomForest(ra15~.,data = tr15,importance=TRUE)
yhat15 = predict(bag15,newdata=te15)
# summary(yhat15) and summary(dtt[-smpl,-c(28,29,31,32)]$ra15)
#predict on entire dataset
dtt$pr15 <- predict(bag15, newdata=dtt[,-c(26,27,28,29,30)])

#REGRESSION
rgr15 <- dtt[dtt$pr15 == "p",] 
smpl15 <- sample.int(nrow(rgr15),size = floor(0.7*nrow(rgr15)),replace=FALSE)
fit15 <- lm(RAMNT_15 ~.,data=rgr15[smpl15,-c(27:31)])
pr15 <- predict.lm(fit15,newdata = rgr15[-smpl15,-c(26:31)])
#predict on entire dataset
rgr15$pra15 <- predict(fit15,newdata = rgr15[,-c(26:31)])
write.csv(rgr15,file="results/rgr15.csv",row.names = FALSE)
rm(list=setdiff(ls(), "j"))
