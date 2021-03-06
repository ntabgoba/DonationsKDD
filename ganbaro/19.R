#PROMOTION_19
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
d19 <- dk 
d19$RAMNT_19 <- j$RAMNT_19
d19$ADATE_19 <- j$ADATE_19
#Encode donations into p&np,
d19$ra19 <- as.factor(ifelse(is.na(d19$RAMNT_19),"np","p"))
# Encode profit/loss 36973 (0 profit),40701 (-0.68 profit), 17738 (donated)
d19$profit_19 <- ifelse(is.na(d19$RAMNT_19),ifelse(is.na(d19$ADATE_19),0,-0.68),d19$RAMNT_19 - 0.68)
#Encode mailing into m$nm
d19$ad19 <- as.factor(ifelse(is.na(d19$ADATE_19),"nm","m"))
#Encode RAMNT_19 and ADATE_19 NAs as 0
d19$RAMNT_19 <- ifelse(is.na(d19$RAMNT_19),0,d19$RAMNT_19)
d19$ADATE_19 <- ifelse(is.na(d19$ADATE_19),0,d19$ADATE_19)
#OMIT NAs
dtt <- na.omit(d19)  #95,412x325 -> 60,805x325
dtt <- dtt[dtt$ad19 == "m",] # 37986*325, and no nm level
dtt$INCOME <- as.factor(dtt$INCOME)
#clear workspace, but d19
#rm(list=setdiff(ls(), "dtt"))

#PARTITION
set.seed(1986)
smpl <- sample.int(nrow(dtt),size = floor(0.7*nrow(dtt)),replace=FALSE)
tr19 <- dtt[smpl,-c(26,27,29,30)]
te19 <- dtt[-smpl,-c(26,27,28,29,30)]
bag19=randomForest(ra19~.,data = tr19,importance=TRUE)
yhat19 = predict(bag19,newdata=te19)
# summary(yhat19) and summary(dtt[-smpl,-c(28,29,31,32)]$ra19)
#predict on entire dataset
dtt$pr19 <- predict(bag19, newdata=dtt[,-c(26,27,28,29,30)])

#REGRESSION
rgr19 <- dtt[dtt$pr19 == "p",] 
smpl19 <- sample.int(nrow(rgr19),size = floor(0.7*nrow(rgr19)),replace=FALSE)
fit19 <- lm(RAMNT_19 ~.,data=rgr19[smpl19,-c(27:31)])
pr19 <- predict.lm(fit19,newdata = rgr19[-smpl19,-c(26:31)])
#predict on entire dataset
rgr19$pra19 <- predict(fit19,newdata = rgr19[,-c(26:31)])
write.csv(rgr19,file="results/rgr19.csv",row.names = FALSE)
rm(list=setdiff(ls(), "j"))
