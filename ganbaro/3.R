#PROMOTION_3
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
d3 <- dk 
d3$RAMNT_3 <- j$RAMNT_3
d3$ADATE_3 <- j$ADATE_3
#Encode donations into p&np,
d3$ra3 <- as.factor(ifelse(is.na(d3$RAMNT_3),"np","p"))
# Encode profit/loss 36973 (0 profit),40701 (-0.68 profit), 17738 (donated)
d3$profit_3 <- ifelse(is.na(d3$RAMNT_3),ifelse(is.na(d3$ADATE_3),0,-0.68),d3$RAMNT_3 - 0.68)
#Encode mailing into m$nm
d3$ad3 <- as.factor(ifelse(is.na(d3$ADATE_3),"nm","m"))
#Encode RAMNT_3 and ADATE_3 NAs as 0
d3$RAMNT_3 <- ifelse(is.na(d3$RAMNT_3),0,d3$RAMNT_3)
d3$ADATE_3 <- ifelse(is.na(d3$ADATE_3),0,d3$ADATE_3)
#OMIT NAs
dtt <- na.omit(d3)  #95,43x325 -> 60,805x325
dtt <- dtt[dtt$ad3 == "m",] # 37986*325, and no nm level
dtt$INCOME <- as.factor(dtt$INCOME)
#clear workspace, but d3
#rm(list=setdiff(ls(), "dtt"))

#PARTITION
set.seed(1986)
smpl <- sample.int(nrow(dtt),size = floor(0.7*nrow(dtt)),replace=FALSE)
tr3 <- dtt[smpl,-c(28,29,31,32)]
te3 <- dtt[-smpl,-c(28,29,30,31,32)]
bag3=randomForest(ra3~.,data = tr3,importance=TRUE)
yhat3 = predict(bag3,newdata=te3)
# summary(yhat3) and summary(dtt[-smpl,-c(28,29,31,32)]$ra3)
#predict on entire dataset
dtt$pr3 <- predict(bag3, newdata=dtt[,-c(28,29,30,31,32)])

#REGRESSION
rgr3 <- dtt[dtt$pr3 == "p",] 
smpl3 <- sample.int(nrow(rgr3),size = floor(0.7*nrow(rgr3)),replace=FALSE)
fit3 <- lm(RAMNT_3 ~.,data=rgr3[smpl3,-c(29:33)])
pr3 <- predict(fit3,newdata = rgr3[-smpl3,-c(28:33)])
#predict on entire dataset
rgr3$pra3 <- predict(fit3,newdata = rgr3[,-c(28:33)])
write.csv(rgr3,file="results/rgr3.csv",row.names = FALSE)
rm(list=setdiff(ls(), "j"))