#Donations
library(dplyr)
library(glmnet)
library(ggplot2)
df <- read.csv("val.csv",na.strings=c("NA","", " "))
mpd_dates <- df %>% select(matches("ADATE_|RAMNT_|RDATE_|RFA_|DOB|ZIP"))
df_nod <- df[,!(colnames(df) %in% colnames(mpd_dates))]
na_nod <- sapply(df_nod, function(x) sum(is.na(x)))
na_no <- na_nod[na_nod < 23666]  # cut from age var
df_gd <- df_nod[,(colnames(df_nod) %in% names(na_no))]

#add dependent var RAMNT_3 and independent var ADATE_2 
df_g <- df[,c("RAMNT_3","ADATE_2",colnames(df_gd))]

#If MAILED, will donate or not
df_g$RA3 <- ifelse(is.na(df_g$RAMNT_3),"np","p")
df_g$RAMNT_3 <- ifelse(is.na(df_g$RAMNT_3),0,df_g$RAMNT_3)
df_g$ADATE_2 <- ifelse(is.na(df_g$ADATE_2),"nm","m")
#partition
sampo <- sample.int(n=dim(df_g)[1],size=floor(0.7*dim(df_g)[1]),
                    replace = F)
train3 <- df_g[sampo,]
test3 <- df_g[-sampo,]
#Logistic Regression
x_3 <- model.matrix(~.,train3[,-c(2,327)])
#check which variables have fewer or un adjusted levels
train3_factors <- train3[,sapply(train3, is.factor)]
train3l <- train3[,!(colnames(train3) %in% colnames(train3_factors))]
fit_lgs <- glmnet()


