#Combine all the predictions
library(ggplot2)
p24 <- read.csv("results/rgr24.csv")
p23 <- read.csv("results/rgr23.csv")
p22 <- read.csv("results/rgr22.csv")
p21 <- read.csv("results/rgr21.csv")
p20 <- read.csv("results/rgr20.csv")
p19 <- read.csv("results/rgr19.csv")
p18 <- read.csv("results/rgr18.csv")
p17 <- read.csv("results/rgr17.csv")
p16 <- read.csv("results/rgr16.csv")
p15 <- read.csv("results/rgr15.csv")
p14 <- read.csv("results/rgr14.csv")
p13 <- read.csv("results/rgr13.csv")
#pick predicted and actual values
sapply(list(p24,p23,p22,p21,p20,p19,p18,p17,p16,p15,p14,p13),function (x) dim(x))
#Add a row or to, of zero to have 34 rows on all.
#put all on same length        
burefu <- sapply(list(p24$pra24,p24$RAMNT_24,p24$profit_24,
                  p23$pra23,p23$RAMNT_23,p23$profit_23,
                  p22$pra22,p22$RAMNT_22,p22$profit_22,
                  p21$pra21,p21$RAMNT_21,p21$profit_21,
                  p20$pra20,p20$RAMNT_20,p20$profit_20,
                  p19$pra19,p19$RAMNT_19,p19$profit_19,
                  p18$pra18,p18$RAMNT_18,p18$profit_18,
                  p17$pra17,p17$RAMNT_17,p17$profit_17,
                  p16$pra16,p16$RAMNT_16,p16$profit_16,
                  p15$pra15,p15$RAMNT_15,p15$profit_15,
                  p14$pra14,p14$RAMNT_14,p14$profit_14,
                  p13$pra13,p13$RAMNT_13,p13$profit_13),
       function(x) length(x) <- 15871
)

p_list <-  list(p24$pra24,p24$RAMNT_24,p24$profit_24,
                p23$pra23,p23$RAMNT_23,p23$profit_23,
                p22$pra22,p22$RAMNT_22,p22$profit_22,
                p21$pra21,p21$RAMNT_21,p21$profit_21,
                p20$pra20,p20$RAMNT_20,p20$profit_20,
                p19$pra19,p19$RAMNT_19,p19$profit_19,
                p18$pra18,p18$RAMNT_18,p18$profit_18,
                p17$pra17,p17$RAMNT_17,p17$profit_17,
                p16$pra16,p16$RAMNT_16,p16$profit_16,
                p15$pra15,p15$RAMNT_15,p15$profit_15,
                p14$pra14,p14$RAMNT_14,p14$profit_14,
                p13$pra13,p13$RAMNT_13,p13$profit_13)
max.length <- max(sapply(p_list, length))
## Add NA values to list elements
p_lNA <- lapply(p_list, function(v) { c(v, rep(NA, max.length-length(v)))})
## Rbind
pal <- do.call(cbind, p_lNA)
paldf <- as.data.frame(pal)
colnames(paldf) <- c("pra24","RAMNT_24","profit_24",
                     "pra23","RAMNT_23","profit_23",
                     "pra22","RAMNT_22","profit_22",
                     "pra21","RAMNT_21","profit_21",
                     "pra20","RAMNT_20","profit_20",
                     "pra19","RAMNT_19","profit_19",
                     "pra18","RAMNT_18","profit_18",
                     "pra17","RAMNT_17","profit_17",
                     "pra16","RAMNT_16","profit_16",
                     "pra15","RAMNT_15","profit_15",
                     "pra14","RAMNT_14","profit_14",
                     "pra13","RAMNT_13","profit_13")

palsum <- sapply(paldf[,seq(1,34,3)],function(x) sum(x,na.rm = TRUE))
palnum <- sapply(paldf[,seq(1,34,3)],function(x) sum(!is.na(x)))
sum_num <- data.frame(palsum,palnum)
sum_num$promo <- 1:dim(sum_num)[1]

ggplot(sum_num) + 
        geom_line(aes(promo,palsum))+
        geom_line(aes(promo,palnum))
