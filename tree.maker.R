
jio <- data.frame(a=c(1:5),b=c(6:10),c=c(11:15),d=c(16:20))
hirwa <- data.frame(a=c(1:5),e=c(6:10),f=c(11:15),g=c(16:20))
hj <- merge(jio,hirwa,by="a")
library(dplyr)
chunk1 <- hj[,c(1,1,2)]
colnames(chunk1) <- c("id","a","b")
chunk2 <- hj[,c(1,2,3)]
colnames(chunk2) <- c("id","a","b")
chunk3 <- hj[,c(1,3,4)]
colnames(chunk3) <- c("id","a","b")
hjj <- bind_rows(list(chunk1,chunk2,chunk3))
hjj[with(hjj, order(id)), ]
pfa[3] <- list(chunk3)
#bind the dfs
do.call("rbind", your_list_with_dfs)
h <- merge(hj,hj,by="a")
chunk_list <- list()
for (i in seq(1,11,2)){
        for (j in 1:6){chunk_list[j] <- list(h[,c(i,i+1,i+2)])}
}