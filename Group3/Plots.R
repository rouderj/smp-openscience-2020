rm(list=ls())
dir()
dat=read.csv(url('https://raw.githubusercontent.com/PerceptionCognitionLab/data3/master/brightCompare1/data'),header=TRUE)


Output <- NULL

Output[dat$correct=="TRUE"&dat$targLevel==1] <- 'Corr_L'
Output[dat$correct=="TRUE"&dat$targLevel==-1] <- 'Corr_R'
Output[dat$correct=="FALSE"&dat$targLevel==1] <- 'Wr_L'
Output[dat$correct=="FALSE"&dat$targLevel==-1] <- 'Wr_R'

Correct <- NULL

for(side in sort(unique(dat$targLevel))){
Correct <- append(Correct, sum(dat$correct[dat$targLevel==side]=='True'))
}

Right <- NULL
Right[dat$correct==TRUE] <- "1"
Right[dat$correct==FALSE] <- "0"

a <- 1
z <- 1
for(p in sort(unique(dat$pid))){
b <- a + 200
plot(c(a:b), Right[a:b], pch=16, cex=1.5)
mtext(paste("Participant", z))
a <- a + 200
z <- z + 1
}


