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
Part <- dat[a:b,]
plot(c(a:b), Right[a:b], pch=16, cex=1.5, axes=F, ann=F)
text(a+150, 0.8, "Right response rate:", cex=2)
text(a+150, 0.75, round(sum(dat$correct[a:b])/200,2), cex=1.5)
axis(1,at=c(a:b),labels=c(1:201))
axis(2,at=c(0,1),labels=c("Wrong","Right"))
mtext(side=1, text = "Trials", line=3, cex=1.5)
mtext(side=2, text = "Outcome", line=1, cex=2.4)
mtext(paste("Responses per trial (Participant ID", dat$pid[a], ")"), cex=1.2, font=2)

Right_Left <- sum(Part$correct[Part$targLevel==1])/100
Right_Right <- sum(Part$correct[Part$targLevel==-1])/100

barplot(c(Right_Left,Right_Right), col= c("deeppink","red"), axes=F, ann=F, ylim=c(0,1))
axis(1,at=c(0.8,1.9),labels=c("Left Correct", "Right Correct"), font=2)
axis(2,at=c(0, 0.25, 0.5, 0.75, 1),labels=c("0","0.25", "0.5","0.75","1"),las=1)
text(0.7,Right_Left+.05,paste(Right_Left),cex=3,col='black',f=2)
text(1.9,Right_Right+.05,paste(Right_Right),cex=3,col='black',f=2)
mtext(paste("Hit rate per side (Participant ID", dat$pid[a], ")"), cex=1.2, font=2)

a <- a + 200
z <- z + 1
}
