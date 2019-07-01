setwd("D:/Documents/R/AgentBasedModel/GenPlot/Readers")
source("Source_InvasionReader.R")
setwd("D:/ParamDumps/Invasion")

CD1 <- InvasionData(Group="Closed", Stat=1, NumInvad=1)
CD4 <-InvasionData(Group="Closed", Stat=1, NumInvad=4)
CO1<-InvasionData(Group="Closed", Stat=2, NumInvad=1)
CO4<-InvasionData(Group="Closed", Stat=2, NumInvad=4)
DC1<-InvasionData(Group="DelayedClosed", Stat=".25", NumInvad=1)
DC4<-InvasionData(Group="DelayedClosed", Stat=".25", NumInvad=4)
DO1<-InvasionData(Group="DelayedClosed", Stat=2, NumInvad=1)
DO4<-InvasionData(Group="DelayedClosed", Stat=2, NumInvad=4)
OC1<-InvasionData(Group="Open", Stat=".25", NumInvad=1)
OC4<-InvasionData(Group="Open", Stat=".25", NumInvad=4)
OD1<-InvasionData(Group="Open", Stat=1, NumInvad=1)
OD4<-InvasionData(Group="Open", Stat=1, NumInvad=4)

pdf("Conversion-Rep.pdf", width=8, height=4)
par(mar=c(1.5,1.5,1.5,1.5), mgp=c(0,0,0), mfrow=c(1,2))
InvasionPlot(0,CD4,DC4,DO4,OD4,OC4,CO4, "Conversion", FALSE,
             tag = FALSE, plot=c(TRUE, FALSE), letter='A')
InvasionPlot(1.5,CD4,DC4,DO4,OD4,OC4,CO4, "Conversion", FALSE,
             tag = FALSE, plot=c(TRUE, FALSE), letter='B')
dev.off()

pdf("Conversion-Match.pdf", width=8, height=4)
par(mar=c(1.5,1.5,1.5,1.5), mgp=c(0,0,0), mfrow=c(1,2))
InvasionPlot(0,CD4,DC4,DO4,OD4,OC4,CO4, "Conversion", FALSE,
             tag = FALSE, plot=c(FALSE, TRUE), letter='A')
InvasionPlot(1.5,CD4,DC4,DO4,OD4,OC4,CO4, "Conversion", FALSE,
             tag = FALSE, plot=c(FALSE, TRUE), letter='B')
dev.off()
#N=3
#pdf("Invasion Averages.pdf")
#par(mar=c(3,3,3,1), mfrow=c(2,3), mgp=c(1.5,.5,0))
#for(i in seq(N,18,by=3)){
#  LrnThrshData(Dir=getwd(), Group="Closed",Stat=1,NumInvad=4, file=i)
#}
#par(mfrow=c(2,3))
#for(i in seq(N,18,by=3)){
#  LrnThrshData(Dir=getwd(), Group="Closed",Stat=2,NumInvad=4, file=i)
#}

#par(mfrow=c(2,3))
#for(i in seq(N,18,by=3)){
#  LrnThrshData(Dir=getwd(), Group="DelayedClosed",Stat=.25,NumInvad=4, file=i)
#}
#par(mfrow=c(2,3))
#for(i in seq(N,18,by=3)){
#  LrnThrshData(Dir=getwd(), Group="DelayedClosed",Stat=2,NumInvad=4, file=i)
#}

#par(mfrow=c(2,3))
#for(i in seq(N,18,by=3)){
#  LrnThrshData(Dir=getwd(), Group="Open",Stat=.25,NumInvad=4, file=i)
#}
#par(mfrow=c(2,3))
#for(i in seq(N,18,by=3)){
#  LrnThrshData(Dir=getwd(), Group="Open",Stat=1,NumInvad=4, file=i)
#}
#dev.off()


#pdf("ConversionPersistence.pdf", width=8, height=8)
#par(mar=c(1,1,1,1), mgp=c(0,0,0), mfrow=c(2,2))#, bty="n")
#InvasionPlot(0,CD4,DC4,DO4,OD4,OC4,CO4, "Conversion", FALSE)
#InvasionPlot(0,CD4,DC4,DO4,OD4,OC4,CO4, "Persistence",FALSE)

#par(mar=c(1,1,1,1), mgp=c(0,0,0), mfrow=c(2,2))#, bty="n")
#InvasionPlot(1.5,CD4,DC4,DO4,OD4,OC4,CO4, "Conversion", FALSE)
#InvasionPlot(1.5,CD4,DC4,DO4,OD4,OC4,CO4, "Persistence",FALSE)
#dev.off()
