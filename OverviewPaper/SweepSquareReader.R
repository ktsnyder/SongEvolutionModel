#setwd("D:/Documents/R/AgentBasedModel/GenPlot/Readers")
rm(list=objects())
source("Source_SweepSquareReader.R")
#setwd( "D:/ParamDumps")


#individual run checks
pdf("RepChecks.pdf", height = 10, width=7)
par(mfrow=c(4,3), mgp=c(1.5, .5, 0), mar=c(2.5,2.5,2,1))
for(i in 1:144){
  CheckRuns(file.path(getwd(),"OverView"), "SylRep", i)
}
dev.off()
pdf("AccChecks.pdf", height = 10, width=7)
par(mfrow=c(4,3), mgp=c(1.5, .5, 0), mar=c(2.5,2.5,2,1))
for(i in 1:144){
  CheckRuns(file.path(getwd(),"OverView"), "Acc", i, ylim=c(0,1))
}
dev.off()
pdf("LrnThrshChecks.pdf", height = 10, width=7)
par(mfrow=c(4,3), mgp=c(1.5, .5, 0), mar=c(2.5,2.5,2,1))
for(i in 1:144){
  CheckRuns(file.path(getwd(),"OverView"), "LrnThrsh", i,ylim=c(0,20))
}
dev.off()

pdf("ReRunChecks.pdf", height = 10, width=7)
par(mfrow=c(4,3), mgp=c(1.5, .5, 0), mar=c(2.5,2.5,2,1))
rerun <- c(76,82,88,94,100,106,112,118)
for(i in rerun){
  CheckRuns(file.path(getwd(),"ReRunDone"), "SylRep", i, NSims=20000)
}
par(mfrow=c(4,3), mgp=c(1.5, .5, 0), mar=c(2.5,2.5,2,1))

for(i in rerun){
  CheckRuns(file.path(getwd(),"ReRunDone"), "LrnThrsh", i,ylim=c(0,20), NSims=20000)
}
par(mfrow=c(4,3), mgp=c(1.5, .5, 0), mar=c(2.5,2.5,2,1))

for(i in rerun){
  CheckRuns(file.path(getwd(),"ReRunDone"), "Acc", i,ylim=c(0,1), NSims=20000)
}
dev.off()





#the Stitcher function was used to get the data into the correct format in all cases.  I.e:
#Stitcher(file.path(getwd(),"foldername"))


#Main boxplots
Data <- LoadData()#file.path(getwd(),"OverView"))
Ordered <- order(Data$FinalData2$PMatchPref, Data$FinalData2$PLrnStrtgy,
                 Data$FinalData2$PLrnPnlty, as.numeric(Data$FinalData2$PLrningThrshld),
                 as.numeric(Data$FinalData2$PRepSize), Data$FinalData2$PAccrcy, decreasing = FALSE)
Labels <- list(Label1=c("LTH=2", "LTH=10"),
               Label2=c("Lpen=0", "Lpen=.75", "Lpen=1.5"),
               Label3=c("Add", "Add/For", "Consen"),
               Label4=c("Repertoire Preferred", "Match Preferred"))
pdf("Overview.pdf", width=11, height=8.5)
  QuadPlots(Labels, Ordered, TRUE, mar=c(10,3,1,.5), End=TRUE)
dev.off()

Labels <- list(Label1=c("LTH=2", "LTH=10"),
               Label2=c("Lpen=0", "Lpen=.75", "Lpen=1.5"),
               Label3=c("Add", "Add/Forget", "Consensus"),
               Label4=NA)

Data <- CombineAll(Data, Ordered, 4)
pdf("Overview-Rep.pdf", width=4.25, height=11)
  QuadPlots(Labels, 1:36, subset=1:18,  FALSE, Match=FALSE, mfrow=c(4,1), mar=c(0,3,1.25,.5), End = FALSE)
dev.off()

pdf("Overview-Match.pdf", width=4.25, height=11)
  QuadPlots(Labels, 1:36, subset=19:36,  FALSE, mfrow=c(5,1), mar=c(0,3,1.25,.5), End = FALSE)
dev.off()


Data <- LoadData()#file.path(getwd(),"ChanceInv"))
Ordered <- order(Data$FinalData2$PMatchPref, Data$FinalData2$PLrnStrtgy,
                 Data$FinalData2$PLrnPnlty, as.numeric(Data$FinalData2$ChanInv))
Labels <- list(Label1=c("ChInv=.01", "ChInv=.05", "ChInv=.5"),
               Label2=c("Lpen=0", "Lpen=.75", "Lpen=1.5"),
               Label3=c("Add", "Add/For", "Consen"),
               Label4=NA)
pdf("ChanceInv-Rep.pdf", width=4.25, height=11)
  QuadPlots(Labels, Ordered, subset=1:27, FALSE, Match=FALSE, mfrow=c(4,1), mar=c(0,3,1.25,.5), End = FALSE, LinePush=-4)
dev.off()

pdf("ChanceInv-Match.pdf", width=4.25, height=11)
  QuadPlots(Labels, Ordered, subset=28:54, FALSE, mfrow=c(5,1), mar=c(0,3,1.25,.5), End = FALSE, LinePush=-4)
dev.off()



Data <- LoadData()#file.path(getwd(),"ChanceFor"))
Ordered <- order(Data$FinalData2$PMatchPref, Data$FinalData2$PLrnStrtgy,
                 Data$FinalData2$PLrnPnlty, Data$FinalData2$ChanFor)
Labels <- list(Label1=c("ChFor=.2", "ChFor=.5", "ChFor=.8"),
               Label2=c("Lpen=0", "Lpen=.75", "Lpen=1.5"),
               Label3=c("Add", "Add/For", "Consen"),
               Label4=NA)
pdf("ChanceFor-Rep.pdf", width=4.25, height=11)
QuadPlots(Labels, Ordered, subset=1:27, FALSE, Match=FALSE, mfrow=c(4,1), mar=c(0,3,1.25,.5), End = FALSE, LinePush=-4)
dev.off()

pdf("ChanceFor-Match.pdf", width=4.25, height=11)
QuadPlots(Labels, Ordered, subset=28:54, FALSE, mfrow=c(5,1), mar=c(0,3,1.25,.5), End = FALSE, LinePush=-4)
dev.off()


Data <- LoadData()#file.path(getwd(),"ConsensusTutors"))
Ordered <- order(Data$FinalData2$PMatchPref, as.numeric(Data$FinalData2$NumTut))
Labels <- list(Label1=2:12,
               Label2=NA,
               Label3=NA,
               Label4=NA)
pdf("ConsensusNoTutors-Rep.pdf", width=4.25, height=11)
QuadPlots(Labels, Ordered, subset=1:11, FALSE, Match=FALSE, mfrow=c(4,1), mar=c(0,3,1.25,.5), End = FALSE, LinePush=-1.5)
dev.off()

pdf("ConsensusNoTutors-Match.pdf", width=4.25, height=11)
QuadPlots(Labels, Ordered, subset=12:22, FALSE, mfrow=c(5,1), mar=c(0,3,1.25,.5), End = FALSE, LinePush=-1.5)
dev.off()


#Checking other tutor strategies
Main <- LoadData()#file.path(getwd(),"Noise"))
Vert <- LoadData()#file.path(getwd(),"VertOnly"))
#Obliq <- LoadData(file.path(getwd(),"ObliqueOnly"))
MainOrdered <- order(Main$FinalData2$PLrnStrtgy,
                 as.numeric(Main$FinalData2$AccNoise))
VertOrdered <- order(as.numeric(Vert$FinalData2$AccNoise))+length(MainOrdered)
#ObliqOrdered <- order(Obliq$FinalData2$PLrnStrtgy, as.numeric(Obliq$FinalData2$AccNoise))+length(MainOrdered)+ length(VertOrdered)
Data <- list(RBoxer=cbind(Main$RBoxer, Vert$RBoxer),#, Obliq$RBoxer),
             LBoxer=cbind(Main$LBoxer, Vert$LBoxer),#, Obliq$LBoxer),
             ABoxer=cbind(Main$ABoxer, Vert$ABoxer),#, Obliq$ABoxer),
             MBoxer=cbind(Main$MBoxer, Vert$MBoxer))#, Obliq$MBoxer))
Ordered <-c(MainOrdered,VertOrdered)#, ObliqOrdered)
Labels <- list(Label1=NULL,
               Label2=paste0("E=", gsub('0[.]', '.', sort(unique(Main$FinalData2$AccNoise)))),
               Label3=c("Add", "Add/For", "Consen", "Only Vert"),#, "O-Add", "O-Add/For", "O-Consen"),
               Label4=NA)
pdf("AccuracyNoise.pdf", width=4.25, height=11)
QuadPlots(Labels, Ordered,Color=c("azure4","white","azure2","azure3"),
          mfrow=c(5,1), mar=c(0,3,1.25,.5), End = FALSE, LinePush=-4)
dev.off()






Data <- LoadData()#file.path(getwd(),"VertObliqueDone"))
Ordered <- order(Data$FinalData2$PMatchPref, as.numeric(Data$FinalData2$NumTut))
Labels <- list(Label1=paste0("Min", c(0,3,7,15)),
               Label2=paste0(c(0,30,60,100), "%"),
               Label3=c("Vo", "vo", "O"),
               Label4=NA)
pdf("VERT.pdf", width=6, height=11)
QuadPlots(Labels, Ordered, subset=1:48, FALSE, Match=FALSE, mfrow=c(4,1), mar=c(0,3,1.25,.5), End = FALSE, LinePush=-2.5)
dev.off()

Data <- LoadData()#file.path(getwd(),"VOP2"))
Ordered <- order(rev(Data$FinalData2$LisThrsh),
                 Data$FinalData2$PLrnStrtgy,
                 Data$FinalData2$Main,
                 as.numeric(Data$FinalData2$PLrnPnlty),
                 as.numeric(Data$FinalData2$PLrningThrshld))
Labels <- list(Label1=c("LTh=2","LTh=10"),
               Label2=c("Lpen=0","Lpen=.75","Lpen=1.5"),
               Label3=c("Vo", "vo", "O"),
               Label4=c("Add", "Add/For", "Consen"))
pdf("VERT2.pdf", width=10, height=11)
QuadPlots(Labels, Ordered, subset=1:54, FALSE, Match=FALSE, mfrow=c(4,1), mar=c(0,3,1.25,.5), End = FALSE, LinePush=-3)
dev.off()





#heat plots
Data <- LoadData()#file.path(getwd(),"SquarePrefLPen"))
pdf("SquareSweep-LearningPenalty.pdf", height=8, width=8)
HeatPlot(Data, XY=c("PLrnPnlty","PRepPref"), Type="RepSize",
         XYLab=c("Learning Penalty", "Repertoire Size Preference"),
         Between = FALSE, Mfrow=c(3,3), Letters = c('A', 'B', 'C'),
         Name="Repertoire Size")
HeatPlot(Data, XY=c("PLrnPnlty", "PRepPref"), Type="LrnThsh",
         XYLab=c("Learning Penalty", "Repertoire Size Preference"),
         Between = FALSE, Add=TRUE, Letters = c('D', 'E', 'F'),
         Name="Learning Threshold")
HeatPlot(Data, XY=c("PLrnPnlty", "PRepPref"), Type="Match",
         XYLab=c("Learning Penalty", "Repertoire Size Preference"),
         Between = FALSE, Add=TRUE, Letters = c('G', 'H', 'I'),
         Name="Matching")
dev.off()

Data <- LoadData()#file.path(getwd(),"SquarePrefLrnThsh"))
pdf("SquareSweep-LearningThresh.pdf", height=8, width=8)
HeatPlot(Data, XY=c("PLrningThrshld","PRepPref"), Type="RepSize",
         XYLab=c("Initial Learning Threshold", "Repertoire Size Preference"),
         Between = FALSE, Mfrow=c(3,3), Letters = c('A', 'B', 'C'),
         Name="Repertoire Size")
HeatPlot(Data, XY=c("PLrningThrshld", "PRepPref"), Type="LrnThsh",
         XYLab=c("Initial Learning Threshold", "Repertoire Size Preference"),
         Between = FALSE, Add=TRUE, Letters = c('D', 'E', 'F'),
         Name="Learning Threshold")
HeatPlot(Data, XY=c("PLrningThrshld", "PRepPref"), Type="Match",
         XYLab=c("Initial Learning Threshold", "Repertoire Size Preference"),
         Between = FALSE, Add=TRUE, Letters = c('G', 'H', 'I'),
         Name="Matching")
dev.off()

Data <- LoadData()#file.path(getwd(),"SquarePrefAcc"))
pdf("SquareSweep-Accuracy.pdf", height=8, width=8)
HeatPlot(Data, XY=c("PAccrcy","PRepPref"), Type="RepSize",
         XYLab=c("Initial Accuracy", "Repertoire Size Preference"),
         Between = FALSE, Mfrow=c(3,3), Letters = c('A', 'B', 'C'),
         Name="Repertoire Size")
HeatPlot(Data, XY=c("PAccrcy", "PRepPref"), Type="LrnThsh",
         XYLab=c("Initial Accuracy", "Repertoire Size Preference"),
         Between = FALSE, Add=TRUE, Letters = c('D', 'E', 'F'),
         Name="Learning Threshold")
HeatPlot(Data, XY=c("PAccrcy", "PRepPref"), Type="Match",
         XYLab=c("Initial Accuracy", "Repertoire Size Preference"),
         Between = FALSE, Add=TRUE, Letters = c('G', 'H', 'I'),
         Name="Matching")
dev.off()


#Stitcher(file.path(getwd(),"Age"))
#Data <- LoadData(file.path(getwd(),"Age"))
#Ordered <- order(Data$FinalData2$PMatchPref, Data$FinalData2$PLrnStrtgy,
#                 as.numeric(Data$FinalData2$ChanInv))
#Labels <- list(Label1=NULL,
#               Label2=c("Age=5", "Age=10", "Age=15", "Age=20", "Age=25"),
#               Label3=c("Add", "Add/For", "Consen"),
#               Label4=c("Repertoire Preferred", "Match Preferred"))
#pdf("Age.pdf", width=4.25, height=11)
#QuadPlots(Labels, Ordered, mfrow=c(5,1), mar=c(0,3,1.25,.5), End = FALSE, LinePush=-5.5)
#dev.off()