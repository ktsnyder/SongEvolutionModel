setwd("D:/Prelim")
rm(list=objects())
source("Source_SweepSquareReader.R")
#install_github("CreanzaLab/SongEvolutionModel/R-Package", force=TRUE, ref="FemalChoiceExpansion")




#Stitcher(file.path(getwd(),"MEFinal"))
Data <- LoadData(file.path(getwd(),"MEFinal", "Stitched"))
Ordered <- order(Data$FinalData2$MDial,
                 as.numeric(Data$FinalData2$Dial),
                 rev(as.logical(Data$FinalData2$Tutor)),
                 rev(as.logical(Data$FinalData2$Vertical)),
                 rev(as.logical(Data$FinalData2$Breed)),
                 rev(as.logical(Data$FinalData2$Uniform)),
                 decreasing = FALSE)
Labels <- list(Label1=paste(c("TRUE", "FALSE")),
               Label2=paste(c("TRUE", "FALSE")), #Tutor
               Label3=paste(c("TRUE", "FALSE")), #Breed
               Label4=paste("Dial", c(1,2,4,8, 16)))
write.csv(Data$FinalData2[,c(12,16, 17, 18, 19, 20)], "TableofKeyParams.csv")
pdf("Overview-Main.pdf", width=14, height=8.5)
  QuadPlots(Labels, Ordered, subset=1:80, FALSE, mar=c(10,3,1,.5), End=TRUE)
  QuadPlots(Labels, Ordered, subset=81:160, FALSE, mar=c(10,3,1,.5), End=TRUE)
  QuadPlots(Labels, Ordered, subset=161:240, FALSE, mar=c(10,3,1,.5), End=TRUE)
dev.off()
#########################
df <- TableData(file.path(getwd(),"MEFinal"), 240)
Results <- cbind.data.frame(Data$FinalData2[1:240,c(12,16, 17, 18, 19, 20)],df)
write.csv(Results, "TableofMagic.csv")
pdf("MatchPlotsFull.pdf", height=11, width=8)
  par(mfrow=c(2,1), mar=c(2.5,2.5,1,1), mgp=c(1.5,.5,0))
  Establish <- which(Results$MDial=="None")
  #########################
  plot((1:240)[-Establish], Results$corner[-Establish]#/(Results$centersd/sqrt(50))
       , type='p', pch=19,
       ylab="Center match for first dialect",
       xlab="Paramater Set",
       ylim=c(0,.9),
       panel.first = {
       })
  plotMe <- c(33,34,49,50, 65, 66, 81, 82, 97, 98, 113, 114, 130, 129, 145, 146)
  points((1:240)[-Establish][plotMe], Results$corner[-Establish][plotMe],
         pch=21,bg="cadetblue1", cex=.7)
  text(seq(1, 240, by=48)+24, .9, paste0("Dialects-",c(1,2,4,8,16)), font=2, adj=.5)
  ####################
  plot((49:240)[Establish],Results$farcorner[49:240][Establish], type='p', pch=19,
       ylab="Center match for last dialect",
       xlab="Paramater Set",
       xlim=c(49,240),
       ylim=c(0,.5)
       , panel.first = {
       })
  plotMe <- c(1,2,5,6,7,18,21,22,33,34,37,38,49,50,53,54)
  points((49:240)[Establish][plotMe], Results$farcorner[49:240][Establish][plotMe],
         pch=21,bg="cadetblue1", cex=.7)
  text(seq(48, 240, by=48)+24, .5, paste0("Dialects-",c(2,4,8,16)), font=2, adj=.5)
dev.off()




Results <- read.csv("TableofMagic.csv")
TrueEstablish <- which(Results$MDial == 'None' & Results$Dial > 1)
EstablishMain <- Results[TrueEstablish,c('Breed', 'Tutor', 'Vertical', 'Uniform', 'Dial', 'corner', 'farcorner')]
colnames(EstablishMain) <- c('Breed', 'Tutor', 'Vertical', 'Uniform', 'Dialects', 'Corner', 'FarCorner')
Success <- which(EstablishMain$FarCorner > .05)
EstablishMain$FarCorner <- round(EstablishMain$FarCorner, digits=3)
EstablishMain$FarCorner[Success] <- paste0(EstablishMain$FarCorner[Success], "*")
rownames(EstablishMain) <- NULL
print(xtable(EstablishMain, digits=3))

Maintain <- which(Results$MDial != 'None')
MaintainMain <- Results[Maintain,c('Breed', 'Tutor', 'Vertical', 'Uniform', 'Dial', 'center', 'edge')]
colnames(MaintainMain) <- c('Breed', 'Tutor', 'Vertical', 'Uniform', 'Dialects', 'Center', 'Edge')
MaintainMain['MDial'] <- rep(rep(c("Same", "Similar"), each=16), 5)
MaintainMain <- MaintainMain[,c(8, 1:7)]
#Success <- which(MaintainMain$Center > .05)
#MaintainMain$Center[Success] <- paste0(MaintainMain$Center[Success], "*")
rownames(MaintainMain) <- NULL
print(xtable(MaintainMain, digits=3))






pdf("MaintenanceExamples.pdf", width=6, height=4.5)
par(mfrow=c(3,4), mar=c(1.5,2.5,2,1), mgp=c(1.5,.5,0), xpd=NA)
OMGPlot(file.path(getwd(),"MEFinal"), 1, FALSE, c(1,21),
        Letter=list('A','B'), title=as.list(paste0("Param 1-", c(0, 4000))),
        LetterHeight=-4)
OMGPlot(file.path(getwd(),"MEFinal"), 49, FALSE, c(1,21),
        Letter=list('E','F'), title=as.list(paste0("Param 33-", c(0, 4000))),
        LetterHeight=-4)
OMGPlot(file.path(getwd(),"MEFinal"), 97, FALSE, c(1,21),
        Letter=list('C','D'), title=as.list(paste0("Param 65-", c(0, 4000))),
        LetterHeight=-4)
OMGPlot(file.path(getwd(),"MEFinal"), 145, FALSE, c(1,21),
        Letter=list('G','H'), title=as.list(paste0("Param 97-", c(0, 4000))),
        LetterHeight=-4)
OMGPlot(file.path(getwd(),"MEFinal"), 193, FALSE, c(1,21),
        Letter=list('I','J'), title=as.list(paste0("Param 127-", c(0, 4000))),
        LetterHeight=-4)
AddScale()
dev.off()

pdf("MaintenancePatterns.pdf", width=6, height=3)
par(mfrow=c(2,4), mar=c(1.5,2.5,2,1), mgp=c(1.5,.5,0), xpd=NA)
OMGPlot(file.path(getwd(),"MEFinal"), 49, FALSE, c(1,21),
        Letter=list('A','B'), title=as.list(paste0("Param 33-", c(0, 4000))),
        LetterHeight=-4)
OMGPlot(file.path(getwd(),"MEFinal"), 53, FALSE, c(1,21),
        Letter=list('C','D'), title=as.list(paste0("Param 37-", c(0, 4000))),
        LetterHeight=-4)
OMGPlot(file.path(getwd(),"MEFinal"), 51, FALSE, c(1,21),
        Letter=list('E','F'), title=as.list(paste0("Param 35-", c(0, 4000))),
        LetterHeight=-4)
#AddScale()
dev.off()

pdf("EstablishExamples.pdf", width=6, height=3)
  par(mfrow=c(2,4), mar=c(1.5,2.5,2,1), mgp=c(1.5,.5,0), xpd=NA)
  OMGPlot(file.path(getwd(),"MEFinal"), 81, FALSE, c(1,21),
          Letter=list('A','B'), title=as.list(paste0("Param 1-", c(0, 4000))),
          LetterHeight=-4)
  OMGPlot(file.path(getwd(),"MEFinal"), 129, FALSE, c(1,21),
          Letter=list('E','F'), title=as.list(paste0("Param 17-", c(0, 4000))),
          LetterHeight=-4)
  OMGPlot(file.path(getwd(),"MEFinal"), 83, FALSE, c(1,21),
          Letter=list('C','D'), title=as.list(paste0("Param 3-", c(0, 4000))),
          LetterHeight=-4)
  OMGPlot(file.path(getwd(),"MEFinal"), 131, FALSE, c(1,21),
          Letter=list('G','H'), title=as.list(paste0("Param 19-", c(0, 4000))),
          LetterHeight=-4)
  #AddScale()
dev.off()

#Stitcher(file.path(getwd(),"MEFinal2"))
Data <- LoadData(file.path(getwd(),"MEFinal2", "Stitched"))
Ordered <- order(Data$FinalData2$MDial,
                 as.numeric(Data$FinalData2$Dial),
                 rev(as.logical(Data$FinalData2$Tutor)),
                 rev(as.logical(Data$FinalData2$Vertical)),
                 rev(as.logical(Data$FinalData2$Breed)),
                 rev(as.logical(Data$FinalData2$Uniform)),
                 decreasing = FALSE)
Labels <- list(Label1=paste(c("TRUE", "FALSE")),
               Label2=paste(c("TRUE", "FALSE")), #Tutor
               Label3=paste(c("TRUE", "FALSE")), #Breed
               Label4=paste("Dial", c(1,2,4,8, 16)))
pdf("Overview-Social.pdf", width=14, height=8.5)
  QuadPlots(Labels, Ordered, subset=1:80, FALSE, mar=c(10,3,1,.5), End=TRUE)
  QuadPlots(Labels, Ordered, subset=81:160, FALSE, mar=c(10,3,1,.5), End=TRUE)
  QuadPlots(Labels, Ordered, subset=161:240, FALSE, mar=c(10,3,1,.5), End=TRUE)
dev.off()
#########################
df <- TableData(file.path(getwd(),"MEFinal2"), 240)
Results2 <- cbind.data.frame(Data$FinalData2[1:240,c(12,16, 17, 18, 19, 20)],df)
write.csv(Results2, "TableofMagic2.csv")
pdf("MatchPlotsFull2.pdf", height=11, width=8)
  par(mfrow=c(2,1), mar=c(2.5,2.5,1,1), mgp=c(1.5,.5,0), xpd=NA)
  Establish <- which(Results2$MDial=="None")
  #########################
  plot((1:240)[-Establish], Results2$corner[-Establish]#/(Results$centersd/sqrt(50))
       , type='p', pch=19,
       ylab="Center match for first dialect",
       xlab="Paramater Set",
       ylim=c(0,.9),
       panel.first = {
         segments((1:240)[-Establish], Results$corner[-Establish],
                  (1:240)[-Establish], Results2$corner[-Establish],
                  col="grey70")
       })
  plotMe <- c(which(Results2$WithDial[-Establish] > .1 & (1:240)[-Establish] > 48),
              65, 82, 97, 98, 113, 114, 130, 129, 145, 146)
  points((1:240)[-Establish][plotMe], Results2$corner[-Establish][plotMe],
         pch=21,bg="cadetblue1", cex=.7)
  text(seq(1, 240, by=48)+24, .9, paste0("Dialects-",c(1,2,4,8,16)), font=2, adj=.5)
  #########################
  plot((49:240)[Establish],Results2$farcorner[49:240][Establish], type='p', pch=19,
       ylab="Center match for last dialect",
       xlab="Paramater Set",
       xlim=c(49,240),
       ylim=c(0,.5)
       , panel.first = {
         segments((49:240)[Establish], Results$farcorner[49:240][Establish],
                  (49:240)[Establish], Results2$farcorner[49:240][Establish],
                  col="grey70")
       })
  plotMe <- which(Results2$farcorner[49:240][Establish] > .04)
  points((49:240)[Establish][plotMe], Results2$farcorner[49:240][Establish][plotMe],
         pch=21,bg="cadetblue1", cex=.7)
  text(seq(48, 240, by=48)+24, .5, paste0("Dialects-",c(2,4,8,16)), font=2, adj=.5)
dev.off()


Results2 <- read.csv("TableofMagic2.csv")
TrueEstablish <- which(Results2$MDial == 'None' & Results2$Dial > 1 & Results2$Uniform & !Results2$Vertical)
EstablishSocial <- Results2[TrueEstablish,c('Breed', 'Tutor', 'Dial', 'corner', 'farcorner')]
colnames(EstablishSocial) <- c('Breed', 'Tutor', 'Dialects', 'Corner', 'FarCorner')
Success <- which(EstablishSocial$FarCorner > .05)
EstablishSocial$FarCorner <- round(EstablishSocial$FarCorner, digits=3)
EstablishSocial$FarCorner[Success] <- paste0(EstablishSocial$FarCorner[Success], "*")
rownames(EstablishSocial) <- NULL
print(xtable(EstablishSocial, digits=3))


Results2 <- read.csv("TableofMagic2.csv")
Maintain <- which(Results2$MDial == 'Same' & Results2$Uniform)
MaintainSocial <- Results2[Maintain,c('Breed', 'Tutor', 'Vertical','Dial', 'center', 'edge')]
colnames(MaintainSocial) <- c('Breed', 'Tutor', 'Vertical', 'Dialects', 'Center', 'Edge')
#Success <- which(MaintainSocial$FarCorner > .05)
#MaintainSocial$Center <- round(MaintainSocial$Center, digits=3)
#MaintainSocial$Edge <- round(MaintainSocial$Edge, digits=3)
#EstablishSocial$FarCorner[Success] <- paste0(EstablishSocial$FarCorner[Success], "*")
rownames(MaintainSocial) <- NULL
print(xtable(MaintainSocial, digits=3))






#Stitcher(file.path(getwd(),"NoPrefFinal"))
Data <- LoadData(file.path(getwd(),"NoPrefFinal", "Stitched"))
Ordered <- order(Data$FinalData2$MDial,
                 as.numeric(Data$FinalData2$Dial),
                 rev(as.logical(Data$FinalData2$Tutor)),
                 rev(as.logical(Data$FinalData2$Vertical)),
                 rev(as.logical(Data$FinalData2$Breed)),
                 rev(as.logical(Data$FinalData2$Uniform)),
                 decreasing = FALSE)
Labels <- list(Label1=paste(c("TRUE", "FALSE")),
               Label2=paste(c("TRUE", "FALSE")), #Tutor
               Label3=paste(c("TRUE", "FALSE")), #Breed
               Label4=paste("Dial", c(2,4)))
write.csv(Data$FinalData2[,c(12,16, 17, 18, 19, 20)], "TableofNoPrefParams.csv")
pdf("Overview-NoPref.pdf", width=14, height=8.5)
QuadPlots(Labels, Ordered, subset=1:16, FALSE, mar=c(10,3,1,.5), End=TRUE)
dev.off()
AcrossItterationMatchPlots(file.path(getwd(),"NoPrefFinal"), 16, 'NP')
df <- TableData(file.path(getwd(),"NoPrefFinal"), 16)
Results3 <- cbind.data.frame(Data$FinalData2[,c(12,16, 17, 18, 19, 20)],df)
write.csv(Results3, "TableofMagicNoPref.csv")

Maintain <- Results3[,c("Breed", "Tutor", "Vertical", "Dial", "center", "edge")]
print(xtable(Maintain, digits=3))


pdf("PrefExamples.pdf", width=6, height=1.5)
par(mfrow=c(1,4), mar=c(1.5,2.5,2,1), mgp=c(1.5,.5,0), xpd=NA)
OMGPlot(file.path(getwd(),"NoPrefFinal"), 1, FALSE, c(1,21),
        Letter=list('A','B'), title=as.list(paste0("Param 1-", c(0, 4000))),
        LetterHeight=-4)
OMGPlot(file.path(getwd(),"NoPrefFinal"), 4, FALSE, c(1,21),
        Letter=list('C','D'), title=as.list(paste0("Param 4-", c(0, 4000))),
        LetterHeight=-4)
#AddScale()

dev.off()







#Stitcher(file.path(getwd(),"SizeCheckFinal"))
Data <- LoadData(file.path(getwd(),"SizeCheckFinal", "Stitched"))
Ordered <- order(Data$FinalData2$MDial,
                 as.numeric(Data$FinalData2$Dial),
                 rev(as.logical(Data$FinalData2$Tutor)),
                 rev(as.logical(Data$FinalData2$Vertical)),
                 rev(as.logical(Data$FinalData2$Breed)),
                 rev(as.logical(Data$FinalData2$Uniform)),
                 decreasing = FALSE)
Labels <- list(Label1=paste(c("TRUE", "FALSE")),
               Label2=paste(c("TRUE", "FALSE")), #Tutor
               Label3=paste(c("TRUE", "FALSE")), #Breed
               Label4=paste("Dial", c(2,4)))
write.csv(Data$FinalData2[,c(12,16, 17, 18, 19, 20)], "TableofSizeCheckParams.csv")
pdf("Overview-SizeCheck.pdf", width=14, height=8.5)
QuadPlots(Labels, Ordered, subset=1:2, FALSE, mar=c(10,3,1,.5), End=TRUE)
dev.off()
AcrossItterationMatchPlots(file.path(getwd(),"SizeCheckFinal"), 2, 'SC')
df <- TableData(file.path(getwd(),"SizeCheckFinal"), 2)
Results4 <- cbind.data.frame(Data$FinalData2[,c(12,16, 17, 18, 19, 20)],df)
write.csv(Results4, "TableofMagicSize.csv")

Results4 <- read.csv("TableofMagicSize.csv")
Sizes <- rbind(Results[c(1,49, 97),c(5,9,10)], Results4[,c(5,9,10)])
Sizes['Dim'] <- c('20x20','20x20','20x20','20x40','40x40')
colnames(Sizes) <- c('Dialects', 'Center', 'Edge', 'Dim')
Sizes <- Sizes[,c(1,4,2,3)]
print(xtable(Sizes, digits=3))


pdf("SizeExamples.pdf", width=6, height=1.5)
par(mfrow=c(1,4), mar=c(1.5,2.5,2,1), mgp=c(1.5,.5,0), xpd=NA)
OMGPlot(file.path(getwd(),"MEFinal"), 97, FALSE, c(1,21),
        Letter=list('A','B'), title=as.list(paste0("Param 65-", c(0, 4000))),
        LetterHeight=-4)
OMGPlot(file.path(getwd(),"SizeCheckFinal"), 2, FALSE, c(1,21),
        Letter=list('C','D'), title=as.list(paste0("Param 2-", c(0, 4000))),
        LetterHeight=-8)
dev.off()




#Stitcher(file.path(getwd(),"ShapesFinal"))
Data <- LoadData(file.path(getwd(),"ShapesFinal", "Stitched"))
Ordered <- order(Data$FinalData2$MDial,
                 as.numeric(Data$FinalData2$Dial),
                 rev(as.logical(Data$FinalData2$Tutor)),
                 rev(as.logical(Data$FinalData2$Vertical)),
                 rev(as.logical(Data$FinalData2$Breed)),
                 as.numeric(Data$FinalData2$Rows),
                 decreasing = FALSE)
Labels <- list(Label1=paste(c("TRUE", "FALSE")),#breed #paste(c("4x100", "5x80", "10x40", "20x20")),
               Label2=paste(c("TRUE", "FALSE")), #Vert
               Label3=paste(c("TRUE", "FALSE")), #Tutor
               Label4=paste("Dial", c(2,4)))
write.csv(Data$FinalData2[,c(12,16, 17, 18, 19, 20)], "TableofShapeParams.csv")
pdf("Overview-ShapesFinal.pdf", width=14, height=8.5)
  QuadPlots(Labels, Ordered, subset=1:64, FALSE, mar=c(10,3,1,.5), End=TRUE)
  QuadPlots(Labels, Ordered, subset=65:128, FALSE, mar=c(10,3,1,.5), End=TRUE)
dev.off()
AcrossItterationMatchPlots(file.path(getwd(),"ShapesFinal"), 128, 'Sh')
df <- TableData(file.path(getwd(),"ShapesFinal"), 128)
Results5 <- cbind.data.frame(Data$FinalData2[,c(12,16, 17, 18, 19, 20, 21)],df)
write.csv(Results5, "TableofMagicShape.csv")

Results5 <- read.csv("TableofMagicShape.csv")
TrueEstablish <- which(Results5$MDial == 'None' & Results5$Dial > 1)
EstablishShape <- Results5[TrueEstablish,c('Breed', 'Tutor', 'Vertical','Rows', 'Dial', 'corner', 'farcorner')]
colnames(EstablishShape) <- c('Breed', 'Tutor', 'Vertical', 'Dim', 'Dialects', 'Corner', 'FarCorner')
Success <- which(EstablishShape$FarCorner > .05)
EstablishShape$FarCorner <- round(EstablishShape$FarCorner, digits=3)
EstablishShape$Corner <- round(EstablishShape$Corner, digits=3)
EstablishShape$FarCorner[Success] <- paste0(EstablishShape$FarCorner[Success], "*")
EstablishShape$Dim <- paste0(EstablishShape$Dim, 'x', 400/EstablishShape$Dim)
reordered <- order(EstablishShape$Dialects, rev(as.logical(EstablishShape$Breed)),
                   rev(as.logical(EstablishShape$Tutor)), rev(as.logical(EstablishShape$Vertical)))
EstablishShape <- EstablishShape[reordered,]
rownames(EstablishShape) <- NULL
print(xtable(EstablishShape, digits=3))


Maintain <- which(Results5$MDial == 'Same' & Results5$Dial > 1)
MaintainShape <- Results5[Maintain,c('Breed', 'Tutor', 'Vertical','Rows', 'Dial', 'center', 'edge')]
colnames(MaintainShape) <- c('Breed', 'Tutor', 'Vertical', 'Dim', 'Dialects', 'Center', 'Edge')
#Success <- which(MaintainShape$FarCorner > .05)
#MaintainShape$Center <- round(MaintainShape$Center, digits=3)
#MaintainShape$Edge <- round(MaintainShape$Edge, digits=3)
#MaintainShape$FarCorner[Success] <- paste0(MaintainShape$FarCorner[Success], "*")
MaintainShape$Dim <- paste0(MaintainShape$Dim, 'x', 400/MaintainShape$Dim)
reordered <- order(MaintainShape$Dialects, rev(as.logical(MaintainShape$Breed)),
      rev(as.logical(MaintainShape$Tutor)), rev(as.logical(MaintainShape$Vertical)))
MaintainShape <- MaintainShape[reordered,]
rownames(MaintainShape) <- NULL
print(xtable(MaintainShape, digits=3))


pdf("EstablishShapeExamples.pdf", width=6, height=3)
par(mfrow=c(2,4), mar=c(1.5,2.5,2,1), mgp=c(1.5,.5,0), xpd=NA)
OMGPlot(file.path(getwd(),"ShapesFinal"), 73, FALSE, c(1,21),
        Letter=list('A','B'), title=as.list(paste0("Param 33-", c(0, 4000))),
        LetterHeight=-.3)
OMGPlot(file.path(getwd(),"ShapesFinal"), 89, FALSE, c(1,21),
        Letter=list('C','D'), title=as.list(paste0("Param 34-", c(0, 4000))),
        LetterHeight=-.5)
OMGPlot(file.path(getwd(),"ShapesFinal"), 105, FALSE, c(1,21),
        Letter=list('E','F'), title=as.list(paste0("Param 35-", c(0, 4000))),
        LetterHeight=-1.5)
OMGPlot(file.path(getwd(),"ShapesFinal"), 121, FALSE, c(1,21),
        Letter=list('G','H'), title=as.list(paste0("Param 36-", c(0, 4000))),
        LetterHeight=-4)
#AddScale()
dev.off()

pdf("MainShapeExamples.pdf", width=6, height=3)
par(mfrow=c(2,4), mar=c(1.5,2.5,2,1), mgp=c(1.5,.5,0), xpd=NA)
OMGPlot(file.path(getwd(),"ShapesFinal"), 65, FALSE, c(1,21),
        Letter=list('A','B'), title=as.list(paste0("Param 33-", c(0, 4000))),
        LetterHeight=-.3)
OMGPlot(file.path(getwd(),"ShapesFinal"), 81, FALSE, c(1,21),
        Letter=list('C','D'), title=as.list(paste0("Param 34-", c(0, 4000))),
        LetterHeight=-.5)
OMGPlot(file.path(getwd(),"ShapesFinal"), 97, FALSE, c(1,21),
        Letter=list('E','F'), title=as.list(paste0("Param 35-", c(0, 4000))),
        LetterHeight=-1.5)
OMGPlot(file.path(getwd(),"ShapesFinal"), 113, FALSE, c(1,21),
        Letter=list('G','H'), title=as.list(paste0("Param 36-", c(0, 4000))),
        LetterHeight=-4)
#AddScale()
dev.off()





#Stitcher(file.path(getwd(),"AddCheckFinal"))
Data <- LoadData(file.path(getwd(),"AddCheckFinal", "Stitched"))
Ordered <- order(Data$FinalData2$MDial,
                 as.numeric(Data$FinalData2$Dial),
                 rev(as.logical(Data$FinalData2$Social)),
                 rev(as.logical(Data$FinalData2$Tutor)),
                 rev(as.logical(Data$FinalData2$Breed)),
                 decreasing = FALSE)
Labels <- list(Label1=paste(c("TRUE", "FALSE")),#breed #paste(c("4x100", "5x80", "10x40", "20x20")),
               Label2=paste(c("TRUE", "FALSE")), #Vert
               Label3=paste(c("TRUE", "FALSE")), #Social
               Label4=paste("Dial", c(1,2,4,8,16)))
write.csv(Data$FinalData2[,c(12,16, 17, 18, 19, 20,22)], "TableofAddParams.csv")
pdf("Overview-AddCheckFinal.pdf", width=14, height=8.5)
QuadPlots(Labels, Ordered, subset=1:80, FALSE, mar=c(10,3,1,.5), End=TRUE)
QuadPlots(Labels, Ordered, subset=81:160, FALSE, mar=c(10,3,1,.5), End=TRUE)
QuadPlots(Labels, Ordered, subset=161:240, FALSE, mar=c(10,3,1,.5), End=TRUE)
dev.off()
AcrossItterationMatchPlots(file.path(getwd(),"AddCheckFinal"), 240, 'ADD')
df <- TableData(file.path(getwd(),"AddCheckFinal"), 240)
Results6 <- cbind.data.frame(Data$FinalData2[,c(12,16, 17, 18, 19, 20, 22)],df)
write.csv(Results6, "TableofMagicAdd.csv")

pdf("MatchPlotsAdd.pdf", height=11, width=8)
par(mfrow=c(2,1), mar=c(2.5,2.5,1,1), mgp=c(1.5,.5,0), xpd=NA)
Establish <- which(Results$MDial=="None")
#########################
plot((1:240)[-Establish], Results6$corner[-Establish]#/(Results$centersd/sqrt(50))
     , type='p', pch=19,
     ylab="Center match for first dialect",
     xlab="Paramater Set",
     ylim=c(0,.9),
     panel.first = {
     })
#plotMe <- c(33,34,49,50, 65, 66, 81, 82, 97, 98, 113, 114, 130, 129, 145, 146)
#points((1:240)[-Establish][plotMe], Results6$corner[-Establish][plotMe],
#       pch=21,bg="cadetblue1", cex=.7)
text(seq(1, 240, by=48)+24, .9, paste0("Dialects-",c(1,2,4,8,16)), font=2, adj=.5)
####################
plot((49:240)[Establish],Results6$farcorner[49:240][Establish], type='p', pch=19,
     ylab="Center match for last dialect",
     xlab="Paramater Set",
     xlim=c(49,240),
     ylim=c(0,.5)
     , panel.first = {
     })
#plotMe <- c(1,2,5,6,7,18,21,22,33,34,37,38,49,50,53,54)
#points((49:240)[Establish][plotMe], Results6$farcorner[49:240][Establish][plotMe],
#       pch=21,bg="cadetblue1", cex=.7)
text(seq(48, 240, by=48)+24, .5, paste0("Dialects-",c(2,4,8,16)), font=2, adj=.5)
dev.off()

Results6 <- read.csv("TableofMagicAdd.csv")
TrueEstablish <- which(Results6$MDial == 'None' & Results6$Dial > 1 & Results6$Uniform & !Results6$Vertical)
EstablishAddSocial <- Results6[TrueEstablish,c('Breed', 'Tutor', 'Social', 'Dial', 'corner', 'farcorner')]
colnames(EstablishAddSocial) <- c('Breed', 'Tutor', 'Social', 'Dialects', 'Corner', 'FarCorner')
Success <- which(EstablishAddSocial$FarCorner > .05)
EstablishAddSocial$FarCorner <- round(EstablishAddSocial$FarCorner, digits=3)
EstablishAddSocial$FarCorner[Success] <- paste0(EstablishAddSocial$FarCorner[Success], "*")
rownames(EstablishAddSocial) <- NULL
print(xtable(EstablishAddSocial, digits=3))

Maintain <- which(Results6$MDial == 'Same' & Results6$Uniform & !Results6$Vertical)
MaintainAddSocial <- Results6[Maintain,c('Breed', 'Tutor', 'Social', 'Dial', 'center', 'edge')]
colnames(MaintainAddSocial) <- c('Breed', 'Tutor', 'Social', 'Dialects', 'Center', 'Edge')
#Success <- which(MaintainAddSocial$FarCorner > .05)
#MaintainAddSocial$Center <- round(MaintainAddSocial$Center, digits=3)
#MaintainAddSocial$Edge <- round(MaintainAddSocial$Edge, digits=3)
#MaintainAddSocial$FarCorner[Success] <- paste0(MaintainAddSocial$FarCorner[Success], "*")
rownames(MaintainAddSocial) <- NULL
print(xtable(MaintainAddSocial, digits=3))


pdf("MainSocialAddExamples.pdf", width=6, height=1.5)
par(mfrow=c(1,4), mar=c(1.5,2.5,2,1), mgp=c(1.5,.5,0), xpd=NA)
OMGPlot(file.path(getwd(),"AddCheckFinal"), 49, FALSE, c(1,21),
        Letter=list('A','B'), title=as.list(paste0("Param 9-", c(0, 4000))),
        LetterHeight=-4)
OMGPlot(file.path(getwd(),"AddCheckFinal"), 51, FALSE, c(1,21),
        Letter=list('C','D'), title=as.list(paste0("Param 10-", c(0, 4000))),
        LetterHeight=-4)
#AddScale()
dev.off()


pdf("EstabSocialAddExamples.pdf", width=6, height=1.5)
par(mfrow=c(1,4), mar=c(1.5,2.5,2,1), mgp=c(1.5,.5,0), xpd=NA)
OMGPlot(file.path(getwd(),"AddCheckFinal"), 81, FALSE, c(1,21),
        Letter=list('A','B'), title=as.list(paste0("Param 1-", c(0, 4000))),
        LetterHeight=-4)
OMGPlot(file.path(getwd(),"AddCheckFinal"), 83, FALSE, c(1,21),
        Letter=list('C','D'), title=as.list(paste0("Param 2-", c(0, 4000))),
        LetterHeight=-4)
#AddScale()
dev.off()










#Stitcher(file.path(getwd(),"FinalOneSong"))
Data <- LoadData(file.path(getwd(),"FinalOneSong", "Stitched"))
Ordered <- order(rev(Data$FinalData2$MDial),
                 as.numeric(Data$FinalData2$Dial),
                 rev(as.logical(Data$FinalData2$Breed)),
                 rev(as.logical(Data$FinalData2$Tutor)),
                 rev(as.logical(Data$FinalData2$Vertical)),
                 as.logical(Data$FinalData2$Social),
                 decreasing = FALSE)
Labels <- list(Label1=paste(c("TRUE", "FALSE")),#breed #paste(c("4x100", "5x80", "10x40", "20x20")),
               Label2=paste(c("TRUE", "FALSE")), #Vert
               Label3=paste(c("TRUE", "FALSE")), #
               Label4=paste("Dialects", c(1,2,4)))
write.csv(Data$FinalData2[,c(12,16, 17, 18, 19, 20,22)], "TableofOneSongParams.csv")
pdf("Overview-FinalOneSong-main.pdf", width=4, height=4)
  QuadPlots(Labels, Ordered, subset=seq(1,48, by=2), FALSE, mar=c(10,3,1,.5), End=TRUE,
            Acc=FALSE, Rep=FALSE, LrnThsh=FALSE, mfrow = c(1,1), Color=c("gray90","white"), l=0)
  mtext(side=2, at=c(-.8, -.42, -.13), line=-.45, text=c("Breed", "Tutor", "Vertical"), cex=.7, font=2)
dev.off()
pdf("Overview-FinalOneSong-est.pdf", width=4, height=4)
  QuadPlots(Labels, Ordered, subset=seq(49, 96, by=2), FALSE, mar=c(10,3,1,.5), End=TRUE,
            Acc=FALSE, Rep=FALSE, LrnThsh=FALSE, Format=FALSE, mfrow = c(1,1), Color=c("gray90","white"), l=0)
  mtext(side=2, at=c(-.8, -.42, -.13), line=-.45, text=c("Breed", "Tutor", "Vertical"), cex=.7, font=2)
dev.off()
AcrossItterationMatchPlots(file.path(getwd(),"FinalOneSong"), 96, 'One')
df <- TableData(file.path(getwd(),"FinalOneSong"), 96)
Results7 <- cbind.data.frame(Data$FinalData2[,c(12,16, 17, 18, 19, 20, 22)],df)
write.csv(Results7, "TableofMagicOneSong.csv")

Results7 <- read.csv("TableofMagicOneSong.csv")
Maintain <- which(Results7$MDial == 'Same' & !Results7$Social)
MaintainOne <- Results7[Maintain,c('Breed', 'Tutor', 'Vertical', 'Dial', 'center', 'edge')]
colnames(MaintainOne) <- c('Breed', 'Tutor', 'Vertical', 'Dialects', 'Center', 'Edge')
#Success <- which(MaintainShape$FarCorner > .05)
#MaintainShape$Center <- round(MaintainShape$Center, digits=3)
#MaintainShape$Edge <- round(MaintainShape$Edge, digits=3)
#MaintainShape$FarCorner[Success] <- paste0(MaintainShape$FarCorner[Success], "*")
#MaintainOne <- MaintainOne[reordered,]
rownames(MaintainOne) <- NULL
print(xtable(MaintainOne, digits=3))


EstablishReal <- which(Results7$MDial == 'None' & !Results7$Social & Results7$Dial > 1)
EstablishShape <- Results7[EstablishReal,c('Breed', 'Tutor', 'Vertical', 'Dial', 'corner', 'farcorner')]
colnames(EstablishShape) <- c('Breed', 'Tutor', 'Vertical', 'Dialects', 'Corner', 'FarCorner')
EstablishShape$FarCorner <- round(EstablishShape$FarCorner, digits=3)
Success <- which(EstablishShape$FarCorner >.05)
#MaintainShape$Edge <- round(MaintainShape$Edge, digits=3)
EstablishShape$FarCorner[Success] <- paste0(EstablishShape$FarCorner[Success], "*")
rownames(EstablishShape) <- NULL
print(xtable(EstablishShape, digits=3))


pdf("MainonesongExamples.pdf", width=6, height=1.5)
par(mfrow=c(1,4), mar=c(1.5,2.5,2,1), mgp=c(1.5,.5,0), xpd=NA)
OMGPlot(file.path(getwd(),"FinalOneSong"), 35, FALSE, c(1,21),
        Letter=list('A','B'), title=as.list(paste0("Param 9-", c(0, 4000))),
        LetterHeight=-4)
OMGPlot(file.path(getwd(),"FinalOneSong"), 67, FALSE, c(1,21),
        Letter=list('C','D'), title=as.list(paste0("Param 17-", c(0, 4000))),
        LetterHeight=-4)
#AddScale()
dev.off()

pdf("EstbonesongExamples.pdf", width=6, height=1.5)
par(mfrow=c(1,4), mar=c(1.5,2.5,2,1), mgp=c(1.5,.5,0), xpd=NA)
OMGPlot(file.path(getwd(),"FinalOneSong"), 51, FALSE, c(1,21),
        Letter=list('A','B'), title=as.list(paste0("Param 9-", c(0, 4000))),
        LetterHeight=-4)
OMGPlot(file.path(getwd(),"FinalOneSong"), 83, FALSE, c(1,21),
        Letter=list('C','D'), title=as.list(paste0("Param 17-", c(0, 4000))),
        LetterHeight=-4)
#AddScale()
dev.off()

sort(Data$MBoxer[,49]) #12
sort(Data$MBoxer[,53]) #10
sort(Data$MBoxer[,51]) #13
sort(Data$MBoxer[,55]) #10




plot((49:240)[-Establish], Results$WithDial[49:240][-Establish]#/(Results$centersd/sqrt(50))
     , type='p', pch=19,
     ylab="Center match minus edge match for first dialect",
     xlab="Paramater Set",
     ylim=c(-.2,.4),
     xlim=c(0,240),
     panel.first = {
     })
points((1:240)[-Establish][plotMe], Results$WithDial[-Establish][plotMe],
       pch=21,bg="cadetblue1", cex=.7)
text(seq(49, 240, by=48)+24, .4, paste0("Dialects-",c(2,4,8,16)), font=2, adj=.5)