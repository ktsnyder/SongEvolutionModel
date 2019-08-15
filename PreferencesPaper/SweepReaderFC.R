setwd("D:/Prelim2")
rm(list=objects())
source("Source_SweepSquareReaderFC.R")
#install_github("CreanzaLab/SongEvolutionModel/R-Package", force=TRUE, ref="FemalChoiceExpansion")




#Stitcher(file.path(getwd(),"StaticFinal"))
#Stitcher(file.path(getwd(),"SkipFinal"))
Data <- LoadData(file.path(getwd(),"StaticFinal", "Stitched"), 2000, 20)
Ordered <- order(Data$FinalData2$Preference,
                 Data$FinalData2$PLrnStrtgy,
                 rev(as.logical(Data$FinalData2$Vertical)),
                 rev(as.logical(Data$FinalData2$Social)),
                 decreasing = FALSE)
Labels <- list(Label1=paste(c("TRUE", "FALSE")),
               Label2=paste(c("TRUE", "FALSE")), #Tutor
               Label3=levels(as.factor(Data$FinalData2$PLrnStrtgy)),
               Label4="lewl")
write.csv(Data$FinalData2[,], "TableofKeyParams.csv")
pdf("Common.pdf", width=6, height=8.5)
  Labels[['Label4']] <- levels(as.factor(Data$FinalData2$Preference))[1]
  QuadPlots(Labels, Ordered, subset=1:24, FALSE, mar=c(0,3,1.5,.5), End=FALSE, mfrow=c(5,1),
            Color = c(rgb(1,1,1), rgb(1,1,1),rgb(.85,.95,1), rgb(.8,.9,.95),
                      rgb(.75,.85,.9), rgb(.7,.8,.85),rgb(.65,.75,.8) ))
  mtext(side=2, at=c(.5, .9), line=-.45, text=c("Vertical","Social"), cex=.7, font=2)
dev.off()
pdf("Match.pdf", width=6, height=8.5)
  Labels[['Label4']] <- levels(as.factor(Data$FinalData2$Preference))[2]
  QuadPlots(Labels, Ordered, subset=25:48, FALSE, mar=c(0,3,1.5,.5), End=FALSE, mfrow=c(5,1),
            Color = c(rgb(1,1,1), rgb(1,1,1),rgb(.85,.95,1), rgb(.8,.9,.95),
                      rgb(.75,.85,.9), rgb(.7,.8,.85),rgb(.65,.75,.8) ))
  mtext(side=2, at=c(.5, .9), line=-.45, text=c("Vertical","Social"), cex=.7, font=2)
dev.off()
pdf("Noise.pdf", width=6, height=8.5)
  Labels[['Label4']] <- levels(as.factor(Data$FinalData2$Preference))[3]
  QuadPlots(Labels, Ordered, subset=49:72, FALSE, mar=c(0,3,1.5,.5), End=FALSE, mfrow=c(5,1),
            Color = c(rgb(1,1,1), rgb(1,1,1),rgb(.85,.95,1), rgb(.8,.9,.95),
                      rgb(.75,.85,.9), rgb(.7,.8,.85),rgb(.65,.75,.8) ))
  mtext(side=2, at=c(.5, .9), line=-.45, text=c("Vertical","Social"), cex=.7, font=2)
dev.off()
pdf("Rare.pdf", width=6, height=8.5)
  Labels[['Label4']] <- levels(as.factor(Data$FinalData2$Preference))[4]
  QuadPlots(Labels, Ordered, subset=73:96, FALSE, mar=c(0,3,1.5,.5), End=FALSE, mfrow=c(5,1),
            Color = c(rgb(1,1,1), rgb(1,1,1),rgb(.85,.95,1), rgb(.8,.9,.95),
                      rgb(.75,.85,.9), rgb(.7,.8,.85),rgb(.65,.75,.8) ))
  mtext(side=2, at=c(.5, .9), line=-.45, text=c("Vertical","Social"), cex=.7, font=2)
dev.off()
pdf("Rep.pdf", width=6, height=8.5)
  Labels[['Label4']] <- levels(as.factor(Data$FinalData2$Preference))[5]
  QuadPlots(Labels, Ordered, subset=97:120, FALSE, mar=c(0,3,1.5,.5), End=FALSE, mfrow=c(5,1),
            Color = c(rgb(1,1,1), rgb(1,1,1),rgb(.85,.95,1), rgb(.8,.9,.95),
                      rgb(.75,.85,.9), rgb(.7,.8,.85),rgb(.65,.75,.8) ))
  mtext(side=2, at=c(.5, .9), line=-.45, text=c("Vertical","Social"), cex=.7, font=2)
dev.off()
pdf("SexySyls.pdf", width=6, height=8.5)
  Labels[['Label4']] <- levels(as.factor(Data$FinalData2$Preference))[6]
  QuadPlots(Labels, Ordered, subset=121:144, FALSE, mar=c(0,3,1.5,.5), End=FALSE, mfrow=c(5,1),
            Color = c(rgb(1,1,1), rgb(1,1,1),rgb(.85,.95,1), rgb(.8,.9,.95),
                      rgb(.75,.85,.9), rgb(.7,.8,.85),rgb(.65,.75,.8) ))
  mtext(side=2, at=c(.5, .9), line=-.45, text=c("Vertical","Social"), cex=.7, font=2)
dev.off()
pdf("Social.pdf", width=6, height=8.5)
  Labels[['Label4']] <- levels(as.factor(Data$FinalData2$Preference))[7]
  QuadPlots(Labels, Ordered, subset=145:168, FALSE, mar=c(0,3,1.5,.5), End=FALSE, mfrow=c(5,1),
            Color = c(rgb(1,1,1), rgb(1,1,1),rgb(.85,.95,1), rgb(.8,.9,.95),
                      rgb(.75,.85,.9), rgb(.7,.8,.85),rgb(.65,.75,.8) ))
  mtext(side=2, at=c(.5, .9), line=-.45, text=c("Vertical","Social"), cex=.7, font=2)
dev.off()

path <- file.path(getwd(), "StaticFinal") 

for(i in 1:168){
  if(file.exists(file.path(path, paste0(i, "MSong.csv")))){
    P <- ReloadParam(file.path(path, "Stitched", paste0(i, ".semp")))
    Songs <- read.csv(file.path(path, paste0(i, "MSong.csv")),
                      header=FALSE, fileEncoding = "UTF-8-BOM")
    Songs <- getAverageSong(P, Songs)
    Songs <- matrix(unlist(Songs), ncol=101, nrow=P$MaxRSize)
    pdf(paste0("Songs/Songer-",i, ".pdf"))
      SongPlot(P,Songs, thin=1)
    dev.off()    
  }
}

SyllableFrequency <- data.frame(Template=numeric(168),
                                RareTemplate=numeric(168),
                                NonTemplate=numeric(168))
for(i in 1:168){
  if(file.exists(file.path(path, paste0(i, "MSong.csv")))){
    P <- ReloadParam(file.path(path, "Stitched", paste0(i, ".semp")))
    Songs <- read.csv(file.path(path, paste0(i, "MSong.csv")),
                      header=FALSE, fileEncoding = "UTF-8-BOM")
    Songs <- getAverageSong(P, Songs)
    Songs <- matrix(unlist(Songs), ncol=101, nrow=P$MaxRSize)
    SyllableFrequency[i,] <-c(mean(Songs[1:5,101])/P$numBirds,
                              mean(Songs[6:7,101])/P$numBirds,
                              mean(Songs[8:500,101])/P$numBirds)  
  }
}
SyllableFrequency[,'Temp-nonTemp'] <-SyllableFrequency$Template - SyllableFrequency$NonTemplate
par(mfrow=c(3,1), mgp=c(1.5, .5, 0), mar=c(2.5,2.5,1,1))
plot(1:168,SyllableFrequency$Template[Ordered], type='b', pch=19, ylim=c(0,1), cex=.6,
     lty=6,
     panel.first = {abline(v=seq(24, 168, by=25)+.5, col="grey80")})
plot(1:168,SyllableFrequency$NonTemplate[Ordered], type='b', pch=19, ylim=c(0,1), cex=.6,
     lty=6,
     panel.first = {abline(v=seq(24, 168, by=25)+.5, col="grey80")})
plot(1:168,SyllableFrequency$`Temp-nonTemp`[Ordered], type='b', pch=19, ylim=c(-.2,1), cex=.6,
     lty=6,
     panel.first = {abline(v=seq(24, 168, by=25)+.5, col="grey80")})
Results <- read.csv("TableofMagic.csv")
