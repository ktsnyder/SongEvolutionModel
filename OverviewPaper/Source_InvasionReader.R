remove(list=objects())
ReloadParam <- function(Filepath){
  raw <- read.table(Filepath, sep="=", as.is=TRUE)
  Final <- rbind.data.frame(raw$V2, stringsAsFactors=FALSE)
  names(Final) <- raw$V1
  Log <- which(Final %in% c("TRUE", "FALSE"))
  Char <- which(names(Final) == "MDial")
  suppressWarnings(Final[,-c(Char, Log)] <- as.numeric(Final[,-c(Char, Log)]))
  Final[,Log] <- as.logical(Final[,Log])
  return(Final)
}
InvasionData <- function(Dir=getwd(),Group="Closed",Stat=1,NumInvad=1,nFiles=18){
  if(Stat==1){
    StatType="DelayedClosed"
  }else if(as.numeric(Stat)==.25){
    StatType="Closed"
  }else{StatType="Open"}
  if(Group == "Closed"){
    Origin=.25
  }else if(Group =="DelayedClosed"){
    Origin=1
  }else{Origin=2}
  Output <- data.frame(Start=rep(Group, nFiles), ConvertTo = rep(StatType,nFiles),
                       Invaders=rep(NumInvad,nFiles), Preference=character(nFiles),
                       LearningType=character(nFiles),LearningPenalty=numeric(nFiles),
                       PercentConverted = numeric(nFiles),PercentPersisted = numeric(nFiles),
                       AveSteps = numeric(nFiles),
                       stringsAsFactors = FALSE)
  for(i in 1:nFiles){
    Data <- read.csv(file.path(Dir,Group,paste0(Group,"-",Stat,"-",NumInvad),paste0(i,"-",as.numeric(Stat), ".csv")), header=FALSE)
    Output$PercentConverted[i] <- length(which(Data[,2] == as.numeric(Stat)))/nrow(Data)*100
    Output$PercentPersisted[i] <- length(which(Data[,2] != Origin))/nrow(Data)*100
    Output$AveSteps[i] <- mean(Data[,1])
    Semp <- ReloadParam(file.path(Dir,Group, paste0(i,".semp")))
    if(Semp$Consen){
      Output$LearningType[i] = "Consensus"
    }else if(Semp$Forget){
      Output$LearningType[i] = "AddForget"
    }else{Output$LearningType[i] = "Add"}
    if(Semp$MatPref==0){
      Output$Preference[i] = "Repertoire Size"
    }else{Output$Preference[i] = "Match"}
    Output$LearningPenalty[i] = Semp$Lpen
  }
  return(Output)
}
LrnThrshData <- function(Dir=getwd(), Group="Closed",Stat=1,NumInvad=1, file=1){
  if(Stat==1){
    StatType="DelayedClosed"
    second <- Stat
  }else if(as.numeric(Stat)==.25){
    StatType="Closed"
    Stat <- ".25"
    second <- 0.25
  }else{StatType="Open"
  second <- Stat}
  if(Group == "Closed"){
    Origin=.25
  }else if(Group =="DelayedClosed"){
    Origin=1
  }else{Origin=2}
  
  FilePathSemp <- file.path(getwd(),Group,paste0(file,".semp"))
  Semp <- ReloadParam(FilePathSemp)
  if(Semp$Consen){
    LStrat = "Consensus"
  }else if(Semp$Forget){
    LStrat  = "AddForget"
  }else{LStrat = "Add"}
  if(Semp$MatPref==0){
    Pref = "Repertoire Size"
    Color="red"
  }else{Pref = "Match"
  Color="blue"}
  
  FilePathData <- file.path(getwd(),Group,paste0(Group,"-",Stat,"-",NumInvad),paste0(file, "-", second,".csv"))
  LrnData <- as.numeric(read.table(FilePathData, sep=",")[,2])
  if(length(unique(LrnData))>1){
    hist(LrnData,
         main=paste0(Group, " to ", StatType,":\n", Pref, ", ", LStrat),
         xlab="Average Learning Threshold",
         ylab="Number of Simulations",
         xlim=c(0,2), col=Color, breaks=20) 
  }else{
    plot(0,type="n", xlim=c(0,1), ylim=c(0,1),
         yaxt="n", xaxt="n", xlab="", ylab="")
    text(.5,.5, paste0("Only ", unique(LrnData)))
  }
}
InvasionPlot <- function(Lpen,CD,DC,DO,OD,OC,CO,Outcome="Conversion",
                         tag=TRUE, autoformat=TRUE, plot=c(TRUE, TRUE),
                         letter=NA){
  #Figure out what to plot
  if(Outcome=="Conversion"){
    if(tag){
      Tag <- "Conversion, "
    }else{
      Tag <- ''
    }
    Read <- "PercentConverted"
  }else{
    if(tag){
      Tag <- "Persistence, "
    }else{
      Tag <- ''
    }
    Read <- "PercentPersisted"
  }
  index <- which(CD$LearningPenalty==Lpen)
  index <- rbind(index[1:3],index[4:6])
  Type <- c("Repertoire Size", "Match")
  #set up plot
  if(autoformat){
    par(mar=c(1,1,1,1), mgp=c(0,0,0), mfrow=c(2,1))#, bty="n") 
  }
  done <- FALSE
  for(i in 1:2){
    if(!plot[i]){
      next()
    }
    plot(1,type="n",xlim=c(-1,11), ylim=c(1,10.5), yaxt="n", xaxt="n",
         ylab="", xlab="")
    #main labels
    if(!is.na(letter)){
      mtext(las=1, letter, side=2, line=.5, at=10.5, font=2)
    }
    if(!done){
      text(5,10,paste0(Tag,"Learning Penalty: ", Lpen),font=2, cex=1.5)
      done <- TRUE
    }
    Openx <- 10;Closedx <- 0;space <-3
    text(c(Closedx,5,Openx),5,c("Closed", "Delayed\nClosed", "Open"),
         adj=.5, font=2)
    if(tag){
      text(5,10,paste("Preference:", Type[i]), font=2)
    }
    squish <- 1.5
    #arrows between states
    arrows(c(Closedx+1.2,5+squish,5-squish,Openx-1,Closedx,Openx),c(5.2,5.2,4.8,4.8,5-space,5+space),
           c(5-squish,Openx-1,Closedx+1.2,5+squish,Closedx,Openx),c(5.2,5.2,4.8,4.8,4.5,5.5), length=.1)
    segments(c(Closedx,Openx,Closedx,Closedx),c(5+space,5-space,5-space,5+space),c(Closedx,Openx,Openx,Openx),c(5.5,4.5,5-space,5+space))
    
    #Rates
    LTypes=c("Add: ", "AddForget: ", "Consensus: ")
    #main
    OfAr<-.65
    OfSp <- 2.3-OfAr
    shove <- .3
    text(Closedx+4,seq(5+OfAr,5+space-OfSp,length=3)+shove,paste0(LTypes, CD[,Read][index[i,]]), adj=1, cex=.7)
    text(Openx-.7,seq(5+OfAr,5+space-OfSp,length=3)+shove,paste0(LTypes, DO[,Read][index[i,]]), adj=1, cex=.7)
    text(Closedx+4,seq(5-OfAr,5-space+OfSp,length=3)-shove,paste0(LTypes, DC[,Read][index[i,]]), adj=1, cex=.7)
    text(Openx-.7,seq(5-OfAr,5-space+OfSp,length=3)-shove,paste0(LTypes, OD[,Read][index[i,]]), adj=1, cex=.7)
    #large jumps
    text(((Openx-Closedx)/2)+Closedx+.7,seq(5+space+.3,5+space+1,length=3),paste0(LTypes, CO[,Read][index[i,]]), adj=1, cex=.7)
    text(((Openx-Closedx)/2)+Closedx+.7,seq(5-space-.3,5-space-1,length=3),paste0(LTypes, OC[,Read][index[i,]]), adj=1, cex=.7)
  }  
}
