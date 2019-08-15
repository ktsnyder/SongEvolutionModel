library('SEM')
library('numbers')
library('xtable')


#loads in parameter readout from a previous run
Stitcher <- function(Direct, NewFolder="Stitched"){
  Usable <- list.files(Direct)
  semps <- grep(".semp",Usable)
  if(length(semps) > 0){
    Usable <- Usable[-semps]
  }
  Usable2 <- as.numeric(gsub("[[:alpha:][:punct:]]", "", Usable))
  datas <- unique(Usable2)
  dir.create(file.path(Direct,NewFolder))
  for(j in seq_along(datas)){
    readme <- which(Usable2==datas[j])
    data <- as.list(4)
    for(i in 1:4){
      data[[i]] <- read.table(file.path(Direct,Usable[readme[i]]), header=FALSE,
                              stringsAsFactors = FALSE, fileEncoding = "UTF-8-BOM")
        if(i == 3){
          if(is.character(data[[i]][1,])){
            data[[i]] <- sapply(1:1050, function(x) mean(as.numeric(unlist(strsplit(data[[i]][x,], ",")))))
          }
      }
      #data[[i]]<- gsub("﻿", "", unlist(Data[[i]]))
    }
    
    df <- data.frame(matrix(unlist(data), ncol=4))
    Names <- gsub("[.]csv","",Usable[readme])
    Names <- gsub("[[:digit:]]", "", Names)
    colnames(df) <- Names
    write.table(df, paste0(Direct,"/",NewFolder,"/", datas[j],".csv"), row.names = FALSE, sep=",")
    file.rename(file.path(Direct,paste0(datas[j],"Parameters.semp")),file.path(Direct,NewFolder,paste0(datas[j],".semp"))) 
  }  
}
ReloadParam <- function(Filepath){
  raw <- read.table(Filepath, sep="=", as.is=TRUE)
  Final <- rbind.data.frame(raw$V2, stringsAsFactors=FALSE)
  names(Final) <- raw$V1
  Log <- which(Final %in% c("TRUE", "FALSE", "True", "False"))
  Char <- which(names(Final) %in% c("MDial", "ConsenS"))
  suppressWarnings(Final[,-c(Char, Log)] <- as.numeric(Final[,-c(Char, Log)]))
  Final[,Log] <- as.logical(Final[,Log])
  return(Final)
}
MessPlot <- function(Num, Type="LrnThsh"){
  data <- read.table(file.path(getwd(),tag, paste0(Num, ".csv")), header=TRUE, sep=",")
  print(FinalData[2*Num-1,1:7])
  M <- max(data$RepSize)
  plot(data$RepSize[1:interval],xaxt="n", ylab="RepSize", ylim=c(0,M),
       type="l", lwd=2, col=rgb(0,0,0,.4))
  axis(1, 1:21, 0:20*200)
  if(Type == "LrnThsh"){
    segments(1:(interval-1), data$LrnThsh[(1:interval)][1:(interval-1)]*(M/20),
             2:interval, data$LrnThsh[(1:interval)][2:interval]*(M/20),
             col=rgb(1,0,0,.4), pch=20, lwd=2) 
    axis(4, seq(1,M,length=21), 0:20, col="red")
  }else if(Type == "Acc"){
    segments(1:(interval-1), data$Acc[(1:interval)][1:(interval-1)]*(M),
             2:interval, data$Acc[(1:interval)][2:interval]*(M),
             col=rgb(1,0,1,.4), pch=20, lwd=2) 
    axis(4, seq(1,M,length=11), seq(0,1,.1), col="purple")
  }else{
    segments(1:(interval-1), data$Match[(1:interval)][1:(interval-1)]*(M),
             2:interval, data$Match[(1:interval)][2:interval]*(M),
             col=rgb(0,0,1,.4), pch=20, lwd=2) 
    axis(4, seq(1,M,length=11), seq(0,1,.1), col="cadetblue")
  }
  for(i in 1:49){
    start <- i*interval
    segments(1:(interval-1), data$RepSize[(start+1):(start+interval)][1:(interval-1)],
             2:interval, data$RepSize[(start+1):(start+interval)][2:interval],
             pch=20, lwd=2, col=rgb(0,0,0,.4))
    if(Type == "LrnThsh"){
      segments(1:(interval-1), data$LrnThsh[(start+1):(start+interval)][1:(interval-1)]*(M/20),
               2:interval, data$LrnThsh[(start+1):(start+interval)][2:interval]*(M/20),
               col=rgb(1,0,0,.4), lwd=2)
    }else if(Type == "Acc"){
      segments(1:(interval-1), data$Acc[(start+1):(start+interval)][1:(interval-1)]*(M),
               2:interval, data$Acc[(start+1):(start+interval)][2:interval]*(M),
               col=rgb(1,0,1,.4), lwd=2)
    }else{
      segments(1:(interval-1), data$Match[(start+1):(start+interval)][1:(interval-1)]*(M),
               2:interval, data$Match[(start+1):(start+interval)][2:interval]*(M),
               col=rgb(0,0,1,.4), lwd=2)
    }
  }  
}

GetUsableFiles <- function(Direct){
  Usable <- list.files(Direct)
  if(length(Usable) == 0){
    stop("YOU HAVE THE WRONG DIRECTORY!")
  }
  Usable <- unlist(strsplit(Usable, "[.]"))
  Usable <- Usable[seq(1, length(Usable), by=2)]
  Usable <- Usable[!duplicated(Usable)]
  Usable <- sort(as.numeric(Usable))
  return(Usable)  
}

CheckRuns <- function(Direct, type, number, NSims=4000,
                      Freq=200, Repeats=50, ylim=c(0,max(data[type][,]))){
  interval <- (NSims/Freq+1)
  data <- read.table(file.path(Direct, paste0(number, ".csv")), header=TRUE, sep=",", stringsAsFactors = FALSE)
  plot(1:interval, type='n', xaxt='n',
       xlab="Time Steps", ylab=type, ylim=ylim)
  mtext(number, side=2, line=.5, at=ylim[2]*1.1, las=1)
  range <- 1:interval
  one <-1:(interval-1)
  two <- 2:(interval)
  for(i in 1:Repeats){
    subset <- data[type][range+(i-1)*interval,]
    segments(one, subset[one], two, subset[two])
  }
  axis(1, at=1:interval, c(0,cumsum(rep(Freq, interval-1))))
  
}
LoadData <- function(Direct, NSims=4000, Freq=200){
  Usable <- GetUsableFiles(Direct)
  Leng <- length(Usable)*2
  interval <- (NSims/Freq+1)
  LengAll <- length(Usable)*interval
  FinalData <- data.frame(PRepPref=numeric(Leng), PMatchPref=numeric(Leng), PLrnStrtgy=character(Leng),
                          PRepSize=numeric(Leng), PAccrcy=numeric(Leng), PLrningThrshld=numeric(Leng),
                          PLrnPnlty=numeric(Leng), ChanInv=numeric(Leng), ChanFor=numeric(Leng), NumTut=numeric(Leng),
                          AccNoise=numeric(Leng),
                          Vertical=logical(Leng), LisThrsh=numeric(Leng), FthrLisThrsh=numeric(Leng),
                          Main=character(Leng), Breed=logical(Leng), Tutor=logical(Leng),
                          Dial=numeric(Leng), MDial=character(Leng), Uniform=character(Leng),
                          Rows=numeric(Leng), Social=logical(Leng),
                          Acc=numeric(Leng), LrnThsh=numeric(Leng), Match=numeric(Leng), RepSize=numeric(Leng),
                          stringsAsFactors = FALSE)
  RBoxer <- data.frame(matrix(0,nrow=50,ncol=length(Usable)))
  LBoxer <- RBoxer
  ABoxer <- RBoxer
  MBoxer <- RBoxer
  for(i in seq_along(Usable)){
    P <- ReloadParam(file.path(Direct,paste0(Usable[i],".semp")))
    data <- read.table(file.path(Direct, paste0(Usable[i], ".csv")), header=TRUE, sep=",", stringsAsFactors = FALSE)
    if(!("Acc" %in% colnames(data))){
      data["Acc"] <- rep(P$Acc0, length(data[,1]))
    }
    if(!("Match" %in% colnames(data))){
      data["Match"] <- rep(0, length(data[,1]))
    }
    if(!("LrnThrsh" %in% colnames(data))){
      data["LrnThrsh"] <- rep(P$LrnThrsh0, length(data[,1]))
    }
    data <- data[,c("Acc", "LrnThrsh", "Match", "SylRep")]
    Init <- round(colMeans(data[seq(1,nrow(data),interval),]), digits=4)
    End <- round(colMeans(data[seq(interval,nrow(data),interval),]), digits=4)
    
    
    if(P$Consen){
      Strat <- "Consensus"      
    }else if(P$Forget){
      Strat <- "AddForget"
    }else{
      Strat <- "Add"
    }
    
    if(!P$Vert){
      VO <- "Oblique"
    }else if(P$FLisThrsh == .999){
      VO <- "Dad"
    }else{
      VO <- "Mixed"
    }
    
    Param <- c(P$RepPref, P$MatPref, Strat, P$RSize0,
               P$Acc0, P$LrnThrsh0, P$Lpen, P$CtI0, P$CtF0, P$ConNoTut,
               P$IAccN, P$Vert,  P$LisThrsh, P$FLisThrsh, VO,
               P$ScopeB, P$ScopeT, P$Dial, P$MDial, P$UniMat, P$R, P$Social)
    FinalData[i*2-1,] <- c(Param,Init)
    FinalData[i*2,] <- c(Param,End)
    RBoxer[,i] <- data$SylRep[seq(interval,length(data[,1]),interval)]
    LBoxer[,i] <- data$LrnThrsh[seq(interval,length(data[,1]),interval)]
    ABoxer[,i] <- data$Acc[seq(interval,length(data[,1]),interval)]
    MBoxer[,i] <- data$Match[seq(interval,length(data[,1]),interval)]
  }
  FinalData2 <- FinalData[seq(1,nrow(FinalData),2),]
  rownames(FinalData2) <- NULL
  return(list(FinalData=FinalData, FinalData2=FinalData2, RBoxer=RBoxer,
              LBoxer=LBoxer, ABoxer=ABoxer, MBoxer=MBoxer))
}

CombineAll <- function(Data, Order, nCombine=4){
  Data$RBoxer <- Combine(Data$RBoxer, Order, nCombine)
  Data$ABoxer <- Combine(Data$ABoxer, Order, nCombine)
  Data$LBoxer <- Combine(Data$LBoxer, Order, nCombine)
  Data$MBoxer <- Combine(Data$MBoxer, Order, nCombine)
  return(Data)
}
Combine <- function(data, Order, nCombine=4){
  newMat <- matrix(0, nrow=nrow(data)*nCombine, ncol=ncol(data)/nCombine)
  for(i in 1:ncol(newMat)){
    newMat[,i] <- as.vector(unlist(data[Order][,(1 +(nCombine*(i-1))):(nCombine*i)]))
  }
  return(newMat)
}
QuadPlots <- function(Labels, Order, Overview=FALSE, Color=c("azure3","white","azure2"),
                      subset = 1:ncol(Data$RBoxer), BoxWex=.8,
                      mfrow=c(2,2), mar=c(10,2.5,.5,.5), mgp=c(1.5,.5,0),
                      Match=TRUE, Rep=TRUE, Acc=TRUE, LrnThsh=TRUE, End=TRUE, LinePush=-3,
                      Format=TRUE, l=1){
  if(Format){
    par(mfrow=mfrow, mar=mar, mgp=mgp)
  }
  cuts <- rev(sapply(Labels, length))
  if(Rep){
    SweepPlots(Data$RBoxer[,Order[subset]], "Repertoire Size", Labels[[1]],Labels[[2]],
               Labels[[3]], Labels[[4]], cuts, Color=Color, BoxWex=BoxWex, Letter= ifelse(l==0, "",LETTERS[l]), End=End)
    if(Overview){
      ExtraLables(length(subset), max(Data$RBoxer))
    }
    if(l >0){
      l <- l+1
    }
  }
  
  if(LrnThsh){
    SweepPlots(Data$LBoxer[,Order[subset]], "Learning Threshold", Labels[[1]],Labels[[2]],
               Labels[[3]], Labels[[4]], cuts, Top=20, Color=Color, BoxWex=BoxWex, Letter= ifelse(l==0, "",LETTERS[l]), End=End)
    if(l >0){
      l <- l+1
    }
  }
  
  if(Acc){
    SweepPlots(Data$ABoxer[,Order[subset]], "Accuracy", Labels[[1]],Labels[[2]],
               Labels[[3]], Labels[[4]], cuts,Top=1, Color=Color, BoxWex=BoxWex, Letter= ifelse(l==0, "",LETTERS[l]), End=End)
    if(l >0){
      l <- l+1
    }
  }

  if(Match){
    SweepPlots(Data$MBoxer[,Order[subset]], "Matching", Labels[[1]],Labels[[2]],
               Labels[[3]], Labels[[4]], cuts, Top=1, Color=Color, BoxWex=BoxWex, Letter= ifelse(l==0, "",LETTERS[l]), End=End)
    if(Overview){
      rect(6,.37,66, .62, border=NA, col = "white")
      text(36,.5,"No data when\n repertorie prefered.", font=2, cex=1.5)
    }
  }
  
  if(!End){
    mar[3] <- 0
    par(mar=mar)
    plot.new()
    plot.window(xlim=c(0, length(subset)+1), ylim=c(0,1), bg="red", xaxs = "i")

    while(cuts[length(cuts)]==0){
      cuts <- cuts[-length(cuts)]
    }
    CumuCuts <- cumprod(cuts)
    AddLabels(Labels[[1]],Labels[[2]], Labels[[3]], Labels[[4]],
              cuts, CumuCuts, Color, Data$RBoxer[,Order[subset]], endit=TRUE, LinePush)
  }
}
ExtraLables <- function(xlen=144, ylen=330){
  text(LETTERS[1:4], x=seq(2,8,length=4), y=100, cex=.7, font=2)
  segments(seq(2,8,length=4)-.25, 110, 1:4,230)
  Legend <- paste0(LETTERS[1:4],
                  rep(c(": Rep=5",": Rep=20"), each=2),
                  c(", Acc=.5",", Acc=.7"))
  rect(xlen*.7986,ylen*.83,xlen,ylen, col="white")
  text(Legend, x=xlen*.8055, y=seq(ylen*.97,ylen*.8545,length=4), adj=0, font=2, cex=.7)
}
SweepPlots <- function(Data, Type, Label1=NULL, Label2=NULL, Label3=NULL,
                       Label4=NULL, Cuts=c(2,3,3,2), Top=max(Data)*1.04,
                       Color=c("azure3","white","azure2"), BoxWex=.8,
                       Letter='A', End=FALSE){
  plot(0, type="n",ylim=c(0,Top), ylab=Type,
       xlim=c(0, ncol(Data)+1), xaxt="n", xlab="",
       xaxs="i", yaxs="i", font.lab=2)
 
  while(Cuts[length(Cuts)]==0){
    Cuts <- Cuts[-length(Cuts)]
  }
  CumuCuts <- cumprod(Cuts)
  SetRecColors(Data,Cuts,CumuCuts, Color=Color, top=Top)
  
  if(End){
    AddLabels(Label1, Label2, Label3, Label4, Cuts, CumuCuts, Color, Data)
  }  
  boxplot(Data, add=TRUE, xaxt="n", yaxt="n", col="white", pch=20, cex.pch=.01, outcol=rgb(.5,.5,.5,.5),
          boxwex=BoxWex)
  mtext(side=2, Letter, las=1, line=2, at=Top, font=2)
}




AddLabels <- function(Label1=NULL, Label2=NULL, Label3=NULL,
                      Label4=NULL, Cuts=c(2,3,3,2), CumuCuts, Color=c("azure3","white","azure2"),
                      Data, endit=FALSE, LinePush=-3){
  
  TicLen <- -.15
  if(endit){
    adj <- 0
    side <- 3
    flip <- -1
    LinePos <- LinePush
    push <- 2
  }else{
    adj <- 1
    side <- 1
    flip <- 1
    LinePos <- .1
    push <- 0
  }
  Pos <- length(CumuCuts)
  if(length(Label1) != 0){
    Inter <- ncol(Data)/CumuCuts[Pos]
    axis(side,c(0,seq(Inter, ncol(Data),by=Inter))+.5, labels=FALSE, tck=TicLen*flip)
    mtext(Label1,side,line=LinePos,las=2, at=seq(Inter/2,ncol(Data),Inter)+.5, cex=.6, adj=adj)
    Pos <- Pos-1
    TicLen <- TicLen-.15
    LinePos <- LinePos+2.75*flip-push
  }
  if(length(Label2) != 0){
    Inter <- ncol(Data)/CumuCuts[Pos]
    axis(side,c(0,seq(Inter, ncol(Data),by=Inter))+.5, labels=FALSE, tck=TicLen*flip)
    mtext(Label2,side,line=LinePos,las=2, at=seq(Inter/2,ncol(Data),Inter)+.5, cex=.8, adj=adj)
    Pos <- Pos-1
    TicLen <- TicLen-.15
    LinePos <- LinePos+3.75*flip
  }
  if(length(Label3) != 0){
    Inter <- ncol(Data)/CumuCuts[Pos]
    axis(side,c(0,seq(Inter, ncol(Data),by=Inter))+.5, labels=FALSE, tck=TicLen*flip)
    mtext(Label3,side,line=LinePos,las=1, at=seq(Inter/2,ncol(Data),Inter)+.5)
    Pos <- Pos-1
    TicLen <- TicLen-.25
    LinePos <- LinePos+1.5*flip
  }
  if(length(Label4) != 0){
    Inter <- ncol(Data)/CumuCuts[Pos]
    axis(side,c(0,seq(Inter, ncol(Data),by=Inter))+.5, labels=FALSE, tck=TicLen*flip)
    mtext(Label4,side,line=LinePos,las=1, at=seq(Inter/2,ncol(Data),Inter)+.5)
  }
}
SetRecColors <- function(Data, cuts, cumcut, Color=c("azure3","white","azure2"),
                         total=(ncol(Data)+1)/multi, top=max(Data)){
  Boxes<-cuts[1]*cuts[2]
  Lines <- cumcut[length(cumcut)]
  Width <- (ncol(Data))
  BoxWidth <- Width/Boxes
  LineWidth <- Width/Lines
  for(i in 1:Boxes){
    rect(BoxWidth*(i-1)+.5,0,BoxWidth*i+.5, top,
         col=Color[i%%length(Color)+1], border=NA)
  }
  abline(v=c(.5,LineWidth*(1:Lines)+.5), col=rgb(0,0,0,.2))
  abline(v=BoxWidth*(1:(Boxes-1))+.5, col=rgb(0,0,0,1))
  abline(h=top)
  abline(v=Width+1)
}

HeatPlot <- function(Data, XY=c("PLrnPnlty","PRepPref"), Type="RepSize", Name="Repertoire Size",
                     XYLab=c("Learning Penalty", "Repertoire Size Preference"),
                    Between=TRUE, Within=TRUE, Mfrow=c(3,2), Add=FALSE,
                    Letters = NA){
  if(!Between || !Within){
    Tag<- FALSE
  }else{
    Tag <- TRUE
  }
  LStyle <- c("Add", "Add/Forget", "Consensus")
  if(!Add){
    par(mfrow=Mfrow, mar=c(3.5,3,3,4), mgp=c(1.5,.5,0), xpd=NA)
  }
  X <- seq(min(Data$FinalData[,XY[1]]),max(Data$FinalData[,XY[1]]),length=10)
  X <- c(X,X[10]+(X[2]-X[1]))-.5*(X[2]-X[1])
  Y <- seq(max(Data$FinalData[,XY[2]]),min(Data$FinalData[,XY[2]]),length=10)
  Y <- c(Y[1]+(Y[9]-Y[10]),Y)-.5*(Y[9]-Y[10])
  Trait <- as.numeric(Data$FinalData[,Type][seq(2,600,by=2)])
  Breaks <- hist(Trait,breaks=15, plot=FALSE)$breaks[-1]
  ColorScale <- seq(1,0,length=length(Breaks))
  lInd <- 1
  for(i in 1:3){
    Use <- Trait[(1+((i-1)*100)):(i*100)]
    UseBetween <- numeric(length(Use))
    for(j in rev(seq_along(Breaks))){
      UseBetween[which(Use<Breaks[j])] <- ColorScale[j]
    }
    #if(length(Maxed)!=0 || length(Minned)!=0){
    #  Use[-c(Maxed,Minned)] <- Use[-c(Maxed,Minned)]/(Mean+StDev)
    #}else{
    #  Use <- Use/(Mean+StDev)
    #}
    #if(length(Maxed)!=0){Use[Maxed] <- 1}
    #if(length(Minned)!=0){Use[Minned] <- 0}

    BreaksWithin <- hist(Use,breaks=15, plot=FALSE)$breaks[-1]
    ColorScaleWithin <- seq(1,0,length=length(BreaksWithin))
    UseWithin <- numeric(length(Use))
    for(j in rev(seq_along(BreaksWithin))){
      UseWithin[which(Use<BreaksWithin[j])] <- ColorScaleWithin[j]
    }
    #Mean <- mean(Use2)
    #StDev <- 2*sd(Use2)
    #Maxed <- which(Use2 > Mean+StDev)
    #Minned <- which(Use2 < Mean-StDev)
    #if(length(Maxed)!=0 || length(Minned)!=0){
    #  Use2[-c(Maxed,Minned)] <- Use2[-c(Maxed,Minned)]/(Mean+StDev)  
    #}else{
    #  Use2 <- Use2/(Mean+StDev)
    #}
    #if(length(Maxed)!=0){Use2[Maxed] <- 1}
    #if(length(Minned)!=0){Use2[Minned] <- 0}
    #Use2 <- 1-Use2
    
    #Between
    if(Between){
      HeatBetween(X, Y, XY, XYLab, LStyle, Name, Data, UseBetween, Breaks,
                  ColorScale, Tag, i)
      if(length(Letters)>0){
        mtext(Letters[lInd], side=2, las=1, at=1.1, line=1.5)
        lInd <- lInd + 1
      }
    }
    if(Within){
      HeatWithin(X, Y, XY, XYLab, LStyle, Name, Data, UseWithin,
                  BreaksWithin, ColorScaleWithin, Tag, i)
      if(length(Letters)>0){
        mtext(Letters[lInd], side=2, las=1, at=1.1, line=1.5)
        lInd <- lInd + 1
      }
    }
  }
   
}


HeatBetween <- function(X, Y, XY, XYLab, LStyle, Type, Data, UseBetween, Breaks,
                        ColorScale, Tag, i){
  if(Tag){
    addTag <- "\nWithin"
  }else{
    addTag <- ""
  }
  plot(1, type="n", xlim=c(min(X),max(X)),ylim=c(min(Y),max(Y)),
      ylab=XYLab[2], xlab=XYLab[1],
      xaxs="i",yaxs="i", xaxt="n", yaxt="n",
      main=paste0(LStyle[i]," - ",Type, addTag),
      cex.main=1, font.lab=2)
  axis(1,round(as.numeric(unique(Data$FinalData[,XY[1]])),digits=2), cex.axis=.8)
  axis(2,round(as.numeric(unique(Data$FinalData[,XY[2]])),digits=2), cex.axis=.8)
  for(j in 1:10){
    rect(X[1:(length(X)-1)],Y[j],
         X[2:length(X)], Y[j+1],
         col=rgb(UseBetween,UseBetween,UseBetween,1)[(1+((j-1)*10)):(j*10)])  
  }
  Range <- seq(min(Y),max(Y),length=length(Breaks)+1)
  rect(max(X)*1.01,Range[1:length(Breaks)],max(X)*1.05,Range[2:length(Range)],
       col=rgb(ColorScale,ColorScale,ColorScale,1))
  text(max(X)*1.06, c(min(Y),max(Y)),
       c(paste("<",Breaks[1]),Breaks[length(Breaks)]),adj=0)
  if(Type == "Matching"){
    text(as.numeric(unique(Data$FinalData[,XY[1]])), 1, "NA") 
  }
  
}
HeatWithin <- function(X, Y, XY, XYLab, LStyle, Type, Data, UseWithin,
                       BreaksWithin, ColorScaleWithin, Tag, i){
  #Within
  if(Tag){
    addTag <- "\nWithin"
  }else{
    addTag <- ""
  }
  
  plot(1, type="n", xlim=c(min(X),max(X)),ylim=c(min(Y),max(Y)),
       ylab=XYLab[2], xlab=XYLab[1],
       xaxs="i",yaxs="i", xaxt="n", yaxt="n",
       main=paste0(LStyle[i]," - ",Type,addTag),
       cex.main=1, font.lab=2)
  axis(1,round(as.numeric(unique(Data$FinalData[,XY[1]])),digits=2), cex.axis=.8)
  axis(2,round(as.numeric(unique(Data$FinalData[,XY[2]])),digits=2), cex.axis=.8)
  for(j in 1:10){
    rect(X[1:(length(X)-1)],Y[j],
         X[2:length(X)], Y[j+1],
         col=rgb(UseWithin,UseWithin,UseWithin,1)[(1+((j-1)*10)):(j*10)])  
  }
  Range <- seq(min(Y),max(Y),length=length(BreaksWithin)+1)
  rect(max(X)*1.01,Range[1:length(BreaksWithin)],max(X)*1.05,Range[2:length(Range)],
       col=rgb(ColorScaleWithin, ColorScaleWithin,ColorScaleWithin,1))
  text(max(X)*1.06, c(min(Y),max(Y)),
       c(paste("<",BreaksWithin[1]),BreaksWithin[length(BreaksWithin)]),adj=0)
  if(Type == "Matching"){
    text(as.numeric(unique(Data$FinalData[,XY[1]])), 1, "NA") 
  }
}


getAverageMatch <- function(P, data, rep=50, freq=200, ave=TRUE){
  Interval <- P$nSim/freq+1
  BaseLocation <- 0:(rep-1)*(P$numBirds*Interval)
  data <- t(as.matrix(data))
  return(lapply(0:(Interval-1), function(i)
    matrix(sapply(1:P$numBirds,
                  function(j) 
                    if(ave){mean(data[BaseLocation+j+P$numBirds*i])
                    }else{
                      sd(data[BaseLocation+j+P$numBirds*i])
                    }),
           ncol=1))
  )
}

TableData <- function(path, N){
  df <- data.frame(corner=numeric(N), center=numeric(N),
                   edge=numeric(N), farcorner=numeric(N))
  for(omg in 1:N){
    P <- ReloadParam(paste0(path, "/Stitched/", omg, '.semp'))
    MData <- read.csv(paste0(path, "/", omg, "Match.csv"),
                      header=FALSE, fileEncoding = "UTF-8-BOM")
    Averages <- getAverageMatch(P, MData)
    #Stdev <- getAverageMatch(P, MData, ave=FALSE)
    
    Div <- GetDivisors(P) 
    height <- P$R/Div[1]
    width <- P$C/Div[2]
    
    corner <- mean(Averages[[21]][c(1,2,P$R+1)])
    centerindex <- P$R*(floor(width/2)-1) + floor(height/2)
    center <- mean(Averages[[21]][c(centerindex,centerindex+1,centerindex+P$R, centerindex+P$R+1)])
    #centersd <- Stdev[[21]][P$R*(floor(width/2)) + floor(height/2)]
    edgeindex <- P$R*(width-1) + floor(height/2)
    edge <- mean(Averages[[21]][c(edgeindex,edgeindex-1, edgeindex+1, edgeindex+2)])
    #node <- Averages[[21]][P$R*(width-1)+height]
    farindex <- P$C*P$R
    farcorner <- mean(Averages[[21]][c(farindex, farindex-1, farindex-P$R)])
    #CrossDial <- corner-farcorner
    #WithDial <- center-edge
    df[omg,] <- c(corner, center, edge, farcorner)
  }
  return(df)
}

AcrossItterationMatchPlots <- function(path, N, where){
  if(length(N) == 1){
    Runs <- 1:N
  }else{
    Runs <- N  
  }
  
  for(omg in Runs){
   OMGPlot(path, omg, where=where) 
  }  
}

OMGPlot<- function(path, omg, plot=TRUE, steps=1:21, where='',
                   title=NULL, xlab="", ylab="", font.lab=1, Letter=NULL,
                   LetterLine=1.25, LetterHeight=-1){
  if(is.null(title)){
    title <- as.list(rep('',length(steps)))
  }
  if(is.null(Letter)){
    Letter <- as.list(rep('',length(steps)))
  }
  P <- ReloadParam(paste0(path, "/Stitched/", omg, '.semp'))
  MData <- read.csv(paste0(path, "/", omg, "Match.csv"),
                    header=FALSE, fileEncoding = "UTF-8-BOM")
  Averages <- getAverageMatch(P, MData)
  Averages[[21]]
  if(plot){
    pdf(paste0('omg',where,'/', omg, '.pdf'))
    par(mfrow=c(3,3), mar=c(2.5,2.5,1,1), mgp=c(1.5,.5,0))
  }
  for(i in seq_along(steps)){
    TerritoryHeatMap(P,1,Averages[[steps[i]]],1,
                     title[[i]], xlab, ylab, font.lab, Letter[[i]],
                     LetterLine, LetterHeight)
  }
  if(plot){
    dev.off()  
  }
}
AddScale <- function(){
  rect(rep(21.4,11),seq(0,18,length=11)+.5,
       rep(23,11),seq(2,20,length=11)+.5,
       border = "black", col=paste0("grey", seq(10,100,length=11)))
  text(rep(24.5,10),seq(1.5,19.5,length=11),
       seq(1, 0, by=-.1),
       adj=0, cex=.8)
  segments(rep(23, 11), seq(1.5,19.5,length=11),
           rep(24, 11), seq(1.5,19.5,length=11))  
}
