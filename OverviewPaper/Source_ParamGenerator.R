DefineParameters <- function(Rows=20, Cols=20, Steps=1,
                             InitialSylRepSize=5, PrcntSylOverhang=.2, MaxSylRepSize=500,
                             InitialAccuracy=.7, InherAccuracyNoise=.15,  AccuracyLimits=c(0,1),
                             MaxAge=20, InitialLearningThreshold=2, InherLearningNoise=.25, LearningLimits=c(0,MaxAge),
                             InitialChancetoInvent=.1, InherChancetoInventNoise=0, ChancetoInventLimits=c(0,1),
                             InitialChancetoForget=.2, InherChancetoForgetNoise=0, ChancetoForgetLimits=c(0,1),
                             ListeningThreshold=7, FatherListeningThreshold=.999, MinLearnedSyls=7,
                             EncounterSuccess=.95, LearningPenalty=.75, AgeDeath=TRUE,
                             PrcntRandomDeath=.1, DeathThreshold=1, ChickSurvival=.3,
                             LocalBreed=FALSE, LocalTutor=FALSE, LearnerStrategy="Add",
                             ConsensusNoTut=8, ConsensusStrategy="Conform",
                             OverLearn=FALSE, OverLearnNoTut=3, VerticalLearnCutOff=.25,
                             ObliqueLearning=TRUE, VerticalLearning=TRUE,
                             RepSizePrefer=1, LogScale=TRUE, MatchPrefer=0, UniformMatch=TRUE, MatchScale=1,
                             Dialects=1, MaleDialects="None", FemaleEvolve=FALSE, ChooseMate=FALSE,
                             SaveMatch=NA, SaveAccuracy=NA, SaveLearningThreshold=NA, SaveChancetoInvent=NA, SaveChancetoForget=NA,
                             SaveNames=FALSE, SaveAge=FALSE, SaveMaleSong=FALSE, SaveFemaleSong=FALSE,
                             numSim=1000, Seed=NA){
  numBirds <- Rows*Cols
  if(AgeDeath){
    InitProp <- CalculateProportion(numBirds, DeathThreshold, ChickSurvival, MaxAge)
  }else{
    InitProp <- 0
  }
  
  if((LearnerStrategy %in% c("Add", "AddForget", "Forget", "Consensus")) == FALSE){
    stop("LearnerStrategy must be either Add, Forget, AddForget, or Consensus.")
  }
  if(LearnerStrategy == "Consensus"){
    Consensus=TRUE
    Add=TRUE
    Forget=TRUE
  }else{
    Consensus=FALSE
    if(LearnerStrategy %in% c("Add", "AddForget")){
      Add=TRUE
    }else{Add=FALSE}
    if(LearnerStrategy %in% c("AddForget", "Forget")){
      Forget=TRUE
    }else{Forget=FALSE}
  }
  
  #match the saves
  SaveMatch <- TestRequirement(SaveMatch, MatchPrefer, FemaleEvolve)
  SaveAccuracy <- TestRequirement(SaveAccuracy, InherAccuracyNoise)
  SaveLearningThreshold <- TestRequirement(SaveLearningThreshold, InherLearningNoise)
  SaveChancetoInvent <- TestRequirement(SaveChancetoInvent, InherChancetoInventNoise)
  SaveChancetoForget <- TestRequirement(SaveChancetoForget, InherChancetoForgetNoise)
  
  Parameters <- data.frame(R=Rows, C=Cols, numBirds=numBirds, Steps=Steps,
                           RSize0=InitialSylRepSize, PerROh=PrcntSylOverhang, MaxRSize=MaxSylRepSize,
                           Acc0=InitialAccuracy, IAccN=InherAccuracyNoise,
                           MinAcc=AccuracyLimits[1], MaxAcc=AccuracyLimits[2],
                           MAge=MaxAge, LrnThrsh0=InitialLearningThreshold, ILrnN=InherLearningNoise,
                           MinLrn=LearningLimits[1], MaxLrn=LearningLimits[2],
                           CtI0=InitialChancetoInvent, ICtIN=InherChancetoInventNoise,
                           MinCtI=ChancetoInventLimits[1], MaxCtI=ChancetoInventLimits[2],
                           CtF0=InitialChancetoForget, ICtFN=InherChancetoForgetNoise,
                           MinCtF=ChancetoForgetLimits[1], MaxCtF=ChancetoForgetLimits[2],
                           LisThrsh=ListeningThreshold, FLisThrsh=FatherListeningThreshold,
                           MinLrnSyl=MinLearnedSyls, EnSuc=EncounterSuccess,
                           Lpen=LearningPenalty, DStrat=AgeDeath, PDead=PrcntRandomDeath,
                           DeadThrsh=DeathThreshold, Pc=ChickSurvival, InitProp=InitProp,
                           ScopeB=LocalBreed, ScopeT=LocalTutor,
                           Consen=Consensus, ConsenS=ConsensusStrategy, Add=Add, Forget=Forget,
                           ConNoTut=ConsensusNoTut, OvrLrn=OverLearn, OLNoTut=OverLearnNoTut,
                           Obliq=ObliqueLearning, Vert=VerticalLearning,
                           VertLrnCut=VerticalLearnCutOff,
                           RepPref=RepSizePrefer, LogScl=LogScale, MatPref=MatchPrefer,
                           NoisePref=1-(RepSizePrefer + MatchPrefer), UniMat=UniformMatch, MScl=MatchScale,
                           Dial=Dialects, MDial=MaleDialects, FEvo=FemaleEvolve, ChoMate=ChooseMate,
                           SMat=SaveMatch, SAcc=SaveAccuracy, SLrn=SaveLearningThreshold,
                           SCtI=SaveChancetoInvent, SCtF=SaveChancetoForget, SNam=SaveNames,
                           SAge=SaveAge, SMSng=SaveMaleSong, SFSng=SaveFemaleSong,
                           SimStep=1, nSim=numSim, Seed=Seed, stringsAsFactors=FALSE)
  P <- CheckP(Parameters)
  return(Parameters)
}
CheckP <- function(P){
  #test if ints within min/max range
  CheckMinMaxInt(P$R, "Rows", 3)
  CheckMinMaxInt(P$C, "Cols", 3)
  CheckMinMaxInt(P$nSim,"numSim", 1)
  CheckMinMaxInt(P$Steps, "Steps", 1)
  CheckMinMaxInt(P$RSize0, "InitialSylRepSize", 1)
  CheckMinMaxInt(P$MaxRSize, "InitialSylRepSize", 1)
  CheckMinMaxInt(P$PerROh, "PrcntSylOverhang", 0, int=FALSE)
  CheckMinMaxInt(P$MAge, "MaxAge", 3, P$MAge, TRUE)
  CheckMinMaxInt(P$MinLrnSyl, "MinLearnedSyls", 0, P$MaxRSize, TRUE)
  CheckMinMaxInt(P$Lpen,"LearningPenalty", 0, int=FALSE)
  CheckMinMaxInt(P$EnSuc,"EncounterSuccess", 0, 1, TRUE, FALSE)
  CheckMinMaxInt(P$OLNoTut,"OverLearnNoTut", 1)
  CheckMinMaxInt(P$ConNoTut,"ConsensusNoTut", 2)
  CheckMinMaxInt(P$VertLrnCut,"VerticalLearnCutOff", 0, 1, TRUE, FALSE)
  CheckMinMaxInt(P$PDead,"PrcntRandomDeath", .01, .9, TRUE, FALSE)
  CheckMinMaxInt(P$RepPref, "RepSizePrefer", 0, 1, TRUE, FALSE)
  CheckMinMaxInt(P$MatPref, "MatchPrefer", 0, 1, TRUE, FALSE)
  CheckMinMaxInt(P$DeadThrsh,"DeathThreshold", .0001, .2*P$numBirds, TRUE, FALSE)
  CheckMinMaxInt(P$Pc,"ChickSurvival", .1, 1, TRUE, FALSE)
  
  #make sure bools are bool
  CheckBool(P$OvrLrn,"OverLearn")
  CheckBool(P$DStrat,"AgeDeath")
  CheckBool(P$SMat,"SaveMatch", TRUE)
  CheckBool(P$SAcc,"SaveAccuracy", TRUE)
  CheckBool(P$SLrn,"SaveLearningThreshold", TRUE)
  CheckBool(P$SCtF,"SaveChancetoForget", TRUE)
  CheckBool(P$SCtI,"SaveChancetoInvent", TRUE)
  CheckBool(P$SNam,"SaveNames")
  CheckBool(P$SAge,"SaveAge")
  CheckBool(P$SMSng,"SaveMaleSong")
  CheckBool(P$SFSng,"SaveFemaleSong")
  CheckBool(P$FEvo,"FemaleEvolve")
  CheckBool(P$ChoMate,"ChooseMate")
  CheckBool(P$LogScl,"LogScale")
  CheckBool(P$UniMat, "UniformMatch")
  CheckBool(P$ScopeB,"LocalBreed")
  CheckBool(P$ScopeT,"LocalTutor")
  CheckBool(P$Vert, "VerticalLearning")
  
  
  #make sure all of the trait vals line up
  CheckTrait(P$Acc0, P$IAccN, P$MinAcc, P$MaxAcc, "Accuracy")
  CheckTrait(P$LrnThrsh0, P$ILrnN, P$MinLrn, P$MaxLrn, "Learning Threshold", P$MAge)
  CheckTrait(P$CtI0, P$ICtIN, P$MinCtI, P$MaxCtI, "Chance Invent")
  CheckTrait(P$CtF0, P$ICtFN, P$MinCtF, P$MaxCtF, "Chance Forget")
  
  
  
  #Misc Errors
  if(P$RSize0*(1+2*P$PerROh) > P$MaxRSize){
    stop(paste0("InitialSylRepSize and/or PrcntSylOverhang is too large for the given MaxSylRepSize. (", P$MaxRSize, ")"))
  }
  
  if(P$Dial < 1 ||
     P$Dial >= P$numBirds ||
     P$numBirds%%P$Dial != 0 ||
     P$Dial%%1 != 0 ||
     P$Dial > floor(P$MaxRSize/(P$RSize0*(1+2*P$PerROh)))){
    stop("Dialects must meet the following criterion:
         1) Must be an integer of 1 or greater.
         2) Cannot be larger than the number of birds.
         3) Must be a factor of the number of birds.
         4) Must be less than or equal to MaxSylRepSize/(InitialSylRepSize*(1+2*PrcntSylOverhang)).")
  }
  if((P$MDial %in% c("None", "Similar", "Same")) == FALSE){
    stop("MaleDialects must None, Similar, or Same.")
  }
  if((P$ConsenS %in% c("Conform", "AllNone", "Percentage")) == FALSE){
    stop("MaleDialects must Conform, AllNone, or Percentage.")
  }
  if(P$RepPref+P$MatPref > 1){
    stop("RepSizePrefer+MatchPrefer cannot exceed 1")
  }
  
  if(is.na(P$Seed)==FALSE && is.numeric(P$Seed) == FALSE){
    stop("Seed must be a number or NA")
  }
  if(P$MatPref == 0  && P$SMat == FALSE && P$SFSng == TRUE){
    stop("Cannot save female song unless it is generated.
         It is not generated unless 1) MatchPrefer > 0,
         2) FemaleEvolve == TRUE,
         or 3) SaveMatch == TRUE.")
  }
  if((P$LisThrsh%%1 != 0 && P$LisThrsh > 1) || (P$LisThrsh > P$MaxRSize)  || P$LisThrsh < 0){
    stop(paste0("ListeningThreshold must either be an integer from 1 to MaxSylRepSize (", P$MaxRSize, ")",
                "or a fraction representing a precentage.*
                *If .999 or greater is typed, it is converted to 100%."))
  }
  if((P$FLisThrsh%%1 != 0 && P$FLisThrsh > 1) || (P$FLisThrsh > P$MaxRSize)  || P$FLisThrsh < 0){
    stop(paste0("FatherListeningThreshold must either be an integer from 1 to MaxSylRepSize (", P$MaxRSize, ")",
                "or a fraction representing a precentage.*
                *If .999 or greater is typed, it is converted to 100%."))
  }
  
  
  #Misc Warnings
  if(P$EnSuc == 0){
    warning("If EncounterSuccess is set to 0, no oblique learning can occur.")
  }
  if(P$Steps >= max(P$R, P$C)-1 && (P$ScopeB || P$ScopeT)){
    warning("Steps cover entire matrix.  LocalBreed and LocalTutor switched to Global")
    P$ScopeB <- FALSE
    P$ScopeT <- FALSE
  }
  if(P$DeadThrsh < 1){
    warning("Small DeathThresholds decrease the chances that any birds will survive
            the selection process long enough to reach the MaxAge.")
  }
  if(P$FEvo == TRUE && P$MatPref ==0){
    warning("FemaleEvolve implimented only when females have a match preference > 0.")
  }
  if(P$MatPref == 0 && P$MDial != "None"){
    warning("MaleDialects only implemented when MatchPrefer is > 0.")
  }
  if(P$MScl != 1){
    warning("MatchScale is not yet implemented!!!")
  }
  return(P)
}
CheckTrait <- function(initial, noise, min, max, name, absMax=1){
  if(min >= max){
    stop(paste("The min for", name, "must be less than the max."))
  }
  if(noise > (max-min)/2){
    stop(paste("The noise for", name, "is too large."))
  }
  if(noise < 0){
    stop(paste("The noise for", name, "cannot be less than 0"))
  }
  if(min < 0){
    stop(paste("The min", name, "cannot be less than 0."))
  }
  if(max > absMax){
    stop(paste("The max", name, "cannot be greater than", absMax, "."))
  }
  if(initial > max || initial < min){
    stop(paste("The initial value for", name, "must be with within the range of its min and max."))
  }
}
CheckMinMaxInt <- function(value, valueName, min=0, max=1, maxed=FALSE, int=TRUE){
  if(value < min){stop(paste(valuename, "cannot be less than",min,"."))}
  if(maxed){if(value>max){stop(paste(valueName, "cannot be greater than",max,"."))}}
  if(int){if(value%%1 !=0){stop(paste(valueName, "must be an integer."))}}
}
CheckBool <- function(value, valueName, NAer=FALSE){
  if(!NAer){
    if(!(value %in% c(TRUE,FALSE))){
      stop(paste(valueName, "must be TRUE or FALSE."))
    }
  }else{
    if(!is.logical(value)){
      stop(paste(valueName, "must be TRUE, FALSE, or NA."))
    }
  }
}
TestRequirement <- function(test, dependancy1 = 0, dependancy2 = FALSE){
  if(is.na(test)){
    if(dependancy1 == 0 && !dependancy2){return(FALSE)}
    return(TRUE)
  }else{return(test)}
}
SaveParam <- function(P, folderName, fileName="Parameters", type="Basic"){
  P$SimStep <- 1
  UsedP <- paste0(names(P), "=", P)
  if(any(grepl("Type", UsedP))){
    UsedP <- UsedP[-which(grepl("Type", UsedP))]
  }
  UsedP <- c(UsedP, paste0("Type=",type))
  write.table(UsedP, file.path(folderName, paste0(fileName,".semp")), quote=FALSE,
              sep = "\n", row.names=FALSE, col.names=FALSE)
}
ReloadParam <- function(filePath){
  Raw <- read.table(filePath, sep="=", as.is=TRUE)
  Final <- rbind.data.frame(Raw$V2, stringsAsFactors=FALSE)
  names(Final) <- Raw$V1
  Log <- which(Final %in% c("TRUE", "FALSE", "True", "False"))
  Log <- which(names(Final) %in% c("MDial", "ConsenS"))
  suppressWarnings(Final[,-c(Char, Log)] <- as.numeric(Final[,-c(Char, Log)]))
  Final[,Log] <- as.logical(Final[,Log])
  return(Final)
}

CalculateProportion <- function(N=400, t=1, Pc=.3, MAge=20){
  #get roots based on set values
  coeff <- c(t-N, rep(t,MAge-1), (t/Pc)+t)
  roots <- polyroot(coeff)
  #pull out the real root
  realroots <- suppressWarnings(as.numeric(roots[which(Im(zapsmall(roots, 12)) == 0)]))
  realroots <- 1/realroots
  Pa <- realroots[which(realroots > 0 & realroots < 1)]
  if(length(Pa)==0){
    stop(paste("MaxAge too large or population of birds (matrix size) too small.", P$SimStep))
  }
  #print(Pa)
  return(Pa)
}
