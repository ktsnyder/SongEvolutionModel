setwd("D:/Documents/R/AgentBasedModel/GenPlot")
source("Source_ParamGenerator.R")

Path <- file.path(getwd(), "ChanceInvSweep")
dir.create(Path)
RSP <- c(1,0); MSP <- c(0,1)
Lstr <- c("Add", "AddForget","Consensus")
Lpen <- c(0,.75, 1.5)
ChanInv <- c(.5,.1,.01)
SylRep <- 5
Acc <- .7
LrTh <- 2
count <- 1
for(i in seq_along(RSP)){
  for(j in seq_along(Lstr)){
    for(k in seq_along(ChanInv)){
      for(l in seq_along(Lpen)){
        P <- DefineParameters(RepSizePrefer = RSP[i], MatchPrefer = MSP[i],
                              LearnerStrategy = Lstr[j],
                              InitialSylRepSize = SylRep,
                              InitialAccuracy=Acc,
                              InitialLearningThreshold=LrTh,
                              InitialChancetoInvent = ChanInv[k],
                              LearningPenalty=Lpen[l],
                              LocalBreed = FALSE, LocalTutor = FALSE,
                              numSim = 4000)
        UsedP <- paste0(names(P), "=", P)
        write.table(UsedP, paste0(Path, "/",count,".semp"), quote=FALSE,
                    sep = "\n", row.names=FALSE, col.names=FALSE)
        count <- count + 1
      }
    }
  }
}


