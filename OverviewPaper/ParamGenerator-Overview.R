setwd("D:/Documents/R/AgentBasedModel/GenPlot")
source("Source_ParamGenerator.R")

Path <- file.path(getwd(), "OverviewSweep")
dir.create(Path)
RSP <- c(1,0); MSP <- c(0,1)
Lstr <- c("Add", "AddForget","Consensus")
SylRep <- c(5,20)
Acc <- c(.5,.7)
LrTh <- c(2, 10)
Lpen <- c(0,.75, 1.5)
count <- 1
for(i in seq_along(RSP)){
  for(j in seq_along(Lstr)){
    for(k in seq_along(SylRep)){
      for(l in seq_along(Acc)){
        for(m in seq_along(LrTh)){
          for(n in seq_along(Lpen)){
            P <- DefineParameters(RepSizePrefer = RSP[i], MatchPrefer = MSP[i],
                                  LearnerStrategy = Lstr[j],
                                  InitialSylRepSize = SylRep[k],
                                  InitialAccuracy=Acc[l],
                                  InitialLearningThreshold=LrTh[m],
                                  LearningPenalty=Lpen[n],
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
  }
}
