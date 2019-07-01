setwd("D:/Documents/R/AgentBasedModel/GenPlot")
source("Source_ParamGenerator.R")

Path <- file.path(getwd(), "SquarePrefLpen")
dir.create(Path)
Lstr <- c("Add", "AddForget","Consensus")
RSP <- seq(1,0, length=10); MSP <- seq(0,1, length=10)
Lpen <- seq(0,2, length=10)

count <- 1
for(j in seq_along(Lstr)){
  for(i in seq_along(RSP)){
    for(n in seq_along(Lpen)){
      P <- DefineParameters(RepSizePrefer = RSP[i], MatchPrefer = MSP[i],
                            LearnerStrategy = Lstr[j],
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
