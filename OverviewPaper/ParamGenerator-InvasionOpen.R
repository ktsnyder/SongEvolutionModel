setwd("D:/Documents/R/AgentBasedModel/GenPlot")
source("Source_ParamGenerator.R")

if(!dir.exists(file.path(getwd(), "Invasion"))){
  dir.create(file.path(getwd(), "Invasion"))
}
Path <- file.path(getwd(), "Invasion", "Open")
dir.create(Path)
RSP <- c(1,0); MSP <- c(0,1)
Lstr <- c("Add", "AddForget","Consensus")
Lpen <- c(0,.75, 1.5)
LrTh <- 2

count <- 1
for(i in seq_along(RSP)){
  for(j in seq_along(Lstr)){
    for(l in seq_along(Lpen)){
      P <- DefineParameters(RepSizePrefer = RSP[i], MatchPrefer = MSP[i],
                            LearnerStrategy = Lstr[j],
                            InitialLearningThreshold=LrTh,
                            LearningPenalty=Lpen[l],
                            LocalBreed =  FALSE, LocalTutor = FALSE,
                            numSim = 500, InherLearningNoise = 0)
      UsedP <- paste0(names(P), "=", P)
      write.table(UsedP, paste0(Path, "/",count,".semp"), quote=FALSE,
                  sep = "\n", row.names=FALSE, col.names=FALSE)
      count <- count + 1
    }
  }
}


