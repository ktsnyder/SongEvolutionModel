setwd("D:/Documents/R/AgentBasedModel/GenPlot")
source("Source_ParamGenerator.R")

Path <- file.path(getwd(), "ConsensusTutors")
dir.create(Path)
RSP <- c(1,0); MSP <- c(0,1)
Lstr <- "Consensus"
NumTut <- 2:12

count <- 1
for(i in seq_along(RSP)){
  for(k in seq_along(NumTut)){
      P <- DefineParameters(RepSizePrefer = RSP[i], MatchPrefer = MSP[i],
                            LearnerStrategy = Lstr,
                            ConsensusNoTut = NumTut[k],
                            LocalBreed = FALSE, LocalTutor = FALSE,
                            numSim = 4000)
      UsedP <- paste0(names(P), "=", P)
      write.table(UsedP, paste0(Path, "/",count,".semp"), quote=FALSE,
                  sep = "\n", row.names=FALSE, col.names=FALSE)
      count <- count + 1
  }
}

