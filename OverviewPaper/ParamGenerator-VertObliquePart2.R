setwd("D:/Documents/R/AgentBasedModel/GenPlot")
source("Source_ParamGenerator.R")

Path <- file.path(getwd(), "VertObliquePart2")
dir.create(Path)
Lstr <- c("Add", "AddForget","Consensus")
Vert <- c(TRUE, TRUE, FALSE)
InherPercentage <- c(.30,.60)
LrTh <- c(2, 10)
Lpen <- c(0,.75, 1.5)

count <- 1
for(h in seq_along(Lstr)){
  for(i in seq_along(Vert)){
    for(j in seq_along(InherPercentage)){
      if(i == 1){
        Finher <- .999 
      }else{
        Finher = InherPercentage[j]
      }
      for(k in seq_along(LrTh)){
        for(l in seq_along(Lpen)){
          P <- DefineParameters(RepSizePrefer = 1, MatchPrefer = 0,
                                LearnerStrategy = Lstr[h],
                                LocalBreed = FALSE, LocalTutor = FALSE,
                                VerticalLearning = Vert[i],
                                ListeningThreshold = InherPercentage[j],
                                FatherListeningThreshold = Finher,
                                MinLearnedSyls = 7,
                                InitialLearningThreshold = LrTh[k],
                                LearningPenalty = Lpen[l],
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

