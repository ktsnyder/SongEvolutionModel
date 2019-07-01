setwd("D:/Documents/R/AgentBasedModel/GenPlot")
source("Source_ParamGenerator.R")

Path <- file.path(getwd(), "NoiseSweep")
dir.create(Path)
Lstr <- c("Add", "AddForget","Consensus")
Noise <- c(.25,.2,.15,.1,.05,.025,0)

count <- 1
for(j in seq_along(Lstr)){
  for(k in seq_along(Noise)){
      P <- DefineParameters(RepSizePrefer = 0, MatchPrefer = 1,
                            LearnerStrategy = Lstr[j],
                            InitialAccuracy=.95,
                            InherAccuracyNoise = Noise[k],
                            InitialLearningThreshold=1,
                            LocalBreed = FALSE, LocalTutor = FALSE,
                            numSim = 4000)
      UsedP <- paste0(names(P), "=", P)
      write.table(UsedP, paste0(Path, "/",count,".semp"), quote=FALSE,
                  sep = "\n", row.names=FALSE, col.names=FALSE)
      count <- count + 1
  }
}

Path <- file.path(getwd(), "NoiseSweep", "VertOnly")
dir.create(Path)
count <- 1
for(k in seq_along(Noise)){
  P <- DefineParameters(RepSizePrefer = 0, MatchPrefer = 1,
                        ObliqueLearning = FALSE,
                        InitialAccuracy=.95,
                        InherAccuracyNoise = Noise[k],
                        InitialLearningThreshold=1,
                        LocalBreed = FALSE, LocalTutor = FALSE,
                        numSim = 4000)
  UsedP <- paste0(names(P), "=", P)
  write.table(UsedP, paste0(Path, "/",count,".semp"), quote=FALSE,
              sep = "\n", row.names=FALSE, col.names=FALSE)
  count <- count + 1
}

Path <- file.path(getwd(), "NoiseSweep", "ObliqueOnly")
dir.create(Path)
count <- 1
for(i in seq_along(Lstr)){
  for(k in seq_along(Noise)){
    P <- DefineParameters(RepSizePrefer = 0, MatchPrefer = 1,
                          LearnerStrategy = Lstr[i],
                          ObliqueLearning = TRUE,
                          VerticalLearning = FALSE,
                          InitialAccuracy=.95,
                          InherAccuracyNoise = Noise[k],
                          InitialLearningThreshold=1,
                          InherLearningNoise = 0,
                          LocalBreed = FALSE, LocalTutor = FALSE,
                          numSim = 4000)
    UsedP <- paste0(names(P), "=", P)
    write.table(UsedP, paste0(Path, "/",count,".semp"), quote=FALSE,
                sep = "\n", row.names=FALSE, col.names=FALSE)
    count <- count + 1
  }  
}

