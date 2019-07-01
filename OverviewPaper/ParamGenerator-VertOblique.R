setwd("D:/Documents/R/AgentBasedModel/GenPlot")
source("Source_ParamGenerator.R")

Path <- file.path(getwd(), "VertOblique")
dir.create(Path)
Vert <- c(TRUE, TRUE, FALSE)
InherPercentage <- c(7,.30,.60,.999)
MinSyls <- c(0,3,7,15)

count <- 1
for(i in seq_along(Vert)){
  for(j in seq_along(InherPercentage)){
    if(i == 1){
      Finher <- .999 
    }else{
      Finher = InherPercentage[j]
    }
    for(k in seq_along(MinSyls)){
      P <- DefineParameters(RepSizePrefer = 1, MatchPrefer = 0,
                            LearnerStrategy = "Add",
                            LocalBreed = FALSE, LocalTutor = FALSE,
                            VerticalLearning = Vert[i],
                            ListeningThreshold = InherPercentage[j],
                            FatherListeningThreshold = Finher,
                            MinLearnedSyls = MinSyls[k],
                            numSim = 4000)
      UsedP <- paste0(names(P), "=", P)
      write.table(UsedP, paste0(Path, "/",count,".semp"), quote=FALSE,
                  sep = "\n", row.names=FALSE, col.names=FALSE)
      count <- count + 1
    }
  }
}
