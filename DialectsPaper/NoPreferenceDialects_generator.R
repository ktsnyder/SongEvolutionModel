#setwd()
library("SEM")

Path <- file.path(getwd(), "NoPref")
dir.create(Path)
RSP <- c(0); MSP <- c(0)
Lstr <- c("Consensus")
IAccNo <- .1

nDial <- c(2,4)
Breed <- c(TRUE, FALSE)
Tutor <- c(TRUE, FALSE)
VLrn <- c(TRUE, FALSE)
MDial <- c("Same")
count <- 1
for(i in seq_along(nDial)){
  for(j in seq_along(MDial)){
    for(k in seq_along(Breed)){
      for(l in seq_along(Tutor)){
        for(m in seq_along(VLrn)){
            P <- DefineParameters(RepSizePrefer = RSP, MatchPrefer = MSP,
                                  LearnerStrategy = Lstr,
                                  InherAccuracyNoise = IAccNo,

                                  Dialects = nDial[i],
                                  MaleDialects = MDial[j],
                                  LocalBreed = Breed[k],
                                  LocalTutor = Tutor[l],
                                  VerticalLearning = VLrn[m],
                                  UniformMatch = TRUE,
                                  
                                  SaveMatch = TRUE,
                                  #SaveMaleSong = TRUE,
                                  #SaveFemaleSong = TRUE,
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
