#setwd()
library("SEM")

Path <- file.path(getwd(), "Shapes")
dir.create(Path)
RSP <- c(0); MSP <- c(1)
Lstr <- c("Consensus")
IAccNo=.1

nDial <- c(2,4)
rows <- c(4,5,10,20)
cols <- c(100,80,40,20)
Breed <- c(TRUE, FALSE)
Tutor <- c(TRUE, FALSE)
UMat <- c(TRUE)
VLrn <- c(TRUE, FALSE)
MDial <- c("Same", "None")
count <- 1

for(i in seq_along(nDial)){
  for(j in seq_along(rows)){
    for(l in seq_along(MDial)){
      for(m in seq_along(Breed)){
        for(n in seq_along(Tutor)){
          for(o in seq_along(VLrn)){
            P <- DefineParameters(RepSizePrefer = RSP, MatchPrefer = MSP,
                                  Rows=rows[j],
                                  Cols=cols[j],
                                  LearnerStrategy = Lstr,
                                  InherAccuracyNoise = IAccNo,

                                  Dialects = nDial[i],
                                  MaleDialects = MDial[l],
                                  LocalBreed = Breed[m],
                                  LocalTutor = Tutor[n],
                                  VerticalLearning = VLrn[o],
                                  UniformMatch = UMat,

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
}
