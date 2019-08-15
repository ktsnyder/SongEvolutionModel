#setwd()
library("SEM")

Path <- file.path(getwd(), "SizeCheck")
dir.create(Path)
RSP <- c(0); MSP <- c(1)
Lstr <- c("Consensus")
IAccNo=.1

nDial <- c(2,4)
rows <- c(20,40)
cols <- c(40,40)
Breed <- c(TRUE)
Tutor <- c(TRUE)
UMat <- c(TRUE)
VLrn <- c(TRUE)
MDial <- c("Same")
count <- 1

for(i in seq_along(nDial)){
            P <- DefineParameters(RepSizePrefer = RSP, MatchPrefer = MSP,
                                  Rows=rows[i],
                                  Cols=cols[i],
                                  LearnerStrategy = Lstr,
                                  InherAccuracyNoise = IAccNo,

                                  Dialects = nDial[i],
                                  MaleDialects = MDial,
                                  LocalBreed = Breed,
                                  LocalTutor = Tutor,
                                  VerticalLearning = VLrn,
                                  UniformMatch = UMat,

                                  #SaveMaleSong = TRUE,
                                  #SaveFemaleSong = TRUE,
                                  numSim = 4000)
            UsedP <- paste0(names(P), "=", P)
            write.table(UsedP, paste0(Path, "/",count,".semp"), quote=FALSE,
                        sep = "\n", row.names=FALSE, col.names=FALSE)
            count <- count + 1
}
