#setwd()
library("SEM")

Path <- file.path(getwd(), "StaticPreferences")
dir.create(Path)

#Rep, Match-Match, Match-Presence, Rare, Common, Social, None
RSP <-  c(1,0,0,0,0,0,0)
FSP <-  c(0,0,0,1,1,0,0)
Rare <- c(T,T,T,T,F,T,T)
MSP <-  c(0,1,1,0,0,0,0)
SSP <-  c(0,0,0,0,0,1,0)
MStr <- c("Match","Presence","Match","Match","Match","Match", "Match")



#Add, AddForget, Consensus-Conform, Consensus-AllNone, Consensus-Precentage, Overlearn and Prune
Lstr <-   c("Add",     "AddForget", "Consensus", "Consensus", "Consensus",  "Forget")
CStr <- c("Conform", "Conform",   "Conform",   "AllNone",   "Percentage", "Conform")
OvrLrn <- c(F,F,F,F,F,T)


VLrn <- c(TRUE, FALSE)


Social <- c(TRUE, FALSE)


count <- 1
for(i in seq_along(RSP)){
  for(j in seq_along(Lstr)){
    for(k in seq_along(VLrn)){
      for(l in seq_along(Social)){
            P <- DefineParameters(RepSizePrefer = RSP[i], MatchPrefer = MSP[i],
                                  FrequencyPrefer = FSP[i], SocialPrefer = SSP[i],
                                  Rare = Rare[i], MatchStrategy = MStr[i],
                                  
                                  LearnerStrategy = Lstr[j],
                                  ConsensusStrategy = CStr[j],
                                  OverLearn = OvrLrn[j],

                                  VerticalLearning = VLrn[k],
                                  SocialCues = Social[l],
                                  
                                  
                                  LocalBreed = TRUE,
                                  LocalTutor = TRUE,

                                  
                                  SaveMaleSong = TRUE,
                                  SaveFemaleSong = TRUE,
                                  SaveMatch = TRUE,
                                  numSim = 2000)
            UsedP <- paste0(names(P), "=", P)
            write.table(UsedP, paste0(Path, "/",count,".semp"), quote=FALSE,
                        sep = "\n", row.names=FALSE, col.names=FALSE)
            count <- count + 1
      }
    }
  }
}
