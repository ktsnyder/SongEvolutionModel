#Life/CircusCycle
#Core circuit for a time step

#' Birth Death Cycle
#'
#' A wrapper that allows birds to die, undergo oblique learning, be born, and undergo vertical learning (in that order).
#' @param P a list of parameters
#' @param population the population of birds
#' @keywords birth death
#' @export
BirthDeathCycle <- function(P, population){
  #Send Birds to circus via Age or Random with probability based on Learning penalty
  if(P$DStrat){
    Vacancy <- AgeDeath(P, population)
  }else{Vacancy <- RandomDeath(P, population)}
  #Mark Vacancy and allow persitant males to age up, learn, recount sylls and rematch
  if(P$Obliq){
    population <- ObliqueLearning(P, population, Vacancy)
    population$Males$Age[-Vacancy] <- population$Males$Age[-Vacancy] + 1
  }
  
  if(P$ChoMat){#Females can pick a male that matches them well
    Assign <- AssignFemale(P, TerritorialBirds$MSongs,
                           TerritorialBirds$FSongs)
    TerritorialBirds$FSongs <- TerritorialBirds$FSongs[Assign[[1]],]
    TerritorialBirds$Males$Match <- Assign[[2]]
  }

  #Determine fathers and generate chicks
  FatherInd <- ChooseFathers(P, population, Vacancy)
  Chicks <- GenerateChicks(P, FatherInd, population, Vacancy)
  population$Males[Vacancy,] <- Chicks$Males
  population$MSongs[Vacancy,] <- Chicks$MSongs

  #Allow females to die with their mates so female song evolves
  if(P$FEvo){population <- FemaleEvolve(P, population, Vacancy, FatherInd)}
  
  #allow for chicks to overlearn, update match and sylrep
  if(P$OvrLrn){population <- OverLearn(P, population, Vacancy)}
  
  #update breeding information
  if(P$Social){
    Pop$Males$Bred[FatherInd] <- P$SocialBred
    Pop$Males$Bred[-FatherInd] <- P$SocialNotBred
  }

  #update survival probability
  if(P$DStrat){population <- UpdateProbabilities(P, FatherInd, population)}
  return(population)
}

#Choose Fathers that breed

#' Choose Fathers
#'
#' Chooses the males who will breed.  Males must be alive and know at least one syllable.  They can be chosen locally or globally.  Males who best fit selection preferences are the most likely to father offspring.  Males can father more than one chick.
#' @param P a list of parameters
#' @param population the population of birds
#' @param vacancy territories that need to be filled
#' @keywords birth
#' @export
ChooseFathers <- function(P, population, vacancy){
  #Picks sires for populating vacant territories
  #Use only living males
  #It is likely for high quality males to sire multiple offspring
  #in both Global and Local varients
  NotAvail <- c(vacancy,which(population$Males$SylRep == 0)) #remove songless birds
  ProbBreed <- numeric(P$numBirds)
  ProbBreed[(1:P$numBirds)[-NotAvail]] <- GetReproductiveProbability(P, population, (1:P$numBirds)[-NotAvail])

  if(P$ScopeB){
    Fathers <- vector(mode="numeric", length(vacancy))
    for(i in seq_along(Fathers)){
      #Pick local males which are alive
      UsableInd <- LocalSearch(P, population, vacancy[i], NotAvail)
      if(length(UsableInd) == 1){
        Fathers[i] <- UsableInd
      }else{
        Fathers[i] <- sample(UsableInd, 1, prob=ProbBreed[UsableInd])
      }
    }
  }else{
    Fathers <- sample(1:P$numBirds, length(vacancy), prob=ProbBreed, replace=TRUE)
  }
  return(Fathers)
}


#' Get Probability of Reproducing
#'
#' Calculates how well a male matches female preferences for repertoires and/or matching (and/or noise, which is added uniformly to all males) to determine their probability of fathering offspring.
#' @param P a list of parameters
#' @param population  the population of birds
#' @param usableInd males that are alive and know at least one syllable
#' @keywords birth female-choice
#' @export
GetReproductiveProbability <- function(P, population, usableInd){
  Choices <- population$Males[usableInd,]
  #Noise
  Bonus <- rep(P$NoisePref, length(usableInd))
  
  
  #SylRep
  if(P$RepPref != 0){
    if(P$LogScl){Rep <- log(Choices$SylRep)
    }else{Rep <- Choices$SylRep}
    
    WorstMale <- min(Rep)
    if(max(Rep) == WorstMale){#if all males have the same rep size
      RepBonus <- rep(1,length(usableInd))
    }else{
      Fraction <- 1/(max(Rep) - WorstMale)
      RepBonus <- (Rep - WorstMale)*Fraction
    }
    Bonus <- Bonus + P$RepPref*RepBonus
  }
  
  
  #Match
  if(P$MatPref != 0){
    Bonus <- Bonus + P$MatPref*Choices$Match
  }
  
  
  #Frequency
  if(P$FreqPref != 0){
    #get syllable frequency
    SyllableFrequency <- colSums(population$MSongs[usableInd,])/length(usableInd)
    if(P$Rare){
      SyllableFrequency <- 1-SyllableFrequency
    }
    #add the syllable frequencies in each song
    MSongFrequency <- sapply(usableInd, function(x) sum(SyllableFrequency[as.logical(population$MSongs[x,])]))
    
    #scale the freq values
    WorstMale <- min(MSongFrequency)
    if(max(MSongFrequency) == WorstMale){#if all males have the same rep size
      RepBonus <- rep(1,length(usableInd))
    }else{
      Fraction <- 1/(max(MSongFrequency) - WorstMale)
      FreqBonus <- (MSongFrequency - WorstMale)*Fraction
    }
    Bonus <- Bonus + P$FreqPref*FreqBonus
  }
  
  
  
  #social
  if(P$SocPref != 0){
    Soc <- Choices$Bred
    
    WorstMale <- min(Soc)
    if(max(Soc) == WorstMale){#if all males have the same rep size
      SocBonus <- rep(1,length(usableInd))
    }else{
      Fraction <- 1/(max(Soc) - WorstMale)
      RepBonus <- (Soc - WorstMale)*Fraction
    }
    Bonus <- Bonus + P$SocPref*SocBonus
  }
  
  
  Bonus[which(Bonus< .001)] <- .001
  return(Bonus)
}

#Death/Circus Functions

#' Random Death
#'
#' Randomly picks a percentage of males in the population to die.  Current age is not a relevant factor in being chosen.  Chicks are as likely to be chosen as adults.
#' @param P a list of parameters
#' @param population  the population of birds
#' @keywords death
#' @export
RandomDeath <- function(P, population){
  #Send a paramaterized percentage of random birds to the circus
  NumLostBirds <- round(P$numBirds*P$PDead, digits=0)
  PickChance <- LearningThrshPenalty(P, population$Males$LrnThsh)
  LostBirds <- sample(1:(P$numBirds), NumLostBirds, replace=FALSE, prob = PickChance)
  return(LostBirds)
}



#' Age Death
#'
#' Males are chosen to die based on their age using a type II survival curve.
#' @param P a list of parameters
#' @param population  the population of birds
#' @keywords death
#' @export
AgeDeath <- function(P, population){
  #Send birds to the circus based on retirement age
  DeadIndex <- vector("list")
  PickChance <- LearningThrshPenalty(P, population$Males$LrnThsh)

  for(i in 1:P$MAge){
    #which birds are age i
    Pool <- which(population$Males$Age==(i-1))
    if(length(Pool) == 0){#if none, skip
      next
    }
    Dead <- length(Pool)*(1-population$AgeDeathProb[i])#must die
    if(Dead%%1 != 0){#chance to die
      if(runif(1) < Dead%%1){
        Dead <- Dead%/%1 +1
      }else{Dead <- Dead%/%1}
    }
    if(Dead == 0){
      next()
    }
    if(Dead >= length(Pool)){#all birds die
      Chosen <- Pool
    }else{
      Chosen <- sample(Pool,Dead, prob = PickChance[Pool])}
      DeadIndex[[i]] <- Chosen
    }
  #Birds of maxagep
  DeadIndex[[P$MAge+1]] <- which(population$Males$Age==(P$MAge))
  return(unlist(DeadIndex))
}


#' Learning Threshold Fitness Penalty
#'
#' Calculates the fitness penalty for longer learning, which is used as the probability that a male will be chosen to die in that timestep.
#' @param P a list of parameters
#' @param lrnThsh a vector of learning thresholds in the population
#' @keywords death
#' @export
LearningThrshPenalty <- function(P, lrnThsh){
  PickChance <- P$Lpen/(P$MAge-1)*(lrnThsh-1)+1
  PickChance[which(PickChance < 1)] <- 1
  return(PickChance)
}


#' Update Survival Proportions
#'
#' At the end of a timestep, it removes the adult survival proportion from the generation that has just been extinguished, and calculates the  adult survival proportion for the chicks that have just been generated.
#' @param P a list of parameters
#' @param chicks vector of chick indicies
#' @param prob the data structure from the population that keeps track of survival probabilities
#' @keywords death survival-curve
#' @export
UpdateProbabilities <- function(P, chicks, prob){
  if(length(prob$AgeDeathProb)==1){#only chick survival
    return(prob)
  }
  if(length(prob$AgeDeathProb)>2){#move over years 2+
    prob$AgeDeathProb[3:length(prob$AgeDeathProb)] <- prob$AgeDeathProb[2:(length(prob$AgeDeathProb)-1)]
  }
  prob$AgeDeathProb[2] <- prob$AgeStore#assign year 1s to previously calced and stored value
  prob$AgeStore <- (1/(P$Pc*length(chicks)))^(1/P$MAge)
  return(prob)
}
