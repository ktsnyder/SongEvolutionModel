# KTS test editing cluster plot
# Replaced trait[,i] with trait[i]
#>     MaxMat <- GetMaxMat(trait[i], P$R, P$C)
#Error in (1:N)[-c(SmallCenterPieces, LargeCenterPieces, SmallEdgePieces,  :
#                    only 0's may be mixed with negative subscripts
# Reset trait[i] to trait[,i]; made for(i in seq_along(trait[1,])) work by adding [1,]
# Works now, but doesn't seem very useful


#' Cluster Plot
#'
#' Cluster Plots are normalized such that the minimal score (a smooth gradient) is zero.  The black line shows the maximal score (a checkerboard pattern) while the grey line shows the average score (no pattern).  Green line plots the score of the real data over time.  The function also prints the mean probability of getting the real values given the Min, Max, and Mean values for the matrix at each timestep.
#' @param P a list of parameters
#' @param trait a saved trait from the Basic sims (requires individual data)
#' @family Cluster Plots
#' @keywords stats-plotting
#' @export
ClusterPlot <- function(P, trait){
  time <- proc.time()
  Timesteps <- length(trait[1,])
  Real <- numeric(Timesteps)
  Max <- Real
  Min <- Real
  UnReal <- matrix(0, ncol=5, nrow=length(trait))
  for(i in seq_along(trait[1,])){
    MaxMat <- GetMaxMat(trait[,i], P$R, P$C)
    MinMat <- sort(trait[,i])
    Real[i] <- ClusterCalc(P,trait[,i])
    for(j in 1:5){
      UnReal[i,j] <- ClusterCalc(P,trait[sample(P$numBirds, P$numBirds),i])
    }
    Max[i] <- ClusterCalc(P,MaxMat)
    Min[i] <- ClusterCalc(P,MinMat)
  }
  #get pvalues
  UnReal <- rowMeans(UnReal)
  print(mean(ppert(Real, min=Min, mode=UnReal, max=Max)))

  #"Background subtract" the miinumum values and plot
  UnReal <- UnReal-Min
  Real <- Real-Min
  Max <- Max-Min
  plot(0,type="n", xlim=c(1,Timesteps),
       ylim=c(min(Real,Max,Min), max(Real,Max,Min)),
       xlab="TimeSteps", ylab=paste(deparse(substitute(trait)),"Score"), font.lab=2)
  segments(1:(Timesteps-1), Real[1:(Timesteps-1)],
           2:Timesteps, Real[2:Timesteps], col="grey50",
           lwd=2)
  segments(1:(Timesteps-1), Max[1:(Timesteps-1)],
           2:Timesteps, Max[2:Timesteps], col="Black",
           lwd=2)

  segments(1:(Timesteps-1), UnReal[1:(Timesteps-1)],
           2:Timesteps, UnReal[2:Timesteps], col="green",
           lwd=1)
  return(proc.time()-time)
}
