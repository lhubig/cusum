#' Setting the Dynamic Probability Control Limits
#' 
#' Zhang, Xiang & Woodall, William. (2016). Dynamic Probability Control Limits for Lower and Two-Sided Risk-Adjusted Bernoulli CUSUM Charts: DPCLs for Lower and Two-Sided Risk-Adjusted Bernoulli CUSUM. Quality and Reliability Engineering International. 10.1002/qre.2044. 
#' 
#' @export
#' @import stats
#' @import data.table
#' @param patient_risks A vector containing patient risk scores
#' @param N Number of simulations
#' @param odds_multiplier Odds multiplier for the alternative hypothesis (<1 looks for decreases)
#' @param alpha False signal probability
#' @param seed An optional seed for simulation
#' @return Returns the control limit
#' 
racusum_limit_dpcl <- function(patient_risks,N , odds_multiplier, alpha, seed){
  # function for setting the Dynamic Probability Control Limit for a defined population
  # p: vector of patient risk in order of observation
  # R1: odds multiplier for alternative hypothesis (=2)
  # alpha: pre-specified false alarm rate (0.005)
  # N: large integer for simulation (50.000)
  # RA: if true= risk adjusted, if false leave R empty  
  
  delta_o <- 1
  n_patients <- length(patient_risks)
  
  hs <- matrix(0,nrow=n_patients,ncol=2 ) 
  
  cs <- 0 # initial cusum value
  
  # loop over patients
  for (j in 1:n_patients){
    p1 <- patient_risks[j] # in control failure rate p
    y1 <- rbinom(N,1,p1)  # generate N Bernoulli random variables with ic-failure p1
    
    # define weights
      ws <- log((1-p1+delta_o*p1)/(1-p1+odds_multiplier*p1)) # survival
      wf <- log(((1-p1+delta_o*p1)*odds_multiplier)/((1-p1+odds_multiplier*p1)*delta_o)) # failure
  
    css <- matrix(0,nrow=N,ncol=2)
    
    # generate N CUSUM statistics
    for (i in 1:N){
      if(j!=1){
        cs <- sample(c,1)
      } else{
        cs <- 0
      }
      w <- ifelse(y1[i]==0, ws,wf)
      
      ct <- max(0, cs +w)
      css[i,] <- c(i, ct)
    }
    
    css <- data.table(css, key="V2") # sort CUSUM statistics in ascending order
    
    # take upper percentile as h
    m <- floor(N * (1 - alpha))
    
    h <- css$V2[m]
    hs[j,] <- c(j,h) 
    
    # remove all cusum statistics with higher value
    c <- css$V2[css$V2<=h]
  }
  
  return(hs)
}
