library(data.table)

racusum_limit_dpcl <- function(patient_risks, delta, delta_o, alpha, iter_sim, RA){
  # function for setting the Dynamic Probability Control Limit for a defined population
  # p: vector of patient risk in order of observation
  # R1: odds multiplier for alternative hypothesis (=2)
  # R: odds multiplier for null hypothesis (=1)
  # alpha: pre-specified false alarm rate (0.005)
  # N: large integer for simulation (50.000)
  # RA: if true= risk adjusted, if false leave R empty  
  

  npat <- length(patient_risks)
  
  hs <- matrix(0,nrow=npat,ncol=2 ) 
  
  cs <- 0 # initial cusum value
  
  # loop over patients
  for (j in 1:npat){
    p1 <- patient_risks[j] # in control failure rate p
    y1 <- rbinom(iter_sim,1,p1)  # generate N Bernoulli random variables with ic-failure p1
    
    # define weights
    if (RA==TRUE){
      ws <- log((1-p1+delta_o*p1)/(1-p1+delta*p1)) # survival
      wf <- log(((1-p1+delta_o*p1)*delta)/((1-p1+delta*p1)*delta_o)) # failure
    } else {
      c0 <- mean(patient_risks)
      ca <- ifelse (patient_risks<0.5, delta*patient_risks, 1-((1-patient_risks)*delta))
      ws <- log((1-ca)/(1-c0))
      wf <- log(ca/c0)
    }
    css <- matrix(0,nrow=iter_sim,ncol=2)
    
    # generate N CUSUM statistics
    for (i in 1:iter_sim){
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
    m <- floor(iter_sim * (1 - alpha))
    
    h <- css$V2[m]
    hs[j,] <- c(j,h) 
    
    # remove all cusum statistics with higher value
    c <- css$V2[css$V2<=h]
  }
  
  return(hs)
}