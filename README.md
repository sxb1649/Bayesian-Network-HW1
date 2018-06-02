# Bayesian-Network-HW1

BUAN 6357 (Johnston) Fall 2017 Homework 1 Due: 17 September 2017 
 
Background:  In the UTDbox entry for this class there is a folder labeled “Homework and Supporting Documents”.  Inside that folder you will find this document and a PDF file labeled “BayesianNetworks_w_synthetic_data.pdf”.  You will need both files to do this assignment.  You will be allowed to submit your answers multiple times and only the last score on each part will count.  Answers will be accepted by eLearning until midnight on the day due. 
Assignment:  Read the “BayesianNetworks” file (internal title: Notes on Bayesian Networks) for general background information and context.  Pay particular attention to section 3 (Sampling from a BN in R) and the network described by the conditional probabilities listed.  After you have reviewed the conditional probabilities, look closely at the R function defined in Figure 4 (R implementation of the Probabilistic Logic Sampling for the BN plotted in panel (a) ).  You will want to enter this code and run it using the first 2 lines of executable code presented in Figure 5 (R implementation of the Probabilistic Logic Sampling for the BN plotted in panel (a) ) with the addition of a statement setting the initial RNG seed value to “1”. 
Deliverable: a single R object named “Data” which is the result of running the function pls() with the formal parameter 200. 
Comments/Hints: 
1. There will be a quiz portion of this homework in eLearning as well as a code submit portion.  You must answer the quiz portion correctly before you can submit code.  After you get full credit for the quiz portion you will NOT need to submit those answers again. 2. The eLearning portions will be available sometime on Monday, 11 September 2017. 3. The conditional probabilities listed at the beginning of section 3 are the governing statement describing the Bayesian Network (BN) under consideration. 



#Background Bayesian Network


#Answers
#SparshBijawat

#Setting Seed
set.seed(1)


#Creating Function pls
pls <- function(n)
{
  X1 <- NULL
  X2 <- NULL
  X3 <- NULL
  X4 <- NULL
  X5 <- NULL
  X6 <- NULL
  
  for (k in 1:n) {
    x1 <- rbinom(1, 1, 0.3)
    X1 <- c(X1, x1)
    x2 <- rbinom(1, 1, 0.7)
    X2 <- c(X2, x2)
    
    if (x1 == 1 && x2 == 1) {
      x3 <- rbinom(1, 1, 0.8)
      X3 <- c(X3, x3)
    }
    
    if (x1 == 1 && x2 == 0) {
      x3 <- rbinom(1, 1, 0.6)
      X3 <- c (X3, x3)
    }
    
    if (x1 == 0 && x2 == 1) {
      x3 <- rbinom(1, 1, 0.6)
      X3 <- c(X3, x3)
    }
    
    if (x1 == 0 && x2 == 0) {
      x3 <- rbinom(1, 1, 0.1)
      X3 <- c(X3, x3)
    }
    
    if (x1 == 1) {
      x4 <- rbinom(1, 1, 0.1)
      X4 <- c(X4, x4)
    }
    
    if (x1 == 0) {
      x4 <- rbinom(1, 1, 0.8)
      X4 <- c(X4, x4)
    }
    
    if (x2 == 1) {
      x5 <- rbinom(1, 1, 0.1)
      X5 <- c(X5, x5)
    }
    
    if (x2 == 0) {
      x5 <- rbinom(1, 1, 0.5)
      X5 <- c(X5, x5)
    }
    
    if (x1 == 1 && x5 == 1) {
      x6 <- rbinom(1, 1, 0.9)
      X6 <- c(X6, x6)
    }
    
    if (x1 == 1 && x5 == 0) {
      x6 <- rbinom(1, 1, 0.5)
      X6 <- c(X6, x6)
    }
    
    if (x1 == 0 && x5 == 1) {
      x6  <- rbinom(1, 1, 0.5)
      X6 <- c(X6, x6)
    }
    
    if (x1 == 0 && x5 == 0) {
      x6 <- rbinom(1, 1, 0.1)
      X6 <- c(X6, x6)
    }
    
  }
  Data <- data.frame(X1, X2, X3, X4, X5, X6)
  return (Data)
}


#Inserting value in Data variable

Data <- pls(200)
