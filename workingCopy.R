#library(PerformanceAnalytics)  # library() loads the package
#library(quantmod)  # getting the basic packages installed
#library(timeSeries)
#library(fPortfolio)
library(fImport)

# opt.R 

bayesStein <- function(x, spec = NULL, ...){
  stopifnot(inherits(x, "timeSeries"))
  x.mat <- getDataPart(x)
  robust = .bayesSteinMeanCov(x, ...)
  mu = robust$center
  Sigma = robust$cov
  list(mu = mu, Sigma = Sigma)
}


optOut <- function(ret, bs, riskType, nPoints){
  
  ## risk type is either a 1 or 2, with 1 being MV and 2 being CV
  ## BayesStein   2 is equal to the BS solution
  
  spec <- portfolioSpec()
  
  setNFrontierPoints(spec) <- nPoints
  
  if (bs == 2){
    setEstimator(spec) <- "bayesStein"
  }
  
  if (riskType == 1){
    setType(spec) <- "MV"
    setSolver(spec) <- "solveRquadprog"
    riskCol <- 2
  }
  
  if (riskType == 2){
    setSolver(spec) <- "solveRglpk"
    setType(spec) <- "CVaR"
    riskCol <- 3
  }
  
  front <- portfolioFrontier(ret, spec)
  
  reward <- front@portfolio@portfolio$targetReturn[, bs]
  risk <- front@portfolio@portfolio$targetRisk[, riskCol]
  weights <- front@portfolio@portfolio$weights
  
  ## combine the return, risk, weight into one matrix
  
  frontData <- cbind(risk, reward, weights)
  index <- 1:length(frontData[, 1])
  ## get the upper bound of the frontier
  
  test <- c(-1, diff(frontData[, 1]))
  index <- index[test > 0]
  UF <- frontData[index, ]
  UF <- cbind(1:nrow(UF)/100, UF)
  ## make the data frame which is easier to use than matrices
  UF <- data.frame(round(UF, 4)) * 100
  
  UF <- cbind(UF, rep(rownames(ret)[1], nrow(UF)))
  colnames(UF) <- c("Portfolio", "Risk", "Return", colnames(ret),
                    "First Return")
  plot(UF[, 2] , UF[, 3] , 
       xlab = "Risk", ylab = "Return", pch = 19)
  text(x = UF[, 2], y = UF[, 3], 1:length(UF[, 1]), pos = 2 , cex = 0.9)
  print(UF)
}

symbols <- c("IWV", "mdlox", "pttrx")
mutualFundData <- yahooSeries(symbols, "2005-01-01" , "2013-12-31", frequency="monthly" )  # ignore days back  
head(mutualFundData)

usefulMFData <- mutualFundData[, c(6, 12, 18)]    ## data is in the format of price ... needs to be converted to a return series
head(usefulMFData)
returnDataA <- returns(usefulMFData, method="discrete")
head(returnDataA) 

optOut(returnDataA, 1, 1, 10)