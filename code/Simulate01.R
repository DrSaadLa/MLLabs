############################################################################
############################################################################
###                                                                      ###
###        DURBIN-WATSON AND BREUSCH-PAGAN TESTS:                        ###
###               SIMULATION STUDY                                       ###
###                                                                      ###
############################################################################
############################################################################

generate_data <- function(nobs = 15, model = c("trend", "dynamic"),
             corr = 0, coef = c(0.25, -0.75), sd = 1) {
   model <- match.arg(model)
   coef <- rep(coef, length.out = 2) 
   
   err <- as.vector(filter(rnorm(nobs, sd = sd), corr,
                           method = "recursive"))
   if(model == "trend") {
     x <- 1:nobs
     y <- coef[1] + coef[2] * x + err
   } else {
     y <- rep(NA, nobs)
     y[1] <- coef[1] + err[1]
     for (i in 2:nobs)
       y[i] <- coef[1]+ coef[2] * y[i-1] + err[i]
     x <- c(0, y[1:(nobs - 1)])
   }
   return(data.frame(y = y, x = x)) 
 }
  
# Generate simpower function:

simpower <- function(nrep = 100, size = 0.05, ...) {
    pval <- matrix(rep(NA, 2 * nrep), ncol = 2) colnames(pval) <- c("dwtest", "bgtest") for(i in 1:nrep) {
      dat <- dgp(...)
      pval[i,1] <- dwtest(y ~ x, data = dat,
                          alternative = "two-sided")$p.value
      pval[i, 2] <- bgtest(y ~ x, data = dat)$p.value
      }
    return(colMeans(pval<size))
}

# Simulation 

simulation <- function(corr = c(0, 0.2, 0.4, 0.6, 0.8,
                                0.9, 0.95, 0.99), 
                       nobs = c(15, 30, 50),
                       model = c("trend", "dynamic"), ...) {
  prs <- expand.grid(corr = corr, nobs = nobs, model = model)
  nprs <- nrow(prs) 
  pow <- matrix(rep(NA, 2 * nprs), ncol = 2)
  for(i in 1:nprs) pow[i,] <- simpower(corr = prs[i,1],
                                       nobs = prs[i,2],
                                       model = as.character(prs[i,3]), ...) 
  rval <- rbind(prs, prs)
  rval$test <- factor(rep(1:2, c(nprs, nprs)),
                      labels = c("dwtest", "bgtest"))
  rval$power <- c(pow[,1], pow[,2])
  rval$nobs <- factor(rval$nobs)
  return(rval) 
}

