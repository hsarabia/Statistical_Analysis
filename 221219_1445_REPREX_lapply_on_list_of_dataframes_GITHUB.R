##############################################################
# REPREX Bootstrap and Kaplan Meier                          #
# using lapply on list of data frames                        # 
#                                                            #
# Hiram Sarabia, MS                                          #
# San Diego Water Board                                      #
# California Water Resources Control Board, San Diego Region #
#                                                            #
# Had issues with NADA2 outdated dependencies - FIXED        #
#                                                            #
# December 2022                                              #
##############################################################

library(boot)
library(wakefield)
setwd('C:\\R\\SDBayBackground\\Input')

#Generate three random datasets as data frames 
#df
Result <- rnorm(1000,10,1)
Censored <- r_sample_logical(1000, name = "Cens")  
df <- data.frame(Result,Censored)
#df1
Result <- rnorm(1000,10,1)
Censored <- r_sample_logical(1000, name = "Cens")  
df1 <- data.frame(Result,Censored)
#df2
Result <- rnorm(1000,10,1)
Censored <- r_sample_logical(1000, name = "Cens")  
df2 <- data.frame(Result,Censored)

#Define list elements
x <- list(df=df,df1=df1,df2=df2)
#Bootstrap
medianfun <- function(x, i) {
  d <- x[i]
  return(median(d))   
}
meanfun <- function(x, i) {
  d <- x[i]
  return(mean(d))   
}
quantfun <- function(x, i) {
  d <- x[i]
  return(quantile(d, c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 0.95)))   
}

set.seed(1234)
R <- 10000
bo.quant <- lapply(x, function(z) boot(z$Result, statistic=quantfun, R=R))
#show all items on output list
bo.quant

censlist <- list(df=df,df1=df1,df2=df2)

#Kaplan Meier
library(NADA2)
km <- lapply(censlist, function(z) 
  cfit(z$Result, z$Censored, qtls=c(.1, .2, .3, .4, .5, .8, .845, .85, .90, .95),
       Cdf=FALSE, printstat=FALSE))
#Show all items on output list
km
#km$df  ## access list `df`. `df1` and `df2` accordingly

