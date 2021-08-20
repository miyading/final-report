# Two UBC Work Learn Research Assistant Positions' Summaries

Since most of my work done during these two appointments are on-going researches involving entire groups, I can only upload snippets of my work results here. 

* 2019 School of Music Dr. Poudrier's Rhythm Computation and Cognition Lab. See Final-Report.pdf or Final-Report.docx file in this repository!!!!


* 2021 Department of Statistics Research Assistant Dr. Jame V. Zidek and Dr. Jiahua Chen and Carolyn Taylor. See below!!!

# permutest

<!-- badges: start -->
<!-- badges: end -->

The goal of permutest is to perform asymptotic or permutation tests, and calculate asymptotic and permutation test statistics and p-values for data in a rotating sampling plan to test whether there is a significant change between each pair of years in the mean and in the fifth and fiftieth percentiles.
This package contain functions to calculate the asymptotic and permutation test statistics and p-values in Chen et al.'s paper, including t-test statistic, Wilcoxon sum statistic (scaled to be small), modified t-statistic, studentized t-statistic for quantile change, empirical distribution based test statistics for quantile change, empirical likelihood based test statistic for quantile change under the density ratio model, and empirical likelihood ratio test statistics for mean or quantile change under the density ratio model, based on Yukun Liu's code.
One can generate the three example data sets, which are described in the paper, using three data simulation functions in this package, or input their real world dataset and use the reformat function before carrying out further analyses using functions in this package. 
## Installation

Upon completion, one can install the released version of permutest from [CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("permutest")
```

## Example

This is an example on running the functions in this package:

``` r
library(permutest)
##################################################################################
##################################################################################
###################            Main program               ########################

if(!require("drmdel")){ install.packages("drmdel")}
if(!require("rootSolve")){ install.packages("rootSolve")}
library(drmdel)                          ### Dr. Song Cai's package for fitting DRM
library(rootSolve)                       ### For solving a system of equations

asked = 1                                ### should we permute clusters not shared between two years?
BasisFun = function(x){ cbind(x, x^2) }  ### needed by our density-ratio-model-based method


##################################################################################
##################################################################################
##################       Examples Using Simulated Data             ###############
no.y <- 5                 ### number of years
no.c <- 36                ### number of mills (clusters) each year
no.d <- 6                 ### number of clusters drapped and added each year

size.c <- 10               ### cluster size
sig1 <- rep(1, no.y)      ### sigma_{i1},  i=0, 1, ..., k-1
sig2 <- rep(1, no.y)      ### sigma_{i2},  i=0, 1, ..., k-1
sig3 <- rep(2, no.y)
cluster.total <- no.c+(no.y-1)*no.d ###  total number of clusters
sz <- rep(size.c, cluster.total)      ###  a vector consisting of the sizes of all clusters

indicator <- NULL
for(i in 1:no.y){
  tmp <- c(rep(0, (i-1)*no.d),rep(1, no.c), rep(0, (no.y-i)*no.d))
  indicator <- rbind(indicator, tmp)
}

year.comp <- c(1, 2)       ### which two populations are compared
###          must be in an increasig order
alpha.level <- c(0.05, 0.5)  ### at which levels quantiles are of interest


no.perm <- 6             ###  number of permutations, (small number for illustration purpose)
nrep <- 2             ###  number of simulation repetitions, (small number for illustration purpose)
pop.n <- 2               ###  it has four choices, -1,  0,  1,  2


#  mu <- c(8, 8-0.4*pop.n, 8+0.5*rnorm(no.y-2))
mu <- c(8, 8+ 0.4, 7.362115 , 8.649618, 7.170385)  ### A realization of the above vector

start.time <- proc.time()
for(i in 1:nrep) {
  set.seed(123)
  data <- NormalObs_gen(mu, sig1, sig2, sig3, indicator, sz)

  # Asymptotic tests
  result1 <- t_mean_clus(data,c(1,2), asymptotic = TRUE)
  result2 <- w_mean_clus(data,c(1,2), asymptotic = TRUE)

  result3 <- tmod_mean_clus(data,c(1,2))
  result4 <- t_quantile_clus(data,c(1,2), alpha.level = alpha.level)

  # Permutation tests, (default to asked = 1)
  result5 <- permutation_test(6,data,c(1,2), BasisFun =NULL, alpha.level = NULL, t_mean_clus, 't.pm', 'mean')
  result6 <- permutation_test(6,data,c(1,2), BasisFun =NULL, alpha.level = NULL, w_mean_clus, 'w.pm', 'mean')
  result7 <- permutation_test(6,data,c(1,2), BasisFun      , alpha.level = NULL, lrt_mean_stat, 'lrt.pm', 'mean')
  result8 <- permutation_test(6,data,c(1,2), BasisFun =NULL, alpha.level       , em_quantile_stat, 'q.em.pm', 'quantile')
  result9 <- permutation_test(6,data,c(1,2), BasisFun      , alpha.level       , el_quantile_stat, 'q.el.pm', 'quantile')
  result10 <- permutation_test(6,data,c(1,2), BasisFun      , alpha.level       , lrt_quantile_stat, 'q.lrt.pm', 'quantile')
  result <- rbind(result1, result2, result3, result4, result5, result6, result7, result8, result9, result10)
  print(result)
}
end.time <- proc.time()
cpu.time <- end.time-start.time
print(cpu.time)


##################################################################################
##################################################################################
##################       Examples Using Real/User-Input Data       ###############
Year <- c(rep("2019",8),rep("2020",7), rep("2021", 1))
ID <- c(rep(9,3),rep(10,2),rep(11,2),rep(14,1),rep(11,2),rep(14,1),rep(4,2),rep(2,2),rep(3,1))
Value <- runif(16)
inputdata <- data.frame(Year, ID, Value)

realdata <- reformat(inputdata, c("2019","2020"))

tmod_mean_clus(realdata, year.comp = c(1,2))

permutedrealdata <- permute_data_rs(realdata,c(1,2), asked = 1)

permutation_test(6, realdata, year.comp = c(1,2), alpha.level = c(0.05,0.5),
                 FUN = em_quantile_stat, nm = 'q.em.pm', param = 'quantile', asked = 0)
                 
BasisFun = function(x){ cbind(x, x^2) }
permutation_test(6, realdata, year.comp = c(1,2), BasisFun, c(0.05,0.5),
                 FUN = lrt_quantile_stat, nm = 'q.lrt.pm', param = 'quantile')
                 
```

## Sample Images of the R Package Permutest
![alt text](https://github.com/miyading/final-report/blob/master/Screen%20Shot%202021-08-19%20at%2011.20.07%20PM.png)
![alt text](https://github.com/miyading/final-report/blob/master/Screen%20Shot%202021-08-19%20at%2011.20.18%20PM.png)
![alt text](https://github.com/miyading/final-report/blob/master/Screen%20Shot%202021-08-19%20at%2011.20.53%20PM.png)
