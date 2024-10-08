#----------------------------------------------------------------------------------
#                               bayesTFR function
#----------------------------------------------------------------------------------
# This script creates the bayesTFR function which will estimate the iTFR, xTFR, and 
# bayesTFR for any given age-structure input and an associated q5 mortality.
#
# To run this script, Stan must be installed on the computer. (http://mc-stan.org/)
# 
# Data required are outlined in the sample_data tibble.
# 
# Authors: Mathew E. Hauer and Carl P. Schmertmann
# Last updated: 04/13/2018
#----------------------------------------------------------------------------------

library(tidyverse)
# library(dplyr)
library(rstan)

## To demonstrate all five iTFR variants, we use data taken from Tables 5.2, 5.3, and 5.4 in The Demography of Roman Egypt by Bagnall and Frier.
## Roman Egypt SIM is their simulated population based on table 5.4.
## Roman Egypt REAL is the surviving age-sex distribution based on table 5.2. 
## The qx values used are based on coale-demeny life tables and located in table 5.3 and calculated as l5/l0.
## The listed year in the sample_data tibble is the mid-point of the period from which ancient Roman Egyptian census returns are available,
##    namely, AD 12 and AD 257.

sample_data <- tribble(
  ~Code, 	~Year, 	~C, 	~W15, 	~W20, 	~W25, 	~W30, 	~W35, 	~W40, 	~W45, 	~qx, 
  # "Roman Egypt SIM", 	135, 	610003, 	202830, 	185728, 	167675, 	149541, 	131736, 	115088, 	99721, 	0.45, 
  # "Roman Egypt REAL", 	135, 	87.74, 	38.5, 	31.17, 	26.93, 	29.05, 	28.25, 	18.76, 	15.11, 	0.45,
  "Roman Egypt Metropolis", 135, 34, 11, 20, 18, 18, 22,  9,  3,  0.45,
  "Roman Egypt Villages",   135, 52, 21, 15, 13,  6,  9,  9, 11,  0.45,
  "Roman Egypt Overall",    135, 86, 32, 35, 31, 24, 31, 18, 14,  0.45
  
)

bayesTFR= function(D){
load(file='./R/DATA-RAW/svd.constants.RData')
m = svd.constants$m
X = svd.constants$X

D <- D %>%
  mutate(W1549 = W15 + W20 + W25 + W30 + W35 + W40 + W45,
         p2534 = (W25+W30)/W1549,
         CWR_hat = (C/(1-qx)) / W1549,
         CWR = C/W1549,
         iTFR_hat = 7*CWR_hat,
         xTFR_hat = (10.65 - 12.55*p2534)*CWR_hat,
         iTFR = 7 * CWR,
         xTFR = (10.65 - 12.55*p2534)*CWR)

#------------- MORTALITY model ------------------

# calculate a and b coeffs for each q5 (2 x 159)
ab = sapply(D$qx, function(this.q) {
  LearnBayes::beta.select( list(x= this.q/2, p=.05), list(x=this.q*2, p=.95))
})

q5_a = ab[1,]
q5_b = ab[2,]

##--- Wilmoth et al. coefficients from Pop Studies
wilmoth = 
  read.csv(text = '
           age,am,bm,cm,vm,af,bf,cf,vf
           0,  -0.5101, 0.8164,-0.0245,     0,-0.6619, 0.7684,-0.0277,     0
           1,      -99,    -99,    -99,   -99,    -99,    -99,    -99,   -99
           5,  -3.0435, 1.5270, 0.0817,0.1720,-2.5608, 1.7937, 0.1082,0.2788
           10, -3.9554, 1.2390, 0.0638,0.1683,-3.2435, 1.6653, 0.1088,0.3423
           15, -3.9374, 1.0425, 0.0750,0.2161,-3.1099, 1.5797, 0.1147,0.4007
           20, -3.4165, 1.1651, 0.0945,0.3022,-2.9789, 1.5053, 0.1011,0.4133
           25, -3.4237, 1.1444, 0.0905,0.3624,-3.0185, 1.3729, 0.0815,0.3884
           30, -3.4438, 1.0682, 0.0814,0.3848,-3.0201, 1.2879, 0.0778,0.3391
           35, -3.4198, 0.9620, 0.0714,0.3779,-3.1487, 1.1071, 0.0637,0.2829
           40, -3.3829, 0.8337, 0.0609,0.3530,-3.2690, 0.9339, 0.0533,0.2246
           45, -3.4456, 0.6039, 0.0362,0.3060,-3.5202, 0.6642, 0.0289,0.1774
           50, -3.4217, 0.4001, 0.0138,0.2564,-3.4076, 0.5556, 0.0208,0.1429
           55, -3.4144, 0.1760,-0.0128,0.2017,-3.2587, 0.4461, 0.0101,0.1190
           60, -3.1402, 0.0921,-0.0216,0.1616,-2.8907, 0.3988, 0.0042,0.0807
           65, -2.8565, 0.0217,-0.0283,0.1216,-2.6608, 0.2591,-0.0135,0.0571
           70, -2.4114, 0.0388,-0.0235,0.0864,-2.2949, 0.1759,-0.0229,0.0295
           75, -2.0411, 0.0093,-0.0252,0.0537,-2.0414, 0.0481,-0.0354,0.0114
           80, -1.6456, 0.0085,-0.0221,0.0316,-1.7308,-0.0064,-0.0347,0.0033
           85, -1.3203,-0.0183,-0.0219,0.0061,-1.4473,-0.0531,-0.0327,0.0040
           90, -1.0368,-0.0314,-0.0184,     0,-1.1582,-0.0617,-0.0259,     0
           95, -0.7310,-0.0170,-0.0133,     0,-0.8655,-0.0598,-0.0198,     0
           100,-0.5024,-0.0081,-0.0086,     0,-0.6294,-0.0513,-0.0134,     0
           105,-0.3275,-0.0001,-0.0048,     0,-0.4282,-0.0341,-0.0075,     0
           110,-0.2212,-0.0028,-0.0027,     0,-0.2966,-0.0229,-0.0041,     0
           ')

af = wilmoth$af[1:11]  # keep age 0,1,...45 
bf = wilmoth$bf[1:11]  # keep age 0,1,...45 
cf = wilmoth$cf[1:11]  # keep age 0,1,...45 
vf = wilmoth$vf[1:11]  # keep age 0,1,...45 

########################## INDEP STAN MODEL ##############################################
#------------ STAN MODEL --------------------
stanModelText = '
data {
    int<lower=0>        C;  // observed number of children 0-4
    vector<lower=0>[7]  W;  // observed number of women 15-19...45-49
    
    real q5_a;              // prior will be q(5) ~ beta( q5_a, q5_b) 
    real q5_b;         
    
    real af[11];        // 11 age groups starting at x=0,1,5,10,...,45
    real bf[11];        // 11 age groups starting at x=0,1,5,10,...,45
    real cf[11];        // 11 age groups starting at x=0,1,5,10,...,45
    real vf[11];        // 11 age groups starting at x=0,1,5,10,...,45
    
    vector[7]     m;       // mean vector for gamma
    matrix[7,2]   X;       // covariance matrix for gamma
    
}                      

parameters {
    real<lower=0,upper=1> q5;    // h = log(q5) in the Wilmoth et al. system
    real                   k;    // k = Wilmoth et al. shape parameter
    
    vector[2]     beta; 
    real<lower=0> TFR; 

}

transformed parameters {
    vector[7]             gamma;
    real<upper=0>         h;        // log(q5)
    simplex[7]            phi;      // proportion of total fertility by age group 
    vector[8]             Fx;       // age-group fertility rates F10...F45  (F10=0)
    real<lower=0>         mx[11];   // mortality rates for age groups starting at 0,1,5,10,...45
    real<lower=0,upper=1> lx[12];   // life table {lx} values for 0,1,5...,50
    real<lower=0,upper=5> Lx[10];   // life table {5Lx} values for 0,5...,45
    
    vector[7] Kx;                   // expected surv 0-4 yr olds per woman aged x to x+4
    
    real Kstar;                     // expected surviving total number of children               
    
    //--- child mortality index for Wilmoth model
    h = log(q5);
    
    //--- fertility rates
    
    gamma = m + X * beta;
    for (i in 1:7) phi[i] = exp(gamma[i]) / sum( exp(gamma));
    
    Fx[1] = 0;                                                // F10
    for (i in 2:8) Fx[i]   = TFR * phi[i-1] / 5;              // F15...F45 
    
    //--- mortality rates, life table survival probs, and big L values
    
    for (i in 1:11) {  mx[i]  = exp( af[i] + bf[i]*h + cf[i]*square(h) + vf[i]*k ); }
    
    mx[2]  = -0.25 * (mx[1] + log(1-q5) );               //  recalculate 1_mu_4 = -1/4 log(l[5]/l[1])
    
    lx[1]  = 1;                                          // x=0
    lx[2]  = lx[1] * exp(-mx[1]);                        // x=1
    lx[3]  = lx[2] * exp(-4*mx[2]);                      // x=5
    for (i in 3:12) lx[i]  = lx[i-1] * exp(-5*mx[i-1]);  // x=5,10,...50
    
    Lx[1]                  = 1* (lx[1]+lx[2])/2 + 4*(lx[2]+lx[3])/2 ;   // 5L0
    for (i in 2:10) Lx[i]  = 5* (lx[i+1]+lx[1+2])/2 ;                   // 5Lx
    
    //--- main result: expected surviving 0-4 yr olds per woman in each age group
    //      indexing is a bit complicated: 
    //       Lx[1:10] is for age groups 0,5,...,45
    //       Fx[1:8]  is for age groups 10,15,...,45
    //       Kx[1:7]  is for age groups 15,...,45
    
    for (i in 1:7) Kx[i]  = (Lx[i+2]/Lx[i+3] * Fx[i] + Fx[i+1]) * Lx[1]/2 ;
    
    Kstar  = dot_product(W, Kx);
}


model {

    // LIKELIHOOD
    C ~ poisson(Kstar);
    
    // PRIORS 
    beta  ~ normal(0,1); 
    
    q5 ~ beta(q5_a, q5_b);   // 90% prior prob. that q5 is between 1/2 and 2x estimated q5
    
    k  ~ normal(0,1); 

}'
  

MODEL = stan_model(model_code=stanModelText, model_name='single schedule TFR')


## construct the matrix of constants for cumulative hazard calculations
n = c(1,5, rep(5,9))    # widths of life table age intervals for x=0,1,5,10...45
cs_constants = matrix(0, 11, 12)
for (j in 1:11) cs_constants[1:j,j+1] = head(n,j)

## construct the constants for the trapezoidal approx of L0...L45 from a row of l0,l1,l5,l10,...,l50
trapez_constants = matrix(0, 12, 10, 
                          dimnames=list(paste0('l', c(0,1,seq(5,50,5))), paste0('L', seq(0,45,5))))
trapez_constants[c('l0','l1','l5'), 'L0'] = c( 1/2, 5/2, 4/2)
for (j in 2:10) trapez_constants[j+1:2, j] = 5/2


stanInits = function(nchains=1) {
  L = vector('list',nchains)
  
  for (i in seq(L)) {
    L[[i]] =   list(
      q5 = rbeta(1, shape1=q5_a, shape2=q5_b),
      k  = rnorm(1, mean=0,sd=1),
      beta = runif(2, min=-.10, max=.10),
      TFR  = pmax( .10, rnorm(1, 2, sd=.50))
    )
  }
  return(L)
} # stanInits


############# LOOP OVER SCHEDULES #####################

start.time = Sys.time()

results = data.frame()

for (k in 1:nrow(D)) {
  
  print(paste('...starting', k, 'of', nrow(D) ))
  
  stanDataList = list(
    C = round(D$C[k]),
    W = as.numeric( D[k, paste0('W',seq(15,45,5))]),
    q5_a = q5_a[k],
    q5_b = q5_b[k],
    af = af,
    bf = bf,
    cf = cf,
    vf = vf,
    cs_constants = cs_constants,
    trapez_constants = trapez_constants,
    m = m,
    X = t(X)   # 7x2 in this version
  )
  
  
  #------------ MCMC --------------------
  
  nchains = 4
  
  fit = sampling(MODEL, 
                 data       = stanDataList,
                 pars       = c('TFR','beta','q5'),
                 init       = stanInits(nchains),
                 seed       = 6447100,
                 iter       = 900,
                 warmup     = 300,
                 thin       = 4,
                 chains     = nchains,
                 control    = list(max_treedepth = 12))
  
  
  tmp = as.data.frame( summary(fit, 'TFR', probs=c(.10,.25,.50,.75,.90))$summary )
  
  names(tmp) = c('post_mean','se_mean','sd',paste0('Q',c(10,25,50,75,90)),'n_eff','Rhat')
  
  tmp$Code      = D$Code[k]  
  tmp$Year      = D$Year[k]
  tmp$iTFR_hat  = D$iTFR_hat[k]
  tmp$xTFR_hat  = D$xTFR_hat[k]
  tmp$iTFR      = D$iTFR[k]
  tmp$xTFR      = D$xTFR[k]
  
  tmp = dplyr::select(tmp, Code: xTFR, post_mean, contains('Q'),n_eff,Rhat) %>%
    mutate_at(vars(iTFR:Rhat), round, digits=3) 
  
  results = rbind( results, tmp)
  
} # for k

rownames(results) = NULL
return(results)}

a <- bayesTFR(sample_data)

write_rds(a, "./R/DATA-PROCESSED/RomanResults.rds")
