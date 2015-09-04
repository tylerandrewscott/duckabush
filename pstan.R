setwd('//Users/TScott/Google Drive/duckabush')
load('dat.lmer.mods.RData')
rm(list=ls()[ls()!='fdat'])
dat=as.data.frame(fdat)
dat$ACT3<-ifelse(dat$ACTIVED>=3,1,0)
dat$STRAHLER<-ifelse(dat$STRAHLER>5,5,dat$STRAHLER)
dat$STRAHLER<-ifelse(dat$STRAHLER==0,1,dat$STRAHLER)

library(rstan)
set_cppo("fast")

shed_code <- '
  data {
    int<lower=0> J; // number of huc4s
real y[J]; // estimated treatment effects
real<lower=0> sigma[J]; // s.e. of effect estimates 
}
parameters {
real mu; 
real<lower=0> tau;
real eta[J];
}
transformed parameters {
real theta[J];
for (j in 1:J)
theta[j] <- mu + tau * eta[j];
}
model {
eta ~ normal(0, 1);
y ~ normal(theta, sigma);
}
'

length(unique(dat$HUC4))
schools_dat <- list(J = 145, 
                    y = c(28,  8, -3,  7, -1,  1, 18, 12),
                    sigma = c(15, 10, 16, 11,  9, 11, 10, 18))

fit <- stan(model_code = schools_code, data = schools_dat, 
            iter = 1000, chains = 4)
install.packages('rethinking')

library(devtools)

pform<-ACT3~huc8imperv+huc8crop+huc8past+HUC8AreaSqKm+POPDENS+
                     huc8npdespermits +(1|STATE)+(1|HUC4)
library(inline)
library(Rcpp)
library(inline) 
library(Rcpp)
library(rstan)
set_cppo('fast')
detach("package:rstan", unload = TRUE)
remove.packages('rstan')
if (!file.exists("~/.R/Makevars")) {
  cat('CXX=g++ -arch x86_64 -ftemplate-depth-256 -stdlib=libstdc++\n
       CXXFLAGS="-mtune=native  -O3 -Wall -pedantic -Wconversion"\n', 
      file="~/.R/Makevars");
} else {
  file.show("~/.R/Makevars");
}

options(repos = c(getOption("repos"), rstan = "http://wiki.rstan-repo.googlecode.com/git/"))
install.packages('rstan', type = 'source')



