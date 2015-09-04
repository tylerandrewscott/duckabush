library(lme4)
library(gam)
setwd('H:/duckabush')
load("dat.lmer.mods.RData")

dat<-as.data.frame(fdat)
dat$STRAHLER<-ifelse(dat$STRAHLER>5,5,dat$STRAHLER)
dat$STRAHLER<-ifelse(dat$STRAHLER==0,1,dat$STRAHLER)

temp2<-glmer((ACTIVED>=3)~huc8crop+huc8past+HUC8AreaSqKm+
               POPDENS+huc8imperv+
               huc8npdespermits+huc8npdesenforceratio+
               county.med.income.est.mc+(1|HUC4)+(1|YEAR), family=binomial,data=dat)
l<-predict(temp2,type='response')

temp3<-glmer(ACTIVED~huc8crop+huc8past+HUC8AreaSqKm+
               POPDENS+huc8imperv+
               huc8npdespermits+huc8npdesenforceratio+
               county.med.income.est.mc+(1|HUC4)+(1|YEAR), family=gaussian,data=dat)
m<-predict(temp3,type='response')



table(dat$HUC4,dat$ACTIVE_bin)

head(dat)
names(dat)

table(dat$FW_ECO3,dat$ACTIVE_bin)
table(dat$STRAHLER,dat$ACTIVE_bin)

library(plyr)
library(nonrandom)
library(CBPS)

library(mgcv)
help(package='mgcv')
rm(list=ls())
library(nlme)
library(mgcv)

library(CausalGAM)  

dat<-dat[is.na(dat$PTL)==FALSE,]
tempdat<-dat[,c('ACTIVE_bin','huc8crop','huc8past','HUC8AreaSqKm','ACT3',
                'POPDENS','huc8imperv','FW_ECO3','STRAHLER',
                'huc8npdespermits','huc8npdesenforceratio',
                'county.med.income.est.mc','PTL','HUC4','STATE','post','XWIDTH',
                'XELEV','YEAR','STRAHLER','PWETL','PFOR','RDDENS','POPDENS','W1_HAG','W1_HNOAG',
                'State.NR.Pay.Ratio','county.nat.work.est.prop','PERCENT_DE','ACTIVED')]
tempdat$STRAHLER<-as.factor(tempdat$STRAHLER) 
                ,'NTL','TURB','ACTIVED',
                'XFC_NAT','MMI_BENT','XCMGW')]
rm(list=ls()[ls()!='tempdat'&ls()!='dat'])

#formp<-ACTIVE_bin~huc8crop+huc8past+HUC8AreaSqKm+
  POPDENS+huc8imperv+
  huc8npdespermits+huc8npdesenforceratio+
  county.med.income.est.mc+post

#formc<-PTL~XWIDTH+XELEV+HUC8AreaSqKm+
  STRAHLER+huc8imperv+huc8crop+huc8past+
  PWETL+PFOR+RDDENS+POPDENS+W1_HAG+W1_HNOAG+State.NR.Pay.Ratio+
  county.med.income.est.mc+
  county.nat.work.est.prop+PERCENT_DE

#formt<-PTL~XWIDTH+XELEV+HUC8AreaSqKm+
  STRAHLER+huc8imperv+huc8crop+huc8past+
  PWETL+PFOR+RDDENS+POPDENS+W1_HAG+W1_HNOAG+State.NR.Pay.Ratio+
  county.med.income.est.mc+
  county.nat.work.est.prop+PERCENT_DE
dat$ACT3<-dat$ACTIVED>=3
formp<-ACT3~huc8crop+huc8past+HUC8AreaSqKm+
POPDENS+huc8imperv+
  huc8npdespermits+huc8npdesenforceratio+
  county.med.income.est.mc

formc<-PTL~HUC8AreaSqKm+FW_ECO3+
STRAHLER+huc8imperv+huc8crop+huc8past+
  PWETL+PFOR+RDDENS+POPDENS+W1_HAG+W1_HNOAG

formt<-PTL~HUC8AreaSqKm+FW_ECO3+
STRAHLER+huc8imperv+huc8crop+huc8past+
  PWETL+PFOR+RDDENS+POPDENS+W1_HAG+W1_HNOAG

formc.ntl<-NTL~HUC8AreaSqKm+
  STRAHLER+huc8imperv+huc8crop+huc8past+
  PWETL+PFOR+RDDENS+POPDENS+W1_HAG+W1_HNOAG

formt.ntl<-NTL~HUC8AreaSqKm+
  STRAHLER+huc8imperv+huc8crop+huc8past+
  PWETL+PFOR+RDDENS+POPDENS+W1_HAG+W1_HNOAG

formc.turb<-TURB~HUC8AreaSqKm+
  STRAHLER+huc8imperv+huc8crop+huc8past+
  PWETL+PFOR+RDDENS+POPDENS+W1_HAG+W1_HNOAG

formt.turb<-TURB~HUC8AreaSqKm+
  STRAHLER+huc8imperv+huc8crop+huc8past+
  PWETL+PFOR+RDDENS+POPDENS+W1_HAG+W1_HNOAG

tempdat$ACT3
source('estimate.ATE.gamm4.R')
library(gamm4)
library(doParallel)
test<-estimate.ATE.gamm4(pscore.formula=formp, pscore.family=binomial,
                              outcome.formula.t = formt, outcome.formula.c = formc, 
                         pscore.form.ranef=~(1|HUC4)+(1|YEAR), treat.form.ranef=~(1|HUC4)+(1|YEAR), 
                         cont.form.ranef=~(1|HUC4)+(1|YEAR),ceiling=20,
                              outcome.family=gaussian, treatment.var='ACTIVE_bin', 
                              data = tempdat[sample(1:nrow(tempdat),nrow(tempdat),replace=T),],
   divby0.action = c("truncate"), divby0.tol = 0.05,
                         nboot = 500, 
                              variance.smooth.deg = 1, variance.smooth.span = 0.75, var.gam.plot = TRUE, 
                              suppress.warnings = FALSE) 

empty<-rep(0,nrow(tempdat))
for (i in 1:length(tempdat$YEAR))
{
  temp<-subset(tempdat,tempdat$YEAR<=tempdat$YEAR[i])
  nobs<-sum(temp$STATE==tempdat$STATE[i])
  if(nobs!=0)
  {
    value<-mean(temp$ACTIVED[temp$STATE==tempdat$STATE[i]])
  }
  else{value=0}
  empty[i]<-value
}
tempdat$state.value<-empty


temp1<-gamm4(formp,family=binomial,random=~(1|STATE)+(1|YEAR),data=tempdat)
temp2<-glmer((ACTIVED>=3)~huc8crop+huc8past+HUC8AreaSqKm+
              POPDENS+huc8imperv+
              huc8npdespermits+huc8npdesenforceratio+
              county.med.income.est.mc+(1|HUC$4)+(1|YEAR), family=binomial,data=tempdat)
l<-predict(temp2,type='response')
ggplot()+geom_histogram(aes(x=l,fill=tempdat$ACTIVED>=3),position='dodge')



tt<-density(l,bw='SJ',kernel='epanechnikov')

tkdensity(y=l,kernel='epanechnikov')

?tkdensity

sum(tt$y)
tt$weights

sum(temp1[[2]]$fitted.values>0.90)
sum(temp1[[2]]$fitted.values<0.10)
class(temp1)
class(temp1[[1]])
summary(temp1[[1]])
summary(temp2)





state.mean.duration<-stack(tapply(tempdat$ACTIVED,tempdat$STATE,mean));colnames(state.mean.duration)<-c('mean.dat','STATE')
tempdat<-join(tempdat,state.mean.duration)

source('estimate.ATE.gamm4.cont.R')
summary(temp[[1]])

tapply(temp[[2]]$fitted.values,tempdat$ACTIVED>=3,mean)

hist(temp[[2]]$fitted.values)
getwd()
source('estimate.ATE.gamm4.R')




warnings()
out <- estimate.ATE.gamm4(pscore.formula = pscore.formula, 
                          pscore.form.ranef, cont.form.ranef,treat.form.ranef,
                          pscore.family = pscore.family, outcome.formula.t = outcome.formula.t, 
                          outcome.formula.c = outcome.formula.c, outcome.family = outcome.family, 
                          treatment.var = treatment.var, variance.smooth.deg = variance.smooth.deg, 
                          variance.smooth.span = variance.smooth.span, 
                          data = data.bs, divby0.action = divby0.action, 
                          divby0.tol = divby0.tol, nboot = 0, var.gam.plot = FALSE, 
                          suppress.warnings = suppress.warnings, zzzzzbsfitzzzzz = TRUE)


tlist
warnings()
q<-c(-7.336477,-165.1292,-1.488624,-1.024874,-1.027021,-2.087589,0.02388376,-2.343616,-46.71664,-0.7599619)
test
sd(q)
?estimate.ATE
library(sfsmisc)
library(tcltk)






ntl.aipw<-estimate.ATE.gamm4(pscore.formula=formp, pscore.family=binomial,
                         outcome.formula.t = formt.ntl, outcome.formula.c = formc.ntl, 
                         # pranform=~(1|HUC4)+(1|STATE), tranform=~(1|HUC4)+(1|YEAR)+(1|STATE), 
                         pscore.form.ranef=~(1|HUC4)+(1|YEAR), treat.form.ranef=~(1|HUC4)+(1|STATE)+(1|YEAR), 
                         cont.form.ranef=~(1|HUC4)+(1|STATE)+(1|YEAR),
                         outcome.family=gaussian, treatment.var='ACTIVE_bin', 
                         data = tempdat, divby0.action = c("truncate"), divby0.tol = 1e-08,
                         nboot = 501, 
                         variance.smooth.deg = 1, variance.smooth.span = 0.75, var.gam.plot = TRUE, 
                         suppress.warnings = FALSE) 

turb.aipw<-estimate.ATE.gamm4(pscore.formula=formp, pscore.family=binomial,
                             outcome.formula.t = formt.turb, outcome.formula.c = formc.turb, 
                             # pranform=~(1|HUC4)+(1|STATE), tranform=~(1|HUC4)+(1|YEAR)+(1|STATE), 
                             pscore.form.ranef=~(1|HUC4)+(1|YEAR), treat.form.ranef=~(1|HUC4)+(1|STATE)+(1|YEAR), 
                             cont.form.ranef=~(1|HUC4)+(1|STATE)+(1|YEAR),
                             outcome.family=gaussian, treatment.var='ACTIVE_bin', 
                             data = tempdat, divby0.action = c("truncate"), divby0.tol = 1e-08,
                             nboot = 501, 
                             variance.smooth.deg = 1, variance.smooth.span = 0.75, var.gam.plot = TRUE, 
                             suppress.warnings = FALSE) 

turb.aipw<-estimate.ATE.gamm4(pscore.formula=formp, pscore.family=binomial,
                              outcome.formula.t = formt.turb, outcome.formula.c = formc.turb, 
                              # pranform=~(1|HUC4)+(1|STATE), tranform=~(1|HUC4)+(1|YEAR)+(1|STATE), 
                              pscore.form.ranef=~(1|HUC4)+(1|YEAR), treat.form.ranef=~(1|HUC4)+(1|STATE)+(1|YEAR), 
                              cont.form.ranef=~(1|HUC4)+(1|STATE)+(1|YEAR),
                              outcome.family=gaussian, treatment.var='ACTIVE_bin', 
                              data = tempdat, divby0.action = c("truncate"), divby0.tol = 1e-08,
                              nboot = 501, 
                              variance.smooth.deg = 1, variance.smooth.span = 0.75, var.gam.plot = TRUE, 
                              suppress.warnings = FALSE) 




ntl.aipw
ptl.aipw
tapply(tempdat$NTL,tempdat$ACTIVE_bin,mean)


warnings()
formp
test1<-estimate.ATE(pscore.formula=formp, pscore.family=binomial,
                          outcome.formula.t = formt, outcome.formula.c = formc, 
                          outcome.family=gaussian, treatment.var='ACTIVE_bin', 
                          data = tempdat, divby0.action = c("truncate"), divby0.tol = 1e-08, nboot = 501, 
                          variance.smooth.deg = 1, variance.smooth.span = 0.75, var.gam.plot = TRUE, 
                          suppress.warnings = FALSE) 

test1
test

  ?estimate.ATE
check<-gamm4(ftemp , random=~(1|HUC4), family = gaussian, 
      data = treated.data, na.action = "na.omit")
mgcv::predict.gam(check[[2]],newdata=tempdat)

?eval

?gam::predict.gam
?mgcv::predict.gam
as.vector(c(1-check[[2]]$fitted.values))
treated.data$PTL
head(treated.data)

class(treated.data$HUC4)

ftemp<-PTL ~ XWIDTH + XELEV + YEAR + HUC8AreaSqKm + STRAHLER + 
  huc8imperv + huc8crop + huc8past + PWETL + PFOR + RDDENS + 
 POPDENS + W1_HAG + W1_HNOAG + State.NR.Pay.Ratio + county.med.income.est.mc + 
  county.nat.work.est.prop + PERCENT_DE



head(treated.data)



?lFormula
formt
formt
formp
treatment.vec<-tempdat$ACTIVE_bin
treatment.values <- sort(unique(treatment.vec))
treated.data <- tempdat[treatment.vec == treatment.values[2],]

gamm4(formt,data = )

  ?gamm4
  tempdat$ACTIVE_bin
  
  
?lFormula

?gamm4
estimate.ATE
CausalGAM



p.mod<-lmer(ACTIVED~huc8crop+huc8past+HUC8AreaSqKm+POPDENS+huc8imperv+
     huc8npdespermits+huc8npdesenforceratio+county.med.income.est.mc+(1|STATE)+(1|HUC4),data=dat)
mySumm <- function(.) {
  c(beta=fixef(.),sigma=sigma(.), sig01=sqrt(unlist(VarCorr(.))))
}


gmod<-glmer(ACTIVE_bin~huc8crop+huc8past+HUC8AreaSqKm+POPDENS+huc8imperv+
         huc8npdespermits+huc8npdesenforceratio+county.med.income.est.mc+(1|STATE)+(1|HUC4),
         family=binomial(link='logit'),data=dat)
glmod<-glmer(ACTIVED~huc8crop+huc8past+HUC8AreaSqKm+POPDENS+huc8imperv+
              huc8npdespermits+huc8npdesenforceratio+county.med.income.est.mc+(1|STATE)+(1|HUC4),
            family='gaussian',data=dat)

dat$w<-predict(gmod,type='response')




