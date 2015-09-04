rm(list=ls())
setwd('//Users/TScott/Google Drive/duckabush')
require(Matrix)
require(lme4)
#source('model_prep.R')
fdat<-read.csv('model.dat.csv')

head(fdat)
table(fdat$FORM_OBJECT)
temp<-fdat[fdat$ACTIVED>0,]
table(temp$FORM_OBJECT)

temp[temp$FORM_OBJECT=='0',]



length(unique(temp$CON_ID))




require(lme4)
require(pbkrtest)
require(boot)
require(LMERConvenienceFunctions)
#require(MCMCpack)
#require(arm)
#require(blme)
require(stargazer)
require(ggplot2)
require(GGally)
require(reshape2)
require(lme4)
require(compiler)
require(parallel)
require(boot)
require(stringr)
require(ggplot2)



mtest<-lm(PTL~ACTIVED,data=fdat)
plot(residuals(mtest)~fdat$ACTIVED)
cor.test(residuals(mtest),fdat$GROUP)



fdat_MMI_BENT<-subset(fdat,is.na(fdat$MMI_BENT)==FALSE)
fdat_XCMGW<-subset(fdat,is.na(fdat$XCMGW_stz)==FALSE)
#DIRECT MODELS




M_PTL_null <-lmer(PTL~XWIDTH+XELEV+as.factor(YEAR)+SQ_MILE+
                    STRAHLER+PURB+
                    PWETL+PFOR+RDDENS+POPDENS+W1_HAG+W1_HNOAG+
                     +resparks.ratio+
                     county.med.income.est.mc+
                     county.nat.work.est.prop+PERCENT_DE+
                    (1|HUC4)+(1|STATE),data=fdat,verbose=TRUE)

M_PTL_directD <- lmer(PTL~XWIDTH+XELEV+log(ACTIVED+0.01)+as.factor(YEAR)+SQ_MILE+
                        STRAHLER+PURB+
                        PWETL+PFOR+RDDENS+POPDENS+W1_HAG+W1_HNOAG+resparks.ratio+
                         county.med.income.est.mc+
                        county.nat.work.est.prop+PERCENT_DE+
                        (1|HUC4)+(1|STATE),data=fdat,verbose=TRUE)


M_NTL_null <- lmer(NTL~XWIDTH+XELEV+as.factor(YEAR)+SQ_MILE+
                        STRAHLER+PURB+
                        PWETL+PFOR+RDDENS+POPDENS+W1_HAG+W1_HNOAG+
                     resparks.ratio+
                     county.med.income.est.mc+
                     county.nat.work.est.prop+PERCENT_DE+
                        (1|HUC4)+(1|STATE),data=fdat,verbose=TRUE)

M_NTL_directD <- lmer(NTL~XWIDTH+XELEV+log(ACTIVED+0.01)+as.factor(YEAR)+SQ_MILE+
                         STRAHLER+PURB+
                         PWETL+PFOR+RDDENS+POPDENS+W1_HAG+W1_HNOAG+resparks.ratio+
                         county.med.income.est.mc+
                         county.nat.work.est.prop+PERCENT_DE+
                         (1|HUC4)+(1|STATE),data=fdat,verbose=TRUE)

M_TURB_null<- lmer(TURB~XWIDTH+XELEV+as.factor(YEAR)+SQ_MILE+
                         STRAHLER+PURB+
                         PWETL+PFOR+RDDENS+POPDENS+W1_HAG+W1_HNOAG+
                   resparks.ratio+
                     county.med.income.est.mc+
                     county.nat.work.est.prop+PERCENT_DE+
                         (1|HUC4)+(1|STATE),data=fdat,verbose=TRUE)


M_TURB_directD <- lmer(TURB~XWIDTH+XELEV+log(ACTIVED+0.01)+as.factor(YEAR)+SQ_MILE+
                         STRAHLER+PURB+
                         PWETL+PFOR+RDDENS+POPDENS+W1_HAG+W1_HNOAG+resparks.ratio+
                         county.med.income.est.mc+
                         county.nat.work.est.prop+PERCENT_DE+
                         (1|HUC4)+(1|STATE),data=fdat,verbose=TRUE)


M_XFC_NAT_null <- lmer(XFC_NAT~XWIDTH+XELEV+as.factor(YEAR)+SQ_MILE+
                            STRAHLER+PURB+
                            PWETL+PFOR+RDDENS+POPDENS+W1_HAG+W1_HNOAG+
                         resparks.ratio+
                         county.med.income.est.mc+
                         county.nat.work.est.prop+PERCENT_DE+
                            (1|HUC4)+(1|STATE),data=fdat,verbose=TRUE)

M_XFC_NAT_directD <- lmer(XFC_NAT~XWIDTH+XELEV+log(ACTIVED+0.01)+as.factor(YEAR)+SQ_MILE+
                         STRAHLER+PURB+
                         PWETL+PFOR+RDDENS+POPDENS+W1_HAG+W1_HNOAG+resparks.ratio+
                         county.med.income.est.mc+
                         county.nat.work.est.prop+PERCENT_DE+
                         (1|HUC4)+(1|STATE),data=fdat,verbose=TRUE)

M_MMI_BENT_null <- lmer(MMI_BENT~XWIDTH+XELEV+as.factor(YEAR)+SQ_MILE+
                             STRAHLER+PURB+
                             PWETL+PFOR+RDDENS+POPDENS+W1_HAG+W1_HNOAG+
                          resparks.ratio+
                          county.med.income.est.mc+
                          county.nat.work.est.prop+PERCENT_DE+
                             (1|HUC4)+(1|STATE),data=fdat_MMI_BENT,verbose=TRUE)

M_MMI_BENT_directD <- lmer(MMI_BENT~XWIDTH+XELEV+log(ACTIVED+0.01)+as.factor(YEAR)+SQ_MILE+
                             STRAHLER+PURB+
                             PWETL+PFOR+RDDENS+POPDENS+W1_HAG+W1_HNOAG+resparks.ratio+
                             county.med.income.est.mc+
                             county.nat.work.est.prop+PERCENT_DE+
                             (1|HUC4)+(1|STATE),data=fdat_MMI_BENT,verbose=TRUE)

M_XCMGW_null<- lmer(XCMGW~XWIDTH+XELEV+as.factor(YEAR)+SQ_MILE+
                          STRAHLER+PURB+
                          PWETL+PFOR+RDDENS+POPDENS+W1_HAG+W1_HNOAG+
                      resparks.ratio+
                      county.med.income.est.mc+
                      county.nat.work.est.prop+PERCENT_DE+
                          (1|HUC4)+(1|STATE),data=fdat_XCMGW,verbose=TRUE)

M_XCMGW_directD <- lmer(XCMGW~XWIDTH+XELEV+log(ACTIVED+0.01)+as.factor(YEAR)+SQ_MILE+
                              STRAHLER+PURB+
                              PWETL+PFOR+RDDENS+POPDENS+W1_HAG+W1_HNOAG+resparks.ratio+
                              county.med.income.est.mc+
                              county.nat.work.est.prop+PERCENT_DE+
                              (1|HUC4)+(1|STATE),data=fdat_XCMGW,verbose=TRUE)

stargazer(M_PTL_directD,M_NTL_directD,M_TURB_directD,
          M_MMI_BENT_directD,M_XCMGW_directD,M_XFC_NAT_directD,
          style='jpam')





corest<-c(cor.test(residuals(M_PTL_directD),fdat$ACTIVED)$estimate,
          cor.test(residuals(M_NTL_directD),fdat$ACTIVED)$estimate,
          cor.test(residuals(M_TURB_directD),fdat$ACTIVED)$estimate,
          cor.test(residuals(M_MMI_BENT_directD),fdat_MMI_BENT$ACTIVED)$estimate,
          cor.test(residuals(M_XCMGW_directD),fdat_XCMGW$ACTIVED)$estimate,
          cor.test(residuals(M_XFC_NAT_directD),fdat$ACTIVED)$estimate)

lbound<-c(cor.test(residuals(M_PTL_directD),fdat$ACTIVED)$conf.int[1],
          cor.test(residuals(M_NTL_directD),fdat$ACTIVED)$conf.int[1],
          cor.test(residuals(M_TURB_directD),fdat$ACTIVED)$conf.int[1],
          cor.test(residuals(M_MMI_BENT_directD),fdat_MMI_BENT$ACTIVED)$conf.int[1],
          cor.test(residuals(M_XCMGW_directD),fdat_XCMGW$ACTIVED)$conf.int[1],
          cor.test(residuals(M_XFC_NAT_directD),fdat$ACTIVED)$conf.int[1])
ubound<-c(cor.test(residuals(M_PTL_directD),fdat$ACTIVED)$conf.int[2],
          cor.test(residuals(M_NTL_directD),fdat$ACTIVED)$conf.int[2],
          cor.test(residuals(M_TURB_directD),fdat$ACTIVED)$conf.int[2],
          cor.test(residuals(M_MMI_BENT_directD),fdat_MMI_BENT$ACTIVED)$conf.int[2],
          cor.test(residuals(M_XCMGW_directD),fdat_XCMGW$ACTIVED)$conf.int[2],
          cor.test(residuals(M_XFC_NAT_directD),fdat$ACTIVED)$conf.int[2])
vars<-c('Nitrogen','Phosphorus','Turbidity','Benthic Index','Riparian Veg.','Fish Cover')

cis<-data.frame(corest,lbound,ubound,vars);colnames(cis)<-c('cor','lbound','ubound','Metric')

residual.cor.plot<-ggplot(cis)+
  geom_segment(aes(y=vars,yend=Metric,x=lbound,xend=ubound),
               lineend='round',lwd=2,colour='dark grey')+
  geom_point(aes(y=Metric,x=cor),size=6,
             lwd=4,colour='dark grey')+
  scale_x_continuous(expand=c(0,0),
                     name='Correlation between group presence and residuals',limits=c(-.25,.25))+
  scale_y_discrete(name='Metric',expand=c(0,.5))+theme_bw()

ggsave('residual.cor.plot.png',plot=residual.cor.plot)



#UN-RESTRICTED MODELS: FORMALIZATION
M_PTL_formD <- lmer(PTL~ ACTIVED+as.factor(YEAR)+XWIDTH+XELEV+SQ_MILE+STRAHLER+PURB+PWETL+
                      PFOR+RDDENS+POPDENS+W1_HAG+W1_HNOAG+
                      resparks.ratio+
                      county.med.income.est.mc+
                      county.nat.work.est.prop+PERCENT_DE+
                      ACTIVED:FACILITATE+
                      ACTIVED:GOALS+
                      ACTIVED:OBJECTIVES+
                      ACTIVED:HAS_BYLAWS+
                      (1|HUC4)+(ACTIVED|STATE),verbose=TRUE,data=fdat)


modsum_PTL_formD<-summary(M_PTL_formD,signature=signature(object = "merModLmerTest"))
#write.csv(ranef(M_PTL_formD)[2], "//Users/TScott/Google Drive/duckabush/ptl_ranef_form_D.csv")


M_NTL_formD <- lmer(NTL~ ACTIVED+as.factor(YEAR)+XWIDTH+XELEV+SQ_MILE+STRAHLER+PURB+
                      PWETL+PFOR+RDDENS+POPDENS+W1_HAG+W1_HNOAG+
                      resparks.ratio+
                      county.med.income.est.mc+
                      county.nat.work.est.prop+PERCENT_DE+
                      ACTIVED:FACILITATE+
                      ACTIVED:GOALS+
                      ACTIVED:OBJECTIVES+
                      ACTIVED:HAS_BYLAWS+
                      (1|HUC4)+(ACTIVED|STATE),verbose=TRUE,data=fdat)
modsum_NTL_formD<-summary(M_NTL_formD,signature=signature(object = "merModLmerTest"))
#write.csv(ranef(M_NTL_formD)[2], "//Users/TScott/Google Drive/duckabush/ntl_ranef_form_D.csv")

M_TURB_formD <- lmer(TURB~ ACTIVED+as.factor(YEAR)+XWIDTH+XELEV+SQ_MILE+STRAHLER+
                       PURB+PWETL+PFOR+RDDENS+POPDENS+W1_HAG+W1_HNOAG+
                       resparks.ratio+
                       county.med.income.est.mc+
                       county.nat.work.est.prop+PERCENT_DE+
                       ACTIVED:FACILITATE+
                       ACTIVED:GOALS+
                       ACTIVED:OBJECTIVES+
                       ACTIVED:HAS_BYLAWS+
                       (1|HUC4)+(ACTIVED|STATE),verbose=TRUE,data=fdat)
modsum_TURB_formD<-summary(M_TURB_formD,signature=signature(object = "merModLmerTest"))
#write.csv(ranef(M_TURB_formD)[2], "//Users/TScott/Google Drive/duckabush/turb_ranef_form_D.csv")

M_XFC_NAT_formD <- lmer(XFC_NAT~ ACTIVED+as.factor(YEAR)+XWIDTH+XELEV+SQ_MILE+STRAHLER+
                          PURB+PWETL+PFOR+RDDENS+POPDENS+W1_HAG+W1_HNOAG+
                          resparks.ratio+
                          county.med.income.est.mc+
                          county.nat.work.est.prop+PERCENT_DE+
                          ACTIVED:FACILITATE+
                          ACTIVED:GOALS+
                          ACTIVED:OBJECTIVES+
                          ACTIVED:HAS_BYLAWS+
                          (1|HUC4)+(ACTIVED|STATE),verbose=TRUE,data=fdat)
modsum_XFC_NAT_formD<-summary(M_XFC_NAT_formD,signature=signature(object = "merModLmerTest"))
#write.csv(ranef(M_XFC_NAT_formD)[2], "//Users/TScott/Google Drive/duckabush/xfc_ranef_form_D.csv")

M_MMI_BENT_formD <- lmer(MMI_BENT~ACTIVED+as.factor(YEAR)+XWIDTH+XELEV+SQ_MILE+STRAHLER+
                           PURB+PWETL+PFOR+RDDENS+POPDENS+W1_HAG+W1_HNOAG+
                           resparks.ratio+
                           county.med.income.est.mc+
                           county.nat.work.est.prop+PERCENT_DE+
                           ACTIVED:FACILITATE+
                           ACTIVED:GOALS+
                           ACTIVED:OBJECTIVES+
                           ACTIVED:HAS_BYLAWS+
                           (1|HUC4)+(ACTIVED|STATE),verbose=TRUE,data=fdat_MMI_BENT)
modsum_MMI_BENT_formD<-summary(M_MMI_BENT_formD,signature=signature(object = "merModLmerTest"))
#write.csv(ranef(M_MMI_BENT_formD)[2], "//Users/TScott/Google Drive/duckabush/mmi_ranef_form_D.csv")

fdat_XCMGW<-subset(fdat,is.na(fdat$XCMGW)==FALSE)
M_XCMGW_formD <- lmer(XCMGW~ ACTIVED+as.factor(YEAR)+XWIDTH+XELEV+SQ_MILE+STRAHLER+
                        PURB+PWETL+PFOR+RDDENS+POPDENS+W1_HAG+W1_HNOAG+
                        resparks.ratio+
                        county.med.income.est.mc+
                        county.nat.work.est.prop+PERCENT_DE+
                        ACTIVED:FACILITATE+
                        ACTIVED:GOALS+
                        ACTIVED:OBJECTIVES+
                        ACTIVED:HAS_BYLAWS+
                        (1|HUC4)+(ACTIVED|STATE),verbose=TRUE,data=fdat_XCMGW)
modsum_XCMGW_formD<-summary(M_XCMGW_formD,signature=signature(object = "merModLmerTest"))
#write.csv(ranef(M_XCMGW_formD)[2], "//Users/TScott/Google Drive/duckabush/xcmgw_ranef_form_D.csv")



#UN-RESTRICTED MODELS: REPRESENTATION Duration
M_PTL_repD <- lmer(PTL~XWIDTH+XELEV+ACTIVED+as.factor(YEAR)+SQ_MILE+STRAHLER+PURB+
                     PWETL+PFOR+RDDENS++POPDENS+W1_HAG+W1_HNOAG+
                     resparks.ratio+
                     county.med.income.est.mc+
                     county.nat.work.est.prop+PERCENT_DE+
                     ACTIVED:CROSS_BOUND:TRANS_BOUND+ACTIVED:MEM_TOTAL+
                     TECH_BODY:ACTIVED+(1|HUC4)+(ACTIVED|STATE),data=fdat,verbose=TRUE)
modsum_PTL_repD<-summary(M_PTL_repD,signature=signature(object = "merModLmerTest"))

M_NTL_repD <- lmer(NTL~XWIDTH+XELEV+ACTIVED+as.factor(YEAR)+SQ_MILE+STRAHLER+
                     PURB+PWETL+PFOR+RDDENS++POPDENS+W1_HAG+W1_HNOAG+
                     resparks.ratio+
                     county.med.income.est.mc+
                     county.nat.work.est.prop+PERCENT_DE+
                     ACTIVED:CROSS_BOUND:TRANS_BOUND+ACTIVED:MEM_TOTAL+
                     TECH_BODY:ACTIVED+(1|HUC4)+(ACTIVED|STATE),data=fdat,verbose=TRUE)
modsum_NTL_repD<-summary(M_NTL_repD,signature=signature(object = "merModLmerTest"))


M_TURB_repD <- lmer(TURB~XWIDTH+XELEV+ACTIVED+as.factor(YEAR)+SQ_MILE+STRAHLER+PURB+
                      PWETL+PFOR+RDDENS++POPDENS+W1_HAG+W1_HNOAG+
                      resparks.ratio+
                      county.med.income.est.mc+
                      county.nat.work.est.prop+PERCENT_DE+
                      ACTIVED:CROSS_BOUND:TRANS_BOUND+ACTIVED:MEM_TOTAL+
                      TECH_BODY:ACTIVED+(1|HUC4)+(ACTIVED|STATE),data=fdat,verbose=TRUE)
modsum_TURB_repD<-summary(M_TURB_repD,signature=signature(object = "merModLmerTest"))
#write.csv(ranef(M_TURB_repD)[2], "//Users/TScott/Google Drive/duckabush/turb_ranef_rep_D.csv")

M_XFC_NAT_repD <- lmer(XFC_NAT~XWIDTH+XELEV+ACTIVED+as.factor(YEAR)+SQ_MILE+STRAHLER+PURB+
                         PWETL+PFOR+RDDENS++POPDENS+W1_HAG+W1_HNOAG+
                         resparks.ratio+
                         county.med.income.est.mc+
                         county.nat.work.est.prop+PERCENT_DE+
                         ACTIVED:CROSS_BOUND:TRANS_BOUND+ACTIVED:MEM_TOTAL+
                         TECH_BODY:ACTIVED+(1|HUC4)+(ACTIVED|STATE),data=fdat,verbose=TRUE)
modsum_XFC_NAT_repD<-summary(M_XFC_NAT_repD,signature=signature(object = "merModLmerTest"))

fdat_MMI_BENT<-subset(fdat,is.na(fdat$MMI_BENT)==FALSE)
M_MMI_BENT_repD <- lmer(MMI_BENT~XWIDTH+XELEV+ACTIVED+as.factor(YEAR)+SQ_MILE+STRAHLER+PURB+PWETL+
                          PFOR+RDDENS++POPDENS+W1_HAG+W1_HNOAG+
                          resparks.ratio+
                          county.med.income.est.mc+
                          county.nat.work.est.prop+PERCENT_DE+
                          ACTIVED:CROSS_BOUND:TRANS_BOUND+ACTIVED:MEM_TOTAL+
                          TECH_BODY:ACTIVED+(1|HUC4)+(ACTIVED|STATE),data=fdat_MMI_BENT,verbose=TRUE)
modsum_MMI_BENT_repD<-summary(M_MMI_BENT_repD,signature=signature(object = "merModLmerTest"))


fdat_XCMGW<-subset(fdat,is.na(fdat$XCMGW)==FALSE)
M_XCMGW_repD <- lmer(XCMGW~XWIDTH+XELEV+ACTIVED+as.factor(YEAR)+SQ_MILE+STRAHLER+PURB+PWETL+
                       PFOR+RDDENS++POPDENS+W1_HAG+W1_HNOAG+
                       resparks.ratio+
                       county.med.income.est.mc+
                       county.nat.work.est.prop+PERCENT_DE+
                       ACTIVED:CROSS_BOUND:TRANS_BOUND+ACTIVED:MEM_TOTAL+
                       TECH_BODY:ACTIVED+(1|HUC4)+(ACTIVED|STATE),data=fdat_XCMGW,verbose=TRUE)
modsum_XCMGW_repD<-summary(M_XCMGW_repD,signature=signature(object = "merModLmerTest"))


#GROUP RESPONSIBILITY
M_PTL_resD <- lmer(PTL~ ACTIVED+as.factor(YEAR)+XWIDTH+XELEV+SQ_MILE+STRAHLER+PURB+
                     PWETL+PFOR+RDDENS+POPDENS+W1_HAG+W1_HNOAG+
                     resparks.ratio+
                     county.med.income.est.mc+
                     county.nat.work.est.prop+PERCENT_DE+
                     ACTIVED:PLAN+
                     ACTIVED:MANAGE+
                     (1|HUC4)+(ACTIVED|STATE),verbose=TRUE,data=fdat)
modsum_PTL_resD<-summary(M_PTL_resD,signature=signature(object = "merModLmerTest"))
#write.csv(ranef(M_PTL_resD)[2], "//Users/TScott/Google Drive/duckabush/ptl_ranef_resD_D.csv")

M_NTL_resD <- lmer(NTL~ ACTIVED+as.factor(YEAR)+XWIDTH+XELEV+SQ_MILE+STRAHLER+PURB+PWETL+
                     PFOR+RDDENS+POPDENS+W1_HAG+W1_HNOAG+
                     resparks.ratio+
                     county.med.income.est.mc+
                     county.nat.work.est.prop+PERCENT_DE+
                     ACTIVED:PLAN+
                     ACTIVED:MANAGE+
                     (1|HUC4)+(ACTIVED|STATE),verbose=TRUE,data=fdat)
modsum_NTL_resD<-summary(M_NTL_resD,signature=signature(object = "merModLmerTest"))
#write.csv(ranef(M_NTL_resD)[2], "//Users/TScott/Google Drive/duckabush/ntl_ranef_resD_D.csv")

M_TURB_resD <- lmer(TURB~ ACTIVED+as.factor(YEAR)+XWIDTH+XELEV+SQ_MILE+STRAHLER+PURB+PWETL+
                      PFOR+RDDENS+POPDENS+W1_HAG+W1_HNOAG+
                      resparks.ratio+
                      county.med.income.est.mc+
                      county.nat.work.est.prop+PERCENT_DE+
                      ACTIVED:PLAN+
                      ACTIVED:MANAGE+
                      (1|HUC4)+(ACTIVED|STATE),verbose=TRUE,data=fdat)
modsum_TURB_resD<-summary(M_TURB_resD,signature=signature(object = "merModLmerTest"))
#write.csv(ranef(M_TURB_resD)[2], "//Users/TScott/Google Drive/duckabush/turb_ranef_resD_D.csv")

M_XFC_NAT_resD <- lmer(XFC_NAT~ ACTIVED+as.factor(YEAR)+XWIDTH+XELEV+SQ_MILE+STRAHLER+PURB+
                         PWETL+PFOR+RDDENS+POPDENS+W1_HAG+W1_HNOAG+
                         resparks.ratio+
                         county.med.income.est.mc+
                         county.nat.work.est.prop+PERCENT_DE+
                         ACTIVED:PLAN+
                         ACTIVED:MANAGE+
                         (1|HUC4)+(ACTIVED|STATE),verbose=TRUE,data=fdat)
modsum_XFC_NAT_resD<-summary(M_XFC_NAT_resD,signature=signature(object = "merModLmerTest"))
#write.csv(ranef(M_XFC_NAT_resD)[2], "//Users/TScott/Google Drive/duckabush/xfc_ranef_resD_D.csv")

fdat_MMI_BENT<-subset(fdat,is.na(fdat$MMI_BENT)==FALSE)
M_MMI_BENT_resD <- lmer(MMI_BENT~ACTIVED+as.factor(YEAR)+XWIDTH+XELEV+SQ_MILE+STRAHLER+PURB+
                          PWETL+PFOR+RDDENS+POPDENS+W1_HAG+W1_HNOAG+
                          resparks.ratio+
                          county.med.income.est.mc+
                          county.nat.work.est.prop+PERCENT_DE+
                          ACTIVED:PLAN+
                          ACTIVED:MANAGE+
                          (1|HUC4)+(ACTIVED|STATE),verbose=TRUE,data=fdat_MMI_BENT)
modsum_MMI_BENT_resD<-summary(M_MMI_BENT_resD,signature=signature(object = "merModLmerTest"))
#write.csv(ranef(M_MMI_BENT_resD)[2], "//Users/TScott/Google Drive/duckabush/mmi_ranef_resD_D.csv")

fdat_XCMGW<-subset(fdat,is.na(fdat$XCMGW)==FALSE)
M_XCMGW_resD <- lmer(XCMGW~ ACTIVED+as.factor(YEAR)+XWIDTH+XELEV+SQ_MILE+STRAHLER+PURB+PWETL+
                       PFOR+RDDENS+POPDENS+W1_HAG+W1_HNOAG+
                       resparks.ratio+
                       county.med.income.est.mc+
                       county.nat.work.est.prop+PERCENT_DE+
                       ACTIVED:PLAN+
                       ACTIVED:MANAGE+
                       (1|HUC4)+(ACTIVED|STATE),verbose=TRUE,data=fdat_XCMGW)
modsum_XCMGW_resD<-summary(M_XCMGW_resD,signature=signature(object = "merModLmerTest"))



