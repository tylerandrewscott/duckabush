rm(list=ls())
setwd('H:/duckabush')
require(Matrix)
#source('model_prep.R')
fdat<-read.csv('model.dat.csv')

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
require(psychometric)

ptl.icc<-ICC1.lme(PTL,paste(HUC4,STATE),fdat)
ntl.icc<-ICC1.lme(NTL,paste(HUC4,STATE),fdat)
turb.icc<-ICC1.lme(TURB,paste(HUC4,STATE),fdat)
xfc.icc<-ICC1.lme(XFC_NAT,paste(HUC4,STATE),fdat)
mmi.icc<-ICC1.lme(MMI_BENT,paste(HUC4,STATE),fdat)
xcm.icc<-ICC1.lme(XCMGW,paste(HUC4,STATE),fdat)

stargazer(ptl.icc,ntl.icc,turb.icc,xfc.icc,mmi.icc,xcm.icc,
        title='Intraclass Correlation Coefficients',
        digits=2,style='jpam')

xfc.icc

#DIRECT MODELS

M_PTL_directD <- lmer(PTL~XWIDTH+XELEV+log(ACTIVED+0.01)+as.factor(YEAR)+SQ_MILE+
                        STRAHLER+PURB+
                        PWETL+PFOR+RDDENS+POPDENS+W1_HAG+W1_HNOAG+
                        (1|HUC8)+(1|STATE),data=fdat,verbose=TRUE)

modsum_PTL_directD<-summary(M_PTL_directD,signature=signature(object = "merModLmerTest"))


#write.csv(ranef(M_PTL_directD)[2], "//Users/TScott/Google Drive/duckabush/ptl_ranef_dir_D.csv")

M_NTL_directD <- lmer(NTL~XWIDTH+XELEV+log(ACTIVED+1)+as.factor(YEAR)+SQ_MILE+STRAHLER+PURB+PWETL+PFOR+RDDENS+POPDENS+W1_HAG+W1_HNOAG+(1|HUC8)+(ACTIVED|STATE),data=fdat,verbose=TRUE)
modsum_NTL_directD<-summary(M_NTL_directD,signature=signature(object = "merModLmerTest"))





write.csv(ranef(M_NTL_directD)[2], "//Users/TScott/Google Drive/duckabush/ntl_ranef_dir_D.csv")
modsum_NTL_directD
M_TURB_directD <- lmer(TURB~XWIDTH+XELEV+ACTIVED+as.factor(YEAR)+SQ_MILE+STRAHLER+PURB+PWETL+PFOR+RDDENS+POPDENS+W1_HAG+W1_HNOAG+(1|HUC8)+(ACTIVED|STATE),data=fdat,verbose=TRUE)
modsum_TURB_directD<-summary(M_TURB_directD,signature=signature(object = "merModLmerTest"))
write.csv(ranef(M_TURB_directD)[2], "//Users/TScott/Google Drive/duckabush/turb_ranef_dir_D.csv")

M_XFC_NAT_directD <- lmer(XFC_NAT~XWIDTH+XELEV+ACTIVED+as.factor(YEAR)+SQ_MILE+STRAHLER+PURB+PWETL+PFOR+RDDENS+POPDENS+W1_HAG+W1_HNOAG+(1|HUC8)+(ACTIVED|STATE),data=fdat,verbose=TRUE)
modsum_XFC_NAT_directD<-summary(M_XFC_NAT_directD,signature=signature(object = "merModLmerTest"))
write.csv(ranef(M_XFC_NAT_directD)[2], "//Users/TScott/Google Drive/duckabush/xfc_ranef_dir_D.csv")

fdat_MMI_BENT<-subset(fdat,is.na(fdat$MMI_BENT)==FALSE)
M_MMI_BENT_directD <- lmer(MMI_BENT~XWIDTH+XELEV+ACTIVED+as.factor(YEAR)+SQ_MILE+STRAHLER+PURB+PWETL+PFOR+RDDENS+POPDENS+W1_HAG+W1_HNOAG+(1|HUC8)+(ACTIVED|STATE),data=fdat_MMI_BENT,verbose=TRUE)
modsum_MMI_BENT_directD<-summary(M_MMI_BENT_directD,signature=signature(object = "merModLmerTest"))
write.csv(ranef(M_MMI_BENT_directD)[2], "//Users/TScott/Google Drive/duckabush/mmi_ranef_dir_D.csv")

fdat_XCMGW<-subset(fdat,is.na(fdat$XCMGW_stz)==FALSE)
M_XCMGW_directD <- lmer(XCMGW~XWIDTH+XELEV+ACTIVED+as.factor(YEAR)+SQ_MILE+STRAHLER+PURB+PWETL+PFOR+RDDENS+POPDENS+W1_HAG+W1_HNOAG+(1|HUC8)+(ACTIVED|STATE),data=fdat_XCMGW,verbose=TRUE)
modsum_XCMGW_directD<-summary(M_XCMGW_directD,signature=signature(object = "merModLmerTest"))
write.csv(ranef(M_XCMGW_directD)[2], "//Users/TScott/Google Drive/duckabush/xcmgw_ranef_dir_D.csv")





#BASE MODELS
M_PTL_base <- lmer(PTL~XWIDTH+XELEV+as.factor(YEAR)+SQ_MILE+STRAHLER+PURB+PWETL+PFOR+RDDENS+POPDENS+W1_HAG+W1_HNOAG+(1|HUC8)+(1|STATE),data=fdat,verbose=TRUE)
modsum_PTL_base<-summary(M_PTL_base,signature=signature(object = "merModLmerTest"))
#write.csv(ranef(M_PTL_base)[2], "//Users/TScott/Google Drive/duckabush/ptl_ranef_D.csv")
bootMer(M_PTL_base,)
modsum_PTL_base

M_NTL_base <- lmer(NTL~XWIDTH+XELEV+as.factor(YEAR)+SQ_MILE+STRAHLER+PURB+PWETL+PFOR+RDDENS+POPDENS+W1_HAG+W1_HNOAG+(1|HUC8)+(1|STATE),data=fdat,verbose=TRUE)
modsum_NTL_base<-summary(M_NTL_base,signature=signature(object = "merModLmerTest"))
#write.csv(ranef(M_NTL_base)[2], "//Users/TScott/Google Drive/duckabush/ntl_ranef_D.csv")

M_TURB_base <- lmer(TURB~XWIDTH+XELEV+as.factor(YEAR)+SQ_MILE+STRAHLER+PURB+PWETL+PFOR+RDDENS+POPDENS+W1_HAG+W1_HNOAG+(1|HUC8)+(1|STATE),data=fdat,verbose=TRUE)
modsum_TURB_base<-summary(M_TURB_base,signature=signature(object = "merModLmerTest"))
#write.csv(ranef(M_TURB_base)[2], "//Users/TScott/Google Drive/duckabush/turb_ranef_D.csv")

M_XFC_NAT_base <- lmer(XFC_NAT~XWIDTH+XELEV+as.factor(YEAR)+SQ_MILE+STRAHLER+PURB+PWETL+PFOR+RDDENS+POPDENS+W1_HAG+W1_HNOAG+(1|HUC8)+(1|STATE),data=fdat,verbose=TRUE)
modsum_XFC_NAT_base<-summary(M_XFC_NAT_base,signature=signature(object = "merModLmerTest"))
#write.csv(ranef(M_XFC_NAT_base)[2], "//Users/TScott/Google Drive/duckabush/xfc_ranef_D.csv")


M_MMI_BENT_base <- lmer(MMI_BENT~XWIDTH+XELEV+as.factor(YEAR)+SQ_MILE+STRAHLER+PURB+PWETL+PFOR+RDDENS+POPDENS+W1_HAG+W1_HNOAG+(1|HUC8)+(1|STATE),data=fdat_MMI_BENT,verbose=TRUE)
modsum_MMI_BENT_base<-summary(M_MMI_BENT_base,signature=signature(object = "merModLmerTest"))
#write.csv(ranef(M_MMI_BENT_base)[2], "//Users/TScott/Google Drive/duckabush/mmi_ranef_D.csv")


M_XCMGW_base <- lmer(XCMGW~XWIDTH+XELEV+as.factor(YEAR)+SQ_MILE+STRAHLER+PURB+PWETL+PFOR+RDDENS+POPDENS+W1_HAG+W1_HNOAG+(1|HUC8)+(1|STATE),data=fdat_XCMGW,verbose=TRUE)
modsum_XCMGW_base<-summary(M_XCMGW_base,signature=signature(object = "merModLmerTest"))
#write.csv(ranef(M_XCMGW_base)[2], "//Users/TScott/Google Drive/duckabush/xcmgw_ranef_D.csv")

#make csv file that is table of fixed effects from base models
fix_base<-cbind(modsum_PTL_base$coef[c(-grep("YEAR",rownames(modsum_NTL_base$coef)),-grep("Intercept",rownames(modsum_NTL_base$coef))),],
                car::Anova(M_PTL_base)[-grep("YEAR",rownames(car::Anova(M_NTL_base))),],
                modsum_NTL_base$coef[c(-grep("YEAR",rownames(modsum_NTL_base$coef)),-grep("Intercept",rownames(modsum_NTL_base$coef))),],
                car::Anova(M_NTL_base)[-grep("YEAR",rownames(car::Anova(M_NTL_base))),],
                modsum_TURB_base$coef[c(-grep("YEAR",rownames(modsum_NTL_base$coef)),-grep("Intercept",rownames(modsum_NTL_base$coef))),],
                car::Anova(M_TURB_base)[-grep("YEAR",rownames(car::Anova(M_NTL_base))),],
                modsum_MMI_BENT_base$coef[c(-grep("YEAR",rownames(modsum_NTL_base$coef)),-grep("Intercept",rownames(modsum_NTL_base$coef))),],
                car::Anova(M_MMI_BENT_base)[-grep("YEAR",rownames(car::Anova(M_NTL_base))),],
                modsum_XCMGW_base$coef[c(-grep("YEAR",rownames(modsum_NTL_base$coef)),-grep("Intercept",rownames(modsum_NTL_base$coef))),],
                car::Anova(M_XCMGW_base)[-grep("YEAR",rownames(car::Anova(M_NTL_base))),],
                modsum_XFC_NAT_base$coef[c(-grep("YEAR",rownames(modsum_NTL_base$coef)),-grep("Intercept",rownames(modsum_NTL_base$coef))),],
                car::Anova(M_XFC_NAT_base)[-grep("YEAR",rownames(car::Anova(M_NTL_base))),])

#DIRECT MODELS
M_PTL_directD <- lmer(PTL~XWIDTH+XELEV+ACTIVED+as.factor(YEAR)+SQ_MILE+STRAHLER+PURB+PWETL+PFOR+RDDENS+POPDENS+W1_HAG+W1_HNOAG+(1|HUC8)+(ACTIVED|STATE),data=fdat,verbose=TRUE)
modsum_PTL_directD<-summary(M_PTL_directD,signature=signature(object = "merModLmerTest"))
write.csv(ranef(M_PTL_directD)[2], "//Users/TScott/Google Drive/duckabush/ptl_ranef_dir_D.csv")

M_NTL_directD <- lmer(NTL~XWIDTH+XELEV+ACTIVED+as.factor(YEAR)+SQ_MILE+STRAHLER+PURB+PWETL+PFOR+RDDENS+POPDENS+W1_HAG+W1_HNOAG+(1|HUC8)+(ACTIVED|STATE),data=fdat,verbose=TRUE)
modsum_NTL_directD<-summary(M_NTL_directD,signature=signature(object = "merModLmerTest"))
write.csv(ranef(M_NTL_directD)[2], "//Users/TScott/Google Drive/duckabush/ntl_ranef_dir_D.csv")
modsum_NTL_directD
M_TURB_directD <- lmer(TURB~XWIDTH+XELEV+ACTIVED+as.factor(YEAR)+SQ_MILE+STRAHLER+PURB+PWETL+PFOR+RDDENS+POPDENS+W1_HAG+W1_HNOAG+(1|HUC8)+(ACTIVED|STATE),data=fdat,verbose=TRUE)
modsum_TURB_directD<-summary(M_TURB_directD,signature=signature(object = "merModLmerTest"))
write.csv(ranef(M_TURB_directD)[2], "//Users/TScott/Google Drive/duckabush/turb_ranef_dir_D.csv")

M_XFC_NAT_directD <- lmer(XFC_NAT~XWIDTH+XELEV+ACTIVED+as.factor(YEAR)+SQ_MILE+STRAHLER+PURB+PWETL+PFOR+RDDENS+POPDENS+W1_HAG+W1_HNOAG+(1|HUC8)+(ACTIVED|STATE),data=fdat,verbose=TRUE)
modsum_XFC_NAT_directD<-summary(M_XFC_NAT_directD,signature=signature(object = "merModLmerTest"))
write.csv(ranef(M_XFC_NAT_directD)[2], "//Users/TScott/Google Drive/duckabush/xfc_ranef_dir_D.csv")

fdat_MMI_BENT<-subset(fdat,is.na(fdat$MMI_BENT)==FALSE)
M_MMI_BENT_directD <- lmer(MMI_BENT~XWIDTH+XELEV+ACTIVED+as.factor(YEAR)+SQ_MILE+STRAHLER+PURB+PWETL+PFOR+RDDENS+POPDENS+W1_HAG+W1_HNOAG+(1|HUC8)+(ACTIVED|STATE),data=fdat_MMI_BENT,verbose=TRUE)
modsum_MMI_BENT_directD<-summary(M_MMI_BENT_directD,signature=signature(object = "merModLmerTest"))
write.csv(ranef(M_MMI_BENT_directD)[2], "//Users/TScott/Google Drive/duckabush/mmi_ranef_dir_D.csv")

fdat_XCMGW<-subset(fdat,is.na(fdat$XCMGW_stz)==FALSE)
M_XCMGW_directD <- lmer(XCMGW~XWIDTH+XELEV+ACTIVED+as.factor(YEAR)+SQ_MILE+STRAHLER+PURB+PWETL+PFOR+RDDENS+POPDENS+W1_HAG+W1_HNOAG+(1|HUC8)+(ACTIVED|STATE),data=fdat_XCMGW,verbose=TRUE)
modsum_XCMGW_directD<-summary(M_XCMGW_directD,signature=signature(object = "merModLmerTest"))
write.csv(ranef(M_XCMGW_directD)[2], "//Users/TScott/Google Drive/duckabush/xcmgw_ranef_dir_D.csv")

#make csv file that is table of fixed effects from directD models
#make csv file that is table of fixed effects from directD models
fix_directD<-cbind(modsum_PTL_directD$coef[c(-grep("YEAR",rownames(modsum_NTL_directD$coef)),-grep("Intercept",rownames(modsum_NTL_directD$coef))),],
                   car::Anova(M_PTL_directD)[-grep("YEAR",rownames(car::Anova(M_NTL_directD))),],
                   modsum_NTL_directD$coef[c(-grep("YEAR",rownames(modsum_NTL_directD$coef)),-grep("Intercept",rownames(modsum_NTL_directD$coef))),],
                   car::Anova(M_NTL_directD)[-grep("YEAR",rownames(car::Anova(M_NTL_directD))),],
                   modsum_TURB_directD$coef[c(-grep("YEAR",rownames(modsum_NTL_directD$coef)),-grep("Intercept",rownames(modsum_NTL_directD$coef))),],
                   car::Anova(M_TURB_directD)[-grep("YEAR",rownames(car::Anova(M_NTL_directD))),],
                   modsum_MMI_BENT_directD$coef[c(-grep("YEAR",rownames(modsum_NTL_directD$coef)),-grep("Intercept",rownames(modsum_NTL_directD$coef))),],
                   car::Anova(M_MMI_BENT_directD)[-grep("YEAR",rownames(car::Anova(M_NTL_directD))),],
                   modsum_XCMGW_directD$coef[c(-grep("YEAR",rownames(modsum_NTL_directD$coef)),-grep("Intercept",rownames(modsum_NTL_directD$coef))),],
                   car::Anova(M_XCMGW_directD)[-grep("YEAR",rownames(car::Anova(M_NTL_directD))),],
                   modsum_XFC_NAT_directD$coef[c(-grep("YEAR",rownames(modsum_NTL_directD$coef)),-grep("Intercept",rownames(modsum_NTL_directD$coef))),],
                   car::Anova(M_XFC_NAT_directD)[-grep("YEAR",rownames(car::Anova(M_NTL_directD))),])

colnames(fix_directD)<-c("PTL","p","NTL",'p','TURB','p','MMI_BENT','p','XCMGW','p','XFC_NAT','p')
#write.csv(fix_directD, "//Users/TScott/Google Drive/duckabush/fixef_directD_D.csv")




stargazer(M_PTL_directD,M_NTL_directD,M_TURB_directD,M_MMI_BENT_directD,M_XCMGW_directD,M_XFC_NAT_directD)


#write.table(stargazer(M_PTL_directD,M_NTL_directD,M_TURB_directD,M_MMI_BENT_directD,M_XCMGW_directD,M_XFC_NAT_directD),file="//Users/TScott/Google Drive/duckabush/latextable_dirD")


#write.table(stargazer(M_PTL_base,M_NTL_base,M_TURB_base,M_MMI_BENT_base,M_XCMGW_base,M_XFC_NAT_base),file="//Users/TScott/Google Drive/duckabush/latextable_base")



#UN-RESTRICTED MODELS: FUNDING
M_PTL_fundD <- lmer(PTL~ ACTIVED+as.factor(YEAR)+XWIDTH+XELEV+SQ_MILE+STRAHLER+PURB+PWETL+PFOR+RDDENS+POPDENS+W1_HAG+W1_HNOAG+
                      ACTIVED:FUND_LOCAL+
                      ACTIVED:FUND_STATE+
                      ACTIVED:FUND_FED+
                      (1|HUC8)+(ACTIVED|STATE),verbose=TRUE,data=fdat)
modsum_PTL_fundD<-summary(M_PTL_fundD,signature=signature(object = "merModLmerTest"))
#write.csv(ranef(M_PTL_fundD)[2], "//Users/TScott/Google Drive/duckabush/ptl_ranef_fund_D.csv")

M_NTL_fundD <- lmer(NTL~ ACTIVED+as.factor(YEAR)+XWIDTH+XELEV+SQ_MILE+STRAHLER+PURB+PWETL+PFOR+RDDENS+POPDENS+W1_HAG+W1_HNOAG+
                      ACTIVED:FUND_LOCAL+
                      ACTIVED:FUND_STATE+
                      ACTIVED:FUND_FED+
                      (1|HUC8)+(ACTIVED|STATE),verbose=TRUE,data=fdat)
modsum_NTL_fundD<-summary(M_NTL_fundD,signature=signature(object = "merModLmerTest"))
#write.csv(ranef(M_NTL_fundD)[2], "//Users/TScott/Google Drive/duckabush/ntl_ranef_fund_D.csv")

M_TURB_fundD <- lmer(TURB~ ACTIVED+as.factor(YEAR)+XWIDTH+XELEV+SQ_MILE+STRAHLER+PURB+PWETL+PFOR+RDDENS+POPDENS+W1_HAG+W1_HNOAG+
                       ACTIVED:FUND_LOCAL+
                       ACTIVED:FUND_STATE+
                       ACTIVED:FUND_FED+
                       (1|HUC8)+(ACTIVED|STATE),verbose=TRUE,data=fdat)
modsum_TURB_fundD<-summary(M_TURB_fundD,signature=signature(object = "merModLmerTest"))
#write.csv(ranef(M_TURB_fundD)[2], "//Users/TScott/Google Drive/duckabush/turb_ranef_fund_D.csv")

M_XFC_NAT_fundD <- lmer(XFC_NAT~ ACTIVED+as.factor(YEAR)+XWIDTH+XELEV+SQ_MILE+STRAHLER+PURB+PWETL+PFOR+RDDENS+POPDENS+W1_HAG+W1_HNOAG+
                          ACTIVED:FUND_LOCAL+
                          ACTIVED:FUND_STATE+
                          ACTIVED:FUND_FED+
                          (1|HUC8)+(ACTIVED|STATE),verbose=TRUE,data=fdat)
modsum_XFC_NAT_fundD<-summary(M_XFC_NAT_fundD,signature=signature(object = "merModLmerTest"))
#write.csv(ranef(M_XFC_NAT_fundD)[2], "//Users/TScott/Google Drive/duckabush/xfc_ranef_fund_D.csv")

fdat_MMI_BENT<-subset(fdat,is.na(fdat$MMI_BENT)==FALSE)
M_MMI_BENT_fundD <- lmer(MMI_BENT~ACTIVED+as.factor(YEAR)+XWIDTH+XELEV+SQ_MILE+STRAHLER+PURB+PWETL+PFOR+RDDENS+POPDENS+W1_HAG+W1_HNOAG+
                           ACTIVED:FUND_LOCAL+
                           ACTIVED:FUND_STATE+
                           ACTIVED:FUND_FED+
                           (1|HUC8)+(ACTIVED|STATE),verbose=TRUE,data=fdat_MMI_BENT)
modsum_MMI_BENT_fundD<-summary(M_MMI_BENT_fundD,signature=signature(object = "merModLmerTest"))
#write.csv(ranef(M_MMI_BENT_fundD)[2], "//Users/TScott/Google Drive/duckabush/mmi_ranef_fund_D.csv")

fdat_XCMGW<-subset(fdat,is.na(fdat$XCMGW)==FALSE)
M_XCMGW_fundD <- lmer(XCMGW~ ACTIVED+as.factor(YEAR)+XWIDTH+XELEV+SQ_MILE+STRAHLER+PURB+PWETL+PFOR+RDDENS+POPDENS+W1_HAG+W1_HNOAG+
                        ACTIVED:FUND_LOCAL+
                        ACTIVED:FUND_STATE+
                        ACTIVED:FUND_FED+
                        (1|HUC8)+(ACTIVED|STATE),verbose=TRUE,data=fdat_XCMGW)
modsum_XCMGW_fundD<-summary(M_XCMGW_fundD,signature=signature(object = "merModLmerTest"))
#write.csv(ranef(M_XCMGW_fundD)[2], "//Users/TScott/Google Drive/duckabush/xcmgw_ranef_fund_D.csv")

#make csv file that is table of fixed effects from fundD models
fix_fundD<-cbind(modsum_PTL_fundD$coef[c(-grep("YEAR",rownames(modsum_NTL_fundD$coef)),-grep("Intercept",rownames(modsum_NTL_fundD$coef))),],
                 car::Anova(M_PTL_fundD)[-grep("YEAR",rownames(car::Anova(M_NTL_fundD))),],
                 modsum_NTL_fundD$coef[c(-grep("YEAR",rownames(modsum_NTL_fundD$coef)),-grep("Intercept",rownames(modsum_NTL_fundD$coef))),],
                 car::Anova(M_NTL_fundD)[-grep("YEAR",rownames(car::Anova(M_NTL_fundD))),],
                 modsum_TURB_fundD$coef[c(-grep("YEAR",rownames(modsum_NTL_fundD$coef)),-grep("Intercept",rownames(modsum_NTL_fundD$coef))),],
                 car::Anova(M_TURB_fundD)[-grep("YEAR",rownames(car::Anova(M_NTL_fundD))),],
                 modsum_MMI_BENT_fundD$coef[c(-grep("YEAR",rownames(modsum_NTL_fundD$coef)),-grep("Intercept",rownames(modsum_NTL_fundD$coef))),],
                 car::Anova(M_MMI_BENT_fundD)[-grep("YEAR",rownames(car::Anova(M_NTL_fundD))),],
                 modsum_XCMGW_fundD$coef[c(-grep("YEAR",rownames(modsum_NTL_fundD$coef)),-grep("Intercept",rownames(modsum_NTL_fundD$coef))),],
                 car::Anova(M_XCMGW_fundD)[-grep("YEAR",rownames(car::Anova(M_NTL_fundD))),],
                 modsum_XFC_NAT_fundD$coef[c(-grep("YEAR",rownames(modsum_NTL_fundD$coef)),-grep("Intercept",rownames(modsum_NTL_fundD$coef))),],
                 car::Anova(M_XFC_NAT_fundD)[-grep("YEAR",rownames(car::Anova(M_NTL_fundD))),])
colnames(fix_fundD)<-c("PTL","p","NTL",'p','TURB','p','MMI_BENT','p','XCMGW','p','XFC_NAT','p')
#write.csv(fix_fundD, "//Users/TScott/Google Drive/duckabush/fixef_fund_D.csv")

stargazer(M_PTL_fundD,M_NTL_fundD,M_TURB_fundD,M_MMI_BENT_fundD,M_XCMGW_fundD,M_XFC_NAT_fundD)

#write.table(stargazer(M_PTL_fundD,M_NTL_fundD,M_TURB_fundD,M_MMI_BENT_fundD,M_XCMGW_fundD,M_XFC_NAT_fundD),file="//Users/TScott/Google Drive/duckabush/latextable_fundD")


#UN-RESTRICTED MODELS: RESPONSIBILITY Duration
M_PTL_formD <- lmer(PTL~ ACTIVED+as.factor(YEAR)+XWIDTH+XELEV+SQ_MILE+STRAHLER+PURB+PWETL+PFOR+RDDENS+POPDENS+W1_HAG+W1_HNOAG+
                      ACTIVED:FACILITATE+
                      ACTIVED:GOALS+
                      ACTIVED:OBJECTIVES+
                      ACTIVED:HAS_BYLAWS+
                      (1|HUC8)+(ACTIVED|STATE),verbose=TRUE,data=fdat)
modsum_PTL_formD<-summary(M_PTL_formD,signature=signature(object = "merModLmerTest"))
#write.csv(ranef(M_PTL_formD)[2], "//Users/TScott/Google Drive/duckabush/ptl_ranef_form_D.csv")

M_NTL_formD <- lmer(NTL~ ACTIVED+as.factor(YEAR)+XWIDTH+XELEV+SQ_MILE+STRAHLER+PURB+PWETL+PFOR+RDDENS+POPDENS+W1_HAG+W1_HNOAG+
                      ACTIVED:FACILITATE+
                      ACTIVED:GOALS+
                      ACTIVED:OBJECTIVES+
                      ACTIVED:HAS_BYLAWS+
                      (1|HUC8)+(ACTIVED|STATE),verbose=TRUE,data=fdat)
modsum_NTL_formD<-summary(M_NTL_formD,signature=signature(object = "merModLmerTest"))
#write.csv(ranef(M_NTL_formD)[2], "//Users/TScott/Google Drive/duckabush/ntl_ranef_form_D.csv")

M_TURB_formD <- lmer(TURB~ ACTIVED+as.factor(YEAR)+XWIDTH+XELEV+SQ_MILE+STRAHLER+PURB+PWETL+PFOR+RDDENS+POPDENS+W1_HAG+W1_HNOAG+
                       ACTIVED:FACILITATE+
                       ACTIVED:GOALS+
                       ACTIVED:OBJECTIVES+
                       ACTIVED:HAS_BYLAWS+
                       (1|HUC8)+(ACTIVED|STATE),verbose=TRUE,data=fdat)
modsum_TURB_formD<-summary(M_TURB_formD,signature=signature(object = "merModLmerTest"))
#write.csv(ranef(M_TURB_formD)[2], "//Users/TScott/Google Drive/duckabush/turb_ranef_form_D.csv")

M_XFC_NAT_formD <- lmer(XFC_NAT~ ACTIVED+as.factor(YEAR)+XWIDTH+XELEV+SQ_MILE+STRAHLER+PURB+PWETL+PFOR+RDDENS+POPDENS+W1_HAG+W1_HNOAG+
                          ACTIVED:FACILITATE+
                          ACTIVED:GOALS+
                          ACTIVED:OBJECTIVES+
                          ACTIVED:HAS_BYLAWS+
                          (1|HUC8)+(ACTIVED|STATE),verbose=TRUE,data=fdat)
modsum_XFC_NAT_formD<-summary(M_XFC_NAT_formD,signature=signature(object = "merModLmerTest"))
#write.csv(ranef(M_XFC_NAT_formD)[2], "//Users/TScott/Google Drive/duckabush/xfc_ranef_form_D.csv")

fdat_MMI_BENT<-subset(fdat,is.na(fdat$MMI_BENT)==FALSE)
M_MMI_BENT_formD <- lmer(MMI_BENT~ACTIVED+as.factor(YEAR)+XWIDTH+XELEV+SQ_MILE+STRAHLER+PURB+PWETL+PFOR+RDDENS+POPDENS+W1_HAG+W1_HNOAG+
                           ACTIVED:FACILITATE+
                           ACTIVED:GOALS+
                           ACTIVED:OBJECTIVES+
                           ACTIVED:HAS_BYLAWS+
                           (1|HUC8)+(ACTIVED|STATE),verbose=TRUE,data=fdat_MMI_BENT)
modsum_MMI_BENT_formD<-summary(M_MMI_BENT_formD,signature=signature(object = "merModLmerTest"))
#write.csv(ranef(M_MMI_BENT_formD)[2], "//Users/TScott/Google Drive/duckabush/mmi_ranef_form_D.csv")

fdat_XCMGW<-subset(fdat,is.na(fdat$XCMGW)==FALSE)
M_XCMGW_formD <- lmer(XCMGW~ ACTIVED+as.factor(YEAR)+XWIDTH+XELEV+SQ_MILE+STRAHLER+PURB+PWETL+PFOR+RDDENS+POPDENS+W1_HAG+W1_HNOAG+
                        ACTIVED:FACILITATE+
                        ACTIVED:GOALS+
                        ACTIVED:OBJECTIVES+
                        ACTIVED:HAS_BYLAWS+
                        (1|HUC8)+(ACTIVED|STATE),verbose=TRUE,data=fdat_XCMGW)
modsum_XCMGW_formD<-summary(M_XCMGW_formD,signature=signature(object = "merModLmerTest"))
#write.csv(ranef(M_XCMGW_formD)[2], "//Users/TScott/Google Drive/duckabush/xcmgw_ranef_form_D.csv")

#make csv file that is table of fixed effects from formD models
fix_formD<-cbind(modsum_PTL_formD$coef[c(-grep("YEAR",rownames(modsum_NTL_formD$coef)),-grep("Intercept",rownames(modsum_NTL_formD$coef))),],
                 car::Anova(M_PTL_formD)[-grep("YEAR",rownames(car::Anova(M_NTL_formD))),],
                 modsum_NTL_formD$coef[c(-grep("YEAR",rownames(modsum_NTL_formD$coef)),-grep("Intercept",rownames(modsum_NTL_formD$coef))),],
                 car::Anova(M_NTL_formD)[-grep("YEAR",rownames(car::Anova(M_NTL_formD))),],
                 modsum_TURB_formD$coef[c(-grep("YEAR",rownames(modsum_NTL_formD$coef)),-grep("Intercept",rownames(modsum_NTL_formD$coef))),],
                 car::Anova(M_TURB_formD)[-grep("YEAR",rownames(car::Anova(M_NTL_formD))),],
                 modsum_MMI_BENT_formD$coef[c(-grep("YEAR",rownames(modsum_NTL_formD$coef)),-grep("Intercept",rownames(modsum_NTL_formD$coef))),],
                 car::Anova(M_MMI_BENT_formD)[-grep("YEAR",rownames(car::Anova(M_NTL_formD))),],
                 modsum_XCMGW_formD$coef[c(-grep("YEAR",rownames(modsum_NTL_formD$coef)),-grep("Intercept",rownames(modsum_NTL_formD$coef))),],
                 car::Anova(M_XCMGW_formD)[-grep("YEAR",rownames(car::Anova(M_NTL_formD))),],
                 modsum_XFC_NAT_formD$coef[c(-grep("YEAR",rownames(modsum_NTL_formD$coef)),-grep("Intercept",rownames(modsum_NTL_formD$coef))),],
                 car::Anova(M_XFC_NAT_formD)[-grep("YEAR",rownames(car::Anova(M_NTL_formD))),])
colnames(fix_formD)<-c("PTL","p","NTL",'p','TURB','p','MMI_BENT','p','XCMGW','p','XFC_NAT','p')
#write.csv(fix_formD, "//Users/TScott/Google Drive/duckabush/fixef_form_D.csv")

stargazer(M_PTL_formD,M_NTL_formD,M_TURB_formD,M_MMI_BENT_formD,M_XCMGW_formD,M_XFC_NAT_formD)

#write.table(stargazer(M_PTL_formD,M_NTL_formD,M_TURB_formD,M_MMI_BENT_formD,M_XCMGW_formD,M_XFC_NAT_formD),file="//Users/TScott/Google Drive/duckabush/latextable_formD")


#UN-RESTRICTED MODELS: REPRESENTATION Duration
M_PTL_repD <- lmer(PTL~XWIDTH+XELEV+ACTIVED+as.factor(YEAR)+SQ_MILE+STRAHLER+PURB+PWETL+PFOR+RDDENS++POPDENS+W1_HAG+W1_HNOAG+ACTIVED:CROSS_BOUND:TRANS_BOUND+ACTIVED:MEM_TOTAL+
                     TECH_BODY:ACTIVED+(1|HUC8)+(ACTIVED|STATE),data=fdat,verbose=TRUE)
modsum_PTL_repD<-summary(M_PTL_repD,signature=signature(object = "merModLmerTest"))
#write.csv(ranef(M_PTL_repD)[2], "//Users/TScott/Google Drive/duckabush/ptl_ranef_rep_D.csv")

M_NTL_repD <- lmer(NTL~XWIDTH+XELEV+ACTIVED+as.factor(YEAR)+SQ_MILE+STRAHLER+PURB+PWETL+PFOR+RDDENS++POPDENS+W1_HAG+W1_HNOAG+ACTIVED:CROSS_BOUND:TRANS_BOUND+ACTIVED:MEM_TOTAL+
                     TECH_BODY:ACTIVED+(1|HUC8)+(ACTIVED|STATE),data=fdat,verbose=TRUE)
modsum_NTL_repD<-summary(M_NTL_repD,signature=signature(object = "merModLmerTest"))
#write.csv(ranef(M_NTL_repD)[2], "//Users/TScott/Google Drive/duckabush/ntl_ranef_rep_D.csv")

M_TURB_repD <- lmer(TURB~XWIDTH+XELEV+ACTIVED+as.factor(YEAR)+SQ_MILE+STRAHLER+PURB+PWETL+PFOR+RDDENS++POPDENS+W1_HAG+W1_HNOAG+ACTIVED:CROSS_BOUND:TRANS_BOUND+ACTIVED:MEM_TOTAL+
                      TECH_BODY:ACTIVED+(1|HUC8)+(ACTIVED|STATE),data=fdat,verbose=TRUE)
modsum_TURB_repD<-summary(M_TURB_repD,signature=signature(object = "merModLmerTest"))
#write.csv(ranef(M_TURB_repD)[2], "//Users/TScott/Google Drive/duckabush/turb_ranef_rep_D.csv")

M_XFC_NAT_repD <- lmer(XFC_NAT~XWIDTH+XELEV+ACTIVED+as.factor(YEAR)+SQ_MILE+STRAHLER+PURB+PWETL+PFOR+RDDENS++POPDENS+W1_HAG+W1_HNOAG+ACTIVED:CROSS_BOUND:TRANS_BOUND+ACTIVED:MEM_TOTAL+
                         TECH_BODY:ACTIVED+(1|HUC8)+(ACTIVED|STATE),data=fdat,verbose=TRUE)
modsum_XFC_NAT_repD<-summary(M_XFC_NAT_repD,signature=signature(object = "merModLmerTest"))
#write.csv(ranef(M_XFC_NAT_repD)[2], "//Users/TScott/Google Drive/duckabush/xfc_ranef_rep_D.csv")

fdat_MMI_BENT<-subset(fdat,is.na(fdat$MMI_BENT)==FALSE)
M_MMI_BENT_repD <- lmer(MMI_BENT~XWIDTH+XELEV+ACTIVED+as.factor(YEAR)+SQ_MILE+STRAHLER+PURB+PWETL+PFOR+RDDENS++POPDENS+W1_HAG+W1_HNOAG+ACTIVED:CROSS_BOUND:TRANS_BOUND+ACTIVED:MEM_TOTAL+
                          TECH_BODY:ACTIVED+(1|HUC8)+(ACTIVED|STATE),data=fdat_MMI_BENT,verbose=TRUE)
modsum_MMI_BENT_repD<-summary(M_MMI_BENT_repD,signature=signature(object = "merModLmerTest"))
#write.csv(ranef(M_MMI_BENT_repD)[2], "//Users/TScott/Google Drive/duckabush/mmi_ranef_rep_D.csv")

fdat_XCMGW<-subset(fdat,is.na(fdat$XCMGW)==FALSE)
M_XCMGW_repD <- lmer(XCMGW~XWIDTH+XELEV+ACTIVED+as.factor(YEAR)+SQ_MILE+STRAHLER+PURB+PWETL+PFOR+RDDENS++POPDENS+W1_HAG+W1_HNOAG+ACTIVED:CROSS_BOUND:TRANS_BOUND+ACTIVED:MEM_TOTAL+
                       TECH_BODY:ACTIVED+(1|HUC8)+(ACTIVED|STATE),data=fdat_XCMGW,verbose=TRUE)
modsum_XCMGW_repD<-summary(M_XCMGW_repD,signature=signature(object = "merModLmerTest"))
#write.csv(ranef(M_XCMGW_repD)[2], "//Users/TScott/Google Drive/duckabush/xcmgw_ranef_rep_D.csv")

#make csv file that is table of fixed effects from repD models
#make csv file that is table of fixed effects from repD models
fix_repD<-cbind(modsum_PTL_repD$coef[c(-grep("YEAR",rownames(modsum_NTL_repD$coef)),-grep("Intercept",rownames(modsum_NTL_repD$coef))),],
                car::Anova(M_PTL_repD)[-grep("YEAR",rownames(car::Anova(M_NTL_repD))),],
                modsum_NTL_repD$coef[c(-grep("YEAR",rownames(modsum_NTL_repD$coef)),-grep("Intercept",rownames(modsum_NTL_repD$coef))),],
                car::Anova(M_NTL_repD)[-grep("YEAR",rownames(car::Anova(M_NTL_repD))),],
                modsum_TURB_repD$coef[c(-grep("YEAR",rownames(modsum_NTL_repD$coef)),-grep("Intercept",rownames(modsum_NTL_repD$coef))),],
                car::Anova(M_TURB_repD)[-grep("YEAR",rownames(car::Anova(M_NTL_repD))),],
                modsum_MMI_BENT_repD$coef[c(-grep("YEAR",rownames(modsum_NTL_repD$coef)),-grep("Intercept",rownames(modsum_NTL_repD$coef))),],
                car::Anova(M_MMI_BENT_repD)[-grep("YEAR",rownames(car::Anova(M_NTL_repD))),],
                modsum_XCMGW_repD$coef[c(-grep("YEAR",rownames(modsum_NTL_repD$coef)),-grep("Intercept",rownames(modsum_NTL_repD$coef))),],
                car::Anova(M_XCMGW_repD)[-grep("YEAR",rownames(car::Anova(M_NTL_repD))),],
                modsum_XFC_NAT_repD$coef[c(-grep("YEAR",rownames(modsum_NTL_repD$coef)),-grep("Intercept",rownames(modsum_NTL_repD$coef))),],
                car::Anova(M_XFC_NAT_repD)[-grep("YEAR",rownames(car::Anova(M_NTL_repD))),])

colnames(fix_repD)<-c("PTL","p","NTL",'p','TURB','p','MMI_BENT','p','XCMGW','p','XFC_NAT','p')
#write.csv(fix_repD, "//Users/TScott/Google Drive/duckabush/fixef_rep_D.csv")

stargazer(M_PTL_repD,M_NTL_repD,M_TURB_repD,M_MMI_BENT_repD,M_XCMGW_repD,M_XFC_NAT_repD)

#write.table(stargazer(M_PTL_repD,M_NTL_repD,M_TURB_repD,M_MMI_BENT_repD,M_XCMGW_repD,M_XFC_NAT_repD),file="//Users/TScott/Google Drive/duckabush/latextable_repD")


M_PTL_resD <- lmer(PTL~ ACTIVED+as.factor(YEAR)+XWIDTH+XELEV+SQ_MILE+STRAHLER+PURB+PWETL+PFOR+RDDENS+POPDENS+W1_HAG+W1_HNOAG+
                     ACTIVED:PLAN+
                     ACTIVED:MANAGE+
                     (1|HUC8)+(ACTIVED|STATE),verbose=TRUE,data=fdat)
modsum_PTL_resD<-summary(M_PTL_resD,signature=signature(object = "merModLmerTest"))
#write.csv(ranef(M_PTL_resD)[2], "//Users/TScott/Google Drive/duckabush/ptl_ranef_resD_D.csv")

M_NTL_resD <- lmer(NTL~ ACTIVED+as.factor(YEAR)+XWIDTH+XELEV+SQ_MILE+STRAHLER+PURB+PWETL+PFOR+RDDENS+POPDENS+W1_HAG+W1_HNOAG+
                     ACTIVED:PLAN+
                     ACTIVED:MANAGE+
                     (1|HUC8)+(ACTIVED|STATE),verbose=TRUE,data=fdat)
modsum_NTL_resD<-summary(M_NTL_resD,signature=signature(object = "merModLmerTest"))
#write.csv(ranef(M_NTL_resD)[2], "//Users/TScott/Google Drive/duckabush/ntl_ranef_resD_D.csv")

M_TURB_resD <- lmer(TURB~ ACTIVED+as.factor(YEAR)+XWIDTH+XELEV+SQ_MILE+STRAHLER+PURB+PWETL+PFOR+RDDENS+POPDENS+W1_HAG+W1_HNOAG+
                      ACTIVED:PLAN+
                      ACTIVED:MANAGE+
                      (1|HUC8)+(ACTIVED|STATE),verbose=TRUE,data=fdat)
modsum_TURB_resD<-summary(M_TURB_resD,signature=signature(object = "merModLmerTest"))
#write.csv(ranef(M_TURB_resD)[2], "//Users/TScott/Google Drive/duckabush/turb_ranef_resD_D.csv")

M_XFC_NAT_resD <- lmer(XFC_NAT~ ACTIVED+as.factor(YEAR)+XWIDTH+XELEV+SQ_MILE+STRAHLER+PURB+PWETL+PFOR+RDDENS+POPDENS+W1_HAG+W1_HNOAG+
                         ACTIVED:PLAN+
                         ACTIVED:MANAGE+
                         (1|HUC8)+(ACTIVED|STATE),verbose=TRUE,data=fdat)
modsum_XFC_NAT_resD<-summary(M_XFC_NAT_resD,signature=signature(object = "merModLmerTest"))
#write.csv(ranef(M_XFC_NAT_resD)[2], "//Users/TScott/Google Drive/duckabush/xfc_ranef_resD_D.csv")

fdat_MMI_BENT<-subset(fdat,is.na(fdat$MMI_BENT)==FALSE)
M_MMI_BENT_resD <- lmer(MMI_BENT~ACTIVED+as.factor(YEAR)+XWIDTH+XELEV+SQ_MILE+STRAHLER+PURB+PWETL+PFOR+RDDENS+POPDENS+W1_HAG+W1_HNOAG+
                          ACTIVED:PLAN+
                          ACTIVED:MANAGE+
                          (1|HUC8)+(ACTIVED|STATE),verbose=TRUE,data=fdat_MMI_BENT)
modsum_MMI_BENT_resD<-summary(M_MMI_BENT_resD,signature=signature(object = "merModLmerTest"))
#write.csv(ranef(M_MMI_BENT_resD)[2], "//Users/TScott/Google Drive/duckabush/mmi_ranef_resD_D.csv")

fdat_XCMGW<-subset(fdat,is.na(fdat$XCMGW)==FALSE)
M_XCMGW_resD <- lmer(XCMGW~ ACTIVED+as.factor(YEAR)+XWIDTH+XELEV+SQ_MILE+STRAHLER+PURB+PWETL+PFOR+RDDENS+POPDENS+W1_HAG+W1_HNOAG+
                       ACTIVED:PLAN+
                       ACTIVED:MANAGE+
                       (1|HUC8)+(ACTIVED|STATE),verbose=TRUE,data=fdat_XCMGW)
modsum_XCMGW_resD<-summary(M_XCMGW_resD,signature=signature(object = "merModLmerTest"))
#write.csv(ranef(M_XCMGW_resD)[2], "//Users/TScott/Google Drive/duckabush/xcmgw_ranef_resD_D.csv")

#make csv file that is table of fixed effects from resD models
fix_resD<-cbind(modsum_PTL_resD$coef[c(-grep("YEAR",rownames(modsum_NTL_resD$coef)),-grep("Intercept",rownames(modsum_NTL_resD$coef))),],
                car::Anova(M_PTL_resD)[-grep("YEAR",rownames(car::Anova(M_NTL_resD))),],
                modsum_NTL_resD$coef[c(-grep("YEAR",rownames(modsum_NTL_resD$coef)),-grep("Intercept",rownames(modsum_NTL_resD$coef))),],
                car::Anova(M_NTL_resD)[-grep("YEAR",rownames(car::Anova(M_NTL_resD))),],
                modsum_TURB_resD$coef[c(-grep("YEAR",rownames(modsum_NTL_resD$coef)),-grep("Intercept",rownames(modsum_NTL_resD$coef))),],
                car::Anova(M_TURB_resD)[-grep("YEAR",rownames(car::Anova(M_NTL_resD))),],
                modsum_MMI_BENT_resD$coef[c(-grep("YEAR",rownames(modsum_NTL_resD$coef)),-grep("Intercept",rownames(modsum_NTL_resD$coef))),],
                car::Anova(M_MMI_BENT_resD)[-grep("YEAR",rownames(car::Anova(M_NTL_resD))),],
                modsum_XCMGW_resD$coef[c(-grep("YEAR",rownames(modsum_NTL_resD$coef)),-grep("Intercept",rownames(modsum_NTL_resD$coef))),],
                car::Anova(M_XCMGW_resD)[-grep("YEAR",rownames(car::Anova(M_NTL_resD))),],
                modsum_XFC_NAT_resD$coef[c(-grep("YEAR",rownames(modsum_NTL_resD$coef)),-grep("Intercept",rownames(modsum_NTL_resD$coef))),],
                car::Anova(M_XFC_NAT_resD)[-grep("YEAR",rownames(car::Anova(M_NTL_resD))),])
colnames(fix_resD)<-c("PTL","p","NTL",'p','TURB','p','MMI_BENT','p','XCMGW','p','XFC_NAT','p')
#write.csv(fix_resD, "//Users/TScott/Google Drive/duckabush/fixef_resD_D.csv")


stargazer(M_PTL_resD,M_NTL_resD,M_TURB_resD,M_MMI_BENT_resD,M_XCMGW_resD,M_XFC_NAT_resD)

#write.table(stargazer(M_PTL_resD,M_NTL_resD,M_TURB_resD,M_MMI_BENT_resD,M_XCMGW_resD,M_XFC_NAT_resD),file="//Users/TScott/Google Drive/duckabush/latextable_resD")




