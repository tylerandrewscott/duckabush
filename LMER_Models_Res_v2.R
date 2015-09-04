rm(list=ls())
setwd('//Users/TScott/Google Drive/duckabush')
source('model_prep.R')
require(lme4)
require(pbkrtest)
require(LMERConvenienceFunctions)
#require(MCMCpack)
#require(arm)
#require(blme)
require(stargazer)


M_PTL_resD <- lmer(PTL~ ACTIVED+as.factor(YEAR)+XWIDTH+XELEV+SQ_MILE+STRAHLER+PURB+PWETL+PFOR+RDDENS+POPDENS+W1_HAG+W1_HNOAG+
ACTIVED:PLAN+
ACTIVED:MANAGE+
(1|HUC4)+(ACTIVED|STATE),verbose=TRUE,data=fdat)
modsum_PTL_resD<-summary(M_PTL_resD,signature=signature(object = "merModLmerTest"))
#write.csv(ranef(M_PTL_resD)[2], "//Users/TScott/Google Drive/duckabush/ptl_ranef_resD_D.csv")

M_NTL_resD <- lmer(NTL~ ACTIVED+as.factor(YEAR)+XWIDTH+XELEV+SQ_MILE+STRAHLER+PURB+PWETL+PFOR+RDDENS+POPDENS+W1_HAG+W1_HNOAG+
ACTIVED:PLAN+
ACTIVED:MANAGE+
(1|HUC4)+(ACTIVED|STATE),verbose=TRUE,data=fdat)
modsum_NTL_resD<-summary(M_NTL_resD,signature=signature(object = "merModLmerTest"))
#write.csv(ranef(M_NTL_resD)[2], "//Users/TScott/Google Drive/duckabush/ntl_ranef_resD_D.csv")

M_TURB_resD <- lmer(TURB~ ACTIVED+as.factor(YEAR)+XWIDTH+XELEV+SQ_MILE+STRAHLER+PURB+PWETL+PFOR+RDDENS+POPDENS+W1_HAG+W1_HNOAG+
ACTIVED:PLAN+
ACTIVED:MANAGE+
(1|HUC4)+(ACTIVED|STATE),verbose=TRUE,data=fdat)
modsum_TURB_resD<-summary(M_TURB_resD,signature=signature(object = "merModLmerTest"))
#write.csv(ranef(M_TURB_resD)[2], "//Users/TScott/Google Drive/duckabush/turb_ranef_resD_D.csv")

M_XFC_NAT_resD <- lmer(XFC_NAT~ ACTIVED+as.factor(YEAR)+XWIDTH+XELEV+SQ_MILE+STRAHLER+PURB+PWETL+PFOR+RDDENS+POPDENS+W1_HAG+W1_HNOAG+
ACTIVED:PLAN+
ACTIVED:MANAGE+
(1|HUC4)+(ACTIVED|STATE),verbose=TRUE,data=fdat)
modsum_XFC_NAT_resD<-summary(M_XFC_NAT_resD,signature=signature(object = "merModLmerTest"))
#write.csv(ranef(M_XFC_NAT_resD)[2], "//Users/TScott/Google Drive/duckabush/xfc_ranef_resD_D.csv")

fdat_MMI_BENT<-subset(fdat,is.na(fdat$MMI_BENT)==FALSE)
M_MMI_BENT_resD <- lmer(MMI_BENT~ACTIVED+as.factor(YEAR)+XWIDTH+XELEV+SQ_MILE+STRAHLER+PURB+PWETL+PFOR+RDDENS+POPDENS+W1_HAG+W1_HNOAG+
ACTIVED:PLAN+
ACTIVED:MANAGE+
(1|HUC4)+(ACTIVED|STATE),verbose=TRUE,data=fdat_MMI_BENT)
modsum_MMI_BENT_resD<-summary(M_MMI_BENT_resD,signature=signature(object = "merModLmerTest"))
#write.csv(ranef(M_MMI_BENT_resD)[2], "//Users/TScott/Google Drive/duckabush/mmi_ranef_resD_D.csv")

fdat_XCMGW<-subset(fdat,is.na(fdat$XCMGW)==FALSE)
M_XCMGW_resD <- lmer(XCMGW~ ACTIVED+as.factor(YEAR)+XWIDTH+XELEV+SQ_MILE+STRAHLER+PURB+PWETL+PFOR+RDDENS+POPDENS+W1_HAG+W1_HNOAG+
ACTIVED:PLAN+
ACTIVED:MANAGE+
(1|HUC4)+(ACTIVED|STATE),verbose=TRUE,data=fdat_XCMGW)
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


