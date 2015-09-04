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

#UN-RESTRICTED MODELS: REPRESENTATION Duration
M_PTL_repD <- lmer(PTL~XWIDTH+XELEV+ACTIVED+as.factor(YEAR)+SQ_MILE+STRAHLER+PURB+PWETL+PFOR+RDDENS++POPDENS+W1_HAG+W1_HNOAG+ACTIVED:CROSS_BOUND:TRANS_BOUND+ACTIVED:MEM_TOTAL+
TECH_BODY:ACTIVED+(1|HUC4)+(ACTIVED|STATE),data=fdat,verbose=TRUE)
modsum_PTL_repD<-summary(M_PTL_repD,signature=signature(object = "merModLmerTest"))
#write.csv(ranef(M_PTL_repD)[2], "//Users/TScott/Google Drive/duckabush/ptl_ranef_rep_D.csv")

M_NTL_repD <- lmer(NTL~XWIDTH+XELEV+ACTIVED+as.factor(YEAR)+SQ_MILE+STRAHLER+PURB+PWETL+PFOR+RDDENS++POPDENS+W1_HAG+W1_HNOAG+ACTIVED:CROSS_BOUND:TRANS_BOUND+ACTIVED:MEM_TOTAL+
TECH_BODY:ACTIVED+(1|HUC4)+(ACTIVED|STATE),data=fdat,verbose=TRUE)
modsum_NTL_repD<-summary(M_NTL_repD,signature=signature(object = "merModLmerTest"))
#write.csv(ranef(M_NTL_repD)[2], "//Users/TScott/Google Drive/duckabush/ntl_ranef_rep_D.csv")

M_TURB_repD <- lmer(TURB~XWIDTH+XELEV+ACTIVED+as.factor(YEAR)+SQ_MILE+STRAHLER+PURB+PWETL+PFOR+RDDENS++POPDENS+W1_HAG+W1_HNOAG+ACTIVED:CROSS_BOUND:TRANS_BOUND+ACTIVED:MEM_TOTAL+
TECH_BODY:ACTIVED+(1|HUC4)+(ACTIVED|STATE),data=fdat,verbose=TRUE)
modsum_TURB_repD<-summary(M_TURB_repD,signature=signature(object = "merModLmerTest"))
#write.csv(ranef(M_TURB_repD)[2], "//Users/TScott/Google Drive/duckabush/turb_ranef_rep_D.csv")

M_XFC_NAT_repD <- lmer(XFC_NAT~XWIDTH+XELEV+ACTIVED+as.factor(YEAR)+SQ_MILE+STRAHLER+PURB+PWETL+PFOR+RDDENS++POPDENS+W1_HAG+W1_HNOAG+ACTIVED:CROSS_BOUND:TRANS_BOUND+ACTIVED:MEM_TOTAL+
TECH_BODY:ACTIVED+(1|HUC4)+(ACTIVED|STATE),data=fdat,verbose=TRUE)
modsum_XFC_NAT_repD<-summary(M_XFC_NAT_repD,signature=signature(object = "merModLmerTest"))
#write.csv(ranef(M_XFC_NAT_repD)[2], "//Users/TScott/Google Drive/duckabush/xfc_ranef_rep_D.csv")

fdat_MMI_BENT<-subset(fdat,is.na(fdat$MMI_BENT)==FALSE)
M_MMI_BENT_repD <- lmer(MMI_BENT~XWIDTH+XELEV+ACTIVED+as.factor(YEAR)+SQ_MILE+STRAHLER+PURB+PWETL+PFOR+RDDENS++POPDENS+W1_HAG+W1_HNOAG+ACTIVED:CROSS_BOUND:TRANS_BOUND+ACTIVED:MEM_TOTAL+
TECH_BODY:ACTIVED+(1|HUC4)+(ACTIVED|STATE),data=fdat_MMI_BENT,verbose=TRUE)
modsum_MMI_BENT_repD<-summary(M_MMI_BENT_repD,signature=signature(object = "merModLmerTest"))
#write.csv(ranef(M_MMI_BENT_repD)[2], "//Users/TScott/Google Drive/duckabush/mmi_ranef_rep_D.csv")

fdat_XCMGW<-subset(fdat,is.na(fdat$XCMGW)==FALSE)
M_XCMGW_repD <- lmer(XCMGW~XWIDTH+XELEV+ACTIVED+as.factor(YEAR)+SQ_MILE+STRAHLER+PURB+PWETL+PFOR+RDDENS++POPDENS+W1_HAG+W1_HNOAG+ACTIVED:CROSS_BOUND:TRANS_BOUND+ACTIVED:MEM_TOTAL+
TECH_BODY:ACTIVED+(1|HUC4)+(ACTIVED|STATE),data=fdat_XCMGW,verbose=TRUE)
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


