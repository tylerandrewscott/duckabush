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


