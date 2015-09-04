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
