
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


#DIRECT MODELS
M_PTL_directD <- lmer(PTL~XWIDTH+XELEV+ACTIVED+as.factor(YEAR)+SQ_MILE+STRAHLER+PURB+PWETL+PFOR+RDDENS+POPDENS+W1_HAG+W1_HNOAG+(1|HUC4)+(ACTIVED|STATE),data=fdat,verbose=TRUE)
modsum_PTL_directD<-summary(M_PTL_directD,signature=signature(object = "merModLmerTest"))
write.csv(ranef(M_PTL_directD)[2], "//Users/TScott/Google Drive/duckabush/ptl_ranef_dir_D.csv")

M_NTL_directD <- lmer(NTL~XWIDTH+XELEV+ACTIVED+as.factor(YEAR)+SQ_MILE+STRAHLER+PURB+PWETL+PFOR+RDDENS+POPDENS+W1_HAG+W1_HNOAG+(1|HUC4)+(ACTIVED|STATE),data=fdat,verbose=TRUE)
modsum_NTL_directD<-summary(M_NTL_directD,signature=signature(object = "merModLmerTest"))
write.csv(ranef(M_NTL_directD)[2], "//Users/TScott/Google Drive/duckabush/ntl_ranef_dir_D.csv")
modsum_NTL_directD
M_TURB_directD <- lmer(TURB~XWIDTH+XELEV+ACTIVED+as.factor(YEAR)+SQ_MILE+STRAHLER+PURB+PWETL+PFOR+RDDENS+POPDENS+W1_HAG+W1_HNOAG+(1|HUC4)+(ACTIVED|STATE),data=fdat,verbose=TRUE)
modsum_TURB_directD<-summary(M_TURB_directD,signature=signature(object = "merModLmerTest"))
write.csv(ranef(M_TURB_directD)[2], "//Users/TScott/Google Drive/duckabush/turb_ranef_dir_D.csv")

M_XFC_NAT_directD <- lmer(XFC_NAT~XWIDTH+XELEV+ACTIVED+as.factor(YEAR)+SQ_MILE+STRAHLER+PURB+PWETL+PFOR+RDDENS+POPDENS+W1_HAG+W1_HNOAG+(1|HUC4)+(ACTIVED|STATE),data=fdat,verbose=TRUE)
modsum_XFC_NAT_directD<-summary(M_XFC_NAT_directD,signature=signature(object = "merModLmerTest"))
write.csv(ranef(M_XFC_NAT_directD)[2], "//Users/TScott/Google Drive/duckabush/xfc_ranef_dir_D.csv")

fdat_MMI_BENT<-subset(fdat,is.na(fdat$MMI_BENT)==FALSE)
M_MMI_BENT_directD <- lmer(MMI_BENT~XWIDTH+XELEV+ACTIVED+as.factor(YEAR)+SQ_MILE+STRAHLER+PURB+PWETL+PFOR+RDDENS+POPDENS+W1_HAG+W1_HNOAG+(1|HUC4)+(ACTIVED|STATE),data=fdat_MMI_BENT,verbose=TRUE)
modsum_MMI_BENT_directD<-summary(M_MMI_BENT_directD,signature=signature(object = "merModLmerTest"))
write.csv(ranef(M_MMI_BENT_directD)[2], "//Users/TScott/Google Drive/duckabush/mmi_ranef_dir_D.csv")

fdat_XCMGW<-subset(fdat,is.na(fdat$XCMGW_stz)==FALSE)
M_XCMGW_directD <- lmer(XCMGW~XWIDTH+XELEV+ACTIVED+as.factor(YEAR)+SQ_MILE+STRAHLER+PURB+PWETL+PFOR+RDDENS+POPDENS+W1_HAG+W1_HNOAG+(1|HUC4)+(ACTIVED|STATE),data=fdat_XCMGW,verbose=TRUE)
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

