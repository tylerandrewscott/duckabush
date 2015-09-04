rm(list=ls())

mdat<-data.frame(read.csv("//Users/TScott/Google Drive/duckabush/management_data_v2.csv",header=TRUE))


for (i in 1:ncol(mdat))
{
	mdat[,i]<-tolower(mdat[,i])
}

edat<-(read.csv("//Users/TScott/Google Drive/duckabush/ready_to_model_dat.csv",header=TRUE))
for (i in 1:ncol(edat))
{
	edat[,i]<-tolower(edat[,i])
}

t1<-(mdat[mdat$SURVEY=='nrsa',])
t2<-(mdat[mdat$SURVEY=='wsa',])
t1<-t1[order(t1$CON_ID),]
t2<-t2[order(t2$CON_ID),]
t1[,1:47]<-t2[,1:47]

ttdat<-rbind(t1,t2)
ttdat$GROUP<-as.numeric(ttdat$GROUP)
#temp=merge(edat,mdat,by=c("CON_ID","SURVEY","HUC8","FED_OWN","UrbanCat","FW_ECO3","YEAR","FW_ECO9","COUNTY","VISIT_NO",
#"STATE","MISS_SUB","ECOREPORT","ECO3","STRATUM","MAJ_BAS_NM","STRAHLER","EPAREG","ID","VISIT","FSEASTWEST",
#"LOC_NAME","WSAREA","XLAT_DD","XLON_DD"),all.x=TRUE)
temp=merge(edat,ttdat,by=c("CON_ID","SURVEY","YEAR","FW_ECO3","FW_ECO9",'STATE','EPAREG','COUNTY','STRAHLER','ECO3','HUC8','UrbanCat','WSAREA','MAJ_BAS_NM'),all.x=TRUE)
fdat = temp

#library(MCMCpack)
library(lme4)
#library(arm)
#library(blme)
library(pbkrtest)
library(LMERConvenienceFunctions)


fdat$FED_OWN<-relevel(as.factor(fdat$FED_OWN.y),ref="non federal")
fdat$SURVEY<-relevel(factor(fdat$SURVEY),ref="wsa")
fdat$YEAR<-as.numeric(fdat$YEAR)
fdat$GROUP_FORM<-as.numeric(fdat$GROUP_FORM)
fdat$GROUP_FORM<-ifelse(is.na(fdat$GROUP_FORM)==TRUE,0,fdat$GROUP_FORM)

#IS GROUP ACTIVE0 AT TIME OF SAMPLING?
#"ACTIVE0_GROUP" IF GROUP PRESENT IN YEAR OF SAMPLING
fdat$GROUP_FORM_lag0<-fdat$GROUP_FORM+0
fdat$ACTIVE0_GROUP<-ifelse(fdat$YEAR>=(fdat$GROUP_FORM_lag0)&fdat$GROUP_FORM_lag0!=0,1,0)
fdat$ACTIVE0<-fdat$ACTIVE0_GROUP

#IS GROUP ACTIVE5 AT TIME OF SAMPLING?
#"ACTIVE5_GROUP" IF GROUP PRESENT IN YEAR OF SAMPLING
fdat$GROUP_FORM_lag5<-fdat$GROUP_FORM+5
fdat$ACTIVE5_GROUP<-ifelse(fdat$YEAR>=(fdat$GROUP_FORM_lag5)&fdat$GROUP_FORM_lag5!=5,1,0)
fdat$ACTIVE5<-fdat$ACTIVE5_GROUP

#IS GROUP ACTIVE10 AT TIME OF SAMPLING?
#"ACTIVE10_GROUP" IF GROUP PRESENT IN YEAR OF SAMPLING
fdat$GROUP_FORM_lag10<-fdat$GROUP_FORM+10
fdat$ACTIVE10_GROUP<-ifelse(fdat$YEAR>=(fdat$GROUP_FORM_lag10)&fdat$GROUP_FORM_lag10!=10,1,0)
fdat$ACTIVE10<-fdat$ACTIVE10_GROUP

#HOW LONG HAS GROUP BEEN ACTIVE5?
#"GROUP_DURATION" is number of years group has been active. GROUP_DURATION=1 IF FOUNDED SAME YEAR AS SAMPLE

fdat$YEAR<-as.numeric(fdat$YEAR)
fdat$GROUP_FORM<-as.numeric(fdat$GROUP_FORM)
fdat$GROUP_DURATION<-rep(0,nrow(fdat))
for (i in 1:nrow(fdat))
{
	if (fdat$GROUP_FORM[i]!=0)
	{
		fdat$GROUP_DURATION[i] = fdat$YEAR[i] - fdat$GROUP_FORM[i]
	}
	else {fdat$GROUP_DURATION[i]=0}
	fdat$GROUP_DURATION[i]<-ifelse(fdat$GROUP_DURATION[i]<0,0,fdat$GROUP_DURATION[i])
}

fdat$ACTIVED<-fdat$GROUP_DURATION
fdat$STRAHLER<-as.numeric(fdat$STRAHLER)
fdat$PTL<-as.numeric(fdat$PTL)
fdat$NTL<-as.numeric(fdat$NTL)
fdat$PURB<-as.numeric(fdat$PURB)
fdat$PURB_std<-as.numeric(fdat$PURB)
fdat$PFOR<-as.numeric(fdat$PFOR)
fdat$PWETL<-as.numeric(fdat$PWETL)
fdat$POPDENS<-as.numeric(fdat$POPDENS)
fdat$GROUP<-as.numeric(fdat$GROUP)
fdat$RDDENS<-as.numeric(fdat$RDDENS)
fdat$XELEV<-as.numeric(fdat$XELEV)
fdat$W1_HALL<-as.numeric(fdat$W1_HALL)
fdat$MMI_BENT<-as.numeric(fdat$MMI_BENT)
fdat$TSS<-as.numeric(fdat$TSS)
fdat$TURB<-as.numeric(fdat$TURB)
fdat$HABT_PT<-as.numeric(fdat$HABT_PT)


#create mean for popdens by major basin to impute for NAs
MEAN_POPDENS<-tapply(na.omit(as.numeric(fdat$POPDENS)),subset(fdat$MAJ_BAS_NM,!is.na(fdat$POPDENS)),mean)
for (i in 1:nrow(fdat)){if (is.na(fdat$POPDENS[i])==TRUE){fdat$POPDENS[i] <- MEAN_POPDENS[which(names(MEAN_POPDENS)==fdat$MAJ_BAS_NM[i])]}}

#create mean for percent urban land area by major basin to impute for NAs
MEAN_PURB<-tapply(na.omit(as.numeric(fdat$PURB)),subset(fdat$MAJ_BAS_NM,!is.na(fdat$PURB)),mean)
for (i in 1:nrow(fdat)){if (is.na(fdat$PURB[i])==TRUE){fdat$PURB[i] <- MEAN_PURB[which(names(MEAN_PURB)==fdat$MAJ_BAS_NM[i])]}}

#create mean for percent forested (PFOR) land area by major basin to impute for NAs
MEAN_PFOR<-tapply(na.omit(as.numeric(fdat$PFOR)),subset(fdat$MAJ_BAS_NM,!is.na(fdat$PFOR)),mean)
for (i in 1:nrow(fdat)){if (is.na(fdat$PFOR[i])==TRUE){fdat$PFOR[i] <- MEAN_PFOR[which(names(MEAN_PFOR)==fdat$MAJ_BAS_NM[i])]}}

#create mean for percent wetlands (PWETL) land area by major basin to impute for NAs
MEAN_PWETL<-tapply(na.omit(as.numeric(fdat$PWETL)),subset(fdat$MAJ_BAS_NM,!is.na(fdat$PWETL)),mean)
for (i in 1:nrow(fdat)){if (is.na(fdat$PWETL[i])==TRUE){fdat$PWETL[i] <- MEAN_PWETL[which(names(MEAN_PWETL)==fdat$MAJ_BAS_NM[i])]}}

#create mean for percent road density (RRDENS) by major basin to impute for NAs
MEAN_RDDENS<-tapply(na.omit(as.numeric(fdat$RDDENS)),subset(fdat$MAJ_BAS_NM,!is.na(fdat$RDDENS)),mean)
for (i in 1:nrow(fdat)){if (is.na(fdat$RDDENS[i])==TRUE){fdat$RDDENS[i] <- MEAN_RDDENS[which(names(MEAN_RDDENS)==fdat$MAJ_BAS_NM[i])]}}

#create mean for elevation (XELEV) by major basin to impute for NAs
MEAN_XELEV<-tapply(na.omit(as.numeric(fdat$XELEV)),subset(fdat$MAJ_BAS_NM,!is.na(fdat$XELEV)),mean)
for (i in 1:nrow(fdat)){if (is.na(fdat$XELEV[i])==TRUE){fdat$XELEV[i] <- MEAN_XELEV[which(names(MEAN_XELEV)==fdat$MAJ_BAS_NM[i])]}}







#library(devtools); 
#install_github('mrp', 'malecki', sub='mrpdata'); 
#install_github(mrp”, “malecki”, sub=”mrp”)

#REPLACE NA'S IN WATERSHED AREA (A FEW MISSING IN NRSA SET, NOT MISSING IN WSA)
AREA<-fdat$WSAREA
for (i in 1:nrow(fdat))
{
	if (is.na(fdat$WSAREA[i])==TRUE){AREA[i]<-ifelse(is.na(fdat$LANDAREA[i])==FALSE,fdat$LANDAREA[i],fdat$LANDAREA[fdat$CON_ID==fdat$CON_ID[i]&fdat$SURVEY=='wsa'])}
}
fdat$WSAREA<-as.numeric(AREA)


#MAKE INDICATOR VARIABLES FOR GROUP ACTIVITY/RESPONSIBILITY

#low level collaboration: information sharing, coordination, outreach, education
temp<-rep(0,length(fdat$CON_ID))
for (i in 1:length(temp))
{
	if (length(grep("coordin",fdat$ENGAGED_IN[i])>0)){temp[i]=1}
	else if (length(grep("outreach",fdat$ENGAGED_IN[i])>0)){temp[i]=1}
	else if (length(grep("education",fdat$ENGAGED_IN[i])>0)){temp[i]=1}
	else {temp[i] = 0}
}
fdat$ENGAGED_COORD<-temp

temp<-rep(0,length(fdat$CON_ID))
for (i in 1:length(temp))
{
	if (length(grep("planning",fdat$ENGAGED_IN[i])>0)){temp[i]=1}
	else if (length(grep("monitoring",fdat$ENGAGED_IN[i])>0)){temp[i]=1}
	else if (length(grep("projects",fdat$ENGAGED_IN[i])>0)){temp[i]=1}
	else {temp[i] = 0}
}
fdat$ENGAGED_PLAN<-temp

temp<-rep(0,length(fdat$CON_ID))
for (i in 1:length(temp))
{
	if (length(grep("management",fdat$ENGAGED_IN[i])>0)){temp[i]=1}
		else {temp[i] = 0}
}
fdat$ENGAGED_MANAG<-temp

temp<-rep(0,length(fdat$CON_ID))
for (i in 1:length(temp))
{
	if (length(grep("advis",fdat$GOVERNANCE_BODY[i])>0)){temp[i]=1}
		else {temp[i] = 0}
}
fdat$ENGAGED_ADVIS<-temp

GROUP_RESPONSIBILITY<-rep(0,nrow(fdat))
for (i in 1:nrow(fdat))
{
	if (fdat$ENGAGED_MANAG[i]==1){GROUP_RESPONSIBILITY[i]="manage"}
	else if (fdat$ENGAGED_PLAN[i]==1){GROUP_RESPONSIBILITY[i]="plan"}
	else if (fdat$ENGAGED_COORD[i]==1){GROUP_RESPONSIBILITY[i]="coord"}
	else if (fdat$ENGAGED_ADVIS[i]==1){GROUP_RESPONSIBILITY[i]="advis"}
}
fdat$GROUP_RESPONSIBILITY<-GROUP_RESPONSIBILITY


#TRANSFORM FUNDING VARIABLE
FUND_STATE<-rep(0,nrow(fdat))
FUND_FED<-rep(0,nrow(fdat))
FUND_LOCAL<-rep(0,nrow(fdat))
for (i in 1:nrow(fdat))
{if (length(grep("local",fdat$FUNDING[i]))>0){FUND_LOCAL[i]=1}
if (length(grep("federal",fdat$FUNDING[i]))>0){FUND_FED[i]=1}
if (length(grep("state",fdat$FUNDING[i]))>0){FUND_STATE[i]=1}}
fdat$FUND_STATE<-FUND_STATE
fdat$FUND_LOCAL<-FUND_LOCAL
fdat$FUND_FED<-FUND_FED

#TRANSFORM GOAL-FORMALIZATION VARIABLE
FORM_OBJECT<-rep(0,nrow(fdat))
for (i in 1:nrow(fdat))
{if (length(grep("mission",fdat$OBJECTIVE_SETTING[i]))>0){FORM_OBJECT[i]="MISSION"}
else if (length(grep("goals",fdat$OBJECTIVE_SETTING[i]))>0){FORM_OBJECT[i]="GOALS"}
if (length(grep("objective",fdat$OBJECTIVE_SETTING[i]))>0){FORM_OBJECT[i]="OBJECTIVES"}}
fdat$FORM_OBJECT<-FORM_OBJECT


#CLEAN UP COORDINATOR VARIABLE, NAME NEW VARIABLE 'FACILITATE'
FACILITATE<-rep(0,nrow(fdat))
for (i in 1:nrow(fdat))
{FACILITATE[i]<-ifelse(fdat$COORDINATOR[i]=="yes",1,0);FACILITATE[i]<-ifelse(is.na(fdat$COORDINATOR[i])==TRUE,0,FACILITATE[i])}
fdat$FACILITATE<-FACILITATE

#CLEAN UP INTER-BOUNDARY VARIABLES
fdat$INTERSTATE<-ifelse(is.na(fdat$INTERSTATE)==TRUE,0,ifelse(fdat$INTERSTATE==1,1,0))
fdat$INTERNAT<-ifelse(is.na(fdat$INTERNAT)==TRUE,0,ifelse(fdat$INTERNAT==1,1,0))
fdat$OTHERSTATES<-ifelse(is.na(fdat$OTHERSTATES)==TRUE,0,ifelse(fdat$OTHERSTATES==1,1,0))
fdat$OTHERNAT<-ifelse(is.na(fdat$OTHERNAT)==TRUE,0,ifelse(fdat$OTHERNAT==1,1,0))
fdat$TRANS_BOUND<-ifelse(fdat$INTERSTATE==1|fdat$INTERNAT==1,1,0)
fdat$CROSS_BOUND<-ifelse(fdat$OTHERSTATES==1|fdat$OTHERNAT==1,1,0)

#clean up bylaws existence
fdat$HAS_BYLAWS<-ifelse(is.na(fdat$BYLAWS)==FALSE&fdat$BYLAWS!="",1,0)


#clean up and recode for inclusiveness, make MEM_TOTAL variable
fdat$MEM_TRIBE<-ifelse(is.na(fdat$MEM_TRIBE)==TRUE,0,ifelse(fdat$MEM_TRIBE=="0",0,1))
fdat$MEM_STAKE<-ifelse(is.na(fdat$MEM_STAKE)==TRUE,0,ifelse(fdat$MEM_STAKE=="0",0,1))
fdat$MEM_NGO<-ifelse(is.na(fdat$MEM_NGO)==TRUE,0,ifelse(fdat$MEM_NGO=="0",0,1))
fdat$MEM_STATE<-ifelse(is.na(fdat$MEM_STATE)==TRUE,0,ifelse(fdat$MEM_STATE=="0",0,1))
fdat$MEM_FED<-ifelse(is.na(fdat$MEM_FED)==TRUE,0,ifelse(fdat$MEM_FED=="0",0,1))
fdat$MEM_BUS<-ifelse(is.na(fdat$MEM_BUS)==TRUE,0,ifelse(fdat$MEM_BUS=="0",0,1))
fdat$MEM_AG<-ifelse(is.na(fdat$MEM_AG)==TRUE,0,ifelse(fdat$MEM_AG=="0",0,1))
fdat$MEM_UNIV<-ifelse(is.na(fdat$MEM_UNIV)==TRUE,0,ifelse(fdat$MEM_UNIV=="0",0,1))

fdat$MEM_TOTAL<-fdat$MEM_TRIBE+fdat$MEM_STAKE+fdat$MEM_NGO+fdat$MEM_STATE+fdat$MEM_FED+fdat$MEM_BUS+fdat$MEM_AG+fdat$MEM_UNIV


#clean up for tech body presence
fdat$TECH_BODY<-ifelse(is.na(fdat$TECH_BODY)==TRUE,0,ifelse(fdat$TECH_BODY=="0",0,1))

fdat$GOVERNANCE_BODY<-ifelse(is.na(fdat$GOVERNANCE_BODY)==TRUE,"",fdat$GOVERNANCE_BODY)

for (i in 1:nrow(fdat))
{
	if (is.na(fdat$GROUP_FORM[i])==FALSE&fdat$GROUP[i]==1&as.numeric(fdat$GROUP_FORM[i])>=fdat$YEAR[i])
	{fdat$GOVERNANCE_BODY[i]<-fdat$GOVERNANCE_BODY[i]}
	else {fdat$GOVERNANCE_BODY[i]<-""}
}


#set all huc8 values to 8 digits (add leading 0)
fdat$HUC8<-ifelse(nchar(fdat$HUC8)==7,paste("0",as.character(fdat$HUC8),sep=""),fdat$HUC8)
fdat$HUC4<-substr(fdat$HUC8,1,4)
fdat$HUC6<-substr(fdat$HUC8,1,6)
fdat$HUC4<-fdat$HUC6
fdat$XWIDTH<-as.numeric(fdat$XWIDTH)
fdat$STRAHLER<-as.numeric(fdat$STRAHLER)
fdat$W1_HAG<-as.numeric(fdat$W1_HAG)
fdat$W1_HNOAG<-as.numeric(fdat$W1_HNOAG)
fdat$FORM_OBJECT_num<-ifelse(fdat$FORM_OBJECT=="0",0,ifelse(fdat$FORM_OBJECT=="MISSION",1,ifelse(fdat$FORM_OBJECT=="GOALS",2,3)))

#reassign group responsibility, so all samples with NO GROUP and all samples with a group engaged in coordination activites are scored as "coord". This way, when group responsibility is fitted with an interaction term for whether or not a group is active, the effect only "turns on" when there is an active group (so the "NO GROUP" samples assigned a designation of "coord" for Group Responsibility cancel out), allowing the estimated effects to compare either "planning" or "management" to the null of "coordination". This is necessary because all groups engage in some sort of activity, so fitting a term for each type of activity overlaps with the effect of group presence whatsoever
fdat$GROUP_RESP_null<-ifelse(fdat$GROUP_RESPONSIBILITY=="0","coord",fdat$GROUP_RESPONSIBILITY)



#standardize each dependent variables so that effects are comparable
std=function(x){if(length(which(is.na(x)))==0) (x-mean(x))/sd(x) else
 
(x-mean(x,na.rm=T))/sd(x,na.rm=T)
}

fdat$PTL_stz<-std(fdat$PTL)
fdat$NTL_stz<-std(fdat$NTL)
fdat$MMI_BENT_stz<-std(fdat$MMI_BENT)
fdat$TURB_stz<-std(fdat$TURB)
fdat$XCMGW<-as.numeric(fdat$XCMGW)
fdat$XCMGW_stz<-std(as.numeric(fdat$XCMGW))
fdat$XFC_NAT<-as.numeric(fdat$XFC_NAT)
fdat$XFC_NAT_stz<-std(as.numeric(fdat$XFC_NAT))
fdat$SQ_MILE<-fdat$WSAREA*0.38610


library(car)


fdat$XWIDTH<-log(fdat$XWIDTH+0.001)
fdat$XELEV<-log(fdat$XELEV+0.001)
fdat$SQ_MILE<-log(fdat$SQ_MILE+0.001)
fdat$PURB<-log(fdat$PURB+0.001)
fdat$PWETL<-log(fdat$PWETL+0.001)
fdat$PFOR<-log(fdat$PFOR+0.001)
fdat$POPDENS<-log(fdat$POPDENS+0.001)
fdat$RDDENS<-log(fdat$RDDENS+0.001)
fdat$W1_HAG<-log(fdat$W1_HAG+0.001)
fdat$W1_HNOAG<-log(fdat$W1_HNOAG+0.001)
fdat$PTL<-log(fdat$PTL+0.001)
fdat$NTL<-log(fdat$NTL+0.001)
fdat$TURB<-log(fdat$TURB+0.001)
fdat$XCMGW<-(fdat$XCMGW^.5)
fdat$XFC_NAT<-(fdat$XFC_NAT^.5)
fdat$MMI_BENT<-(fdat$MMI_BENT)

fdat$PLAN<-ifelse(fdat$GROUP_RESP_null=="plan",1,0)
fdat$MANAGE<-ifelse(fdat$GROUP_RESP_null=="manage",1,0)

#UN-RESTRICTED MODELS: GROUP RESPONSIBILITY 0 lag
M_PTL_res0 <- lmer(PTL~ ACTIVE0+as.factor(YEAR)+XWIDTH+XELEV+SQ_MILE+STRAHLER+PURB+PWETL+PFOR+RDDENS+POPDENS+W1_HAG+W1_HNOAG+
ACTIVE0:PLAN+
ACTIVE0:MANAGE+
(1|HUC4)+(ACTIVE0|STATE),verbose=TRUE,data=fdat)
modsum_PTL_res0<-summary(M_PTL_res0,signature=signature(object = "merModLmerTest"))
write.csv(ranef(M_PTL_res0)[2], "//Users/TScott/Google Drive/duckabush/ptl_ranef_res0_0.csv")

M_NTL_res0 <- lmer(NTL~ ACTIVE0+as.factor(YEAR)+XWIDTH+XELEV+SQ_MILE+STRAHLER+PURB+PWETL+PFOR+RDDENS+POPDENS+W1_HAG+W1_HNOAG+
ACTIVE0:PLAN+
ACTIVE0:MANAGE+
(1|HUC4)+(ACTIVE0|STATE),verbose=TRUE,data=fdat)
modsum_NTL_res0<-summary(M_NTL_res0,signature=signature(object = "merModLmerTest"))
write.csv(ranef(M_NTL_res0)[2], "//Users/TScott/Google Drive/duckabush/ntl_ranef_res0_0.csv")

M_TURB_res0 <- lmer(TURB~ ACTIVE0+as.factor(YEAR)+XWIDTH+XELEV+SQ_MILE+STRAHLER+PURB+PWETL+PFOR+RDDENS+POPDENS+W1_HAG+W1_HNOAG+
ACTIVE0:PLAN+
ACTIVE0:MANAGE+
(1|HUC4)+(ACTIVE0|STATE),verbose=TRUE,data=fdat)
modsum_TURB_res0<-summary(M_TURB_res0,signature=signature(object = "merModLmerTest"))
write.csv(ranef(M_TURB_res0)[2], "//Users/TScott/Google Drive/duckabush/turb_ranef_res0_0.csv")

M_XFC_NAT_res0 <- lmer(XFC_NAT~ ACTIVE0+as.factor(YEAR)+XWIDTH+XELEV+SQ_MILE+STRAHLER+PURB+PWETL+PFOR+RDDENS+POPDENS+W1_HAG+W1_HNOAG+
ACTIVE0:PLAN+
ACTIVE0:MANAGE+
(1|HUC4)+(ACTIVE0|STATE),verbose=TRUE,data=fdat)
modsum_XFC_NAT_res0<-summary(M_XFC_NAT_res0,signature=signature(object = "merModLmerTest"))
write.csv(ranef(M_XFC_NAT_res0)[2], "//Users/TScott/Google Drive/duckabush/xfc_ranef_res0_0.csv")

fdat_MMI_BENT<-subset(fdat,is.na(fdat$MMI_BENT)==FALSE)
M_MMI_BENT_res0 <- lmer(MMI_BENT~ACTIVE0+as.factor(YEAR)+XWIDTH+XELEV+SQ_MILE+STRAHLER+PURB+PWETL+PFOR+RDDENS+POPDENS+W1_HAG+W1_HNOAG+
ACTIVE0:PLAN+
ACTIVE0:MANAGE+
(1|HUC4)+(ACTIVE0|STATE),verbose=TRUE,data=fdat_MMI_BENT)
modsum_MMI_BENT_res0<-summary(M_MMI_BENT_res0,signature=signature(object = "merModLmerTest"))
write.csv(ranef(M_MMI_BENT_res0)[2], "//Users/TScott/Google Drive/duckabush/mmi_ranef_res0_0.csv")

fdat_XCMGW<-subset(fdat,is.na(fdat$XCMGW)==FALSE)
M_XCMGW_res0 <- lmer(XCMGW~ ACTIVE0+as.factor(YEAR)+XWIDTH+XELEV+SQ_MILE+STRAHLER+PURB+PWETL+PFOR+RDDENS+POPDENS+W1_HAG+W1_HNOAG+
ACTIVE0:PLAN+
ACTIVE0:MANAGE+
(1|HUC4)+(ACTIVE0|STATE),verbose=TRUE,data=fdat_XCMGW)
modsum_XCMGW_res0<-summary(M_XCMGW_res0,signature=signature(object = "merModLmerTest"))
write.csv(ranef(M_XCMGW_res0)[2], "//Users/TScott/Google Drive/duckabush/xcmgw_ranef_res0_0.csv")

#make csv file that is table of fixed effects from res0 models
fix_res0<-cbind(modsum_PTL_res0$coef[c(-grep("YEAR",rownames(modsum_NTL_res0$coef)),-grep("Intercept",rownames(modsum_NTL_res0$coef))),],
car::Anova(M_PTL_res0)[-grep("YEAR",rownames(car::Anova(M_NTL_res0))),],
modsum_NTL_res0$coef[c(-grep("YEAR",rownames(modsum_NTL_res0$coef)),-grep("Intercept",rownames(modsum_NTL_res0$coef))),],
car::Anova(M_NTL_res0)[-grep("YEAR",rownames(car::Anova(M_NTL_res0))),],
modsum_TURB_res0$coef[c(-grep("YEAR",rownames(modsum_NTL_res0$coef)),-grep("Intercept",rownames(modsum_NTL_res0$coef))),],
car::Anova(M_TURB_res0)[-grep("YEAR",rownames(car::Anova(M_NTL_res0))),],
modsum_MMI_BENT_res0$coef[c(-grep("YEAR",rownames(modsum_NTL_res0$coef)),-grep("Intercept",rownames(modsum_NTL_res0$coef))),],
car::Anova(M_MMI_BENT_res0)[-grep("YEAR",rownames(car::Anova(M_NTL_res0))),],
modsum_XCMGW_res0$coef[c(-grep("YEAR",rownames(modsum_NTL_res0$coef)),-grep("Intercept",rownames(modsum_NTL_res0$coef))),],
car::Anova(M_XCMGW_res0)[-grep("YEAR",rownames(car::Anova(M_NTL_res0))),],
modsum_XFC_NAT_res0$coef[c(-grep("YEAR",rownames(modsum_NTL_res0$coef)),-grep("Intercept",rownames(modsum_NTL_res0$coef))),],
car::Anova(M_XFC_NAT_res0)[-grep("YEAR",rownames(car::Anova(M_NTL_res0))),])
colnames(fix_res0)<-c("PTL","p","NTL",'p','TURB','p','MMI_BENT','p','XCMGW','p','XFC_NAT','p')
write.csv(fix_res0, "//Users/TScott/Google Drive/duckabush/fixef_res0_0.csv")



#UN-RESTRICTED MODELS: GROUP RESPONSIBILITY 5 lag



M_PTL_res5 <- lmer(PTL~ ACTIVE5+as.factor(YEAR)+XWIDTH+XELEV+SQ_MILE+STRAHLER+PURB+PWETL+PFOR+RDDENS+POPDENS+W1_HAG+W1_HNOAG+
ACTIVE5:PLAN+
ACTIVE5:MANAGE+
(1|HUC4)+(ACTIVE5|STATE),verbose=TRUE,data=fdat)
modsum_PTL_res5<-summary(M_PTL_res5,signature=signature(object = "merModLmerTest"))
write.csv(ranef(M_PTL_res5)[2], "//Users/TScott/Google Drive/duckabush/ptl_ranef_res5_5.csv")

M_NTL_res5 <- lmer(NTL~ ACTIVE5+as.factor(YEAR)+XWIDTH+XELEV+SQ_MILE+STRAHLER+PURB+PWETL+PFOR+RDDENS+POPDENS+W1_HAG+W1_HNOAG+
ACTIVE5:PLAN+
ACTIVE5:MANAGE+
(1|HUC4)+(ACTIVE5|STATE),verbose=TRUE,data=fdat)
modsum_NTL_res5<-summary(M_NTL_res5,signature=signature(object = "merModLmerTest"))
write.csv(ranef(M_NTL_res5)[2], "//Users/TScott/Google Drive/duckabush/ntl_ranef_res5_5.csv")

M_TURB_res5 <- lmer(TURB~ ACTIVE5+as.factor(YEAR)+XWIDTH+XELEV+SQ_MILE+STRAHLER+PURB+PWETL+PFOR+RDDENS+POPDENS+W1_HAG+W1_HNOAG+
ACTIVE5:PLAN+
ACTIVE5:MANAGE+
(1|HUC4)+(ACTIVE5|STATE),verbose=TRUE,data=fdat)
modsum_TURB_res5<-summary(M_TURB_res5,signature=signature(object = "merModLmerTest"))
write.csv(ranef(M_TURB_res5)[2], "//Users/TScott/Google Drive/duckabush/turb_ranef_res5_5.csv")

M_XFC_NAT_res5 <- lmer(XFC_NAT~ ACTIVE5+as.factor(YEAR)+XWIDTH+XELEV+SQ_MILE+STRAHLER+PURB+PWETL+PFOR+RDDENS+POPDENS+W1_HAG+W1_HNOAG+
ACTIVE5:PLAN+
ACTIVE5:MANAGE+
(1|HUC4)+(ACTIVE5|STATE),verbose=TRUE,data=fdat)
modsum_XFC_NAT_res5<-summary(M_XFC_NAT_res5,signature=signature(object = "merModLmerTest"))
write.csv(ranef(M_XFC_NAT_res5)[2], "//Users/TScott/Google Drive/duckabush/xfc_ranef_res5_5.csv")

fdat_MMI_BENT<-subset(fdat,is.na(fdat$MMI_BENT)==FALSE)
M_MMI_BENT_res5 <- lmer(MMI_BENT~ACTIVE5+as.factor(YEAR)+XWIDTH+XELEV+SQ_MILE+STRAHLER+PURB+PWETL+PFOR+RDDENS+POPDENS+W1_HAG+W1_HNOAG+
ACTIVE5:PLAN+
ACTIVE5:MANAGE+
(1|HUC4)+(ACTIVE5|STATE),verbose=TRUE,data=fdat_MMI_BENT)
modsum_MMI_BENT_res5<-summary(M_MMI_BENT_res5,signature=signature(object = "merModLmerTest"))
write.csv(ranef(M_MMI_BENT_res5)[2], "//Users/TScott/Google Drive/duckabush/mmi_ranef_res5_5.csv")

fdat_XCMGW<-subset(fdat,is.na(fdat$XCMGW)==FALSE)
M_XCMGW_res5 <- lmer(XCMGW~ ACTIVE5+as.factor(YEAR)+XWIDTH+XELEV+SQ_MILE+STRAHLER+PURB+PWETL+PFOR+RDDENS+POPDENS+W1_HAG+W1_HNOAG+
ACTIVE5:PLAN+
ACTIVE5:MANAGE+
(1|HUC4)+(ACTIVE5|STATE),verbose=TRUE,data=fdat_XCMGW)
modsum_XCMGW_res5<-summary(M_XCMGW_res5,signature=signature(object = "merModLmerTest"))
write.csv(ranef(M_XCMGW_res5)[2], "//Users/TScott/Google Drive/duckabush/xcmgw_ranef_res5_5.csv")

#make csv file that is table of fixed effects from res5 models
fix_res5<-cbind(modsum_PTL_res5$coef[c(-grep("YEAR",rownames(modsum_NTL_res5$coef)),-grep("Intercept",rownames(modsum_NTL_res5$coef))),],
car::Anova(M_PTL_res5)[-grep("YEAR",rownames(car::Anova(M_NTL_res5))),],
modsum_NTL_res5$coef[c(-grep("YEAR",rownames(modsum_NTL_res5$coef)),-grep("Intercept",rownames(modsum_NTL_res5$coef))),],
car::Anova(M_NTL_res5)[-grep("YEAR",rownames(car::Anova(M_NTL_res5))),],
modsum_TURB_res5$coef[c(-grep("YEAR",rownames(modsum_NTL_res5$coef)),-grep("Intercept",rownames(modsum_NTL_res5$coef))),],
car::Anova(M_TURB_res5)[-grep("YEAR",rownames(car::Anova(M_NTL_res5))),],
modsum_MMI_BENT_res5$coef[c(-grep("YEAR",rownames(modsum_NTL_res5$coef)),-grep("Intercept",rownames(modsum_NTL_res5$coef))),],
car::Anova(M_MMI_BENT_res5)[-grep("YEAR",rownames(car::Anova(M_NTL_res5))),],
modsum_XCMGW_res5$coef[c(-grep("YEAR",rownames(modsum_NTL_res5$coef)),-grep("Intercept",rownames(modsum_NTL_res5$coef))),],
car::Anova(M_XCMGW_res5)[-grep("YEAR",rownames(car::Anova(M_NTL_res5))),],
modsum_XFC_NAT_res5$coef[c(-grep("YEAR",rownames(modsum_NTL_res5$coef)),-grep("Intercept",rownames(modsum_NTL_res5$coef))),],
car::Anova(M_XFC_NAT_res5)[-grep("YEAR",rownames(car::Anova(M_NTL_res5))),])
colnames(fix_res5)<-c("PTL","p","NTL",'p','TURB','p','MMI_BENT','p','XCMGW','p','XFC_NAT','p')
write.csv(fix_res5, "//Users/TScott/Google Drive/duckabush/fixef_res5_5.csv")



#UN-RESTRICTED MODELS: RESPONSIBILITY 10 lag

M_PTL_res10 <- lmer(PTL~ ACTIVE10+as.factor(YEAR)+XWIDTH+XELEV+SQ_MILE+STRAHLER+PURB+PWETL+PFOR+RDDENS+POPDENS+W1_HAG+W1_HNOAG+
ACTIVE10:PLAN+
ACTIVE10:MANAGE+
(1|HUC4)+(ACTIVE10|STATE),verbose=TRUE,data=fdat)
modsum_PTL_res10<-summary(M_PTL_res10,signature=signature(object = "merModLmerTest"))
write.csv(ranef(M_PTL_res10)[2], "//Users/TScott/Google Drive/duckabush/ptl_ranef_res10_10.csv")

M_NTL_res10 <- lmer(NTL~ ACTIVE10+as.factor(YEAR)+XWIDTH+XELEV+SQ_MILE+STRAHLER+PURB+PWETL+PFOR+RDDENS+POPDENS+W1_HAG+W1_HNOAG+
ACTIVE10:PLAN+
ACTIVE10:MANAGE+
(1|HUC4)+(ACTIVE10|STATE),verbose=TRUE,data=fdat)
modsum_NTL_res10<-summary(M_NTL_res10,signature=signature(object = "merModLmerTest"))
write.csv(ranef(M_NTL_res10)[2], "//Users/TScott/Google Drive/duckabush/ntl_ranef_res10_10.csv")

M_TURB_res10 <- lmer(TURB~ ACTIVE10+as.factor(YEAR)+XWIDTH+XELEV+SQ_MILE+STRAHLER+PURB+PWETL+PFOR+RDDENS+POPDENS+W1_HAG+W1_HNOAG+
ACTIVE10:PLAN+
ACTIVE10:MANAGE+
(1|HUC4)+(ACTIVE10|STATE),verbose=TRUE,data=fdat)
modsum_TURB_res10<-summary(M_TURB_res10,signature=signature(object = "merModLmerTest"))
write.csv(ranef(M_TURB_res10)[2], "//Users/TScott/Google Drive/duckabush/turb_ranef_res10_10.csv")

M_XFC_NAT_res10 <- lmer(XFC_NAT~ ACTIVE10+as.factor(YEAR)+XWIDTH+XELEV+SQ_MILE+STRAHLER+PURB+PWETL+PFOR+RDDENS+POPDENS+W1_HAG+W1_HNOAG+
ACTIVE10:PLAN+
ACTIVE10:MANAGE+
(1|HUC4)+(ACTIVE10|STATE),verbose=TRUE,data=fdat)
modsum_XFC_NAT_res10<-summary(M_XFC_NAT_res10,signature=signature(object = "merModLmerTest"))
write.csv(ranef(M_XFC_NAT_res10)[2], "//Users/TScott/Google Drive/duckabush/xfc_ranef_res10_10.csv")

fdat_MMI_BENT<-subset(fdat,is.na(fdat$MMI_BENT)==FALSE)
M_MMI_BENT_res10 <- lmer(MMI_BENT~ACTIVE10+as.factor(YEAR)+XWIDTH+XELEV+SQ_MILE+STRAHLER+PURB+PWETL+PFOR+RDDENS+POPDENS+W1_HAG+W1_HNOAG+
ACTIVE10:PLAN+
ACTIVE10:MANAGE+
(1|HUC4)+(ACTIVE10|STATE),verbose=TRUE,data=fdat_MMI_BENT)
modsum_MMI_BENT_res10<-summary(M_MMI_BENT_res10,signature=signature(object = "merModLmerTest"))
write.csv(ranef(M_MMI_BENT_res10)[2], "//Users/TScott/Google Drive/duckabush/mmi_ranef_res10_10.csv")

fdat_XCMGW<-subset(fdat,is.na(fdat$XCMGW)==FALSE)
M_XCMGW_res10 <- lmer(XCMGW~ ACTIVE10+as.factor(YEAR)+XWIDTH+XELEV+SQ_MILE+STRAHLER+PURB+PWETL+PFOR+RDDENS+POPDENS+W1_HAG+W1_HNOAG+
ACTIVE10:PLAN+
ACTIVE10:MANAGE+
(1|HUC4)+(ACTIVE10|STATE),verbose=TRUE,data=fdat_XCMGW)
modsum_XCMGW_res10<-summary(M_XCMGW_res10,signature=signature(object = "merModLmerTest"))
write.csv(ranef(M_XCMGW_res10)[2], "//Users/TScott/Google Drive/duckabush/xcmgw_ranef_res10_10.csv")

#make csv file that is table of fixed effects from res10 models
fix_res10<-cbind(modsum_PTL_res10$coef[c(-grep("YEAR",rownames(modsum_NTL_res10$coef)),-grep("Intercept",rownames(modsum_NTL_res10$coef))),],
car::Anova(M_PTL_res10)[-grep("YEAR",rownames(car::Anova(M_NTL_res10))),],
modsum_NTL_res10$coef[c(-grep("YEAR",rownames(modsum_NTL_res10$coef)),-grep("Intercept",rownames(modsum_NTL_res10$coef))),],
car::Anova(M_NTL_res10)[-grep("YEAR",rownames(car::Anova(M_NTL_res10))),],
modsum_TURB_res10$coef[c(-grep("YEAR",rownames(modsum_NTL_res10$coef)),-grep("Intercept",rownames(modsum_NTL_res10$coef))),],
car::Anova(M_TURB_res10)[-grep("YEAR",rownames(car::Anova(M_NTL_res10))),],
modsum_MMI_BENT_res10$coef[c(-grep("YEAR",rownames(modsum_NTL_res10$coef)),-grep("Intercept",rownames(modsum_NTL_res10$coef))),],
car::Anova(M_MMI_BENT_res10)[-grep("YEAR",rownames(car::Anova(M_NTL_res10))),],
modsum_XCMGW_res10$coef[c(-grep("YEAR",rownames(modsum_NTL_res10$coef)),-grep("Intercept",rownames(modsum_NTL_res10$coef))),],
car::Anova(M_XCMGW_res10)[-grep("YEAR",rownames(car::Anova(M_NTL_res10))),],
modsum_XFC_NAT_res10$coef[c(-grep("YEAR",rownames(modsum_NTL_res10$coef)),-grep("Intercept",rownames(modsum_NTL_res10$coef))),],
car::Anova(M_XFC_NAT_res10)[-grep("YEAR",rownames(car::Anova(M_NTL_res10))),])
colnames(fix_res10)<-c("PTL","p","NTL",'p','TURB','p','MMI_BENT','p','XCMGW','p','XFC_NAT','p')
write.csv(fix_res10, "//Users/TScott/Google Drive/duckabush/fixef_res10_10.csv")


#UN-RESTRICTED MODELS: RESPONSIBILITY Duration


M_PTL_resD <- lmer(PTL~ ACTIVED+as.factor(YEAR)+XWIDTH+XELEV+SQ_MILE+STRAHLER+PURB+PWETL+PFOR+RDDENS+POPDENS+W1_HAG+W1_HNOAG+
ACTIVED:PLAN+
ACTIVED:MANAGE+
(1|HUC4)+(ACTIVED|STATE),verbose=TRUE,data=fdat)
modsum_PTL_resD<-summary(M_PTL_resD,signature=signature(object = "merModLmerTest"))
write.csv(ranef(M_PTL_resD)[2], "//Users/TScott/Google Drive/duckabush/ptl_ranef_resD_D.csv")

M_NTL_resD <- lmer(NTL~ ACTIVED+as.factor(YEAR)+XWIDTH+XELEV+SQ_MILE+STRAHLER+PURB+PWETL+PFOR+RDDENS+POPDENS+W1_HAG+W1_HNOAG+
ACTIVED:PLAN+
ACTIVED:MANAGE+
(1|HUC4)+(ACTIVED|STATE),verbose=TRUE,data=fdat)
modsum_NTL_resD<-summary(M_NTL_resD,signature=signature(object = "merModLmerTest"))
write.csv(ranef(M_NTL_resD)[2], "//Users/TScott/Google Drive/duckabush/ntl_ranef_resD_D.csv")

M_TURB_resD <- lmer(TURB~ ACTIVED+as.factor(YEAR)+XWIDTH+XELEV+SQ_MILE+STRAHLER+PURB+PWETL+PFOR+RDDENS+POPDENS+W1_HAG+W1_HNOAG+
ACTIVED:PLAN+
ACTIVED:MANAGE+
(1|HUC4)+(ACTIVED|STATE),verbose=TRUE,data=fdat)
modsum_TURB_resD<-summary(M_TURB_resD,signature=signature(object = "merModLmerTest"))
write.csv(ranef(M_TURB_resD)[2], "//Users/TScott/Google Drive/duckabush/turb_ranef_resD_D.csv")

M_XFC_NAT_resD <- lmer(XFC_NAT~ ACTIVED+as.factor(YEAR)+XWIDTH+XELEV+SQ_MILE+STRAHLER+PURB+PWETL+PFOR+RDDENS+POPDENS+W1_HAG+W1_HNOAG+
ACTIVED:PLAN+
ACTIVED:MANAGE+
(1|HUC4)+(ACTIVED|STATE),verbose=TRUE,data=fdat)
modsum_XFC_NAT_resD<-summary(M_XFC_NAT_resD,signature=signature(object = "merModLmerTest"))
write.csv(ranef(M_XFC_NAT_resD)[2], "//Users/TScott/Google Drive/duckabush/xfc_ranef_resD_D.csv")

fdat_MMI_BENT<-subset(fdat,is.na(fdat$MMI_BENT)==FALSE)
M_MMI_BENT_resD <- lmer(MMI_BENT~ACTIVED+as.factor(YEAR)+XWIDTH+XELEV+SQ_MILE+STRAHLER+PURB+PWETL+PFOR+RDDENS+POPDENS+W1_HAG+W1_HNOAG+
ACTIVED:PLAN+
ACTIVED:MANAGE+
(1|HUC4)+(ACTIVED|STATE),verbose=TRUE,data=fdat_MMI_BENT)
modsum_MMI_BENT_resD<-summary(M_MMI_BENT_resD,signature=signature(object = "merModLmerTest"))
write.csv(ranef(M_MMI_BENT_resD)[2], "//Users/TScott/Google Drive/duckabush/mmi_ranef_resD_D.csv")

fdat_XCMGW<-subset(fdat,is.na(fdat$XCMGW)==FALSE)
M_XCMGW_resD <- lmer(XCMGW~ ACTIVED+as.factor(YEAR)+XWIDTH+XELEV+SQ_MILE+STRAHLER+PURB+PWETL+PFOR+RDDENS+POPDENS+W1_HAG+W1_HNOAG+
ACTIVED:PLAN+
ACTIVED:MANAGE+
(1|HUC4)+(ACTIVED|STATE),verbose=TRUE,data=fdat_XCMGW)
modsum_XCMGW_resD<-summary(M_XCMGW_resD,signature=signature(object = "merModLmerTest"))
write.csv(ranef(M_XCMGW_resD)[2], "//Users/TScott/Google Drive/duckabush/xcmgw_ranef_resD_D.csv")

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
write.csv(fix_resD, "//Users/TScott/Google Drive/duckabush/fixef_resD_D.csv")

stargazer(M_PTL_res0,M_NTL_res0,M_TURB_res0,M_MMI_BENT_res0,M_XCMGW_res0,M_XFC_NAT_res0)
stargazer(M_PTL_res5,M_NTL_res5,M_TURB_res5,M_MMI_BENT_res5,M_XCMGW_res5,M_XFC_NAT_res5)
stargazer(M_PTL_res10,M_NTL_res10,M_TURB_res10,M_MMI_BENT_res10,M_XCMGW_res10,M_XFC_NAT_res10)
stargazer(M_PTL_resD,M_NTL_resD,M_TURB_resD,M_MMI_BENT_resD,M_XCMGW_resD,M_XFC_NAT_resD)

write.table(stargazer(M_PTL_res0,M_NTL_res0,M_TURB_res0,M_MMI_BENT_res0,M_XCMGW_res0,M_XFC_NAT_res0),file="//Users/TScott/Google Drive/duckabush/latextable_res0")
write.table(stargazer(M_PTL_res5,M_NTL_res5,M_TURB_res5,M_MMI_BENT_res5,M_XCMGW_res5,M_XFC_NAT_res5),file="//Users/TScott/Google Drive/duckabush/latextable_res5")
write.table(stargazer(M_PTL_res10,M_NTL_res10,M_TURB_res10,M_MMI_BENT_res10,M_XCMGW_res10,M_XFC_NAT_res10),file="//Users/TScott/Google Drive/duckabush/latextable_res10")
write.table(stargazer(M_PTL_resD,M_NTL_resD,M_TURB_resD,M_MMI_BENT_resD,M_XCMGW_resD,M_XFC_NAT_resD),file="//Users/TScott/Google Drive/duckabush/latextable_resD")


