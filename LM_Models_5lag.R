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

#IS GROUP ACTIVE AT TIME OF SAMPLING?
#"ACTIVE_GROUP" IF GROUP PRESENT IN YEAR OF SAMPLING
fdat$GROUP_FORM_lag<-fdat$GROUP_FORM+5
fdat$ACTIVE_GROUP<-ifelse(fdat$YEAR>=(fdat$GROUP_FORM_lag)&fdat$GROUP_FORM_lag!=5,1,0)
fdat$ACTIVE<-fdat$ACTIVE_GROUP
#HOW LONG HAS GROUP BEEN ACTIVE?
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
#disable for lagged active group
#fdat$ACTIVE<-fdat$GROUP_DURATION

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


#BASE MODELS
M_PTL_base <-lm(PTL~XWIDTH+XELEV+ACTIVE+as.factor(YEAR)+SQ_MILE+STRAHLER+PURB+PWETL+PFOR+RDDENS+POPDENS+W1_HAG+W1_HNOAG+HUC4,data=fdat,verbose=TRUE)
summary(M_PTL_base)
modsum_PTL_base<-summary(M_PTL_base,signature=signature(object = "merModLmerTest"))

M_NTL_base <- lm(NTL~XWIDTH+XELEV+ACTIVE+as.factor(YEAR)+SQ_MILE+STRAHLER+PURB+PWETL+PFOR+RDDENS+POPDENS+W1_HAG+W1_HNOAG+HUC4,data=fdat,verbose=TRUE)
modsum_NTL_base<-summary(M_NTL_base,signature=signature(object = "merModLmerTest"))

M_TURB_base <- lm(TURB~XWIDTH+XELEV+ACTIVE+as.factor(YEAR)+SQ_MILE+STRAHLER+PURB+PWETL+PFOR+RDDENS+POPDENS+W1_HAG+W1_HNOAG+HUC4,data=fdat,verbose=TRUE)
modsum_TURB_base<-summary(M_TURB_base,signature=signature(object = "merModLmerTest"))


M_XFC_NAT_base <- lm(XFC_NAT~XWIDTH+XELEV+ACTIVE+as.factor(YEAR)+SQ_MILE+STRAHLER+PURB+PWETL+PFOR+RDDENS+POPDENS+W1_HAG+W1_HNOAG+HUC4,data=fdat,verbose=TRUE)
modsum_XFC_NAT_base<-summary(M_XFC_NAT_base,signature=signature(object = "merModLmerTest"))


fdat_MMI_BENT<-subset(fdat,is.na(fdat$MMI_BENT)==FALSE)
M_MMI_BENT_base <- lm(MMI_BENT~XWIDTH+XELEV+ACTIVE+as.factor(YEAR)+SQ_MILE+STRAHLER+PURB+PWETL+PFOR+RDDENS+POPDENS+W1_HAG+W1_HNOAG+HUC4,data=fdat_MMI_BENT,verbose=TRUE)
modsum_MMI_BENT_base<-summary(M_MMI_BENT_base,signature=signature(object = "merModLmerTest"))


fdat_XCMGW<-subset(fdat,is.na(fdat$XCMGW_stz)==FALSE)
M_XCMGW_base <- lm(XCMGW~XWIDTH+XELEV+ACTIVE+as.factor(YEAR)+SQ_MILE+STRAHLER+PURB+PWETL+PFOR+RDDENS+POPDENS+W1_HAG+W1_HNOAG+HUC4,data=fdat_XCMGW,verbose=TRUE)
modsum_XCMGW_base<-summary(M_XCMGW_base,signature=signature(object = "merModLmerTest"))


#make csv file that is table of fixed effects from base models
fix_base<-cbind(modsum_PTL_base$coef[,1],
modsum_PTL_base$coef[,4],
modsum_NTL_base$coef[,1],
modsum_NTL_base$coef[,4],
modsum_TURB_base$coef[,1],
modsum_TURB_base$coef[,4],
modsum_MMI_BENT_base$coef[,1],
modsum_MMI_BENT_base$coef[,4],
modsum_XCMGW_base$coef[,1],
modsum_XCMGW_base$coef[,4],
modsum_XFC_NAT_base$coef[,1],
modsum_XFC_NAT_base$coef[,4])
colnames(fix_base)<-c("PTL","p","NTL",'p','TURB','p','MMI_BENT','p','XCMGW','p','XFC_NAT','p')
write.csv(fix_base, "//Users/TScott/Google Drive/duckabush/fixef_base_lm5.csv")


#UN-RESTRICTED MODELS: REPRESENTATION
M_PTL_rep <- lm(PTL~XWIDTH+XELEV+ACTIVE+as.factor(YEAR)+SQ_MILE+STRAHLER+PURB+PWETL+PFOR+RDDENS++POPDENS+W1_HAG+W1_HNOAG+ACTIVE:CROSS_BOUND:TRANS_BOUND+ACTIVE:MEM_TOTAL+
TECH_BODY:ACTIVE+HUC4,data=fdat,verbose=TRUE)
modsum_PTL_rep<-summary(M_PTL_rep,signature=signature(object = "merModLmerTest"))



M_NTL_rep <- lm(NTL~XWIDTH+XELEV+ACTIVE+as.factor(YEAR)+SQ_MILE+STRAHLER+PURB+PWETL+PFOR+RDDENS++POPDENS+W1_HAG+W1_HNOAG+ACTIVE:CROSS_BOUND:TRANS_BOUND+ACTIVE:MEM_TOTAL+
TECH_BODY:ACTIVE+HUC4,data=fdat,verbose=TRUE)
modsum_NTL_rep<-summary(M_NTL_rep,signature=signature(object = "merModLmerTest"))

M_TURB_rep <- lm(TURB~XWIDTH+XELEV+ACTIVE+as.factor(YEAR)+SQ_MILE+STRAHLER+PURB+PWETL+PFOR+RDDENS++POPDENS+W1_HAG+W1_HNOAG+ACTIVE:CROSS_BOUND:TRANS_BOUND+ACTIVE:MEM_TOTAL+
TECH_BODY:ACTIVE+HUC4,data=fdat,verbose=TRUE)
modsum_TURB_rep<-summary(M_TURB_rep,signature=signature(object = "merModLmerTest"))

M_XFC_NAT_rep <- lm(XFC_NAT~XWIDTH+XELEV+ACTIVE+as.factor(YEAR)+SQ_MILE+STRAHLER+PURB+PWETL+PFOR+RDDENS++POPDENS+W1_HAG+W1_HNOAG+ACTIVE:CROSS_BOUND:TRANS_BOUND+ACTIVE:MEM_TOTAL+
TECH_BODY:ACTIVE+HUC4,data=fdat,verbose=TRUE)
modsum_XFC_NAT_rep<-summary(M_XFC_NAT_rep,signature=signature(object = "merModLmerTest"))


fdat_MMI_BENT<-subset(fdat,is.na(fdat$MMI_BENT)==FALSE)
M_MMI_BENT_rep <- lm(MMI_BENT~XWIDTH+XELEV+ACTIVE+as.factor(YEAR)+SQ_MILE+STRAHLER+PURB+PWETL+PFOR+RDDENS++POPDENS+W1_HAG+W1_HNOAG+ACTIVE:CROSS_BOUND:TRANS_BOUND+ACTIVE:MEM_TOTAL+
TECH_BODY:ACTIVE+HUC4,data=fdat_MMI_BENT,verbose=TRUE)
modsum_MMI_BENT_rep<-summary(M_MMI_BENT_rep,signature=signature(object = "merModLmerTest"))


fdat_XCMGW<-subset(fdat,is.na(fdat$XCMGW_stz)==FALSE)
M_XCMGW_rep <- lm(XCMGW~XWIDTH+XELEV+ACTIVE+as.factor(YEAR)+SQ_MILE+STRAHLER+PURB+PWETL+PFOR+RDDENS++POPDENS+W1_HAG+W1_HNOAG+ACTIVE:CROSS_BOUND:TRANS_BOUND+ACTIVE:MEM_TOTAL+
TECH_BODY:ACTIVE+HUC4,data=fdat_XCMGW,verbose=TRUE)
modsum_XCMGW_rep<-summary(M_XCMGW_rep,signature=signature(object = "merModLmerTest"))


#make csv file that is table of fixed effects from base models
fix_rep<-cbind(modsum_PTL_rep$coef[,1],
modsum_PTL_rep$coef[,4],
modsum_NTL_rep$coef[,1],
modsum_NTL_rep$coef[,4],
modsum_TURB_rep$coef[,1],
modsum_TURB_rep$coef[,4],
modsum_MMI_BENT_rep$coef[,1],
modsum_MMI_BENT_rep$coef[,4],
modsum_XCMGW_rep$coef[,1],
modsum_XCMGW_rep$coef[,4],
modsum_XFC_NAT_rep$coef[,1],
modsum_XFC_NAT_rep$coef[,4])
colnames(fix_rep)<-c("PTL","p","NTL",'p','TURB','p','MMI_BENT','p','XCMGW','p','XFC_NAT','p')
write.csv(fix_rep, "//Users/TScott/Google Drive/duckabush/fixef_rep_lm5.csv")




#UN-RESTRICTED MODELS: GROUP RESPONSIBILITY
M_PTL_res <- lm(PTL~ ACTIVE+YEAR+XWIDTH+XELEV+SQ_MILE+STRAHLER+PURB+PWETL+PFOR+RDDENS+POPDENS+W1_HAG+W1_HNOAG+
ACTIVE:as.factor(GROUP_RESP_null)+HUC4,verbose=TRUE,data=fdat)
modsum_PTL_res<-summary(M_PTL_res,signature=signature(object = "merModLmerTest"))


M_NTL_res <- lm(NTL~ ACTIVE+YEAR+XWIDTH+XELEV+SQ_MILE+STRAHLER+PURB+PWETL+PFOR+RDDENS+POPDENS+W1_HAG+W1_HNOAG+
ACTIVE:as.factor(GROUP_RESP_null)+HUC4,verbose=TRUE,data=fdat)
modsum_NTL_res<-summary(M_NTL_res,signature=signature(object = "merModLmerTest"))

M_TURB_res <- lm(TURB~ ACTIVE+YEAR+XWIDTH+XELEV+SQ_MILE+STRAHLER+PURB+PWETL+PFOR+RDDENS+POPDENS+W1_HAG+W1_HNOAG+
ACTIVE:as.factor(GROUP_RESP_null)+HUC4,verbose=TRUE,data=fdat)
modsum_TURB_res<-summary(M_TURB_res,signature=signature(object = "merModLmerTest"))

M_XFC_NAT_res <- lm(XFC_NAT~ ACTIVE+YEAR+XWIDTH+XELEV+SQ_MILE+STRAHLER+PURB+PWETL+PFOR+RDDENS+POPDENS+W1_HAG+W1_HNOAG+
ACTIVE:as.factor(GROUP_RESP_null)+HUC4,verbose=TRUE,data=fdat)
modsum_XFC_NAT_res<-summary(M_XFC_NAT_res,signature=signature(object = "merModLmerTest"))

fdat_MMI_BENT<-subset(fdat,is.na(fdat$MMI_BENT)==FALSE)
M_MMI_BENT_res <- lm(MMI_BENT~ ACTIVE+YEAR+XWIDTH+XELEV+SQ_MILE+STRAHLER+PURB+PWETL+PFOR+RDDENS+POPDENS+W1_HAG+W1_HNOAG+
ACTIVE:as.factor(GROUP_RESP_null)+HUC4,verbose=TRUE,data=fdat_MMI_BENT)
modsum_MMI_BENT_res<-summary(M_MMI_BENT_res,signature=signature(object = "merModLmerTest"))

fdat_XCMGW<-subset(fdat,is.na(fdat$XCMGW)==FALSE)
M_XCMGW_res <- lm(MMI_BENT~ ACTIVE+YEAR+XWIDTH+XELEV+SQ_MILE+STRAHLER+PURB+PWETL+PFOR+RDDENS+POPDENS+W1_HAG+W1_HNOAG+
ACTIVE:as.factor(GROUP_RESP_null)+HUC4,verbose=TRUE,data=fdat_XCMGW)
modsum_XCMGW_res<-summary(M_XCMGW_res,signature=signature(object = "merModLmerTest"))


#make csv file that is table of fixed effects from base models
fix_res<-cbind(modsum_PTL_res$coef[,1],
modsum_PTL_res$coef[,4],
modsum_NTL_res$coef[,1],
modsum_NTL_res$coef[,4],
modsum_TURB_res$coef[,1],
modsum_TURB_res$coef[,4],
modsum_MMI_BENT_res$coef[,1],
modsum_MMI_BENT_res$coef[,4],
modsum_XCMGW_res$coef[,1],
modsum_XCMGW_res$coef[,4],
modsum_XFC_NAT_res$coef[,1],
modsum_XFC_NAT_res$coef[,4])
colnames(fix_res)<-c("PTL","p","NTL",'p','TURB','p','MMI_BENT','p','XCMGW','p','XFC_NAT','p')
write.csv(fix_res, "//Users/TScott/Google Drive/duckabush/fixef_res_lm5.csv")



fdat$obj_trans<-ifelse(fdat$FORM_OBJECT=="0","MISSION",fdat$FORM_OBJECT)
fdat$obj_trans<-relevel(as.factor(fdat$obj_trans),ref="MISSION")


#UN-RESTRICTED MODELS: FORMALIZATION
M_PTL_form <- lm(PTL~ACTIVE+YEAR+XWIDTH+XELEV+SQ_MILE+STRAHLER+PURB+PWETL+PFOR+RDDENS+POPDENS+W1_HAG+W1_HNOAG+
ACTIVE:FACILITATE+
ACTIVE:obj_trans+
ACTIVE:HAS_BYLAWS+HUC4,verbose=TRUE,data=fdat)
modsum_PTL_form<-summary(M_PTL_form,signature=signature(object = "merModLmerTest"))


M_NTL_form <- lm(NTL~ACTIVE+YEAR+XWIDTH+XELEV+SQ_MILE+STRAHLER+PURB+PWETL+PFOR+RDDENS+POPDENS+W1_HAG+W1_HNOAG+
ACTIVE:FACILITATE+
ACTIVE:obj_trans+
ACTIVE:HAS_BYLAWS+HUC4,verbose=TRUE,data=fdat)
modsum_NTL_form<-summary(M_NTL_form,signature=signature(object = "merModLmerTest"))

M_TURB_form <- lm(TURB~ACTIVE+YEAR+XWIDTH+XELEV+SQ_MILE+STRAHLER+PURB+PWETL+PFOR+RDDENS+POPDENS+W1_HAG+W1_HNOAG+
ACTIVE:FACILITATE+
ACTIVE:obj_trans+
ACTIVE:HAS_BYLAWS+HUC4,verbose=TRUE,data=fdat)
modsum_TURB_form<-summary(M_TURB_form,signature=signature(object = "merModLmerTest"))

M_XFC_NAT_form <- lm(XFC_NAT~ACTIVE+YEAR+XWIDTH+XELEV+SQ_MILE+STRAHLER+PURB+PWETL+PFOR+RDDENS+POPDENS+W1_HAG+W1_HNOAG+
ACTIVE:FACILITATE+
ACTIVE:obj_trans+
ACTIVE:HAS_BYLAWS+HUC4,verbose=TRUE,data=fdat)
modsum_XFC_NAT_form<-summary(M_XFC_NAT_form,signature=signature(object = "merModLmerTest"))

fdat_MMI_BENT<-subset(fdat,is.na(fdat$MMI_BENT)==FALSE)
M_MMI_BENT_form <- lm(MMI_BENT~ACTIVE+YEAR+XWIDTH+XELEV+SQ_MILE+STRAHLER+PURB+PWETL+PFOR+RDDENS+POPDENS+W1_HAG+W1_HNOAG+
ACTIVE:FACILITATE+
ACTIVE:obj_trans+
ACTIVE:HAS_BYLAWS+HUC4,verbose=TRUE,data=fdat_MMI_BENT)
modsum_MMI_BENT_form<-summary(M_MMI_BENT_form,signature=signature(object = "merModLmerTest"))

fdat_XCMGW<-subset(fdat,is.na(fdat$XCMGW)==FALSE)
M_XCMGW_form <- lm(XCMGW~ACTIVE+YEAR+XWIDTH+XELEV+SQ_MILE+STRAHLER+PURB+PWETL+PFOR+RDDENS+POPDENS+W1_HAG+W1_HNOAG+
ACTIVE:FACILITATE+
ACTIVE:obj_trans+
ACTIVE:HAS_BYLAWS+HUC4,verbose=TRUE,data=fdat_XCMGW)
modsum_XCMGW_form<-summary(M_XCMGW_form,signature=signature(object = "merModLmerTest"))


#make csv file that is table of fixed effects from base models
fix_form<-cbind(modsum_PTL_form$coef[,1],
modsum_PTL_form$coef[,4],
modsum_NTL_form$coef[,1],
modsum_NTL_form$coef[,4],
modsum_TURB_form$coef[,1],
modsum_TURB_form$coef[,4],
modsum_MMI_BENT_form$coef[,1],
modsum_MMI_BENT_form$coef[,4],
modsum_XCMGW_form$coef[,1],
modsum_XCMGW_form$coef[,4],
modsum_XFC_NAT_form$coef[,1],
modsum_XFC_NAT_form$coef[,4])
colnames(fix_form)<-c("PTL","p","NTL",'p','TURB','p','MMI_BENT','p','XCMGW','p','XFC_NAT','p')
write.csv(fix_form, "//Users/TScott/Google Drive/duckabush/fixef_form_lm5.csv")



#UN-RESTRICTED MODELS: FUNDING
M_PTL_fun <- lm(PTL~ACTIVE+YEAR+XWIDTH+XELEV+SQ_MILE+STRAHLER+PURB+PWETL+PFOR+RDDENS+POPDENS+W1_HAG+W1_HNOAG+
ACTIVE:FUND_LOCAL+
ACTIVE:FUND_STATE+
ACTIVE:FUND_FED+HUC4,verbose=TRUE,data=fdat)
modsum_PTL_fun<-summary(M_PTL_fun,signature=signature(object = "merModLmerTest"))


M_NTL_fun <- lm(NTL~ACTIVE+YEAR+XWIDTH+XELEV+SQ_MILE+STRAHLER+PURB+PWETL+PFOR+RDDENS+POPDENS+W1_HAG+W1_HNOAG+
ACTIVE:FUND_LOCAL+
ACTIVE:FUND_STATE+
ACTIVE:FUND_FED+HUC4,verbose=TRUE,data=fdat)
modsum_NTL_fun<-summary(M_NTL_fun,signature=signature(object = "merModLmerTest"))

M_TURB_fun <- lm(TURB~ACTIVE+YEAR+XWIDTH+XELEV+SQ_MILE+STRAHLER+PURB+PWETL+PFOR+RDDENS+POPDENS+W1_HAG+W1_HNOAG+
ACTIVE:FUND_LOCAL+
ACTIVE:FUND_STATE+
ACTIVE:FUND_FED+HUC4,verbose=TRUE,data=fdat)
modsum_TURB_fun<-summary(M_TURB_fun,signature=signature(object = "merModLmerTest"))

M_XFC_NAT_fun <- lm(XFC_NAT~ACTIVE+YEAR+XWIDTH+XELEV+SQ_MILE+STRAHLER+PURB+PWETL+PFOR+RDDENS+POPDENS+W1_HAG+W1_HNOAG+
ACTIVE:FUND_LOCAL+
ACTIVE:FUND_STATE+
ACTIVE:FUND_FED+HUC4,verbose=TRUE,data=fdat)
modsum_XFC_NAT_fun<-summary(M_XFC_NAT_fun,signature=signature(object = "merModLmerTest"))

fdat_MMI_BENT<-subset(fdat,is.na(fdat$MMI_BENT)==FALSE)
M_MMI_BENT_fun <- lm(MMI_BENT~ACTIVE+YEAR+XWIDTH+XELEV+SQ_MILE+STRAHLER+PURB+PWETL+PFOR+RDDENS+POPDENS+W1_HAG+W1_HNOAG+
ACTIVE:FUND_LOCAL+
ACTIVE:FUND_STATE+
ACTIVE:FUND_FED+HUC4,verbose=TRUE,data=fdat_MMI_BENT)
modsum_MMI_BENT_fun<-summary(M_MMI_BENT_fun,signature=signature(object = "merModLmerTest"))

fdat_XCMGW<-subset(fdat,is.na(fdat$XCMGW)==FALSE)
M_XCMGW_fun <- lm(XCMGW~ACTIVE+YEAR+XWIDTH+XELEV+SQ_MILE+STRAHLER+PURB+PWETL+PFOR+RDDENS+POPDENS+W1_HAG+W1_HNOAG+
ACTIVE:FUND_LOCAL+
ACTIVE:FUND_STATE+
ACTIVE:FUND_FED+HUC4,verbose=TRUE,data=fdat_XCMGW)
modsum_XCMGW_fun<-summary(M_XCMGW_fun,signature=signature(object = "merModLmerTest"))


#make csv file that is table of fixed effects from base models
fix_fun<-cbind(modsum_PTL_fun$coef[,1],
modsum_PTL_fun$coef[,4],
modsum_NTL_fun$coef[,1],
modsum_NTL_fun$coef[,4],
modsum_TURB_fun$coef[,1],
modsum_TURB_fun$coef[,4],
modsum_MMI_BENT_fun$coef[,1],
modsum_MMI_BENT_fun$coef[,4],
modsum_XCMGW_fun$coef[,1],
modsum_XCMGW_fun$coef[,4],
modsum_XFC_NAT_fun$coef[,1],
modsum_XFC_NAT_fun$coef[,4])
colnames(fix_fun)<-c("PTL","p","NTL",'p','TURB','p','MMI_BENT','p','XCMGW','p','XFC_NAT','p')
write.csv(fix_fun, "//Users/TScott/Google Drive/duckabush/fixef_fun_lm5.csv")




