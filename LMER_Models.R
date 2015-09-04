mdat<-data.frame(read.csv("//Users/TScott/Google Drive/duckabush/management_data_v2.csv",header=TRUE))

for (i in 1:nrow(mdat))
{
	if (mdat$SURVEY[i]=='wsa')
		{pairs<-which(mdat$CON_ID==mdat$CON_ID[i])	
		mdat[pairs[which(pairs!=i)],which(colnames(mdat)=="Impaired"):which(colnames(mdat)=="WATERSHED")]<-mdat[i,which(colnames(mdat)=="Impaired"):which(colnames(mdat)=="WATERSHED")]
		}
}

for (i in 1:ncol(mdat))
{
	mdat[,i]<-tolower(mdat[,i])
}

edat<-(read.csv("//Users/TScott/Google Drive/duckabush/ready_to_model_dat.csv",header=TRUE))
for (i in 1:ncol(edat))
{
	edat[,i]<-tolower(edat[,i])
}


#temp=merge(edat,mdat,by=c("CON_ID","SURVEY","HUC8","FED_OWN","UrbanCat","FW_ECO3","YEAR","FW_ECO9","COUNTY","VISIT_NO",
#"STATE","MISS_SUB","ECOREPORT","ECO3","STRATUM","MAJ_BAS_NM","STRAHLER","EPAREG","ID","VISIT","FSEASTWEST",
#"LOC_NAME","WSAREA","XLAT_DD","XLON_DD"),all.x=TRUE)
temp=merge(edat,mdat,by=intersect(names(edat),names(mdat)),all.x=TRUE)
fdat = temp
library(MCMCpack)
library(lme4)
library(arm)
library(blme)
library(LMERConvenienceFunctions)


#NOTE: FOLLOWING ADVICE OF GELMAN, I STANDARDIZE ALL NUMERIC INPUTS BY DIVIDING EACH VALUE BY 2 STANDARD DEVIATIONS. THIS PLACES EACH NUMERIC INPUT ON A SIMILAR SCALE TO THAT OF THE BINARY (0 OR 1) INPUTS, FOR EASE OF COMPARISON AND INTERPRETATION
fdat$FED_OWN<-relevel(as.factor(fdat$FED_OWN),ref="non federal")
fdat$SURVEY<-relevel(factor(fdat$SURVEY),ref="wsa")
fdat$YEAR<-as.numeric(fdat$YEAR)-2000
fdat$GROUP_FORM<-as.numeric(fdat$GROUP_FORM)-2000
fdat$STRAHLER<-as.numeric(fdat$STRAHLER)
fdat$PTL<-as.numeric(fdat$PTL)
fdat$NTL<-as.numeric(fdat$NTL)

#fill in missing group indicators
for (i in 1:length(fdat$GROUP))
{if (is.na(fdat$GROUP[i])==TRUE){fdat$GROUP[i]<-0}}

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


#IS GROUP ACTIVE AT TIME OF SAMPLING?
#"ACTIVE_GROUP" IF GROUP PRESENT IN YEAR OF SAMPLING
fdat$ACTIVE_GROUP<-ifelse(fdat$YEAR>=fdat$GROUP_FORM,1,0)
fdat$ACTIVE_GROUP<-ifelse(is.na(fdat$ACTIVE_GROUP)==TRUE,0,fdat$ACTIVE_GROUP)

#HOW LONG HAS GROUP BEEN ACTIVE?
#"GROUP_DURATION" is number of years group has been active. GROUP_DURATION=1 IF FOUNDED SAME YEAR AS SAMPLE
temp<-ifelse(((fdat$YEAR+2000)-(fdat$GROUP_FORM+2000)+1)<1,0,((fdat$YEAR+2000)-(fdat$GROUP_FORM+2000)+1))
temp2<-ifelse(is.na(temp)==TRUE,0,temp)
fdat$GROUP_DURATION<-temp2

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


#BASE MODELS
M_PTL_base <- lmerTest::lmer(log(fdat$PTL+.001)~ fdat$ACTIVE+fdat$YEAR+fdat$XWIDTH+fdat$SQ_MILE+fdat$STRAHLER+fdat$PURB+fdat$PWETL+fdat$PFOR+fdat$RDDENS+fdat$POPDENS+fdat$W1_HAG+fdat$W1_HNOAG+(1|fdat$HUC4)+(fdat$ACTIVE|fdat$STATE),control=list(maxIter=1000L),verbose=TRUE)
summary(M_PTL_base)
car::Anova(M_PTL_base)
ranef(M_PTL_base)[2]
write.csv(ranef(M_PTL_base)[2], "//Users/TScott/Google Drive/duckabush/ptl_ranef.csv")

M_NTL_base <- lmer(log(fdat$NTL+0.001)~ fdat$ACTIVE+fdat$YEAR+fdat$XWIDTH+fdat$SQ_MILE+fdat$STRAHLER+fdat$PURB+fdat$PWETL+fdat$PFOR+fdat$RDDENS+fdat$POPDENS+fdat$W1_HAG+fdat$W1_HNOAG+(1|fdat$HUC4)+(fdat$ACTIVE|fdat$STATE),control=list(maxIter=1000L),verbose=TRUE)
summary(M_NTL_base)
car::Anova(M_NTL_base)
ranef(M_NTL_base)
write.csv(ranef(M_NTL_base)[2], "//Users/TScott/Google Drive/duckabush/ntl_ranef.csv")

M_TURB_base <- lmer(log(fdat$TURB+.001)~ fdat$ACTIVE+fdat$YEAR+fdat$XWIDTH+fdat$SQ_MILE+fdat$STRAHLER+fdat$PURB+fdat$PWETL+fdat$PFOR+fdat$RDDENS+fdat$POPDENS+fdat$W1_HAG+fdat$W1_HNOAG+(1|fdat$HUC4)+(fdat$ACTIVE|fdat$STATE),control=list(maxIter=1000L),verbose=TRUE)
summary(M_TURB_base)
car::Anova(M_TURB_base)
ranef(M_TURB_base)
write.csv(ranef(M_TURB_base)[2], "//Users/TScott/Google Drive/duckabush/turb_ranef.csv")

M_XFC_NAT_base <- lmer(log(fdat$XFC_NAT+0.001)~ fdat$ACTIVE+fdat$YEAR+fdat$XWIDTH+fdat$SQ_MILE+fdat$STRAHLER+fdat$PURB+fdat$PWETL+fdat$PFOR+fdat$RDDENS+fdat$POPDENS+fdat$W1_HAG+fdat$W1_HNOAG+(1|fdat$HUC4)+(fdat$ACTIVE|fdat$STATE),control=list(maxIter=1000L),verbose=TRUE)
summary(M_XFC_NAT_base)
car::Anova(M_XFC_NAT_base)
ranef(M_XFC_NAT_base)
write.csv(ranef(M_XFC_NAT_base)[2], "//Users/TScott/Google Drive/duckabush/xfc_ranef.csv")

fdat_MMI_BENT<-subset(fdat,is.na(fdat$MMI_BENT)==FALSE)
M_MMI_BENT_base <- lmer(log(fdat_MMI_BENT$MMI_BENT+0.001)~ fdat_MMI_BENT$ACTIVE+fdat_MMI_BENT$YEAR+fdat_MMI_BENT$XWIDTH+fdat_MMI_BENT$SQ_MILE+fdat_MMI_BENT$STRAHLER+fdat_MMI_BENT$PURB+fdat_MMI_BENT$PWETL+fdat_MMI_BENT$PFOR+fdat_MMI_BENT$RDDENS+fdat_MMI_BENT$POPDENS+fdat_MMI_BENT$W1_HAG+fdat_MMI_BENT$W1_HNOAG+(1|fdat_MMI_BENT$HUC4)+(fdat_MMI_BENT$ACTIVE|fdat_MMI_BENT$STATE),control=list(maxIter=1000L),verbose=TRUE)
summary(M_MMI_BENT_base)
car::Anova(M_MMI_BENT_base)
ranef(M_MMI_BENT_base)
write.csv(ranef(M_MMI_BENT_base)[2], "//Users/TScott/Google Drive/duckabush/mmi_ranef.csv")

fdat_XCMGW<-subset(fdat,is.na(fdat$XCMGW_stz)==FALSE)
M_XCMGW_base <- lmer(log(fdat_XCMGW$XCMGW+0.001)~ fdat_XCMGW$ACTIVE+fdat_XCMGW$YEAR+fdat_XCMGW$XWIDTH+
fdat_XCMGW$SQ_MILE+fdat_XCMGW$STRAHLER+fdat_XCMGW$PURB+fdat_XCMGW$PWETL+fdat_XCMGW$PFOR+fdat_XCMGW$RDDENS+fdat_XCMGW$POPDENS+fdat_XCMGW$W1_HAG+fdat_XCMGW$W1_HNOAG+(1|
fdat_XCMGW$HUC4)+(fdat_XCMGW$ACTIVE|fdat_XCMGW$STATE),data=data.frame(fdat_XCMGW),control=list(maxIter=1000L),verbose=TRUE)
summary(M_XCMGW_base)
fixef(M_XCMGW_base)
car::Anova(M_XCMGW_base)
ranef(M_XCMGW_base)
write.csv(ranef(M_XCMGW_base)[2], "//Users/TScott/Google Drive/duckabush/xcmgw_ranef.csv")

#make csv file that is table of fixed effects from base models
base_fixef<-cbind(fixef(M_PTL_base),fixef(M_NTL_base),fixef(M_TURB_base),fixef(M_MMI_BENT_base),
fixef(M_XCMGW_base),fixef(M_XFC_NAT_base))
colnames(base_fixef)<-c('PTL',"NTL",'TURB','MMI_BENT','XCMGW','XFC_NAT')
write.csv(base_fixef, "//Users/TScott/Google Drive/duckabush/base_fixef.csv")





#UN-RESTRICTED MODELS: REPRESENTATION

M_PTL_rep <- lmerTest::lmer(log(fdat$PTL+.001)~ fdat$ACTIVE+fdat$YEAR+fdat$XWIDTH+fdat$SQ_MILE+fdat$STRAHLER+fdat$PURB+fdat$PWETL+fdat$PFOR+fdat$RDDENS+fdat$POPDENS+fdat$W1_HAG+fdat$W1_HNOAG+fdat$ACTIVE:fdat$CROSS_BOUND:fdat$TRANS_BOUND+fdat$ACTIVE:fdat$MEM_TOTAL+
fdat$TECH_BODY:fdat$ACTIVE+(1|fdat$HUC4)+(fdat$ACTIVE|fdat$STATE),control=list(maxIter=1000L),verbose=TRUE)
summary(M_PTL_rep)
car::Anova(M_PTL_rep)
ranef(M_PTL_rep)[2]
write.csv(ranef(M_PTL_rep)[2], "//Users/TScott/Google Drive/duckabush/ptl_ranef_rep.csv")


M_NTL_rep <- lmer(log(fdat$NTL+0.001)~ fdat$ACTIVE+fdat$YEAR+fdat$XWIDTH+fdat$SQ_MILE+fdat$STRAHLER+fdat$PURB+fdat$PWETL+fdat$PFOR+fdat$RDDENS+fdat$POPDENS+fdat$W1_HAG+fdat$W1_HNOAG+fdat$ACTIVE:fdat$CROSS_BOUND:fdat$TRANS_BOUND+fdat$ACTIVE:fdat$MEM_TOTAL+
fdat$TECH_BODY:fdat$ACTIVE+(1|fdat$HUC4)+(fdat$ACTIVE|fdat$STATE),control=list(maxIter=1000L),verbose=TRUE)
summary(M_NTL_rep)
car::Anova(M_NTL_rep)
ranef(M_NTL_rep)
write.csv(ranef(M_NTL_rep)[2], "//Users/TScott/Google Drive/duckabush/ntl_ranef_rep.csv")

M_TURB_rep <- lmer(log(fdat$TURB+.001)~ fdat$ACTIVE+fdat$YEAR+fdat$XWIDTH+fdat$SQ_MILE+fdat$STRAHLER+fdat$PURB+fdat$PWETL+fdat$PFOR+fdat$RDDENS+fdat$POPDENS+fdat$W1_HAG+fdat$W1_HNOAG++fdat$ACTIVE:fdat$CROSS_BOUND:fdat$TRANS_BOUND+fdat$ACTIVE:fdat$MEM_TOTAL+
fdat$TECH_BODY:fdat$ACTIVE+(1|fdat$HUC4)+(fdat$ACTIVE|fdat$STATE),control=list(maxIter=1000L),verbose=TRUE)
summary(M_TURB_rep)
car::Anova(M_TURB_rep)
ranef(M_TURB_rep)
write.csv(ranef(M_TURB_rep)[2], "//Users/TScott/Google Drive/duckabush/turb_ranef_rep.csv")

M_XFC_NAT_rep <- lmer(log(fdat$XFC_NAT+0.001)~ fdat$ACTIVE+fdat$YEAR+fdat$XWIDTH+fdat$SQ_MILE+fdat$STRAHLER+fdat$PURB+fdat$PWETL+fdat$PFOR+fdat$RDDENS+fdat$POPDENS+fdat$W1_HAG+fdat$W1_HNOAG+fdat$ACTIVE:fdat$CROSS_BOUND:fdat$TRANS_BOUND+fdat$ACTIVE:fdat$MEM_TOTAL+
fdat$TECH_BODY:fdat$ACTIVE+(1|fdat$HUC4)+(fdat$ACTIVE|fdat$STATE),control=list(maxIter=1000L),verbose=TRUE)
summary(M_XFC_NAT_rep)
car::Anova(M_XFC_NAT_rep)
ranef(M_XFC_NAT_rep)
write.csv(ranef(M_XFC_NAT_rep)[2], "//Users/TScott/Google Drive/duckabush/xfc_ranef_rep.csv")

fdat_MMI_BENT<-subset(fdat,is.na(fdat$MMI_BENT)==FALSE)
M_MMI_BENT_rep <- lmer(log(fdat_MMI_BENT$MMI_BENT+0.001)~ fdat_MMI_BENT$ACTIVE+fdat_MMI_BENT$YEAR+fdat_MMI_BENT$XWIDTH+fdat_MMI_BENT$SQ_MILE+fdat_MMI_BENT$STRAHLER+fdat_MMI_BENT$PURB+fdat_MMI_BENT$PWETL+fdat_MMI_BENT$PFOR+fdat_MMI_BENT$RDDENS+fdat_MMI_BENT$POPDENS+fdat_MMI_BENT$W1_HAG+fdat_MMI_BENT$W1_HNOAG+fdat_MMI_BENT$ACTIVE:fdat_MMI_BENT$CROSS_BOUND:fdat_MMI_BENT$TRANS_BOUND+fdat_MMI_BENT$ACTIVE:fdat_MMI_BENT$MEM_TOTAL+
fdat_MMI_BENT$TECH_BODY:fdat_MMI_BENT$ACTIVE+(1|fdat_MMI_BENT$HUC4)+(fdat_MMI_BENT$ACTIVE|fdat_MMI_BENT$STATE),control=list(maxIter=1000L),verbose=TRUE)
summary(M_MMI_BENT_rep)
car::Anova(M_MMI_BENT_rep)
ranef(M_MMI_BENT_rep)
write.csv(ranef(M_MMI_BENT_rep)[2], "//Users/TScott/Google Drive/duckabush/mmi_ranef_rep.csv")

fdat_XCMGW<-subset(fdat,is.na(fdat$XCMGW_stz)==FALSE)
M_XCMGW_rep <- lmer(log(fdat_XCMGW$XCMGW+0.001)~ fdat_XCMGW$ACTIVE+fdat_XCMGW$YEAR+fdat_XCMGW$XWIDTH+
fdat_XCMGW$SQ_MILE+fdat_XCMGW$STRAHLER+fdat_XCMGW$PURB+fdat_XCMGW$PWETL+fdat_XCMGW$PFOR+fdat_XCMGW$RDDENS+fdat_XCMGW$POPDENS+fdat_XCMGW$W1_HAG+fdat_XCMGW$W1_HNOAG+fdat_XCMGW$ACTIVE:fdat_XCMGW$CROSS_BOUND:fdat_XCMGW$TRANS_BOUND+fdat_XCMGW$ACTIVE:fdat_XCMGW$MEM_TOTAL+
fdat_XCMGW$TECH_BODY:fdat_XCMGW$ACTIVE+(1|
fdat_XCMGW$HUC4)+(fdat_XCMGW$ACTIVE|fdat_XCMGW$STATE),data=data.frame(fdat_XCMGW),control=list(maxIter=1000L),verbose=TRUE)
summary(M_XCMGW_rep)
car::Anova(M_XCMGW_rep)
ranef(M_XCMGW_rep)
write.csv(ranef(M_XCMGW_rep)[2], "//Users/TScott/Google Drive/duckabush/xcmgw_ranef_rep.csv")

rep_fixef<-cbind(fixef(M_PTL_rep),fixef(M_NTL_rep),fixef(M_TURB_rep),fixef(M_MMI_BENT_rep),
fixef(M_XCMGW_rep),fixef(M_XFC_NAT_rep))
colnames(rep_fixef)<-c('PTL',"NTL",'TURB','MMI_BENT','XCMGW','XFC_NAT')
write.csv(rep_fixef, "//Users/TScott/Google Drive/duckabush/rep_fixef.csv")



#UN-RESTRICTED MODELS: GROUP RESPONSIBILITY
M_PTL_res <- lmerTest::lmer(log(fdat$PTL+.001)~ fdat$ACTIVE+fdat$YEAR+fdat$XWIDTH+fdat$SQ_MILE+fdat$STRAHLER+fdat$PURB+fdat$PWETL+fdat$PFOR+fdat$RDDENS+fdat$POPDENS+fdat$W1_HAG+fdat$W1_HNOAG+
fdat$ACTIVE:as.factor(fdat$GROUP_RESP_null)+
(1|fdat$HUC4)+(fdat$ACTIVE|fdat$STATE),control=list(maxIter=1000L),verbose=TRUE)
summary(M_PTL_res)
car::Anova(M_PTL_res)
ranef(M_PTL_res)[2]
write.csv(ranef(M_PTL_res)[2], "//Users/TScott/Google Drive/duckabush/ptl_ranef_res.csv")

M_NTL_res <- lmer(log(fdat$NTL+0.001)~ fdat$ACTIVE+fdat$YEAR+fdat$XWIDTH+fdat$SQ_MILE+fdat$STRAHLER+fdat$PURB+fdat$PWETL+fdat$PFOR+fdat$RDDENS+fdat$POPDENS+fdat$W1_HAG+fdat$W1_HNOAG+
fdat$ACTIVE:as.factor(fdat$GROUP_RESP_null)+
(1|fdat$HUC4)+(fdat$ACTIVE|fdat$STATE),control=list(maxIter=1000L),verbose=TRUE)
summary(M_NTL_res)
car::Anova(M_NTL_res)
ranef(M_NTL_res)
write.csv(ranef(M_NTL_res)[2], "//Users/TScott/Google Drive/duckabush/ntl_ranef_res.csv")

M_TURB_res <- lmer(log(fdat$TURB+.001)~ fdat$ACTIVE+fdat$YEAR+fdat$XWIDTH+fdat$SQ_MILE+fdat$STRAHLER+fdat$PURB+fdat$PWETL+fdat$PFOR+fdat$RDDENS+fdat$POPDENS+fdat$W1_HAG+fdat$W1_HNOAG+
fdat$ACTIVE:as.factor(fdat$GROUP_RESP_null)+
(1|fdat$HUC4)+(fdat$ACTIVE|fdat$STATE),control=list(maxIter=1000L),verbose=TRUE)
summary(M_TURB_res)
car::Anova(M_TURB_res)
ranef(M_TURB_res)
write.csv(ranef(M_TURB_res)[2], "//Users/TScott/Google Drive/duckabush/turb_ranef_res.csv")

M_XFC_NAT_res <- lmer(log(fdat$XFC_NAT+0.001)~ fdat$ACTIVE+fdat$YEAR+fdat$XWIDTH+fdat$SQ_MILE+fdat$STRAHLER+fdat$PURB+fdat$PWETL+fdat$PFOR+fdat$RDDENS+fdat$POPDENS+fdat$W1_HAG+fdat$W1_HNOAG+
fdat$ACTIVE:as.factor(fdat$GROUP_RESP_null)+
(1|fdat$HUC4)+(fdat$ACTIVE|fdat$STATE),control=list(maxIter=1000L),verbose=TRUE)
summary(M_XFC_NAT_res)
car::Anova(M_XFC_NAT_res)
ranef(M_XFC_NAT_res)
write.csv(ranef(M_XFC_NAT_res)[2], "//Users/TScott/Google Drive/duckabush/xfc_ranef_res.csv")

fdat_MMI_BENT<-subset(fdat,is.na(fdat$MMI_BENT)==FALSE)
M_MMI_BENT_res <- lmer(log(fdat_MMI_BENT$MMI_BENT+0.001)~ fdat_MMI_BENT$ACTIVE+fdat_MMI_BENT$YEAR+fdat_MMI_BENT$XWIDTH+fdat_MMI_BENT$SQ_MILE+fdat_MMI_BENT$STRAHLER+fdat_MMI_BENT$PURB+fdat_MMI_BENT$PWETL+fdat_MMI_BENT$PFOR+fdat_MMI_BENT$RDDENS+fdat_MMI_BENT$POPDENS+fdat_MMI_BENT$W1_HAG+fdat_MMI_BENT$W1_HNOAG+
fdat_MMI_BENT$ACTIVE:as.factor(fdat_MMI_BENT$GROUP_RESP_null)+
(1|fdat_MMI_BENT$HUC4)+(fdat_MMI_BENT$ACTIVE|fdat_MMI_BENT$STATE),control=list(maxIter=1000L),verbose=TRUE)
summary(M_MMI_BENT_res)
car::Anova(M_MMI_BENT_res)
ranef(M_MMI_BENT_res)
write.csv(ranef(M_MMI_BENT_res)[2], "//Users/TScott/Google Drive/duckabush/mmi_ranef_res.csv")

fdat_XCMGW<-subset(fdat,is.na(fdat$XCMGW_stz)==FALSE)
M_XCMGW_res <- lmer(log(fdat_XCMGW$XCMGW+0.001)~ fdat_XCMGW$ACTIVE+fdat_XCMGW$YEAR+fdat_XCMGW$XWIDTH+
fdat_XCMGW$SQ_MILE+fdat_XCMGW$STRAHLER+fdat_XCMGW$PURB+fdat_XCMGW$PWETL+fdat_XCMGW$PFOR+fdat_XCMGW$RDDENS+fdat_XCMGW$POPDENS+fdat_XCMGW$W1_HAG+fdat_XCMGW$W1_HNOAG+
fdat_XCMGW$ACTIVE:as.factor(fdat_XCMGW$GROUP_RESP_null)+
(1|fdat_XCMGW$HUC4)+(fdat_XCMGW$ACTIVE|fdat_XCMGW$STATE),data=data.frame(fdat_XCMGW),control=list(maxIter=1000L),verbose=TRUE)
summary(M_XCMGW_res)
car::Anova(M_XCMGW_res)
ranef(M_XCMGW_res)
write.csv(ranef(M_XCMGW_res)[2], "//Users/TScott/Google Drive/duckabush/xcmgw_ranef_res.csv")


#make csv file that is table of fixed effects from unrestricted models
res_fixef<-cbind(fixef(M_PTL_res),fixef(M_NTL_res),fixef(M_TURB_res),fixef(M_MMI_BENT_res),
fixef(M_XCMGW_res),fixef(M_XFC_NAT_res))
colnames(res_fixef)<-c('PTL',"NTL",'TURB','MMI_BENT','XCMGW','XFC_NAT')
write.csv(res_fixef, "//Users/TScott/Google Drive/duckabush/res_fixef.csv")



fdat$obj_trans<-ifelse(fdat$FORM_OBJECT=="0","MISSION",fdat$FORM_OBJECT)
fdat$obj_trans<-relevel(as.factor(fdat$obj_trans),ref="MISSION")
fdat$obj_trans

#UN-RESTRICTED MODELS: FORMALIZATION
M_PTL_form <- lmerTest::lmer(log(fdat$PTL+.001)~ fdat$ACTIVE+fdat$YEAR+fdat$XWIDTH+fdat$SQ_MILE+fdat$STRAHLER+fdat$PURB+fdat$PWETL+fdat$PFOR+fdat$RDDENS+fdat$POPDENS+fdat$W1_HAG+fdat$W1_HNOAG+
fdat$ACTIVE:fdat$FACILITATE+
fdat$ACTIVE:fdat$obj_trans+
fdat$ACTIVE:fdat$HAS_BYLAWS+
(1|fdat$HUC4)+(fdat$ACTIVE|fdat$STATE),control=list(maxIter=1000L),verbose=TRUE)
summary(M_PTL_form)
car::Anova(M_PTL_form)
ranef(M_PTL_form)[2]
write.csv(ranef(M_PTL_form)[2], "//Users/TScott/Google Drive/duckabush/ptl_ranef_form.csv")


M_NTL_form <- lmer(log(fdat$NTL+0.001)~ fdat$ACTIVE+fdat$YEAR+fdat$XWIDTH+fdat$SQ_MILE+fdat$STRAHLER+fdat$PURB+fdat$PWETL+fdat$PFOR+fdat$RDDENS+fdat$POPDENS+fdat$W1_HAG+fdat$W1_HNOAG+
fdat$ACTIVE:fdat$FACILITATE+
fdat$ACTIVE:fdat$obj_trans+
fdat$ACTIVE:fdat$HAS_BYLAWS+(1|fdat$HUC4)+(fdat$ACTIVE|fdat$STATE),control=list(maxIter=1000L),verbose=TRUE)
summary(M_NTL_form)
car::Anova(M_NTL_form)
ranef(M_NTL_form)
write.csv(ranef(M_NTL_form)[2], "//Users/TScott/Google Drive/duckabush/ntl_ranef_form.csv")

M_TURB_form <- lmer(log(fdat$TURB+.001)~ fdat$ACTIVE+fdat$YEAR+fdat$XWIDTH+fdat$SQ_MILE+fdat$STRAHLER+fdat$PURB+fdat$PWETL+fdat$PFOR+fdat$RDDENS+fdat$POPDENS+fdat$W1_HAG+fdat$W1_HNOAG+fdat$ACTIVE:fdat$FACILITATE+
fdat$ACTIVE:fdat$obj_trans+
fdat$ACTIVE:fdat$HAS_BYLAWS+(1|fdat$HUC4)+(fdat$ACTIVE|fdat$STATE),control=list(maxIter=1000L),verbose=TRUE)
summary(M_TURB_form)
car::Anova(M_TURB_form)
ranef(M_TURB_form)
write.csv(ranef(M_TURB_form)[2], "//Users/TScott/Google Drive/duckabush/turb_ranef_form.csv")

M_XFC_NAT_form <- lmer(log(fdat$XFC_NAT+0.001)~ fdat$ACTIVE+fdat$YEAR+fdat$XWIDTH+fdat$SQ_MILE+fdat$STRAHLER+fdat$PURB+fdat$PWETL+fdat$PFOR+fdat$RDDENS+fdat$POPDENS+fdat$W1_HAG+fdat$W1_HNOAG+fdat$ACTIVE:fdat$FACILITATE+
fdat$ACTIVE:fdat$obj_trans+
fdat$ACTIVE:fdat$HAS_BYLAWS+(1|fdat$HUC4)+(fdat$ACTIVE|fdat$STATE),control=list(maxIter=1000L),verbose=TRUE)
summary(M_XFC_NAT_form)
car::Anova(M_XFC_NAT_form)
ranef(M_XFC_NAT_form)
write.csv(ranef(M_XFC_NAT_form)[2], "//Users/TScott/Google Drive/duckabush/xfc_ranef_form.csv")

fdat_MMI_BENT<-subset(fdat,is.na(fdat$MMI_BENT)==FALSE)
M_MMI_BENT_form <- lmer(log(fdat_MMI_BENT$MMI_BENT+0.001)~ fdat_MMI_BENT$ACTIVE+fdat_MMI_BENT$YEAR+fdat_MMI_BENT$XWIDTH+fdat_MMI_BENT$SQ_MILE+fdat_MMI_BENT$STRAHLER+fdat_MMI_BENT$PURB+fdat_MMI_BENT$PWETL+fdat_MMI_BENT$PFOR+fdat_MMI_BENT$RDDENS+fdat_MMI_BENT$POPDENS+fdat_MMI_BENT$W1_HAG+fdat_MMI_BENT$W1_HNOAG+
fdat_MMI_BENT$ACTIVE:fdat_MMI_BENT$FACILITATE+
fdat_MMI_BENT$ACTIVE:fdat_MMI_BENT$obj_trans+
fdat_MMI_BENT$ACTIVE:fdat_MMI_BENT$HAS_BYLAWS+
(1|fdat_MMI_BENT$HUC4)+(fdat_MMI_BENT$ACTIVE|fdat_MMI_BENT$STATE),control=list(maxIter=1000L),verbose=TRUE)
summary(M_MMI_BENT_form)
car::Anova(M_MMI_BENT_form)
ranef(M_MMI_BENT_form)
write.csv(ranef(M_MMI_BENT_form)[2], "//Users/TScott/Google Drive/duckabush/mmi_ranef_form.csv")

fdat_XCMGW<-subset(fdat,is.na(fdat$XCMGW_stz)==FALSE)
M_XCMGW_form <- lmer(log(fdat_XCMGW$XCMGW+0.001)~ fdat_XCMGW$ACTIVE+fdat_XCMGW$YEAR+fdat_XCMGW$XWIDTH+
fdat_XCMGW$SQ_MILE+fdat_XCMGW$STRAHLER+fdat_XCMGW$PURB+fdat_XCMGW$PWETL+fdat_XCMGW$PFOR+fdat_XCMGW$RDDENS+fdat_XCMGW$POPDENS+fdat_XCMGW$W1_HAG+fdat_XCMGW$W1_HNOAG+
fdat_XCMGW$ACTIVE:fdat_XCMGW$FACILITATE+
fdat_XCMGW$ACTIVE:fdat_XCMGW$obj_trans+
fdat_XCMGW$ACTIVE:fdat_XCMGW$HAS_BYLAWS+
(1|fdat_XCMGW$HUC4)+(fdat_XCMGW$ACTIVE|fdat_XCMGW$STATE),data=data.frame(fdat_XCMGW),control=list(maxIter=1000L),verbose=TRUE)
summary(M_XCMGW_form)
car::Anova(M_XCMGW_form)
ranef(M_XCMGW_form)
write.csv(ranef(M_XCMGW_form)[2], "//Users/TScott/Google Drive/duckabush/xcmgw_ranef_form.csv")


#make csv file that is table of fixed effects from unrestricted models
form_fixef<-cbind(fixef(M_PTL_form),fixef(M_NTL_form),fixef(M_TURB_form),fixef(M_MMI_BENT_form),
fixef(M_XCMGW_form),fixef(M_XFC_NAT_form))
colnames(form_fixef)<-c('PTL',"NTL",'TURB','MMI_BENT','XCMGW','XFC_NAT')
write.csv(form_fixef, "//Users/TScott/Google Drive/duckabush/form_fixef.csv")



#UN-RESTRICTED MODELS: FUNDING
M_PTL_fun <- lmerTest::lmer(log(fdat$PTL+.001)~ fdat$ACTIVE+fdat$YEAR+fdat$XWIDTH+fdat$SQ_MILE+fdat$STRAHLER+fdat$PURB+fdat$PWETL+fdat$PFOR+fdat$RDDENS+fdat$POPDENS+fdat$W1_HAG+fdat$W1_HNOAG+
fdat$ACTIVE:fdat$FUND_LOCAL+
fdat$ACTIVE:fdat$FUND_STATE+
fdat$ACTIVE:fdat$FUND_FED+
(1|fdat$HUC4)+(fdat$ACTIVE|fdat$STATE),control=list(maxIter=1000L),verbose=TRUE)
summary(M_PTL_fun)
car::Anova(M_PTL_fun)
ranef(M_PTL_fun)[2]
write.csv(ranef(M_NTL_fun)[2], "//Users/TScott/Google Drive/duckabush/ptl_ranef_fun.csv")



M_NTL_fun <- lmer(log(fdat$NTL+0.001)~ fdat$ACTIVE+fdat$YEAR+fdat$XWIDTH+fdat$SQ_MILE+fdat$STRAHLER+fdat$PURB+fdat$PWETL+fdat$PFOR+fdat$RDDENS+fdat$POPDENS+fdat$W1_HAG+fdat$W1_HNOAG++fdat$ACTIVE:fdat$FUND_LOCAL+fdat$ACTIVE:fdat$FUND_STATE+fdat$ACTIVE:fdat$FUND_FED+
(1|fdat$HUC4)+(fdat$ACTIVE|fdat$STATE),control=list(maxIter=1000L),verbose=TRUE)
summary(M_NTL_fun)
car::Anova(M_NTL_fun)
ranef(M_NTL_fun)
write.csv(ranef(M_NTL_fun)[2], "//Users/TScott/Google Drive/duckabush/ntl_ranef_fun.csv")

M_TURB_fun <- lmer(log(fdat$TURB+.001)~ fdat$ACTIVE+fdat$YEAR+fdat$XWIDTH+fdat$SQ_MILE+fdat$STRAHLER+fdat$PURB+fdat$PWETL+fdat$PFOR+fdat$RDDENS+fdat$POPDENS+fdat$W1_HAG+fdat$W1_HNOAG+fdat$ACTIVE:fdat$FUND_LOCAL+fdat$ACTIVE:fdat$FUND_STATE+fdat$ACTIVE:fdat$FUND_FED+
(1|fdat$HUC4)+(fdat$ACTIVE|fdat$STATE),control=list(maxIter=1000L),verbose=TRUE)
summary(M_TURB_fun)
car::Anova(M_TURB_fun)
ranef(M_TURB_fun)
write.csv(ranef(M_TURB_fun)[2], "//Users/TScott/Google Drive/duckabush/turb_ranef_fun.csv")

M_XFC_NAT_fun <- lmer(log(fdat$XFC_NAT+0.001)~ fdat$ACTIVE+fdat$YEAR+fdat$XWIDTH+fdat$SQ_MILE+fdat$STRAHLER+fdat$PURB+fdat$PWETL+fdat$PFOR+fdat$RDDENS+fdat$POPDENS+fdat$W1_HAG+fdat$W1_HNOAG+fdat$ACTIVE:fdat$FUND_LOCAL+fdat$ACTIVE:fdat$FUND_STATE+fdat$ACTIVE:fdat$FUND_FED+
(1|fdat$HUC4)+(fdat$ACTIVE|fdat$STATE),control=list(maxIter=1000L),verbose=TRUE)
summary(M_XFC_NAT_fun)
car::Anova(M_XFC_NAT_fun)
ranef(M_XFC_NAT_fun)
write.csv(ranef(M_XFC_NAT_fun)[2], "//Users/TScott/Google Drive/duckabush/xfc_ranef_fun.csv")

fdat_MMI_BENT<-subset(fdat,is.na(fdat$MMI_BENT)==FALSE)
M_MMI_BENT_fun <- lmer(log(fdat_MMI_BENT$MMI_BENT+0.001)~ fdat_MMI_BENT$ACTIVE+fdat_MMI_BENT$YEAR+fdat_MMI_BENT$XWIDTH+fdat_MMI_BENT$SQ_MILE+fdat_MMI_BENT$STRAHLER+fdat_MMI_BENT$PURB+fdat_MMI_BENT$PWETL+fdat_MMI_BENT$PFOR+fdat_MMI_BENT$RDDENS+fdat_MMI_BENT$POPDENS+fdat_MMI_BENT$W1_HAG+fdat_MMI_BENT$W1_HNOAG+fdat_MMI_BENT$ACTIVE:fdat_MMI_BENT$FUND_LOCAL+fdat_MMI_BENT$ACTIVE:fdat_MMI_BENT$FUND_STATE+fdat_MMI_BENT$ACTIVE:fdat_MMI_BENT$FUND_FED+
(1|fdat_MMI_BENT$HUC4)+(fdat_MMI_BENT$ACTIVE|fdat_MMI_BENT$STATE),control=list(maxIter=1000L),verbose=TRUE)
summary(M_MMI_BENT_fun)
car::Anova(M_MMI_BENT_fun)
ranef(M_MMI_BENT_fun)
write.csv(ranef(M_MMI_BENT_fun)[2], "//Users/TScott/Google Drive/duckabush/mmi_ranef_fun.csv")

fdat_XCMGW<-subset(fdat,is.na(fdat$XCMGW_stz)==FALSE)
M_XCMGW_fun <- lmer(log(fdat_XCMGW$XCMGW+0.001)~ fdat_XCMGW$ACTIVE+fdat_XCMGW$YEAR+fdat_XCMGW$XWIDTH+
fdat_XCMGW$SQ_MILE+fdat_XCMGW$STRAHLER+fdat_XCMGW$PURB+fdat_XCMGW$PWETL+fdat_XCMGW$PFOR+fdat_XCMGW$RDDENS+fdat_XCMGW$POPDENS+fdat_XCMGW$W1_HAG+fdat_XCMGW$W1_HNOAG+fdat_XCMGW$ACTIVE:fdat_XCMGW$FUND_LOCAL+fdat_XCMGW$ACTIVE:fdat_XCMGW$FUND_STATE+fdat_XCMGW$ACTIVE:fdat_XCMGW$FUND_FED+
(1|fdat_XCMGW$HUC4)+(fdat_XCMGW$ACTIVE|fdat_XCMGW$STATE),data=data.frame(fdat_XCMGW),control=list(maxIter=1000L),verbose=TRUE)
summary(M_XCMGW_fun)
car::Anova(M_XCMGW_fun)
ranef(M_XCMGW_fun)
write.csv(ranef(M_XCMGW_fun)[2], "//Users/TScott/Google Drive/duckabush/xcmgw_ranef_fun.csv")


#make csv file that is table of fixed effects from unrestricted models
fun_fixef<-cbind(fixef(M_PTL_fun),fixef(M_NTL_fun),fixef(M_TURB_fun),fixef(M_MMI_BENT_fun),
fixef(M_XCMGW_fun),fixef(M_XFC_NAT_fun))
colnames(fun_fixef)<-c('PTL',"NTL",'TURB','MMI_BENT','XCMGW','XFC_NAT')
write.csv(fun_fixef, "//Users/TScott/Google Drive/duckabush/fun_fixef.csv")




