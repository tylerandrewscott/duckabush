rm(list=ls())

library(stringr)
library(lme4)
library(LMERConvenienceFunctions)
#library(ggmap)
#library(DeducerSpatial)
#library(UScensus2010)
library(ggplot2)
library(rgdal) # librarys sp, will use proj.4 if installed
library(acs)
library(plyr)
library(XML)
library(doParallel)
library(raster)
library(maptools)
library(proj4)
library(foreign)

api.key.install('4dcea91b372b1e5e195117d49267de8dbb70871c')
#install.packages('p')
#install.packages('rgdal',type='source')
#require(pbkrtest)
#library(arm)
#library(blme)
#install.packages('pbkrtest')
#require(ggplot2)
#require(plyr)
#require(Hmisc)



mdat<-data.frame(read.csv("H:/duckabush/management_data_v2.csv",header=TRUE))

edat<-(read.csv("H:/duckabush/ready_to_model_dat.csv",header=TRUE))
si<-read.csv('H:/duckabush/NRSA_site_info.csv')

mdat$XLAT_DD<-si$XLAT_DD[match(mdat$CON_ID,si$MASTER_SITEID)]
mdat$XLON_DD<-si$XLON_DD[match(mdat$CON_ID,si$MASTER_SITEID)]
edat$XLAT_DD<-si$XLAT_DD[match(mdat$CON_ID,si$MASTER_SITEID)]
edat$XLON_DD<-si$XLON_DD[match(mdat$CON_ID,si$MASTER_SITEID)]


t1<-(mdat[mdat$SURVEY=='nrsa',])
t2<-(mdat[mdat$SURVEY=='wsa',])
t1<-t1[order(t1$CON_ID),]
t2<-t2[order(t2$CON_ID),]
t1[,1:47]<-t2[,1:47]

ttdat<-rbind(t1,t2)
ttdat$GROUP<-as.numeric(ttdat$GROUP)

edat2<-edat[,names(edat) %in% names(ttdat)==FALSE|names(edat) %in% 
              c('CON_ID','COUNTY','VISIT','YEAR')]


temp<-join(ttdat,edat2)
fdat = temp
huc4 = readOGR(dsn='.',layer="WBDHU4")
huc8 = readOGR(dsn='.',layer="WBDHU8")
coordinates(fdat)<-c('XLON_DD','XLAT_DD')
proj4string(fdat)<-CRS('+proj=longlat +datum=NAD83')
temp<-fdat
temp<-spTransform(temp,CRS(proj4string(huc4)))
inhuc4<-over(temp,huc4)
fdat$HUC4<-inhuc4$HUC4
fdat$HUC4name<-inhuc4$Name
inhuc8<-over(temp,huc8)
fdat$HUC8<-inhuc8$HUC8
fdat$HUC8name<-inhuc8$Name
fdat$HUC8AreaSqKm<-inhuc8$AreaSqKm
fdat$HUC4AreaSqKm<-inhuc4$AreaSqKm

fdat$FED_OWN<-relevel(as.factor(fdat$FED_OWN),ref="Non Federal")
fdat$SURVEY<-relevel(factor(fdat$SURVEY),ref="wsa")
fdat$YEAR<-as.numeric(fdat$YEAR)
fdat$GROUP_FORM<-as.numeric(fdat$GROUP_FORM)

fdat$YEAR<-as.numeric(fdat$YEAR)

temp<-fdat$YEAR - fdat$GROUP_FORM + 1
temp<-ifelse(is.na(temp),0,temp)
temp<-ifelse(temp<0,0,temp)

fdat$GROUP_DURATION <- temp

#fdat$GROUP_FORM<-ifelse(is.na(fdat$GROUP_FORM)==TRUE,0,fdat$GROUP_FORM)

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

#MAKE INDICATOR VARIABLES FOR GROUP ACTIVITY/RESPONSIBILITY

temp<-rep(0,length(fdat$CON_ID))
fdat$ENGAGED_IN<-tolower(fdat$ENGAGED_IN)
temp[grep('coordin',fdat$ENGAGED_IN)]<-1
temp[grep('outreach',fdat$ENGAGED_IN)]<-1
temp[grep('education',fdat$ENGAGED_IN)]<-1

temp[grep('plan',fdat$ENGAGED_IN)]<-2
temp[grep('monitor',fdat$ENGAGED_IN)]<-2
temp[grep('advis',fdat$ENGAGED_IN)]<-2

temp[grep('project',fdat$ENGAGED_IN)]<-3
temp[grep('manag',fdat$ENGAGED_IN)]<-3
fdat$GROUP_RESPONSIBILITY<-temp


#TRANSFORM FUNDING VARIABLE
#FUND_STATE<-rep(0,nrow(fdat))
#FUND_FED<-rep(0,nrow(fdat))
#FUND_LOCAL<-rep(0,nrow(fdat))
#for (i in 1:nrow(fdat))
#{if (length(grep("local",fdat$FUNDING[i]))>0){FUND_LOCAL[i]=1}
# if (length(grep("federal",fdat$FUNDING[i]))>0){FUND_FED[i]=1}
# if (length(grep("state",fdat$FUNDING[i]))>0){FUND_STATE[i]=1}}
#fdat$FUND_STATE<-FUND_STATE
#fdat$FUND_LOCAL<-FUND_LOCAL
#fdat$FUND_FED<-FUND_FED

#TRANSFORM GOAL-FORMALIZATION VARIABLE
FORM_OBJECT<-rep(0,nrow(fdat))
fdat$OBJECTIVE_SETTING<-tolower(fdat$OBJECTIVE_SETTING)
FORM_OBJECT[grep('mission',fdat$OBJECTIVE_SETTING)]<-'MISSION'
FORM_OBJECT[grep('goal',fdat$OBJECTIVE_SETTING)]<-'GOALS'
FORM_OBJECT[grep('object',fdat$OBJECTIVE_SETTING)]<-'OBJECTIVES'
fdat$FORM_OBJECT<-FORM_OBJECT
fdat$FORM_OBJECT[fdat$GROUP==1&fdat$FORM_OBJECT=='0']<-'MISSION'

#CLEAN UP COORDINATOR VARIABLE, NAME NEW VARIABLE 'FACILITATE'
FACILITATE<-rep(0,nrow(fdat))
fdat$COORDINATOR<-tolower(fdat$COORDINATOR)
FACILITATE[grep('yes',fdat$COORDINATOR)]<-1
fdat$FACILITATE<-FACILITATE

#CLEAN UP INTER-BOUNDARY VARIABLES
fdat$INTERSTATE<-ifelse(is.na(fdat$INTERSTATE),0,fdat$INTERSTATE)
fdat$INTERNATION<-ifelse(is.na(fdat$INTERNATION) ,0,fdat$INTERNATION)

fdat$OTHERSTATES<-ifelse(is.na(fdat$OTHERSTATES)==TRUE,0,ifelse(fdat$OTHERSTATES==1,1,0))
fdat$OTHERNAT<-ifelse(is.na(fdat$OTHERNAT)==TRUE,0,ifelse(fdat$OTHERNAT==1,1,0))
fdat$TRANS_BOUND<-ifelse(fdat$INTERSTATE==1|fdat$INTERNATION==1,1,0)
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


fdat$GOVERNANCE_BODY<-ifelse(fdat$ACTIVED>0,fdat$GOVERNANCE_BODY,'')


#set all huc8 values to 8 digits (add leading 0)
fdat$HUC6<-substr(fdat$HUC8,1,6)
fdat$XWIDTH<-as.numeric(fdat$XWIDTH)
fdat$STRAHLER<-as.numeric(fdat$STRAHLER)
fdat$W1_HAG<-as.numeric(fdat$W1_HAG)
fdat$W1_HNOAG<-as.numeric(fdat$W1_HNOAG)
fdat$FORM_OBJECT_num<-ifelse(fdat$FORM_OBJECT=="0",0,ifelse(fdat$FORM_OBJECT=="MISSION",1,ifelse(fdat$FORM_OBJECT=="GOALS",2,3)))
fdat$COORDINATOR<-ifelse(fdat$COORDINATOR=='yes',1,0)
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


fdat$ACTIVE_bin<-ifelse(fdat$ACTIVED>0,1,0)
temp<-stack(tapply(fdat$ACTIVED,fdat$HUC4,mean));colnames(temp)<-c('meantreat','HUC4')
temp1<-stack(tapply(fdat$ACTIVE_bin,fdat$HUC4,mean));colnames(temp1)<-c('meantreatbin','HUC4')
tempmerge<-merge(temp,temp1,by='HUC4')
fdat<-merge(fdat,tempmerge)

fdat$COUNTY<-ifelse(fdat$COUNTY=='Bedford','Bedford County',as.character(fdat$COUNTY))
fdat$COUNTY<-ifelse(fdat$COUNTY=='Dekalb','DeKalb County',as.character(fdat$COUNTY))
fdat$COUNTY<-ifelse(fdat$COUNTY=='Mchenry','McHenry County',as.character(fdat$COUNTY))
fdat$COUNTY<-ifelse(fdat$COUNTY=='Lewis And Clark','Lewis and Clark County',as.character(fdat$COUNTY))
fdat$COUNTY<-ifelse(fdat$COUNTY=='Clay','Clay County',as.character(fdat$COUNTY))
fdat$COUNTY<-ifelse(fdat$COUNTY=='Mclennan','McLennan County',as.character(fdat$COUNTY))

geo.place<-foreach(i = 1:nrow(fdat)) %do% geo.make(state=as.character(fdat$STATE[i]),
                                                   county=as.character(fdat$COUNTY[i]))

county.med.income<-llply(.data=geo.place,.progress='text',
                         .fun=acs.fetch,endyear=2011,span=5,table.number='B19301')

#All workers >16
county.tot.workers<-llply(.data=geo.place,.progress='text',col.names='pretty',
                          .fun=acs.fetch,endyear=2011,span=5,
                          table.number='B99241',variable='B99241_001')
#Agriculture, Forestry, Fishing And Hunting, And Mining
county.nat.workers<-llply(.data=geo.place,.progress='text',col.names='pretty',
                          .fun=acs.fetch,endyear=2011,span=5,
                          table.number='C24050',variable='C24050_002')

setwd('H:/duckabush/elpo08p020_nt00335')
elec = readOGR(dsn='H:/duckabush/elpo08p020_nt00335',
               layer="elpo08p020")
elec@data$id = rownames(elec@data)
elec.points = ggplot2::fortify(elec, region="id")
elec.df = join(elec.points, elec@data, by="id")
elec.sub.df<-elec.df[elec.df$STATE!='HI'&elec.df$STATE!='AK',]
elec.sub.df$COUNTY_STATE<-paste(tolower(elec.sub.df$COUNTY),elec.sub.df$STATE,sep=', ')
elec.sub.df$COUNTY_STATE<-(gsub(' COUNTY','',elec.sub.df$COUNTY_STATE))
elec.sub.df$COUNTY_STATE<-(gsub(' parish','',elec.sub.df$COUNTY_STATE))
elec.sub.df<-elec.sub.df[duplicated(elec.sub.df$COUNTY_STATE)==FALSE,]
elec.sub.df$COUNTY_STATE<-gsub(' county', '',elec.sub.df$COUNTY_STATE)
fdat$COUNTY_STATE<-paste(tolower(fdat$COUNTY),fdat$STATE,sep=', ')
fdat$COUNTY_STATE<-gsub('st. ','saint ',fdat$COUNTY_STATE)
fdat$COUNTY_STATE<-gsub('foresaint','forest,',fdat$COUNTY_STATE)
fdat$COUNTY_STATE<-gsub(' county','',fdat$COUNTY_STATE)

temp<-data.frame(as.numeric(as.character(elec.sub.df$PERCENT_DE)),
                 as.numeric(as.character(elec.sub.df$PERCENT_RE)),
                 as.character(elec.sub.df$COUNTY_STATE))
colnames(temp)<-c("PERCENT_DE",'PERCENT_RE','COUNTY_STATE')
temp$COUNTY_STATE<-as.character(temp$COUNTY_STATE)

fdat$PERCENT_DE<-temp$PERCENT_DE[match(fdat$COUNTY_STATE,temp$COUNTY_STATE)]
fdat$PERCENT_RE<-temp$PERCENT_RE[match(fdat$COUNTY_STATE,temp$COUNTY_STATE)]


county.nat.work.est<-laply(county.nat.workers,estimate)
county.tot.work.est<-laply(county.tot.workers,acs::estimate)
county.nat.work.est.prop<-county.nat.work.est/county.tot.work.est
county.med.income.est<-laply(county.med.income,estimate)

county.vars<-data.frame(
  fdat$COUNTY_STATE,
  county.nat.work.est,county.tot.work.est,county.nat.work.est.prop,county.med.income.est)
colnames(county.vars)<-c('COUNTY_STATE',colnames(county.vars)[2:5])
county.vars$COUNTY_STATE<-as.character(county.vars$COUNTY_STATE)

fdat$county.nat.work.est<-county.vars$county.nat.work.est[match(fdat$COUNTY_STATE,county.vars$COUNTY_STATE)]
fdat$county.tot.work.est<-county.vars$county.tot.work.est[match(fdat$COUNTY_STATE,county.vars$COUNTY_STATE)]
fdat$county.nat.work.est.prop<-county.vars$county.nat.work.est.prop[match(fdat$COUNTY_STATE,county.vars$COUNTY_STATE)]
fdat$county.med.income.est<-county.vars$county.med.income.est[match(fdat$COUNTY_STATE,county.vars$COUNTY_STATE)]

setwd('H:/duckabush')
state.fund<-read.csv('state_2007_funding.csv')
colnames(state.fund)<-c('STATE','Function','Total.Pay','Full.Time.Equivalent')

state07.total<-subset(state.fund,Function=='Total')
state07.resparks<-subset(state.fund,Function!='Total')


tem<-stack(tapply(state07.resparks$Total.Pay,state07.resparks$STATE,sum))
colnames(tem)<-c('resparks.spend','STATE')
state07.spend<-join(state07.total,tem)
state07.spend$resparks.ratio<-state07.spend$resparks.spend/state07.spend$Total.Pay

state.df<-data.frame(cbind(state.abb,state.name))
colnames(state.df)<-c('state.abb','STATE')
state07.spend<-join(state07.spend,state.df)
colnames(state07.spend)<-c('sname','Function','Total.Pay','Full.Time.Equivalent',
                           'resparks.spend','resparks.ratio','STATE')

fdat$Total.State.NR.Pay<-state07.spend$Total.Pay[match(fdat$STATE,state07.spend$STATE)]
fdat$FTE.NR.State<-state07.spend$Full.Time.Equivalent[match(fdat$STATE,state07.spend$STATE)]
fdat$State.NR.Pay<-state07.spend$resparks.spend[match(fdat$STATE,state07.spend$STATE)]
fdat$State.NR.Pay.Ratio<-state07.spend$resparks.ratio[match(fdat$STATE,state07.spend$STATE)]

fdat$county.nat.work.est.prop<-fdat$county.nat.work.est.prop

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
fdat$county.tot.work.est<-as.numeric(fdat$county.tot.work.est)
fdat$county.tot.work.est.prop<-as.numeric(fdat$county.nat.work.est.prop)
fdat$county.nat.work.est<-as.numeric(fdat$county.nat.work.est)
fdat$county.med.income.est.mc<-as.numeric(fdat$county.med.income.est.mc)
fdat$county.med.income.est<-as.numeric(fdat$county.med.income.est)

fdat$PERCENT_DE<-as.numeric(fdat$PERCENT_DE)
fdat$PERCENT_RE<-as.numeric(fdat$PERCENT_RE)
fdat$State.NR.Pay<-as.numeric(fdat$State.NR.Pay)
fdat$State.NR.Pay.Ratio<-as.numeric(fdat$State.NR.Pay.Ratio)

fdat$WSAREA<-as.numeric(fdat$WSAREA)
fdat$XFC_NAT<-as.numeric(fdat$XFC_NAT)
fdat$WSA[is.na(fdat$WSA)]<-""

links1 <- paste(paste('http://iaspub.epa.gov/enviro/efservice/PCS_PERMIT_FACILITY/USGS_HYDRO_BASIN_CODE/',
                     fdat$HUC8,sep=''),'/permit_issued_date/</01-JAN-01/permit_issued_date/>/01-JAN-95/count',sep='')
llist1<-as.list(links1)
temp1<-llply(llist1,xmlTreeParse,getDTD=F,isURL=T,.progress='text')
temp2 <- llply(temp1,xmlRoot)
temp3 <- llply(temp2,xmlValue)
temp4 <- llply(temp3, as.numeric)
npdes.permits.huc8.95.00 <- unlist(temp4)

linksa <- paste(paste('http://iaspub.epa.gov/enviro/efservice/PCS_PERMIT_FACILITY/USGS_HYDRO_BASIN_CODE/',
                     fdat$HUC8,sep=''),'/permit_issued_date/</01-JAN-07/permit_issued_date/>/01-JAN-02/count',sep='')
llista<-as.list(linksa)
tempa<-llply(llista,xmlTreeParse,getDTD=F,isURL=T,.progress='text')
tempb <- llply(tempa,xmlRoot)
tempc <- llply(tempb,xmlValue)
tempd <- llply(tempc, as.numeric)
npdes.permits.huc8.02.07 <- unlist(tempd)

linksi <- paste(paste(
  'http://iaspub.epa.gov/enviro/efservice/PCS_PERMIT_FACILITY/USGS_HYDRO_BASIN_CODE/',
  fdat$HUC8,sep=''),
  '/permit_issued_date/</01-JAN-07/permit_issued_date/>/01-JAN-02/PCS_ENFOR_ACTION/count',sep='')
llisti<-as.list(linksi)
tempi<-llply(llisti,xmlTreeParse,getDTD=F,isURL=T,.progress='text')
tempii <- llply(tempi,xmlRoot)
tempiii <- llply(tempii,xmlValue)
tempiv <- llply(tempiii, as.numeric)
npdes.enforcement.huc8.02.07 <- unlist(tempiv)

linksaa <- paste(paste(
  'http://iaspub.epa.gov/enviro/efservice/PCS_PERMIT_FACILITY/USGS_HYDRO_BASIN_CODE/',
  fdat$HUC8,sep=''),
  '/permit_issued_date/</01-JAN-00/permit_issued_date/>/01-JAN-95/PCS_ENFOR_ACTION/count',sep='')
llistaa<-as.list(linksaa)
tempaa<-llply(llistaa,xmlTreeParse,getDTD=F,isURL=T,.progress='text')
tempbb<- llply(tempaa,xmlRoot)
tempcc <- llply(tempbb,xmlValue)
tempdd <- llply(tempcc, as.numeric)
npdes.enforcement.huc8.95.00 <- unlist(tempdd)



npdes.enforce.ratio.02.07<-npdes.enforcement.huc8.02.07/npdes.permits.huc8.02.07
npdes.enforce.ratio.02.07[is.na(npdes.enforce.ratio.02.07)]<-0

npdes.enforce.ratio.95.00<-npdes.enforcement.huc8.95.00/npdes.permits.huc8.95.00
npdes.enforce.ratio.95.00[is.na(npdes.enforce.ratio.95.00)]<-0


fdat$npdes.enforce.ratio.95.00<-npdes.enforce.ratio.95.00
fdat$npdes.enforce.ratio.02.07<- npdes.enforce.ratio.02.07
fdat$npdes.permits.huc8.02.07<- npdes.permits.huc8.02.07
fdat$npdes.permits.huc8.95.00<- npdes.permits.huc8.95.00


temp<-fdat
p806<-read.dbf('past_huc8_2006.dbf')
p801<-read.dbf('past_huc8_2001.dbf')
c806<-read.dbf('crop_huc8_2006.dbf')
c801<-read.dbf('crop_huc8_2001.dbf')
p406<-read.dbf('past_huc4_2006.dbf')
p401<-read.dbf('past_huc4_2001.dbf')
c406<-read.dbf('crop_huc4_2006.dbf')
c401<-read.dbf('crop_huc4_2001.dbf')
i401<-read.dbf('imp_huc4_2001.dbf')
i406<-read.dbf('imp_huc4_2006.dbf')
i801<-read.dbf('imp_huc8_2001.dbf')
i806<-read.dbf('imp_huc8_2006.dbf')
temp$post<-temp$YEAR>=2006
temp$huc8imperv<-ifelse(temp$post,i806$MEAN[match(temp$HUC8,i806$HUC8)],i801$MEAN[match(temp$HUC8,i801$HUC8)])
temp$huc4imperv<-ifelse(temp$post,i406$MEAN[match(temp$HUC4,i406$HUC4)],i401$MEAN[match(temp$HUC4,i401$HUC4)])
temp$huc8crop<-ifelse(temp$post,c806$MEAN[match(temp$HUC8,c806$HUC8)],c801$MEAN[match(temp$HUC8,c801$HUC8)])
temp$huc4crop<-ifelse(temp$post,c406$MEAN[match(temp$HUC4,c406$HUC4)],c401$MEAN[match(temp$HUC4,c401$HUC4)])
temp$huc8past<-ifelse(temp$post,p806$MEAN[match(temp$HUC8,p806$HUC8)],p801$MEAN[match(temp$HUC8,p801$HUC8)])
temp$huc4past<-ifelse(temp$post,p406$MEAN[match(temp$HUC4,p406$HUC4)],p401$MEAN[match(temp$HUC4,p401$HUC4)])

temp$huc8npdespermits<-ifelse(temp$post,npdes.permits.huc8.02.07,npdes.permits.huc8.95.00)
temp$huc8npdesenforceratio<-ifelse(temp$post,npdes.enforce.ratio.02.07,npdes.enforce.ratio.95.00)
fdat<-temp

write.csv(fdat,'model.dat.csv')
dat<-as.data.frame(fdat)
dat$STRAHLER<-ifelse(dat$STRAHLER>5,5,dat$STRAHLER)
dat$STRAHLER<-ifelse(dat$STRAHLER==0,1,dat$STRAHLER)
dat$ACT3<-ifelse(dat$ACTIVED>=3,1,0)
dat$STRAHLER<-as.factor(dat$STRAHLER) 
dat$YEAR<-as.factor(dat$YEAR)
dat$GROUP_RESP<-ifelse(dat$GROUP_RESPONSIBILITY==1,0,dat$GROUP_RESPONSIBILITY)
dat$GROUP_OBJ<-ifelse(dat$FORM_OBJECT=='MISSION',0,as.character(dat$FORM_OBJECT))

dat$huc8imperv.std<-(log(dat$huc8imperv+.1) - mean(log(dat$huc8imperv+.1),na.rm=T))/ (2 * sd(log(dat$huc8imperv+.1),na.rm=T))
dat$huc8past.std<-(log(dat$huc8past+.1) - mean(log(dat$huc8past+.1),na.rm=T))/ (2 * sd(log(dat$huc8past+.1),na.rm=T))
dat$huc8crop.std<-(log(dat$huc8crop+.1) - mean(log(dat$huc8crop+.1),na.rm=T))/ (2 * sd(log(dat$huc8crop+.1),na.rm=T))

dat$PWETL.std<-(log(dat$PWETL+.1) - mean(log(dat$PWETL+.1),na.rm=T))/ (2 * sd(log(dat$PWETL+.1),na.rm=T))
dat$PFOR.std<-(log(dat$PFOR+.1) - mean(log(dat$PFOR+.1),na.rm=T))/ (2 * sd(log(dat$PFOR+.1),na.rm=T))
dat$RDDENS.std<-(log(dat$RDDENS+.1) - mean(log(dat$RDDENS+.1),na.rm=T))/ (2 * sd(log(dat$RDDENS+.1),na.rm=T))
dat$W1_HAG.std<-(log(dat$W1_HAG+.1) - mean(log(dat$W1_HAG+.1),na.rm=T))/ (2 * sd(log(dat$W1_HAG+.1),na.rm=T))
dat$W1_HALL.std<-(log(dat$W1_HALL+.1) - mean(log(dat$W1_HALL+.1),na.rm=T))/ (2 * sd(log(dat$W1_HALL+.1),na.rm=T))
dat$W1_HNOAG.std<-(log(dat$W1_HNOAG+.1) - mean(log(dat$W1_HNOAG+.1),na.rm=T))/ (2 * sd(log(dat$W1_HNOAG+.1),na.rm=T))
dat$POPDENS.std<-(log(dat$POPDENS+.1) - mean(log(dat$POPDENS+.1),na.rm=T))/ (2 * sd(log(dat$POPDENS+.1),na.rm=T))
dat$county.med.income.est.std<-(log(dat$county.med.income.est+.1) - mean(log(dat$county.med.income.est+.1),na.rm=T))/ (2 * sd(log(dat$county.med.income.est+.1),na.rm=T))
dat$State.NR.Pay.Ratio.std<-(log(dat$State.NR.Pay.Ratio+.1) - mean(log(dat$State.NR.Pay.Ratio+.1),na.rm=T))/ (2 * sd(log(dat$State.NR.Pay.Ratio+.1),na.rm=T))
dat$PTL.std<-(log(dat$PTL+.1) - mean(log(dat$PTL+.1),na.rm=T))/ (2 * sd(log(dat$PTL+.1),na.rm=T))
dat$NTL.std<-(log(dat$NTL+.1) - mean(log(dat$NTL+.1),na.rm=T))/ (2 * sd(log(dat$NTL+.1),na.rm=T))
dat$TURB.std<-(log(dat$TURB+.1) - mean(log(dat$TURB+.1),na.rm=T))/ (2 * sd(log(dat$TURB+.1),na.rm=T))
dat$MMI_BENT.std<-(log(dat$MMI_BENT+.1) - mean(log(dat$MMI_BENT+.1),na.rm=T))/ (2 * sd(log(dat$MMI_BENT+.1),na.rm=T))
dat$XCMGW.std<-(log(dat$XCMGW+.1) - mean(log(dat$XCMGW+.1),na.rm=T))/ (2 * sd(log(dat$XCMGW+.1),na.rm=T))
dat$XFC_NAT.std<-(log(dat$XFC_NAT+.1) - mean(log(dat$XFC_NAT+.1),na.rm=T))/ (2 * sd(log(dat$XFC_NAT+.1),na.rm=T))
dat$HUC8AreaSqKm.std<-(log(dat$HUC8AreaSqKm+.1) - mean(log(dat$HUC8AreaSqKm+.1),na.rm=T))/ (2 * sd(log(dat$HUC8AreaSqKm+.1),na.rm=T))
dat$huc8npdesenforceratio.std<-(log(dat$huc8npdesenforceratio+.1) - mean(log(dat$huc8npdesenforceratio+.1),na.rm=T))/ (2 * sd(log(dat$huc8npdesenforceratio+.1),na.rm=T))
dat$huc8npdespermits.std<-(log(dat$huc8npdespermits+.1) - mean(log(dat$huc8npdespermits+.1),na.rm=T))/ (2 * sd(log(dat$huc8npdespermits+.1),na.rm=T))
dat$PERCENT_DE.std<-(log(dat$PERCENT_DE+.1) - mean(log(dat$PERCENT_DE+.1),na.rm=T))/ (2 * sd(log(dat$PERCENT_DE+.1),na.rm=T))
dat$county.nat.work.est.prop.std<-(log(dat$county.nat.work.est.prop+.1) - mean(log(dat$county.nat.work.est.prop+.1),na.rm=T))/ (2 * sd(log(dat$county.nat.work.est.prop+.1),na.rm=T))
dat$XWIDTH.std<-(log(dat$XWIDTH+.1) - mean(log(dat$XWIDTH+.1),na.rm=T))/ (2 * sd(log(dat$XWIDTH+.1),na.rm=T))
dat$XELEV.std<-(log(dat$XELEV+.1) - mean(log(dat$XELEV+.1),na.rm=T))/ (2 * sd(log(dat$XELEV+.1),na.rm=T))
dat$huc8ag.std<-(log(dat$huc8crop+dat$huc8past+.1) - mean(log(dat$huc8crop+dat$huc8past+.1),na.rm=T))/ 
  (2 * sd(log(dat$huc8crop+dat$huc8past+.1),na.rm=T))

write.csv(dat,'cleaned.dat.csv')
save.image("data.ready.lmer.RData")



