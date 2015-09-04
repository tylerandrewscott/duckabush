rm(list=ls())
require(mapproj)
require(ggmap)
require(DeducerSpatial)
require(UScensus2010)
require("rgdal") # requires sp, will use proj.4 if installed
require("maptools")
require("ggplot2")
require("plyr")
require(acs)
require(plyr)
require(Hmisc)
require(doParallel)

setwd('//Users/TScott/Google Drive/duckabush')
source('model_prep.R')




#dat<-read.csv('07slsstab2a.csv')
#names(dat)
fdat$long<-fdat$XLON_DD.x
fdat$lat<-fdat$XLAT_DD.x

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

setwd('//Users/TScott/Downloads/elpo08p020_nt00335')
elec = readOGR(dsn='//Users/TScott/Downloads/elpo08p020_nt00335',
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
temp2<-join(fdat,temp,by='COUNTY_STATE')

fdat<-temp2

county.nat.work.est<-laply(county.nat.workers,estimate)
county.tot.work.est<-laply(county.tot.workers,estimate)
county.nat.work.est.prop<-county.nat.work.est/county.tot.work.est
county.med.income.est<-laply(county.med.income,estimate)

county.vars<-data.frame(
  fdat$COUNTY_STATE,
        county.nat.work.est,county.tot.work.est,county.nat.work.est.prop,county.med.income.est)
colnames(county.vars)<-c('COUNTY_STATE',colnames(county.vars)[2:5])
county.vars$COUNTY_STATE<-as.character(county.vars$COUNTY_STATE)

fdat<-cbind(fdat,county.vars[,-1])
fdat$GROUP[is.na(fdat$GROUP)]<-0

setwd('//Users/TScott/Google Drive/duckabush')
state.fund<-read.csv('state_2007_funding.csv')
colnames(state.fund)<-c('STATE','Function','Total.Pay','Full.Time.Equivalent')

state07.total<-subset(state.fund,Function=='Total')
state07.resparks<-subset(state.fund,Function!='Total')
head(state07.resparks)
state07.total
tem<-stack(tapply(state07.resparks$Total.Pay,state07.resparks$STATE,sum))
colnames(tem)<-c('resparks.spend','STATE')
state07.spend<-join(state07.total,tem)
state07.spend$resparks.ratio<-state07.spend$resparks.spend/state07.spend$Total.Pay

state.df<-data.frame(cbind(state.abb,state.name))
colnames(state.df)<-c('state.abb','STATE')
state07.spend<-join(state07.spend,state.df)
colnames(state07.spend)<-c('sname','Function','Total.Pay','Full.Time.Equivalent',
                           'resparks.spend','resparks.ratio','STATE')

fdat<-join(fdat,state07.spend)

