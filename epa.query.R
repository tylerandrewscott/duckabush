path ''


library(XML)
install.packages('XML')
USGS_HYDRO_BASIN_CODE/17110013/COUNT/
fdat$HUC8
doc<-xmlTreeParse(
'http://iaspub.epa.gov/enviro/efservice/PCS_PERMIT_FACILITY/state_abbr/WA/permit_issued_date/</2000/count',
getDTD=F)


library(doParallel)
library(plyr)
install.packages('dplyr')
library(dplyr)
?aaply
foreach(i = 1:length(fdat$HUC8)) %do%
  

links<-
  

links <- paste(paste('http://iaspub.epa.gov/enviro/efservice/PCS_PERMIT_FACILITY/USGS_HYDRO_BASIN_CODE/',
                     fdat$HUC8,sep=''),'/permit_issued_date/</01-JAN-01/permit_issued_date/>/01-JAN-95/count',sep='')
huc8.npdes.95.00<-foreach(i = 1:length(links)) %do% as.numeric(xmlValue(xmlRoot(xmlTreeParse(links[i],getDTD=F)))[[1]]

links <- paste(paste('http://iaspub.epa.gov/enviro/efservice/PCS_PERMIT_FACILITY/USGS_HYDRO_BASIN_CODE/',
    fdat$HUC8,sep=''),'/permit_issued_date/</01-JAN-01/permit_issued_date/>/01-JAN-95/count',sep='')[1:5]
temp1<-foreach(i = 1:length(links)) %do%  xmlTreeParse(links[i],getDTD=F)
temp2 <- llply(temp1,xmlRoot)
temp3 <- llply(temp2,xmlValue)
temp4 <- llply(temp3, as.numeric)
npdes.permits.huc8.95.00 <- unlist(temp4)

links <- paste(paste('http://iaspub.epa.gov/enviro/efservice/PCS_PERMIT_FACILITY/USGS_HYDRO_BASIN_CODE/',
                     fdat$HUC8,sep=''),'/permit_issued_date/</01-JAN-07/permit_issued_date/>/01-JAN-02/count',sep='')[1:5]
temp1<-foreach(i = 1:length(links)) %do%  xmlTreeParse(links[i],getDTD=F)
temp2 <- llply(temp1,xmlRoot)
temp3 <- llply(temp2,xmlValue)
temp4 <- llply(temp3, as.numeric)
npdes.permits.huc8.02.07 <- unlist(temp4)

  class(temp1)
  as.numeric(xmlValue(xmlRoot(
  
 ))[[1]])
                                                                                                                              
huc8.npdes.95.00                                                                                                                            
                                                                                                                              
?foreach
?xmlTreeParse
rm(test)
%.%
  alply()

doc<-xmlTreeParse(
  'http://iaspub.epa.gov/enviro/efservice/PCS_PERMIT_FACILITY/USGS_HYDRO_BASIN_CODE/17110013/permit_issued_date/</01-JAN-01/permit_issued_date/>/01-JAN-95/count',
  getDTD=F)
r <- xmlRoot(doc)
c <- as.numeric(xmlValue(r[[1]]))
?'%.%'

xmlName(r)
xmlSize(r)
r[[1]]
xmlAttrs(r)

sapply(xmlChildren(r[[1]]),xmlName)

xmlSApply(r[[1]], xmlName)
xmlApply(r[[1]], xmlAttrs)
xmlSApply(r[[1]], xmlSize)
class(r[[1]][[1]])

r
doc

doc
src = xpathApply(doc, "//a[@envirofacts]", xmlGetAttr, "envirofacts")

xmlTreeParse('http://iaspub.epa.gov/enviro/efservice/PCS_PERMIT_FACILITY/USGS_HYDRO_BASIN_CODE/17110013/COUNT')
xpathApply(temp,'COUNT')
xmlGetAttr(temp,'COUNT')
getRelativeURL("17110013/COUNT/", "http://iaspub.epa.gov/enviro/efservice/PCS_PERMIT_FACILITY/USGS_HYDRO_BASIN_CODE/")
