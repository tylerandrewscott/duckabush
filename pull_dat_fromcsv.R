#this is the most up-to-date version

siteinfo.wsa.df<-read.csv("//Users/TScott/Google Drive/Watershed/Build/Inputs/WSA_Data/WSAMarch2_2009/wsa_siteinfo_ts_final.csv")
ID<-siteinfo.wsa.df$SITE_ID
VISIT<-siteinfo.wsa.df$VISIT_NO
YEAR_T1<-siteinfo.wsa.df$YEAR
NAME<-siteinfo.wsa.df$SITENAME
XLONDD<-siteinfo.wsa.df$XLON_DD
XLATDD<-siteinfo.wsa.df$XLAT_DD
STRAHLER<-siteinfo.wsa.df$STRAHLER
COUNTY<-siteinfo.wsa.df$COUNTY
STATE<-siteinfo.wsa.df$STATE
ECOREGION_NAME<-siteinfo.wsa.df$ECO3_NM
ECOREGION_NUM<-siteinfo.wsa.df$ECO3
ECOREGION_MEGA<-siteinfo.wsa.df$ECOWSA3
ECOREGION_WSA<-siteinfo.wsa.df$ECOWSA9
EPAREGION<-siteinfo.wsa.df$EPAREGION
NAECOCODE_LEVEL1<-siteinfo.wsa.df$NAECO1
NAECOCODE_LEVEL2<-siteinfo.wsa.df$NAECO2
NAECOCODE_LEVEL3<-siteinfo.wsa.df$NAECO3
REF_CONDITION<-siteinfo.wsa.df$RT_WSA
EAST_WEST<-siteinfo.wsa.df$WESTEAST
WEIGHT<-siteinfo.wsa.df$WGT_WSA
WATERSHED_AREA_1<-siteinfo.wsa.df$WSAREA
ELEVATION<-siteinfo.wsa.df$XELEV
siteinfo.wsa.df<-data.frame(ID,YEAR_T1,VISIT,NAME,XLONDD,XLATDD,STRAHLER,COUNTY,STATE,ECOREGION_NAME,ECOREGION_NUM,ECOREGION_MEGA,ECOREGION_WSA,EPAREGION,NAECOCODE_LEVEL1,NAECOCODE_LEVEL2,NAECOCODE_LEVEL3,REF_CONDITION,EAST_WEST,WEIGHT,WATERSHED_AREA_1,ELEVATION)

benmet.wsa.df<-read.csv("//Users/TScott/Google Drive/Watershed/Build/Inputs/WSA_Data/WSAMarch2_2009/wsa_benmet300_ts_final.csv")
tmp<-benmet.wsa.df
VISIT<-tmp$VISIT_NO
WSA_6NAT<-tmp$MMI_WSA
WSA_6BEST<-tmp$MMI_WSABEST
ID<-tmp$SITE_ID
wsa.benthic<-data.frame(ID,VISIT,WSA_6NAT,WSA_6BEST)

stressor.wsa.df<-read.csv("//Users/TScott/Google Drive/Watershed/Build/Inputs/WSA_Data/WSAMarch2_2009/watershedstressor.csv")
tmp<-stressor.wsa.df
VISIT<-tmp$VISIT_NO
AGAREA_PERC<-tmp$PAGT
FORAREA_PERC<-tmp$PFOR
POPDENS<-tmp$POPDENS
URBAREA_PERC<-tmp$PURB
WETAREA_PERC<-tmp$PWETL
ROAD_DENS<-tmp$RDDENS
TOT_AREA<-tmp$LANDAREA
ID<-tmp$SITE_ID

wsa.stress<-data.frame(ID,VISIT,TOT_AREA,ROAD_DENS,AGAREA_PERC,FORAREA_PERC,POPDENS,URBAREA_PERC,WETAREA_PERC)

watchem.wsa.df<-read.csv("//Users/TScott/Google Drive/Watershed/Build/Inputs/WSA_Data/WSAMarch2_2009/waterchemistry.csv")
tmp<-watchem.wsa.df

ID<-tmp$SITE_ID
VISIT<-tmp$VISIT_NO
NITR_LEV_1<-tmp$NTL
SALI_LEV_1<-tmp$CONCAL
PHOS_LEV_1<-tmp$PTL
ACID_LEV_1<-tmp$ANC
wsa.watchem<-data.frame(ID,VISIT,NITR_LEV_1,SALI_LEV_1,PHOS_LEV_1,ACID_LEV_1)

wsa.merge<-merge(merge(merge(siteinfo.wsa.df,wsa.stress,by=c("ID","VISIT")),wsa.watchem,by=c("ID","VISIT")),wsa.benthic,by=c("ID","VISIT"))


###############################
###############################
###############################
#Code in NRSA data
#basic site info
siteinfo.nrsa.df<-read.csv("//Users/TScott/Google Drive/Watershed/Build/Inputs/NRSA_Data/NRSA0809_FinalDataFiles/NRSA0809_Data_SiteInformation_130411.csv")
tmp<-siteinfo.nrsa.df
VISIT<-tmp$VISIT_NO
YEAR_T2<-tmp$YEAR
LOCAL_NAME<-tmp$LOC_NAME
MASTER_ID<-tmp$MASTER_SITEID
ID<-tmp$SITE_ID
WATERSHED_AREA_2<-tmp$NHDWAT_AREA_SQKM
URBAN<-tmp$URBAN
nrsa.site<-data.frame(YEAR_T2,VISIT,LOCAL_NAME,ID,MASTER_ID,WATERSHED_AREA_2,URBAN)

#enterococci
enter.nrsa.df<-read.csv("//Users/TScott/Google Drive/Watershed/Build/Inputs/NRSA_Data/NRSA0809_FinalDataFiles/NRSA0809_Data_Enterococci_Condition_130322.csv")
tmp<-enter.nrsa.df
ID<-tmp$SITE_ID
VISIT<-tmp$VISIT_NO
WSA<-tmp$WSA
WHICH_SAMPLE<-tmp$Cat_Unique
FED<-tmp$FED_OWN
USFS<-tmp$FSEASTWEST
nrsa.enter<-data.frame(ID,VISIT,WSA,WHICH_SAMPLE,FED,USFS)
head(enter.nrsa.df)
nrow(subset(enter.nrsa.df,enter.nrsa.df$WSA=="WSA"))

fish.nrsa.df<-read.csv("//Users/TScott/Google Drive/Watershed/Build/Inputs/NRSA_Data/NRSA0809_FinalDataFiles/NRSA0809_Data_Fish_Condition_130322.csv")
tmp<-fish.nrsa.df
VISIT<-tmp$VISIT_NO
FISH_COND<-tmp$FISH_MMI_COND
ID<-tmp$SITE_ID
nrsa.fish<-data.frame(ID,VISIT,FISH_COND)

hab.nrsa.df<-read.csv("//Users/TScott/Google Drive/Watershed/Build/Inputs/NRSA_Data/NRSA0809_FinalDataFiles/NRSA0809_Data_PhysicalHabitat_Condition_130322.csv")
tmp<-hab.nrsa.df
ID<-tmp$SITE_ID
VISIT<-tmp$VISIT_NO
HUM_DIST<-tmp$W1_HALL
RIPVEG_COND<-tmp$RIPVEG_COND
RIPDIST_COND<-tmp$RIPDIST_COND
INSTCOVER_COND<-tmp$INSTRMCVR_COND
BEDSED_COND<-tmp$BEDSED_COND
nrsa.hab<-data.frame(ID,VISIT,HUM_DIST,RIPVEG_COND,RIPDIST_COND,INSTCOVER_COND,BEDSED_COND)

watchem.nrsa.df<-read.csv("//Users/TScott/Google Drive/Watershed/Build/Inputs/NRSA_Data/NRSA0809_FinalDataFiles/NRSA0809_Data_WaterChemistry_Condition_130322.csv")
tmp<-watchem.nrsa.df
ID<-tmp$SITE_ID
VISIT<-tmp$VISIT_NO
ACID_COND_2<-tmp$ANC_COND
NITR_COND_2<-tmp$NTL_COND
PHOS_COND_2<-tmp$PTL_COND
SALI_COND_2<-tmp$SAL_COND
NITR_LEV_2<-tmp$NTL
PHOS_LEV_2<-tmp$PTL
ACID_LEV_2<-tmp$ANC
SALI_LEV_2<-tmp$COND

nrsa.chem<-data.frame(ID,VISIT,ACID_COND_2,NITR_COND_2,PHOS_COND_2,SALI_COND_2,SALI_LEV_2,ACID_LEV_2,PHOS_LEV_2,NITR_LEV_2)

nrsa.merge<-merge(merge(merge(merge(nrsa.site,nrsa.enter,by=c("ID","VISIT")),nrsa.fish,by=c("ID","VISIT")),nrsa.hab,by=c("ID","VISIT")),nrsa.chem,by=c("ID","VISIT"))
head(nrsa.merge)

nrsa.merge$VISIT
wsa.merge$VISIT
nrow(subset(nrsa.merge,nrsa.merge$WSA=="WSA"&nrsa.merge$VISIT==1))
###############################
###############################
###############################

both.merge<-merge(wsa.merge,nrsa.merge[,-1],by.x="ID",by.y="MASTER_ID")

write.csv(both.merge,"//Users/TScott/Google Drive/Watershed/Build/Inputs/Combined_Data.csv")
