library(gdata)

rm(list=ls())
setwd("//Users/TScott/Google Drive/dosewallips/Matrix sampling/Survey Responses _Raw data")

setwd("//Users/TScott/Google Drive/dosewallips/Matrix sampling")
list.files()[10]

library(xlsx)
library(gdata)



test<-read.xlsx(list.files()[10],sheetIndex=3)
nrow(test)


col1<-read.xlsx(list.files()[7],sheetIndex=2)
col2<-read.xlsx(list.files()[7],sheetIndex=3)
file_list<-list.files()

sheet_count<-rep(0,length(list.files()))
for (i in 1:length(list.files()))
{
	sheet_count[i]<-(sheetCount(file_list[i]))
}


drop_front<-sheet_count
screener<-data.frame(matrix(0,ncol=9))
#temp<-read.xls(list.files()[7],method="csv",sheet=3)
colnames(screener)<-seq(1,ncol(screener),1)
both<-data.frame(matrix(0,ncol=10))
#temp<-read.xls(list.files()[7],method="csv",sheet=4)
colnames(both)<-seq(1,ncol(both),1)
scvec<-data.frame(matrix(0,ncol=1))
quvec<-data.frame(matrix(0,ncol=2))

for (i in 1:length(file_list))
#for (i in 1:10)
{
temp_front<-read.xlsx(file_list[i],sheetIndex=1)
	print("i");print(i)
	for (j in 1:sheet_count[i])	
	{
temp<-read.xlsx(file_list[i],sheetIndex=j)
colnames(temp)<-seq(1,ncol(temp),1)
if (ncol(temp)==9)
{
print(nrow(temp))
print("j");print(j)
screener<-rbind(screener,temp)
Screen<-as.character(temp_front$Question.text[1])
scvec<-c(scvec,rep(as.character(Screen),nrow(temp)))
}
else if (ncol(temp)==10)
{
	print(nrow(temp))
both<-rbind(both,temp)
Screen<-as.character(temp_front$Question.text[1])
Qt2<-as.character(temp_front$Question.text[j-2])
quvec2<-rep(as.character(Qt2),nrow(temp))
quvec1<-rep(as.character(Screen),nrow(temp))
quvec12<-cbind(as.character(quvec1),as.character(quvec2))
colnames(quvec12)<-c("X1","X2")
quvec<-rbind(quvec,quvec12)
}
}}
	
head(quvec)	
tt<-cbind(both[-1,],quvec[-1,])
colnames(tt)<-c(colnames(both),"ScreenQ","Q2")

screener$ScreenQ<-as.vector(as.character(scvec))
screener<-screener[-1,]
ff<-screener

write.csv(ff,"//Users/TScott/Google Drive/dosewallips/GoogleDat_ScreenOnly")
write.csv(tt,"//Users/TScott/Google Drive/dosewallips/GoogleDat_QuestionPairs")

