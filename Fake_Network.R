rm(list=ls())

library(statnet)

dat_imp=sort(read.csv(file="//Users/TScott/Google Drive/elwha/edgelist_implement.csv",row.names=1))
dat_plan=sort(read.csv(file="//Users/TScott/Google Drive/elwha/edgelist_plan.csv",row.names=1))
dat_cons=sort(read.csv(file="//Users/TScott/Google Drive/elwha/edgelist_consult.csv",row.names=1))
dat_all=sort(read.csv(file="//Users/TScott/Google Drive/elwha/edgelist_all.csv",row.names=1))
dat_all=sort(dat_all[dat_all$Contact!=" "&dat_all$Contact!="",])
resp.dat=read.csv(file="//Users/TScott/Google Drive/elwha/Response.Contact.Dat.csv",row.names=1)
edge.dat=read.csv(file="//Users/TScott/Google Drive/elwha/Edgelist.Dat.Good.csv",row.names=1)
head(sort(dat_all$Contact),10)



temp = edge.dat[edge.dat$TType=="WT",]
dat_imp = temp[temp$Contact!=""&temp$Contact!=" ",]
dat_imp$Type = "Imp"

temp = edge.dat[edge.dat$TType=="PT",]
dat_plan = temp[temp$Contact!=""&temp$Contact!=" ",]
dat_plan$Type = "Plan"

temp = edge.dat[edge.dat$TType=="CT",]
dat_cons = temp[temp$Contact!=""&temp$Contact!=" ",]
dat_cons$Type = "Cons"

temp = edge.dat
dat_all = temp[temp$Contact!=""&temp$Contact!=" ",]

vertexes.all<-sort(unique(c(as.character(dat_all[,1]),subset(as.character(dat_all[,2]),dat_all[,2]!=""&dat_all[,2]!=" "))))

temp <- dat_imp
unique.edges<-data.frame(cbind(as.character(sort(unique(paste(temp$ORG,temp$Contact,sep="@@x@@")))),as.numeric(table(paste(temp$ORG,temp$Contact,sep="@@x@@")))))
unique.edges[,2]<-as.numeric(unique.edges[,2]);colnames(unique.edges)<-c("combi","count")
library(stringr)
temp<-data.frame(cbind((str_split_fixed(unique.edges$combi, "@@x@@", 2)),unique.edges$count))
temp$Type = "Imp"
colnames(temp) = c("Tail","Head","Count","Type")
edge_imp = temp

imp_net = as.network.matrix(as.matrix(edge_imp[,1:2]),matrix.type="edgelist",directed=TRUE)
socmat_imp = as.sociomatrix(imp_net)


temp <- dat_plan
unique.edges<-data.frame(cbind(as.character(sort(unique(paste(temp$ORG,temp$Contact,sep="@@x@@")))),as.numeric(table(paste(temp$ORG,temp$Contact,sep="@@x@@")))))
unique.edges[,2]<-as.numeric(unique.edges[,2]);colnames(unique.edges)<-c("combi","count")
library(stringr)
temp<-data.frame(cbind((str_split_fixed(unique.edges$combi, "@@x@@", 2)),unique.edges$count))
temp$Type = "Plan"
colnames(temp) = c("Tail","Head","Count","Type")
edge_plan = temp

plan_net = as.network.matrix(as.matrix(edge_plan[,1:2]),matrix.type="edgelist",directed=TRUE)
socmat_plan = as.sociomatrix(plan_net)

temp <- dat_cons
unique.edges<-data.frame(cbind(as.character(sort(unique(paste(temp$ORG,temp$Contact,sep="@@x@@")))),as.numeric(table(paste(temp$ORG,temp$Contact,sep="@@x@@")))))
unique.edges[,2]<-as.numeric(unique.edges[,2]);colnames(unique.edges)<-c("combi","count")
library(stringr)
temp<-data.frame(cbind((str_split_fixed(unique.edges$combi, "@@x@@", 2)),unique.edges$count))
temp$Type = "Cons"
colnames(temp) = c("Tail","Head","Count","Type")
edge_cons = temp

cons_net = as.network.matrix(as.matrix(edge_cons[,1:2]),matrix.type="edgelist",directed=TRUE)
socmat_cons = as.sociomatrix(cons_net)


test = sort(data.frame(matrix(cbind(as.character(dat_all$ORG),as.character(dat_all$Contact),as.character(dat_all$TType)),ncol=3)))
test2 = as.matrix(test)
all_net = as.network.matrix(as.matrix(test2[,1:2]),matrix.type="edgelist",directed=TRUE)
set.edge.attribute(all_net,value=test2[,3],attrname="Type")
socmat_all = as.sociomatrix(all_net)[1:100,1:100]
socmat_type = as.sociomatrix(all_net,attrname="Type")[1:100,1:100]



#plot network, save plot

library(ggplot2)
library(grid)
library(sna)
library(Hmisc)
library(reshape2)
# Empty ggplot2 theme
new_theme_empty <- theme_bw()
new_theme_empty$line <- element_blank()
new_theme_empty$rect <- element_blank()
new_theme_empty$strip.text <- element_blank()
new_theme_empty$axis.text <- element_blank()
new_theme_empty$plot.title <- element_blank()
new_theme_empty$axis.title <- element_blank()
new_theme_empty$plot.margin <- structure(c(-1, -1, -1, -1), unit = "lines",
                                         valid.unit = 3L, class = "unit")
new_theme_empty$legend.position <- "bottom"
new_theme_empty$legend.title <- element_text(face="bold",size=rel(1))
new_theme_empty$legend.key.height<-c(3,unit="lines")
#new_theme_empty$legend.margin<- unit(c(0,1,1,1),"cm")



adjacencyMatrix <- socmat_all
tieMatrix<-socmat_type

layoutCoordinates <- gplot(adjacencyMatrix)  # Get graph layout coordinates
adjacencyList <- melt(adjacencyMatrix)  # Convert to list of ties only
adjacencyTT<-melt(tieMatrix)
adjacencyList <- adjacencyList[adjacencyList$value > 0, ]
adjacencyTie <- adjacencyTT[adjacencyTT$value != "0", ]

#get.edge.value(first.net,attrname="TType")
#list.edge.attributes(first.net)
#tietype.names<-get.edge.attribute(first.net$mel,attrname="TType")


# Function to generate paths between each connected node

#len was 1000
edgeMaker <- function(whichRow, len = 1000, curved = TRUE, tie.cols=adjacencyTie){
  fromC <- layoutCoordinates[adjacencyList[whichRow, 1], ]  # Origin
  toC <- layoutCoordinates[adjacencyList[whichRow, 2], ]  # Terminus
  TieC <- tie.cols[whichRow, 3]  # TieType
  # Add curve:
  graphCenter <- colMeans(layoutCoordinates)  # Center of the overall graph
  bezierMid <- c(fromC[1], toC[2])  # A midpoint, for bended edges
  distance1 <- sum((graphCenter - bezierMid)^2)
  if(distance1 < sum((graphCenter - c(toC[1], fromC[2]))^2)){
    bezierMid <- c(toC[1], fromC[2])
    }  # To select the best Bezier midpoint
  bezierMid <- (fromC + toC + bezierMid) / 3  # Moderate the Bezier midpoint
  if(curved == FALSE){bezierMid <- (fromC + toC) / 2}  # Remove the curve
 
  edge <- data.frame(bezier(c(fromC[1], bezierMid[1], toC[1]),  # Generate
                            c(fromC[2], bezierMid[2], toC[2]),  # X & y
                            evaluation = len))  # Bezier path coordinates
  edge$Sequence <- 1:len  # For size and colour weighting in plot
  edge$Tie <- TieC
  edge$Group <- paste(adjacencyList[whichRow, 1:2], collapse = ">")
  return(edge)
  }


set.vertex.attribute(all_net,attrname="Ideg",value=degree(all_net,cmode="indegree"))
set.vertex.attribute(all_net,attrname="Odeg",value=degree(all_net,cmode="outdegree"))
Idg<-get.vertex.attribute(all_net,"Ideg")
Odg<-get.vertex.attribute(all_net,"Odeg")
Nme<-network.vertex.names(all_net)
lC.df<-data.frame(layoutCoordinates,Idg,Odg,Nme)
#Sample.LC<-data.frame(lC.df[sample(seq(1,nrow(lC.df),1),200),])
#Set.LC<-data.frame(lC.df[sample(seq(1,nrow(lC.df),1),200),])

Sample.LC<-data.frame(lC.df[
c(
which(lC.df$Nme=="WDOE"),
which(lC.df$Nme=="USEPA"),
which(lC.df$Nme=="WDFW"),
which(lC.df$Nme=="King County"),
which(lC.df$Nme=="NWIFC"),
which(lC.df$Nme=="Mason County"),
which(lC.df$Nme=="WSU"),
which(lC.df$Nme=="Skagit Tribe"),
which(lC.df$Nme=="PSP"),
which(lC.df$Nme=="NOAA "),
which(lC.df$Nme=="City of Kent"),
which(lC.df$Nme=="Lummi Tribe"),
which(lC.df$Nme=="Port of Tacoma"),
which(lC.df$Nme=="Trout Unlimited"),
which(lC.df$Nme=="Island Recycling"),
which(lC.df$Nme=="Vashon Park District"),
which(lC.df$Nme=="NatureBridge"),
which(lC.df$Nme=="US Navy"),
which(lC.df$Nme=="Stillwaters Environmental Center"),
which(lC.df$Nme=="City of SeaTac")),])
levels(Sample.LC$Nme)[170]<-"EPA"
Sample.LC$Nme[Sample.LC$Nme=="USEPA"]<-"EPA"
sort(layoutCoordinates[,1])

# Generate a (curved) edge path for each pair of connected nodes
allEdges <- lapply(1:nrow(adjacencyList), edgeMaker, len = 500, curved = TRUE,tie.cols=adjacencyTie)
allEdges <- do.call(rbind, allEdges)  # a fine-grained path ^, with bend ^

zp1 <- ggplot(allEdges)  # Pretty simple plot code
zp1 <- zp1 + geom_path(aes(x = x, y = y, group = Group, 
						 colour = Tie, size = -Sequence,position="dodge"),alpha=.8 ) # Edges with gradient
                            # and taper
#zp1 <- zp1 + geom_path(aes(x = x, y = y, group = Group,  # Edges with gradient
                          # colour = e.values, size = -Sequence))  # and taper
zp1 <- zp1 + geom_point(data = data.frame(layoutCoordinates),  # Add nodes
                        aes(x = x, y = y), size = 2, pch = 21,
                        colour = "black", fill = "gray")  # Customize gradient v
##zp1 <- zp1 + geom_text(data = data.frame(layoutCoordinates),  # Add nodes
                       # aes(x = x, y = y,label=Nme), size = 2,
                        #colour = "black", fill = "gray")
zp1 <- zp1 + geom_text(data = data.frame(Sample.LC),  # Add nodes
                        aes(x = x, y = y+2.2,label=Nme), size = 2,
                        colour = "black", fill = "gray",fontface="bold") 
zp1 <- zp1 + geom_point(data = data.frame(Sample.LC),  # Add nodes
                        aes(x = x, y = y), size = 2, pch = 21,
                        colour = "black", fill = "black",position="jitter")  # Customize gradient v             
#zp1 <- zp1 + geom_text(data = lC.df, aes(x=x,y=x,label=ifelse(Odg>20|Idg>20,as.character(Nme),"")),)  
#zp1 <- zp1 + geom_text(data = Sample.LC, aes(x=x,y=x,label=as.character(Nme)))                       
#zp1 <- zp1 + scale_colour_gradient(low = gray(0), high = gray(9/10), guide = "none")
zp1 <- zp1 + scale_size(range = c(1/10, 1), guide = "none")  # Customize taper
#zp1 <- zp1 + scale_x_continuous(limits = c(-55, 160))
#zp1 <- zp1 + scale_y_continuous(limits= c(-175,40)) 
#zp1 <- zp1 + theme(legend.background = element_rect(colour = 'purple', fill = 'pink', size = 3, linetype='dashed'))
zp1 <- zp1 + scale_colour_discrete(name="Tie Type",labels=c("Consultation","Planning","Implementation"))
zp1 <- zp1 + guides(colour = guide_legend(title.position="top",title.hjust=.5,keyheight=1),line=element_line(size=2))
zp2 <- zp1 + new_theme_empty  # Clean up plot
print(zp2)
#ggsave("/Users/TScott/Google Drive/elwha/ggplot network tie edges.png", zp2, type = "cairo-png")


#new_theme_empty$legend.margin<-structure(c(-1, -1, -1, 0), unit = "lines")
#data(coleman)  # Load a high school friendship network
#adjacencyMatrix <- coleman[1, , ]  # Fall semester
adjacencyMatrix <- socmat_cons
#tieMatrix<-socmat_type
layoutCoordinates <- gplot(adjacencyMatrix) # Get graph layout coordinates
adjacencyList <- melt(adjacencyMatrix)  # Convert to list of ties only
#adjacencyTT<-melt(tieMatrix)
adjacencyList <- adjacencyList[adjacencyList$value > 0, ]
#adjacencyTie <- adjacencyTT[adjacencyTT$value != "0", ]

#get.edge.value(first.net,attrname="TType")
#list.edge.attributes(first.net)
#tietype.names<-get.edge.attribute(first.net$mel,attrname="TType")


# Function to generate paths between each connected node

#len was 1000
edgeMaker <- function(whichRow, len = 1000, curved = TRUE, tie.cols=adjacencyTie){
  fromC <- layoutCoordinates[adjacencyList[whichRow, 1], ]  # Origin
  toC <- layoutCoordinates[adjacencyList[whichRow, 2], ]  # Terminus
  #TieC <- tie.cols[whichRow, 3]  # TieType
  TieC = "Cons"
  # Add curve:
  graphCenter <- colMeans(layoutCoordinates)  # Center of the overall graph
  bezierMid <- c(fromC[1], toC[2])  # A midpoint, for bended edges
  distance1 <- sum((graphCenter - bezierMid)^2)
  if(distance1 < sum((graphCenter - c(toC[1], fromC[2]))^2)){
    bezierMid <- c(toC[1], fromC[2])
    }  # To select the best Bezier midpoint
  bezierMid <- (fromC + toC + bezierMid) / 3  # Moderate the Bezier midpoint
  if(curved == FALSE){bezierMid <- (fromC + toC) / 2}  # Remove the curve
 
  edge <- data.frame(bezier(c(fromC[1], bezierMid[1], toC[1]),  # Generate
                            c(fromC[2], bezierMid[2], toC[2]),  # X & y
                            evaluation = len))  # Bezier path coordinates
  edge$Sequence <- 1:len  # For size and colour weighting in plot
  edge$Tie <- "CT"
  edge$Group <- paste(adjacencyList[whichRow, 1:2], collapse = ">")
  return(edge)
  }


set.vertex.attribute(cons_net,attrname="Ideg",value=degree(all_net,cmode="indegree"))
set.vertex.attribute(cons_net,attrname="Odeg",value=degree(all_net,cmode="outdegree"))
Idg<-get.vertex.attribute(cons_net,"Ideg")
Odg<-get.vertex.attribute(cons_net,"Odeg")
Nme<-network.vertex.names(cons_net)
lC.df<-data.frame(layoutCoordinates,Idg,Odg,Nme)
#Sample.LC<-data.frame(lC.df[sample(seq(1,nrow(lC.df),1),200),])
#Set.LC<-data.frame(lC.df[sample(seq(1,nrow(lC.df),1),200),])

Sample.LC<-data.frame(lC.df[
c(
which(lC.df$Nme=="WDOE"),
which(lC.df$Nme=="USEPA"),
which(lC.df$Nme=="WDFW"),
which(lC.df$Nme=="King County"),
which(lC.df$Nme=="NWIFC"),
which(lC.df$Nme=="Mason County"),
which(lC.df$Nme=="WSU"),
which(lC.df$Nme=="Skagit Tribe"),
which(lC.df$Nme=="PSP"),
which(lC.df$Nme=="NOAA "),
which(lC.df$Nme=="City of Kent"),
which(lC.df$Nme=="Lummi Tribe"),
which(lC.df$Nme=="Port of Tacoma"),
which(lC.df$Nme=="Trout Unlimited"),
which(lC.df$Nme=="Island Recycling"),
which(lC.df$Nme=="Vashon Park District"),
which(lC.df$Nme=="NatureBridge"),
which(lC.df$Nme=="US Navy"),
which(lC.df$Nme=="Stillwaters Environmental Center"),
which(lC.df$Nme=="City of SeaTac")),])
levels(Sample.LC$Nme)[170]<-"EPA"
Sample.LC$Nme[Sample.LC$Nme=="USEPA"]<-"EPA"

# Generate a (curved) edge path for each pair of connected nodes
allEdges <- lapply(1:nrow(adjacencyList), edgeMaker, len = 500, curved = TRUE,tie.cols=adjacencyTie)
allEdges <- do.call(rbind, allEdges)  # a fine-grained path ^, with bend ^

zp1 <- ggplot(allEdges)  # Pretty simple plot code
zp1 <- zp1 + geom_path(aes(x = x, y = y, group = Group, 
		 size = -Sequence,position="dodge"),colour = "pink" ,alpha=.8 ) 
		# Edges with gradient
                            # and taper
#zp1 <- zp1 + geom_path(aes(x = x, y = y, group = Group,  # Edges with gradient
                          # colour = e.values, size = -Sequence))  # and taper
zp1 <- zp1 + geom_point(data = data.frame(layoutCoordinates),  # Add nodes
                        aes(x = x, y = y), size = 2, pch = 21,
                        colour = "black", fill = "gray")  # Customize gradient v
##zp1 <- zp1 + geom_text(data = data.frame(layoutCoordinates),  # Add nodes
                       # aes(x = x, y = y,label=Nme), size = 2,
                        #colour = "black", fill = "gray")
zp1 <- zp1 + geom_text(data = data.frame(Sample.LC),  # Add nodes
                        aes(x = x, y = y+2.2,label=Nme), size = 2,
                        colour = "black", fill = "gray",fontface="bold") 
zp1 <- zp1 + geom_point(data = data.frame(Sample.LC),  # Add nodes
                        aes(x = x, y = y), size = 2, pch = 21,
                        colour = "black", fill = "black")  # Customize gradient v             
#zp1 <- zp1 + geom_text(data = lC.df, aes(x=x,y=x,label=ifelse(Odg>20|Idg>20,as.character(Nme),"")),)  
#zp1 <- zp1 + geom_text(data = Sample.LC, aes(x=x,y=x,label=as.character(Nme)))                       
#zp1 <- zp1 + scale_colour_gradient(low = gray(0), high = gray(9/10), guide = "none")
zp1 <- zp1 + scale_size(range = c(1/10, 1), guide = "none")  # Customize taper
#zp1 <- zp1 + scale_x_continuous(limits = c(-30, 57))
#zp1 <- zp1 + scale_y_continuous(limits= c(-24,61.5)) 
#zp1 <- zp1 + theme(legend.background = element_rect(colour = 'purple', fill = 'pink', size = 3, linetype='dashed'))
zp1 <- zp1 + scale_colour_discrete(name="Tie Type",labels=c("Consultation"))
zp1 <- zp1 + guides(colour = guide_legend(title.position="top",title.hjust=.5,keyheight=1),line=element_line(size=2))
zp3 <- zp1 + new_theme_empty  # Clean up plot

ggsave("/Users/TScott/Google Drive/elwha/ggplot network tie consultation edges.png", zp3, h = 9/2, w = 9/2, type = "cairo-png")

#new_theme_empty$legend.margin<-structure(c(-1, -1, -1, 0), unit = "lines")
#data(coleman)  # Load a high school friendship network
#adjacencyMatrix <- coleman[1, , ]  # Fall semester
adjacencyMatrix <- socmat_plan
#tieMatrix<-socmat_type
layoutCoordinates <- gplot(adjacencyMatrix)  # Get graph layout coordinates
adjacencyList <- melt(adjacencyMatrix)  # Convert to list of ties only
#adjacencyTT<-melt(tieMatrix)
adjacencyList <- adjacencyList[adjacencyList$value > 0, ]
#adjacencyTie <- adjacencyTT[adjacencyTT$value != "0", ]

#len was 1000
edgeMaker <- function(whichRow, len = 1000, curved = TRUE, tie.cols=adjacencyTie){
  fromC <- layoutCoordinates[adjacencyList[whichRow, 1], ]  # Origin
  toC <- layoutCoordinates[adjacencyList[whichRow, 2], ]  # Terminus
  #TieC <- tie.cols[whichRow, 3]  # TieType
  TieC = "Plan"
  # Add curve:
  graphCenter <- colMeans(layoutCoordinates)  # Center of the overall graph
  bezierMid <- c(fromC[1], toC[2])  # A midpoint, for bended edges
  distance1 <- sum((graphCenter - bezierMid)^2)
  if(distance1 < sum((graphCenter - c(toC[1], fromC[2]))^2)){
    bezierMid <- c(toC[1], fromC[2])
    }  # To select the best Bezier midpoint
  bezierMid <- (fromC + toC + bezierMid) / 3  # Moderate the Bezier midpoint
  if(curved == FALSE){bezierMid <- (fromC + toC) / 2}  # Remove the curve
 
  edge <- data.frame(bezier(c(fromC[1], bezierMid[1], toC[1]),  # Generate
                            c(fromC[2], bezierMid[2], toC[2]),  # X & y
                            evaluation = len))  # Bezier path coordinates
  edge$Sequence <- 1:len  # For size and colour weighting in plot
  edge$Tie <- "PT"
  edge$Group <- paste(adjacencyList[whichRow, 1:2], collapse = ">")
  return(edge)
  }


set.vertex.attribute(plan_net,attrname="Ideg",value=degree(plan_net,cmode="indegree"))
set.vertex.attribute(plan_net,attrname="Odeg",value=degree(plan_net,cmode="outdegree"))
Idg<-get.vertex.attribute(plan_net,"Ideg")
Odg<-get.vertex.attribute(plan_net,"Odeg")
Nme<-network.vertex.names(plan_net)
lC.df<-data.frame(layoutCoordinates,Idg,Odg,Nme)
#Sample.LC<-data.frame(lC.df[sample(seq(1,nrow(lC.df),1),200),])
#Set.LC<-data.frame(lC.df[sample(seq(1,nrow(lC.df),1),200),])

Sample.LC<-data.frame(lC.df[
c(
which(lC.df$Nme=="WDOE"),
which(lC.df$Nme=="USEPA"),
which(lC.df$Nme=="WDFW"),
which(lC.df$Nme=="King County"),
which(lC.df$Nme=="NWIFC"),
which(lC.df$Nme=="Mason County"),
which(lC.df$Nme=="WSU"),
which(lC.df$Nme=="Skagit Tribe"),
which(lC.df$Nme=="PSP"),
which(lC.df$Nme=="NOAA "),
which(lC.df$Nme=="City of Kent"),
which(lC.df$Nme=="Lummi Tribe"),
which(lC.df$Nme=="Port of Tacoma"),
which(lC.df$Nme=="Trout Unlimited"),
which(lC.df$Nme=="Island Recycling"),
which(lC.df$Nme=="Vashon Park District"),
which(lC.df$Nme=="NatureBridge"),
which(lC.df$Nme=="US Navy"),
which(lC.df$Nme=="Stillwaters Environmental Center"),
which(lC.df$Nme=="City of SeaTac")),])
levels(Sample.LC$Nme)[170]<-"EPA"
Sample.LC$Nme[Sample.LC$Nme=="USEPA"]<-"EPA"

# Generate a (curved) edge path for each pair of connected nodes
allEdges <- lapply(1:nrow(adjacencyList), edgeMaker, len = 500, curved = TRUE,tie.cols=adjacencyTie)
allEdges <- do.call(rbind, allEdges)  # a fine-grained path ^, with bend ^

zp1 <- ggplot(allEdges)  # Pretty simple plot code
zp1 <- zp1 + geom_path(aes(x = x, y = y, group = Group, 
						  size = -Sequence,position="dodge"),color = "light green",alpha=.8 ) # Edges with gradient
                            # and taper
#zp1 <- zp1 + geom_path(aes(x = x, y = y, group = Group,  # Edges with gradient
                          # colour = e.values, size = -Sequence))  # and taper
zp1 <- zp1 + geom_point(data = data.frame(layoutCoordinates),  # Add nodes
                        aes(x = x, y = y), size = 2, pch = 21,
                        colour = "black", fill = "gray")  # Customize gradient v
##zp1 <- zp1 + geom_text(data = data.frame(layoutCoordinates),  # Add nodes
                       # aes(x = x, y = y,label=Nme), size = 2,
                        #colour = "black", fill = "gray")
zp1 <- zp1 + geom_text(data = data.frame(Sample.LC),  # Add nodes
                        aes(x = x, y = y+2.2,label=Nme), size = 2,
                        colour = "black", fill = "gray",fontface="bold") 
zp1 <- zp1 + geom_point(data = data.frame(Sample.LC),  # Add nodes
                        aes(x = x, y = y), size = 2, pch = 21,
                        colour = "black", fill = "black")  # Customize gradient v             
#zp1 <- zp1 + geom_text(data = lC.df, aes(x=x,y=x,label=ifelse(Odg>20|Idg>20,as.character(Nme),"")),)  
#zp1 <- zp1 + geom_text(data = Sample.LC, aes(x=x,y=x,label=as.character(Nme)))                       
#zp1 <- zp1 + scale_colour_gradient(low = gray(0), high = gray(9/10), guide = "none")
zp1 <- zp1 + scale_size(range = c(1/10, 1), guide = "none")  # Customize taper
#zp1 <- zp1 + scale_x_continuous(limits = c(-30, 57))
#zp1 <- zp1 + scale_y_continuous(limits= c(-24,61.5)) 
#zp1 <- zp1 + theme(legend.background = element_rect(colour = 'purple', fill = 'pink', size = 3, linetype='dashed'))
zp1 <- zp1 + scale_colour_brewer(name="Tie Type",labels=c("Planning"))
zp1 <- zp1 + guides(colour = guide_legend(title.position="top",title.hjust=.5,keyheight=1),line=element_line(size=2))
zp4 <- zp1 + new_theme_empty  # Clean up plot

ggsave("/Users/TScott/Google Drive/elwha/ggplot network tie planning edges.png", zp4, h = 9/2, w = 9/2, type = "cairo-png")


#new_theme_empty$legend.margin<-structure(c(-1, -1, -1, 0), unit = "lines")
#data(coleman)  # Load a high school friendship network
#adjacencyMatrix <- coleman[1, , ]  # Fall semester
adjacencyMatrix <- socmat_imp
#tieMatrix<-socmat_type
layoutCoordinates <- gplot(adjacencyMatrix)  # Get graph layout coordinates
adjacencyList <- melt(adjacencyMatrix)  # Convert to list of ties only
#adjacencyTT<-melt(tieMatrix)
adjacencyList <- adjacencyList[adjacencyList$value > 0, ]
#adjacencyTie <- adjacencyTT[adjacencyTT$value != "0", ]

#len was 1000
edgeMaker <- function(whichRow, len = 1000, curved = TRUE, tie.cols=adjacencyTie){
  fromC <- layoutCoordinates[adjacencyList[whichRow, 1], ]  # Origin
  toC <- layoutCoordinates[adjacencyList[whichRow, 2], ]  # Terminus
  #TieC <- tie.cols[whichRow, 3]  # TieType
  TieC = "Work"
  # Add curve:
  graphCenter <- colMeans(layoutCoordinates)  # Center of the overall graph
  bezierMid <- c(fromC[1], toC[2])  # A midpoint, for bended edges
  distance1 <- sum((graphCenter - bezierMid)^2)
  if(distance1 < sum((graphCenter - c(toC[1], fromC[2]))^2)){
    bezierMid <- c(toC[1], fromC[2])
    }  # To select the best Bezier midpoint
  bezierMid <- (fromC + toC + bezierMid) / 3  # Moderate the Bezier midpoint
  if(curved == FALSE){bezierMid <- (fromC + toC) / 2}  # Remove the curve
 
  edge <- data.frame(bezier(c(fromC[1], bezierMid[1], toC[1]),  # Generate
                            c(fromC[2], bezierMid[2], toC[2]),  # X & y
                            evaluation = len))  # Bezier path coordinates
  edge$Sequence <- 1:len  # For size and colour weighting in plot
  edge$Tie <- "WT"
  edge$Group <- paste(adjacencyList[whichRow, 1:2], collapse = ">")
  return(edge)
  }


set.vertex.attribute(imp_net,attrname="Ideg",value=degree(imp_net,cmode="indegree"))
set.vertex.attribute(imp_net,attrname="Odeg",value=degree(imp_net,cmode="outdegree"))
Idg<-get.vertex.attribute(imp_net,"Ideg")
Odg<-get.vertex.attribute(imp_net,"Odeg")
Nme<-network.vertex.names(imp_net)
lC.df<-data.frame(layoutCoordinates,Idg,Odg,Nme)
#Sample.LC<-data.frame(lC.df[sample(seq(1,nrow(lC.df),1),200),])
#Set.LC<-data.frame(lC.df[sample(seq(1,nrow(lC.df),1),200),])

Sample.LC<-data.frame(lC.df[
c(
which(lC.df$Nme=="WDOE"),
which(lC.df$Nme=="USEPA"),
which(lC.df$Nme=="WDFW"),
which(lC.df$Nme=="King County"),
which(lC.df$Nme=="NWIFC"),
which(lC.df$Nme=="Mason County"),
which(lC.df$Nme=="WSU"),
which(lC.df$Nme=="Skagit Tribe"),
which(lC.df$Nme=="PSP"),
which(lC.df$Nme=="NOAA "),
which(lC.df$Nme=="City of Kent"),
which(lC.df$Nme=="Lummi Tribe"),
which(lC.df$Nme=="Port of Tacoma"),
which(lC.df$Nme=="Trout Unlimited"),
which(lC.df$Nme=="Island Recycling"),
which(lC.df$Nme=="Vashon Park District"),
which(lC.df$Nme=="NatureBridge"),
which(lC.df$Nme=="US Navy"),
which(lC.df$Nme=="Stillwaters Environmental Center"),
which(lC.df$Nme=="City of SeaTac")),])
levels(Sample.LC$Nme)[170]<-"EPA"
Sample.LC$Nme[Sample.LC$Nme=="USEPA"]<-"EPA"

# Generate a (curved) edge path for each pair of connected nodes
allEdges <- lapply(1:nrow(adjacencyList), edgeMaker, len = 500, curved = TRUE)
allEdges <- do.call(rbind, allEdges)  # a fine-grained path ^, with bend ^

zp1 <- ggplot(allEdges)  # Pretty simple plot code
zp1 <- zp1 + geom_path(aes(x = x, y = y, group = Group, 
						  size = -Sequence,position="dodge"),color = "light blue",alpha=.8 ) 
						  # Edges with gradient
                            # and taper
#zp1 <- zp1 + geom_path(aes(x = x, y = y, group = Group,  # Edges with gradient
                          # colour = e.values, size = -Sequence))  # and taper
zp1 <- zp1 + geom_point(data = data.frame(layoutCoordinates),  # Add nodes
                        aes(x = x, y = y), size = 2, pch = 21,
                        colour = "black", fill = "gray")  # Customize gradient v
##zp1 <- zp1 + geom_text(data = data.frame(layoutCoordinates),  # Add nodes
                       # aes(x = x, y = y,label=Nme), size = 2,
                        #colour = "black", fill = "gray")
zp1 <- zp1 + geom_text(data = data.frame(Sample.LC),  # Add nodes
                        aes(x = x, y = y+2.2,label=Nme), size = 2,
                        colour = "black", fill = "gray",fontface="bold") 
zp1 <- zp1 + geom_point(data = data.frame(Sample.LC),  # Add nodes
                        aes(x = x, y = y), size = 2, pch = 21,
                        colour = "black", fill = "black")  # Customize gradient v             
#zp1 <- zp1 + geom_text(data = lC.df, aes(x=x,y=x,label=ifelse(Odg>20|Idg>20,as.character(Nme),"")),)  
#zp1 <- zp1 + geom_text(data = Sample.LC, aes(x=x,y=x,label=as.character(Nme)))                       
#zp1 <- zp1 + scale_colour_gradient(low = gray(0), high = gray(9/10), guide = "none")
zp1 <- zp1 + scale_size(range = c(1/10, 1), guide = "none")  # Customize taper
#zp1 <- zp1 + scale_x_continuous(limits = c(-30, 57))
#zp1 <- zp1 + scale_y_continuous(limits= c(-24,61.5)) 
#zp1 <- zp1 + theme(legend.background = element_rect(colour = 'purple', fill = 'pink', size = 3, linetype='dashed'))
zp1 <- zp1 + scale_colour_brewer(name="Tie Type",labels=c("Implementation"),palette="Set3")
zp1 <- zp1 + guides(colour = guide_legend(title.position="top",title.hjust=.5,keyheight=1),line=element_line(size=2))
zp1 <- zp1 + new_theme_empty  # Clean up plot

ggsave("/Users/TScott/Google Drive/elwha/ggplot network tie implement edges.png", zp1, h = 9/2, w = 9/2, type = "cairo-png")




