library(statnet)


fnet<-simulate(mod_H2a_cons)


fnet



socmat_all = as.sociomatrix(fnet)




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
tieMatrix<-socmat_all
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
#zp1 <- zp1 + geom_text(data = data.frame(Sample.LC),  # Add nodes
 #                       aes(x = x, y = y+2.2,label=Nme), size = 2,
  #                      colour = "black", fill = "gray",fontface="bold") 
#zp1 <- zp1 + geom_point(data = data.frame(Sample.LC),  # Add nodes
 #                       aes(x = x, y = y), size = 2, pch = 21,
  #                      colour = "black", fill = "black",position="jitter")  # Customize gradient v             
#zp1 <- zp1 + geom_text(data = lC.df, aes(x=x,y=x,label=ifelse(Odg>20|Idg>20,as.character(Nme),"")),)  
#zp1 <- zp1 + geom_text(data = Sample.LC, aes(x=x,y=x,label=as.character(Nme)))                       
#zp1 <- zp1 + scale_colour_gradient(low = gray(0), high = gray(9/10), guide = "none")
zp1 <- zp1 + scale_size(range = c(1/10, 1), guide = "none")  # Customize taper
#zp1 <- zp1 + scale_x_continuous(limits = c(-55, 160))
#zp1 <- zp1 + scale_y_continuous(limits= c(-175,40)) 
#zp1 <- zp1 + theme(legend.background = element_rect(colour = 'purple', fill = 'pink', size = 3, linetype='dashed'))
#zp1 <- zp1 + scale_colour_discrete(name="Tie Type",labels=c("Consultation","Planning","Implementation"))
zp1 <- zp1 + guides(colour = guide_legend(title.position="top",title.hjust=.5,keyheight=1),line=element_line(size=2))
zp2 <- zp1 + new_theme_empty  # Clean up plot

print(zp2)
