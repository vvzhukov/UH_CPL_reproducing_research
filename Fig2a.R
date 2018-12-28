## Paper "Cross-disciplinary evolution of the genomics revolution"
## Fig 2a
## Research reproduced by: Vitalii Z.
## State: Remake with Gephi lib

## Loading libraries, defining variables

library(splitstackshape)
library(igraph)

varY<-1990 ## Year of network
path <- "/Users/apple/Scolar-Plot/XD_Human_genom/Data_OSF" ## Path to data

## Uploading data 

## Merged Google Scholar and U.S. Funding data
data3 <- read.csv(paste(path,"/Faculty_GoogleScholar_Funding_Data_N4190.csv", sep=''),
                  header = TRUE)

## Google Scholar — publication and citation data
data4 <- read.csv(paste(path,"/GoogleScholar_paper_stats.csv", sep=''),
                  header = F)
colnames(data4)<-c("google_id", "year", "citations", "coathor_codes")

## Transforming Data and building F-network (without polinators)

nodes <- data3
links <- data4

colnames(nodes)[1]<-c("id") ## google_id modified to id
colnames(links)[1]<-c("from")
colnames(links)[4]<-c("to")

## Transforming links to format from:to = 1:1 and removing links to itself (loops)
links <- cSplit(links, "to", ",", "long")[to!=""]
links <- links[,c(1,4,2,3)]
links <- subset(links,! to %in% c(1,2,0) & ! as.character(from) == as.character(to))

links <- subset(links, year <= varY) ## Limiting year by variable
links <- subset(links, from %in% nodes$id) ##
links <- subset(links, to %in% nodes$id)

## Validation: unique(links$from) %in% unique(nodes$id) && unique(links$to) %in% unique(nodes$id)

net <- graph.data.frame(links, nodes, directed=F) ## Defining network
## E(net) # The edges of the "net" object 
## V(net) # The vertices of the "net" object 

V(net)$size=log10(degree(net)) ##
V(net)$color=V(net)$XDIndicator ## Assigning the "XDIndicator" attribute as the vertex color
V(net)$color=gsub("CS","magenta",V(net)$color) ## CS will be magenta
V(net)$color=gsub("BIO","green",V(net)$color) ## BIO will be green
V(net)$color=gsub("XD","black",V(net)$color) ## XD will be black
V(net)$frame.color=V(net)$color ## Making node frame the same color as its body

net<-delete.vertices(simplify(net), degree(net)==0) ## Deleting vetices with 0 degree
plot(net, edge.color="grey", vertex.label=NA, 
     edge.width=0.2, layout=layout.kamada.kawai(net), 
     main=varY,frame=T)
legend(x=-1.5, y=-1.1, c("BIO","CS", "XD"), pch=21,
       col="#777777", pt.bg=c("green", "magenta", "black"), 
       pt.cex=2, cex=.8, bty="n", ncol=3)
