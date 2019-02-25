## Paper "Cross-disciplinary evolution of the genomics revolution"
## Fig 2a
## Research reproduced by: Vitalii Z.
## State: Remake with Gephi lib

## Loading libraries, defining variables

library(splitstackshape)
library(igraph)
varY0<-1900 ## Year of network, lowest border
varY<-1990 ## Year of network, highest border

path <- "/Users/apple/UH-CPL/XD_Human_genom/Data_OSF" ## Path to data

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

summary(nodes)
nodes[duplicated(nodes$id)]
links[duplicated(links$id)]

colnames(nodes)[1]<-c("id") ## google_id modified to id
colnames(links)[1]<-c("from")
colnames(links)[4]<-c("to")

## Transforming links to format from:to = 1:1 and removing links to itself (loops)
links <- cSplit(links, "to", ",", "long")[to!=""]
links <- links[,c(1,4,2,3)]
links <- subset(links,! to %in% c(1,2,0) & ! as.character(from) == as.character(to))

links <- subset(links, (year <= varY) & (year >= varY0)) ## Limiting year by variable
links <- subset(links, from %in% nodes$id) ##
links <- subset(links, to %in% nodes$id)

## Validation: unique(links$from) %in% unique(nodes$id) && unique(links$to) %in% unique(nodes$id)
colnames(links)[1]<-c("Source")
colnames(links)[2]<-c("Target")
write.csv(links, file = "links.csv", row.names=F)
write.csv(nodes, file = "nodes.csv", row.names=F)

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

## CORRECTION:
library(igraph)
set.seed(1234)
G = erdos.renyi.game(20, 0.25)
V(G)$Group1 = sample(3,20, replace=TRUE)

plot(G, vertex.color=rainbow(3, alpha=0.4)[V(G)$Group1])

G_Grouped = G
E(G_Grouped)$weight = 1

## Add edges with high weight between all nodes in the same group
for(i in unique(V(G)$Group1)) {
    GroupV = which(V(G)$Group1 == i)
    G_Grouped = add_edges(G_Grouped, combn(GroupV, 2), attr=list(weight=5))
} 

## Now create a layout based on G_Grouped
set.seed(567)
LO = layout_with_fr(G_Grouped)

## Use the layout to plot the original graph
plot(G, vertex.color=rainbow(3, alpha=0.4)[V(G)$Group1], layout=LO)
