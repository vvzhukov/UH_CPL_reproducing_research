## Fig 2a

library(splitstackshape)
library(igraph)

## Uploading data 
## Citation distribution normalization data
data1 <- read.csv("/Users/apple/Scolar-Plot/XD_Human_genom/Data_OSF/Biology_citations_stats_CitationNormalizationData.csv", header = TRUE)
## str(data1)
data2 <- read.csv("/Users/apple/Scolar-Plot/XD_Human_genom/Data_OSF/ComputerScience_citations_stats_CitationNormalizationData.csv", header = TRUE)
## str(data2)
## Merged Google Scholar and U.S. Funding data
data3 <- read.csv("/Users/apple/Scolar-Plot/XD_Human_genom/Data_OSF/Faculty_GoogleScholar_Funding_Data_N4190.csv", header = TRUE)
## str(data3)

## Google Scholar — publication and citation data
data4 <- read.csv("/Users/apple/Scolar-Plot/XD_Human_genom/Data_OSF/GoogleScholar_paper_stats.csv", header = F)
colnames(data4)<-c("google_id", "year", "citations", "coathor_codes")
## str(data4)
## head(data4)

## Building F-network (without polinators)
nodes <- data3
links <- data4

colnames(nodes)[1]<-c("id") ## google_id modified to id
colnames(links)[1]<-c("from")
colnames(links)[4]<-c("to")

## Transforming links to format from:to = 1:1 and removing links to itself

links <- cSplit(links, "to", ",", "long")[to!=""]
links <- links[,c(1,4,2,3)]
links <- subset(links,! to %in% c(1,2,0) & ! as.character(from) == as.character(to))

nodes$id <- as.character(nodes$id)
links$to <- as.character(links$to)
links$from <- as.character(links$from)

links <- subset(links, year <= 2010) ## Limiting year by 1990
nodes <- subset(nodes, id %in% links$from) ##leaving only connected nodes
links <- subset(links, from %in% nodes$id) 
links <- subset(links, to %in% nodes$id)

## head(links); str(links)
## head(nodes); str(nodes)
## Validation: unique(links$from) %in% unique(nodes$id) && unique(links$to) %in% unique(nodes$id)


net <- graph.data.frame(links, nodes, directed=F) 
## net
## E(net) # The edges of the "net" object 
## V(net) # The vertices of the "net" object 

V(net)$size=log10(degree(net))
V(net)$color=V(net)$XDIndicator #assign the "XDIndicator" attribute as the vertex color
V(net)$color=gsub("CS","magenta",V(net)$color) #CS will be magenta
V(net)$color=gsub("BIO","green",V(net)$color) #BIO will be green
V(net)$color=gsub("XD","black",V(net)$color) #XD will be black
## plot.igraph(net,vertex.label=NA)

plot(delete.vertices(simplify(net), degree(net)==0), vertex.label=NA, layout=layout.auto(net))
## net <- simplify(net, remove.multiple = F, remove.loops = T)
## plot(net, edge.arrow.size=.4,vertex.label=NA)
