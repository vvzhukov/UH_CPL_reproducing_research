## Paper "Cross-disciplinary evolution of the genomics revolution"
## Fig S1a
## Research reproduced by: Vitalii Z.
## State: In progress..

## Loading libraries, defining variables

library(splitstackshape)
library(igraph)
library(CINNA)
library(evmix)
library(NetSwan)

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

#summary(nodes)
#nodes[duplicated(nodes$id)]
#links[duplicated(links$id)]

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
length(V(net)) ## = nodes
E(net)

## VERTICES

i<-1 #loop iterations
result<-c() #var to store the absolute size of current giantic subnetwork
result_B <- c()
result_PR <- c()
while (length(V(net))!=0) {
    g_comp <- giant_component_extract(net)
    result[i] <- length(V(g_comp[[1]])) #store their size and number
    result_B[i] <- sum(V(g_comp[[1]])$BetCentrality)
    result_PR[i] <- sum(V(g_comp[[1]])$PRCentrality)
    net <- delete.vertices(net, V(net)[V(g_comp[[1]])]) #substrack vetices from orig network
    i<-i+1
}  

par(mfrow=c(1,3))

x <- 1-result/max(result)
y <- 1-(which(result %in% result)/length(result))
smoothingSpline = smooth.spline(x, y, spar=0.4)

plot(x, y, xlab="", ylab="", 
     main="Robustness check; Volume/size; \n Size of giant component \n Sg(q)/Sg(q=0)", 
     cex=1, col="steelblue2", cex.main=0.9)
lines(smoothingSpline, col="black", lwd=3)

x <- 1-result_B/max(result_B)
y <- 1-(which(result_B %in% result_B)/length(result_B))
smoothingSpline = smooth.spline(x, y, spar=1.5)
plot(x, y, xlab="", ylab="", 
     main="Robustness check; BetCentrality; \n Size of giant component \n Sg(q)/Sg(q=0)", 
     cex=1, col="black", cex.main=0.9)
lines(smoothingSpline,
      col="dark blue", lwd=3)

x<-1-result_PR/max(result_PR)
y<-1-(which(result_PR %in% result_PR)/length(result_PR))
smoothingSpline = smooth.spline(x, y, spar=0.5)
plot(x, y, xlab="", ylab="", 
     main="Robustness check; PRCentrality; \n Size of giant component \n Sg(q)/Sg(q=0)", 
     cex=1, col="darkgreen", cex.main=0.9)
lines(smoothingSpline,
      col="purple", lwd=3)

# SWAN test
par(mfrow=c(1,1))

elec <- as.matrix(links[,1:2])
gra<-graph.edgelist(elec, directed=FALSE)

f<-swan_efficiency(gra)
vertex_attr(gra, "efficiency_loss", index = V(gra))<-f
vertex_attr(gra)

f2<-swan_closeness(gra)
bet<-betweenness(gra)
reg<-lm(bet~f2)
summary(reg)

f3<-swan_connectivity(gra)

f4<-swan_combinatory(gra,1841)
plot(f4[,1],f4[,5], type='o', col='yellow',xlab="Fraction of nodes removed",
     ylab="Connectivity loss")
lines(f4[,1],f4[,3], type='o', col='red')
lines(f4[,1],f4[,4], type='o', col='orange')
lines(f4[,1],f4[,2], type='o', col='blue')
legend('bottomright',c("Random", "Betweenness", "Degree", "Cascading"), 
       lty=c(1,1,1,1), pch=c(1,1,1,1), 
       col=c("yellow","blue","red", "orange"))

##----- END TRAINING

## EDGES
i<-1 #loop iterations
result2<-c() #var to store the absolute size of current giantic subnetwork
while (length(E(net))!=0) {
    g_comp <- giant_component_extract(net)
    result2[i] <- length(E(g_comp[[1]])) #store their size and number
    net <- delete.edges(net, E(net)[E(g_comp[[1]])]) #substrack vetices from orig network
    i<-i+1
}  


plot(x=1-result2/max(result2),y=1-(which(result2 %in% result2)/length(result2)),
     xlab="", ylab="", 
     main="Robustness check \n Size of giant component \n Sg(q)/Sg(q=0) (by Edges)", 
     cex=1.3, col="black", cex.main=0.9)
lines(x=1-result2/max(result2),y=1-(which(result2 %in% result2)/length(result2)),
      col="dark red", lwd=3)


# BONUS
# Plot the egree distribution for our network:
deg.dist <- degree_distribution(net, cumulative=T, mode="all")
plot( x=0:max(degree(net)), y=1-deg.dist, pch=19, cex=1.2, col="orange", 
      xlab="Degree", ylab="Cumulative Frequency")
lines(x=0:max(degree(net)), y=1-deg.dist, col="dark green")


# Supplementary #S1.2
# Swan.net package
