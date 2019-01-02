## Paper "Cross-disciplinary evolution of the genomics revolution"
## Fig 2b
## Research reproduced by: Vitalii Z.
## State: In progress

## Loading libraries, defining variables
library(ggplot2)
path <- "/Users/apple/UH-CPL/XD_Human_genom/Data_OSF" ## Path to data

## Uploading data 

## Merged Google Scholar and U.S. Funding data
data1 <- read.csv(paste(path,"/Faculty_GoogleScholar_Funding_Data_N4190.csv", sep=''),
                  header = TRUE)

str(data1)
data2 <- subset(data1,XDIndicator == "XD")
head(data2)

## total number of faculty collaborators KTotal
## find among them how many come from the opposite department and take their ratio
ggplot(data=data2,aes(x=min_year, y=KDirect/KTotal)) +
    geom_point() +
    geom_smooth() +
    xlim(1980, 2015) +
    xlab("")

ggplot(data=data2,aes(x=min_year, y=KMediated/KTotal)) +
    geom_point() +
    geom_smooth() +
    xlim(1980, 2015) +
    xlab("")