## Paper "Cross-disciplinary evolution of the genomics revolution"
## Fig 2b
## Research reproduced by: Vitalii Z.

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

ggplot(data=data2,aes(x=min_year, y=Chi)) +
    geom_smooth() +
    xlim(1980, 2015) +
    xlab("")

ggplot(data=data1,aes(x=min_year, y=KDirect)) +
    geom_smooth() +
    xlim(1980, 2015)