## Paper "Cross-disciplinary evolution of the genomics revolution"
## Fig 4
## Research reproduced by: Vitalii Z.
## State: On progress

## Loading libraries, defining variables
library(ggplot2)
path <- "/Users/apple/UH-CPL/XD_Human_genom/Data_OSF" ## Path to data

## Uploading data 
## Merged Google Scholar and U.S. Funding data
data1 <- read.csv(paste(path,"/Faculty_GoogleScholar_Funding_Data_N4190.csv", sep=''),
                  header = TRUE)

## Linear model
lm(log(t_pubs_citations)~log(SchoolRank)+
                        log(h_index)+
                        log(t_deflated_nsf),
                        log(num_nsf)+
                        log(t_deflated_nih)+
                        log(num_nih)+
                        log(PRCentrality)+
                        log(Chi), 
                        na.action=na.exclude, data=data1)