## Paper "Cross-disciplinary evolution of the genomics revolution"
## Fig 4
## Research reproduced by: Vitalii Z.
## State: In progress

## Loading libraries, defining variables
library(ggplot2)
library(data.table)
library(splitstackshape)
path <- "/Users/apple/UH-CPL/XD_Human_genom/Data_OSF" ## Path to data

## Uploading data 
## Merged Google Scholar and U.S. Funding data
data1 <- read.csv(paste(path,"/Faculty_GoogleScholar_Funding_Data_N4190.csv", sep=''),
                  header = TRUE)

data2 <- read.csv(paste(path,"/ComputerScience_citations_stats_CitationNormalizationData.csv", sep=''),
                  header = TRUE)

data3 <- read.csv(paste(path,"/Biology_citations_stats_CitationNormalizationData.csv", sep=''),
                  header = TRUE)

## Google Scholar — publication and citation data
data4 <- read.csv(paste(path,"/GoogleScholar_paper_stats.csv", sep=''),
                  header = F)
colnames(data4)<-c("google_id", "year", "citations", "coathor_codes")

## CV+Network: Adding new column with citations summ data for each author, based on data4
data4 <- cSplit(data4, "coathor_codes", ",", "long")[grep("_",coathor_codes)]


dt_data4 <- data.table(data4)
dt_data4 <- dt_data4[,by=google_id, year, citations]

dt_data4 <- dt_data4[,list(citations_summ=sum(citations)),by=google_id]
merged_data <- merge(data1,dt_data4,by="google_id", all.y = T)

## CV+Network (normalized): 
Norm_data <- merge(data3, data4, all=T)
dt_Norm_data <- dt_Norm_data[,list(citations_summ=sum(citations)),by=google_id]

## Linear model
## Natural logarithm is only defined for x>0, log1p instead of log

## Linear model for CV data
mdl_cv <- lm(log1p(t_pubs_citations)~log1p(SchoolRank)+
                        log1p(h_index)+
                        log1p(t_deflated_nsf)+
                        log1p(num_nsf)+
                        log1p(t_deflated_nih)+
                        log1p(num_nih)+
                        log1p(PRCentrality)+
                        log1p(Chi), 
                        na.action=na.exclude, data=data1)

## Linear model for combined CV+Network data
mdl_cv_nw <- lm(log1p(citations_summ)~log1p(SchoolRank)+
                 log1p(h_index)+
                 log1p(t_deflated_nsf)+
                 log1p(num_nsf)+
                 log1p(t_deflated_nih)+
                 log1p(num_nih)+
                 log1p(PRCentrality)+
                 log1p(Chi), 
             na.action=na.exclude, data=merged_data)
