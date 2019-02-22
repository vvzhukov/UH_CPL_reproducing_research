## Paper "Cross-disciplinary evolution of the genomics revolution"
## Fig 2b
## Research reproduced by: Vitalii Z.
## State: In progress

## Loading libraries, defining variables
library(stringr)
library(ggplot2)
library(tidyr)
library(dplyr)

path <- "/Users/apple/UH-CPL/XD_Human_genom/Data_OSF" ## Path to data

## Uploading data 

## Merged Google Scholar and U.S. Funding data
data1 <- read.csv(paste(path,"/Faculty_GoogleScholar_Funding_Data_N4190.csv", sep=''),
                  header = TRUE)

data4 <- read.csv(paste(path,"/GoogleScholar_paper_stats.csv", sep=''),
                  header = F)

colnames(data4)<-c("google_id", "year", "citations", "coathor_codes")
# Count total number of coathors
data4 <- cbind(data4,apply(data4["coathor_codes"],1,function(x){length(strsplit(x,',')[[1]])}))
colnames(data4)<-c("google_id", "year", "citations", "coathor_codes","total_coathors")

## Comment:
## total number of faculty collaborators KTotal
## find among them how many come from the opposite department and take their ratio

# Data description:
# coauthor_codes: [string] Indicates the number of coauthors and types of coauthors belonging to the publication. 
# Numbers indicate “pollinator” coauthors (individuals who do not belong to the group of 4,190   set). These 
# pollinators are coded as 0,1, or 2, whereas are coded according to their google_id. For example, the first 
# publication line is: "1,1,1,--nVNvIAAAAJ", indicating that this publication has 4 coauthors, and only one is a 
# faculty member (  ), which in this case corresponds to the google_id corresponding to the Google Schoar profile 
# researcher. Pollinator codes correspond to their disciplinary type determined by the union of that particular 
# individual’s set of coauthors: 0 = pollinator j only appeared in our dataset with other BIO ; 1 = pollinator j only 
# appeared with other CS ; and 2 corresponds to a mixture of CS and BIO   - i.e. coauthor j is a cross-pollinator.

# 0 - BIO
# 1 - CS
# 2 - XD - pollinators (red line)
# code - direct link (blue line)

# Counting direct, mediated
data4$XD_pollinators <- str_count(data4$coathor_codes, "2,|,2")

data4$Direct_polinators <- data4$total_coathors - (str_count(data4$coathor_codes, "0,|,0|1,|,1|2,|,2")) 
# CORRECTION REQUIRED: not all coathors are XD (see line 61)

data4$author_dept <- left_join(data4, data1, by = c("google_id"))["dept"]
data4$raw_direct_ids <- gsub("0,|,0|1,|,1|2,|,2", "", data4$coathor_codes)

data4$direct_depts <- 0

# Now lets get all direct coathors depts.
# This will take time, as O(n) for this task will be n*m*(1-150), where n-size of data4, m-size of data1, (1-150) amount of coathors. 

for (row in 1:nrow(data4)) {
     data4$direct_depts[row] <- paste(apply(filter(data1, 
                                                   google_id %in% 
                                                       strsplit(data4$raw_direct_ids[row],',')[[1]])["dept"], 
                                            1, 
                                            paste, 
                                            collapse=""), 
                                      collapse = ",")
     }
# # ET~ 32 minutes [i7-5257U @3.1 Ghz]
# 
# # Count XD direct connections for each node
# data4$dir_XD_p <- ifelse(data4$author_dept =="BIO", 
#                           str_count(data4$direct_depts, pattern = "CS"), 
#                           str_count(data4$direct_depts, pattern = "BIO"))

# Direct XD polinators (leafs)
data4$dXDpolinators <- as.numeric(ifelse(data4$author_dept =="BIO",
                          str_count(data4$coathor_codes, pattern = "1"),
                          str_count(data4$coathor_codes, pattern = "0")))

# Direct XD polinators (Fi)
data4$dXDpolinators_ids <- as.numeric(ifelse(data4$author_dept =="BIO",
                                         str_count(data4$direct_depts, pattern = "CS"),
                                         str_count(data4$direct_depts, pattern = "BIO")))

# Sum of direct XD polinators
data4$SUM_XD_Dpolinators <- data4$dXDpolinators + data4$dXDpolinators

# Group by year and get summary:
data4 %>% group_by(year) %>% summarise (Total_authors = sum(total_coathors),
                                        Total_direct_p = sum(Direct_polinators),
                                        Total_direct_XD_p = sum(SUM_XD_Dpolinators),
                                        Total_mediated_XD_p = sum(XD_pollinators),
                                        Total_records = n()) %>% as.data.frame() -> data6

# Dumping data, in order not to run this expensive loop every time 
write.csv(data6, file = paste(path,"/GoogleScholar_paper_stats_summary.csv", sep=""))

# Reading dumped data
data6 <- read.csv(paste(path,"/GoogleScholar_paper_stats_summary.csv", sep=""))

# Total coathors = 'Direct XD links'
ggplot(data=data6) +
    geom_rect(aes(xmin = 1990, xmax = 2003, ymin = -Inf, ymax = Inf, fill = "pink"), alpha = 0.1) +
    geom_line(aes(x=year, y=Total_direct_XD_p/Total_authors), color = "blue") +
    geom_line(aes(x=year, y=Total_mediated_XD_p/Total_authors), color = "red") +
    xlim(1980, 2015) +
    xlab("") +
    ylab("Fraction of XD collaborations") +
    theme(legend.position="none") +
    geom_text(aes(1980,0.12,label = 'Mediated XD links'), color = "red", hjust = 0) +
    geom_text(aes(1980,0.11,label = 'Direct XD links'), color = "blue", hjust = 0) +
    geom_text(aes(1990,0.18,label = ' HGP (1990-2003)'), color = "black", hjust = 0)
