## Paper "Cross-disciplinary evolution of the genomics revolution"
## Fig 2b
## Research reproduced by: Vitalii Z.
## State: In progress

## Loading libraries, defining variables
library(stringr)
library(ggplot2)
library(tidyr)
library(dplyr)
library(gtools)

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
colnames(data4)<-c("google_id", "year", "citations", "coathor_codes","coathors")

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
data4$XD_mediators <- str_count(data4$coathor_codes, "2,|,2")

data4$direct <- data4$coathors - (str_count(data4$coathor_codes, "0,|,0|1,|,1|2,|,2")) - 1

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

# Polinators XD (leafs)
data4$XD_polinators <- as.numeric(ifelse(data4$author_dept =="BIO",
                          str_count(data4$coathor_codes, pattern = "1"),
                          str_count(data4$coathor_codes, pattern = "0")))
# Direct XD (Fi)
data4$XD_direct <- as.numeric(ifelse(data4$author_dept =="BIO",
                                         str_count(data4$direct_depts, pattern = "CS"),
                                         str_count(data4$direct_depts, pattern = "BIO")))
# Sum of direct = poli XD 
data4$XD_dir_poli <- data4$XD_polinators + data4$XD_direct

# Amunt of poli links
data4$polinators <-  data4$coathors - data4$direct

# Group by year and get summary:
data4 %>% group_by(year) %>% summarise (Total_authors = sum(coathors), # Total amount of link (TOTAL)
                                        Total_direct = sum(direct), # Link id -to- id (Direct) (DIRECT)
                                        Total_XD_dir_poli = sum(XD_dir_poli), # Direct (id-id) + Direct(id-num) (TOTAL XD, id, num)
                                        Total_XD_direct = sum(XD_direct), # Direct (id-id) (TOTAL XD, id)
                                        Total_XD_poli = sum(XD_polinators), # Id -to- 0/1 (-)
                                        Total_poli = sum(polinators), #Id -to- 0/1/2 (POLINATORS)
                                        Total_XD_mediators = sum(XD_mediators), # mediated (id -to- 2) (MEDIATED POLI)
                                        Total_records = n()) %>% as.data.frame() -> data6

# Dumping data, in order not to run this expensive loop every time 
write.csv(data6, file = paste(path,"/GoogleScholar_paper_stats_summary2.csv", sep=""))

# Reading dumped data
data6 <- read.csv(paste(path,"/GoogleScholar_paper_stats_summary2.csv", sep=""))

# Grouping by each 2 years (odd vs even). data6 -> data8
data6$calc_value <- data6$Total_XD_direct/data6$Total_direct
data7<-data6[odd(data6$year),]
data8<-data6[even(data6$year),]
data8$Total_authors <- (data7$Total_authors+data8$Total_authors)/2
data8$Total_direct <- (data7$Total_direct+data8$Total_direct)/2
data8$Total_XD_dir_poli <- (data7$Total_XD_dir_poli+data8$Total_XD_dir_poli)/2
data8$Total_XD_poli <- (data7$Total_XD_poli+data8$Total_XD_poli)/2
data8$Total_poli <- (data7$Total_poli+data8$Total_poli)/2
data8$Total_XD_mediators <- (data7$Total_XD_mediators+data8$Total_XD_mediators)/2
data8$Total_records <- (data7$Total_records+data8$Total_records)/2
data8$calc_value <- (data7$calc_value+data8$calc_value)/2
remove(data7)

# new plot (Mohammed experiment)
ggplot(data=data8) +
    geom_rect(aes(xmin = 1990, xmax = 2003, ymin = -Inf, ymax = Inf, fill = "pink"), alpha = 0.1) +
    geom_line(aes(x=year, y=calc_value), color = "blue", size=2) +
    geom_line(aes(x=year, y=Total_XD_mediators/Total_poli), color = "red", size=2) +
    xlim(1980, 2015) +
    #scale_x_continuous(breaks=seq(1980,2015,by=2), limits=c(1980,2015)) +
    xlab("Year") +
    ylab("Fraction of XD collaborations") +
    theme(legend.position="none") +
    geom_text(aes(2005,0.24,label = 'Mediated XD links'), color = "red", hjust = 0, size = 5) +
    geom_text(aes(2005,0.11,label = 'Direct XD links'), color = "blue", hjust = 0, size = 5) +
    geom_text(aes(1990,0.3,label = ' HGP (1990-2003)'), color = "black", hjust = 0, size = 4)
