## Paper "Cross-disciplinary evolution of the genomics revolution"
## Fig 3
## Research reproduced by: Vitalii Z.
## State: Issues with medians, D figure and 

## Loading libraries, defining variables
library(ggplot2)
library(plyr)
library(data.table)

path <- "/Users/apple/UH-CPL/XD_Human_genom/Data_OSF" ## Path to data

## Uploading data 
## Merged Google Scholar and U.S. Funding data
data1 <- read.csv(paste(path,"/Faculty_GoogleScholar_Funding_Data_N4190.csv", sep=''),
                  header = TRUE)

## Defining function for building typical PDF graphs
PDF_and_median_scaled <- function(dv, xv, ## data variable, x-axis variable
                                  gv, XscaleMin, ## group variable
                                  XscaleMax, Xstep) {
    
    mu <- aggregate(xv,list(gv),mean)

    pv <- ggplot(data=dv, aes(x=xv, y=..density.., colour=gv, fill=gv)) +
        geom_density(alpha=0.2, adjust=1, trim=T) +
        geom_vline(data=mu, aes(xintercept=x, colour=Group.1),
                   linetype="dashed", size=1) +
        scale_x_continuous(breaks = seq(XscaleMin, XscaleMax, Xstep)) +
        xlab(deparse(substitute(xv))) +
        ylab('PDF')
    return(pv)
}


#A PDF for min publ year 
# Change the name of axis, everything else is correct
PDF_and_median_scaled(data1,data1$min_year,data1$XDIndicator,1955,2020,10)

#B PDF for total collaboration degree
PDF_and_median_scaled(data1,log(data1$KTotal),data1$XDIndicator,0,2000,500)

#C PDF for Cross-disciplinarity
PDF_and_median_scaled(data1,data1$Chi,data1$XDIndicator,0,1,0.2)

#D PDF for Page rank centrality
PDF_and_median_scaled(data1,data1$PRCentrality*4190,data1$XDIndicator,0,10,1)

#E PDF for mean publication impact factor
PDF_and_median_scaled(data1,data1$mean_of_IF,data1$XDIndicator,0,25,5)

#PDF Total career citations log10
PDF_and_median_scaled(data1,log10(data1$t_pubs_citations),data1$XDIndicator,0,6,1)


## DRAFTS:

# 
# PDF_and_median_log_scaled <- function(dv, xv, ## data variable, x-axis variable
#                                       gv, XscaleMin, ## group variable
#                                       XscaleMax, Xstep, Yscale, breaksv) {
#     
#     mu <- aggregate(xv,list(gv),mean)
#     
#     pv <- ggplot(data=dv, aes(x=xv, y=..density.., colour=gv, fill=gv)) +
#         geom_density(alpha=0.2, adjust=1, trim=T) +
#         geom_vline(data=mu, aes(xintercept=x, colour=Group.1),
#                    linetype="dashed", size=1) +
#         scale_x_continuous(breaks = seq(XscaleMin, XscaleMax, Xstep)) +
#         scale_y_log10(limits=Yscale, breaks=breaksv) +
#         xlab(deparse(substitute(xv))) +
#         ylab('PDF')
#     
#     return(pv)
# }

# mu <- ddply(dv, gv, plyr::here(summarise), gv_mean=mean(xv))
# library(dplyr)
# mu <- ddply(data1, data1$XDIndicator, plyr::here(summarise), gv_mean=mean(data1$min_year))
# ddply(data1,.(X),transform,Y.New = mean(Y))
# 
# library(dplyr)
# df1 <- data1 %>% 
#     group_by(XDIndicator) %>% 
#     mutate(gv_mean = mean(min_year))
# 
# library(data.table)
# df2 <- setDT(data1)[, gv_mean := mean(min_year), by = XDIndicator]
# mu <- setDT(data1)[, gv_mean := mean(data1$min_year), by = data1$XDIndicator]
# mu <- setDT(data1)[, gv_mean := mean(min_year), by = XDIndicator]
# 
# library(data.table)
# mu <- aggregate(data1$min_year, list(data1$XDIndicator), mean)
# mu$Group.1
# aggregate(gv,list(xv),mean)
# 
# 
# PDF_and_median_log_scaled(data1,log10(data1$PRCentrality),data1$XDIndicator,0,10,1,
#                           c(0.001,1), trans_breaks("log10", function(x) 10^x))
# 

