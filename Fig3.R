## Paper "Cross-disciplinary evolution of the genomics revolution"
## Fig 3
## Research reproduced by: Vitalii Z.

## Loading libraries, defining variables
library(ggplot2)
path <- "/Users/apple/UH-CPL/XD_Human_genom/Data_OSF" ## Path to data

## Uploading data 
## Merged Google Scholar and U.S. Funding data
data1 <- read.csv(paste(path,"/Faculty_GoogleScholar_Funding_Data_N4190.csv", sep=''),
                  header = TRUE)

PDF_and_median_scaled <- function(dv, xv, ## data variable, x-axis variable
                                  gv, XscaleMin, ## group variable
                                  XscaleMax, Xstep) {
    pv <- ggplot(data=dv,
                      aes(x=xv, y=..density.., group=gv, fill=gv)) +
        geom_density(alpha=0.2) +
        geom_vline(data=dv, aes(xintercept=mean(xv), 
                                     group=gv, colour=gv),
                   linetype="dashed", size=1) +
        scale_x_continuous(breaks = seq(XscaleMin, XscaleMax, Xstep))
    return(pv)
}

#PDF for min publ year
PDF_and_median_scaled(data1,data1$min_year,data1$XDIndicator,1960,2020,10)
#PDF for total collaboration degree
PDF_and_median_scaled(data1,data1$KTotal,data1$XDIndicator,0,2000,500)
#PDF for Cross-disciplinarity
PDF_and_median_scaled(data1,data1$Chi,data1$XDIndicator,0,1,0.2)
#PDF for Page rank centrality
PDF_and_median_scaled(data1,data1$PRCentrality*data1$t_publication,data1$XDIndicator,0,10,1)
#PDF for mean publication impact factor
PDF_and_median_scaled(data1,data1$mean_of_IF,data1$XDIndicator,0,25,5)
#PDF Total career citations log10
PDF_and_median_scaled(data1,log10(data1$t_pubs_citations),data1$XDIndicator,0,6,1)