## Paper "Cross-disciplinary evolution of the genomics revolution"
## Fig 4
## Research reproduced by: Vitalii Z.
## State: In progress

## Loading libraries, defining variables
library(ggplot2)
library(data.table)
library(splitstackshape)
library(sjstats)
library(arm)
library(openintro)

path <- "/Users/apple/UH-CPL/XD_Human_genom/Data_OSF" ## Path to data

## Uploading data 
## Merged Google Scholar and U.S. Funding data
data1 <- read.csv(paste(path,"/Faculty_GoogleScholar_Funding_Data_N4190.csv", sep=''),
                  header = TRUE)

data1<-data.table(data1)
data1[Chi==0,Chi:=0.001]
data1[PRCentrality==0,PRCentrality:=0.0001]

## Linear model
## Natural logarithm is only defined for x>0, log1p instead of log

## Linear model for CV data
mdl_cv <- lm(log1p(t_pubs_citations)~
                 log1p(SchoolRank)+ #Br 
                 log1p(h_index)+ #Bh
                 log1p(t_deflated_nsf)+ #B$1
                 log1p(num_nsf)+ #Bn1
                 log1p(t_deflated_nih)+ #B$2
                 log1p(num_nih)+ #Bn2
                 #log(PRCentrality)+ #Bc
                 #log(Chi)+ #Bx
                 factor(XDIndicator)+
                 factor(Y05yr),
             na.action=na.exclude, data=data1)

summary(mdl_cv)
summary(mdl_cv)$coefficients[,2]

coef.vect <- coef(mdl_cv)[c(2,4,5,6,7,8,9)]
sd.vect <- summary(mdl_cv)$coefficients[,2][c(2,4,5,6,7,8,9)]
sign.vect <- summary(mdl_cv)$coefficients[,4][c(2,4,5,6,7,8,9)]

longnames <- c("Br","B$1","Bn1","B$2","Bn2","Bc","Bx") #names(coef(mdl_cv)[2:9])

coefplot (coef.vect, sd.vect, varnames=longnames, vertical=FALSE,
          main="Standardized regression coefficents \n point estimate with 95% confidence interval",
          ylim=c(-0.13, 0.13),
          font.main=1,
          adj = 0.5, line = 1,
          cex.pts=1.3,
          var.las=1, cex.var=1)
Braces(3, 0.10, face.radians = 3 * pi / 2, long = 4.5, short = 0.02)
Braces(6.5, 0.10, face.radians = 3 * pi / 2, long = 1.75, short = 0.02)
text(x = 3, y = 0.12, 'CV', srt = 0, cex = 1)
text(x = 6.5, y = 0.12, 'Network', srt = 0, cex = 1)


## Linear model for combined CV+Network data
data5 <- data1[KDirect !=0]

mdl_cv2 <- lm(log1p(t_pubs_citations)~
                 log1p(SchoolRank)+ #Br 
                 log1p(h_index)+ #Bh
                 log1p(t_deflated_nsf)+ #B$1
                 log1p(num_nsf)+ #Bn1
                 log1p(t_deflated_nih)+ #B$2
                 log1p(num_nih)+ #Bn2
                 log(PRCentrality)+ #Bc
                 log(Chi)+ #Bx
                 factor(XDIndicator)+
                 factor(Y05yr),
                 na.action=na.exclude, data=data5)
summary(mdl_cv2)


