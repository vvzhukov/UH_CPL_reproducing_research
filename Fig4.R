## Paper "Cross-disciplinary evolution of the genomics revolution"
## Fig 4, Table S2, Table S3

## State: In progress

## Loading libraries, defining variables
library(ggplot2)
library(data.table)
library(splitstackshape)
library(sjstats)
library(arm)
library(openintro)
library(dplyr)
library(estimatr)
library(knitr)
path <- "/Users/apple/UH-CPL/XD_Human_genom/Data_OSF/" ## Path to data

## Uploading data 
## Merged Google Scholar and U.S. Funding data
data1 <- read.csv(paste(path,"Faculty_GoogleScholar_Funding_Data_N4190.csv", sep=''),
                  header = TRUE)

data1<-data.table(data1)
data1[Chi==0,Chi:=0.001]
data1[PRCentrality==0,PRCentrality:=0.0001]
data1[BetCentrality==0,BetCentrality:=0.0001]

## Linear model
## Natural logarithm is only defined for x>0, log1p instead of log


#####################################################################################################################
## Linear model for CV data
mdl_cv <- lm(log(t_pubs_citations)~
                 log(SchoolRank)+ #Br 
                 log(h_index)+ #Bh
                 log1p(t_deflated_nsf)+ #B$1
                 log1p(num_nsf)+ #Bn1
                 log1p(t_deflated_nih)+ #B$2
                 log1p(num_nih)+ #Bn2
                 factor(XDIndicator)+
                 factor(Y05yr),
             na.action=na.exclude, data=data1)

#summary(mdl_cv)

## regression model - CV only with clustered sandwich estimator
### use this model for creating table since it gives better Error estimates
lnmdl_cv <- lm_robust(log(t_pubs_citations)~log(SchoolRank)+
                          log(h_index)+
                          log1p(t_deflated_nsf)+
                          log1p(num_nsf)+
                          log1p(t_deflated_nih)+
                          log1p(num_nih)+
                          factor(XDIndicator)+
                          factor(Y05yr),
                      data=data1,
                      se_type ="HC1",
                      alpha = 0.05)
mdl_2a<-lnmdl_cv
#summary(lnmdl_cv)
#summary(mdl_cv)$coefficients[,2]

#####################################################################################################################
## Linear model for combined CV+Network data
data5 <- data1[KDirect !=0]
lm_
mdl_cv2 <- lm(log(t_pubs_citations)~
                  log(SchoolRank)+ #Br 
                  log(h_index)+ #Bh
                  log1p(t_deflated_nsf)+ #B$1
                  log1p(num_nsf)+ #Bn1
                  log1p(t_deflated_nih)+ #B$2
                  log1p(num_nih)+ #Bn2
                  log(PRCentrality)+ #Bc
                  Chi+ #Bx
                  factor(XDIndicator)+
                  factor(Y05yr),
              na.action=na.exclude, data=data5)
#summary(mdl_cv2)
## CV + NW with clustered sandwich estimator
### use this model for creating table since it gives better Error estimates

lnmdl_cv_nw_robust <- lm_robust(log(t_pubs_citations)~log(SchoolRank)+
                                    log(h_index)+
                                    log1p(t_deflated_nsf)+
                                    log1p(num_nsf)+
                                    log1p(t_deflated_nih)+
                                    log1p(num_nih)+
                                    log(PRCentrality)+
                                    Chi+
                                    factor(XDIndicator)+
                                    factor(Y05yr),
                                data=data5,
                                se_type ="HC3",
                                alpha = 0.05)
#summary(lnmdl_cv_nw_robust)
mdl_2b<-lnmdl_cv_nw_robust

#####################################################################################################################
## CV + Network [Normalized] - Model 3

mdl_cv_norm <- std_beta(mdl_cv2)
mdl_2c <- lm(scale(log(t_pubs_citations))~
                      scale(log(SchoolRank))+ #Br 
                      scale(log(h_index))+ #Bh
                      scale(log1p(t_deflated_nsf))+ #B$1
                                scale(log1p(num_nsf))+ #Bn1
                                          scale(log1p(t_deflated_nih))+ #B$2
                                                    scale(log1p(num_nih))+ #Bn2
                                                              scale(log(PRCentrality))+ #Bc
                                                                        scale(Chi)+ #Bx
                      factor(XDIndicator)+
                      factor(Y05yr),
                  na.action=na.exclude, data=data5)
names(mdl_2c$coefficients) <- c("(Intercept)",                  "log(SchoolRank)",       "log(h_index)",          "log1p(t_deflated_nsf)", "log1p(num_nsf)",       
                                "log1p(t_deflated_nih)", "log1p(num_nih)",        "log(PRCentrality)",     "Chi",                   "factor(XDIndicator)CS",       
                                "factor(XDIndicator)XD",        "factor(Y05yr)1950",            "factor(Y05yr)1955",            "factor(Y05yr)1960",            "factor(Y05yr)1965",           
                                "factor(Y05yr)1970",            "factor(Y05yr)1975",            "factor(Y05yr)1980",            "factor(Y05yr)1985",            "factor(Y05yr)1990",           
                                "factor(Y05yr)1995",            "factor(Y05yr)2000",            "factor(Y05yr)2005",            "factor(Y05yr)2010"   )
#####################################################################################################################
#####################################################################################################################
#Fig 4#

#####################################################################################################################

## Fig 4 based on model 3_v1

mdl_cv3_v1 <- std_beta(mdl_cv2)
coef.vect1 <- mdl_cv3_v1$std.estimate[c(1,3,4,5,6,7,8)]
sd.vect1 <- mdl_cv3_v1$std.error[c(1,3,4,5,6,7,8)]

longnames <- c("Br","B$1","Bn1","B$2","Bn2","Bc","Bx") #names(coef(mdl_cv)[2:9])

coef_plot <- coefplot (coef.vect1, sd.vect1, varnames=longnames, vertical=FALSE,
                       main="Standardized regression coefficents \n point estimate with 95% confidence interval",
                       ylim=c(-0.12, 0.12),
                       font.main=1,
                       adj = 0.5, line = 1,
                       cex.pts=1.3,
                       var.las=1, cex.var=1)
Braces(3, 0.11, face.radians = 3 * pi / 2, long = 4.5, short = 0.02)
Braces(6.5, 0.11, face.radians = 3 * pi / 2, long = 1.75, short = 0.02)
text(x = 3, y = 0.125, 'CV', srt = 0, cex = 1)
text(x = 6.5, y = 0.125, 'Network', srt = 0, cex = 1)
text(x = 1, y = 0.12, "***",srt = 0, cex = 1 )
text(x = 4, y = 0.12, "***",srt = 0, cex = 1 )
text(x = 5, y = 0.12, "***",srt = 0, cex = 1 )
text(x = 7, y = 0.12, "***",srt = 0, cex = 1 )
box(which = "plot",lty = "solid" )


#####################################################################################################################
#TABLE S2#
#####################################################################################################################

library(sjPlot)
library(sjmisc)
library(sjlabelled)

v_terms <- c("log(SchoolRank)", 
             "log(h_index)", 
             "log1p(t_deflated_nsf)",
             "log1p(num_nsf)",
             "log1p(t_deflated_nih)",
             "log1p(num_nih)",
             "log(PRCentrality)",
             "Chi",
             "(Intercept)")


pl <- c("Constant",
        "Departmental rank",
        "Productivity (h-index)",
        "Total NSF funding",
        "# of NSF grants",
        "Total NIH funding",
        "# of NIH grants",
        "PageRank centrality",
        "Cross-disciplinarity"
)


tab_model(mdl_2a, mdl_2b, mdl_2c ,dv.labels = c("CV", "CV + Network", "CV + Network [Standardized]"), 
          show.ci=FALSE, show.se=TRUE, digits=3,digits.p=3,pred.labels = pl, terms = v_terms,
          order.terms = c(2,3,4,5,6,7,8,9,1),CSS = list(css.modelcolumn3 = 'background-color: lightgrey;',
                                                        css.modelcolumn1 = 'background-color: lightgrey;'))


#####################################################################################################################
#TABLE S3#
#####################################################################################################################

## Table S3 - model A is same as previous model
lnmdl_cv_nw_robust <- lm_robust(log(t_pubs_citations)~log(SchoolRank)+
                                    log(h_index)+
                                    log1p(t_deflated_nsf)+
                                    log1p(num_nsf)+
                                    log1p(t_deflated_nih)+
                                    log1p(num_nih)+
                                    log(PRCentrality)+
                                    Chi+
                                    factor(XDIndicator)+
                                    factor(Y05yr),
                                data=data5,
                                se_type ="HC3",
                                alpha = 0.05)
mdl_3a <- lnmdl_cv_nw_robust


#####################################################################################################################

## Table S3 - model B

data5[t_deflated_nsf==0,t_deflated_nsf:=1]
data5[t_deflated_nih==0,t_deflated_nih:=1]
data5[num_nsf==0,num_nsf:=1]
data5[num_nih==0,num_nih:=1]

#removing entries with extremely small Betweenes centrality
data6 <- data5[BetCentrality > 0.000100]

mdl_cv4B <- lm_robust(log(t_pubs_citations)~
                          log(SchoolRank)+ #Br 
                          log(h_index)+ #Bh
                          log1p(t_deflated_nsf)+ #B$1
                          log1p(num_nsf)+ #Bn1
                          log1p(t_deflated_nih)+ #B$2
                          log1p(num_nih)+ #Bn2
                          log(BetCentrality)+ #Bc
                          Chi+ #Bx
                          factor(XDIndicator)+
                          factor(Y05yr),
                      data=data6,
                      se_type ="HC3",
                      alpha = 0.05)
#summary(mdl_cv4B)
mdl_3b <- mdl_cv4B
#####################################################################################################################

##Table S3 - model C
mdl_cv4C <- lm_robust(log(t_pubs_citations)~
                          log(SchoolRank)+ #Br 
                          log(h_index)+ #Bh
                          log1p(t_deflated_nsf)+ #B$1
                          log1p(num_nsf)+ 
                          log1p(t_deflated_nih)+ #B$2
                          log1p(num_nih)+
                          log(KDirect)+ #Bc
                          Chi+ #Bx
                          factor(XDIndicator)+
                          factor(Y05yr),
                      se_type ="HC3",
                      alpha = 0.05, data=data5)
#summary(mdl_cv4C)
mdl_3c <- mdl_cv4C
#####################################################################################################################

##Table S3 - model D
mdl_cv4D <- lm_robust(log(t_pubs_citations)~
                          log(SchoolRank)+ #Br 
                          log(h_index)+ #Bh
                          log1p(t_deflated_nsf)+ #B$1
                          log1p(t_deflated_nih)+ #B$2
                          log(PRCentrality)+ #Bc
                          Chi+ #Bx
                          factor(XDIndicator)+
                          factor(Y05yr),
                      se_type ="HC3",
                      alpha = 0.05, data=data5)
#summary(mdl_cv4D)
mdl_3d <- mdl_cv4D
#####################################################################################################################

##Table S3 - model E
mdl_cv4E <- lm_robust(log(t_pubs_citations)~
                          log(h_index)+ #Bh
                          log1p(t_deflated_nsf)+ #B$1
                          log1p(num_nsf)+ #Bn1
                          log1p(t_deflated_nih)+ #B$2
                          log1p(num_nih)+ #Bn2
                          log(PRCentrality)+ #Bc
                          Chi+ #Bx
                          factor(XDIndicator)+
                          factor(Y05yr),
                      se_type ="HC3",
                      alpha = 0.05, data=data5)
#summary(mdl_cv4E)
mdl_3e <- mdl_cv4E

#####################################################################################################################
#####################################################################################################################


summary(mdl_3a)
summary(mdl_3b)
summary(mdl_3c)
summary(mdl_3d)
summary(mdl_3e)
summary(mdl_3a)

library(sjPlot)
library(sjmisc)
library(sjlabelled)

v_terms2 <- c("log(SchoolRank)", 
             "log(h_index)", 
             "log1p(t_deflated_nsf)",
             "log1p(num_nsf)",
             "log1p(t_deflated_nih)",
             "log1p(num_nih)",
             "log(PRCentrality)",
             "log(BetCentrality)",
             "log(KDirect)",
             "Chi",
             "(Intercept)")


pl2 <- c("Constant",
        "Departmental rank",
        "Productivity (h-index)",
        "Total NSF funding",
        "# of NSF grants",
        "Total NIH funding",
        "# of NIH grants",
        "PageRank centrality",
        "Cross-disciplinarity",
        "Betweeness centrality",
        "Degree centrality"
)

tab_model(mdl_3a, mdl_3b, mdl_3c, mdl_3d, mdl_3e, dv.labels = c("A. [PR]","B. [B]","C. [D]","D. [-Bn1,-Bn2]","E. [-Br]"), show.ci=FALSE, show.se=TRUE,pred.labels = pl2, terms = v_terms2,
          order.terms = c(2,3,4,5,6,7,8,10,11,9,1), digits=3,digits.p=3,CSS = list(css.modelcolumn5 = 'background-color: lightgrey;',
                                                                                   css.modelcolumn3 = 'background-color: lightgrey;',
                                                                                   css.modelcolumn1 = 'background-color: lightgrey;'))


