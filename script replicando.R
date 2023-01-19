#Regulation and corruption (2015) Holcombe & Boudreaux


# Preelimininaries ---------------------------------------------------------
# Loading packages

library(car) #for heteroskedasticity robust errors and hypothesis testing
library(lmtest) #for more on heteroskedasticity, coeftest command, testing for htkd
library(dplyr) #for the filter command as well as the select one
# Inserting the database
library(ggplot2) # for graphics

corruption1<-read.csv('cor1.csv')

# Descriptive Stats -------------------------------------------------------

mean_cpi<-mean(corruption1$cpi,na.rm = T)
mean_reg<-mean(corruption1$reg,na.rm=T)
mean_cpi<-mean(corruption1$govexp,na.rm = T)

scandinavia_countries<-filter(corruption1, corruption1$scandinavia==1)
n_scand<-nrow(scandinavia_countries)

presidential<-filter(corruption1, corruption1$pres==1)
n_scand<-nrow(presidential) # seems to be a lot of missing countries

sd_cpi<-sd(corruption1$cpi,na.rm = T)
sd_reg<-sd(corruption1$reg,na.rm=T)
sd_cpi<-sd(corruption1$govexp,na.rm = T)
          
# First Regression: The Scandinavian Factor -------------------------------

# Estimate regression CPI~scand
reg_scand<-lm(cpi~scandinavia,data=corruption1)
summary(reg_scand)
coeftest(reg_scand,vcov=hccm)
# Results show that the CPI used is inverted compared to the one in the Holcombe Paper
# Less is explained compared to the paper
#Results are heteroskedasticity robust

# Second Regression: dividing government size -----------------------------

# Estimate regression CPI~reg+govexp
reg_govsize<-lm(cpi~reg+govexp,data=corruption1)
summary(reg_govsize)
coeftest(reg_govsize,vcov=hccm)

# The results from the paper are preserved if the reg variable really does mean
# that more freedom is a higher varaible. The efects from government expenditure are small
# Government expenditure is not as significant with White errors

# Third Regression CPI~reg+govexp+scandinavia -----------------------------

reg_scg<-lm(cpi~scandinavia+reg+govexp,data=corruption1)
summary(reg_scg)
coeftest(reg_scg,vcov=hccm)

# Results preserved, scandinavia still less corrupt even when accounting for the reg
# and the govexp variables. 
# The same thing happens with govexp with the White errors


# Fourth Regression: Accounting for Parliamentary Democracies -------------

reg_parl<-lm(cpi~pres,data=corruption1)
summary(reg_parl)
coeftest(reg_parl,vcov=hccm)

#Results are similar, presidential democracies tend to be more corrupt.
#Results are heteroskedasticity robust

# Fifth Regression: Parliamentary and others ------------------------------

reg_parlet<-lm(cpi~pres+govexp+reg,data=corruption1)
summary(reg_parlet)
coeftest(reg_parlet,vcov=hccm)

# Presidential democracies no longer significant when accounting for govsize
# Results are heteroskedasticity robust


# Sixth Regression: Scandinavia with parliamentary ------------------------
reg_parlscan<-lm(cpi~pres+scandinavia,data=corruption1)
summary(reg_parlscan)
coeftest(reg_parlscan,vcov=hccm)

# Results confirm paper results, heteroskedasticity robust and strengthens significance

# Seventh Regression: Scandinavia with others -----------------------------

reg_scan1<-lm(cpi~pres+scandinavia+govexp+reg,data=corruption1)
summary(reg_scan1)
coeftest(reg_scan1,vcov=hccm)

# Results mostly consistent although the presidential democracy is no longer significant. 
# Perhaps due to missing data. 


# Practical Effect Analysis for first 4 variables -------------------------

beta_pres<-coef(reg_scan1)['pres']
beta_scandinavia<-coef(reg_scan1)['scandinavia']
beta_govexp<-coef(reg_scan1)['govexp']
beta_reg<-coef(reg_scan1)['reg']

# practical effect of regulation is somewhat significative. 60% of a std dev
# in the CPI index


# Further regressions: adding ceteris paribus variables -------------------

# Create UK colonizer variable
corruption1$col_uk<-ifelse(corruption1$col=="GBR",1,0)

# Create log variables for dollar amounts and number of people
corruption1$lgdp_pc<-log(corruption1$gdp_pc)
corruption1$lpop<-log(corruption1$pop)
corruption1$lfor<-log(corruption1$foraid)

# Now delete the infinite values from the column we computed ln's, by setting as NA
corruption1$lgdp_pc[is.infinite(corruption1$lgdp_pc)] <- NA
corruption1$lpop[is.infinite(corruption1$lpop)] <- NA
corruption1$lfor[is.infinite(corruption1$lfor)] <- NA

#Run regression (1) from the paper, without foreign aid however. 

reg_1<-lm(cpi~scandinavia+agedem+prot+col_uk+lgdp_pc+reg+govexp+lpop+nat,data=corruption1)
summary(reg_1)
coeftest(reg_1,vcov=hccm)

#results somewhat consistent; scandinavia no longer significant yet could be 
#biased toward 0. 
# agedem very significant. similar small sign tho. protestan somewhat
# uk colony similar, not significant. Results are heteroskedasticity robust
# better r^2

# Run reg 2
reg_2<-lm(cpi~scandinavia+agedem+prot+col_uk+lgdp_pc+reg+govexp+lpop+nat+pres,data=corruption1)
summary(reg_2)
coeftest(reg_2,vcov=hccm)

#adding the pres democracy variable changes things terribly. because too little data. 
# however we have a higher R^2; lookout for stuff like this

# Run reg 3 

reg_3<-lm(cpi~scandinavia+agedem+prot+col_uk+lgdp_pc+reg+govexp+lpop+nat+legint,data=corruption1)
summary(reg_3)
coeftest(reg_3,vcov=hccm)

# legal integrity seems to be a better predictor of corruption than regulation. 
#perhaps they are very correlated. lookout. We could've been biasing reg upward

cor_legintreg<-cor(corruption1$reg,corruption1$legint, use="complete.obs")

#indeed heavily correlated. 

#Now run all regressions with CCI

reg_1a<-lm(cci~scandinavia+agedem+prot+col_uk+lgdp_pc+reg+govexp+lpop+nat,data=corruption1)
summary(reg_1a)
coeftest(reg_1a,vcov=hccm)

reg_2a<-lm(cci~scandinavia+agedem+prot+col_uk+lgdp_pc+reg+govexp+lpop+nat+pres,data=corruption1)
summary(reg_2a)
coeftest(reg_2a,vcov=hccm)

reg_3a<-lm(cci~scandinavia+agedem+prot+col_uk+lgdp_pc+reg+govexp+lpop+nat+legint,data=corruption1)
summary(reg_3a)
coeftest(reg_3a,vcov=hccm)

# Now some of our own

reg_3aa<-lm(cci~scandinavia+agedem+prot+col_uk+lgdp_pc+reg+govexp+lpop+nat+legint+oil+min,data=corruption1)
summary(reg_3a)
coeftest(reg_3a,vcov=hccm)

reg_3ab<-lm(cpi~scandinavia+agedem+prot+col_uk+lgdp_pc+reg+govexp+lpop+nat+legint,data=corruption1)
summary(reg_3a)
coeftest(reg_3a,vcov=hccm)

# natural resources seems to increase corruption when measured with cpi. 


# most results seem to be heteroskedasticity robust. lets do a test
bptest(reg_3aa)
bptest(reg_3ab)
bptest(reg_3aa, ~fitted(reg_3aa)+I(fitted(reg_3aa)^2))
bptest(reg_3ab, ~fitted(reg_3ab)+I(fitted(reg_3ab)^2))

# Testing out 

