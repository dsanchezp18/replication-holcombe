# Corruption by Countries Analysis (Latex Example)

# Loading database

df<-read.csv('cor1.csv', fileEncoding = 'UTF-8-BOM')

# Load some libraries

library(psych) # for descriptive stats (by group too)
library(openxlsx) # for exporting to xlsx
library(stargazer) # for latex tables
library(lmtest) # for heteroskedasticity robust estimation and others
library(car)
library(sandwich) # for more versatile robust estimation

# Descriptive Analysis of the dataset

summary_stats<-describe(df, na.rm=T) # simple summary stats, creates a dataframe
write.xlsx(summary_stats,'summary.stats.xlsx', rowNames=T) # writes an xlsx with data

sum_region<-describeBy(df$cci, df$region, na.rm=T) # summary stats by region
sum_region2<-do.call('rbind', sum_region) # convert to dataframe data type
write.xlsx(sum_region2, 'cpi_byregion.xlsx', rowNames=T)

# Use stargazer package for normal descriptive stats
stargazer(df, summary=T) # give a dataframe and it gives you descriptive stats

# To run regressions, build the log of GDP and population variables
df$lgdp_pc<-log(df$gdp_pc)
df$lpop<-log(df$pop)

# Replace infinite values for NAs
df$lgdp_pc[is.infinite(df$lgdp_pc)] <- NA
df$lpop[is.infinite(df$lpop)] <- NA
  
# Now run some simple models
reg1<-lm(cpi~lgdp_pc+lpop, data=df)
reg1h<-coeftest(reg1,vcov=hccm)

reg2<-lm(cpi~lgdp_pc+lpop+reg,data=df)
reg2h<-coeftest(reg2,vcov=hccm)

reg3<-lm(cpi~lgdp_pc+lpop+efw+govexp,data=df)
reg3h<-coeftest(reg3, vcov=hccm)

reg4<-lm(cpi~lgdp_pc+lpop+efw+govexp+nat+oil,data=df)
reg4h<-coeftest(reg4, vcov=hccm)


# Use stargazer package to export latex output for the models
stargazer(reg1)
# Manipulating title and column names
vars<-c('Log of GDP per capita', 'Log of Population', 'Economic Freedom of the World Index',
        'Government expenditure (% of GDP)')
stargazer(reg1, title='Corruption explained by political, social and economic variables',
          covariate.labels = vars, dep.var.labels = 'Corruption Perceptions Index')
# To report heteroskedasticty robust SE's, we first calculate them apart
cov1<-vcovHC(reg1, type='HC1')
robust1<-sqrt(diag(cov1))

# Now apply it to the ses in the normal stargazer


stargazer(reg1, title='Corruption explained by political, social and economic variables',
          covariate.labels = vars, dep.var.labels = 'Corruption Perceptions Index', 
          se=list(robust1))
# Use stargazer for many models
stargazer(reg1,reg2,reg3,reg4)
