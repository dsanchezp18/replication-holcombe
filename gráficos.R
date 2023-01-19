# Regulation and Corruption (2015)
# Graphs for data analysis

# Load libraries

library(ggplot2) # for graphics


# Import database

corruption1<-read.csv('cor1.csv', fileEncoding="UTF-8-BOM")



# Simple Graphics from Holcombe Estimations---------------------------------

# Scatter Plot CPI and EFW index

efw_cpi<-ggplot(corruption1, aes(x=efw,y=cpi))+
        geom_point()
efw_cpi+geom_smooth(method=lm, se=F)+xlab('Economic Freedom of the World Index')+
        ylab('Corruption Perceptions Index')
# Estimate the SLR model to add information to the graph
reg_efwcpi<-lm(cpi~efw, data=corruption1)
summary(model)

# Scatter Plot LNGDP vs EFW (using lngdpc from the replicando script)
lgdp_cpi<-ggplot(corruption1, aes(x=lgdp_pc, y=cpi))+geom_point()
lgdp_cpi+geom_smooth(method=lm, se=F)+xlab('Log of GDP per capita (2017 PPP Dollars)')+
  ylab('Corruption Perceptions Index')

# Bar plot CPI by region (world bank)
cpi_region<-ggplot(corruption1, aes(x=region,y=cpi))+
            geom_col()

cpi_region+geom_smooth(method=lm, se=F)

# drop taiwan from our analysis
corruption2<-corruption1[-143,]

# Bar plot CPI by income group (world bank)
 
cpi_incg<-ggplot(corruption2, aes(x=inc_group,y=cpi))+
  geom_col()
cpi_incg

# needs to change the labels so it can be seen better

# A histogram of the CPI value
cpi_hist<-ggplot(corruption1, aes(x=cpi))+
          geom_histogram(binwidth=2)


