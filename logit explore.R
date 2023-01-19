# Explore the dataframe and R packages through a simple estimation of logit/probit models

library(margins)
library(stargazer)
library(tidyverse)

# Create a dummy variable, 1 for relatively high income countries

corruption1$hinc<-ifelse(corruption1$inc_group == 'Upper middle income'| corruption1$region == 'High income', 1, 0)

# Dummy for america

corruption1$america<-ifelse(corruption1$region == 'Latin America & Caribbean'| corruption1$region == 'North America', 1, 0)

# Logit Probit models


log1<-glm(hinc~gdp_pc+reg, 
          data = corruption1, 
          family = binomial (link = 'logit'))
summary(log1)

prob1<-glm(hinc~gdp_pc+reg, 
          data = corruption1, 
          family = binomial (link = 'probit'))
summary(prob1)

stargazer(log1, prob1, type = 'text')

margins(log1) %>% summary()