#Analysis on mental health and climate
library(readxl)
mhd2 <- read_excel("Mental health data 2.xlsx")
View(mhd2)


library(plm)
library(stargazer)
library(gtsummary)

#This is a preliminary analysis between mental health and climate using panel regression 


#Fixed effects models
#Suicide and temperature
reg1 = plm(suicr ~ temp, index = c("country", "year"),
              model = "within", data = mhd2)
reg2 = plm(suicrm ~ temp, index = c("country", "year"),
           model = "within", data = mhd2)
reg3 = plm(suicrf ~ temp, index = c("country", "year"),
           model = "within", data = mhd2)
stargazer(reg1, reg2, reg3, type = "text")

#Suicide has a strong relation with temperature


#Suicide rate and gdp
reg4 = plm(suicr ~ gdp, index = c("country", "year"),
           model = "within", data = mhd2)
reg5 = plm(suicrm ~ gdp, index = c("country", "year"),
           model = "within", data = mhd2)
reg6 = plm(suicrf ~ gdp, index = c("country", "year"),
            model = "within", data = mhd2)
stargazer(reg4, reg5, reg6, type = "text")


#Suicide and inflation
reg7 = plm(suicr ~ inf, index = c("country", "year"),
            model = "within", data = mhd2)
reg8 = plm(suicrm ~ inf, index = c("country", "year"),
            model = "within", data = mhd2)
reg9 = plm(suicrf ~ inf, index = c("country", "year"),
            model = "within", data = mhd2)
stargazer(reg7, reg8, reg9, type = "text")



#Suicide and unemployment
reg10 = plm(suicr ~ unem, index = c("country", "year"),
           model = "within", data = mhd2)
reg11 = plm(suicrm ~ unem, index = c("country", "year"),
           model = "within", data = mhd2)
reg12 = plm(suicrf ~ unem, index = c("country", "year"),
           model = "within", data = mhd2)
stargazer(reg10, reg11, reg12, type = "text")


##Multivariate analysis

#Relation between suicide and all economic variables
reg13 = plm(suicr ~ gdp + unem, index = c("country", "year"),
            model = "within", data = mhd2)
reg14 = plm(suicrm ~ gdp + unem, index = c("country", "year"),
            model = "within", data = mhd2)
reg15 = plm(suicrf ~ gdp + unem, index = c("country", "year"),
            model = "within", data = mhd2)
stargazer(reg13, reg14, reg15, type = "text")


reg16 = plm(suicr ~ gdp + unem + inf, index = c("country", "year"),
            model = "within", data = mhd2)
reg17 = plm(suicrm ~ gdp + unem + inf, index = c("country", "year"),
            model = "within", data = mhd2)
reg18 = plm(suicrf ~ gdp + unem +inf, index = c("country", "year"),
            model = "within", data = mhd2)
stargazer(reg16, reg17, reg18, type = "text")


#Relation between suicide temperature and economic variables

reg19 = plm(suicr ~ temp + gdp, index = c("country", "year"),
            model = "within", data = mhd2)
reg20 = plm(suicrm ~ temp + gdp, index = c("country", "year"),
            model = "within", data = mhd2)
reg21 = plm(suicrf ~ temp + gdp, index = c("country", "year"),
            model = "within", data = mhd2)
stargazer(reg19, reg20, reg21, type = "text")



reg22 = plm(suicr ~ temp + unem, index = c("country", "year"),
           model = "within", data = mhd2)
reg23 = plm(suicrm ~ temp + unem, index = c("country", "year"),
            model = "within", data = mhd2)
reg24 = plm(suicrf ~ temp + unem, index = c("country", "year"),
            model = "within", data = mhd2)
stargazer(reg22, reg23, reg24, type = "text")


reg25 = plm(suicr ~  temp + gdp + unem, index = c("country", "year"),
           model = "within", data = mhd2)
reg26 = plm(suicrm ~  temp + gdp + unem, index = c("country", "year"),
           model = "within", data = mhd2)
reg27 = plm(suicrf ~  temp + gdp + unem, index = c("country", "year"),
            model = "within", data = mhd2)
stargazer(reg25, reg26, reg27, type= "text")


reg28 = plm(suicr ~  temp + gdp + unem + inf, index = c("country", "year"),
            model = "within", data = mhd2)
reg29 = plm(suicrm ~  temp + gdp + unem + inf, index = c("country", "year"),
            model = "within", data = mhd2)
reg30 = plm(suicrf ~  temp + gdp + unem + inf, index = c("country", "year"),
            model = "within", data = mhd2)
stargazer(reg28, reg29, reg30, type= "text")



#Interaction model
reg31 = plm(suicr ~ temp*gdp, index = c("country", "year"),
            model = "within", data = mhd2)
reg32 = plm(suicrm ~ temp*gdp, index = c("country", "year"),
            model = "within", data = mhd2)
reg33 = plm(suicrf ~ temp*gdp, index = c("country", "year"),
            model = "within", data = mhd2)
stargazer(reg31, reg32, reg33, type = "text")

reg34 = plm(suicr ~ temp + unem*gdp, index = c("country", "year"),
            model = "within", data = mhd2)
reg35 = plm(suicrm ~ temp + unem*gdp, index = c("country", "year"),
            model = "within", data = mhd2)
reg36 = plm(suicrf ~ temp + unem*gdp, index = c("country", "year"),
            model = "within", data = mhd2)
stargazer(reg34, reg35, reg36, type = "text")


## Temperature and suicide have a strong negative relation 
## meaning that an increase in temperatures leads to a decreasse in suicide rates

## GDP also has a negative relation with suicide

## Unemployment may not be an effective indicator due to how it is estimated
## in different countries, sometimes prople are considered unemployed while in other countries
## they may be counted as economically inactive 
## Some countries have near 0 unemployment which in principle shouldn't be possible
## as in normal economic conditions there is a certain level of frictional unemployment

## Economic variables have a stronger effect on men's suicide than
## women's suicide and it's R2 is twice as high




