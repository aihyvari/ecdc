rm(list=ls())

library(dplyr)
library(readxl)
library(utils)
require(reshape)
#require(GA)
require(ggplot2)

#read the Dataset sheet into "R". The dataset will be called "data".
data <- read.csv("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv", 
                 na.strings = "", fileEncoding = "UTF-8-BOM")
data$dateRep<-as.Date(data$dateRep, format="%d/%m/%Y")
##################################################
data<-data[as.Date(data$dateRep)>as.Date("2020-03-01"),]
##
table(data$countriesAndTerritories)
EU<- data %>% filter(countriesAndTerritories %in% c("Finland","France", "Belgium","Spain", #"Estonia",
                                                  "Germany","Italy" ,"Austria"
))
ggplot(EU, aes(dateRep, Cumulative_number_for_14_days_of_COVID.19_cases_per_100000, 
                  colour=factor(countriesAndTerritories))) + 
  #geom_line()+
  geom_smooth(method=loess, se=FALSE)+
  scale_y_continuous(limits = c(0, NA))+
  geom_hline(yintercept=10, linetype="dashed", color = "red")+
  labs(x = "", y= "Kumul. 14 vrk tapauksia per 100 000", colour="Maa")

######################################################################
pmaat<- data %>% filter(countriesAndTerritories %in% c("Finland", "Sweden", "Norway", "Denmark" )) #, "Estonia"))
pmaat$tapPer100<-pmaat$cases/(pmaat$popData2019/100000) 

ggplot(pmaat, aes(dateRep, tapPer100, colour=factor(countriesAndTerritories))) + 
    #geom_line()+
    geom_smooth(method=loess, se=FALSE)+
    scale_y_continuous(limits = c(0, NA))+
    labs(x = "", y= "Tapauksia per 100 000", colour="Maa")

colnames(pmaat)
ggplot(pmaat, aes(dateRep, Cumulative_number_for_14_days_of_COVID.19_cases_per_100000, 
                  colour=factor(countriesAndTerritories))) + 
  geom_line()+
  geom_smooth(method=loess, se=TRUE)+
  scale_y_continuous(limits = c(0, NA))+
  geom_hline(yintercept=8, linetype="dashed", color = "red")+
  labs(x = "", y= "Kumul. 14 vrk tapauksia per 100 000", colour="Maa")
###############################################################################
#Yli Suomen matkustusrajan
df2<-data[as.Date(data$dateRep)>as.Date("2020-07-01"),]
df2<-df2[df2$continentExp=="Europe",]
yli<-df2[as.Date(df2$dateRep)==as.Date("2020-08-01") #,]
          & df2$Cumulative_number_for_14_days_of_COVID.19_cases_per_100000>10,]
ei_kiin<-c("Albania", "Andorra", "Armenia", "Azerbaijan",
           "Faroe_Islands", "Gibraltar", "Kosovo",
           "Malta", "Monaco", "Montenegro", "North_Makedonia")
yli<-yli[!(yli$countriesAndTerritories %in% ei_kiin),]


df<-df2[df2$countryterritoryCode %in% yli$countryterritoryCode,]

ggplot(df, aes(dateRep, Cumulative_number_for_14_days_of_COVID.19_cases_per_100000, 
                  colour=factor(countriesAndTerritories))) + 
  #geom_line()+
  geom_smooth(method=loess, se=FALSE)+
  scale_y_continuous(limits = c(0, NA))+
  geom_hline(yintercept=8, linetype="dashed", color = "black")+
  labs(x = "", y= "Kumul. 14 vrk tapauksia per 100 000", colour="Maa")

#mitka maat alittaa 1-8-2020
ali<-df2[as.Date(df2$dateRep)==as.Date("2020-08-01") #,]
         & df2$Cumulative_number_for_14_days_of_COVID.19_cases_per_100000<10,]

table(ali$countriesAndTerritories)


#######################################################

maat<-unique(df$countriesAndTerritories)
#yksi<-maat[1:8]
#yksi<-maat[9:16]
yksi<-maat[17:length(maat)]


tmp<-df2[df2$countriesAndTerritories %in% yksi,]
ggplot(tmp, aes(dateRep, Cumulative_number_for_14_days_of_COVID.19_cases_per_100000, 
               colour=factor(countriesAndTerritories))) + 
  #geom_line()+
  geom_smooth(method=loess, se=FALSE)+
  scale_y_continuous(limits = c(0, NA))+
  geom_hline(yintercept=8, linetype="dashed", color = "black")+
  labs(x = "", y= "Kumul. 14 vrk tapauksia per 100 000", colour="Maa")

##########################################################3

sol=lm(Cumulative_number_for_14_days_of_COVID.19_cases_per_100000~dateRep, data=df2)
summary(sol)
library(plyr)
# Break up d by state, then fit the specified model to each piece and
# return a list
models <- dlply(df2, "countriesAndTerritories", function(df) 
  lm(Cumulative_number_for_14_days_of_COVID.19_cases_per_100000 ~ dateRep, data = df))

ldply(models, coef)

#l_ply(models, summary, .print = TRUE)

fin<-data[data$countriesAndTerritories=="Finland",]
ggplot(fin, aes(dateRep, cases))+
  geom_line()+
  geom_smooth(method=loess, se=FALSE)+
  scale_y_continuous(limits = c(0, NA))+
  geom_hline(yintercept=32, linetype="dashed", color = "black")   +
  labs(x = "", y= "Tapauksia per pv")# , colour="Maa")
  
  13*8/14

tmp<-data[data$countriesAndTerritories=="Sweden",]
tmp$Cumulative_number_for_14_days_of_COVID.19_cases_per_100000


cas<-tmp[1:14,]
pop<-tmp$popData2019[1]/100000
sum(cas$cases)/pop

