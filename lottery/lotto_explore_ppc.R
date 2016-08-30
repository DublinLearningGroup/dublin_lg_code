rm(list=ls())
library(dplyr)
library(lubridate)
library(ggplot2)

lotto=read.delim("C:/Patrick/DataScience/Big Lottery Dataset.txt")
lotto.bkup=lotto
  
lotto$ANNOUNCEMENT_DATE=as.POSIXct(lotto$ANNOUNCEMENT_DATE,format ="%d/%m/%Y")
lotto$DECISION_DATE=as.POSIXct(lotto$DECISION_DATE,format ="%d/%m/%Y")
lotto$APPLICANT_START_DATE=as.POSIXct(lotto$APPLICANT_START_DATE,format ="%d/%m/%Y")



lotto$CURRENT_AWARD=as.numeric(gsub(",","",as.character(lotto$CURRENT_AWARD)))

lotto$Organisation.age=as.numeric(gsub(",","",as.character(lotto$Organisation.age)))
lotto$Funding.per.head=as.numeric(gsub(",","",as.character(lotto$Funding.per.head)))
lotto$census2011ks101_Student_home_population=as.numeric(gsub(",","",as.character(lotto$census2011ks101_Student_home_population)))
lotto$census2011ks101_Male_population=as.numeric(gsub(",","",as.character(lotto$census2011ks101_Male_population)))


lotto=lotto[-14] # remove keyword.filter
lotto=lotto[-35] # remove number.of.records
###finished pre cleaning ####



lotto %>% group_by(Council.or.School) %>% summarise(`25%`=quantile(CURRENT_AWARD, probs=0.25),
          `50%`=quantile(CURRENT_AWARD, probs=0.5),
          `75%`=quantile(CURRENT_AWARD, probs=0.75),
          `90%`=quantile(CURRENT_AWARD, probs=0.9),ct=n())

qplot(log(lotto$CURRENT_AWARD),fill=lotto$Council.or.School)
