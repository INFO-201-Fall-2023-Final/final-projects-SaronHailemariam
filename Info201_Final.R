library(dplyr)
library(stringr)
library(ggplot2)
prescription_rates_df<- read.csv("Medicaid Opioid Prescribing Rates by Geography.csv") 
overdose_df<-read.csv("NCHS_-_Drug_Poisoning_Mortality_by_County__United_States.csv") 

#Adds a new column named State in the prescription_rates_df
prescription_rates_df$State<-prescription_rates_df$Geo_Desc

#Cleans datasets so that they can be joined
prescription_rates_df<-filter(prescription_rates_df,!is.na(Opioid_Prscrbng_Rate))
prescription_rates_df<-filter(prescription_rates_df,!is.na(Opioid_Prscrbng_Rate_1Y_Chg))
prescription_rates_df<-filter(prescription_rates_df,!is.na(Opioid_Prscrbng_Rate_5Y_Chg))

grouped_prescription<-group_by(prescription_rates_df, State, Year)
grouped_prescription_rates_df<-summarise(
  grouped_prescription, 
  sum_population=sum(Tot_Clms), 
  sum_population=sum(Tot_Opioid_Clms),
  mean_Opioid_Prscrbng_Rate=mean(Opioid_Prscrbng_Rate),
  mean_Opioid_Prscrbng_Rate_1Y_Chg=mean(Opioid_Prscrbng_Rate_1Y_Chg),
  meanOpioid_Prscrbng_Rate_5Y_Chg=mean(Opioid_Prscrbng_Rate_5Y_Chg))


overdose_df<-filter(overdose_df,!is.na(Population))
grouped_overdose<-group_by(overdose_df, State, Year)
grouped_overdose_df<-summarise(
  grouped_overdose, 
  mean_Model.based.Death.Rate=mean(Model.based.Death.Rate),
  mean_Standard.Deviation=mean(Standard.Deviation),
  mean_Lower.Confidence.Limit=mean(Lower.Confidence.Limit),
  mean_Upper.Confidence.Limit=mean(Upper.Confidence.Limit))



#Joins the datasets
df<-merge(grouped_overdose_df,grouped_prescription_rates_df,by=c("State", "Year"))

#Evalutes if a state is high risk for drug overdose per year and return 
#TRUE if it is and false if not. The criteria for a high risk state 
#is if the death rate is greater than national average. NEW CATEGORIAL VARIABLE
risk_state<-function(state, year){
  National_death_rate<-mean(df$mean_Model.based.Death.Rate)
  st_gp<-group_by(df,State, Year)
  st<-summarise(
    st_gp,
    mean_Model.based.Death.Rate_st=mean(mean_Model.based.Death.Rate))
  
  state_data <- filter(st, State == state & Year==year)
  if(state_data$mean_Model.based.Death.Rate_st>=National_death_rate){
    return(TRUE)
  }else {
    return(FALSE)
  }
}

#Adds a column to df using the risk_state function, stores TRUE/FAlSE
df$high_overdose_death_rate<-mapply(risk_state, df$State, df$Year)

#makes summarization data frames first one by state and second one by year 
by_state<-group_by(df,State)
rates_by_state<-summarize(
  by_state, 
  avg_opioid_prscrbng_rate_state=mean(mean_Opioid_Prscrbng_Rate),
  avg_overdose_death_rate_state=mean(mean_Model.based.Death.Rate),
  avg_Opioid_Prscrbng_Rate_1Y_Chg_state=mean(mean_Opioid_Prscrbng_Rate_1Y_Chg))

by_year<-group_by(df,Year)
rates_by_Year<-summarise(
  by_year, 
  avg_opioid_prscrbng_rate_yr=mean(mean_Opioid_Prscrbng_Rate),
  avg_overdose_death_rate_yr=mean(mean_Model.based.Death.Rate),
  avg_Opioid_Prscrbng_Rate_1Y_Chg_state=mean(mean_Opioid_Prscrbng_Rate_1Y_Chg))


