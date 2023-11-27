library(dplyr)
library(stringr)
library(ggplot2)
prescription_rates_df<- read.csv("Medicaid Opioid Prescribing Rates by Geography.csv") 
overdose_df<-read.csv("NCHS_-_Drug_Poisoning_Mortality_by_County__United_States.csv") 

#Data Joining
#You fist need to to create a unified dataset (i.e. you need to join your datasets together). 
#This means that the records in your two datasets need to be related some how, either by a 
#shared key or a combination of fields. For example, say you are trying to calculate the 
#relationship between SAT scores and poverty rate and you have two data sets:
  #1) "School SAT Scores.csv" that has the columns "school_ID" and "SAT score"
  #2) "Free Lunch.csv" that has the columns "free lunch eligibility ratio" and "school ID"
#These two datasets can be joined together using the "school ID" column to create one unified 
#dataset that contains columns from both datasets. 
#Data Cleaning & Augmentation
#Once you have created your joined dataset, you should then make sure your dataset is clean and usable.
#i.e. make sure your columns properly formatted as a single CSV file that can be easily readable by R. 
#Make sure your combined dataset doesn't have more than roughly 25,000 rows -- if you have a larger 
#dataset than that I recommend you consider narrowing your dataset down in some way either by filtering 
#or through aggregation. 


#You will then also need to create additional columns in your dataset
#Must create at least one new categorical variable

#Must create at least one new continuous/numerical variable

#Must create at least one summarization data frame 
#Note - your summarization table does not need to be exported to a csv file, you just need to have code
#that create this data frame. 

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

#sum_Population=sum(Population),

#Joins the datasets
df<-merge(grouped_overdose_df,grouped_prescription_rates_df,by=c("State", "Year"))
  
#Must create at least one new categorical variable
#Death rate increase?


#Must create at least one new continuous/numerical variable 
#use for loops to see the chnage in death from the previous year
#Function gives the average death rate per year 
num_deaths_per_yr<-function(Yr){
  yearly_death_rate<-group_by(df,Year)
  deaths_yr_rate<-mean(yearly_death_rate$mean_Model.based.Death.Rate[yearly_death_rate$Year==Yr])
  return(deaths_yr_rate)
}
print(num_deaths_per_yr(2020))



years<-unique(df$Year)

death_per_yr<-vector("integer",length(i))
for (i in seq_along(all_years)) {           
  death_per_yr[i] <- num_deaths_per_yr(all_years[[i]])     
}
print(length(death_per_yr))

#Function gives the average death rate by state 
num_deaths_by_state<-function(State){
  state_death_rate<-group_by(df,State)
  deaths_state_rate<-mean(state_death_rate$mean_Model.based.Death.Rate[state_death_rate$State==State])
  return(deaths_state_rate)
}
print(num_deaths_by_state(Alabama))


#make summarization data frame 
by_state<-group_by(df,State)
rates_by_state<-summarize(
  by_state, 
  avg_opioid_prscrbng_rate=mean(mean_Opioid_Prscrbng_Rate),
  avg_overdose_death_rate=mean(mean_Model.based.Death.Rate))

by_year<-group_by(df,Year)
rates_by_Year<-summarise(
  by_year, 
  avg_opioid_prscrbng_rate=mean(mean_Opioid_Prscrbng_Rate),
  avg_overdose_death_rate=mean(mean_Model.based.Death.Rate))

