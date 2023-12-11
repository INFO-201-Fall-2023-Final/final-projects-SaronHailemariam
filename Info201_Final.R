library(dplyr)
library(stringr)
library(ggplot2)
prescription_rates_df<- read.csv("Medicaid Opioid Prescribing Rates by Geography.csv") 
overdose_df<-read.csv("NCHS_-_Drug_Poisoning_Mortality_by_County__United_States.csv") 

#Data Joining

#Adds a new column named State in the prescription_rates_df
prescription_rates_df$State<-prescription_rates_df$Geo_Desc

#Cleans prescription_rates_df
prescription_rates_df<-filter(prescription_rates_df,!is.na(Opioid_Prscrbng_Rate))
prescription_rates_df<-filter(prescription_rates_df,!is.na(Opioid_Prscrbng_Rate_1Y_Chg))
prescription_rates_df<-filter(prescription_rates_df,!is.na(Opioid_Prscrbng_Rate_5Y_Chg))

grouped_prescription<-group_by(prescription_rates_df, State, Year)
grouped_prescription_rates_df<-summarise(
  grouped_prescription, 
  sum_Tot_Clms=sum(Tot_Clms), 
  sum_Tot_Opioid_Clms=sum(Tot_Opioid_Clms),
  mean_Opioid_Prscrbng_Rate=mean(Opioid_Prscrbng_Rate),
  mean_Opioid_Prscrbng_Rate_1Y_Chg=mean(Opioid_Prscrbng_Rate_1Y_Chg),
  mean_Opioid_Prscrbng_Rate_5Y_Chg=mean(Opioid_Prscrbng_Rate_5Y_Chg))

#Take out the commas to make population numeric
overdose_df$Population<-as.numeric(gsub(",","",overdose_df$Population))
typeof(overdose_df$Population)

#Cleans overdose_df
overdose_df<-filter(overdose_df,!is.na(Population))
grouped_overdose<-group_by(overdose_df, State, Year)
grouped_overdose_df<-summarise(
  grouped_overdose, 
  sum_Population=sum(Population),
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

#calculate and add column perc_opioid_clms, to store the percentage of total claims that were for opioids
perc_opioid<-function(state, year){
  perc_info<-df[df$State==state & df$Year==year,]
  perc<-((perc_info$sum_Tot_Opioid_Clms/perc_info$sum_Tot_Clms)*100)
  return(perc)
}

df$perc_opioid_clms<-mapply(perc_opioid, df$State, df$Year )




#makes summarization data frames first one by state and second one by year 
by_state<-group_by(df,State)
rates_by_state<-summarize(
  by_state, 
  avg_opioid_prscrbng_rate_state=mean(mean_Opioid_Prscrbng_Rate),
  avg_overdose_death_rate_state=mean(mean_Model.based.Death.Rate),
  avg_Opioid_Prscrbng_Rate_1Y_Chg_state=mean(mean_Opioid_Prscrbng_Rate_1Y_Chg),
  avg_perc_opioid_clms=mean(perc_opioid_clms))

by_year<-group_by(df,Year)
rates_by_Year<-summarise(
  by_year, 
  avg_opioid_prscrbng_rate_yr=mean(mean_Opioid_Prscrbng_Rate),
  avg_overdose_death_rate_yr=mean(mean_Model.based.Death.Rate),
  avg_Opioid_Prscrbng_Rate_1Y_Chg_state=mean(mean_Opioid_Prscrbng_Rate_1Y_Chg),
  avg_perc_opioid_clms=mean(perc_opioid_clms))





#Must create at least one new categorical variable______high risk state true/false ++++ death rate change 1y is postive/negative
#Death rate increase?

#Must create at least one new continuous/numerical variable___________death rate change 1y 
#use for loops to see the chnage in death from the previous year
#Function gives the average death rate per year 


rate_deaths_per_yr<-function(Year){
  yearly_death_rate<-group_by(df,Year)
  deaths_yr_rate<-mean(yearly_death_rate$mean_Model.based.Death.Rate[yearly_death_rate$Year==Year])
  return(deaths_yr_rate)
}
rate_deaths_per_yr(2018)

#Function gives the average death rate by state
rate_deaths_by_state<-function(State){
  state_death_rate<-group_by(df,State)
  deaths_state_rate<-round(mean(state_death_rate$mean_Model.based.Death.Rate[state_death_rate$State==State]))
  return(deaths_state_rate)
}

#Function gives the average death rate per year and by state
rate_deaths_state_yr<-function(state, year){
  rdsy<-df[df$State==state & df$Year==year,]
  state_yr_death_rate<-mean(rdsy$mean_Model.based.Death.Rate)
  return(state_yr_death_rate)
}


#This part stores some national value

#Percentage of all claims that are for opioids 
total_Opioid_Clms<-sum(df$sum_Tot_Opioid_Clms)
total_Clms<-sum(df$sum_Tot_Clms)
perc_opioid_prescribed<-((total_Opioid_Clms/df$sum_Tot_Clms)*100)
 
#National avg drug related death rate  
avg_death_rate<-mean(df$mean_Model.based.Death.Rate)


rate_deaths_per_yr<-function(Year){
  yearly_death_rate<-group_by(df, Year)
  deaths_yr_rate<-round(mean(yearly_death_rate$mean_Model.based.Death.Rate[yearly_death_rate$Year==Year]))
  return(deaths_yr_rate)
}
rate_deaths_per_yr(2018)

#Function gives the average death rate by state
rate_deaths_by_state<-function(State){
  state_death_rate<-group_by(df,State)
  deaths_state_rate<-round(mean(state_death_rate$mean_Model.based.Death.Rate[state_death_rate$State==State]))
  return(deaths_state_rate)
}

#Function gives the average death rate per year and by state
rate_deaths_state_yr<-function(state, year){
  rdsy<-df[df$State==state & df$Year==year,]
  state_yr_death_rate<-mean(rdsy$mean_Model.based.Death.Rate)
  return(state_yr_death_rate)
}


#This part stores some national value

#Percentage of all claims that are for opioids 
total_Opioid_Clms<-sum(df$sum_Tot_Opioid_Clms)
total_Clms<-sum(df$sum_Tot_Clms)
perc_opioid_prescribed<-((total_Opioid_Clms/df$sum_Tot_Clms)*100)
 
#National avg drug related death rate  
avg_death_rate<-mean(df$mean_Model.based.Death.Rate)



#___________________________________________________________________________________
#Function gives the average death rate by state from part 1
#rate_deaths_by_state<-function(State){
#  state_death_rate<-group_by(df,State)
#  deaths_state_rate<-mean(state_death_rate$mean_Model.based.Death.Rate[state_death_rate$State==State])
#  return(h3(HTML(deaths_state_rate)))
#}




TwentyEighteen<-(rate_deaths_per_yr(2018))
twentytwentyone<-(rate_deaths_per_yr(2021))



National_average<-round(mean(df$mean_Model.based.Death.Rate))
above_avg<-nrow(df[df$high_overdose_death_rate_code==1,])
below_avg<-nrow(df[df$high_overdose_death_rate_code==2,])


risk_state_code<-function(state, year){
  National_death_rate<-mean(df$mean_Model.based.Death.Rate)
  st_gp<-group_by(df,State, Year)
  st<-summarise(
    st_gp,
    mean_Model.based.Death.Rate_st=mean(mean_Model.based.Death.Rate))
  
  state_data <- filter(st, State == state & Year==year)
  if(state_data$mean_Model.based.Death.Rate_st>=National_death_rate){
    return(1)
  }else {
    return(2)
  }
}

df$high_overdose_death_rate_code<-mapply(risk_state_code, df$State, df$Year)


rate_year<-function(df, year_death){
  return((df$yr[df$Year==year_death]))
}
rate_state<-function(df, state_death){
  return((df$st[df$State==state_death]))
}


west_states <- c("Washington", "Oregon", "California", "Nevada", "Idaho", "Montana", "Wyoming", "Utah", "Colorado", "Arizona", "New Mexico", "Alaska", "Hawaii")
midwest_states <- c("North Dakota", "South Dakota", "Nebraska", "Kansas", "Minnesota", "Iowa", "Missouri", "Wisconsin", "Illinois", "Michigan", "Indiana", "Ohio")
south_states <- c("Texas", "Oklahoma", "Arkansas", "Louisiana", "Kentucky", "Tennessee", "Mississippi", "Alabama", "Georgia", "Florida", "South Carolina", "North Carolina", "Virginia", "West Virginia", "Maryland", "Delaware", "District of Columbia")
north_states <- c("Maine", "New Hampshire", "Vermont", "Massachusetts", "Rhode Island", "Connecticut", "New York", "New Jersey", "Pennsylvania")

df <- df %>%
  mutate(Region = case_when(
    State %in% west_states ~ "West",
    State %in% midwest_states ~ "Midwest",
    State %in% south_states ~ "South",
    State %in% north_states ~ "North",
    TRUE ~ NA_character_  # for any state not matching the above
  ))
  
