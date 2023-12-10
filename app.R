library(dplyr)
library(stringr)
library(ggplot2)
library(shiny)
library(plotly)
source("Info201_Final.R")


prescription_rates_df<- read.csv("Medicaid Opioid Prescribing Rates by Geography.csv") 
overdose_df<-read.csv("NCHS_-_Drug_Poisoning_Mortality_by_County__United_States.csv") 



ui<-fluidPage(
  
  #Page 1
  #__________________________________________________________
 page_one<-titlePanel("Introduction"),
 h3("cool"),
 
   
   
   
   
#Page 2
#______________________________________________________________________
page_two<- sidebarLayout(
  sidebarPanel(
    titlePanel("The problem"),
    selectInput( 
      inputId = "death_map", 
      label= "Select year to view overdose death rates",
      choices= df$Year)
  ),
  mainPanel(
    h3("Bar graph of deaths"),
    plotlyOutput(outputId = "bar")
  )), 


#___________________________________________________________
tabsetPanel(
  tabPanel("Death by Drug Overdose", page_one),
 tabPanel("The problem", page_two),
 # tabPanel("Medicare", page_three),
 # tabPanel("Question 1", page_four),
 # tabPanel("Question 2", page_five),
 # tabPanel("Qestion 3", page_six),
 # tabPanel("Conclusion", page_seven)
),
)

server<-function(input, output){

output$bar<-renderPlotly({
      B<-ggplotly(df, aes(x= Year , y=mean_Model.based.Death.Rate))+
        geom_bar(stat="identity")+
        labs(x="Year", y="Death Rate",
             title="...",
             caption = ...)
      B<-ggplotly(B, tooltip = "text")
      return(B)
    })
  }        

    
shinyApp(ui,server)  
