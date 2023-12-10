library(dplyr)
library(stringr)
library(ggplot2)
library(shiny)
library(plotly)
source("Info201_Final.R")


prescription_rates_df<- read.csv("Medicaid Opioid Prescribing Rates by Geography.csv") 
overdose_df<-read.csv("NCHS_-_Drug_Poisoning_Mortality_by_County__United_States.csv") 


#Page 1
#__________________________________________________________
ui<-navbarPage("Death by Drug Overdose", 
              h3("Introduction")
                 p("As drug overdose related deaths continues to prevail in society it is now necessary more 
                than ever to understand the basis of drug overdose realted and implications on public health deaths
                . Analysis of drug overdose related deaths has important implications for the clinical use of drugs, 
                detection procedure, and public policy. In the present day, prescribed opioid use,  is brought into a 
                new light as emerging laws and studies question the rates and nessecaitys of its use. And is therefore 
                a good starting point.")
   
#Page 2
#______________________________________________________________________
tabPanel("Explore the problem",
sidebarLayout(
  sidebarPanel(
    selectInput( 
      inputId = "state_death", 
      label= "Select a state to view overdose death rates",
      choices= df$State)
  ),
    selectInput( 
      inputId = "year_death", 
      label= "Select a year to view overdose death rates",
      choices= df$Year)
  ),
  mainPanel(
    h3("Seeing is believing"),
    tabsetPanel(
      tabPanel("Death by Year", plotlyOutput(outputId = "year")),
      tabPanel("Death by State" , plotlyOutput(outputId = "state")),
      #tabPanel("State and Year", plotlyOutput(outputId = "state and year"))
    )
    
  )))

#by state, by year, by year and state , 
#___________________________________________________________


server<-function(input, output){

#For Page 1
#__________________________________________________



 #For Page 2
#__________________________________________________
output$state <- renderPlotly({  
    A<-ggplot(df, aes(x=State, y=reorder(mean_Model.based.Death.Rate), ))+
      geom_bar(stat="identity")+
      labs(x="State", y= "Death Rate", )
   
    A<- ggplotly(A, tooltip = "text") 
    
    return(A)
    })
output$year<- renderPlotly({  
  A<-ggplot(df, aes(x=df$Year, y=df$mean_Model.based.Death.Rate, ))+
    geom_bar(stat="identity")+
    labs(x="Year ", y= "Death Rate", )
  A<- ggplotly(A, tooltip = "text")
  return(A)
})

  
  }        

    
shinyApp(ui,server)  
