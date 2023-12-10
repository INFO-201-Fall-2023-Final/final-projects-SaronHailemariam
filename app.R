library(dplyr)
library(stringr)
library(ggplot2)
library(shiny)
library(plotly)
source("Info201_Final.R")


prescription_rates_df<- read.csv("Medicaid Opioid Prescribing Rates by Geography.csv") 
overdose_df<-read.csv("NCHS_-_Drug_Poisoning_Mortality_by_County__United_States.csv") 


#Page 1

 
      intro_panel<- fluidPage( 
              h3("Introduction "),
              p("As drug overdose related deaths continues to prevail in society it is now necessary more 
                than ever to understand the basis of drug overdose realted and implications on public health deaths
                . Analysis of drug overdose related deaths has important implications for the clinical use of drugs, 
                detection procedure, and public policy. In the present day, prescribed opioid use,  is brought into a 
                new light as emerging laws and studies question the rates and nessecaitys of its use. And is therefore 
                a good starting point."),
         
        
         br(),
         h2("Our Methods"),
         p("We looked at death rates from... in... this mannyt people have died from drug overdoes in themost
         recent year, 2021. 
        we will explore how this number compares to previous year. We will also investigate a possible cause that might not be obvous..  "),
br(),
p("By: "),
p("Using:"),
p("Sources:"))
         
 
  #__________________________________________________________


 
   
   
   
   
#Page 2 The drug overdoe rates
#______________________________________________________________________
problem_panel<- fluidPage("Explore the problem",
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
      choices= df$Year),
 # htmlOutput(outputId = "yr"),
  #htmlOutput(outputId = "st"),
 # htmlOutput(outputId = "year"),
  ))
  mainPanel(
    h3("Seeing is believing"),
    tabsetPanel(
      tabPanel("Death by Year", plotlyOutput(outputId = "year")),
      tabPanel("Death by State" , plotlyOutput(outputId = "state"))))
      #tabPanel("State and Year", plotlyOutput(outputId = "state and year"))))
    
    


#Page 3 Medicare rates, 
#______________________________________________________________________
  Medicare_panel<- fluidPage("Medicare",
                             
                             )


#Page 4 Zoom into the 4 years 2018-2021 and compare prescription rates and death rates
#exploring medicare as a possible cause 
#______________________________________________________________________
  Comapre_panel<- fluidPage("Exploring Medicare as the causitve agent",
                             
  ) 
#Page 5 Conclusions Is drug overdose deaths actually a probelm? is it caused by medicare? does medicare contribute at alll?
#_____________________________________________________
  Conclusions<- fluidPage("Conclusion"
                            
  ) 



#Server stuff 
#___________________________________________________________
ui<-navbarPage("Death by Drug Overdose",
               tabPanel("About", intro_panel),
               tabPanel("Explore the problem", problem_panel),
               tabPanel("Medicare",  Medicare_panel),
               tabPanel("Exploring Medicare as the causitve agent", Comapre_panel),
               tabPanel("Conclusion", Conclusions)
               )
               
               
               

server<-function(input, output){
 
output$state <- renderPlotly({  
    A<-ggplot(df, aes(x=State, y=mean_Model.based.Death.Rate))+
      geom_bar(stat="identity")+
      labs(x="State", y= "Death Rate" )
   
    A<- ggplotly(A, tooltip = "text") 
    
    return(A)
    })

output$year<- renderPlotly({  
  A<-ggplot(df, aes(x=Year, y=mean_Model.based.Death.Rate))+
    geom_bar(stat="identity")+
    labs(x="Year ", y= "Death Rate" )
  A<- ggplotly(A, tooltip = "text")
  return(A)
} ) 
#output$yr <- renderUI({
#  return(rate_deaths_per_yr(df, input$year_death)) 
#})

#output$st <- renderUI({
#  return(rate_deaths_per_state(df, input$state_death)) 
#})
}
     

    
shinyApp(ui,server) 
