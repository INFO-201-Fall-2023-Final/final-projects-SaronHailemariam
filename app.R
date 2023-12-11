library(dplyr)
library(stringr)
library(ggplot2)
library(shiny)
library(plotly)
library(usmap)
source("Info201_Final.R")

#learned how to make a hyperlink from https://stackoverflow.com/questions/42047422/create-url-hyperlink-in-r-shiny
prescription_rates_df<- read.csv("Medicaid Opioid Prescribing Rates by Geography.csv") 
overdose_df<-read.csv("NCHS_-_Drug_Poisoning_Mortality_by_County__United_States.csv") 
df<-read.csv("infodf.csv")

#Page 1

 
intro_panel<- fluidPage( 
              h2("Introduction "),
              p(" In the year 2021 alone,", twentytwentyone, "people have died as a result of drug overdoes  in the United States. 
As drug overdose related deaths continues to prevail in society it is now necessary more 
                than ever to understand the basis of drug overdose realted deaths and the implications on public health deaths
 . Analysis of drug overdose related deaths has important implications for the clinical use of drugs, 
                detection procedure, and public policy. In the present day, prescribed opioid use,  is brought into a 
                new light as emerging laws and studies question the rates and nessecaitys of its use. And is therefore 
                a good starting point."),
              p("We explore how this number compares to previous year to determine if there really is a problem . 
                We will also investigate a possible cause that might not be obvious initially "),
         
        
         br(),
         h3("Our Methods"),
         p("We will look at the drug overdose related death rates in the United States. Figures on the numbers of deaths from drug overdose comes 
         from the Centers for Disease Control and is linked below. We 
         also examine Meidcare prescription rates from the U.S Medicare and Medicade Services. That datset is  also linked below. 
        "),
         
         
br(),
p("By: Saron and Varun "),

p("Using:RStudio"),
br(),
p("Sources:"),
uiOutput(outputId = "NIH"),
uiOutput(outputId = "Medicare"),
      tags$style(HTML("
    h2 {
            background-color: #acd5d8;
            color: Black;
            }"),
                 
        tabPanel("Map of the us", plotlyOutput(outputId = "map"))
        ))
                 

         
 
  #__________________________________________________________


 
   
   
   
   
#Page 2 The drug overdose rates
#______________________________________________________________________
problem_panel<- fluidPage("Explore the problem",
sidebarLayout(
  sidebarPanel(
    selectInput( 
      inputId = "state_death", 
      label= "Select a state to view overdose death rates",
      choices= df$State),
    htmlOutput(outputId = "st"),
    selectInput( 
      inputId = "year_death", 
      label= "Select a year to view overdose death rates",
      choices= df$Year),
    htmlOutput(outputId = "yr"),
    p("When investigating widespread public health matters, it is important to reflect on our own communities. 
      How does your state compare to others? Are the results what you would expect based on your experience, what you see on
      the news, and/or hear from others.  "),
    ),
    mainPanel(
      h3("Seeing is believing"),
       tabsetPanel(
       tabPanel("Death by Year", plotlyOutput(outputId = "year"), 
       p("It is evident that the model based rate of drug overdose related 
         deaths in the U.S. is increasing at an alarmingly fast rate. In 2018 the rate was", TwentyEighteen,
                                    "By 2021 the rate had increased alarmingly to",twentytwentyone,".It is important to note that we
                                    zoomed in to the most recent four years in wich data was available because it emphazise the mostcurrennt trend." )),
       
       tabPanel("Death by State", plotlyOutput(outputId = "state"))),
      p("As shown, there is vraibaility in the model based death rates between States.", highest_state,"had the highest drug overdose mortality rate and",lowest_state,"had the lowest")
)),
tabPanel(
  h3("Comparing model based death rates between States"),
  p("We have defined states high risk if the drug related overdose rate are greater than the national average,",National_average,"."),
  tabPanel("At Risk States", plotlyOutput(outputId = "risk")),


)
)
 #htmlOutput(outputId = "yr"),

 # htmlOutput(outputId = "year"),


 
      #tabPanel("State and Year", plotlyOutput(outputId = "state and year")))),
    
    


#Page 3 Medicare rates stuff goes here , 
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
  Conclusions<- fluidPage("Conclusion",
                            
  )



#Server stuff 
#___________________________________________________________
ui<-navbarPage("Death by Drug Overdose",
               tabPanel("About", intro_panel ),
               tabPanel("Explore the problem", problem_panel),
               tabPanel("Medicare",  Medicare_panel),
               tabPanel("Exploring Medicare as the causitve agent", Comapre_panel),
               tabPanel("Conclusion", Conclusions)
               )
            
               
               

server<-function(input, output){
  
  output$yr <- renderUI({
    return(rate_year(df,input$year_death)) 
  })
  
  output$st <- renderUI({
    return(rate_deaths_per_state(df, input$state_death)) 
  })
  
  
  
output$state <- renderPlotly({  
    A<-ggplot(df, aes(x=mean_Model.based.Death.Rate, y=reorder(State,mean_Model.based.Death.Rate), colors=mean_Model.based.Death.Rate))+
      geom_bar(stat="identity")+
      labs(x="State", y= "Death Rate", axis.text=element_text(size=2))
      coord_flip()
    A<- ggplotly(A, tooltip ="text") 
    return(A)
    })

output$year<- renderPlotly({  
  B<-ggplot(df, aes(x=Year, y=mean_Model.based.Death.Rate))+
    geom_bar(stat="identity")+
    labs(x="Year ", y= "Death Rate", Title="Death Rate by Year", caption = "It is evident that the rate of drug overdose related 
         deaths in the U.S. is increasing at an alarmingly fast rate. In 2018 the death rate was", TwentyEighteen,
         "By 2021 the death rate had increased alarmingly to ")
  B<- ggplotly(B, tooltip = "text")
  return(B)
} ) 



output$map<-renderPlotly({ 

  M<-plot_usmap(data = statepop, values = "mean_Model.based.Death.Rate", color = "red") + 
  scale_fill_continuous(name = "Population (2015)", label = scales::comma) + 
  theme(legend.position = "right")
M<-ggplotly(M, tooltip = "text")

return(M)
} ) 

output$NIH<-renderUI({
  url<-a("CDC Drug Ovderdose Rates", href="https://www.cdc.gov/nchs/data-visualization/drug-poisoning-mortality/index.htm")
    tagList("URL link:", url)
  })

output$Medicare<-renderUI({
  url<-a("Medicare Prescpription Rates", href="https://catalog.data.gov/dataset/medicaid-opioid-prescribing-rates-by-geography-98137")
  tagList("URL link:", url)
})




}
     

    
shinyApp(ui,server)  
