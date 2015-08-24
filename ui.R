#Richard Shanahan  
#https://github.com/rjshanahan  
#rjshanahan@gmail.com
#4 July 2015
#UniSA Project for Boeing 737 Survivability by Seat

## load packages

library(shiny)
library(dplyr)
library(ggplot2)
library(reshape2)
library(devtools)
library(cluster)
library(BH)


#GitHub URL
rjs <- as.character(tags$html(
  tags$body(
    a("my GitHub repository", href="https://github.com/rjshanahan/INFS5098_Boeing737_SurvivabilitybySeat", target="_blank"))))

selections <- c("SEAT_EXIT_AVAILABLE",             
                "SEAT_EXIT_PROXIMITY",             
                "FUSELAGE_RUPTURE",                
                "FIRE_PRESENCE",                   
                "CREW_PROXIMITY_OPERATIONAL_COUNT",
                "CREW_OPERATIONAL_PER_REGION",     
                "SEAT_WIDTH",                      
                "SEAT_PITCH",                      
                "fatality",                        
                "passenger_seat_region",           
                "passenger_exit_type",             
                "passenger_class",                 
                "neigbour_fatality")


############################################################
## shiny user interface function
############################################################

ui <- fluidPage(
  titlePanel('k-Means Clustering of Boeing 737 Survivability by Seat'),
  sidebarPanel(
    sliderInput(inputId="k","How many clusters do you want to find?",value=4,min=2,max=15,step=1),
    helpText("Choose a value of k to find the number of natural groupings or 'clusters' in the Boeing 737 Survivability by Seat dataset."),
    helpText("For background information on this dataset and related code please refer to ",rjs),
    actionButton("cluststart","Start clustering now!"),
    width=12), # end sidebarPanel
  mainPanel(
    plotOutput("clusplot"),
    width=12
  ),  # end mainPanel
  fluidRow(column(3,
                  div(checkboxGroupInput(inputId="centre_select", "Select at least two attributes to see their average value per cluster:",
                                         selections,
                                         selected=c("fatality", "passenger_seat_region")            
                  ),
                  style = "font-size:75%")),
           column(9,
                  div(dataTableOutput("centre_choice"),
                      style = "font-size:75%"))),
  
  
  helpText("Descriptions of each of the attributes follows:"),
  helpText("SEAT_EXIT_AVAILABLE: is a passenger's closest exit available"),
  helpText("SEAT_EXIT_PROXIMITY: distance passengers must travel to reach closest exit"),  
  helpText("FUSELAGE_RUPTURE: was the fuselage in the passenger's vicinity ruptured during the accident"),
  helpText("FIRE_PRESENCE: was there fire in the passenger's vicinity"),
  helpText("CREW_PROXIMITY_OPERATIONAL_COUNT: distance from passenger to a functioning crewmember"),
  helpText("CREW_OPERATIONAL_PER_REGION: count of functioning crew in passenger's vicinity"),
  helpText("SEAT_WIDTH: width of passenger's seat"),
  helpText("SEAT_PITCH: width of passenger's seat"),
  helpText("fatality: was the passenger a fatality or survivor (includes injuries)"),
  helpText("passenger_seat_region: portion of the cabin the passenger was seated in, starting from front port to aft starboard"),
  helpText("passenger_exit_type: type of exit closest to passenger"),
  helpText("passenger_class: business/first class or economy"),
  helpText("neigbour_fatality: were the passenger's neighbours survivors or fatalities")
  #fluidRow(column(12, dataTableOutput("table_centres")))
)

