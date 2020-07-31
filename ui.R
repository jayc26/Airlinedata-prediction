#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)

# Define UI for application that draws a histogram
shinyUI(
  dashboardPage(
  dashboardHeader(title="Air Fare Analysis"),
  dashboardSidebar(
    
    sidebarMenu(
    #menuItem("Home", tabName = "HomePage"),
    menuItem("Delay Analysis", tabName = "Delay",icon = icon("plane-departure")),
  
    menuItem("Air Fare Analysis",icon = icon("plane")),
      #menuSubItem("Price And Duration",tabName = "m1"),
      menuSubItem("Price and Duration", tabName = "pd",icon = icon("money-bill")),   
      menuSubItem("Price Prediction", tabName = "pre",icon = icon("exclamation-circle")),
    menuItem("Airport Analysis",icon = icon("suitcase-rolling")),
      menuSubItem("Airport Rank Data",tabName = "m3",icon = icon("passport")),
      menuSubItem("Airport and Passenger Analysis",tabName = "m4",icon = icon("map-marked-alt"))
      #menuSubItem("Passangers By Airport",tabName = "m5"),
      #menuSubItem("Passanger Analysis",tabName = "m7")
    
    
    
  )),
  dashboardBody(
    tabItems(
      #tabItem(tabName = "Home",h1("Airfare Analysis")),
      tabItem(tabName = "Delay",fluidRow(valueBoxOutput("delay"),valueBoxOutput("adelay"),valueBoxOutput("cancel"),valueBoxOutput("tpr"),valueBoxOutput("cdel"),valueBoxOutput("nasdel")),plotOutput("da"),plotOutput("dan"),plotOutput("dep"),plotOutput("arr")),
      #tabItem(tabName ="m1",fluidRow(plotOutput("a")),fluidRow(plotOutput("c")),fluidRow(plotOutput("pd")),fluidRow(plotOutput("pdl")),fluidRow(plotOutput("seats")),fluidRow(plotOutput("pi")),fluidRow(plotOutput("gg"))),
      #tabItem(tabName ="mc1",fluidRow(plotOutput("ae"),plotOutput("a"))),
      tabItem(tabName = "pd",fluidRow(plotOutput("ae"),plotOutput("a"),plotOutput("c"),plotOutput("pd"),plotOutput("pdl"),plotOutput("seats"),plotOutput("pi"),plotOutput("pf1"),plotOutput("pf3"),plotOutput("pf4"),plotOutput("g1"),plotOutput("g2"),plotOutput("g3"),plotOutput("g4"),plotOutput("g5"),plotOutput("g6"))),
      tabItem(tabName = "m3",fluidRow(tableOutput("d"))),
      #tabItem(tabName = "m5",fluidRow(plotOutput("pas"))),
      #tabItem(tabName = "m7",fluidRow(plotOutput("rp"),plotOutput("npp"))),
      tabItem(tabName = "m4",fluidRow(plotOutput("af"),plotOutput("pas"),plotOutput("gg"))),
      tabItem(tabName ="pre",fluidRow(plotOutput("e")),fluidRow(plotOutput("pe")),fluidRow(plotOutput("p")),fluidRow(plotOutput("pp")),fluidRow(plotOutput("ch")),fluidRow(plotOutput("peh")),fluidRow(textOutput("pehl")))
      
      
  )
  
  # Application title
)))