library(shiny)
library(rsconnect)
#setwd
#animate=animationOptions(interval = 10, loop = FALSE)
global_param=data.frame(
  default=c(infect = 1.4247/ 4,recov  = 0.14286 / c(1.2, 1.3),death = c(1/10, 1/2),hosp = c(0.1, 0.2)),
  fast=c(infect=10,recov  = 0.14286 / c(1.2, 1.3),death = c(1/10, 1/2),hosp = c(0.1, 0.2)),
  furious=c(infect = 100,recov  = 0/ c(1.2, 1.3),death = c(1,2),hosp = c(0.1, 0.2))
)
source("Beta_QM.R");
ui <- fluidPage(
  
  titlePanel("Epidemic Simulator"),
  sidebarLayout(
    sidebarPanel("Settings and Variables",position="right"),
    mainPanel("Other stuff")
  ),
  mainPanel("All the cool stuff"),
  textOutput("boy")
)
ui1 <- fluidPage(plotOutput("DRE1"))
ui <- fluidPage("My app",
                sidebarLayout(
                sidebarPanel(
                  selectInput(inputId="select",label=" Select model ",
                              choices = list("SIR" = 1, "SIR_Custom" = 2,"Hosp_Com" = 3, "Hosp_Com_Custom" = 4),selected=1),
                  
                  sliderInput(inputId = "time",label="Number of days",
                              value=1, min=1,max=720, step=1),
                  
                  sliderInput(inputId = "infect",label="Rate of infection",
                              value=0.0001, min=0.0001,max=0.7, step=0.001),
                  
                  sliderInput(inputId = "recov",label="Rate of recovery",
                              value=0.0001, min=0.0001,max=0.7, step=0.001),
                  
                  sliderInput(inputId = "recov.h",label="Rate of recovery in hospitals",
                              value=0.0001, min=0.0001,max=0.7, step=0.001),
                  
                  sliderInput(inputId = "death",label="Rate of death",
                              value=0.0001, min=0.0001,max=0.7, step=0.001),
                  
                  sliderInput(inputId = "death.h",label="Rate of death in hospitals",
                              value=0.0001, min=0.0001,max=0.7, step=0.001),
                  
                  sliderInput(inputId = "hosp",label="Rate of hospitalisation",
                              value=0.0001, min=0.0001,max=0.7, step=0.001),
                  
                  sliderInput(inputId = "hosp.v",label="Rate of hospitalisation for old people",
                              value=0.0001, min=0.0001,max=0.7, step=0.001),
                                 textOutput("John")),
                mainPanel("hi",plotOutput("DRE"))),
                 
)
#recov  = 0.14286 / c(1.2, 1.3)
server <- function(input,output){
 # output$boy<-renderText({})
  # TODO
  # make all input into a list
  # allow the init to receive a list as a parameter(reduces call length, maybe easier to debug?)
 output$John=renderText(global_param$default)
  output$DRE<-renderPlot({
    #initSIR_Death_Comparison(recov=input$it/c(1.2,1.3))
    graph=input$select
    custom=c(input$infect,input$recov,input$recov.h,input$death,input$death.h,input$hosp,input$hosp.v)
    #list_test=c(infect = 1.4247,recov  = 0.14286 / c(1.2, 1.3),death = recov * c(1/10, 1/2),hosp = c(0.1, 0.2))
    switch(graph,
    "1"=initSIR_Basic(global_param$default,input$time),
    "2"=initSIR_Basic(custom,input$time),
    "3"=initSIR_Hosp_Com(global_param$default,input$time),
    "4"=initSIR_Hosp_Com(custom,input$time),
    
    #"3"=initSIR_Hosp_Com(global_param$furious,input$time)
    )
  })
  output$DRE1 =renderPlot({})
}

shinyApp(ui=ui,server=server)

# to implement
# tabsets(with their own models and parameters)
# fancy(-er) display(plot up, params down)