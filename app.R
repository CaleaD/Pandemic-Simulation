library(shiny)
library(rsconnect)
library(shinyjs)
library(png)
#setwd
#animate=animationOptions(interval = 10, loop = FALSE)
#global_param=data.frame(
 # default=c(infect = 1.4247/ 4,recov  = 0.14286 / c(1.2, 1.3),death = c(1/10, 1/5, 3/4,1/2),hosp = c(0.1, 0.2)),
  #fast=c(infect=10,recov  = 0.14286 / c(1.2, 1.3),death = c(1/10, 1/2),hosp = c(0.1, 0.2)),
  #furious=c(infect = 100,recov  = 0/ c(1.2, 1.3),death = c(1,2),hosp = c(0.1, 0.2))
#)
source("Beta_QM.R");
source("TwoViruses3D(with seed).R")
source("diagrams.R")
ui2 <- fluidPage(
  
  titlePanel("Epidemic Simulator"),
  sidebarLayout(
    sidebarPanel("Settings and Variables",position="right",
                 sliderInput(inputId = "its",label="Number of iterations",
                             value=1, min=1,max=720, step=1,
                             animate=animationOptions(interval = 10, loop = FALSE))),
    mainPanel("Other stuff",plotOutput("virus"))
  ),
  mainPanel("All the cool stuff"),
  textOutput("boy")

)
server2<-function(input,output)
{
  output$virus=renderPlot({
    TV3D(input$its,1)
  })
}

ui1 <- fluidPage(plotOutput("DRE1"))
ui <- fluidPage("Epidemic Simulation",useShinyjs(),
                tabsetPanel(
                  tabPanel("Basic SIR model",
                           plotOutput("BasicPl"),
                           
                           
                           hr(),
                           
                  fluidRow(
                  column(4,
                  sliderInput(inputId = "timeB",label="Number of days",
                              value=1, min=1,max=720, step=1)),
                  
                  column(4,
                  sliderInput(inputId = "infectB",label="Rate of infection",
                              value=0.0001, min=0,max=0.7, step=0.001)),
                  
                  column(4,
                  sliderInput(inputId = "recovB",label="Rate of recovery",
                              value=0.0001, min=0,max=0.7, step=0.001)),
                  imageOutput("BasicDia")
                  )),
                  
                  
                  tabPanel("Hospitalization SIR model",
                           plotOutput("Hosp"),
                           hr(),
                           fluidRow(
                             column(4,
                                    sliderInput(inputId = "timeH",label="Number of days",
                                                value=1, min=1,max=720, step=1),
                                    sliderInput(inputId = "infectH",label="Rate of infection",
                                                value=0.0001, min=0,max=0.7, step=0.001),
                                    sliderInput(inputId = "recovH",label="Rate of recovery",
                                                value=0.0001, min=0,max=0.7, step=0.001)),
                           column(4, 
                  sliderInput(inputId = "recov.hH",label="Rate of recovery in hospitals",
                              value=0.0001, min=0,max=0.7, step=0.001),
                  
                  sliderInput(inputId = "deathH",label="Rate of death",
                              value=0.0001, min=0,max=0.7, step=0.001),
                  
                  sliderInput(inputId = "death.hH",label="Rate of death in hospitals",
                              value=0.0001, min=0,max=0.7, step=0.001)),
                  column(4,
                  sliderInput(inputId = "hospH",label="Rate of hospitalisation",
                              value=0.0001, min=0,max=0.7, step=0.001),
                  
                  sliderInput(inputId = "hosp.vH",label="Rate of hospitalisation for old people",
                              value=0.0001, min=0,max=0.7, step=0.001))
                  )
                  ), 
                  tabPanel("Vaccination SIR model",
                           plotOutput("Vacc"),
                           hr(),
                           fluidRow(
                             column(3,
                                    sliderInput(inputId = "timeV",label="Number of days",
                                                value=1, min=1,max=720, step=1),
                                    sliderInput(inputId = "infectV",label="Rate of infection",
                                                value=0.0001, min=0,max=0.7, step=0.001),
                                    sliderInput(inputId = "recovV",label="Rate of recovery",
                                                value=0.0001, min=0,max=0.7, step=0.001)),
                             column(3, 
                                    sliderInput(inputId = "recov.hV",label="Rate of recovery in hospitals",
                                                value=0.0001, min=0,max=0.7, step=0.001),
                                    
                                    sliderInput(inputId = "deathV",label="Rate of death",
                                                value=0.0001, min=0,max=0.7, step=0.001),
                                    
                                    sliderInput(inputId = "death.hV",label="Rate of death in hospitals",
                                                value=0.0001, min=0,max=0.7, step=0.001)),
                             column(3,
                                    sliderInput(inputId = "death.oV",label="Rate of death in old people",
                                                value=0.0001, min=0,max=0.7, step=0.001),
                                    
                                    sliderInput(inputId = "death.ohV",label="Rate of death in hospitalised old people",
                                                value=0.0001, min=0,max=0.7, step=0.001),
                            
                                    sliderInput(inputId = "hospV",label="Rate of hospitalisation",
                                                value=0.0001, min=0,max=0.7, step=0.001)),
                             column(3,
                                    sliderInput(inputId = "hosp.vV",label="Rate of hospitalisation for old people",
                                                value=0.0001, min=0,max=0.7, step=0.001),
                                    
                             sliderInput(inputId = "vacc.yV",label="Rate of vaccination for old people",
                                         value=0.0001, min=0,max=0.001, step=0.0001),
                                     sliderInput(inputId = "vacc.oV",label="Rate of vaccination for young people",
                                       value=0.0001, min=0,max=0.001, step=0.0001)),
                             imageOutput("Complex")
                           )
                           
                  ),
                  tabPanel("Two Viruses",
                           plotOutput("Virus"),
                           hr(),
                           fluidRow(
                             column(12,
                                    sliderInput(inputId = "its",label="Number of iterations",
                                                value=1, min=1,max=720, step=1,
                                                animate=animationOptions(interval =250, loop = FALSE))           
                             )))
                )
)
                
#recov  = 0.14286 / c(1.2, 1.3)
server <- function(input,output){
 # output$boy<-renderText({})
  # TODO
  # make all input into a list
  # allow the init to receive a list as a parameter(reduces call length, maybe easier to debug?)
  #isay=reactive(input$time)
 output$Complex=renderImage({
   list(src="./ye2.1.png",width = 510,
        height = 524)
 }
 )
 output$John=renderText(
                               #global_param$default
                               if(input$time>50){
                                 shinyjs::hide(id="DRE")
                                 shinyjs::show(id="DRE1")}
                               else{
                                 shinyjs::show(id="DRE")
                                 shinyjs::hide(id="DRE1")
                               }
                               
   )
  output$BasicPl<-renderPlot({

    custom=c(input$infectB,input$recovB)
    initSIR_Basic(custom,input$timeB)
    
  })
  output$BasicDia<-renderImage({
      list(src="./ye1.1.png",width = 458*2.5,
      height = 154*2.5)
   #if(input$toggle)
   # img=readPNG(source="ye1.png")
   # writePNG(img)
   # else
    #{
    #initSIR_Death_Comparison(recov=input$it/c(1.2,1.3))
   # 
    #list_test=c(infect = 1.4247/4,recov  = 0.14286 / c(1.2, 1.3),death = recov * c(1/10, 1/2),hosp = c(0.1, 0.2))
   # 
   # }
    #"3"=initSIR_Hosp_Com(global_param$furious,input$time)
  },deleteFile = FALSE)
  output$Hosp<-renderPlot({
    custom=c(input$infectH,input$recovH,input$recov.hH,input$deathH,input$death.hH,input$hospH,input$hosp.vH)
    initSIR_Hosp_Com(custom,input$timeH)
  })
  output$Virus=renderPlot({
    TV3D(input$its,2)
  })
  output$Vacc=renderPlot({
  custom=c(input$infectV,
           input$recovV,
           input$recov.hV,
           input$deathV,
           input$death.hV,
           input$hospV,
           input$hosp.vV,
           input$vacc.oV,
           input$vacc.yV,
           input$death.oV,
           input$death.ohV)
  initSIR_VAacc_Older(custom,input$timeV)
})
}
shinyApp(ui=ui,server=server)

# to implement
# tabsets(with their own models and parameters)
# fancy(-er) display(plot up, params down)