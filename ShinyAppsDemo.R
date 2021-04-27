library(shiny)
library(rsconnect)
setwd("C:/Users/NitrousOxide/Desktop/Faculty/Year 2, Sem2/Team Project/Epidemic");
source("Modulated Scripts.R");
ui <- fluidPage(
  
  titlePanel("Epidemic Simulator"),
  sidebarLayout(
    sidebarPanel("Settings and Variables",position="right"),
    mainPanel("Other stuff")
  ),
  mainPanel("All the cool stuff"),
  textOutput("boy")
)
ui <- fluidPage("My app",
                sliderInput(inputId = "it",label="Number of iterations",
                            value=0.14286, min=0.001,max=0.3, step=0.001
                            #animate=animationOptions(interval = 10, loop = FALSE)
                ),
                 plotOutput("DRE")
)
#recov  = 0.14286 / c(1.2, 1.3)
server <- function(input,output){
 # output$boy<-renderText({})
  # TODO
  # make all input into a list
  # allow the init to receive a list as a parameter(reduces call length, maybe easier to debug?)
  output$DRE<-renderPlot({
    initSIR_Death_Comparison(recov=input$it/c(1.2,1.3))
  })
}

shinyApp(ui=ui,server=server)