library(shiny)
library(rsconnect)
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
           #      tabPanel("hehe,boy"),
            #     tabPanel("hehe,boy"),
             #    tabPanel("hehe,boy"),
                 plotOutput("DRE")
)

server <- function(input,output){
 # output$boy<-renderText({})
  output$DRE<-renderPlot({
    initSIR_Death_Comparison()
  })
}

shinyApp(ui=ui,server=server)