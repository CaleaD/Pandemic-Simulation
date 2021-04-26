library(shiny)
library(rsconnect)
#load R scripts
#setwd to change working directory
#source("script/name")
#https://www.r-graph-gallery.com/38-rcolorbrewers-palettes.html
initInfect=1e-6
speed=1/100
infect = 1.4247 / 4
recov  = 0.14286 / 1.2
ui <- fluidPage(
  sliderInput(inputId = "recov",label="Recovery rate",
              value= 0.14286 / 1.2, min=0,max=1, step=0.01,
              animate=animationOptions(interval = speed*8000, loop = FALSE)),
  sliderInput(inputId = "sus",label="Infection rate",
              value= 1.4247 / 4, min=0,max=1, step=0.01,
              animate=animationOptions(interval = speed*8000, loop = FALSE)),
  sliderInput(inputId = "num", label="Number line",
              value = 10, min=1, max=3000,
              animate=animationOptions(interval = 100, loop = FALSE)),
                plotOutput("History"))#name to be used in output$
server <- function(input,output){
  #save object to output$/name/ <- code, where name is the ID of the reactive object
  #$ allows extracting elements by name from a list
  #whatever you save into output, should be passed through a render function
  # „{}” are basically telling the function to take this as an unified code block
  #as many lines of code as i want
  output$History = renderPlot({
    ### Basic SIR Model
    sir <- function(time, state, parameters) {
      with(as.list(c(state, parameters)), {
        dS = -infect * S * I;
        dI =  infect * S * I - recov * I;
        dR =  recov * I;
        
        return(list(c(dS, dI, dR)))
      })
    }
    
    solve.sir = function(sir.f, init, parameters, times) {
      ## Solve using ode (General Solver for Ordinary Differential Equations)
      out = ode(y = init, times = times, func = sir.f, parms = parameters)
      ## change to data frame
      out = as.data.frame(out)
      ## Delete time variable
      out$time <- NULL
      return(out)
    }
    
    ### Plot SIR
    legend.xyf = function(times, x=c(0,0)) {
      c(max(times)*2/3, 0.7) + x;
    }
    plot.sir = function(y, times, legend.lbl=c("Susceptible", "Infected", "Recovered"),
                        legend.xy, leg.off=c(0,0), ylab="Susceptible and Recovered") {
      if(missing(legend.xy)) legend.xy=legend.xyf(times, leg.off)
      matplot(x = times, y = y, type = "l",
              xlab = "Time", ylab = ylab, main = "SIR Model",
              lwd = 1, lty = 1, bty = "l", col = 2:6) # TODO: col = ncol(y);
      
      ## Add legend
      legend(legend.xy[1], legend.xy[2], legend.lbl,
             pch = 1, col = 2:6, bty = "n")
    }
    basic.lbl = c("Susceptible", "Infected", "Recovered");
    
    #############
    ### run Model
    
    ### Set parameters
    
    ### Time frame
    end.time = 120
    times = seq(0, input$num, by = 0.1)
    
    ### Proportion in each compartment:
    # Susceptible = 0.999999;
    # Infected = 0.000001;
    # Recovered = 0;
    init = c(S =1-1e-6, I = 1e-6, R = 0.0)
    
    # probability of infection;
    infect = 1.4247 / 4
    # recovery rate;
    recov  = 0.14286 / 1.2
    
    parameters = c(infect = input$sus, recov = input$recov)
    
    
    ### Solve using ode
    out = solve.sir(sir, init, parameters, times)
    
    ### Plot
    plot.sir(out, times)
  })
  
}
shinyApp(ui=ui,server=server)