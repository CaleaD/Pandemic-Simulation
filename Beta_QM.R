#This whole section is from SIR_Hosp_Com.R
#####################
#####################

### SIR + Old Age + Hospital
# - different mortalities for Hospital vs Community;

### incarcare librarii
library(shiny)
library(ggplot2)
library(deSolve)

# Used for basic SIR
solve.SIR = function(param, type="infect") {
  type = match(type, c("infect", "recov"))
  if(type == 1) {
    parameters = c(infect = param, recov = recov)
  } else if(type == 2) {
    parameters = c(infect = infect, recov = param)
  }
  solve.sir(sir, init, parameters, times)$S;
}

# Used for the others
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
                    legend.xy, leg.off=c(0,0), ylab="Susceptible and Recovered",...) {
  if(missing(legend.xy)) legend.xy=legend.xyf(times, leg.off)
  matplot(x = times, y = y, type = "l",
          xlab = "Time", ylab = ylab, main = "SIR Model",
          lwd = 1, lty = 1, bty = "l", col = 2:10,...)
  
  ## Add legend
  legend(legend.xy[1], legend.xy[2], legend.lbl,
         pch = 1, col = 2:10, bty = "n")
}



basic.lbl = c("Susceptible", "Infected", "Recovered");


sir_comunity <- function(time, state, parameters) {
  with(as.list(c(state, parameters)), {
    dS = -infect * S * I - infect * S * H; # both I & H infect!
    dV = -infect * V * I - infect * V * H;
    dT = (-infect * S * I - infect * S * H) + (-infect * V * I - infect * V * H);
    dI =  infect * S * I + infect * S * H + infect * V * I + infect * V * H - recov * I - death * I - hosp * I;
    dH =  hosp * I - recov.h * H - death.h * H;
    dR =  recov * I + recov.h * H;
    dDc =  death * I;
    dDh =  death.h * H;
    return(list(c(dT, dS, dI, dR, dH, dV, dDc, dDh)));
  })
}


basic_sir <- function(time, state, parameters) {
  with(as.list(c(state, parameters)), {
    dS = -infect * S * I;
    dI =  infect * S * I - recov * I;
    dR =  recov * I;
    return(list(c(dS, dI, dR)))
  })
}


initSIR_Basic = function(list,end.time){
  times = seq(0,end.time, by=1)
  parameters = c(infect = list[1],
                 recov = list[2])
  print(parameters)
  
  init = c(S = 1-1e-6, I = 1e-6, R = 0.0)  ### Solve using ode
  ### Solve using ode
  out = solve.sir(basic_sir, init, parameters, times)
  head(out, 10)
  ### Plot
  
  plot.sir(out, times, legend.lbl = c("Susceptible", "Infected", "Recovered"), leg.off=c(-0.1, 0.3))
  
  # V = S0 * proportie_varstnici; [ex. 0.2]
  
}


# probability of infection;
# recovery rate
# death rate
# hospitalization
initSIR_Hosp_Com = function(list,end.time)
{
  times = seq(0,end.time, by = 1)
  # (!) S represents the nr of susceptible young people
  print(list["recov1"])
  #god bless us all, do not change the list
  parameters = c(infect = list[1],
                 recov = list[2], recov.h = list[3],
                 death=list[2]*list[4], death.h = list[3]*list[5],
                 hosp = list[6], hosp.v = list[7])
  print(parameters)
  init = c(T = 1 - 1e-6, S = (1 - 1e-6) * 0.8, I = 1e-6, R = 0.0, H = 0.0, V = (1 - 1e-6) * 0.2, D = 0.0, Dh = 0.0)
  out = solve.sir(sir_comunity, init, parameters, times)
  head(out, 10)
  ### Plot
  plot.sir(out, times, legend.lbl = c("Susceptible", "Young", "Infected", "Recovered", "Hosp", "Old", "DeathCommunity", "DeathHospital"), leg.off=c(-0.1, 0.3))
  
  # V = S0 * proportie_varstnici; [ex. 0.2]
  
}





