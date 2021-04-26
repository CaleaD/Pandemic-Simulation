# - comparisons in Mortality vs different Vaccination rates;
#--------------------------------
#####################
#####################

### SIR + Vaccine
library(ggplot2)
library(deSolve)

solve.sir = function(sir.f, init, parameters, times) {
  ## Solve using ode (General Solver for Ordinary Differential Equations)
  out = ode(y = init, times = times, func = sir.f, parms = parameters)
  ## change to data frame
  out = as.data.frame(out)
  ## Delete time variable
  out$time <- NULL
  return(out)
}

sir <- function(time, state, parameters) {
  with(as.list(c(state, parameters)), {
    dV = min(S, vacc); # V protects, therefore reduces S;
    dS = -infect * S * I - infect * S * H - dV; # both I & H infect!
    dI =  infect * S * I + infect * S * H - recov * I - death * I - hosp * I;
    dH =  hosp * I - recov.h * H - death.h * H;
    dR =  recov * I + recov.h * H + dV;
    dD =  death * I + death.h * H;
    # Note: dR includes also dV;
    
    return(list(c(dS, dI, dR, dD, dH, dV)));
  })
}


# probability of infection;
infect = 1.4247 / 4
# recovery rate;
recov  = 0.14286 / c(1.2, 1.3)
# death rate;
death = recov * c(1/10, 1/2);
# hospitalization
hosp = 0.1;
# vaccination
vacc = 1 / 1000; # ~ 3 years for entire population;

parameters = c(infect = infect,
               recov = recov[1], recov.h = recov[2],
               death=death[1], death.h = death[2], hosp = hosp, vacc = vacc)
init = c(S = 1-1e-6, I = 1e-6, R = 0.0, D = 0.0, H = 0.0, V = 0.0)


### Solve using ode
out = solve.sir(sir, init, parameters, times)
head(out, 10)

### Plot Death Comparison

solve.SIR.Death = function(param, type="vacc") {
  type = match(type, c("infect", "recov","recov.h", "death", "death.h", "hosp", "vacc"))
  if(is.na(type)) stop("Invalid type!")
  parameters[type] = param
  solve.sir(sir, init, parameters, times)$D;
}

vacc_death.seq = vacc * seq(0, 10, by=2)
out = sapply(vacc_death.seq, solve.SIR.Death, type="vacc")
plot.sir(out, times, ylab="Mortality",
         legend.lbl=paste("Vaccination", vacc.seq), legend.xy=c(15,0.15))


### Plot Hospitalization Comparison


solve.SIR.Hosp = function(param, type="vacc") {
  type = match(type, c("infect", "recov","recov.h", "death", "death.h", "hosp", "vacc"))
  if(is.na(type)) stop("Invalid type!")
  parameters[type] = param
  solve.sir(sir, init, parameters, times)$H;
}

vacc_hosp.seq = vacc * seq(0, 10, by=1)
out = sapply(vacc_hosp.seq, solve.SIR.Hosp, type="vacc")
plot.sir(out, times, ylab="Hospitalization",
         legend.lbl=paste("Vaccination", vacc.seq), legend.xy=c(2,0.08))