#####################
#####################

### SIR + Old Age + Hospital
# - different mortalities for Hospital vs Community;
# - different mortalities for Old age vs Young;
# - preferential vaccination in older individuals:
#   model with old age group;

### incarcare librarii
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


### Plot SIR
legend.xyf = function(times, x=c(0,0)) {
  c(max(times)*2/3, 0.7) + x;
}
plot.sir = function(y, times, legend.lbl=c("Susceptible", "Infected", "Recovered"),
                    legend.xy, leg.off=c(0,0), ylab="Susceptible and Recovered") {
  if(missing(legend.xy)) legend.xy=legend.xyf(times, leg.off)
  matplot(x = times, y = y, type = "l",
          xlab = "Time", ylab = ylab, main = "SIR Model",
          lwd = 1, lty = 1, bty = "l", col = 2:10)
  
  ## Add legend
  legend(legend.xy[1], legend.xy[2], legend.lbl,
         pch = 1, col = 2:10, bty = "n")
}

basic.lbl = c("Susceptible", "Infected", "Recovered");

# SIR Representation for Hospital vs Community mortality
sir.com <- function(time, state, parameters) {
  with(as.list(c(state, parameters)), {
    dS = -infect * S * I - infect * S * H; # both I & H infect!
    dOld = -infect * Old * I - infect * Old * H;
    dT = dS + dOld;
    dI =  infect * S * I + infect * S * H + infect * Old * I + infect * Old * H - recov * I - death * I - hosp * I;
    dH =  hosp * I - recov.h * H - death.h * H;
    dR =  recov * I + recov.h * H;
    dDc =  death * I;
    dDh =  death.h * H;
    return(list(c(dT, dS, dI, dR, dH, dOld, dDc, dDh)));
  })
}

# SIR Representation for Old vs Young mortality
sir.old <- function(time, state, parameters) {
  with(as.list(c(state, parameters)), {
    dS = -infect * S * (Is + Iv) - infect * S * H; # both I & H infect!
    dOld = -infect * Old * (Is + Iv) - infect * Old * H;
    dT = dS + dOld;
    dIs = - dS - recov * Is - death * Is - hospYoung * Is;
    #dIv = - dOld - recov * Iv - death * Iv - hospOld * Iv;
    dIv = - dOld - recov * Iv - death.v * Iv - hospOld * Iv; # death.v
    dH = hospYoung * Is + hospOld * Iv - death.h * H - recov.h * H;
    #dHc = hospYoung * Is + hospOld * Iv; #cummulative
    
    dR =  recov * Is + recov * Iv + recov.h * H;
    #dD =  death * Is + death * Iv + death.h * H;
    dD =  death * Is + death.v * Iv + death.h * H;
    return(list(c(dT, dS, dIs, dIv, dR, dD, dH, dOld)));
  })
}


# Hospital Representation for preferential vaccination in older individuals
sir.vacc <- function(time, state, parameters) {
  with(as.list(c(state, parameters)), {
    dV = min(S, vacc);
    dVo = min(Old, vacc.o);
    dS = -infect * S * (Is+Iv) - infect * S * H - dV; # both I & H infect! S = young
    dOld = -infect * Old * (Is+Iv) - infect * Old * H - dVo;
    dT = dS + dOld;
    #dI =  infect * S * I + infect * S * H + infect * O * I + infect * O * H - recov * I - death * I - hosp * I;
    dIs = - dS - recov * Is - death * Is - hospYoung * Is;
    dIv = - dOld - recov * Iv - death * Iv - hospOld * Iv;
    dH = hospYoung * Is + hospOld * Iv - death.h * H - recov.h * H;
    dR =  recov * Is + recov * Iv + recov.h * H;
    dD =  death * Is + death * Iv + death.h * H;
    #return(list(c(dT, dS, dI, dR, dD, dH, dV, dRo, dHo, dDo)));
    return(list(c(dT, dS, dIs, dIv, dR, dD, dH, dOld,dV,dVo)));
  })
}

# probability of infection;
infect = 1.4247 / 4
# recovery rate;
recov  = 0.14286 / c(1.2, 1.3, 1.5)
# death rate;
death = recov * c(1/10, 1/2, 1/8, 3/4);
# hospitalization
hosp = c(0.1, 0.5);
# vaccination rate
vacc = c(1/1000,1/500);

# (!) S represents the nr of susceptible young people

parameters_com = c(infect = infect,
               recov = recov[1], recov.h = recov[2],
               death=death[1], death.h = death[2],
               hosp = hosp[1], hosp.v = hosp[2])

parameters_old = c(infect = infect,
               recov = recov[1], recov.h = recov[2],
               death=death[1], death.h = death[2], death.v = death[3],
               hospYoung = hosp[1], hospOld = hosp[2])

parameters_vacc_old = c(infect = infect, vacc= vacc[1], vacc.o = vacc[2],
                       recov = recov[1], recov.h = recov[2],
                       hospYoung = hosp[1], hospOld = hosp[2],
                       death=death[1], death.h = death[2], death.o = death[3], death.oh = death[4], hosp = hosp[1], hosp.v = hosp[2])

init_com = c(T = 1 - 1e-6, S = (1 - 1e-6) * 0.8, I = 1e-6, R = 0.0, H = 0.0, Old = (1 - 1e-6) * 0.2, D = 0.0, Dh = 0.0)
#init_old = c(T = 1 - 1e-6, S = (1 - 1e-6) * 0.8, Is = 4e-7, Iv = 6e-7, R = 0.0, D = 0.0, H = 0.0, Old = (1 - 1e-6) * 0.2)
init_old = c(T = 1 - 1e-6, S = 0.8, Is = 1e-6, Iv = 0, R = 0.0, D = 0.0, H = 0.0, Old = 0.2)
init_vacc_old = c(T = 1 - 1e-6, S = (1 - 1e-6) * 0.8, Is = 4e-7, Iv = 6e-7, R = 0.0, D = 0.0, H = 0.0, Old = (1 - 1e-6) * 0.2, V =0.0, Vo = 0.0)


### Solve using ode
outputCom = solve.sir(sir.com, init_com, parameters_com, times)
outputOld = solve.sir(sir.old, init_old, parameters_old, times)
outputVacc = solve.sir(sir.vacc, init_vacc_old, parameters_vacc_old, times)
head(outputCom, 10)
head(outputOld, 10)
head(outputVacc, 10)

### Plot different mortalities for Hospital vs Community
plot.sir(outputCom, times, legend.lbl = c("Total", "Young", "Infected", "Recovered", "Hosp", "Old", "DeathCommunity", "DeathHospital"), leg.off=c(-0.2, 0.3))

### Plot different mortalities for Old age vs Young
plot.sir(outputOld, times, legend.lbl = c("Total", "Young", "InfectedSusc", "InfectedOld", "Recovered", "Death", "Hosp", "Old"), leg.off=c(-0.1, 0.3))

### Plot preferential vaccination in older individuals
plot.sir(out, times, legend.lbl = c("Total", "Young", "InfectedYoung", "InfectedOld","Recovered", "Death", "Hosp", "Old", "VaccinatedYoung", "VaccinatedOld"), leg.off=c(-0.1, 0.3))

