#a script file to implement custom functions

#Persoane tinere & persoane varstnice;
# - generati un grid cu un amestec de persoane varstnice si tinere;
# - probabilitatea de a se infecta: mai mare la persoanele varstnice;
# -- folositi deci pv > pt;
# - va puteti inspira din codul Percolate.R si creati o matrice cu 30%
#   persoane varstnice;



#multiple "#" means that there is a function between two sets

# TODO
# parametrise general functions(like solve.sir, plot.sir, etc.)



#This whole section is from the YOUNG&OLD.R script from TEAM
#library(SpaDES.core)
#############################################################################################
sample.it = function(it, p) {
  sample(c(it+1, 0), 1, prob=c(p, 1-p))
}
#############################################################################################
transmit = function(m, mp, it, pv=pv, pt=pt, surv.time) {
  m2 = m;
  recov = max(1, it - surv.time);
  for(nc in 1:ncol(m)) {
    for(nr in 1:nrow(m)) {
      if(m[nr, nc] < recov) next;
      # We verify whether the person is old or young and apply the respective inf probability
      if (mp[nr,nc]==1){
        if(nc > 1 && m[nr, nc - 1] == 0) {
          m2[nr, nc-1] = max(m2[nr, nc-1], sample.it(it, pt))
        }
        if(nc < ncol(m) && m[nr, nc + 1] == 0) {
          m2[nr, nc+1] = max(m2[nr, nc+1], sample.it(it, pt))
        }
        if(nr > 1 && m[nr - 1, nc] == 0) {
          m2[nr-1, nc] = max(m2[nr-1, nc], sample.it(it, pt))
        }
        if(nr < nrow(m) && m[nr + 1, nc] == 0) {
          m2[nr+1, nc] = max(m2[nr+1, nc], sample.it(it, pt))
        }
      }
      else {
        if(nc > 1 && m[nr, nc - 1] == 0) {
          m2[nr, nc-1] = max(m2[nr, nc-1], sample.it(it, pv))
        }
        if(nc < ncol(m) && m[nr, nc + 1] == 0) {
          m2[nr, nc+1] = max(m2[nr, nc+1], sample.it(it, pv))
        }
        if(nr > 1 && m[nr - 1, nc] == 0) {
          m2[nr-1, nc] = max(m2[nr-1, nc], sample.it(it, pv))
        }
        if(nr < nrow(m) && m[nr + 1, nc] == 0) {
          m2[nr+1, nc] = max(m2[nr+1, nc], sample.it(it, pv))
        }
      }
    }
  }
  return(m2)
}
#############################################################################################
transmit.it = function(m, mp, pv, pt, surv.time, maxIt=50) {
  for(it in 1:maxIt) {
    m = transmit(m, mp, it, pv, pt, surv.time=surv.time)
  }
  invisible(m)
}
#############################################################################################
newRaster = function(x=80, y=x, val=0, setMid=TRUE) {
  m = matrix(val, nrow=y, ncol=x)
  if(setMid) {
    mid = round(dim(m) / 2);
    m[mid[1], mid[2]] = 1
  }
  invisible(m)
}
#############################################################################################
#A function which initializes the matrix with 30% old people 
population = function (x=80, y=x) {
  m = matrix(rbinom(x * y, 1, 0.7), ncol = x, nrow = y)
  invisible(m)
}
#############################################################################################
### Init
initYOUNG_and_OLD = function(pv=0.1,pt=0.15,surv.time=5,xdim=200)
{
  m = newRaster(x=xdim)
  mp = population(x=xdim)
  system.time( {
    inf.m = transmit.it(m, mp, pv=pv, pt=pt, surv.time=surv.time, maxIt=xdim*4) # 0.8
  } )
  img = toRaster(inf.m)
  
  old.par = par(mar=c(0,0,1,0) + 0.1)
  plot(img)
  par(old.par)
}






































































#This whole section is from Models.Types(1).R from TEAM

##############
### Models ###
##############

### Plan for Modeling
# - will be discussed during next week;


### A.) Compartment Models
# - SIR & derivatives;
# - based on solving systems of ODEs;


### B.) Graphical Models
# - based on the compartment models;
# - but the parameters are NOT constant;
# - parameters are drawn from distributions;
# - will probably NOT go into detail during this semester;
#   [advanced statistics]


### C.) Event & Agent-based Models
# - Discrete event simulation;
# - Agent-based models;
# - packages: SpaDES;
# https://cran.r-project.org/web/packages/SpaDES.core/vignettes/i-introduction.html


#install.packages("SpaDES.core")


library(SpaDES.core)

#############################################################################################
demoSim <- suppressMessages(simInit(
  times = list(start = 0, end = 200),
  modules = "SpaDES_sampleModules",
  params = list(
    .globals = list(burnStats = "nPixelsBurned"),
    randomLandscapes = list(
      nx = 2e2, ny = 2e2, .saveObjects = "landscape",
      .plotInitialTime = NA, .plotInterval = NA, inRAM = TRUE
    ),
    caribouMovement = list(
      N = 1e2, .saveObjects = c("caribou"),
      .plotInitialTime = 1, .plotInterval = 1, moveInterval = 1
    ),
    fireSpread = list(
      nFires = 1e1, spreadprob = 0.235, persistprob = 0, its = 1e6,
      returnInterval = 10, startTime = 0,
      .plotInitialTime = 0, .plotInterval = 10
    )
  ),
  path = list(modulePath = system.file("sampleModules", package = "SpaDES.core"))
))
#spades=plot
#spades(demoSim)
#############################################################################################

### spread()
# - in SpaDES.tools;
# https://cran.r-project.org/web/packages/SpaDES.tools/SpaDES.tools.pdf
# TODO;

#####################

######################
### Explicit Model ###
######################

### helper Functions

# install.packages("raster")

# library(raster)
#############################################################################################
sample.it = function(it, p) {
  sample(c(it+1, 0), 1, prob=c(p, 1-p))
}
#############################################################################################
transmit = function(m, it, p, surv.time) {
  m2 = m;
  recov = max(1, it - surv.time);
  # TODO: verificat ordinea buclei: cum ruleaza mai repede?
  # sys.time()
  for(nc in 1:ncol(m)) {
    for(nr in 1:nrow(m)) {
      if(m[nr, nc] < recov) next;
      # TODO: cred ca max() NU e necesar;
      if(nc > 1 && m[nr, nc - 1] == 0) {
        m2[nr, nc-1] = max(m2[nr, nc-1], sample.it(it, p))
      }
      if(nc < ncol(m) && m[nr, nc + 1] == 0) {
        m2[nr, nc+1] = max(m2[nr, nc+1], sample.it(it, p))
      }
      if(nr > 1 && m[nr - 1, nc] == 0) {
        m2[nr-1, nc] = max(m2[nr-1, nc], sample.it(it, p))
      }
      if(nr < nrow(m) && m[nr + 1, nc] == 0) {
        m2[nr+1, nc] = max(m2[nr+1, nc], sample.it(it, p))
      }
    }
  }
  return(m2)
}
#############################################################################################
transmit.it = function(m, p, surv.time, maxIt=50) {
  for(it in 1:maxIt) {
    m = transmit(m, it, p, surv.time=surv.time)
    # display(m, "raster")
  }
  invisible(m)
}
#############################################################################################
newRaster = function(x=80, y=x, val=0, setMid=TRUE) {
  m = matrix(val, nrow=y, ncol=x)
  if(setMid) {
    mid = round(dim(m) / 2);
    m[mid[1], mid[2]] = 1
  }
  invisible(m)
}
#############################################################################################
### Init
initModels_Types_1 = function (p=0.15,surv.time=5,xdim=200)
{m = newRaster(x=xdim)
system.time( {
  inf.m = transmit.it(m, p=p, surv.time=surv.time, maxIt=xdim*4) # 0.8
} )
# sau folsoind Sys.time()

### toRaster()
# - from Percolation.R;
# - TODO: implement a cutoff = 5;
img = toRaster(inf.m)

old.par = par(mar=c(0,0,1,0) + 0.1)
plot(img)
par(old.par)
}

# package raster: not needed;
# img = addLayer(img, m2)


### TODO:
transmit = function(id, m, it, p) {
  # a more efficient implementation
  isI = (m[,id] >= it);
  isS = (m[,id] == 0);
  canI = (isS & c(FALSE, head(isI, -1)));
  canI = (c(FALSE, tail(isS, -1)) & isI);
}


###Models_max.R s very similar to Models.Types(1).R
###anyways from max i got this function
transmit = function(m, it, p, surv.time) {
  m2 = m;
  recov = max(1, it - surv.time);
  for(nc in 1:ncol(m)) {
    for(nr in 1:nrow(m)) {
      if(m[nr, nc] < recov) next;
      # Without max there seem to be more uninfected people although the reason of why remains unclear
      # for the time being
      if(nc > 1 && m[nr, nc - 1] == 0) {
        m2[nr, nc-1] = sample.it(it, p)
      }
      if(nc < ncol(m) && m[nr, nc + 1] == 0) {
        m2[nr, nc+1] = sample.it(it, p)
      }
      if(nr > 1 && m[nr - 1, nc] == 0) {
        m2[nr-1, nc] = sample.it(it, p)
      }
      if(nr < nrow(m) && m[nr + 1, nc] == 0) {
        m2[nr+1, nc] = sample.it(it, p)
      }
    }
  }
  return(m2)
}

















































#This whole secton is from SIR_Death_Comparison.r from TEAM


# - comparisons in Mortality vs different Vaccination rates;
#--------------------------------
#####################
#####################

### SIR + Vaccine

SIR_Death_Comparison <- function(time, state, parameters) {
  with(as.list(c(state, parameters)), {
    # TODO: verify (time > 500) & correct!
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
# recovery rate;
# death rate;
# hospitalization
# vaccination
# ~ 3 years for entire population;


# maybe turn the parameters into a list?
initSIR_Death_Comparison = function(infect = 1.4247 / 4,recov  = 0.14286 / c(1.2, 1.3),death = recov * c(1/10, 1/2),hosp = 0.1,vacc = 1 / 1000,vacc_seq= seq(1, 60, by=10),ylim=c(0,0.4))
{
  end.time = 120
  times = seq(0, end.time, by = 1)
  # TODO
  # If ylim exists, fix the legend, otherwise make the legend dynamic(moves with graph)
  # Parametrise the plot.sir to allow it to work for all different models
  plot.sir = function(y, times, legend.lbl=c("Susceptible", "Infected", "Recovered"),
                      legend.xy, leg.off=c(0,0), ylab="Susceptible and Recovered",ylim=NULL) {
    if(missing(legend.xy)) legend.xy=legend.xyf(times, leg.off)
    matplot(x = times, y = y, type = "l",
            xlab = "Time", ylab = ylab, main = "SIR Model",
            lwd = 1, lty = 1, bty = "l", col = 2:10)
    
    ## Add legend
    legend(legend.xy[1], legend.xy[2], legend.lbl,
           pch = 1, col = 2:10, bty = "n")
  }
  
  parameters = c(infect = infect,
                 recov = recov[1], recov.h = recov[2],
                 death=death[1], death.h = death[2], hosp = hosp, vacc = vacc)
  init = c(S = 1-1e-6, I = 1e-6, R = 0.0, D = 0.0, H = 0.0, V = 0.0)
  ### Solve using ode
  out = solve.sir(SIR_Death_Comparison, init, parameters, times)
  head(out, 10)
  #TODO select parameter
  #output type to init
  solve.SIR = function(param, type="vacc", output="D") {
    type = match(type, c("infect", "recov", "death", "hosp", "vacc"))
    if(is.na(type)) stop("Invalid type!")
    parameters[type] = param
    solve.sir(SIR_Death_Comparison, init, parameters, times)[,output];
  }
  
  vacc.seq = vacc * vacc_seq
  out = sapply(vacc.seq, solve.SIR, type="vacc")
  # If ylim is sent, fix legend, if not
  plot.sir(out, times, ylab="Mortality",                          #v turn this into a parameter
           legend.lbl=paste("Vaccination", vacc.seq), legend.xy=c(15,3/4*max(out))) # ylim becomes parameter
  #plot.sir(out, times, ylab="Mortality",legend.lbl=paste("Vaccination", death.seq), legend.xy=c(0,1))
}


























#This whole section is from SIR_Hosp_Com.R
#####################
#####################

### SIR + Old Age + Hospital
# - different mortalities for Hospital vs Community;

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


sir <- function(time, state, parameters) {
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

# probability of infection;
# recovery rate
# death rate
# hospitalization
initSIR_Hosp_Com = function(infect = 1.4247 / 4,recov  = 0.14286 / c(1.2, 1.3),death = recov * c(1/10, 1/2),hosp = c(0.1, 0.2))
{
  # (!) S represents the nr of susceptible young people
  parameters = c(infect = infect,
                 recov = recov[1], recov.h = recov[2],
                 death=death[1], death.h = death[2],
                 hosp = hosp[1], hosp.v = hosp[2])
  init = c(T = 1 - 1e-6, S = (1 - 1e-6) * 0.8, I = 1e-6, R = 0.0, H = 0.0, V = (1 - 1e-6) * 0.2, D = 0.0, Dh = 0.0)
  ### Solve using ode
  out = solve.sir(sir, init, parameters, times)
  head(out, 10)
  ### Plot
  plot.sir(out, times, legend.lbl = c("Total", "Young", "Infected", "Recovered", "Hosp", "Old", "DeathCommunity", "DeathHospital"), leg.off=c(-0.1, 0.3))
  
  # V = S0 * proportie_varstnici; [ex. 0.2]
  
}














# - comparisons in Hospitalization vs different Vaccination rates;

sir <- function(time, state, parameters) {
  with(as.list(c(state, parameters)), {
    # TODO: verify (time > 500) & correct!
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
# recovery rate;
# death rate;
# hospitalization
# vaccination
# ~ 3 years for entire population
initSIR_Hosp_Comparison = function(infect = 1.4247 / 4,recov  = 0.14286 / c(1.2, 1.3),death = recov * c(1/10, 1/2),hosp = 0.1,vacc = 1 / 1000)
{

parameters = c(infect = infect,
               recov = recov[1], recov.h = recov[2],
               death=death[1], death.h = death[2], hosp = hosp, vacc = vacc)
init = c(S = 1-1e-6, I = 1e-6, R = 0.0, D = 0.0, H = 0.0, V = 0.0)


### Solve using ode
out = solve.sir(sir, init, parameters, times)
head(out, 10)

### Plot


### TODO:
# - comparisons in Mortality & Hospitalization
#   vs different Vaccination rates;

solve.SIR = function(param, type="vacc") {
  type = match(type, c("infect", "recov", "death", "hosp", "vacc"))
  if(is.na(type)) stop("Invalid type!")
  parameters[type] = param
  solve.sir(sir, init, parameters, times)$H;
}

vacc.seq = vacc * seq(1, 60, by=10)
out = sapply(vacc.seq, solve.SIR, type="vacc")
plot.sir(out, times, ylab="Hospitalization",
         legend.lbl=paste("Vaccination", vacc.seq), legend.xy=c(10,0.1))
#plot.sir(out, times, ylab="Mortality",legend.lbl=paste("Vaccination", death.seq), legend.xy=c(0,1))

}





























#this whole section is from SIR_Hosp_Old from TEAM
#####################
#####################

### SIR + Old Age + Hospital
# - different mortalities for Old age vs Young;

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


sir <- function(time, state, parameters) {
  with(as.list(c(state, parameters)), {
    dS = -infect * S * I - infect * S * H; # both I & H infect!
    dV = -infect * V * I - infect * V * H;
    dT = (-infect * S * I - infect * S * H) + (-infect * V * I - infect * V * H);
    dI =  infect * S * I + infect * S * H + infect * V * I + infect * V * H - recov * I - death * I - hosp * I;
    dH =  hosp * I - recov.h * H - death.h * H;
    #dH =  hosp * (I * 0.8) - recov.h * H - death.h * H;
    #dHo = hosp.v * (I * 0.2) - recov.v * Ho - death.v * Ho;
    dR =  recov * I + recov.h * H;
    #dR =  recov * I + recov.h * H;
    #dRo = recov.v * I + recov.h * H;
    dD =  death * (I * 0.8) + death.h * H;
    dDo = death.v * (I * 0.2) + death.vh * H;
    #return(list(c(dT, dS, dI, dR, dD, dH, dV, dRo, dHo, dDo)));
    return(list(c(dT, dS, dI, dR, dD, dH, dV, dDo)));
  })
}
# probability of infection;
# recovery rate;
# death rate;
# hospitalization
initSIR_Hosp_Old= function(infect = 1.4247 / 4,recov  = 0.14286 / c(1.2, 1.3, 1.5),death = recov * c(1/10, 1/2, 1/5, 3/4),hosp = c(0.1, 0.2))
{


# (!) S represents the nr of susceptible young people

parameters = c(infect = infect,
               recov = recov[1], recov.h = recov[2], recov.v = recov[3],
               death=death[1], death.h = death[2], death.v = death[3], death.vh = death[4], hosp = hosp[1], hosp.v = hosp[2])
init = c(T = 1 - 1e-6, S = (1 - 1e-6) * 0.8, I = 1e-6, R = 0.0, D = 0.0, H = 0.0, V = (1 - 1e-6) * 0.2, Do = 0.0)


### Solve using ode
out = solve.sir(sir, init, parameters, times)
head(out, 10)

### Plot
plot.sir(out, times, legend.lbl = c("Total", "Young", "Infected", "Recovered", "Death", "Hosp", "Old", "OldDeath"), leg.off=c(-0.1, 0.3))

# V = S0 * proportie_varstnici; [ex. 0.2]
}


























#this whole section is from SIR_Vacc_OlderGroup from TEAM
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
          lwd = 1, lty = 1, bty = "l", col = 2:9)
  
  ## Add legend
  legend(legend.xy[1], legend.xy[2], legend.lbl,
         pch = 1, col = 2:9, bty = "n")
}

basic.lbl = c("Susceptible", "Infected", "Recovered");


sir <- function(time, state, parameters) {
  with(as.list(c(state, parameters)), {
    dV = min(S, vacc);
    dVo = min(O, vacc.o);
    dS = -infect * S * I - infect * S * H - dV; # both I & H infect! S = young
    dO = -infect * O * I - infect * O * H - dVo;
    dT = (-infect * S * I - infect * S * H) + (-infect * O * I - infect * O * H) - dV;
    dI =  infect * S * I + infect * S * H + infect * O * I + infect * O * H - recov * I - death * I - hosp * I;
    dH =  hosp * I - recov.h * H - death.h * H;
    dR =  recov * I + recov.h * H;
    dD =  death * (I * 0.8) + death.h * H;
    dDo = death.o * (I * 0.2) + death.oh * H;
    #return(list(c(dT, dS, dI, dR, dD, dH, dV, dRo, dHo, dDo)));
    return(list(c(dT, dS, dI, dR, dD, dH, dO, dDo,dV,dVo)));
  })
}

# probability of infection;
# recovery rate;
# death rate;
# hospitalization
# vaccination rate
initSIR_VAacc_Older = function(infect = 1.4247 / 4,recov  = 0.14286 / c(1.2, 1.3, 1.5),death = recov * c(1/10, 1/2, 1/5, 3/4),hosp = c(0.1, 0.2),vacc = c(1/1000,1/500))
{


# (!) S represents the nr of susceptible young people

parameters = c(infect = infect, vacc= vacc[1], vacc.o = vacc[2],
               recov = recov[1], recov.h = recov[2],
               death=death[1], death.h = death[2], death.o = death[3], death.oh = death[4], hosp = hosp[1], hosp.v = hosp[2])
init = c(T = 1 - 1e-6, S = (1 - 1e-6) * 0.8, I = 1e-6, R = 0.0, D = 0.0, H = 0.0, O = (1 - 1e-6) * 0.2, Do = 0.0, V =0.0, Vo = 0.0)


### Solve using ode
out = solve.sir(sir, init, parameters, times)
head(out, 10)

### Plot
plot.sir(out, times, legend.lbl = c("Total", "Young", "Infected", "Recovered", "Death", "Hosp", "Old", "OldDeath", "VaccinatedYoung", "VaccinatedOld"), leg.off=c(-0.1, 0.3))

# O = S0 * proportie_varstnici; [ex. 0.2]
}






















#this whole section is from TwoViruses3D.R from TEAM
#2.2.) 2 Tipuri de virus
# - V1 cu p1 si V2 cu p2;
# - inoculati 2 celule la 1/3 si 2/3 pe axa OX;
# - de experimentat: delay la inocularea cu V2 (unde p2 > p1, dar infectia
#   incepe mai tarziu cu cateva zile);
# - sa va ganditi la concepte cum putem sa implementam acest model:
#  structuri date (array in 3D vs alt tip de codificare?), codificarea
#  tulpinilor, etc;


sample.it = function(it, p) {
  sample(c(it+1, 0), 1, prob=c(p, 1-p))
}

transmit = function(m, it, v2, p1, p2, surv.time) {
  m2 = m;
  recov = max(1, it - surv.time);
  getProb = function(idVirus) {
    if(v2==TRUE){
      if(idVirus == 1) p1
      else p2;
    }
    else {
      p1;
    }
  }
  for(nc in 1:ncol(m)) {
    for(nr in 1:nrow(m)) {
      if(m[nr, nc, 1] < recov) next;
      if(m[nr, nc, 1] >= recov) {
        if(nc > 1 && m[nr, nc - 1, 1] == 0) {
          # inf states whether the neighbor becomes infected using the appropriate probability for v1 and v2
          inf = max(m2[nr, nc-1, 1], sample.it(it, getProb(m[nr, nc, 2])))
          # Place the result in the first layer
          m2[nr, nc-1, 1] = inf
          # If there has been an infection and the subject was not previously infected with another virus,
          # Place the virus id in the second layer, otherwise it stays the same.
          if(inf==1 && m2[nr, nc-1, 2]==0) m2[nr, nc-1, 2] = m2[nr, nc, 2];
        }
        if(nc < ncol(m) && m[nr, nc + 1, 1] == 0) {
          inf = max(m2[nr, nc+1, 1], sample.it(it, getProb(m[nr, nc, 2])))
          m2[nr, nc+1, 1] = inf
          if(inf==1 && m2[nr, nc+1, 2]==0) m2[nr, nc+1, 2] =  m2[nr, nc, 2];
        }
        if(nr > 1 && m[nr - 1, nc, 1] == 0) {
          inf =  max(m2[nr-1, nc, 1], sample.it(it, getProb(m[nr, nc, 2])))
          m2[nr-1, nc, 1] = inf
          if(inf==1 && m2[nr-1, nc, 2]==0) m2[nr-1, nc, 2] =  m[nr, nc, 2];
        }
        if(nr < nrow(m) && m[nr + 1, nc, 1] == 0) {
          inf = max(m2[nr+1, nc, 1], sample.it(it, getProb(m[nr, nc, 2])))
          m2[nr+1, nc, 1] = inf
          if(inf==1 && m2[nr+1, nc, 2]==0) m2[nr+1, nc, 2] =  m[nr, nc, 2];
        }
      }
    }
  }
  return(m2)
}


#We check if V2 became active at iteration inf.time
transmit.it = function(m, p1, p2, surv.time, inf.time, maxIt=50) {
  for(it in 1:maxIt) {
    if(it >= inf.time){
      m = transmit(m, it, v2 = TRUE, p1, p2, surv.time=surv.time)
    }
    else {
      m = transmit(m, it, v2 = FALSE, p1, p2, surv.time=surv.time)
    }
  }
  invisible(m)
}


#Viruses initialization in the population matrix
newRaster = function(x=80, y=x, val=0, setV1=TRUE, setV2=TRUE, dim) {
  # Init 3d matrix with 0
  m = array(0, dim);
  # Init the layers with the infections starting positions
  mid = round(dim(m) / 2);
  if(setV1) {
    start = round(dim(m) * 1/3);
    m[mid[2], start[1], 2] = 1
    m[mid[2], start[1], 1] = 1
  }
  if(setV2) {
    start = round(dim(m) * 2/3);
    m[mid[2], start[1], 2] = 2
    m[mid[2], start[1], 1] = 1
  }
  invisible(m)
}

initTwoViruses = function(p1 = 0.15,p2 = 0.20,surv.time = 5,inf.time = 20,xdim = 200)
{
#inf.time --> time until v2 starts infecting people
### Init Raster
;
dim = c(xdim, xdim, 2);
m = newRaster(x=xdim,dim=dim)

system.time( {
  inf.m = transmit.it(m, p1=p1, p2=p2, surv.time=surv.time, inf.time=inf.time, maxIt=xdim*4) # 0.8
} )

img = toRaster(inf.m[,,1])

old.par = par(mar=c(0,0,1,0) + 0.1)
plot(img)
par(old.par)

}




#TwoViruses.r has not been included, instead, I added TwoViruses3D.r
















































































































#check the other asRaster

#Scripts from Leo, what to add?