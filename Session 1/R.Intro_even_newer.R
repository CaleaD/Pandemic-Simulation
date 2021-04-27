
###############
### R Intro ###
###############

### Link:
# https://cran.r-project.org/

# Instalare pachete:
# install.packages("_nume_pachet_")

### Vizualizare / Grafica
# install.packages("ggplot2")

### Ecuatii diferentiale
# install.packages("deSolve")

#################

### incarcare librarii
library(ggplot2)
library(deSolve)

##################
##################

#################
### SIR Model ###
#################

# S = Susceptible
# I = Infected
# R = Removed: Immune (recovered) + Dead;

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
times = seq(0, end.time, by = 1)

### Proportion in each compartment:
# Susceptible = 0.999999;
# Infected = 0.000001;
# Recovered = 0;
init = c(S = 1-1e-6, I = 1e-6, R = 0.0)

# probability of infection;
infect = 1.4247 / 4
# recovery rate;
recov  = 0.14286 / 1.2

parameters = c(infect = infect, recov = recov)


### Solve using ode
out = solve.sir(sir, init, parameters, times)
head(out, 10)

### Plot 2
plot.sir(out, times)

min(out$S)



#############
#############

solve.SIR = function(param, type="infect") {
	type = match(type, c("infect", "recov"))
	if(type == 1) {
		parameters = c(infect = param, recov = recov)
	} else if(type == 2) {
		parameters = c(infect = infect, recov = param)
	}
	solve.sir(sir, init, parameters, times)$S;
}

# probability of infection;
infect = 1.4247 / 4
# recovery rate;
recov  = 0.14286 / 1.2

infect.seq = c(0.2, 0.3, 0.4, 0.5, 0.6, seq(0.8, 3, by=0.4))
out = sapply(infect.seq, solve.SIR)
plot.sir(out, times, legend.lbl=paste("inf rate", infect.seq), ylab="Susceptibles")


#####################
#####################

### SIR + Death Model

sir <- function(time, state, parameters) {
  with(as.list(c(state, parameters)), {
    dS = -infect * S * I;
    dI =  infect * S * I - recov * I - death * I; 
    dR =  recov * I;
  	dD =  death * I;
    return(list(c(dS, dI, dR, dD)));
  })
}


# probability of infection;
infect = 1.4247 / 4
# recovery rate;
recov  = 0.14286 / 1.2
# death rate;
death = recov / 3;

parameters = c(infect = infect, recov = recov, death=death)
init = c(S = 1-1e-6, I = 1e-6, R = 0.0, D = 0.0)


### Solve using ode
out = solve.sir(sir, init, parameters, times)
head(out, 10)

### Plot 3
plot.sir(out, times, legend.lbl = c(basic.lbl, "Death"), leg.off=c(0, -0.2))


### Comparisons
solve.SIR = function(param, type="infect") {
	type = match(type, c("infect", "recov", "death", "hosp"))
	if(is.na(type)) stop("Invalid type!")
	parameters[type] = param
	solve.sir(sir, init, parameters, times)$D;
}



death.seq = recov * seq(0.1, 0.6, by=0.1) #?
out = sapply(death.seq, solve.SIR, type="death")
plot.sir(out, times, legend.lbl=paste("Death", death.seq), legend.xy=c(10,0.25))



#####################
#####################

### SIR + Hospital
# - different mortalities in Hospital vs Community

sir <- function(time, state, parameters) {
  with(as.list(c(state, parameters)), {
    dS = -infect * S * I - infect * S * H; # both I & H infect!
    dI =  infect * S * I + infect * S * H - recov * I - death * I - hosp * I;
	  dH =  hosp * I - recov.h * H - death.h * H;
    dR =  recov * I + recov.h * H;
	  dD =  death * I + death.h * H;

    return(list(c(dS, dI, dR, dD, dH)));
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

parameters = c(infect = infect,
	recov = recov[1], recov.h = recov[2],
	death=death[1], death.h = death[2], hosp = hosp)
init = c(S = 1 - 1e-6, I = 1e-6, R = 0.0, D = 0.0, H = 0.0)


### Solve using ode
out = solve.sir(sir, init, parameters, times)
head(out, 10)

### Plot
plot.sir(out, times, legend.lbl = c(basic.lbl, "Death", "Hosp"), leg.off=c(0, -0.2))


### Comparisons
solve.SIR = function(param, type="infect") {
	type = match(type, c("infect", "recov", "death", "hosp"))
	if(is.na(type)) stop("Invalid type!")
	parameters[type] = param
	solve.sir(sir, init, parameters, times)$H;
}

infect.seq = c(0.2, 0.3, 0.4, 0.5, 0.6, seq(0.8, 3, by=0.4))
out = sapply(infect.seq, solve.SIR, type="infect")
plot.sir(out, times, ylab="Hospitalized",
	legend.lbl=paste("Infect", infect.seq), legend.xy=c(70,0.185))



#####################
#####################

### SIR + Old Age + Hospital
# - different mortalities for Old age vs Young;
# - different mortalities for Hospital vs Community;

### TODO: homework;


# V = S0 * proportie_varstnici; [ex. 0.2]



#####################
#####################

### SIR + Vaccine

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

### Plot
plot.sir(out, times, legend.lbl = c(basic.lbl, "Death", "Hosp", "Vacc"), leg.off=c(0, -0.2))


### TODO:
# - comparisons in Mortality & Hospitalization
#   vs different Vaccination rates;
# - preferential vaccination in older individuals:
#   model with old age group;


#####################
#####################

### SIR + Hospital + Superinfection

### TODO:
# - add Term: superinfect * S * I^2;
# - evaluate impact of "superinfection" on hospitalization (H):
#   superinfect = seq(0, 10, by=1);



### SIR + Hospital(S) + Hospital(I)

### TODO:
# - Hospital:
#   Hn = non-infected, Hi = infected hospitalized;
#   Hi: infects only Hn;
#   dHn = - infectH * Hn * Hi;
#   dHi = ... + infectH * Hn * Hi;



### Vizualizare

### TODO (urmatoarele ore)
# - interfete interactive: shiny app, dashboard;


