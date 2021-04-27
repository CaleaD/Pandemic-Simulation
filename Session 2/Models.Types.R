
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
spades(demoSim)




###-------------------------------------------------------------------------------

# install.packages("raster")

library(raster)

sample.it = function(it, p) {
  sample(c(it+1, 0), 1, prob=c(p, 1-p))
}

transmit = function(m, it, p, surv.time) {
  m2 = m;
  recov = max(1, it - surv.time);
  for(nc in 1:ncol(m)) {
    for(nr in 1:nrow(m)) {
      if(m[nr, nc] < recov) next;
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

### Init
m = matrix(0, nrow=80, ncol=80)
p = 0.2
surv.time = 5
mid = round(dim(m) / 2);
m[mid[1],mid[2]] = 1

maxIt = 50
for(it in 1:maxIt) {
  m = transmit(m, it, p, surv.time=surv.time)
  #display(m, "raster")
}

img = as.raster(m/max(m))
isITime = (maxIt - surv.time); # TODO: stack() ???

plot(img)

# img = addLayer(img, m2)

# display(m / max(m), "raster")



### TODO:
transmit = function(id, m, it, p) {
  isI = (m[,id] >= it);
  isS = (m[,id] == 0);
  canI = (isS & c(FALSE, head(isI, -1)));
  canI = (c(FALSE, tail(isS, -1)) & isI);
}




