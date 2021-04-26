
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


install.packages("SpaDES.core")


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

sample.it = function(it, p) {
	sample(c(it+1, 0), 1, prob=c(p, 1-p))
}
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
transmit.it = function(m, p, surv.time, maxIt=50) {
	for(it in 1:maxIt) {
		m = transmit(m, it, p, surv.time=surv.time)
		# display(m, "raster")
	}
	invisible(m)
}
newRaster = function(x=80, y=x, val=0, setMid=TRUE) {
	m = matrix(val, nrow=y, ncol=x)
	if(setMid) {
		mid = round(dim(m) / 2);
		m[mid[1], mid[2]] = 1
	}
	invisible(m)
}

### Init
p = 0.15
surv.time = 5

xdim = 200
m = newRaster(x=xdim)

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




