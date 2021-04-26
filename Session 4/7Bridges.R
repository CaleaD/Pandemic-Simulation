
library(SpaDES.core)
library(splus2R)



sample.it = function(it, p) {
  sample(c(it+1, 0), 1, prob=c(p, 1-p))
}

transmit = function(m, mp, it, pv=pv, pt=pt, surv.time) {
  m2 = m;
  recov = max(1, it - surv.time);
  getProb = function(idOld) {
    if(idOld == 1) pv else pt;
  }
  for(nc in 1:ncol(m)) {
    for(nr in 1:nrow(m)) {
      if(m[nr, nc] < recov) next;
      # We verify whether the person is old or young and
      # apply the respective infection probability;
      # p = if (mp[nr,nc] == 1) pt else pv; # wrong person;
      if(nc > 1 && m[nr, nc - 1] == 0) {
        m2[nr, nc-1] = max(m2[nr, nc-1], sample.it(it, getProb(mp[nr, nc-1])))
      }
      if(nc < ncol(m) && m[nr, nc + 1] == 0) {
        m2[nr, nc+1] = max(m2[nr, nc+1], sample.it(it, getProb(mp[nr, nc+1])))
      }
      if(nr > 1 && m[nr - 1, nc] == 0) {
        m2[nr-1, nc] = max(m2[nr-1, nc], sample.it(it, getProb(mp[nr-1, nc])))
      }
      if(nr < nrow(m) && m[nr + 1, nc] == 0) {
        m2[nr+1, nc] = max(m2[nr+1, nc], sample.it(it, getProb(mp[nr+1, nc])))
      }
    }
  }
  return(m2)
}

transmit.it = function(m, mp, pv, pt, surv.time, maxIt=50) {
  for(it in 1:maxIt) {
    m = transmit(m, mp, it, pv, pt, surv.time=surv.time)
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

# initializes the matrix with 30% old people 
population = function (x=80, y=x, p=0.3) {
  m = matrix(rbinom(x * y, 1, p), ncol = x, nrow = y)
  invisible(m)
}

### TODO:
# - evaluate alternative implementations:
#   array(as.integer(0), c(x, y, 2));
#   array(as.integer(0), c(2, x, y));
#   array(as.double(0), c(x, y, 2));
#   array(as.double(0), c(2, x, y));
#   2 separate matrices;
#   list(t = _time_matrix_, p = _population_matrix_ );


###############

### Init Raster
xdim = 150
mp = population(x=xdim)

m = newRaster(x=xdim)


### 7 Bridges of Konigsberg
# TODO: bridges(n, w=5, y=-10)
# m = initial matrix
# n = number of bridges;
# w = width of bridge;
# y = position of bridge (relative pos if y < 0);
# install.packages('splus2R') -> is.missing
bridge = function(m, n, w, y){
  if(is.missing(y)){
    y = mid()-10;
  }
  else if(y<0){
    y = mid()+y;
  } 
  nr = xdim %/% 2 + y;
  x = xdim/n;
  for(nc in seq(xdim)) {
    if((nc %% x) %in% 0:(w-1)) next;
    m[nr, nc] = -1;
  }
  invisible(m)
}


m = bridge(m, 5,1,y=20)


plot.rs(m)


### Model Parameters
pv = 0.15
pt = 0.10

surv.time = 5


### run Model
system.time( {
  inf.m = transmit.it(m, mp, pv=pv, pt=pt, surv.time=surv.time, maxIt=xdim*4) # 0.8
} )


plot.rs(inf.m)
# from file: Percolation.R
plot(inf.m[inf.m > 0])
plot(table(inf.m[inf.m > 0]))

img = toRaster(inf.m)

old.par = par(mar=c(0,0,1,0) + 0.1)
plot(img)
par(old.par)

### Count infected per day
plot(table(inf.m[inf.m > 0]))
