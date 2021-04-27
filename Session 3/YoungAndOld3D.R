# Persoane tinere & persoane varstnice;
# - generati un grid cu un amestec de persoane varstnice si tinere;
# - probabilitatea de a se infecta: mai mare la persoanele varstnice;
# -- folositi deci pv > pt;
# - va puteti inspira din codul Percolate.R si creati o matrice cu 30%
#   persoane varstnice;


library(SpaDES.core)



sample.it = function(it, p) {
  sample(c(it+1, 0), 1, prob=c(p, 1-p))
}

transmit = function(m, it, pv, pt, surv.time) {
  m2 = m;
  recov = max(1, it - surv.time);
  getProb = function(idOld) {
    if(idOld == 1) pv else pt;
  }
  for(nc in 1:ncol(m)) {
    for(nr in 1:nrow(m)) {
      if(m[nr, nc, 1] < recov) next;
      # We verify whether the person is old or young and
      # apply the respective infection probability;
      # p = if (mp[nr,nc]==1) pt else pv;
      if(nc > 1 && m[nr, nc - 1, 1] == 0) {
        m2[nr, nc-1, 1] = max(m2[nr, nc-1, 1], sample.it(it, getProb(m2[nr, nc-1, 2])))
      }
      if(nc < ncol(m) && m[nr, nc + 1, 1] == 0) {
        m2[nr, nc+1, 1] = max(m2[nr, nc+1, 1], sample.it(it, getProb(m2[nr, nc+1, 2])))
      }
      if(nr > 1 && m[nr - 1, nc, 1] == 0) {
        m2[nr-1, nc, 1] = max(m2[nr-1, nc, 1], sample.it(it, getProb(m2[nr-1, nc, 2])))
      }
      if(nr < nrow(m) && m[nr + 1, nc, 1] == 0) {
        m2[nr+1, nc, 1] = max(m2[nr+1, nc, 1], sample.it(it, getProb(m2[nr+1, nc, 2])))
      }
    }
  }
  return(m2)
}

transmit.it = function(m, pv, pt, surv.time, maxIt=50) {
  for(it in 1:maxIt) {
    m = transmit(m, it, pv, pt, surv.time=surv.time)
  }
  invisible(m)
}

newRaster = function(x=80, y=x, val=0, setMid=TRUE, dim) {
  # Init 3d matrix with 0
  m = array(0, dim);
  # Init the 2nd layer with 30% old people 
  m[,,2] = matrix(rbinom(x * y, 1, 0.3), ncol = x, nrow = y)
  # Init the 1st layer with the infection starting in the middle
  if(setMid) {
    mid = round(dim(m) / 2);
    m[mid[1], mid[2],1] = 1
  }
  invisible(m)
}


###############

### Init Raster
xdim = 200;
dim = c(xdim, xdim, 2);
m = newRaster(x=xdim,dim=dim)


### Model Parameters
pv = 0.15
pt = 0.10

surv.time = 5


### run Model
system.time( {
  inf.m = transmit.it(m, pv=pv, pt=pt, surv.time=surv.time, maxIt=xdim*4) # 0.8
} )


### Analyze / Plot

# plot.rs(inf.m)
# from file: Percolation.R


### Raster pentru vizualizarea difuziei unei infectii
toRaster = function(m, showVal=0) {
  rs.m = array(0, c(dim(m), 3));
  if( ! is.na(showVal)) {
    isZero = (m == showVal);
    doShow = TRUE;
  } else {
    doShow = FALSE;
  }
  
  ### R
  layer.m = m;
  layer.m[m < 0] = 0
  val.max = max(layer.m);
  if(val.max > 0) layer.m = layer.m / val.max;
  if(doShow) layer.m[isZero] = 1;
  rs.m[,,1] = layer.m;
  
  ### G
  layer.m = 1 - layer.m;
  layer.m[m <= 0] = 0
  if(doShow) layer.m[isZero] = 1;
  rs.m[,,2] = layer.m
  
  ### B
  if(doShow) {
    layer.m = array(0, dim(m));
    layer.m[isZero] = 1;
    rs.m[,,3] = layer.m
  }
  
  rs.m = as.raster(rs.m)
  return(rs.m);
}



img = toRaster(inf.m[,,1])

old.par = par(mar=c(0,0,1,0) + 0.1)
### Count infected per day
plot(table(inf.m[inf.m > 0]))
plot(img)
par(old.par)


