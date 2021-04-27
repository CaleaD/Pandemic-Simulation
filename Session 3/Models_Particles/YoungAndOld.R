#Persoane tinere & persoane varstnice;
# - generati un grid cu un amestec de persoane varstnice si tinere;
# - probabilitatea de a se infecta: mai mare la persoanele varstnice;
# -- folositi deci pv > pt;
# - va puteti inspira din codul Percolate.R si creati o matrice cu 30%
#   persoane varstnice;


library(SpaDES.core)

sample.it = function(it, p) {
  sample(c(it+1, 0), 1, prob=c(p, 1-p))
}

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

transmit.it = function(m, mp, pv, pt, surv.time, maxIt=50) {
  for(it in 1:maxIt) {
    m = transmit1(m, mp, it, pv, pt, surv.time=surv.time)
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

#A function which initializes the matrix with 30% old people 
population = function (x=80, y=x) {
  m = matrix(rbinom(x * y, 1, 0.7), ncol = x, nrow = y)
  invisible(m)
}

### Init
pv = 0.10
pt = 0.15

surv.time = 5


xdim = 200
m = newRaster(x=xdim)
mp = population(x=xdim)

system.time( {
  inf.m = transmit.it(m, mp, pv=pv, pt=pt, surv.time=surv.time, maxIt=xdim*4) # 0.8
} )

img = toRaster(inf.m)

old.par = par(mar=c(0,0,1,0) + 0.1)
plot(img)
par(old.par)


