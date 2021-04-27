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
  for(nc in 1:ncol(m)) {
    for(nr in 1:nrow(m)) {
      if(m[nr, nc] < recov) next;
      # V1
      if(m[nr, nc] >= recov) {
        if(nc > 1 && m[nr, nc - 1] == 0) {
          m2[nr, nc-1] = max(m2[nr, nc-1], sample.it(it, p1))
        }
        if(nc < ncol(m) && m[nr, nc + 1] == 0) {
          m2[nr, nc+1] = max(m2[nr, nc+1], sample.it(it, p1))
        }
        if(nr > 1 && m[nr - 1, nc] == 0) {
          m2[nr-1, nc] = max(m2[nr-1, nc], sample.it(it, p1))
        }
        if(nr < nrow(m) && m[nr + 1, nc] == 0) {
          m2[nr+1, nc] = max(m2[nr+1, nc], sample.it(it, p1))
        }
      }
      #V2
      if(m[nr, nc] >= recov && v2 == T) {
        if(nc > 1 && m[nr, nc - 1] == 0) {
          m2[nr, nc-1] = max(m2[nr, nc-1], sample.it(it, p2))
        }
        if(nc < ncol(m) && m[nr, nc + 1] == 0) {
          m2[nr, nc+1] = max(m2[nr, nc+1], sample.it(it, p2))
        }
        if(nr > 1 && m[nr - 1, nc] == 0) {
          m2[nr-1, nc] = max(m2[nr-1, nc], sample.it(it, p2))
        }
        if(nr < nrow(m) && m[nr + 1, nc] == 0) {
          m2[nr+1, nc] = max(m2[nr+1, nc], sample.it(it, p2))
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
newRaster = function(x=80, y=x, val=0, setV1=TRUE, setV2=TRUE) {
  m = matrix(val, nrow=y, ncol=x)
  mid = round(dim(m) / 2);
  if(setV1) {
    start = round(dim(m) * 1/3);
    m[mid[2], start[1]] = 1
  }
  if(setV2) {
    start = round(dim(m) * 2/3);
    m[mid[2], start[1]] = 2
  }
  invisible(m)
}


### Init
p1 = 0.20
p2 = 0.25

surv.time = 5
#inf.time --> time until v2 starts infecting people
inf.time = 20

xdim = 100
m = newRaster(x=xdim)

system.time( {
  inf.m = transmit.it(m, p1=p1, p2=p2, surv.time=surv.time, inf.time=inf.time, maxIt=xdim*4) # 0.8
} )

img = toRaster(inf.m)

old.par = par(mar=c(0,0,1,0) + 0.1)
plot(img)
par(old.par)


