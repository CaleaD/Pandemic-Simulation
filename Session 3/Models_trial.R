
library(SpaDES.core)

sample.it = function(it, p) {
  sample(c(it+1, 0), 1, prob=c(p, 1-p))
}



transmit1 = function(m, it, p, surv.time) {
  m2 = m;
  recov = max(1, it - surv.time);
  # We tried to run both functions with reversed cols and rows on larger matrices
  # At 500x500 we found that there is a delay of > 10-30 sec for transmit1 (which uses col -> rows).
  # This margin increases in relation with the matrix size.
  # For smaller matrices the difference between the two is nearly inconsequential.
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



transmit2 = function(m, it, p, surv.time) {
  m2 = m;
  recov = max(1, it - surv.time);
  for(nc in 1:nrow(m)) {
    for(nr in 1:ncol(m)) {
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

transmit1.it = function(m, p, surv.time, maxIt=50) {
  for(it in 1:maxIt) {
    m = transmit1(m, it, p, surv.time=surv.time)
  }
  invisible(m)
}

transmit2.it = function(m, p, surv.time, maxIt=50) {
  for(it in 1:maxIt) {
    m = transmit2(m, it, p, surv.time=surv.time)
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

xdim = 500
m = newRaster(x=xdim)

system.time( {
  inf.m = transmit1.it(m, p=p, surv.time=surv.time, maxIt=xdim*4) # 0.8
} )

system.time( {
  inf.m = transmit2.it(m, p=p, surv.time=surv.time, maxIt=xdim*4) # 0.8
} )

img = toRaster(inf.m)

old.par = par(mar=c(0,0,1,0) + 0.1)
plot(img)
par(old.par)

transmit = function(id, m, it, p) {
  # a more efficient implementation
  isI = (m[,id] >= it);
  isS = (m[,id] == 0);
  canI = (isS & c(FALSE, head(isI, -1)));
  canI = (c(FALSE, tail(isS, -1)) & isI);
}




