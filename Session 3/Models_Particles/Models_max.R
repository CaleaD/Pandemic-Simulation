# RUN TEST ON MAX

sample.it = function(it, p) {
  sample(c(it+1, 0), 1, prob=c(p, 1-p))
}


transmit = function(m, it, p, surv.time) {
  m2 = m;
  recov = max(1, it - surv.time);
  for(nc in 1:ncol(m)) {
    for(nr in 1:nrow(m)) {
      if(m[nr, nc] < recov) next;
      # Without max there seem to be more uninfected people
      # We have to use max to fix the situation where an infected cell might
      # Become uninfected in the next iteration
      # If it has already been infected, it should remain infected until recovered
      if(nc > 1 && m[nr, nc - 1] == 0) {
        m2[nr, nc-1] = max(m2[nr, nc-1], sample.it(it, p))
      }
      if(nc < ncol(m) && m[nr, nc + 1] == 0) {
        m2[nr, nc+1] = max(m2[nr, nc+], sample.it(it, p))
      }
      if(nr > 1 && m[nr - 1, nc] == 0) {
        m2[nr-1, nc] = max(m2[nr-1, nc], sample.it(it, p))
      }
      if(nr < nrow(m) && m[nr + 1, nc] == 0) {
        m2[nr+1, nc] = max(m2[nr+, nc], sample.it(it, p))
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
p = 0.10
surv.time = 5

xdim = 200
m = newRaster(x=xdim)

system.time( {
  inf.m = transmit.it(m, p=p, surv.time=surv.time, maxIt=xdim*4) # 0.8
} )

img = toRaster(inf.m)

old.par = par(mar=c(0,0,1,0) + 0.1)
plot(img)
par(old.par)


### TODO:
transmit = function(id, m, it, p) {
  # a more efficient implementation
  isI = (m[,id] >= it);
  isS = (m[,id] == 0);
  canI = (isS & c(FALSE, head(isI, -1)));
  canI = (c(FALSE, tail(isS, -1)) & isI);
}




