# Persoane tinere & persoane varstnice;
# - generati un grid cu un amestec de persoane varstnice si tinere;
# - probabilitatea de a se infecta: mai mare la persoanele varstnice;
# -- folositi deci pv > pt;
# - va puteti inspira din codul Percolate.R si creati o matrice cu 30%
#   persoane varstnice;



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


### 7 Bridges of Konigsberg
# n = number of bridges;
# w = width of bridge;
# y = position of bridge (relative pos if y < 0);
# evaluate: dy vs y as explicit parameters;
bridge = function(n, m, w=5, y){
	xdim = dim(m)[1];
	if(missing(y)) {
		nr = xdim %/% 2 - 10;
	} else if(any(y < 0)) {
		# negative offset from mid-line;
		nr = xdim %/% 2 + y;
	} else {
		nr = y;
	}
	len = round(xdim / n);
	x.mid = len %/% 2;
	br.x = seq(x.mid, x.mid + w, by=1);
	for(nc in seq(xdim)) {
		if((nc %% len) %in% br.x) next;
		m[nr, nc] = -1
	}
	invisible(m)
}


###############

### Init Raster
xdim = 200
mp = population(x=xdim)

m = newRaster(x=xdim)

m = bridge(5, m)
# m = bridge(5, m, c(-10, 10))

plot.rs(m)


### Model Parameters
pv = 0.15
pt = 0.10

surv.time = 5


### run Model
system.time( {
  inf.m = transmit.it(m, mp, pv=pv, pt=pt, surv.time=surv.time, maxIt=xdim*4) # 0.8
} )


### Analyze / Plot

# from file: Percolation.R
plot.rs(inf.m)

plot(inf.m[inf.m > 0])
plot(table(inf.m[inf.m > 0]))

# [old]
img = toRaster(inf.m)

old.par = par(mar=c(0,0,1,0) + 0.1)
plot(img)
par(old.par)

### Count infected per day
plot(table(inf.m[inf.m > 0]))


### Plot by Group
isInfected = (inf.m > 0);
y = inf.m[isInfected];
id = seq(prod(dim(inf.m)))[isInfected];
x.df = data.frame(id=id, time=y, group=mp[isInfected])
plot(id, y)

# library(ggplot2)
ggplot(x.df, aes(x=id, y=time, colour=as.factor(group))) +
	geom_point()
# TODO:
# How do we interpret this image?


### Alternative implementation

transmit.fast = function(m, mp, iter, pv=pv, pt=pt, surv.time) {
	recov = max(1, iter - surv.time);
	getProb = function(idOld) {
		if(idOld == 1) pv else pt;
	}
	# Infected Cells
	isInfect = (m >= recov);
	idInfect = which(isInfect);
	# Neighbours
	nRow = idInfect %% nrow(m);
	idN = c(idInfect-nrow(m), idInfect[nRow != 1]-1, idInfect[nRow != 0]+1, idInfect+nrow(m))
	idN = sort(idN);
	posStart = 1;
	while(posStart <= length(idN)) {
		if(idN[posStart] >= 1) break;
		posStart = posStart + 1;
	}
	posEnd = length(idN);
	posMax = prod(dim(m));
	while(posEnd >= 1) {
		if(idN[posEnd] <= posMax) break;
		posEnd = posEnd - 1;
	}
	idN = idN[posStart:posEnd];
	# Susceptible
	isSusc = (m[idN] == 0);
	idN = idN[isSusc];
	# New Infections
	p = sapply(idN, function(id) sample.it(iter, getProb(mp[id])))
	p = tapply(p, idN, max);
	m[unique(idN)] = p;
	invisible(m);
}

transmit.it = function(m, mp, pv, pt, surv.time, maxIt=50) {
  for(it in 1:maxIt) {
    m = transmit.fast(m, mp, it, pv, pt, surv.time=surv.time)
  }
  invisible(m)
}

### run Model
system.time( {
  inf.m = transmit.it(m, mp, pv=pv, pt=pt, surv.time=surv.time, maxIt=xdim*4) # 0.8
} )


