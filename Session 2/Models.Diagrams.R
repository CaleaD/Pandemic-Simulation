
########################
### Models: Diagrams ###
########################

# install.packages("shape")

library(shape)


####################

### helper functions

### line-arrow
lnarrow = function(xy1, xy2, r=r0, head.jt=0.01, lwd=1.5, ...) {
	tol = 0.002;
	if(abs(xy1[2] - xy2[2]) <= tol) {
		if(xy2[1] >= xy1[1]) {
			Arrows(xy1[1] + r, xy1[2], xy2[1] - r - head.jt, xy2[2], lwd=lwd, ...)
		} else {
			Arrows(xy1[1] - r - head.jt, xy1[2], xy2[1] + r, xy2[2], lwd=lwd, ...)
		}
	} else if(abs(xy1[1] - xy2[1]) <= tol) {
		if(xy2[2] >= xy1[2]) {
			Arrows(xy1[1], xy1[2] + r, xy2[1], xy2[2] - r - head.jt, lwd=lwd, ...)
		} else {
			Arrows(xy1[1], xy1[2] - r, xy2[1], xy2[2] + r + head.jt, lwd=lwd, ...)
		}
	} else {
		slope = (xy2[2] - xy1[2]) / (xy2[1] - xy1[1])
		slope.sc = r / sqrt(1 + slope^2);
		rx = slope.sc; ry = slope * slope.sc;
		if(xy1[1] < xy2[1]) {
			Arrows(xy1[1] + rx, xy1[2] + ry,
				xy2[1] - rx - head.jt, xy2[2] - ry - sign(slope)*head.jt, lwd=lwd, ...)
		} else {
			# TODO
			ry = - ry;
			Arrows(xy1[1] - rx, xy1[2] + ry,
				xy2[1] + rx + head.jt, xy2[2] - ry + sign(slope)*head.jt, lwd=lwd, ...)
		}
	}
}
### curved-arrow
cvarrow = function(xy, col, w=w0, r=r0, mid.off = c(0.005, 0.035)) {
	xy = xy - c(r, 0);
	filledcircle(r/2 + w, r/2, mid=xy + mid.off, from=pi/4, to=-pi, col=col)
	Arrowhead(xy[1] - r/2, xy[2] + 2*w, angle=-90, arr.type="triangle", lcol=col)
	# TODO: other orientations;
}
circle = function(txt, xy, r=r0, col) {
	filledcircle(r, mid=xy, col=col)
	text(xy[1], xy[2], txt)
}

r0 = 0.1;
w0 = 0.01;


################

################
### Diagrams ###
################

x0 = 0.05;
y0 = 0.6;

### SIR + Hospital + Death

#dev.new(width = 11.7, height = 8.3)
# png(file="Diagram.Model.H.png", width = 11.7, height = 8.3, units="in", res=100) # run this to save as png;
par.old = par(mar=c(0,0,2,0) + 0.01)
emptyplot(main = "SIR Model")

### Compartments
xyS = c(x0, y0);
circle("S", xyS, col="yellow");

xyI = c(x0 + 0.3, y0);
circle("I", xyI, col="red");

xyHs = c(x0, y0 - 3*r0);
circle("Hs", xyHs, col="grey");

xyHi = c(x0 + 0.3, y0 - 3*r0);
circle("Hi", xyHi, col="indianred1");

### R & D
xyR = c(x0 + 0.7, y0);
circle("R", xyR, col="green");

xyD = c(x0 + 0.7, y0 - 3*r0);
circle("D", xyD, col="indianred1");


### Arrows
lnarrow(xyS, xyI)
lnarrow(xyHs, xyHi)
# R & D
lnarrow(xyI, xyR)
lnarrow(xyI, xyD)
lnarrow(xyHi, xyR)
lnarrow(xyHi, xyD)

x.jt = c(0.005, 0)
lnarrow(xyS + x.jt, xyHs + x.jt)
lnarrow(xyHs - x.jt, xyS - x.jt)
lnarrow(xyI, xyHi)


cvarrow(xyI, col="red")
cvarrow(xyHi, col="indianred1")


#######################
#######################

### SIR: Old age
# including Hospital + Death

### TODO: Extensions
# - H => (S -> I);
# - H => Hs + Hi;

x0 = 0.02;
y0 = 0.6;

#dev.new(width = 11.7, height = 8.3)
# png(file="Diagram.Model.oldAge.png", width = 11.7, height = 8.3, units="in", res=100) # run this to save as png;
par.old = par(mar=c(0,0,2,0) + 0.01)
emptyplot(main = "SIR Model")

### Compartments

### S & Old
xyS = c(x0, y0);
circle("S", xyS, col="yellow");

xyO = c(x0, y0 - 3*r0);
circle("Old", xyO, col="grey");

### Is & Iv
xyIs = c(x0 + 0.3, y0);
circle("Is", xyIs, col="red");

xyIv = c(x0 + 0.3, y0 - 3*r0);
circle("Iv", xyIv, col="indianred1");

### H
# - only H due to Infection;
xyH = c(x0 + 0.65, y0 + 0.25);
circle("H", xyH, col="orange");

### R & D
xyR = c(x0 + 1, y0);
circle("R", xyR, col="green");

xyD = c(x0 + 1, y0 - 3*r0);
circle("D", xyD, col="indianred1");


### Arrows
lnarrow(xyS, xyIs)
lnarrow(xyO, xyIv)

cvarrow(xyIs, col="red")
cvarrow(xyIv, col="indianred1")

lnarrow(xyIs, xyR)
lnarrow(xyIs, xyD)
lnarrow(xyIv, xyR)
lnarrow(xyIv, xyD)

# Hospitalization
lnarrow(xyIv, xyH)
lnarrow(xyIs, xyH)
# Outcome from H:
lnarrow(xyH, xyR)
lnarrow(xyH, xyD)

# png(file="Diagram.Model.oldAge.png", width = 11.7, height = 8.3, units="in", res=100)
	# ... run specific code: without dev.new()!
# dev.off() # close file



#######################
#######################

#####################
### Save Diagrams ###
#####################

# - run the specific code between the PNG/PDF lines of code;

# set working directory
#setwd("... _path_to_directory_ ...")

### PNG
#png(file="Diagram.Model_1.png", width = 11.7, height = 8.3, units="in", res=100)
	# ... run specific code, but without the dev.new(...) line!
#dev.off() # close file


### PDF
#pdf(file="Diagram.Model_1.pdf")
	# ... run specific code
#dev.off() # close file

