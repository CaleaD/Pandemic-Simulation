# install.packages(diagram)

library(diagram)

diagram1  = function(file="BasicSIR.png", save.png=FALSE) {
  
  if(save.png) {
    # run this to save as png;
    png(file=file, width = 11.7, height = 8.3, units="in", res=100)
  } else {
   # dev.new(width = 11.7, height = 8.3)
  }
  
  # number of categories in the sir model
  Numgenerations <- 3
  DiffMat <- matrix(data = 0, nrow = Numgenerations, ncol = Numgenerations)
  m <- as.data.frame(DiffMat)
  
  # names and colors of boxes
  name <- c('S','I','R')
  color <-  c("yellow","red","green")
  
  # arrows 
  m[[2,1]] = ""
  m[[3,2]] = ""
  
  # plotting the diagram
  
  plotmat(A = m, pos = 3, name = name, lwd = 2,
          arr.width = 0.25, curve = 0,
          box.size = 0.08, box.col = color, arr.type = "simple", 
          arr.pos = 0.75, main = "SIR model",box.cex = 5)
  
  # the curved arrow (coordinates hard coded)
  curvedarrow(from = c(0.45,0.58), to = c(0.3,0.5), lwd = 2,
              arr.width = 0.4, curve = 0.5, arr.type = "triangle", 
              arr.pos = 0.8, lcol = "red", arr.col = "red")
}

#diagram1() 

diagram3  = function(file="SIR + Vaccination.png", save.png=FALSE,scaleX=1/2, scaleY=1/2) {
  
  if(save.png) {
    # run this to save as png;
    png(file=file, width = 11.7, height = 8.3, units="in", res=100)
  } else {
    #dev.new(width = 11.7, height = 8.3)
  }
  
  # number of categories in the sir model
  Numgenerations <- 9
  DiffMat <- matrix(data = 0, nrow = Numgenerations, ncol = Numgenerations)
  m <- as.data.frame(DiffMat)
  
  # names and colors of boxes
  name <- c(expression(V[Y]), #1
            expression(S[Y]), #2
            "H", #3
            expression(I[Y]), #4
            expression(I[O]), #5
            "D", #6
            expression(S[O]), #7
            "R", #8
            expression(V[O])) #9
  
  color <-  c("light green","yellow","orange","red","red","dark red","grey","green","light green")
  
  # arrows 
  m[[1,2]] = ""
  m[[4,2]] = ""
  m[[5,7]] = ""
  m[[9,7]] = ""
  m[[3,4]] = ""
  m[[3,5]] = ""
  m[[6,4]] = ""
  m[[6,5]] = ""
  m[[8,4]] = ""
  m[[8,5]] = ""
  
  # positions of boxes
  coord = matrix(nrow = Numgenerations, ncol = 2)
  
  # Vy
  coord[1,1] = 0.5 - 0.2 * scaleX
  coord[1,2] = 0.5 + 0.4 * scaleY
  
  # Sy
  coord[2,1] = 0.5 - 0.4 * scaleX
  coord[2,2] = 0.5 + 0.2 * scaleY
  
  # H
  coord[3,1] = 0.5 + 0.2 * scaleX
  coord[3,2] = 0.5 + 0.4 * scaleY
  
  # Inf young
  coord[4,1] = 0.5
  coord[4,2] = 0.5 + 0.2 * scaleY
  
  # Inf old
  coord[5,1] = 0.5
  coord[5,2] = 0.5 -0.2 * scaleY
  
  # D
  coord[6,1] = 0.5 + 0.4*scaleX
  coord[6,2] = 0.5
  
  # So
  coord[7,1] = 0.5 - 0.4 * scaleX
  coord[7,2] =0.5 - 0.2 * scaleY
  
  # R
  coord[8,1] = 0.5 + 0.2 * scaleY
  coord[8,2] = 0.5 - 0.4 * scaleY
  
  # Vo
  coord[9,1] = 0.5 - 0.2 * scaleX
  coord[9,2] = 0.5 - 0.4 * scaleY
  
  # plotting the diagram
  plotmat(A = m, pos = coord, name = name, lwd = 2,
          arr.width = 0.25, curve = 0,
          box.size = 0.021, box.col = color, arr.type = "simple", 
          arr.pos = 0.85, main = "SIR + Vaccination model")
  
  # the curved arrows (coordinates hard coded)
  # from Inf young to SusYoung
  curvedarrow(from = c(0.5, 0.5 + 0.2 * scaleY), to = c(0.5 - 0.15*scaleX, 0.5 + 0.2*scaleY), lcol = "red",
              curve =0.7, arr.pos = 0.95)
  
  # from Inf old to SusOld
  curvedarrow(from = c(0.5, 0.5 -0.25 * scaleY), to = c(0.5 - 0.15*scaleX, 0.5 - 0.22 *scaleY), lcol = "red",
              curve =-0.7, arr.pos = 0.95)
  
  # from H to Sy
  curvedarrow(from = c(0.5 + 0.18*scaleX, 0.5 + 0.4 * scaleY), to = c(0.5 - 0.1*scaleX, 0.5 + 0.3*scaleY), lcol = "orange",
              curve = 0.2, arr.pos = 0.9)
  
  # from H to So
  curvedarrow(from = c(0.5 + 0.21*scaleX, 0.5 +0.37*scaleY), to = c(0.5 -0.1*scaleX, 0.5 - 0.34*scaleY), lcol = "orange",
              curve = -0.2, arr.pos = 0.9)     
  
}

#diagram3()
diagram2  = function(file="SIR + Vaccination.png", save.png=FALSE,scaleX=3/4, scaleY=3/4) {
  
  if(save.png) {
    # run this to save as png;
    png(file=file, width = 11.7, height = 8.3, units="in", res=100)
  } else {
    #dev.new(width = 11.7, height = 8.3)
  }
  
  # number of categories in the sir model
  Numgenerations <- 7
  DiffMat <- matrix(data = 0, nrow = Numgenerations, ncol = Numgenerations)
  m <- as.data.frame(DiffMat)
  
  # names and colors of boxes
  name <- c(
    expression(S[Y]), #1
    "H", #2
    expression(I[Y]), #3
    expression(I[O]), #4
    "D", #5
    expression(S[O]), #6
    "R") #7
  
  color <-  c("yellow","orange","red","red","dark red","grey","green")
  
  # arrows 
  m[[3,1]] = ""
  m[[4,6]] = ""
  m[[2,3]] = ""
  m[[5,3]] = ""
  m[[7,3]] = ""
  m[[2,4]] = ""
  m[[5,4]] = ""
  m[[7,4]] = ""
  #m[[6,5]] = ""
  #m[[8,4]] = ""
  #m[[8,5]] = ""
  
  # positions of boxes
  coord = matrix(nrow = Numgenerations, ncol = 2)
  
  # Vy
  # coord[1,1] = 0.5 - 0.2 * scaleX
  # coord[1,2] = 0.5 + 0.4 * scaleY
  
  # Sy
  coord[1,1] = 0.5 - 0.4 * scaleX
  coord[1,2] = 0.5 + 0.2 * scaleY
  
  # H
  coord[2,1] = 0.5 + 0.2 * scaleX
  coord[2,2] = 0.5 + 0.4 * scaleY
  
  # Inf young
  coord[3,1] = 0.5
  coord[3,2] = 0.5 + 0.2 * scaleY
  
  # Inf old
  coord[4,1] = 0.5
  coord[4,2] = 0.5 -0.2 * scaleY
  
  # D
  coord[5,1] = 0.5 + 0.4*scaleX
  coord[5,2] = 0.5
  
  # So
  coord[6,1] = 0.5 - 0.4 * scaleX
  coord[6,2] =0.5 - 0.2 * scaleY
  
  # R
  coord[7,1] = 0.5 + 0.2 * scaleY
  coord[7,2] = 0.5 - 0.4 * scaleY
  
  # Vo
  # coord[9,1] = 0.5 - 0.2 * scaleX
  #coord[9,2] = 0.5 - 0.4 * scaleY
  
  # plotting the diagram
  plotmat(A = m, pos = coord, name = name, lwd = 2,
          arr.width = 0.25, curve = 0,
          box.size = 0.021, box.col = color, arr.type = "simple", 
          arr.pos = 0.85, main = "SIR + Vaccination model")
  
  # the curved arrows (coordinates hard coded)
  # from Inf young to SusYoung
  curvedarrow(from = c(0.5, 0.5 + 0.2 * scaleY), to = c(0.5 - 0.15*scaleX, 0.5 + 0.2*scaleY), lcol = "red",
              curve =0.7, arr.pos = 0.95)
  
  # from Inf old to SusOld
  curvedarrow(from = c(0.5, 0.5 -0.25 * scaleY), to = c(0.5 - 0.15*scaleX, 0.5 - 0.22 *scaleY), lcol = "red",
              curve =-0.7, arr.pos = 0.95)
  
  # from H to Sy
  curvedarrow(from = c(0.5 + 0.18*scaleX, 0.5 + 0.4 * scaleY), to = c(0.5 - 0.1*scaleX, 0.5 + 0.3*scaleY), lcol = "orange",
              curve = 0.2, arr.pos = 0.9)
  
  # from H to So
  curvedarrow(from = c(0.5 + 0.21*scaleX, 0.5 +0.37*scaleY), to = c(0.5 -0.1*scaleX, 0.5 - 0.34*scaleY), lcol = "orange",
              curve = -0.2, arr.pos = 0.9)     
  
}
diagram2()