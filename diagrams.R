# install.packages(diagram)

library(diagram)

diagram1  = function(file="BasicSIR.png", save.png=FALSE) {
  
  if(save.png) {
    # run this to save as png;
    png(file=file, width = 11.7, height = 8.3, units="in", res=100)
  } else {
    dev.new(width = 11.7, height = 8.3)
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
          arr.width = 0.25, my = -0.1, curve = 0,
          box.size = 0.08, box.col = color, arr.type = "simple", 
          arr.pos = 0.68, main = "SIR model")
  
  # the curved arrow (coordinates hard coded)
  curvedarrow(from = c(0.5,0.48), to = c(0.33,0.4), lwd = 2,
              arr.width = 0.4, curve = 0.5, arr.type = "triangle", 
              arr.pos = 0.8, lcol = "red", arr.col = "red")
}

diagram1()

diagram3  = function(file="SIR + Vaccination.png", save.png=FALSE) {
  
  if(save.png) {
    # run this to save as png;
    png(file=file, width = 11.7, height = 8.3, units="in", res=100)
  } else {
    dev.new(width = 11.7, height = 8.3)
  }
  
  # number of categories in the sir model
  Numgenerations <- 8
  DiffMat <- matrix(data = 0, nrow = Numgenerations, ncol = Numgenerations)
  m <- as.data.frame(DiffMat)
  
  # names and colors of boxes
  name <- c(expression(V[Y]),
            expression(S[Y]),
            "H",
            "I",
            "D",
            expression(S[O]),
            "R",
            expression(V[O]))
  
  color <-  c("light green","yellow","orange","red","dark red","grey","green","light green")
  
  # arrows 
  m[[1,2]] = ""
  m[[4,2]] = ""
  m[[4,6]] = ""
  m[[8,6]] = ""
  m[[3,4]] = ""
  m[[5,4]] = ""
  m[[7,4]] = ""
  m[[5,3]] = ""
  m[[7,3]] = ""
  
  # positions of boxes
  coord = matrix(nrow = Numgenerations, ncol = 2)
  
  # Vy
  coord[1,1] = 0.3
  coord[1,2] = 0.9
  
  # Sy
  coord[2,1] = 0.1
  coord[2,2] = 0.7
  
  # H
  coord[3,1] = 0.7
  coord[3,2] = 0.9
  
  # I
  coord[4,1] = 0.5
  coord[4,2] = 0.5
  
  # D
  coord[5,1] = 0.9
  coord[5,2] = 0.5
  
  # So
  coord[6,1] = 0.1
  coord[6,2] = 0.3
  
  # R
  coord[7,1] = 0.7
  coord[7,2] = 0.1
  
  # Vo
  coord[8,1] = 0.3
  coord[8,2] = 0.1
  
  # plotting the diagram
  plotmat(A = m, pos = coord, name = name, lwd = 2,
          arr.width = 0.25, curve = 0,
          box.size = 0.06, box.col = color, arr.type = "simple", 
          arr.pos = 0.75, main = "SIR + Vaccination model")
  
  # the curved arrows (coordinates hard coded)
  # from Inf to SusYoung
  curvedarrow(from = c(0.5, 0.52), to = c(0.3, 0.59), lcol = "red",
              curve =0.7, arr.pos = 0.95)
  
  # from Inf to SusOld
  curvedarrow(from = c(0.5, 0.4), to = c(0.3, 0.39), lcol = "red",
              curve =-0.7, arr.pos = 0.95)
  
  # from H to Sy
  curvedarrow(from = c(0.64, 0.9), to = c(0.4, 0.7), lcol = "orange",
              curve = 0.2, arr.pos = 0.9)
  
  # from H to So
  curvedarrow(from = c(0.645, 0.87), to = c(0.4, 0.25), lcol = "orange",
              curve = -0.2, arr.pos = 0.9)
  
}

diagram3()