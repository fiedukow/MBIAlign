source("DynAlign.r")
library("combinat")
library("scatterplot3d")
library("plotrix")

STATE = DynAlignInit(c("a","b","c"), matrix(c(2,-1,-2,-1,3,-2,-2,-2,4), nrow=3, byrow=3), -1,
                     list(c("a","b","c","a"),c("a","c","a"),c("b","b","c")))
for (i in 1:2000)
STATE = DynAlignStep(STATE)

points = expand.grid(1:dim(STATE$T)[1], 1:dim(STATE$T)[2], 1:dim(STATE$T)[3])
sp = scatterplot3d(unlist(points[1]),
              unlist(points[2]),
              unlist(points[3]),
              type="h",
              lty.hplot=2,
              lwd=1,
              pch=19,
              color=color.scale(STATE$T[as.matrix(points)], cs1=c(1,0), cs2=c(0,1), cs3=c(0,0)))

