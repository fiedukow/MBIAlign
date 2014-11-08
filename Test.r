source("DynAlign.r")

STATE = DynAlignInit(c("a","b","c"), matrix(c(2,-1,-2,-1,3,-2,-2,-2,4), nrow=3, byrow=3), -1,
                     list(c("a","b","c","a"),c("a","c","a"),c("b","b","c")))
for (i in 1:2000)
STATE = DynAlignStep(STATE)
