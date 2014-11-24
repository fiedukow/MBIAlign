S = c("A","C","T","G")
M= matrix(c( 1,-2,-1,-1,
             -2, 1,-1,-1,
             -1,-1, 1,-1,
             -1,-1,-1, 1),
          nrow=4, ncol=4, byrow=TRUE)
d = -5
s = list(c("A","C","C","A"),
         c("A","A","A","T","T"),
         c("C","A","A","G"))
STATE = DynAlignInit(S, M,d,s)

test.H_IteratePosition <- function() {
  checkEquals(H_IteratePosition(c(3,2,2), c(5,5,5)), c(4,2,2))
}

test.H_VirtualTValue <- function() {
  checkEquals(H_VirtualTValue(STATE$T, c(1,1,1)), 0)
  checkEquals(H_VirtualTValue(STATE$T, c(2,1,1)), (-Inf))
  for (i in 1:(5*6*5-1)) {
    STATE = DynAlignStep(STATE)
  }
  checkTrue(H_VirtualTValue(STATE$T, c(5,6,5))!=(-Inf))
}

test.DynAlign_e <- function() {
  checkEquals(sum(STATE$M[t(combn(c("A", "A", "C"), 2))]), -3)
}