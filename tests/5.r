S = c("A","C","T","G")
M= matrix(c( 1,-1,-1,-1,
             -1, 1,-1,-1,
             -1,-1, 1,-1,
             -1,-1,-1, 1),
          nrow=4, ncol=4, byrow=TRUE)
d = -1
s = list(c("A","A"),
         c("A","A"),
         c("A","A"))
STATE = DynAlignInit(S, M,d,s)

test.all <- function() {
  for(i in 1 : (3*3*3 - 1)) {
    STATE = DynAlignStep(STATE)
  }
  checkEquals(STATE$TIndex, c(3,3,3))
  checkEquals(STATE$T[t(c(3,3,3))], 6)
  for(i in 1 : 3) {
    STATE = DynAlignStep(STATE)
  }
  checkEquals(STATE$L, list(c(1,1,1),c(2,2,2),c(3,3,3)))
  for(i in 1 : 3) {
    STATE = DynAlignStep(STATE)
  }
  checkEquals(STATE$s_out, list(c("A","A"),c("A","A"),c("A","A")))
}