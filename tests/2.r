S = c("A","C","T","G")
M= matrix(c( 1,-1,-1,-1,
             -1, 1,-1,-1,
             -1,-1, 1,-1,
             -1,-1,-1, 1),
          nrow=4, ncol=4, byrow=TRUE)
d = -5
s = list(c("A","C","C","A"),
          c("A","A","A","T","T"),
          c("C","A","A","G"))
STATE = DynAlignInit(S, M,d,s)
TFillIterations = 5*6*5 -1
LFillIterations = 6
OutGenIterations = 6

test.iterations <- function()
{
  for (i in 1:TFillIterations) {
    STATE = DynAlignStep(STATE)
  }
  checkEquals(length(STATE$L), 0)
  for (i in 1:LFillIterations) {
    STATE = DynAlignStep(STATE)
  }
  checkEquals(length(STATE$s_out), 0)
  for (i in 1:OutGenIterations) {
    STATE = DynAlignStep(STATE)
  }
  STATE1 = DynAlignStep(STATE)
}