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

test.TFill <- function() {
  checkEquals(STATE$TIndex, c(2,1,1))
  checkEquals(STATE$T[t(c(1,1,1))], 0)
  for(i in 1 : (3*3*3 - 1)) {
    STATE = TFill(STATE)
  }
  checkEquals(STATE$TIndex, c(3,3,3))
  checkEquals(STATE$T[t(c(3,3,3))], 6)
}

test.DynAlign_previous <- function() {
  for(i in 1 : (3*3*3 - 1)) {
    STATE = TFill(STATE)
  }
  STATE = DynAlign_previous(STATE)
  checkEquals(STATE$TIndex, c(2,2,2))
  STATE = DynAlign_previous(STATE)
  checkEquals(STATE$TIndex, c(1,1,1))
}

test.LFill <- function() {
  for(i in 1 : (3*3*3 - 1)) {
    STATE = TFill(STATE)
  }
  STATE = LFill(STATE)
  checkEquals(STATE$L, list(c(3,3,3)))
  STATE = LFill(STATE)
  checkEquals(STATE$L, list(c(2,2,2),c(3,3,3)))
  STATE = LFill(STATE)
  checkEquals(STATE$L, list(c(1,1,1),c(2,2,2),c(3,3,3)))
  STATE = LFill(STATE)
}

test.OutGen <- function() {
  for(i in 1 : (3*3*3 - 1)) {
    STATE = TFill(STATE)
  }
  for(i in 1 : 3) {
    STATE = LFill(STATE)
  }
  STATE = OutGen(STATE)
  checkEquals(STATE$s_out, list(c("A"),c("A"),c("A")))
  STATE = OutGen(STATE)
  checkEquals(STATE$s_out, list(c("A","A"),c("A","A"),c("A","A")))
}