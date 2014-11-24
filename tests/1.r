S = c("A","C","T","G")
M= matrix(c( 1,-1,-1,-1,
             -1, 1,-1,-1,
             -1,-1, 1,-1,
             -1,-1,-1, 1),
          nrow=4, ncol=4, byrow=TRUE)
d = -1
s = list(c("A","C","C","A","G","T"),
         c("A","A","A","T","T"),
         c("C","A","A","G","T"),
         c("C","A","A","G","T"))

test.inputStrings <- function()
{
  checkException(DynAlignInit(S, M,d,s), "For now this interface is only valid for exacly 3 input strings.")
}

s1 = list(c("A","C","C","A","G","T"),
         c("A","A","A","T","T"),
         c("C","A","A","G","T"))

STATE = DynAlignInit(S, M,d,s1)


test.equals <- function()
{
  checkEquals(length(STATE$M), 25)
  checkEquals(rownames(STATE$M), c("A","C","T","G", "-"))
  checkEquals(colnames(STATE$M), c("A","C","T","G", "-"))
  checkEquals(STATE$T[1,1,1], 0)
}
