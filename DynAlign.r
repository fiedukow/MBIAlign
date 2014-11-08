library("combinat")

DynAlignInit = function(S, M, d, s) {
  # TODO - extend Mmatrix by d
  dims = length(s)
  if (dims != 3)
    stop("For now this interface is only valid for exacly 3 input strings.")
  STATE = list();
  STATE$S = S;
  STATE$M = rbind(cbind(M, d), d)
  rownames(STATE$M) = c(S, "-")
  colnames(STATE$M) = c(S, "-")
  STATE$s = s;
  STATE$L = c();
  STATE$T = array(-Inf, unlist(lapply(s, length)) + 1)
  STATE$T[t(replicate(dims, 1))] = 0
  STATE$s_out = list();
  STATE$StateAction = TFill
  STATE$TIndex = c(2, replicate(dims - 1, 1))
  return(STATE)
}

DynAlignStep = function(STATE) {
  return(STATE$StateAction(STATE))
}

TFill = function(STATE) {
  # FIXME - only 3D limitation
  # Do your job
  if(STATE$TIndex[1] > 1) x = STATE$s[[1]][STATE$TIndex[1] - 1] else x = "-"  # just use space as symbol if out of
  if(STATE$TIndex[2] > 1) y = STATE$s[[2]][STATE$TIndex[2] - 1] else y = "-"  # range - it will be -Inf due
  if(STATE$TIndex[3] > 1) z = STATE$s[[3]][STATE$TIndex[3] - 1] else z = "-"  # to the VirtualTValue anyway.

  STATE$T[t(STATE$TIndex)] = max(
    H_VirtualTValue(STATE$T, STATE$TIndex + c(-1, -1, -1)) + DynAlign_e(c( x , y , z ), STATE$M),
    H_VirtualTValue(STATE$T, STATE$TIndex + c(-1, -1,  0)) + DynAlign_e(c( x , y ,"-"), STATE$M),
    H_VirtualTValue(STATE$T, STATE$TIndex + c(-1,  0, -1)) + DynAlign_e(c( x ,"-", z ), STATE$M),
    H_VirtualTValue(STATE$T, STATE$TIndex + c( 0, -1, -1)) + DynAlign_e(c("-", y , z ), STATE$M),
    H_VirtualTValue(STATE$T, STATE$TIndex + c(-1,  0,  0)) + DynAlign_e(c( x ,"-","-"), STATE$M),
    H_VirtualTValue(STATE$T, STATE$TIndex + c( 0, -1,  0)) + DynAlign_e(c("-", y ,"-"), STATE$M),
    H_VirtualTValue(STATE$T, STATE$TIndex + c( 0,  0, -1)) + DynAlign_e(c("-","-", z ), STATE$M)
  )

  # Update state for next step
  STATE$TIndex = H_IteratePosition(STATE$TIndex, dim(STATE$T))
  if (sum(STATE$TIndex == 1) == length(STATE$TIndex))
    STATE$StateAction = LFill
  return(STATE);
}

LFill = function(STATE) {
  return(STATE)
}

DynAlign_e = function(symbols, M) {
  return(sum(M[t(combn(symbols, 2))]))
}

H_IteratePosition = function(position, limit) {
  # TODO There must be a way to do this in not such an ugly way.
  pointer = 1;
  while (pointer <= length(limit)) {
    if (position[pointer] < limit[pointer]) {
      position[pointer] = position[pointer] + 1
      return(position)
    }
    position[pointer] = 1
    pointer = pointer + 1
  }
  return(replicate(length(limit), 1))
}

H_VirtualTValue = function(T, position) {
  if (sum(position < 1) > 0 || sum(position > dim(T)) > 0)
    return(-Inf)
  else
    return(T[t(position)])
}

