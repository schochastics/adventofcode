a <- scan(("input01.txt"),blank.lines.skip = FALSE)
splitNA <- function(x){
  idx <- 1 + cumsum(is.na(x))
  keep <- !is.na(x)
  split( x[keep], idx[keep] )
}
max(sapply(splitNA(a),sum))
sum(tail(sort(sapply(splitNA(a),sum)),3))
