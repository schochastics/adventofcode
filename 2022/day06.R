chars <- strsplit(readLines("input06.txt"),"")[[1]]
check_last_x <- function(x=3,chars,i){
  any(duplicated(chars[i:(i-x)]))
}
which(!sapply(seq_along(chars)[-(1:3)],check_last_x,x=3,chars = chars))[1]+3
which(!sapply(seq_along(chars)[-(1:13)],check_last_x,x=13,chars = chars))[1]+13

