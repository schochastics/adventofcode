seq_test <- function(x,part=1){
  x <- as.numeric(x)
  a <- seq(x[1],x[2])
  b <- seq(x[3],x[4])
  lab <- length(intersect(a,b))
  if(part==1){
    (length(a)==lab | length(b)==lab)    
  } else if(part==2){
    lab>0
  }
}

# part 1
sum(sapply(strsplit(readLines("input04.txt"),"\\-|,"),seq_test,part = 1))
# part 2
sum(sapply(strsplit(readLines("input04.txt"),"\\-|,"),seq_test,part = 2))

