# day 01 ----
depths <- as.numeric(readLines("01_input.txt"))
sapply(c(1, 3), \(L) sum(diff(depths, L) > 0))

# day 02 ----
course <- read.delim("02_input.txt", header = FALSE, sep = " ")
prod(c(sum(course$V2[course$V1 == "forward"]), sum(course$V2[course$V1 == "down"]) - sum(course$V2[course$V1 == "up"])))
with(course, {
  x <- (V1 == "forward") * V2
  y <- ((V1=="down") - (V1=="up")) * V2
  depth <- cumsum(y)*x
  sum(x)*sum(depth)
})

# day 03 ----
bin_nums <- readLines("03_input.txt")
A <- do.call("rbind",strsplit(bin_nums,""))
mode(A) <- "double"
gamma <- (apply(A,2,sum)>(nrow(A)/2))+0
epsilon <- 1-gamma
strtoi(paste0(gamma,collapse = ""), base = 2) * strtoi(paste0(epsilon,collapse = ""), base = 2)

B <- A
for(i in 1:ncol(B)){
  n <- nrow(B)
  tmp <- sum(B[,i])
  if(tmp>=(n/2)){
    B <- B[B[,i]==1,]
  } else{
    B <- B[B[,i]==0,]
  }
}
oxy <- strtoi(paste0(B,collapse = ""), base = 2)

B <- A
for(i in 1:ncol(B)){
  n <- nrow(B)
  tmp <- sum(B[,i])
  if(tmp>=(n/2)){
    B <- B[B[,i]==0,]
  } else{
    B <- B[B[,i]==0,]
  }
}
co2 <- strtoi(paste0(B,collapse = ""), base = 2)
co2*oxy

