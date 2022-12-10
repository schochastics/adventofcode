input <- readLines("input09.txt")
directions <- unlist(sapply(strsplit(input," "),function(x) rep(x[1],as.integer(x[2]))))
h <- c(0,0)
t <- c(0,0)
move <- function(h,t,direction) {
  # Move the h in the specified direction
  if (direction == "R") {
    h[1] <- h[1] + 1
  } else if (direction == "L") {
    h[1] <- h[1] - 1
  } else if (direction == "D") {
    h[2] <- h[2] - 1
  } else if (direction == "U") {
    h[2] <- h[2] + 1
  }
    

  if (abs(h[1] - t[1]) == 2 || abs(h[2] - t[2]) == 2) {

    t[1] <- t[1] + sign(h[1] - t[1])
    t[2] <- t[2] + sign(h[2] - t[2])
  } else if(abs(t[1]-h[1])==1 && abs(t[1]-h[1])==1){
    t <- t
  } else if (h[1] != t[1] && h[2] != t[2]) {

    t[1] <- t[1] + sign(h[1] - t[1])
    t[2] <- t[2] + sign(h[2] - t[2])
  }
  return(list(head=h,tail=t))
}

hmat <- t(as.matrix(h))
tmat <- t(as.matrix(t))
for(i in seq_along(directions)){
  tmp <- move(hmat[i,],tmat[i,],directions[i])
  hmat <- rbind(hmat,tmp$head)
  tmat <- rbind(tmat,tmp$tail)
}
sum(!duplicated(tmat))
plot(tmat[,1:2],type="l")
lines(hmat[,1:2],col="red")

#part 2
h <- t(as.matrix(c(0,0)))
mat_lst <- list(h,h,h,h,h,h,h,h,h,h)
for(i in seq_along(directions)){
  for(j in 1:9){
    tmp <- move(mat_lst[[j]][i,],mat_lst[[j+1]][i,],directions[i])
    if(j==1){
      mat_lst[[j]] <- rbind(mat_lst[[j]],tmp$head)
    }
    mat_lst[[j+1]] <- rbind(mat_lst[[j+1]],tmp$tail)  
  }
}
sum(!duplicated(mat_lst[[10]]))
