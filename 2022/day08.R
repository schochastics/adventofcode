input <- readLines("input08.txt")
char_lst <- strsplit(input,"")
A <- matrix(as.integer(unlist(char_lst)),
            ncol = length(char_lst),
            nrow = length(char_lst),byrow = TRUE)

# part 1
visible <- matrix(FALSE,ncol = length(char_lst),nrow = length(char_lst))

is_visible <- function(x){
  v <- rep(TRUE,length(x))
  v[2:length(v)] <- sapply(2:length(x),function(i) all(x[i]>x[1:(i-1)]))
  v
}

for(i in 1:nrow(A)){
  visible[i,] <- visible[i,] | is_visible(A[i,]) | rev(is_visible(rev(A[i,])))
  visible[,i] <- visible[,i] | is_visible(A[,i]) | rev(is_visible(rev(A[,i])))
}
sum(visible)

# part 2
scenic_score <- function(x,i){
  n <- length(x)
  if(i==n){
    score <- 0
  } else{
    score <- which(x[(i+1):n]>=x[i])[1]
    if(is.na(score)){
      score <- n-i
    }
  }
  return(score)
}

LR <- RL <- TB <- BT <- matrix(1,nrow(A),ncol(A)) 
n <- nrow(A)
for(i in 1:n){
  for(j in 1:n){
    LR[i,j] <- scenic_score(A[i,],j)
    RL[i,n-j+1] <- scenic_score(rev(A[i,]),j)
    TB[j,i] <- scenic_score(A[,i],j)
    BT[n-j+1,i] <- scenic_score(rev(A[,i]),j)
    
  }
}
max(LR*RL*TB*BT)
