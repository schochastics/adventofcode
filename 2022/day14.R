input <- readLines("input14.txt")

paths <- lapply(strsplit(input," -> "),function(x) do.call("rbind",strsplit(x,",")))
for(i in 1:length(paths)){
  mode(paths[[i]]) <- "numeric"
  
}

lines1 <- matrix(0,nrow = 0,ncol=2)
for(p in seq_along(paths)){
  lines <- paths[[p]]
  for(i in 1:(nrow(lines)-1)){
    tmp <- cbind(seq(lines[i,1],lines[i+1,1]),seq(lines[i,2],lines[i+1,2]))
    lines1 <- rbind(lines1,tmp)
  }
}
A <- matrix(0,ncol = max(lines1[,1])+1,nrow = max(lines1[,2])+1)
A[lines1[,2:1]+1] <- 1

#part 2: add a line of stones
A <- cbind(A,matrix(0,nrow = nrow(A),ncol = ncol(A)))
A <- rbind(rbind(A,0),0)
A[nrow(A),] <- 1
bottom <- nrow(A)
sand <- 0
inside <- TRUE
while(inside){
  sand <- sand+1
  down <- TRUE
  loc <- c(1,501)
  while(down){
    if((loc[1]+1)>bottom){
      inside <- FALSE
      break
    }
    if((loc[2]+1)>ncol(A)){
      inside <- FALSE
      break
    }
    if((loc[2]-1)==0){
      inside <- FALSE
      break
    }
    if(A[t(as.matrix(loc+c(1,0)))]==0){
      loc <- loc+c(1,0)
    } else if(A[t(as.matrix(loc+c(1,0)))]!=0 && A[t(as.matrix(loc+c(1,-1)))]==0){
      loc <- loc+c(1,-1)
    } else if(A[t(as.matrix(loc+c(1,0)))]!=0 && A[t(as.matrix(loc+c(1,-1)))]!=0 && A[t(as.matrix(loc+c(1,1)))]==0){   
      loc <- loc+c(1,1)
    } else{
      A[t(as.matrix(loc))] <- 2
      down <- FALSE
      if(all(loc==c(1,501))){
        inside <- FALSE
        break
      }
    }
  }
}
sand


