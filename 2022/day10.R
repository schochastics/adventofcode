input <- readLines("input10.txt")
status <- matrix(0,nrow=0,ncol=2)
status <- rbind(status,c(1,1))
for(i in 1:length(input)){
  if(grepl("noop",input[i])){
    status <- rbind(status,status[nrow(status),]+c(1,0))
  } else{
    tx <- as.numeric(gsub("addx ","",input[i]))
    status <- rbind(status,status[nrow(status),]+c(1,0))
    status <- rbind(status,status[nrow(status),]+c(1,tx))
  }
}
check <- c(20,60,100,140,180,220)
res <- status[status[,1]%in%check,]
res
sum(res[,1]*res[,2])

crt <- matrix(" ",6,40)
sprite <- c(1,2,3)
k <- 1
for(j in 1:6){
  for(i in 1:40){
    if(i%in%sprite){
      crt[j,i] <- "#"
    }
    k <- k+1
    sprite <- c(status[k,2],status[k,2]+1,status[k,2]+2)
  }
}
apply(crt,1,paste,collapse = ".")