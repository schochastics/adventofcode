compare <- function(l,r){
  if(is.numeric(l) && ! is.numeric(r)){
    return(compare(list(l),r))
  } else if(!is.numeric(l) && is.numeric(r)){
    return(compare(l,list(r)))
  } else if(is.numeric(l) && is.numeric(r)){
    if(l<r){
      return(TRUE)  
    } else if(l>r){
      return(FALSE)
    } 
    return(NA)
  }
  if(min(length(l),length(r))>0){
    for(id in 1:min(length(l),length(r))){
      val <- compare(l[[id]],r[[id]])
      if(!is.na(val)){
        return(val)
      }
    }
  }
  if(length(l)<length(r)){
    return(TRUE)
  } else if(length(l)>length(r)){
    return(FALSE)
  }
  return(NA)
}


input <- readLines("input13.txt")
start <- seq(1,length(input),3)
res <- 0
for(i in start){
  l1 <- jsonlite::fromJSON(input[i],simplifyVector = FALSE)
  l2 <- jsonlite::fromJSON(input[i+1],simplifyVector = FALSE)
  res <- res+compare(l1,l2)*ceiling(i/3)
}
res

all <- c("[[2]]",input[start],"[[6]]",input[start+1])
resMat <- matrix(NA,length(all),length(all))

for(idx in 1:(length(all)-1)){
  for(idy in (idx+1):length(all)){
    val <- compare(jsonlite::fromJSON(all[idx],simplifyVector = FALSE),
                   jsonlite::fromJSON(all[idy],simplifyVector = FALSE))
    resMat[idx,idy] <- val
    resMat[idy,idx] <- !val
  }
}
ord <- order(rowSums(resMat,na.rm = TRUE),decreasing = TRUE)
new <- all[ord]
prod(which(new%in%c("[[2]]","[[6]]")))
