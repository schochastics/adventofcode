input <- readLines("input05.txt")
get_crates <- function(input){
  idx <- grep("^\\s*[0-9]",input)
  crate_no <- max(as.numeric(strsplit(input[idx],"\\s+")[[1]]),na.rm = TRUE)
  crates <- vector("list",crate_no)
  crates_txt <- input[1:(idx-1)]
  for(line in crates_txt){
    crate_idx <- gregexpr("\\[[A-Z]\\]",line)
    crate_name <- lapply(crate_idx,function(x) ceiling(x/4))[[1]]
    box_name <- gsub("\\[|\\]","",regmatches(line,crate_idx)[[1]])
    for(b in seq_along(crate_name)){
      crates[[crate_name[b]]] <- c(crates[[crate_name[b]]],box_name[b])  
    }
  }
  crates
}

get_moves <- function(input){
  idx <- grep("^\\s*[0-9]",input)
  moves_txt <- input[(idx+2):length(input)]
  lapply(regmatches(moves_txt,gregexpr("[0-9]+",moves_txt)),as.integer)
}

move_crates <- function(crates,moves,rev = TRUE){
  for(i in seq_along(moves)){
    move <- moves[[i]]
    if(isTRUE(rev)){
      items <- rev(crates[[move[2]]][1:move[1]])  
    } else{
      items <- crates[[move[2]]][1:move[1]]
    }
    crates[[move[3]]] <- c(items,crates[[move[3]]])
    crates[[move[2]]] <- crates[[move[2]]][-(1:move[1])]
  }
  crates
}

crates <- get_crates(input)
moves <- get_moves(input)
cat(sapply(move_crates(crates,moves,rev = TRUE),function(x) x[1]),sep="")
cat(sapply(move_crates(crates,moves,rev = FALSE),function(x) x[1]),sep="")
