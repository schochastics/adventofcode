# part 1
split_middle <- function(txt){
  a <- substr(txt,1,nchar(txt)/2)
  b <- substr(txt,nchar(txt)/2+1,nchar(txt))
  intersect(strsplit(a,"")[[1]],strsplit(b,"")[[1]])
}
sum(match(sapply(readLines("input03.txt"),split_middle),c(letters,LETTERS)))

#part 2
split(readLines("input03.txt"),rep(1:100,each=3)) |> 
  lapply(strsplit,split="") |> 
  sapply(function(x) Reduce(intersect,x)) |> 
  match(x = _,c(letters,LETTERS)) |> 
  sum()
