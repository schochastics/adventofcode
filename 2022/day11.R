input <- readLines("input11.txt")

idx <- grep("^\\s+Starting",input)
items <- gsub("  Starting items: ","",input[idx])
bags <- lapply(strsplit(items,", "),as.integer)
  
idx <- grep("^\\s+Operation",input)
fcts <- gsub("  Operation: ","",input[idx])
op <- lapply(gsub("new = ","function\\(old\\) ",fcts),function(x) eval(parse(text = x)))
  
idx <- grep("^\\s+Test",input)
fcts <- gsub("  Test: ","",input[idx])
test <- lapply(paste0(gsub("divisible by ","function\\(x\\) `%%`(x,",fcts),")"),function(x) eval(parse(text = x)))
mods <- as.integer(gsub("divisible by ","",fcts))

idx <- grep("^\\s+If true:",input)
ftrue <- as.integer(gsub("    If true: throw to monkey ","",input[idx]))+1
  
idx <- grep("^\\s+If false:",input)
ffalse <- as.integer(gsub("    If false: throw to monkey ","",input[idx])  )+1

counter <- rep(0,length(bags))
for(round in 1:20){
  for(m in 1:length(bags)){
    while(length(bags[[m]])!=0){
      counter[m] <- counter[m]+1
      item <- bags[[m]][1]

      item <- floor(op[[m]](item)/3)
      #part 2: item <- op[[m]](item) %% prod(mods)
      if(test[[m]](item)==0){
        newm <- ftrue[m]
      } else{
        newm <- ffalse[m]
      }
      bags[[newm]] <- c(bags[[newm]],item)
      bags[[m]] <- bags[[m]][-1]
    }
  }
}
prod(tail(sort(counter),n=2))
