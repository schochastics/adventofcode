input <- readLines("input07.txt")
options(scipen = 999)

add_parentheses <- function(x) {
  n <- sum(strsplit(x, "")[[1]] %in% c("+", "*", "|"))
  x <- paste0(strrep("(", n), x)
  x <- gsub("([+*|])", ")\\1", x, perl = TRUE)
  return(x)
}

concat <- function(x, y) {
  as.numeric(paste0(x, y))
}

`|` <- concat

generate <- function(nums, ops = c("+", "*"), out) {
  op_perms <- as.matrix(expand.grid(rep(list(ops), length(nums) - 1), stringsAsFactors = FALSE))
  for (i in 1:nrow(op_perms)) {
    expr <- suppressWarnings(c(rbind(nums, op_perms[i, ])))
    expr <- paste(expr[-length(expr)], collapse = "")
    if (eval(parse(text = add_parentheses(expr))) == out) {
      return(out)
    }
  }
  return(0)
}

res <- sapply(input, \(x){
  tmp <- strsplit(x, ": ")[[1]]
  out <- as.numeric(tmp[1])
  nums <- as.numeric(strsplit(tmp[[2]], " ")[[1]])
  generate(nums, ops = c("+", "*"), out = out)
})

sum(res)

res <- sapply(input, \(x){
  tmp <- strsplit(x, ": ")[[1]]
  out <- as.numeric(tmp[1])
  nums <- as.numeric(strsplit(tmp[[2]], " ")[[1]])
  generate(nums, ops = c("+", "*", "|"), out = out)
})

sum(res)
