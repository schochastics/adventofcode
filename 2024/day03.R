input <- paste0(readLines("input03.txt"), collapse = "")
## Parse/eval

eval_parse <- function(input) {
  mul <- `*`
  mul_str <- unlist(stringr::str_extract_all(input, "mul\\([0-9]+,[0-9]+\\)"))
  sum(vapply(mul_str, \(x) eval(parse(text = x)), numeric(1)))
}
eval_parse(input)

input <- paste0("do()", input, "don't()")
matches <- paste0(stringr::str_extract_all(input, "(?<=do\\(\\))(.*?)(?=don't\\(\\))")[[1]], collapse = "")
mul_str <- unlist(stringr::str_extract_all(matches, "mul\\([0-9]+,[0-9]+\\)"))
sum(vapply(mul_str, \(x) eval(parse(text = x)), numeric(1)))

stringi_match <- function(input) {
  regex <- "mul\\(([0-9]+),([0-9]+)\\)"
  matches <- stringi::stri_match_all_regex(input, regex)[[1]]
  sum(as.numeric(matches[, 2]) * as.numeric(matches[, 3]))
}

microbenchmark::microbenchmark(
  eval_parse(input),
  stringi_match(input)
)
