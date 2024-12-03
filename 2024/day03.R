input <- paste0(readLines("input03.txt"), collapse = "")
mul <- `*`

mul_str <- unlist(stringr::str_extract_all(input, "mul\\([0-9]+,[0-9]+\\)"))
sum(vapply(mul_str, \(x) eval(parse(text = x)), numeric(1)))

input <- paste0("do()", input, "don't()")
matches <- paste0(stringr::str_extract_all(input, "(?<=do\\(\\))(.*?)(?=don't\\(\\))")[[1]], collapse = "")
mul_str <- unlist(stringr::str_extract_all(matches, "mul\\([0-9]+,[0-9]+\\)"))
sum(vapply(mul_str, \(x) eval(parse(text = x)), numeric(1)))
