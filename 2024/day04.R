library(stringi)

input <- readLines("input04.txt")

str_t <- function(input) {
  n <- nchar(input[1])
  tmp <- sapply(1:n, function(i) {
    stri_sub(input, i, i)
  })
  apply(tmp, 2, paste, collapse = "")
}

diag_off <- function(k = 1, input, main = TRUE) {
  mat <- do.call(rbind, strsplit(input, ""))
  if (isTRUE(main)) {
    return(paste0(mat[row(mat) == col(mat) - k], collapse = ""))
  } else {
    return(paste0(mat[row(mat) + col(mat) == k], collapse = ""))
  }
}

str_diag <- function(input, main = TRUE) {
  n <- nchar(input[1])
  if (isTRUE(main)) {
    return(sapply(-(n - 1):(n - 1), diag_off, input = input, main = TRUE))
  } else {
    return(sapply(2:(2 * n), diag_off, input = input, main = FALSE))
  }
}


sum(stringi::stri_count_fixed(input, "XMAS")) +
  sum(stringi::stri_count_fixed(stri_reverse(input), "XMAS")) +
  sum(stringi::stri_count_fixed(str_t(input), "XMAS")) +
  sum(stringi::stri_count_fixed(stri_reverse(t_str(input)), "XMAS")) +
  sum(stringi::stri_count_fixed(str_diag(input, TRUE), "XMAS")) +
  sum(stringi::stri_count_fixed(stri_reverse(str_diag(input, TRUE)), "XMAS")) +
  sum(stringi::stri_count_fixed(str_diag(input, FALSE), "XMAS")) +
  sum(stringi::stri_count_fixed(stri_reverse(str_diag(input, FALSE)), "XMAS"))

mat <- do.call(rbind, strsplit(input, ""))
n <- dim(mat)[1]
all_A <- which(mat == "A", arr.ind = TRUE)
all_A <- all_A[!all_A[, 1] %in% c(1, n) & !all_A[, 2] %in% c(1, n), ]
x_search <- function(mat, idx) {
  ul <- idx - 1
  lr <- idx + 1
  ur <- idx + c(1, -1)
  ll <- idx + c(-1, 1)
  found <- 0
  if ((mat[ul[1], ul[2]] == "M" & mat[lr[1], lr[2]] == "S") |
    (mat[ul[1], ul[2]] == "S" & mat[lr[1], lr[2]] == "M")) {
    found <- found + 1
  }
  if ((mat[ll[1], ll[2]] == "M" & mat[ur[1], ur[2]] == "S") |
    (mat[ll[1], ll[2]] == "S" & mat[ur[1], ur[2]] == "M")) {
    found <- found + 1
  }
  found == 2
}

sum(apply(all_A, 1, x_search, mat = mat))
