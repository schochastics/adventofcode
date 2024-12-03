input <- readLines("input02.txt")
strsplit(input, " ") |>
  vapply(\(x) {
    y <- diff(as.numeric(x))
    r1 <- all(y > 0 | all(y < 0))
    r2 <- all(abs(y) >= 1 & abs(y) <= 3)
    r1 & r2
  }, logical(1)) |>
  sum()

is_safe <- function(report) {
  diffs <- diff(report)
  all(diffs >= 1 & diffs <= 3) || all(diffs <= -1 & diffs >= -3)
}

is_safe_with_dampener <- function(report) {
  n <- length(report)

  if (is_safe(report)) {
    return(TRUE)
  }

  for (i in 1:n) {
    modified_report <- report[-i]
    if (is_safe(modified_report)) {
      return(TRUE)
    }
  }

  return(FALSE)
}

count_safe_reports <- function(data) {
  reports <- strsplit(data, "\n")[[1]]
  reports <- lapply(reports, function(line) as.numeric(strsplit(line, " ")[[1]]))
  safe_count <- sum(sapply(reports, is_safe_with_dampener))
  return(safe_count)
}
sum(sapply(input, count_safe_reports))
