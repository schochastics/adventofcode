mat <- do.call("rbind", strsplit(readLines("input06.txt"), ""))
pos <- which(mat == "^", arr.ind = TRUE)
n <- nrow(mat)
on_map <- TRUE
steps <- 0
face <- "u"

while (on_map) {
  pos_old <- pos
  if (face == "u") {
    stop <- which(mat[1:pos[1], pos[2]] == "#")
    if (length(stop) == 0) {
      on_map <- FALSE
      stop <- 0
    }
    pos <- c(stop[length(stop)] + 1, pos[2])
    mat[pos_old[1]:pos[1], pos[2]] <- "X"
    face <- "r"
  } else if (face == "r") {
    stop <- (pos[2]:n)[which(mat[pos[1], pos[2]:n] == "#")]
    if (length(stop) == 0) {
      on_map <- FALSE
      stop <- n + 1
    }

    pos <- c(pos[1], stop[1] - 1)
    mat[pos[1], pos_old[2]:pos[2]] <- "X"
    face <- "d"
  } else if (face == "d") {
    stop <- (pos[1]:n)[which(mat[pos[1]:n, pos[2]] == "#")]
    if (length(stop) == 0) {
      on_map <- FALSE
      stop <- n + 1
    }
    pos <- c(stop[1] - 1, pos[2])
    mat[pos_old[1]:pos[1], pos[2]] <- "X"
    face <- "l"
  } else if (face == "l") {
    stop <- which(mat[pos[1], 1:pos[2]] == "#")
    if (length(stop) == 0) {
      on_map <- FALSE
      stop <- 0
    }

    pos <- c(pos[1], stop[length(stop)] + 1)
    mat[pos[1], pos_old[2]:pos[2]] <- "X"
    face <- "u"
  }
}
sum(mat == "X")
