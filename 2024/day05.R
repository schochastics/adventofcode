library(igraph)
input <- readLines("input05.txt")
rules <- input[grepl("\\|", input)]
update <- input[!grepl("\\|", input) & input != ""]

g <- graph_from_edgelist(do.call("rbind", strsplit(rules, "\\|")))

mid_path <- function(g, nodes, correct = TRUE) {
  ids <- match(nodes, V(g)$name)
  wrong <- FALSE
  swap <- TRUE
  while (swap) {
    swap <- FALSE
    for (i in seq_len(length(ids) - 1)) {
      if (!are_adjacent(g, ids[i], ids[i + 1])) {
        wrong <- TRUE
        if (correct) {
          return(NULL)
        } else {
          if (are_adjacent(g, ids[i + 1], ids[i])) {
            ids[c(i, i + 1)] <- ids[c(i + 1, i)]
            swap <- TRUE
          }
        }
      }
    }
  }
  if (!correct) {
    nodes <- V(g)$name[ids]
  }
  if (!correct & !wrong) {
    return(NULL)
  }
  return(as.numeric(nodes[ceiling(length(nodes) / 2)]))
}

sum(unlist(sapply(strsplit(update, ","), mid_path, g = g, correct = TRUE)))
sum(unlist(sapply(strsplit(update, ","), mid_path, g = g, correct = FALSE)))
