library(igraph)
A <- do.call("rbind", strsplit(readLines("input12.txt"), ""))

g <- make_lattice(c(nrow(A), ncol(A)), directed = TRUE, mutual = TRUE)
V(g)$name <- c(A)
V(g)$char <- ifelse(V(g)$name == "S", "a", ifelse(V(g)$name == "E", "z", V(g)$name))
for (i in 1:vcount(g)) {
  x <- V(g)$char[i]
  neigh <- as.integer(neighborhood(g, order = 1, nodes = i, mindist = 1, mode = "out")[[1]])
  del_idx <- which((match(x, letters) - match(V(g)$char[neigh], letters)) < -1)
  if (length(del_idx) > 0) {
    del_edges <- rbind(rep(i, length(del_idx)), as.integer(neigh[del_idx]))
    g <- delete_edges(g, get.edge.ids(g, c(del_edges)))
  }
}
# part 1
distances(g, which(V(g)$name == "S"), which(V(g)$name == "E"), mode = "out")
# part 2
min(distances(g, which(V(g)$name == "a"), which(V(g)$name == "E"), mode = "out"))
