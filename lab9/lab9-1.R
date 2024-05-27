install.packages("igraph")
library(igraph)


create_and_modify_graph <- function(G_size, N) {
  g1 <- make_empty_graph(n = G_size, directed = FALSE)

  V(g1)$color <- "yellow"
  num_edges1 <- N * 8
  edges1 <- sample(1:G_size, 2 * num_edges1, replace = TRUE)
  g1 <- add_edges(g1, edges1)
  E(g1)$color <- "red"

  plot(g1, vertex.size = 10, vertex.label = NA, edge.color = E(g1)$color)
  cat("Added red edges:\n")
  print(as_adjacency_matrix(g1))

  num_edges2 <- N * 10
  edges2 <- sample(1:G_size, 2 * num_edges2, replace = TRUE)
  g1 <- add_edges(g1, edges2)
  E(g1)$color[(num_edges1 + 1):(num_edges1 + num_edges2)] <- "blue"

  plot(g1, vertex.size = 10, vertex.label = NA, edge.color = E(g1)$color)
  cat("Add blue edges:\n")
  print(as_adjacency_matrix(g1))
}

G_size <- 10
N <- 5

create_and_modify_graph(G_size, N)