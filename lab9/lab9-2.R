install.packages("igraph")
library(igraph)

find_meeting_point <- function(N, K, roads) {
  g <- graph.empty(n = N, directed = FALSE)

  for (i in 1:K) {
    g <- add_edges(g, c(roads[i, 1], roads[i, 2]), weight = roads[i, 3])
  }

  distances <- distances(g, mode = "all", weights = E(g)$weight)

  total_distances <- rowSums(distances)

  meeting_point <- which.min(total_distances)

  return(meeting_point)
}

N <- 5
K <- 6
roads <- matrix(c(1, 2, 1,
                  1, 3, 2,
                  2, 3, 1,
                  2, 4, 3,
                  3, 4, 1,
                  4, 5, 2), ncol = 3, byrow = TRUE)

meeting_point <- find_meeting_point(N, K, roads)
cat("House with a minimum sum dist:", meeting_point, "\n")