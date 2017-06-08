V(g)$degree = degree(g, normalized = FALSE)
V(g)$betweenness = betweenness(g, normalized = TRUE, weights = NULL)
# cities ranked by decreasing betweenness
V(g)$name[ order(V(g)$betweenness, decreasing = TRUE) ]