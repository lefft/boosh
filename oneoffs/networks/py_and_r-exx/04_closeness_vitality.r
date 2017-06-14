closeness_vitality = function(g) {
  a = sum(igraph::distances(g))
  v = sapply(1:igraph::vcount(g), function(v) {
    d = igraph::distances(igraph::delete_vertices(g, v))
    a - sum(d[ !is.infinite(d) ])
  })
  names(v) = V(g)$name
  v
}