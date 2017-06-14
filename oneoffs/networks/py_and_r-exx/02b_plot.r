library(ggmap)
geo = geocode(paste(V(g)$name, "France"), output = "latlona")
plot(g, layout = as.matrix(geo[, 1:2]),
     vertex.color = NA, vertex.size = 10, vertex.frame.color = NA,
     vertex.label.family = "Helvetica", vertex.label.color = "black",
     vertex.label.cex = V(g)$degree / 5)