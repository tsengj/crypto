# load termDocMatrix
#load("export/termDocMatrix.rdata")

termDocMatrix <- as.matrix(term.doc.matrix)

# change it to a Boolean matrix
termDocMatrix[termDocMatrix>=1] <- 1
# transform into a term-term adjacency matrix
termMatrix <- termDocMatrix %*% t(termDocMatrix)

library(igraph)
# build a graph from the above matrix
g <- graph.adjacency(termMatrix, weighted=T, mode = "undirected")
# remove loops
g <- simplify(g)
# set labels and degrees of vertices
V(g)$label <- V(g)$name
V(g)$degree <- degree(g)
          
# set seed to make the layout reproducible
set.seed(3952)
layout1 <- layout.fruchterman.reingold(g)
plot(g, layout=layout1)
          
plot(g, layout=layout.kamada.kawai)
tkplot(g, layout=layout.kamada.kawai)
          
V(g)$label.cex <- 2.2 * V(g)$degree / max(V(g)$degree)+ .2
V(g)$label.color <- rgb(0, 0, .2, .8)
V(g)$frame.color <- NA
egam <- (log(E(g)$weight)+.4) / max(log(E(g)$weight)+.4)
E(g)$color <- rgb(.5, .5, 0, egam)
E(g)$width <- egam

# plot the graph in layout1
plot(g, layout=layout1)

#Export image to disk
png(filename="./plot/icx_networks.png",
    height=1080, 
    width=1080,
    units="px"
)
plot(g, layout=layout1)
dev.off()