### Introduction ####

# Select the working directory

library(igraph)
library(sand)

### Creating Graphs ####

## Undirected Graphs 

g <- graph.formula(1-2, 1-3, 2-3, 2-4, 3-5, 4-5, 4-6,
                   4-7, 5-6, 6-7)
V(g)

E(g)

str(g)

plot(g)

## Directed Graphs  

y <- c(1:5)

dg <- graph.formula(1-+2, 1-+3, 2++3)

plot(dg)

dg.1 <- graph.formula(Sam-+Mary, Sam-+Tom, Mary++Tom)

str(dg.1)

## Representations of Graphs

edgelist <- get.edgelist(g)
View(edgelist)

matrix <- as.matrix(get.adjacency(g))
View(matrix)

adjlist <- get.adjlist(g)
adjlist

## Operations on graphs

# Consider the subgraph of g induced by the first five vertices.

h <- induced.subgraph(g, 1:5)
plot(h)

# Also, we could remove vertices 6 and 7 from g to generate h

h <- g - vertices(c(6,7))
plot(h)

# Recovering g from h by adding these two vertices  and the apropiate edges

h <- h + vertices(c(6,7))  
plot(h)
g.1 <- h + edges(c(4,6),c(4,7),c(5,6),c(6,7))
plot(g.1)

### Graph Data ####

dg.2 <- graph.formula(1-+2, 1-+3, 2++3)

plot(dg.1)

# Adding Attributes

dg.2$name <- "Toy Graph"

V(dg.2)$name <- c("Sam", "Mary", "Tom")

plot(dg.2)

V(dg.2)$gender <- c("M","F","M")

V(dg.2)$color <- "red"

summary(dg.2)
plot(dg.2)

# Using Data Frames

library(sand)

View(v.attr.lazega)

View(elist.lazega)

g.lazega <- graph.data.frame(elist.lazega,  directed="FALSE",   vertices=v.attr.lazega)

vcount(g.lazega)
ecount(g.lazega)
list.vertex.attributes(g.lazega)

str(g.lazega)
summary(g.lazega)

edge.list.lazega <- data.frame(get.edgelist(g.lazega))

vertices.attributes.lazega <- data.frame(
        name = V(g.lazega)$name,
        seniority = V(g.lazega)$Seniority,
        status = V(g.lazega)$Status
)

# Importing and Exporting Data

# Uploading vertices and edges

vertices <- read.csv("Igraph_vertices.csv", stringsAsFactors = FALSE)
View(vertices)

edges <- read.csv("Igraph_edges.csv", stringsAsFactors = FALSE)
View(edges)

g.lazega.1 <- graph.data.frame(edges, directed = "FALSE", vertices = vertices)
summary(g.lazega.1)

# Uploading matrix format

matrix.lazega <- read.csv("Igraph_matrix_1.csv", stringsAsFactors = FALSE)

matrix.lazega.1 <- as.matrix(matrix.lazega)

g.matrix.lazega <- graph.adjacency(adjmatrix = matrix.lazega.1, 
                                   mode = "undirected") 
summary(g.matrix.lazega)

# Uploading graph object

lazega <- read.graph("lazega.graphml", format = "graphml")
summary(lazega)

# Exporting graph objects as vertices and edges

write.csv(v.attr.lazega, "lazega_vertices.csv", row.names = FALSE)

write.csv(elist.lazega, "lazega_edges.csv", row.names = FALSE)

# Exporting graph objects as graph objects

write.graph(g.lazega, "lazega.graphml", format = "graphml")

### Plotting ####

## Basics

plot(g, vertex.size = 30, vertex.shape = "rectangle", 
     vertex.color = "green")
title("Toy Graph")

## Example of SAND

library(igraphdata)
data(karate)
# Reproducible layout
set.seed(42)
l <- layout.kamada.kawai(karate)
# Plot undecorated first.
par(mfrow=c(1,1))
plot(karate, layout=l, vertex.label=NA)
# Now decorate, starting with labels.
V(karate)$label <- sub("Actor ", "", V(karate)$name)
# Two leaders get shapes different from club members.
V(karate)$shape <- "circle"
V(karate)[c("Mr Hi", "John A")]$shape <- "rectangle"
# Differentiate two factions by color.
V(karate)[Faction == 1]$color <- "red"
V(karate)[Faction == 2]$color <- "dodgerblue"
# Vertex area proportional to vertex strength
# (i.e., total weight of incident edges).
V(karate)$size <- 4*sqrt(graph.strength(karate))
V(karate)$size2 <- V(karate)$size * .5
# Weight edges by number of common activities
E(karate)$width <- E(karate)$weight
# Color edges by within/between faction.
F1 <- V(karate)[Faction==1]
F2 <- V(karate)[Faction==2]
E(karate)[ F1 %--% F1 ]$color <- "pink"
E(karate)[ F2 %--% F2 ]$color <- "lightblue"
E(karate)[ F1 %--% F2 ]$color <- "yellow"
# Offset vertex labels for smaller points (default=0).
V(karate)$label.dist <- 
        ifelse(V(karate)$size >= 10, 0, 0.75)
# Plot decorated graph, using same layout.
plot(karate, layout=l)

### Exercise ####

# 1. Create a directed graph of 5 vertices

dg.ex <- graph.formula(1-2, 1-3, 2-3, 3-4, 4-5, 3-5)

# 2. Add attributes: name, gender and color

V(dg.ex)$name <- c("mary", "Vivi", "Martha", "Sebas", "Mateo")
V(dg.ex)$gender <- c("f", "f", "f", "m", "m")
V(dg.ex)$color <- c("pink", "pink", "pink", "blue", "blue")

# 3. Get edge and attribute list

edge.list <- data.frame(get.edgelist(dg.ex))

vertices.attributes <- data.frame(
        name = V(dg.ex)$name,
        gender = V(dg.ex)$gender,
        color = V(dg.ex)$color)

# 4. Create a graph object from the edge list 

dg.ex.1 <- graph.data.frame(edgelist, directed = FALSE)

# 5. Save the graph in "graphml" format

write.graph(dg.ex.1, "graph_excerise.graphml", "graphml")





