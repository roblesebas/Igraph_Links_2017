# Creating the environment ####

## Set working directroy

setwd("C:/Users/Sebastian/Google Drive/DOCTORADO/INTERNSHIP/Kentucky University/Activities Internship/F LINKS IGRAPH")

## Upload packages

library(igraph)

# Create functions

giant.component <- function(graph) {
        cl <- clusters(graph)
        induced.subgraph(graph, which(cl$membership == which.max(cl$csize)))}

# Getting Data ####

nodes.raw <-  read.csv("Example_Nodes_SNAOrganization.csv", stringsAsFactors = FALSE)
edges.raw <- read.csv("Example_Edges_SNAOrganization.csv", stringsAsFactors = FALSE)

head(nodes.raw)
head(edges.raw)

# Cleaning Data ####

## Creating our graph object

net.clean.1 <- graph.data.frame(d = edges.raw, directed = TRUE, 
                            vertices = nodes.raw)
net.clean.1 <- simplify(net.clean.1)

summary(net.clean.1)

## Adding in and out degree network metrics

V(net.clean.1)$indegree <- degree(net.clean.1, mode = "in")
V(net.clean.1)$outdegree <- degree(net.clean.1, mode = "out")

head(as_data_frame(net.clean.1, what = "vertices"))

## Deleting nodes with in-degree 1 and out-degree 0

net.clean.2 <- delete.vertices(net.clean.1, V(net.clean.1)[indegree == 1 &
                                                                 outdegree == 0])

head(as_data_frame(net.clean.2, what = "vertices"))

## Extract giant component 

net.clean.3 <- giant.component(net.clean.2)

summary(net.clean.3)

# Tidying Data ####

net.tidied.1 <- net.clean.3

# Delete attributes in and out degree

net.tidied.2 <- delete_vertex_attr(net.tidied.1, "outdegree" )
net.tidied.3 <- delete_vertex_attr(net.tidied.2, "indegree" )

net.tidied <- net.tidied.3

summary(net.tidied)

# Exploratory Analysis ####

## Global properties

# Density

edge_density(net.tidied, loops = FALSE)

# Transitivity 

transitivity(net.tidied, type = "global")

# Diameter

diameter(net.tidied, directed = TRUE, weights = NA)

# Centralization

centr_degree(net.tidied, mode = "all")$centralization

## Local properties

# Degree: in and out

V(net.tidied)$indegree <- degree(net.tidied, mode = "in")
V(net.tidied)$outdegree <- degree(net.tidied, mode = "out")
V(net.tidied)$degree <- degree(net.tidied, mode = "all")

# Betweenness

V(net.tidied)$bet <- betweenness(net.tidied)

# Bonacich

V(net.tidied)$bonacich <- power_centrality(net.tidied)

# Transitivity 

V(net.tidied)$transitivity <- transitivity(net.tidied, type = "local")

head(as_data_frame(net.tidied, what = "vertices"))

## Subgroups and communities 

# Based on greedy optimization of modularity

community <- cluster_fast_greedy(as.undirected(net.tidied))
V(net.tidied)$community <- community$membership

table(V(net.tidied)$community)

# Coreness

V(net.tidied)$coreness <- coreness(net.tidied)

table(V(net.tidied)$coreness)

## Topological Properties

# Degree distribution

hist(degree(net.tidied, mode = "in"), col="blue", 
     main = "In-degree distribution on co-citation networks",
     xlab = "Vertex Degree", ylab = "Frecuency")

hist(degree(net.tidied, mode = "out"), col="blue", 
     main = "Out-degree distribution on co-citation networks",
     xlab = "Vertex Degree", ylab = "Frecuency")

# Log-log degree distribution

d.net.tidied <- degree(net.tidied, mode = "all") 
dd.net.tidied <- degree.distribution(net.tidied)

d <- 1:max(d.net.tidied)
ind <- (dd.net.tidied != 0)
plot(d[ind], dd.net.tidied[ind], log = "xy", col = "blue", 
     xlab = c("Log-Degree"), ylab = c("Log-Intensity"), 
     main = "Log-Log Degree Distribution")

# Log-log degree distribution

a.nn.deg.net.tidied <- graph.knn(net.tidied,V(net.tidied))$knn
plot(d.net.tidied, a.nn.deg.net.tidied, 
     log="xy", col="goldenrod", 
     xlab=c("Log Vertex Degree"),
     ylab=c("Log Average Neighbor Degree"))

# Present Results ####

## Identifidying key actors in the network: Seminal, Structural, and Current papers

df.1 <- as_data_frame(net.tidied, what = "vertices")

df.1 <- df.1[, c("label", "outdegree", "indegree", "bet")]

seminals <- df.1[df.1$outdegree == 0,]
seminals <- head(seminals[order(seminals$indegree, 
                                rev(seminals$indegree),
                                decreasing = TRUE), "label"], 10)

structurals <- head(df.1[order(df.1$bet, rev(df.1$bet),
                            decreasing = TRUE), "label"], 10)

currents <- df.1[df.1$indegree == 0,]
currents <- head(currents[order(currents$outdegree, 
                                rev(currents$outdegree),
                                decreasing = TRUE), "label"], 10)

key.papers <- data.frame(seminals, structurals, currents, stringsAsFactors = FALSE)

key.papers

# Export document

write.csv(key.papers, "key_papers.csv", row.names = FALSE)

# Export network 

write.graph(net.tidied, "net_tidied.graphml", "graphml")
