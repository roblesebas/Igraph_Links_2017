

setwd("C:/Users/Sebastian/Google Drive/DOCTORADO/INTERNSHIP/Kentucky University/Activities Internship/F LINKS IGRAPH")


## Upload packages


library(igraph)


# Create functions

# Getting Data


karate.vertices <- read.csv("karate_vertices.csv", stringsAsFactors = FALSE)
karate.edges <- read.csv("karate_edges.csv", stringsAsFactors = FALSE)

head(karate.vertices)
head(karate.edges)


# Cleaning Data


net.karate <- graph.data.frame(karate.edges, directed = FALSE)

summary(net.karate)


# Tidying Data


net.karate.1 <- set.vertex.attribute(net.karate, name = "Faction", 
                                     value = karate.vertices$Faction)

net.karate.2 <- set.vertex.attribute(net.karate, name = "label", 
                                     value = karate.vertices$label)

net.karate.3 <- set.vertex.attribute(net.karate, name = "color", 
                                     value = karate.vertices$color)

net.karate.tidied <- net.karate.3

summary(net.karate.tidied)


# Exploratory Analysis

## Global properties

### Density


edge_density(net.karate.tidied, loops = FALSE)


### Transitivity 


transitivity(net.karate.tidied, type = "global")


### Diameter


diameter(net.karate.tidied, directed = TRUE, weights = NA)


### Centralization


centr_degree(net.karate.tidied, mode = "all")$centralization


## Local properties

### Degree


V(net.karate.tidied)$degree <- degree(net.karate.tidied, mode = "all")


### Betweenness


V(net.karate.tidied)$bet <- betweenness(net.karate.tidied)


### Bonacich


V(net.karate.tidied)$bonacich <- power_centrality(net.karate.tidied)


### Transitivity 


V(net.karate.tidied)$transitivity <- transitivity(net.karate.tidied, type = "local")
head(as_data_frame(net.karate.tidied, what = "vertices"))


## Subgroups and communities 

### Based on greedy optimization of modularity


community <- cluster_fast_greedy(as.undirected(net.karate.tidied))
V(net.karate.tidied)$community <- community$membership

table(V(net.karate.tidied)$community)


### Coreness


V(net.karate.tidied)$coreness <- coreness(net.karate.tidied)

table(V(net.karate.tidied)$coreness)


## Topological properties

### Degree distribution



hist(degree(net.karate.tidied, mode = "all"), col="blue", 
     main = "Degree distribution karate network",
     xlab = "Vertex Degree", ylab = "Frecuency")


### Log-log degree distribution


d.net.karate.tidied <- degree(net.karate.tidied, mode = "all") 
dd.net.karate.tidied <- degree.distribution(net.karate.tidied)

d <- 1:max(d.net.karate.tidied)
ind <- (dd.net.karate.tidied != 0)
plot(d[ind], dd.net.karate.tidied[ind], log = "xy", col = "blue", 
     xlab = c("Log-Degree"), ylab = c("Log-Intensity"), 
     main = "Log-Log Degree Distribution")


### Average network deree versus vertex degree


a.nn.deg.net.karate.tidied <- graph.knn(net.karate.tidied,V(net.karate.tidied))$knn
plot(d.net.karate.tidied, a.nn.deg.net.karate.tidied, 
     log="xy", col="goldenrod", 
     xlab=c("Log Vertex Degree"),
     ylab=c("Log Average Neighbor Degree"),
     main = "Average network degree versus degree (log-log scale)")


# Visualization


kc <- fastgreedy.community(net.karate.tidied)
plot(kc, net.karate.tidied, vertex.label = NA)


# Export network 


write.graph(net.karate.tidied, "net_karate_tidied.graphml", "graphml")
