# packages
library(dplyr)
library(statnet)
library(ggraph) # installed on othe rmac


# functions ---------------------------------------------------------------

ego.effective.size <- function(g, ego, ...) {
  egonet <- induced.subgraph(g, neighbors(g, ego, ...))
  n <- vcount(egonet)
  t <- ecount(egonet)
  return(n - (2 * t) / n)
}

effective.size <- function(g, ego=NULL, ...) {
  if(!is.null(ego)) {
    return(ego.effective.size(g, ego, ...))
  }
  return (sapply(V(g), function(x) {ego.effective.size(g,x, ...)}))
}

shortest.path.vertex <- function(g){
  shortest.path.m <- shortest.paths(g, v=V(g), to=V(g))
  shortest.path.v <- apply(shortest.path.m, 1, mean)
  return(shortest.path.v)
}

msloc_snametric <- function(gs) {
  # Centrality
  V(gs)$degree      <- degree(gs, mode = "total", normalized=T)
  V(gs)$indegree    <- degree(gs, mode = "in", normalized=T)
  V(gs)$outdegree   <- degree(gs, mode = "out", normalized=T)
  V(gs)$betweenness <- betweenness(gs, normalized=T, weights=NA)
  # error w/ evcent. results don't make sense. needs symmetrized data
  # V(gs)$evcent      <- eigen_centrality(gs, directed=T, scale=T, weights=NA)
  V(gs)$closeness_in<- closeness(gs, normalized=T, mode="in")
  V(gs)$flowbet     <- sna::flowbet(as.matrix(get.adjacency(gs)))
  E(gs)$betweenness <- edge.betweenness(gs)
  # Local position
  V(gs)$effsize     <- effective.size(gs, mode = "all")
  V(gs)$constraint  <- constraint(gs)
  # Clustering
  com <- edge.betweenness.community(gs, directed=T)
  V(gs)$memb        <- com$membership
  # Whole network
  gs <- set.graph.attribute(gs, "density", graph.density(gs))
  gs <- set.graph.attribute(gs, "avgpathlength", average.path.length(gs))
  gs <- set.graph.attribute(gs, "modularity", modularity(com))
  gs <- set.graph.attribute(gs, "betcentralization", centralization.betweenness(gs)$centralization)
  gs <- set.graph.attribute(gs, "degcentralization", centralization.degree(gs, mode = "total")$centralization)
  gs <- set.graph.attribute(gs, "size", vcount(gs))
  gs <- set.graph.attribute(gs, "edgecount", ecount(gs))
  vertexsdata <- get.data.frame(gs, what = "vertices")
  edgedata <- get.data.frame(gs, what ="edges")
  #gstats <- #do.call('rbind', lapply(gs, function(y) {
  ga <- list.graph.attributes(gs)
  gstats <- sapply(ga, function(x) {
    get.graph.attribute(gs, x)
  })
  allresults <- list(gs, vertexsdata, edgedata, gstats)
  return(allresults)
}



# Load Data ---------------------------------------------------------------

info <- read.csv("army-MaturingWorkforce [Nodes]_communication_august2017.csv", header = T)
ed <- read.csv("Army-MaturingWorkforce [Edges].csv", header=T)
nodes <- read.csv("Army-MaturingWorkforce [Nodes].csv", header = T)
nodes[which(nodes[,10]) == " "] <- "missing Position"



# subset by relationship type ---------------------------------------------


info.el <- subset(ed, select = c(Source, Target, InformationInteger))
comm.el <- subset(ed, select = c(Source, Target, CommunicationInteger))


# select relationship to work with ----------------------------------------

relationship <- "Information Exchange" # or "comm"

graph.el <- info.el # MODIFY THIS LINE DEPENDING ON RELATIONSHIP

###########MODIFY ALSO LINE 117 FOR SAVING THE ROBJECT

# creating networks using igraph ------------------------------------------


library(igraph)
names(graph.el)[3]<-"weight"
graph.g <- graph.data.frame(graph.el, directed=T, vertices = nodes[,c(1,3:10)])
graph.g

# dichotomize data
E(graph.g)$bin <- E(graph.g)$weight
graph.g <- graph.g %>% set_edge_attr("bin", which(E(graph.g)$weight > 3), 1)
graph.g <- graph.g %>% set_edge_attr("bin", which (E(graph.g)$weight < 4), 0)



# subset networks by branch -----------------------------------------------

# delete weak ties
graph.str <- graph.g %>% delete_edges(which(E(graph.g)$weight < 4))

graph.exec <- induced_subgraph(graph.g, which(V(graph.g)$Branch.Area == "Executive Area") )
graph.strat <- induced_subgraph(graph.g, which(V(graph.g)$Branch.Area == "Strategic Systems Branch") )
graph.op <- induced_subgraph(graph.g, which(V(graph.g)$Branch.Area == "Operational Comms") )
graph.infra <- induced_subgraph(graph.g, which(V(graph.g)$Branch.Area == "Infrastructure Support") )
graph.engin <- induced_subgraph(graph.g, which(V(graph.g)$Branch.Area == "Engineering") )

# save graph objects for plotting -----------------------------------------
save.image("comm_graphs.RData") #change line according to name of network shown in line 36

# analysing network -----------------------------------------------------

graph.g <- info.g # modify this line
graph.exec <- induced_subgraph(graph.g, which(V(graph.g)$Branch.Area == "Executive Area") )
graph.strat <- induced_subgraph(graph.g, which(V(graph.g)$Branch.Area == "Strategic Systems Branch") )
graph.op <- induced_subgraph(graph.g, which(V(graph.g)$Branch.Area == "Operational Comms") )
graph.infra <- induced_subgraph(graph.g, which(V(graph.g)$Branch.Area == "Infrastructure Support") )
graph.engin <- induced_subgraph(graph.g, which(V(graph.g)$Branch.Area == "Engineering") )

# vertex descriptions
vert_desc <- rbind("all"= rs_graph.g[[2]],"exec" = rs_graph.exec[[2]], "strat"= rs_graph.strat[[2]],
                   "op"= rs_graph.op[[2]], "infra"= rs_graph.infra[[2]], "engin" = rs_graph.engin[[2]])
View(vert_desc)

# edge descriptives
edge_desc <- rbind("all"= rs_graph.g[[3]],"exec" = rs_graph.exec[[3]], "strat"= rs_graph.strat[[3]],
                   "op"= rs_graph.op[[3]], "infra"= rs_graph.infra[[3]], "engin" = rs_graph.engin[[3]])
# graph descriptives
graph_desc <- rbind("all"= rs_graph.g[[4]],"exec" = rs_graph.exec[[4]], "strat"= rs_graph.strat[[4]],
                    "op"= rs_graph.op[[4]], "infra"= rs_graph.infra[[4]], "engin" = rs_graph.engin[[4]])
# save output -------------------------------------------------------------
write.table(vert_desc, "vertices_descriptives.csv", append = T)
write.table(edge_desc, "edges_descriptives.csv", append = T)
write.table(graph_desc, "graph_descriptives.csv", append = T)

# create networks using statnet

# density tables ----------------------------------------------------------

library(rDNA)
# info.el[which(info.el$weight < 4),3] <- 0
# info.el[which(info.el$weight > 3),3] <- 1
# info.ed <- info.el[which(info.el$weight == 1),]

# turn into a matrix
library(igraph)  
library(reshape2)
library(ggplot2)

density_tables <-function(graphobject, col_index_for_groups, relationship, group){
  graph <- get.adjacency(graphobject , names=T, attr="bin")
  groups <- matrix(nodes[,col_index_for_groups]) 
  row.names(groups) <- nodes[,1]
  
  #calculate binary density table
  density_table <- dna.density(graph, partitions = groups, weighted=F)
  
  #prep for drawing
  dens_melt <- melt(density_table)
  
  #draw and return
  p<- ggplot(dens_melt, aes(Var1, Var2)) + 
    geom_tile(aes(fill=value), color = "white") + 
    #geom_text(aes(y=1:5, x=1:5, label=dep_dens_melt$value))
    scale_fill_gradient2(low="#e5f5f9", mid = "#99d8c9", high="#2ca25f", 
                         space = "Lab", midpoint = mean(dep_dens_melt$value)-0.05, guide="colorbar")+
    labs(title=paste("Proportion of ", relationship,"within and between Groups (",group,")"),
         x ="Group of Sender", y = "Group of Receiver") +
    theme(axis.text.x = element_text(angle=45, hjust=+1)
    )
  return(p)
  
}
colnames(nodes)
density_tables(graph.g, 4, relationship, group="Age")
group="Age"
ggsave(paste("density_table_bin_", relationship,"_", group,".png"))

density_tables(graph.g, 5, relationship, group="Job")
group="Job"
ggsave(paste("density_table_bin_", relationship,"_", group,".png"))

density_tables(graph.g, 6, relationship, group="Tenure")
group="Tenure"
ggsave(paste("density_table_bin_", relationship,"_", group,".png"))

density_tables(graph.g, 8, relationship, group="Embedded")
group="Embedded"
ggsave(paste("density_table_bin_", relationship,"_", group,".png"))

density_tables(graph.g, 9, relationship, group="Gender")
group="Gender"
ggsave(paste("density_table_bin_", relationship,"_", group,".png"))

density_tables(graph.g, 10, relationship, group="Position")
group="Position"
ggsave(paste("density_table_bin_", relationship,"_", group,".png"))



#original script for calculating density table. keep for reference
graphstr.m <- get.adjacency(graph.str , names=T, attr="bin")
graph.m <- get.adjacency(graph.g , names=T, attr="weight")
groups <- matrix(nodes[,7]) # groups = departments
row.names(groups) <- nodes[,1]

dep_density_table <- dna.density(graphstr.m, partitions = groups, weighted=F)
dep_density_table_value <- dna.density(graph.m, partitions = groups, weighted=T)
dep_dens_melt <- melt(dep_density_table)

dep_dens_melt <- melt(dep_density_table)

ggplot(dep_dens_melt, aes(Var1, Var2)) + 
  geom_tile(aes(fill=value), color = "white") + 
  #geom_text(aes(y=1:5, x=1:5, label=dep_dens_melt$value))
  scale_fill_gradient2(low="#e5f5f9", mid = "#99d8c9", high="#2ca25f", 
                       space = "Lab", midpoint = mean(dep_dens_melt$value)-0.05, guide="colorbar")+
  labs(title=paste("Proportion of ", relationship,"within and between Departments"),
       x ="Department of Sender", y = "Department of Receiver") +
  theme(axis.text.x = element_text(angle=45, hjust=+1)
        )
ggsave(paste("density_table_bin_", relationship,".png"))
