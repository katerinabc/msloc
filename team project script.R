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



# subset by relationship type ---------------------------------------------


info.el <- subset(ed, select = c(Source, Target, InformationInteger))
comm.el <- subset(ed, select = c(Source, Target, CommunicationInteger))


# creating networks using igraph ------------------------------------------


library(igraph)
names(info.el)[3]<-"weight"
info.g <- graph.data.frame(info.el, directed=T, vertices = nodes[,c(1,3:10)])
info.g

# dichotomize data
info.str <- which (E(info.g)$weight > 3)
info.wk <- which (E(info.g)$weight < 4)
info.g <- info.g %>% 
  set_edge_attr("weight", info.str, 1)
info.g <- info.g %>% set_edge_attr("weight", info.wk, 0)

# delete edges with weight = 0
info.g
info.g <- info.g %>% delete_edges(info.wk)
info.g


# subset networks by branch -----------------------------------------------

graph.g <- comm.g # modify this line
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

#info.el[which(info.el$InformationInteger < 4),3] <- 0
#info.el[which(info.el$InformationInteger > 3),3] <- 1

# nodes[,4:10] <- apply(nodes[,4:10], 2,function(x) as.character(x))
# info.n <- network(info, 
#                   #vertex.attr = c(nodes$Age.Levels, nodes$Job.Level, nodes$Tenure, as.character(nodes$Branch.Area), nodes$Embedded, nodes$Gender, nodes$Position),
#                   #vertex.attrnames = list("age", "job", "tenure", "branch", "embedded", "gender", "position"),
#                   directed = T, 
#                   matrix.type="edgelist"
#           )
# 
# # add vertex attributes
# info.n %v% "age" <- nodes$Age.Levels
# info.n %v% "job" <- nodes$Job.Level
# info.n %v% "tenure" <- nodes$Tenure
# info.n %v% "branch" <- nodes$Branch.Area
# info.n %v% "embedded" <- nodes$Embedded
# info.n %v% "gender" <- nodes$Gender
# info.n %v% "position" <- nodes$Position
# 
# # subset by department
# branch <- info.n %v% "branch"
# unique_branch <- unique(branch)
# 
# 
# info.n_exec <- info.n %s% which(branch == "Executive Area")
# info.n_strat <- info.n %s% which(info.n %v% "branch" == "Strategic Systems Branch")
# info.n_opcom <- info.n %s% which(info.n %v% "branch" == "Operational Comms")
# info.n_infa <- info.n %s% which(info.n %v% "branch" == "Infrastructure Support")
# info.n_engin <- info.n %s% which(info.n %v% "branch" == "Engineering")
# 
# # sna overview of each department
# 
# msloc_sna_metric <- function(network){
#   library(sna)
#   ind <- degree(network, gmode = "digraph", cmode= "indegree", rescale = T )
#   outd <- degree(network, gmode = "digraph", cmode= "outdegree" , rescale = T)
#   btw <- betweenness(network, gmode="digraph", cmode = "directed", rescale = T)
#     strholes
# }
