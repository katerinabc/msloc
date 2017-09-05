# plotting networks. copy afterwards to other script

# load per communication tie
load("info_graphs.RData")
rm(list=ls()[-c(2,18:21,24:29)])

library(igraph)
library(ggraph)

# plot using ggraph
# create color schema for branch area
# size nodes by age
dep.color <- data.frame(dep = unique(V(info.g)$Branch.Area), 
                   color = c("#e41a1c", "#377eb8", "#4daf4a", "#984ea3", "#ff7f00"))

V(info.g)$color <- V(info.g)$Branch.Area
V(info.g)$color <- as.character(dep.color$color[match(V(info.g)$color,dep.color$dep)])


ggraph(info.g, "igraph", algorithm = "nicely") +
  geom_edge_fan(aes(alpha = ..index..), show.legend = T) +
  geom_node_point(aes(color=color), size = 6) +
  scale_edge_alpha('Edge direction', guide = 'edge_direction') +
  #geom_edge_density() + # shading added to plot area based on density. kind of a density plot
  geom_node_text(aes(label=Node), size = 4, color = "black", repel=F, fontface="bold") +
  labs(title = "Information Exchange Network (strong ties)", 
       caption = "Q: About how often do you go to {name} for information to help you complete work
assignments ?") + 
  scale_color_manual(name="Department", 
                     values = c("#e41a1c", "#377eb8", "#4daf4a", "#984ea3", "#ff7f00"),
                     labels = unique(V(info.g)$Branch.Area)) + 
  theme_graph(foreground = 'steelblue', fg_text_colour = 'white') + 
  ggforce::theme_no_axes()
ggsave("info.g_no_labels_color_dep_size_age.png")

msloc_subgraphs <- function(igraph_objects, department){
      ggraphobject <- ggraph(igraph_objects, "igraph", algorithm = "nicely") +
        geom_edge_fan(aes(alpha = ..index..), show.legend = T) +
        geom_node_point(aes(color=Gender, size = Age.Levels)) + 
        geom_node_text(aes(label=Node), size = 4, color = "black") +
        scale_edge_alpha('Edge direction', guide = 'edge_direction') +
        labs(title = paste("Information Exchange Network (strong ties) for", department, "Department"),
             caption = "Q: About how often do you go to {name} for information to help you complete work
assignments ?",
             size = "Age Level") + 
        scale_size(breaks=c(1,2,3,4,5), labels=c("<35","35-44" ,"45-54", "55-64", ">65")) + 
        theme_graph(foreground = 'steelblue', fg_text_colour = 'white') + 
        ggforce::theme_no_axes() 
      
      ggsave(ggraphobject, filename=paste(as.character(department), "graph.png"), 
             device = "png")
      
      #return(ggraphobject)
}

subsetnames <- unique(V(info.g)$Branch.Area)
subsetgraphs <- list(graph.exec, graph.strat, graph.op, graph.infra,graph.engin)


#info strat function not work. error with age labels. only wants 1 or 3, not 5

#msloc_subgraphs(graph.strat, "strategy")
mapply(function(x, y) msloc_subgraphs(x, y), x = subsetgraphs, y = subsetnames) 
