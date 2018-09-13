library("devtools")
install_github("pablobarbera/scholarnetwork")

library(scholarnetwork)
d <- extractNetwork(id="EXdyoWAAAAAJ", n=5000)
str(d)
setwd("~/Documents/R/CV/network") #I know, not working with file = "network/..."
plotNetwork(d$nodes, d$edges, file="network.html", width = 1000, height = 1000)
head(d)

#other options
library(ggplot2)
library(igraph)
# cleaning network data
network <- graph_from_data_frame(d$edges, directed=FALSE)

#detour
plot(network)
net <- simplify(network, remove.multiple = F, remove.loops = T) 
plot(net, edge.arrow.size=.4,vertex.label=NA)

colrs <- 1:max(gr$group)
ids <- V(net)
str(ids)
names(ids)
gr <- merge(data.frame(names(ids), label = names(ids)), d$nodes, sort = FALSE)
V(net)$color <- colrs[gr$group]

# Compute node degrees (#links) and use that to set node size:
deg <- degree(net, mode="all")
V(net)$size <- deg*0.7
max(V(net)$size)
V(net)$size[1] <- 22
# We could also use the audience size value:
#V(net)$size <- V(net)$audience.size*0.6

# The labels are currently node IDs.
# Setting them to NA will render no labels:
V(net)$label <- NA
V(net)$label[c(1,2,4,6,7,8,9,26,35,41)] <- c("Bartomeus", "Vilà", "Stavert",
                                             "Sol", "Magrach", "Garibaldi",
                                             "Bommarco", "Rader", "Winfree",
                                             "Tylianakis")
#V(net)$label

# Set edge width based on weight:
E(net)$width <- E(net)$weight

#change arrow size and edge color:
E(net)$arrow.size <- .2
E(net)$edge.color <- "gray80"
E(net)$width <- 1+E(net)$weight
plot(net, label.cex = 0.5, label.color = "grey80", label.family = "Arial") 

l <- layout.fruchterman.reingold(network, niter=1500) # layout
fc <- walktrap.community(network) # community detection

# node locations
nodes <- data.frame(l); names(nodes) <- c("x", "y")
nodes$cluster <- factor(fc$membership)
nodes$label <- fc$names
nodes$degree <- degree(network)

# edge locations
edgelist <- get.edgelist(network, names=FALSE)
edges <- data.frame(nodes[edgelist[,1],c("x", "y")], nodes[edgelist[,2],c("x", "y")])
names(edges) <- c("x1", "y1", "x2", "y2")
plot(net) 

# and now visualizing it...
library(ggplot2)
p <- ggplot(nodes, aes(x=x, y=y, color=cluster, label=label, size=degree))
pq <- p + geom_text(color="black", aes(label=label, size=degree),
                    show_guide=FALSE) +
  # nodes
  geom_point(color="grey20", aes(fill=cluster),
             shape=21, show_guide=FALSE, alpha=1/2) +
  # edges
  geom_segment(
    aes(x=x1, y=y1, xend=x2, yend=y2, label=NA),
    data=edges, size=0.25, color="grey20", alpha=1/5) +
  ## note that here I add a border to the points
  scale_fill_discrete(labels=labels) +
  scale_size_continuous(range = c(5, 8)) +
  theme(
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fill="white"),
    axis.line = element_blank(), axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(), panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.background = element_rect(colour = F, fill = "black"),
    legend.key = element_rect(fill = "black", colour = F),
    legend.title = element_text(color="white"),
    legend.text = element_text(color="white")
  ) +
  ## changing size of points in legend
  guides(fill = guide_legend(override.aes = list(size=5)))

pq


df <- data.frame(Source = d$edges$node1, Target = d$edges$node2)
write.csv(df, file="edgelist-gephi.csv", row.names=FALSE)

