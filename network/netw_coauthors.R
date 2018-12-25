# See better version in bartomeuslab webpage!

library("devtools")
install_github("pablobarbera/scholarnetwork")

library(scholarnetwork)
d <- extractNetwork(id="EXdyoWAAAAAJ", n=5000)
str(d)
#Fix: 
#F Rodriguez     F Rodriguez  0.7500000     9
#F Rodríguez     F Rodríguez  0.7500000     9
#F Sánchez         F Sánchez  0.5000000     1
#J Pino               J Pino  0.8000000     2
#J Piñol             J Piñol  0.8333333     2
#M Má Collado   M Má Collado  0.6666667     2
#M Vila               M Vila  1.4666667     6
#M Vilà               M Vilà  5.0666667     3

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


#interactive plot:
library(igraph)
# cleaning network data
network <- graph_from_data_frame(d, directed=FALSE)

l <- layout.fruchterman.reingold(network, niter=1500) # layout
fc <- walktrap.community(network) # community detection

# node locations
nodes <- data.frame(l); names(nodes) <- c("x", "y")
nodes$cluster <- factor(fc$membership)
nodes$label <- fc$names
nodes$degree <- degree(network)

library('visNetwork') 
colnames(nodes)[4] <- "id"
nodes$shape <- "dot"  
nodes$shadow <- TRUE # Nodes will drop shadow
nodes$attribute <- as.character(nodes$cluster) 
nodes$title <- nodes$id # Text on click
#nodes$id
nodes$label <- nodes$type # Node label
net <- simplify(network, remove.multiple = T, remove.loops = T) 
deg <- degree(net, mode="all")
nodes$size <- sqrt(deg)*10 # Node size
nodes$size[1] <- 40
nodes$borderWidth <- 0.2 # Node border width

nodes$color.border <- "black"
nodes$color.highlight.background <- "orange"
nodes$color.highlight.border <- "darkred"
links <- d$edges
#links <- subset(links, Plant_gen_sp != "NA NA")
links$width <- sqrt(links$weight) # line width
links$color <- "gray"    # line color  
#links$arrows <- "middle" # arrows: 'from', 'to', or 'middle'
links$smooth <- TRUE    # should the edges be curved?
links$shadow <- FALSE    # edge shadow
colnames(links)[1:2] <- c("from", "to")
#prune
#(table(links$to))
#links <- subset(links, !to %in% c("Linum bienne", "Retama sphaerocarpa"))
#nodes <- subset(nodes, !id %in% names(which(table(links$to) == 0)))
#nodes <- subset(nodes, !id %in% names(which(table(links$from) == 0)))
#cols
#dim(nodes)
nodes$type <- nodes$cluster
map2color<-function(x,pal,limits=NULL){
  if(is.null(limits)) limits=range(x)
  pal[findInterval(x,seq(limits[1],limits[2],length.out=length(pal)+1), all.inside=TRUE)]
}
nodes$color.background <- map2color(as.numeric(nodes$cluster), pal = rainbow(200), limits=c(1,10))
#plot
v <- visNetwork(nodes, links)
vv <- visPhysics(v, timestep = 0.05, stabilization = FALSE)
visIgraphLayout(vv)
visSave(vv, "ntw.html", selfcontained = T)
