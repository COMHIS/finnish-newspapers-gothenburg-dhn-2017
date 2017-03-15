
# http://www.kateto.net/polnet2015

source("./functions-data/crossreference-functions.R")

cross_references_1905_jsonfile <- "input-data/references_v1.json"

# install.packages("igraph")
# install.packages("network") 
# install.packages("sna")
# install.packages("ndtv")



datadir <- "~/projects/data/suomalaiset-lehdet/lehtien_levikki/"
newspaper_base_data <- read.csv(paste0(datadir, "lehti4.csv"))


crossreference_data <- create_crossreference_data(cross_references_1905_jsonfile)
# filter volume < 4
crossreference_data <- crossreference_data[crossreference_data$volume > 3,]
# filter autoreferences 
crossreference_data <- subset(crossreference_data, from != to)
filter_ids <- as.character(newspaper_base_data[newspaper_base_data$AINYLEISMAARE == "SAN", "ISSN"])
#  & newspaper_base_data$KIELI == "fin" 
crossreference_data <- subset(crossreference_data, from %in% filter_ids & to %in% filter_ids)
# crossreference_data <- subset(crossreference_data, crossreference_data$from %in% crossreference_data$to)


issn_name_mapping <- newspaper_base_data[, c("ISSN", "PAANIMEKE")]
names(issn_name_mapping) <- c("id", "title")
issn_name_mapping$id <- as.character(issn_name_mapping$id)
issn_name_mapping$title <- as.character(issn_name_mapping$title)
unique_ids1 <- unique(crossreference_data$from)
unique_ids2 <- unique(crossreference_data$to)
unique_ids3 <- c(unique_ids1, unique_ids2)
unique_ids <- unique(unique_ids3)
issn_name_mapping_for_reals <- issn_name_mapping[issn_name_mapping$id %in% unique_ids, ]

library(igraph)

net <- graph.data.frame(crossreference_data, issn_name_mapping_for_reals, directed = T)
# ceb <- cluster_edge_betweenness(net)
# plot(ceb, net)
# dendPlot(ceb, mode = "hclust")
# net2 <- graph.data.frame(crossreference_data, issn_name_mapping_for_reals, directed = F)
# net[1, 1]
# E(net)$volume
# V(net)$title
# simple_net <- simplify(net, )
# E(net)$width <- 1+E(net)$volume/30

# hist(crossreference_data$volume)
# sd(crossreference_data$volume)
# mean(crossreference_data$volume)

simple_net <- simplify(net,  remove.multiple = F, remove.loops = T)
simple_net_undirected <- as.undirected(simple_net, mode = "collapse", edge.attr.comb = list(volume = "sum", "ignore"))
E(simple_net_undirected)$width <- 1 + E(simple_net_undirected)$volume/10


ceb <- cluster_edge_betweenness(simple_net_undirected, weights = E(simple_net_undirected)$volume)
plot(ceb, simple_net_undirected, edge.arrow.size = .4, vertex.size = 5, edge.curved = .1,
     vertex.label = V(net)$title, layout = layout.kamada.kawai(simple_net_undirected)) # 
dendPlot(ceb, mode = "hclust")

cfg <- cluster_fast_greedy(simple_net_undirected, weights = E(simple_net_undirected)$volume)
plot(cfg, simple_net_undirected, edge.arrow.size = .4, vertex.size = 5, edge.curved = .1,
     vertex.label = V(net)$title, layout = layout.kamada.kawai(simple_net_undirected))
dendPlot(cfg, mode = "hclust")

clp <- cluster_label_prop(simple_net_undirected, weights = E(simple_net_undirected)$volume)
plot(clp, net, vertex.label = V(net)$title)
# dendPlot(clp, mode = "hclust")

cle <- cluster_leading_eigen(simple_net_undirected, weights = E(simple_net_undirected)$volume)
plot(cle, simple_net_undirected, edge.arrow.size = .4, vertex.size = 5, edge.curved = .1,
     vertex.label = V(net)$title, layout = layout.kamada.kawai(simple_net_undirected))

clou <- cluster_louvain(simple_net_undirected, weights = E(simple_net_undirected)$volume)
plot(clou, simple_net_undirected, edge.arrow.size = .4, vertex.size = 5, edge.curved = .1,
     vertex.label = V(net)$title, layout = layout.kamada.kawai(simple_net_undirected))
# dendPlot(clo, mode = "hclust")

cwt <- cluster_walktrap(simple_net_undirected, weights = E(simple_net_undirected)$volume, steps = 3)
plot(cwt, simple_net_undirected, edge.arrow.size = .4, vertex.size = 7, edge.curved = .1,
     vertex.label = V(net)$title) # , layout = layout.kamada.kawai(simple_net_undirected)
dendPlot(cwt, mode = "hclust")

compare(ceb, cwt)

# simple_net_nondirectional <- as.undirected(simple_net, mode = "collapse", edge.attr.comb = list(volume="sum", "ignore"))



# cut_off <- (mean(crossreference_data$volume)) /2
cut_off <- 2
net.sp <- delete.edges(simple_net, E(simple_net)[volume<cut_off])
l <- layout.fruchterman.reingold(net.sp, repulserad=vcount(net)^2.1)
plot(net.sp, layout=l) 
l <- layout.kamada.kawai(net.sp)

plot(net.sp, edge.arrow.size = .1, vertex.size = 5, edge.curved = .1,
     vertex.label = V(net)$title, layout = l)


# l <- layout.circle(net)

# l <- layout.fruchterman.reingold(net, repulserad=vcount(net)^3, 
#                                  area=vcount(net)^2.4)

# l <- layout.lgl
l <- layout.kamada.kawai(net)
# l <- layout.fruchterman.reingold


plot(net, edge.arrow.size = .4, vertex.size = 5, edge.curved = .1,
     vertex.label = V(net)$title)

plot(net, edge.arrow.size = .1, vertex.size = 5, edge.curved = .1,
     vertex.label = V(net)$title, layout = l)



