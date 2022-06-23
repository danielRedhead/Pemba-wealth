########################################.
#
#   LNM Analyses  
#
########################################.

# Clear working space
rm(list = ls())

# Set the working directory
# You will need to specify the correct file path for your computer

setwd("/Users/danielredhead/Pemba-wealth")       # working directory for apple
#setwd("./Pemba-wealth")       # working directory for apple

# Load function
normalize <- function(y) {
    x<-y[!is.na(y)]
    x<-(x - min(x)) / (max(x) - min(x))
    y[!is.na(y)]<-x
    return(y)
}

# Load libraries
library(igraph)
library(ggplot2)


w <- read.table("./2-processed-data/su_wealth.csv", sep = ",", header = TRUE)
sharing <- as.matrix(read.table("./3-analyses/estimated_network.csv", sep = ",", header = TRUE, row.names = 1))
s1 <- as.matrix(read.table("./2-processed-data/su_sharing1.csv", sep = ",", header = TRUE, row.names = 1))
s2 <- as.matrix(read.table("./2-processed-data/su_sharing2.csv", sep = ",", header = TRUE, row.names = 1))


w$SharingUnit2 <- tolower(w$SharingUnit2) 

all(w$SharingUnit2 == colnames(sharing))

probs_net <- graph_from_adjacency_matrix(sharing, mode= "directed", weighted = TRUE )

bin_sharing <- round(sharing,)
union_net <- ifelse(s1==1 | t(s2)==1, 1,0)
intersect_net <- ifelse(s1==1 & t(s2)==1, 1,0)

# network descriptives

networks <- list(bin_sharing,
                 s1,
                 s2,
                 union_net,
                 intersect_net)

rou <- list()
rin <- list()
net_desc <- list()

for( i in 1:length(networks)) {
    rou[[i]] = range(rowSums(networks[[i]]))   
    rin[[i]] = range(colSums(networks[[i]]))

    net_desc[[i]] <- t(data.frame(   
    	n_households = nrow(networks[[i]]),
        n_ties = sum(networks[[i]], na.rm = T),
        density = round(edge_density(graph_from_adjacency_matrix(networks[[i]])), 3),                 # get the network density
        reciprocity = round(reciprocity(graph_from_adjacency_matrix(networks[[i]])), 3),                  # general tendency towards reciprocity
        transitivity = round(transitivity(graph_from_adjacency_matrix(networks[[i]])), 3),                 # general tendency towards transitivity
        m_degree = round(mean(rowSums(networks[[i]])),3) ,             # indegree for plotting
        r_indegree = paste(rin[[i]][1], rin[[i]][2], sep = " - "),
        r_outdegree = paste(rou[[i]][1], rou[[i]][2], sep = " - ")
    )
  )

}

net_descriptives <- do.call(cbind, net_desc)

colnames(net_descriptives) <- c("est_net", "sharing_out", "sharing_in", "Union", "Intersection")

write.csv(net_descriptives, "./3-analyses/full_network_descriptives.csv")


V(probs_net)$colour <- "deepskyblue4"

pdf("./3-analyses/est_net_digraph.pdf")
plot(probs_net, edge.arrow.size=0.2,edge.arrow.width= E(probs_net)$weight,weights = E(probs_net)$weight, vertex.color= V(probs_net)$colour, vertex.size=2.5+(0.6*degree(probs_net)), 
      vertex.label = NA, edge.curved=0.4, edge.width = E(probs_net)$weight, layout = layout_nicely)
dev.off()
