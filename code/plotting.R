########################################.
#
#   PE Analyses  
#   Daniel Redhead
#
########################################.

# Clear working space
rm(list = ls())

sitecode <- "PE"

# Set the working directory
# You will need to specify the correct file path for your computer

setwd(paste0("/Volumes/ecology/eco_endow/endow-",sitecode, sep = ""))       # working directory for apple

# Load libraries
library(igraph)
library(ineq)
library(ggplot2)

# Read in the tables
indiv <- read.csv("./database/people_observations.csv", header = TRUE)
people <- read.csv("./database/people.csv", header = TRUE)
el <- read.csv("./analyses/preliminary-analyses/outputs/predicted_edgelist.csv", header = TRUE, row.names = 1)
wealth <- read.csv(paste0("./analyses/wealth/",sitecode,"_SU_wealth.csv"), header = TRUE)

# Convert to igraph format
i_el <- graph_from_edgelist(as.matrix(el))

# Do some standardization
wealth$SharingUnit2 <- tolower(wealth$SharingUnit2)

# Add group ID for not colouring
indiv$group_id <- people$group_id[match(indiv$personid, people$ego)]
wealth$group_id <- indiv$group_id[match(wealth$SharingUnit2, indiv$su_id_2)]

#Calculate in/out degree and append to SU wealth table
wealth$in_deg <- degree(i_el, mode = "in")[match(wealth$SharingUnit2, V(i_el)$name)]
wealth$out_deg <- degree(i_el, mode = "out")[match(wealth$SharingUnit2, V(i_el)$name)]
wealth$between <- betweenness(i_el, normalized = TRUE)[match(wealth$SharingUnit2, V(i_el)$name)]

# Calculate Gini
gini <- ineq(wealth$CashValue_ssu, type = "Gini")

# Calculate in/out degree centralization
in_cent <- centralization.degree(i_el, mode = "in")$centralization
out_cent <- centralization.degree(i_el, mode = "out")$centralization
bet_cent <- centralization.betweenness(i_el)$centralization


# Plot figure with indegree 
png(paste0("/Volumes/ecology/eco_endow/endow-analyses/2022-NSF-application/outputs/within-site-digraphs/",sitecode,"-in-network.png"))
par(fig = c(0,1,0,1), mar = c(5, 5, 3.5, 0))
# First the igraph plot (i.e., the network)
plot(i_el, 
     vertex.color = as.factor(wealth$group_id[match(V(i_el)$name, wealth$SharingUnit2)]), 
     vertex.label = NA, 
     vertex.size = scales::rescale(as.vector(wealth$CashValue_ssu[match(V(i_el)$name, wealth$SharingUnit2)]), to = c(0,15)), 
     edge.arrow.size = 0.25, 
     edge.curved = TRUE, 
     main = paste0(sitecode," \n Material Wealth Gini =", round(gini,3), "\nIn-Degree Centralization = ", round(in_cent,3))
)

par(fig = c(0,0.4, 0, 0.5), new = T, xpd=FALSE, mgp=c(1.5,0.6,0))  
# Then the Lorenz curve
plot(Lc(wealth$CashValue_ssu), 
     col = "seagreen",
     xlab = "Cum. Prop. of Residents", 
     ylab = "Cum. Prop. of Wealth", 
     main = "",
     cex.lab = 0.7, 
     cex.axis = 0.6)
lines(Lc(wealth$in_deg), 
      col = "magenta4", 
      lty = 2)
legend("topleft", legend=c("Material Wealth", "Relational Wealth"),
       col=c("seagreen", "magenta4"), lty=1:2, cex = 0.5)
dev.off()

# Plot figure with outdegree
png(paste0("/Volumes/ecology/eco_endow/endow-analyses/2022-NSF-application/outputs/within-site-digraphs/",sitecode,"-out-network.png"))
par(fig = c(0,1,0,1), mar = c(5, 5, 3.5, 0))
plot(i_el, 
     vertex.color = as.factor(wealth$group_id[match(V(i_el)$name, wealth$SharingUnit2)]), 
     vertex.label = NA, 
     vertex.size = scales::rescale(as.vector(wealth$CashValue_ssu[match(V(i_el)$name, wealth$SharingUnit2)]), to = c(0,15)), 
     edge.arrow.size = 0.25, 
     edge.curved = TRUE, 
     main = paste0(sitecode," \n Material Wealth Gini =", round(gini,3), "\nOut-Degree Centralization = ", round(out_cent,3))
)

par(fig = c(0,0.4, 0, 0.5), new = T, xpd=FALSE, mgp=c(1.5,0.6,0))  
plot(Lc(wealth$CashValue_ssu), 
     col = "seagreen",
     xlab = "Cum. Prop. of Residents", 
     ylab = "Cum. Prop. of Wealth", 
     main = "",
     cex.lab = 0.7, 
     cex.axis = 0.6)
lines(Lc(wealth$out_deg), 
      col = "magenta4", 
      lty = 2)
legend("topleft", legend=c("Material Wealth", "Relational Wealth"),
       col=c("seagreen", "magenta4"), lty=1:2, cex = 0.5)
dev.off()


# Plot figure with betweenness
png(paste0("/Volumes/ecology/eco_endow/endow-analyses/2022-NSF-application/outputs/within-site-digraphs/",sitecode,"-betweenness-network.png"))
par(fig = c(0,1,0,1), mar = c(5, 5, 3.5, 0))
plot(i_el, 
     vertex.color = as.factor(wealth$group_id[match(V(i_el)$name, wealth$SharingUnit2)]), 
     vertex.label = NA, 
     vertex.size = scales::rescale(as.vector(wealth$CashValue_ssu[match(V(i_el)$name, wealth$SharingUnit2)]), to = c(0,15)), 
     edge.arrow.size = 0.25, 
     edge.curved = TRUE, 
     main = paste0(sitecode," \n Material Wealth Gini =", round(gini,3), "\n Betweenness Centralization = ", round(bet_cent,3))
)

par(fig = c(0,0.4, 0, 0.5), new = T, xpd=FALSE, mgp=c(1.5,0.6,0))  
plot(Lc(wealth$CashValue_ssu), 
     col = "seagreen",
     xlab = "Cum. Prop. of Residents", 
     ylab = "Cum. Prop. of Wealth", 
     main = "",
     cex.lab = 0.7, 
     cex.axis = 0.6)
lines(Lc(wealth$between), 
      col = "magenta4", 
      lty = 2)
legend("topleft", legend=c("Material Wealth", "Relational Wealth"),
       col=c("seagreen", "magenta4"), lty=1:2, cex = 0.5)
dev.off()



options(scipen=10000)
# scatterplot of in-degree & wealth
png(paste0("/Volumes/ecology/eco_endow/endow-analyses/2022-NSF-application/outputs/within-site-degree-wealth/",sitecode,"-in-deg.png"))
ggplot(wealth, aes(x=in_deg, y=CashValue_ssu)) + 
    geom_jitter(width = 0.08) +
    geom_smooth(method = "lm") +
scale_x_continuous(breaks = seq(0, 13, by = 1)) +
  #  geom_text(aes(label=gsub("^.*?_","",site)),hjust=0, vjust=0) +   
     xlab("In-Degree") +
     ylab("Material Wealth (USD)") +
     ggtitle(sitecode)+
     theme_bw()  +
     theme(plot.title = element_text(hjust = 0.5))
dev.off()

# scatterplot of out-degree & wealth
png(paste0("/Volumes/ecology/eco_endow/endow-analyses/2022-NSF-application/outputs/within-site-degree-wealth/",sitecode,"-out-deg.png"))
ggplot(wealth, aes(x=out_deg, y=CashValue_ssu)) + 
    geom_jitter(width = 0.08) +
    geom_smooth(method = "lm") +
    scale_x_continuous(breaks = seq(0, 13, by = 1)) +
  #  geom_text(aes(label=gsub("^.*?_","",site)),hjust=0, vjust=0) +
     xlab("Out-Degree") +
     ylab("Material Wealth (USD)") +
     ggtitle(sitecode) +
     theme_bw()  +
     theme(plot.title = element_text(hjust = 0.5))
dev.off()

# scatterplot of betweenness & wealth
png(paste0("/Volumes/ecology/eco_endow/endow-analyses/2022-NSF-application/outputs/within-site-degree-wealth/",sitecode,"-betweenness.png"))
ggplot(wealth, aes(x=between, y=CashValue_ssu)) + 
    geom_jitter(width = 0.0001) +
    geom_smooth(method = "lm") +
    #scale_x_continuous(breaks = seq(0, 3, by = 0.001)) +
  #  geom_text(aes(label=gsub("^.*?_","",site)),hjust=0, vjust=0) +
     xlab("betweenness") +
     ylab("Material Wealth (USD)") +
     ggtitle(sitecode) +
     theme_bw()  +
     theme(plot.title = element_text(hjust = 0.5))
dev.off()
