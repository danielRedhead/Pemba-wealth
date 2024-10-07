########################################.
#
#   Descriptives & plots
#
########################################.

# Clear working space
rm(list = ls())

# Set the working directory
# You will need to specify the correct file path for your computer

setwd("/Users/danielredhead/Dropbox/Pemba-inequality")       # working directory for apple
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
library(colourvalues)
library(rethinking)
library(forcats)
library(ineq)

su <- read.table("./2-processed-data/sharing_unit.csv", sep = ",", header = TRUE)
sharing <- as.matrix(read.table("./3-analyses/estimated_network.csv", sep = ",", header = TRUE, row.names = 1))
s1 <- as.matrix(read.table("./2-processed-data/su_sharing1.csv", sep = ",", header = TRUE, row.names = 1))
s2 <- as.matrix(read.table("./2-processed-data/su_sharing2.csv", sep = ",", header = TRUE, row.names = 1))
f_k <- as.matrix(read.table("./2-processed-data/su_female_k.csv", sep = ",", header = TRUE, row.names = 1))
f_q <- as.matrix(read.table("./2-processed-data/su_female_q.csv", sep = ",", header = TRUE, row.names = 1))
m_k <- as.matrix(read.table("./2-processed-data/su_male_k.csv", sep = ",", header = TRUE, row.names = 1))
m_q <- as.matrix(read.table("./2-processed-data/su_male_q.csv", sep = ",", header = TRUE, row.names = 1))
load(file = "./3-analyses/results_log_wealth.RData")

probs_net <- graph_from_adjacency_matrix(sharing, mode= "directed", weighted = TRUE )

bin_sharing <- round(sharing,)
union_net <- ifelse(s1==1 | t(s2)==1, 1,0)
intersect_net <- ifelse(s1==1 & t(s2)==1, 1,0)

##################################
# wealth descriptives
##################################

ineq(su$su_direct_wealth, type = "Gini")

poss_cost <- read.csv("./1-raw-data/possession_costs.csv", stringsAsFactors = F)
su_obs <- read.csv("./1-raw-data/su_observations.csv", stringsAsFactors = F)

poss_cost$final.prices.used.for.pe_posscost..to.submit[poss_cost$item.to.submit == "cloves_amount"]
#price is 130000, but is character. Hardcode cost 

ineq(su_obs$cloves_amount*130000, type = "Gini")

##################################
# network descriptives
##################################

networks <- list(bin_sharing,
                 s1,
                 s2,
                 union_net,
                 intersect_net,
                 f_k,
                 f_q,
                 m_k,
                 m_q
                 )

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

colnames(net_descriptives) <- c("est_net", "sharing_out", "sharing_in", "Union", "Intersection",
                                  "women_friends", "women_work", "men_friends", "men_work")

write.csv(net_descriptives, "./3-analyses/full_network_descriptives.csv")


##################################
# network digraphs
##################################

networks <- list(food_sharing = graph_from_adjacency_matrix(bin_sharing, diag = F),
                 women_friends = graph_from_adjacency_matrix(f_k, diag = F),
                 women_work = graph_from_adjacency_matrix(f_q, diag = F),
                 men_friends = graph_from_adjacency_matrix(m_k, diag = F),
                 men_work = graph_from_adjacency_matrix(m_q, diag = F)
                 )

colrs <- data.frame(
  id = su$su_id,
  wealth = su$su_direct_wealth, 
  wealth_rank = rank(su$su_direct_wealth))

colrs <- colrs[order(colrs$wealth_rank, decreasing = T),]
colrs$colour <- hcl.colors(length(colrs$wealth_rank), rev = F, palette = "TealGrn")

for(i in 1:length(networks)){
V(networks[[i]])$id <- su$su_id
V(networks[[i]])$colour <- colrs$colour[match(V(networks[[i]])$id, colrs$id)]
V(networks[[i]])$status <- su$status[match(V(networks[[i]])$name, su$su_id)]
}

data.frame(V(networks[[i]])$id, V(networks[[i]])$colour)

set.seed(3)
coords <- layout_nicely(networks[[1]])

for(i in 1:length(networks)){
pdf(paste0("./3-analyses/net_digraph_",names(networks[i]),".pdf"))
plot(networks[[i]], edge.arrow.size=0.2, 
          vertex.color= V(networks[[i]])$colour, 
          vertex.size=1.5+(0.4*degree(networks[[i]])), 
          vertex.label = NA,
          #vertex.frame.color = V(networks)$status,
          edge.curved=0.4, 
          layout = coords)
dev.off()
}

##################################
# results tables
##################################


sharing_results_table <- rbind(
    as.data.frame(res$summary_list$`False positive rate`),
    as.data.frame(res$summary_list$`Recall of true ties`),
    as.data.frame(res$summary_list$`Theta: question-order effects`),
    as.data.frame(res$summary_list$`Focal efffects: Out-degree`),
    as.data.frame(res$summary_list$`Target effects: In-degree`),
    as.data.frame(res$summary_list$`Dyadic effects`),
    as.data.frame(res$summary_list$`Other estimates`)
    )

sharing_results_table$type <- NA
sharing_results_table[1:4,]$type <- "False Positive Rate"
sharing_results_table[5:8,]$type <- "Recall of True Ties"
sharing_results_table[9:10,]$type <- "Question Duplication"
sharing_results_table[11:12,]$type <- "Sender Effects"
sharing_results_table[13,]$type <- "Receiver Effects"
sharing_results_table[c(14:17,22:23),]$type <- "Dyadic Effects"
sharing_results_table[18:21,]$type <- "Cross-Network Effects"
sharing_results_table[24:32,]$type <- "Block Effects"
sharing_results_table$type2 <- "not block"
sharing_results_table[26:28,]$type2 <- "Religious Group"
sharing_results_table[29:31,]$type2 <- "Dev. group"
# Format names
sharing_results_table[1,]$Variable <- "False positive rate intercept (outgoing)"
sharing_results_table[2,]$Variable <- "False positive rate intercept (incoming)"
sharing_results_table[3,]$Variable <- "False positive rate SD (outgoing)"
sharing_results_table[4,]$Variable <- "False positive rate SD (incoming)"

sharing_results_table[5,]$Variable <- "Recall of true ties intercept (outgoing)"
sharing_results_table[6,]$Variable <- "Recall of true ties intercept (incoming)"
sharing_results_table[7,]$Variable <- "Recall of true ties SD (outgoing)"
sharing_results_table[8,]$Variable <- "Recall of true ties SD (incoming)"
sharing_results_table[9,]$Variable <- "Question duplication intercept"
sharing_results_table[10,]$Variable <- "Question duplication SD"

sharing_results_table[11,]$Variable <- "Sender effects SD"
sharing_results_table[12,]$Variable <- "(Log) Wealth sender effect"

sharing_results_table[13,]$Variable <- "Receiver effects SD"

sharing_results_table[14,]$Variable <- "Dyadic effects SD"
sharing_results_table[15,]$Variable <- "(Log) Wealth distance"
sharing_results_table[16,]$Variable <- "Genetic relatedness"
sharing_results_table[17,]$Variable <- "Physical distance"
sharing_results_table[18,]$Variable <- "Women's friendships"
sharing_results_table[19,]$Variable <- "Women's co-working"
sharing_results_table[20,]$Variable <- "Men's friendships"
sharing_results_table[21,]$Variable <- "Men's co-working"
sharing_results_table[22,]$Variable <- "Generalized reciprocity"
sharing_results_table[23,]$Variable <- "Dyadic reciprocity"

sharing_results_table[24,]$Variable <- "Any to Any"
sharing_results_table[25,]$Variable <- "Religious status to Religious status"
sharing_results_table[26,]$Variable <- "Religious status to No religious status"
sharing_results_table[27,]$Variable <- "No religious status to Religious status"
sharing_results_table[28,]$Variable <- "No religious status to No religious status"
sharing_results_table[29,]$Variable <- "Non-dev. group to Non-dev. group"
sharing_results_table[30,]$Variable <- "Non-dev. group to Dev. group"
sharing_results_table[31,]$Variable <- "Dev. group to Non-dev. group"
sharing_results_table[32,]$Variable <- "Dev. group to Dev. group"

write.csv(sharing_results_table,"./3-analyses/sharing_results.csv", row.names = FALSE)
colnames(sharing_results_table)[colnames(sharing_results_table)=="HPDI:0.05"] <- "lower"
colnames(sharing_results_table)[colnames(sharing_results_table)=="HPDI:0.95"] <- "upper"

gender_results <- list(
                    female_friends = rbind(
                        as.data.frame(f_k_res$summary_list$`Focal efffects: Out-degree`),
                        as.data.frame(f_k_res$summary_list$`Target effects: In-degree`),
                        as.data.frame(f_k_res$summary_list$`Dyadic effects`),
                        as.data.frame(f_k_res$summary_list$`Other estimates`)
                        ) ,
                    female_work = rbind(
                        as.data.frame(f_q_res$summary_list$`Focal efffects: Out-degree`),
                        as.data.frame(f_q_res$summary_list$`Target effects: In-degree`),
                        as.data.frame(f_q_res$summary_list$`Dyadic effects`),
                        as.data.frame(f_q_res$summary_list$`Other estimates`)
                        ),                     
                    male_friends = rbind(
                        as.data.frame(m_k_res$summary_list$`Focal efffects: Out-degree`),
                        as.data.frame(m_k_res$summary_list$`Target effects: In-degree`),
                        as.data.frame(m_k_res$summary_list$`Dyadic effects`),
                        as.data.frame(m_k_res$summary_list$`Other estimates`)
                        ) ,
                    male_work = rbind(
                        as.data.frame(m_q_res$summary_list$`Focal efffects: Out-degree`),
                        as.data.frame(m_q_res$summary_list$`Target effects: In-degree`),
                        as.data.frame(m_q_res$summary_list$`Dyadic effects`),
                        as.data.frame(m_q_res$summary_list$`Other estimates`)
                        )
                    ) 
  
for(i in 1:length(gender_results)) {                  
gender_results[[i]]$type <- NA
gender_results[[i]]$type2 <- "not block"

gender_results[[i]][1:2,]$type <- "Sender Effects"
gender_results[[i]][3,]$type <- "Receiver Effects"
gender_results[[i]][c(4:7,12:13),]$type <- "Dyadic Effects"
gender_results[[i]][8:11,]$type <- "Cross-Network Effects"
gender_results[[i]][14:22,]$type <- "Block Effects"

gender_results[[i]][15:18,]$type2 <- "Religious Group"
gender_results[[i]][19:22,]$type2 <- "Dev. group"

gender_results[[i]][1,]$Variable <- "Sender effects SD"
gender_results[[i]][2,]$Variable <- "(Log) Wealth sender effect"

gender_results[[i]][3,]$Variable <- "Receiver effects SD"

gender_results[[i]][4,]$Variable <- "Dyadic effects SD"
gender_results[[i]][5,]$Variable <- "(Log) Wealth distance"
gender_results[[i]][6,]$Variable <- "Genetic relatedness"
gender_results[[i]][7,]$Variable <- "Physical distance"
gender_results[[i]][12,]$Variable <- "Generalized reciprocity"
gender_results[[i]][13,]$Variable <- "Dyadic reciprocity"

gender_results[[i]][14,]$Variable <- "Any to Any"
gender_results[[i]][15,]$Variable <- "Religious status to Religious status"
gender_results[[i]][16,]$Variable <- "Religious status to No religious status"
gender_results[[i]][17,]$Variable <- "No religious status to Religious status"
gender_results[[i]][18,]$Variable <- "No religious status to No religious status"
gender_results[[i]][19,]$Variable <- "Non-dev. group to Non-dev. group"
gender_results[[i]][20,]$Variable <- "Non-dev. group to Dev. group"
gender_results[[i]][21,]$Variable <- "Dev. group to Non-dev. group"
gender_results[[i]][22,]$Variable <- "Dev. group to Dev. group"
}

gender_results[[1]][8, ]$Variable <- "Women's co-working"
gender_results[[1]][9, ]$Variable <- "Food sharing"
gender_results[[1]][10, ]$Variable <- "Men's friendships"
gender_results[[1]][11, ]$Variable <- "Men's co-working"

gender_results[[2]][8, ]$Variable <- "Women's friendships"
gender_results[[2]][9, ]$Variable <- "Food sharing"
gender_results[[2]][10, ]$Variable <- "Men's friendships"
gender_results[[2]][11, ]$Variable <- "Men's co-working"

gender_results[[3]][8, ]$Variable <- "Women's friendships"
gender_results[[3]][9, ]$Variable <- "Women's co-working"
gender_results[[3]][10, ]$Variable <- "Food sharing"
gender_results[[3]][11, ]$Variable <- "Men's co-working"

gender_results[[4]][8, ]$Variable <- "Women's friendships"
gender_results[[4]][9, ]$Variable <- "Women's co-working"
gender_results[[4]][10, ]$Variable <- "Men's friendships"
gender_results[[4]][11, ]$Variable <- "Food sharing"

write.csv(gender_results[[1]],"./3-analyses/women_friends_results.csv", row.names = FALSE)
write.csv(gender_results[[2]],"./3-analyses/women_work_results.csv", row.names = FALSE)
write.csv(gender_results[[3]],"./3-analyses/men_friends_results.csv", row.names = FALSE)
write.csv(gender_results[[4]],"./3-analyses/men_work_results.csv", row.names = FALSE)

for(i in 1:length(gender_results)){
colnames(gender_results[[i]])[colnames(gender_results[[i]])=="HPDI:L"] <- "lower"
colnames(gender_results[[i]])[colnames(gender_results[[i]])=="HPDI:H"] <- "upper"
gender_results[[i]]$Network <- names(gender_results[i])
}

sharing_results_table$Network <- "Food sharing"
gender_results[[1]]$Network <- "Women's friendships"
gender_results[[2]]$Network <- "Women's co-working"
gender_results[[3]]$Network <- "Men's friendships"
gender_results[[4]]$Network <- "Men's co-working"

df <- rbind(sharing_results_table, do.call(rbind, gender_results))

df$Network <- rev(levels(factor(df$Network)))

indiv_df <- df[df$type %in% c("Sender Effects", "Receiver Effects"), ]
dyad_df <- df[df$type %in% c("Dyadic Effects"), ]
net_df <- df[df$type %in% c("Cross-Network Effects"), ]
rel_block_df <- df[df$type2 %in% c("Religious Group"), ] 
dev_block_df <- df[df$type2 %in% c("Dev. group"), ] 

indiv_fig <- ggplot(data=indiv_df,
    aes(x=Variable, y=as.numeric(Median), color = Network,
        ymin=as.numeric(lower), ymax=as.numeric(upper))) +
    geom_pointrange(position=position_dodge(width = 0.5), size = 1.2) +
    geom_hline(yintercept= 0, lty=2, aes(fill=Network)) +
    coord_flip() +
    scale_color_manual(values = c("#481567FF", "#287D8EFF", "#AFEEEE", "#00cca5","#ACF0B0")) +
    xlab("") + ylab("Estimate (95% CI)") +
    theme_bw() +
    theme(axis.text=element_text(size=15), axis.title=element_text(size=20), legend.text=element_text(size=15)) +
    guides(colour = guide_legend(reverse=T))  
indiv_fig

dyad_fig <- ggplot(data=dyad_df,
    aes(x=Variable, y=as.numeric(Median), color = Network,
        ymin=as.numeric(lower), ymax=as.numeric(upper))) +
    geom_pointrange(position=position_dodge(width = 0.5), size = 2) +
    geom_hline(yintercept= 0, lty=2, aes(fill=Network)) +
    coord_flip() +
    scale_color_manual(values = c("#481567FF", "#287D8EFF", "#AFEEEE", "#00cca5","#ACF0B0")) +
    xlab("") + ylab("Estimate (95% CI)") +
    theme_bw() +
    theme(axis.text=element_text(size=20), axis.title=element_text(size=20), legend.text=element_text(size=20)) +
    guides(colour = guide_legend(reverse=T)) 
dyad_fig

net_fig <- ggplot(data=net_df,
    aes(x=Variable, y=as.numeric(Median), color = Network,
        ymin=as.numeric(lower), ymax=as.numeric(upper))) +
    geom_pointrange(position=position_dodge(width = 0.5), size = 2) +
    geom_hline(yintercept= 0, lty=2, aes(fill=Network)) +
    coord_flip() +
    scale_color_manual(values = c("#481567FF", "#287D8EFF", "#AFEEEE", "#00cca5","#ACF0B0")) +
    xlab("") + ylab("Estimate (95% CI)") +
    theme_bw() +
    theme(axis.text=element_text(size=20), axis.title=element_text(size=20), legend.text=element_text(size=20)) +
    guides(colour = guide_legend(reverse=T)) 
net_fig


ggsave(indiv_fig, filename = "./3-analyses/indiv_effects_figure.pdf", device =  "pdf", width = 14, height = 8)
ggsave(dyad_fig, filename = "./3-analyses/dyad_effects_figure.pdf", device =  "pdf", width = 14, height = 18)
ggsave(net_fig, filename = "./3-analyses/network_effects_figure.pdf", device =  "pdf", width = 14, height = 18)

res_list <- list(food_sharing = res, women_friends = f_k_res, women_work = f_q_res, men_friends = m_k_res, men_work = m_q_res)
rel_ss <- list()
dev_ss <- list()
rel_block_samples <- list()
dev_block_samples <- list()

for(i in 1:length(res_list)){
        rel_block_samples[[i]] <- res_list[[i]]$samples$srm_model_samples$block_parameters[[2]]
        dev_block_samples[[i]] <- res_list[[i]]$samples$srm_model_samples$block_parameters[[3]]

    for(j in 1:2000){
        rel_ss[[i]] <- rel_block_samples[[i]][j,,]
        rel_ss[[i]]  <- rel_ss[[i]] - rel_ss[[i]][1,1]
        rel_block_samples[[i]][j, , ] <- rel_ss[[i]] 
        dev_ss[[i]] <- dev_block_samples[[i]][j,,]
        dev_ss[[i]]  <- dev_ss[[i]]-dev_ss[[i]][2,2]
        dev_block_samples[[i]][j, , ] <- dev_ss[[i]]
     }
 
  }

names(rel_block_samples) <- names(res_list)
names(dev_block_samples) <- names(res_list)

res_tables <- list(food_sharing = df[df$Network == "Food sharing",], women_friends = df[df$Network == "Women's friendships",], women_work = df[df$Network == "Women's co-working",], men_friends = df[df$Network == "Men's friendships",], men_work = df[df$Network == "Men's co-working",]) 
rel_contrasts <- list()
dev_contrasts <- list()

#Create contrasts for block effects
for(i in 1:length(res_list)){

rel_contrasts[[i]] <- res_tables[[i]][res_tables[[i]]$type2 == "Religious Group" , colnames(res_tables[[i]]) %in% c("Variable", "Median", "lower", "upper", "Network")]
rel_contrasts[[i]]$Median[rel_contrasts[[i]]$Variable == "No religious status to No religious status"] <- median(rel_block_samples[[i]][, 2,2])
rel_contrasts[[i]]$Median[rel_contrasts[[i]]$Variable == "No religious status to Religious status"] <- median(rel_block_samples[[i]][, 2, 1])
rel_contrasts[[i]]$Median[rel_contrasts[[i]]$Variable == "Religious status to No religious status"] <- median(rel_block_samples[[i]][, 1, 2])
rel_contrasts[[i]]$lower[rel_contrasts[[i]]$Variable == "No religious status to No religious status"] <- HPDI(rel_block_samples[[i]][, 2,2])[1]
rel_contrasts[[i]]$upper[rel_contrasts[[i]]$Variable == "No religious status to No religious status"] <- HPDI(rel_block_samples[[i]][, 2,2])[2]
rel_contrasts[[i]]$lower[rel_contrasts[[i]]$Variable == "No religious status to Religious status"] <- HPDI(rel_block_samples[[i]][, 2, 1])[1]
rel_contrasts[[i]]$upper[rel_contrasts[[i]]$Variable == "No religious status to Religious status"] <- HPDI(rel_block_samples[[i]][, 2, 1])[2]
rel_contrasts[[i]]$lower[rel_contrasts[[i]]$Variable == "Religious status to No religious status"] <- HPDI(rel_block_samples[[i]][, 1, 2])[1]
rel_contrasts[[i]]$upper[rel_contrasts[[i]]$Variable == "Religious status to No religious status"] <- HPDI(rel_block_samples[[i]][, 1, 2])[2]

dev_contrasts[[i]] <- res_tables[[i]][res_tables[[i]]$type2 == "Dev. group" , colnames(res_tables[[i]]) %in% c("Variable", "Median", "lower", "upper", "Network")]
dev_contrasts[[i]]$Median[dev_contrasts[[i]]$Variable == "Non-dev. group to Non-dev. group"] <- median(dev_block_samples[[i]][, 1,1])
dev_contrasts[[i]]$Median[dev_contrasts[[i]]$Variable == "Non-dev. group to Dev. group"] <- median(dev_block_samples[[i]][, 1, 2])
dev_contrasts[[i]]$Median[dev_contrasts[[i]]$Variable == "Dev. group to Non-dev. group"] <- median(dev_block_samples[[i]][, 2, 1])
dev_contrasts[[i]]$lower[dev_contrasts[[i]]$Variable == "Non-dev. group to Non-dev. group"] <- HPDI(dev_block_samples[[i]][, 1,1])[1]
dev_contrasts[[i]]$upper[dev_contrasts[[i]]$Variable == "Non-dev. group to Non-dev. group"] <- HPDI(dev_block_samples[[i]][, 1,1])[2]
dev_contrasts[[i]]$lower[dev_contrasts[[i]]$Variable == "Non-dev. group to Dev. group"] <- HPDI(dev_block_samples[[i]][, 1,2])[1]
dev_contrasts[[i]]$upper[dev_contrasts[[i]]$Variable == "Non-dev. group to Dev. group"] <- HPDI(dev_block_samples[[i]][, 1,2])[2]
dev_contrasts[[i]]$lower[dev_contrasts[[i]]$Variable == "Dev. group to Non-dev. group"] <- HPDI(dev_block_samples[[i]][, 2,1])[1]
dev_contrasts[[i]]$upper[dev_contrasts[[i]]$Variable == "Dev. group to Non-dev. group"] <- HPDI(dev_block_samples[[i]][, 2,1])[2]

}


rel_contrast_df <- do.call(rbind, rel_contrasts)
dev_contrast_df <- do.call(rbind, dev_contrasts)

rel_block_df <- rel_block_df[ ,colnames(rel_block_df) %in% colnames(rel_contrast_df)]
rel_block_df$plot <- "Estimate"
rel_contrast_df$plot <- "Contrast"
religion_df <- rbind(rel_block_df,rel_contrast_df)

dev_block_df <- dev_block_df[ ,colnames(dev_block_df) %in% colnames(dev_contrast_df)]
dev_block_df$plot <- "Estimate"
dev_contrast_df$plot <- "Contrast"
development_df <- rbind(dev_block_df,dev_contrast_df)

religion_df[religion_df$Variable == "Religious status to Religious status" & religion_df$plot == "Contrast", c("Median", "lower", "upper")] <- 0
development_df[development_df$Variable == "Dev. group to Dev. group" & development_df$plot == "Contrast", c("Median", "lower", "upper")] <- 0

religion_df <- rbind(religion_df, data.frame(Variable = "Religious status to Religious status", Median = 0, lower = 0, upper = 0, Network = "Food sharing", plot = "Contrast"))
development_df <- rbind(development_df, data.frame(Variable = "Dev. group to Dev. group", Median = 0, lower = 0, upper = 0, Network = "Food sharing", plot = "Contrast"))


rel_block_fig <- ggplot(data=religion_df,
    aes(x=fct_rev(Variable), y=as.numeric(Median), color = Network,
        ymin=as.numeric(lower), ymax=as.numeric(upper))) +
    geom_pointrange(position=position_dodge(width = 0.5), size = 1.2) +
    geom_hline(yintercept= 0, lty=2, aes(fill=Network)) +
    facet_grid( ~ fct_rev(plot) ,scales="free",space = "free_y")+ labs(x=" ") +
    coord_flip() +
    scale_color_manual(values = c("#481567FF", "#287D8EFF", "#AFEEEE", "#00cca5","#ACF0B0")) +
    xlab("") + ylab("") +
    theme_bw() +
    theme(axis.text=element_text(size=15), axis.title=element_text(size=20), legend.text=element_text(size=15), panel.spacing = unit(1.5, "lines")) +
    guides(colour = guide_legend(reverse=T)) 
rel_block_fig

dev_block_fig <- ggplot(data=development_df,
    aes(x=Variable, y=as.numeric(Median), color = Network,
        ymin=as.numeric(lower), ymax=as.numeric(upper))) +
    geom_pointrange(position=position_dodge(width = 0.5), size = 1.2) +
    geom_hline(yintercept= 0, lty=2, aes(fill=Network)) +
    facet_grid( ~ fct_rev(plot) ,scales="free",space = "free_y")+ labs(x=" ") +
    coord_flip() +
    scale_color_manual(values = c("#481567FF", "#287D8EFF", "#AFEEEE", "#00cca5","#ACF0B0")) +
    xlab("") + ylab("") +
    theme_bw() + 
    theme(axis.text=element_text(size=15), axis.title=element_text(size=20), legend.text=element_text(size=15), panel.spacing = unit(1.5, "lines")) +
    guides(colour = guide_legend(reverse=T)) 
dev_block_fig

ggsave(rel_block_fig, filename = "./3-analyses/religion_block_effects_figure.pdf", device =  "pdf", width = 14, height = 8)
ggsave(dev_block_fig, filename = "./3-analyses/development_block_effects_figure.pdf", device =  "pdf", width = 14, height = 8)



