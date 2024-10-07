########################################.
#
#   LNM Analyses  
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


su <- read.table("./2-processed-data/sharing_unit.csv", sep = ",", header = TRUE)
sharing <- as.matrix(read.table("./3-analyses/estimated_network.csv", sep = ",", header = TRUE, row.names = 1))
s1 <- as.matrix(read.table("./2-processed-data/su_sharing1.csv", sep = ",", header = TRUE, row.names = 1))
s2 <- as.matrix(read.table("./2-processed-data/su_sharing2.csv", sep = ",", header = TRUE, row.names = 1))
f_k <- as.matrix(read.table("./2-processed-data/su_female_k.csv", sep = ",", header = TRUE, row.names = 1))
f_q <- as.matrix(read.table("./2-processed-data/su_female_q.csv", sep = ",", header = TRUE, row.names = 1))
m_k <- as.matrix(read.table("./2-processed-data/su_male_k.csv", sep = ",", header = TRUE, row.names = 1))
m_q <- as.matrix(read.table("./2-processed-data/su_male_q.csv", sep = ",", header = TRUE, row.names = 1))
load(file = "./3-analyses/results_no_overlap.RData")

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
sharing_results_table[14:19,]$type <- "Dyadic Effects"
sharing_results_table[20:28,]$type <- "Block Effects"

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
sharing_results_table[12,]$Variable <- "Wealth sender effect"

sharing_results_table[13,]$Variable <- "Receiver effects SD"

sharing_results_table[14,]$Variable <- "Dyadic effects SD"
sharing_results_table[15,]$Variable <- "Wealth distance"
sharing_results_table[16,]$Variable <- "Genetic relatedness"
sharing_results_table[17,]$Variable <- "Physical distance"
sharing_results_table[18,]$Variable <- "Generalized reciprocity"
sharing_results_table[19,]$Variable <- "Dyadic reciprocity"

sharing_results_table[20,]$Variable <- "Any to Any"
sharing_results_table[21,]$Variable <- "religious group to religious group"
sharing_results_table[22,]$Variable <- "religious group to non-group"
sharing_results_table[23,]$Variable <- "Non-group to religious group"
sharing_results_table[24,]$Variable <- "Non-group to religious group"
sharing_results_table[25,]$Variable <- "Non-group (develop) to non-group (develop)"
sharing_results_table[26,]$Variable <- "Non-group (develop) to develop group"
sharing_results_table[27,]$Variable <- "Develop group to non-group (develop)"
sharing_results_table[28,]$Variable <- "Develop group to develop group"

write.csv(sharing_results_table,"./3-analyses/no_overlap_sharing_results.csv", row.names = FALSE)
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
gender_results[[i]][1:2,]$type <- "Sender Effects"
gender_results[[i]][3,]$type <- "Receiver Effects"
gender_results[[i]][4:9,]$type <- "Dyadic Effects"
gender_results[[i]][10:18,]$type <- "Block Effects"

gender_results[[i]][1,]$Variable <- "Sender effects SD"
gender_results[[i]][2,]$Variable <- "Wealth sender effect"

gender_results[[i]][3,]$Variable <- "Receiver effects SD"

gender_results[[i]][4,]$Variable <- "Dyadic effects SD"
gender_results[[i]][5,]$Variable <- "Wealth distance"
gender_results[[i]][6,]$Variable <- "Genetic relatedness"
gender_results[[i]][7,]$Variable <- "Physical distance"
gender_results[[i]][8,]$Variable <- "Generalized reciprocity"
gender_results[[i]][9,]$Variable <- "Dyadic reciprocity"
gender_results[[i]][10,]$Variable <- "Any to Any"

gender_results[[i]][11,]$Variable <- "religious group to religious group"
gender_results[[i]][12,]$Variable <- "religious group to Non-group "
gender_results[[i]][13,]$Variable <- "Non-group to religious group"
gender_results[[i]][14,]$Variable <- "Non-group to Non-group"

gender_results[[i]][15,]$Variable <- "Non-group (develop) to Non-group (develop)"
gender_results[[i]][16,]$Variable <- "Non-group (develop) to develop group"
gender_results[[i]][17,]$Variable <- "develop group to Non-group (develop)"
gender_results[[i]][18,]$Variable <- "develop group to develop group"
}

write.csv(gender_results[[1]],"./3-analyses/no_overlap_women_friends_results.csv", row.names = FALSE)
write.csv(gender_results[[2]],"./3-analyses/no_overlap_women_work_results.csv", row.names = FALSE)
write.csv(gender_results[[3]],"./3-analyses/no_overlap_men_friends_results.csv", row.names = FALSE)
write.csv(gender_results[[4]],"./3-analyses/no_overlap_men_work_results.csv", row.names = FALSE)

for(i in 1:length(gender_results)){
colnames(gender_results[[i]])[colnames(gender_results[[i]])=="HPDI:L"] <- "lower"
colnames(gender_results[[i]])[colnames(gender_results[[i]])=="HPDI:H"] <- "upper"
}

sharing_results_table$network <- "Food sharing"
gender_results[[1]]$network <- "Women's friendships"
gender_results[[2]]$network <- "Women's co-working"
gender_results[[3]]$network <- "Men's friendships"
gender_results[[4]]$network <- "Men's co-working"

df <- rbind(sharing_results_table, do.call(rbind, gender_results))

indiv_df <- df[df$type %in% c("Sender Effects", "Receiver Effects"), ]
dyad_df <- df[df$type %in% c("Dyadic Effects"), ]
block_df <- df[df$type %in% c("Block Effects"), ]

indiv_fig <- ggplot(data=indiv_df,
    aes(x=Variable, y=as.numeric(Median), color = network,
        ymin=as.numeric(lower), ymax=as.numeric(upper))) +
    geom_pointrange(position=position_dodge(width = 0.5)) +
    geom_hline(yintercept= 0, lty=2, aes(fill=network)) +
    coord_flip() +
    scale_color_manual(values = c("#481567FF","#CC99FF","#287D8EFF", "#00cca5","#ACF0B0")) +
    xlab("") + ylab("Estimate (95% CI)") +
    theme_bw() +
   theme(axis.text=element_text(size=20), axis.title=element_text(size=20), legend.text=element_text(size=20))
indiv_fig

dyad_fig <- ggplot(data=dyad_df,
    aes(x=Variable, y=as.numeric(Median), color = network,
        ymin=as.numeric(lower), ymax=as.numeric(upper))) +
    geom_pointrange(position=position_dodge(width = 0.5)) +
    geom_hline(yintercept= 0, lty=2, aes(fill=network)) +
    coord_flip() +
    scale_color_manual(values = c("#481567FF","#CC99FF","#287D8EFF", "#00cca5","#ACF0B0")) +
    xlab("") + ylab("Estimate (95% CI)") +
    theme_bw() +
   theme(axis.text=element_text(size=20), axis.title=element_text(size=20), legend.text=element_text(size=20))
dyad_fig

block_fig <- ggplot(data=block_df,
    aes(x=Variable, y=as.numeric(Median), color = network,
        ymin=as.numeric(lower), ymax=as.numeric(upper))) +
    geom_pointrange(position=position_dodge(width = 0.5)) +
    geom_hline(yintercept= 0, lty=2, aes(fill=network)) +
    coord_flip() +
    scale_color_manual(values = c("#481567FF","#CC99FF","#287D8EFF", "#00cca5","#ACF0B0")) +
    xlab("") + ylab("Estimate (95% CI)") +
    theme_bw() +
   theme(axis.text=element_text(size=20), axis.title=element_text(size=20), legend.text=element_text(size=20))
block_fig

ggsave(indiv_fig, filename = "./3-analyses/no_overlap_indiv_effects_figure.pdf", device =  "pdf", width = 14, height = 8)
ggsave(dyad_fig, filename = "./3-analyses/no_overlap_dyad_effects_figure.pdf", device =  "pdf", width = 14, height = 8)
ggsave(block_fig, filename = "./3-analyses/no_overlap_block_effects_figure.pdf", device =  "pdf", width = 14, height = 8)
