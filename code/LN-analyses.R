########################################.
#
#   LNM Analyses  
#
########################################.

# Clear working space
rm(list = ls())

# Set the working directory
# You will need to specify the correct file path for your computer

#setwd("/Users/danielredhead/Pemba-wealth")       # working directory for apple
setwd("./Pemba-wealth")       # working directory for server

# Load function
normalize <- function(y) {
    x<-y[!is.na(y)]
    x<-(x - min(x)) / (max(x) - min(x))
    y[!is.na(y)]<-x
    return(y)
}

# Load libraries
library(STRAND)
library(ggplot2)


w <- read.table("./2-processed-data/su_wealth.csv", sep = ",", header = TRUE)
w_dist <- as.matrix(read.table("./2-processed-data/su_wealth_dist.csv", sep = ",", header = TRUE, row.names = 1))
d <- as.matrix(read.table("./2-processed-data/su_distance.csv", sep = ",", header = TRUE, row.names = 1))
r <- as.matrix(read.table("./2-processed-data/su_relatedness.csv", sep = ",", header = TRUE, row.names = 1))
s1 <- as.matrix(read.table("./2-processed-data/su_sharing1.csv", sep = ",", header = TRUE, row.names = 1))
s2 <- as.matrix(read.table("./2-processed-data/su_sharing2.csv", sep = ",", header = TRUE, row.names = 1))

w$SharingUnit2 <- tolower(w$SharingUnit2) 

all(w$SharingUnit2 == colnames(s2))

# Impute median distance for the one missing SU lat/long
d[which(is.na(d))] <- median(d, na.rm = TRUE)

# Create the STRAND data object
nets <- list(sharing_out = s1 , sharing_in = s2)

dyad <- list(Relatedness = r, Distance = normalize(d),
              Wealth_Distance = normalize(w_dist))

ind <- data.frame(Wealth = log(w$CashValue_ssu))

group_ids <- data.frame(no_block = as.factor(rep(1, nrow(s1))))

dat <- make_strand_data(self_report = nets,
          block_covariates = group_ids, individual_covariates = ind,
          dyadic_covariates = dyad)

fit <- fit_latent_network_model( data=dat,
      fpr_regression = ~ 1, 
      rtt_regression = ~ 1, 
      theta_regression = ~ 1, 
      focal_regression = ~ Wealth, 
      target_regression = ~ Wealth, 
      dyad_regression = ~ Relatedness + Distance,
      block_regression = ~ no_block,
       mode="mcmc",
       return_latent_network = TRUE, 
       stan_mcmc_parameters = list(chains = 1, parallel_chains = 1, refresh = 1,
    iter_warmup = 1000, iter_sampling = 2000,
     max_treedepth = NULL, adapt_delta = .98))

# Extract results
res <- summarize_strand_results(fit)


# Generate predictions about the association between relational and material wealth

wel <- res$samples$srm_model_samples$focal_coeffs[,1]

Q <- 2000
K <- 100

pred1 <- matrix(NA, Q, K)

wealth <- seq(1.5, 10,length.out = K)
## Simulate

for(q in 1:Q) {
    for(k in 1:100) {
        pred1[q, k] <-  wel[q]*wealth[k]
    }
}


med <- apply(pred1, 2, median)
HL <- apply(pred1, 2, HPDI)

df <- data.frame(
    wealth = wealth,
    median = med,
    lower = HL[1,],
    upper= HL[2,],
    type = "food sharing"
)

rel_plot <- ggplot(data=df, aes(x=wealth, y=median, ymin=lower, ymax=upper, fill=type, linetype=type)) + 
 geom_line() + 
 geom_ribbon(alpha=0.5) + 
 scale_fill_manual(values=c("darkseagreen3", "deepskyblue4", "darkorchid4"), name="fill") +
 xlab("(Log) Wealth") + 
 ylab("Food Sharing Outdegree") +
 theme_bw() +
 guides(linetype = "none")

ggsave(rel_plot, filename = "./3-analyses/wealth_fig.pdf", device =  "pdf", width = 10, height = 10)


# Extract estimated network
est_net <- round(apply(res$samples$latent_network_sample,2:3,median ))
rownames(est_net) <- rownames(s1)
colnames(est_net) <- colnames(s1)

write.csv(est_net, "./3-analyses/estimated_network.csv")