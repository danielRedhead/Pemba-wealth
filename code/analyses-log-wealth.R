########################################.
#
#  Analyses for Main manuscript
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


su <- read.table("./2-processed-data/sharing_unit.csv", sep = ",", header = TRUE)
w_dist <- as.matrix(read.table("./2-processed-data/su_log_wealth_dist.csv", sep = ",", header = TRUE, row.names = 1))
d <- as.matrix(read.table("./2-processed-data/su_distance.csv", sep = ",", header = TRUE, row.names = 1))
r <- as.matrix(read.table("./2-processed-data/su_relatedness.csv", sep = ",", header = TRUE, row.names = 1))
s1 <- as.matrix(read.table("./2-processed-data/su_sharing1.csv", sep = ",", header = TRUE, row.names = 1))
s2 <- as.matrix(read.table("./2-processed-data/su_sharing2.csv", sep = ",", header = TRUE, row.names = 1))
f_k <- as.matrix(read.table("./2-processed-data/su_female_k.csv", sep = ",", header = TRUE, row.names = 1))
f_q <- as.matrix(read.table("./2-processed-data/su_female_q.csv", sep = ",", header = TRUE, row.names = 1))
m_k <- as.matrix(read.table("./2-processed-data/su_male_k.csv", sep = ",", header = TRUE, row.names = 1))
m_q <- as.matrix(read.table("./2-processed-data/su_male_q.csv", sep = ",", header = TRUE, row.names = 1))


all(su$su_id == colnames(s2))

# Impute median distance for the one missing SU lat/long
d[which(is.na(d))] <- median(d, na.rm = TRUE)

# Create the STRAND data object
nets <- list(sharing_out = s1 , sharing_in = s2)

dyad <- list(Relatedness = r, Distance = normalize(d),
              Wealth_Distance = w_dist,
              Female_k = f_k, Female_q = f_q, 
              Male_k = m_k, Male_q = m_q
              )

ind <- data.frame(Wealth = su$log_wealth)

group_ids <- data.frame(Religious = as.factor(su$status), Development = as.factor(su$khazi))

dat <- make_strand_data(self_report = nets,
          block_covariates = group_ids, individual_covariates = ind,
          dyadic_covariates = dyad)

fit <- fit_latent_network_model( data=dat,
      fpr_regression = ~ 1, 
      rtt_regression = ~ 1, 
      theta_regression = ~ 1, 
      focal_regression = ~ Wealth, 
      target_regression = ~ 1, 
      dyad_regression = ~ Wealth_Distance + Relatedness + Distance + Female_k + Female_q + Male_k + Male_q,
      block_regression = ~ Religious + Development,
       mode="mcmc",
       return_latent_network = TRUE, 
       stan_mcmc_parameters = list(chains = 1, parallel_chains = 1, refresh = 1,
    iter_warmup = 1000, iter_sampling = 2000,
     max_treedepth = NULL, adapt_delta = .98))

# Extract results
res <- summarize_strand_results(fit)

# Extract estimated network
est_net <- round(apply(res$samples$latent_network_sample,2:3,median ))
rownames(est_net) <- rownames(s1)
colnames(est_net) <- colnames(s1)
est_net <- round(est_net)

# Female Q analysis 
nets <- list(female_working = f_q)

dyad <- list(Relatedness = r, Distance = normalize(d),
              Wealth_Distance = normalize(w_dist),
              Sharing = est_net, 
              Female_q = f_q, 
              Female_k = f_k, 
              Male_k = m_k, Male_q = m_q
              )

dat <- make_strand_data(self_report = nets,
          block_covariates = group_ids, individual_covariates = ind,
          dyadic_covariates = dyad)

f_q_fit <- fit_block_plus_social_relations_model( data = dat,
                  focal_regression = ~ Wealth, 
                  target_regression = ~ 1, 
                  dyad_regression = ~ Wealth_Distance + Relatedness + Distance + Female_k + Sharing + Male_k + Male_q,
                  block_regression = ~ Religious + Development,
                  mode="mcmc",
                  return_latent_network = TRUE, 
                  stan_mcmc_parameters = list(chains = 1, parallel_chains = 1, refresh = 1,
                        iter_warmup = 1000, iter_sampling = 2000,
                        max_treedepth = NULL, adapt_delta = .98)
                  )

f_q_res <- summarize_strand_results(f_q_fit)

# Female k analysis 
nets <- list(female_friends = f_k)

dyad <- list(Relatedness = r, Distance = normalize(d),
              Wealth_Distance = normalize(w_dist),
              Sharing = est_net, 
              Female_q = f_q,
              Male_k = m_k, Male_q = m_q
              )

dat <- make_strand_data(self_report = nets,
          block_covariates = group_ids, individual_covariates = ind,
          dyadic_covariates = dyad)

f_k_fit <- fit_block_plus_social_relations_model( data = dat,
                  focal_regression = ~ Wealth, 
                  target_regression = ~ 1, 
                  dyad_regression = ~ Wealth_Distance + Relatedness + Distance + Female_q + Sharing + Male_k + Male_q,
                  block_regression = ~ Religious + Development,
                  mode="mcmc",
                  return_latent_network = TRUE, 
                  stan_mcmc_parameters = list(chains = 1, parallel_chains = 1, refresh = 1,
                        iter_warmup = 1000, iter_sampling = 2000,
                        max_treedepth = NULL, adapt_delta = .98)
                  )
f_k_res <- summarize_strand_results(f_k_fit)

# male q analysis 
nets <- list(Male_work = m_q)

dyad <- list(Relatedness = r, Distance = normalize(d),
              Wealth_Distance = normalize(w_dist),
              Sharing = est_net, 
              Female_q = f_q,
              Female_k = f_k,
              Male_k = m_k
              )

dat <- make_strand_data(self_report = nets,
          block_covariates = group_ids, individual_covariates = ind,
          dyadic_covariates = dyad)

m_q_fit <- fit_block_plus_social_relations_model( data = dat,
                  focal_regression = ~ Wealth, 
                  target_regression = ~ 1, 
                  dyad_regression = ~ Wealth_Distance + Relatedness + Distance + Female_q + Female_k + Sharing + Male_k ,
                  block_regression = ~ Religious + Development,
                  mode="mcmc",
                  return_latent_network = TRUE, 
                  stan_mcmc_parameters = list(chains = 1, parallel_chains = 1, refresh = 1,
                        iter_warmup = 1000, iter_sampling = 2000,
                        max_treedepth = NULL, adapt_delta = .98)
                  )
m_q_res <- summarize_strand_results(m_q_fit)

# male k analysis

nets <- list(Male_friends = m_k)

dyad <- list(Relatedness = r, Distance = normalize(d),
              Wealth_Distance = normalize(w_dist),
              Sharing = est_net, 
              Female_q = f_q,
              Female_k = f_k,
              Male_q = m_q
              )

dat <- make_strand_data(self_report = nets,
          block_covariates = group_ids, individual_covariates = ind,
          dyadic_covariates = dyad)

m_k_fit <- fit_block_plus_social_relations_model( data = dat,
                  focal_regression = ~ Wealth, 
                  target_regression = ~ 1, 
                  dyad_regression = ~ Wealth_Distance + Relatedness + Distance + Female_q + Female_k + Sharing + Male_q ,
                  block_regression = ~ Religious + Development,
                  mode="mcmc",
                  return_latent_network = TRUE, 
                  stan_mcmc_parameters = list(chains = 1, parallel_chains = 1, refresh = 1,
                        iter_warmup = 1000, iter_sampling = 2000,
                        max_treedepth = NULL, adapt_delta = .98)
                  )

m_k_res <- summarize_strand_results(m_k_fit)

save.image(file = "./3-analyses/results_log_wealth.RData")
write.csv(est_net, "./3-analyses/estimated_network_log_wealth.csv")
