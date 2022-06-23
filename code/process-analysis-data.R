########################################.
#
#   Analysis Data Prep  
#   
########################################.

# Clear working space
rm(list = ls())

# Set the working directory
# You will need to specify the correct file path for your computer

 setwd("/Users/danielredhead/Pemba-wealth")       # working directory for apple

# Load function
normalize <- function(y) {
    x<-y[!is.na(y)]
    x<-(x - min(x)) / (max(x) - min(x))
    y[!is.na(y)]<-x
    return(y)
}

# Load libraries (install them if you don't have them)
library(tidyverse)
library(kinship2)
library(geosphere)
library(reshape2)

# Load the primary sources 
database <- list()
database[[1]] <- read.csv("./1-raw-data/people.csv", stringsAsFactors = F)
database[[2]] <- read.csv("./1-raw-data/people_observations.csv", stringsAsFactors = F)
database[[3]] <- read.csv("./1-raw-data/networks.csv", stringsAsFactors = F)
database[[4]] <- read.csv("./1-raw-data/residents.csv", stringsAsFactors = F)
database[[5]] <- read.csv("./1-raw-data/su_observations.csv", stringsAsFactors = F)
database[[6]] <- read.csv("./1-raw-data/su_distances.csv", stringsAsFactors = F)
database[[7]] <- read.csv("./1-raw-data/partnerships.csv", stringsAsFactors = F)

wealth <- read.table("./2-processed-data/su_wealth.csv", sep = ",", header = TRUE)

######################################################################################################
#
#   Prepare data & specify attributes
#
######################################################################################################

people <- database[[1]]
people_observations <- database[[2]]
networks <- database[[3]]
residents <- database[[4]]
su_observations <- database[[5]]
su_distances <- database[[6]]
partnerships <- database[[7]]

wealth$SharingUnit2 <- tolower(wealth$SharingUnit2)

# Prepare attributes data

colnames(people)[1] <- "personid"
att <- left_join(people_observations, select(people,-site_code,-dob, -dod), by = "personid")
att <- att[!att$su_id_2 == "", ]
######################################################################################################
#
#   Specify kinship & proximity 
#
######################################################################################################

# Prepare kinship/relatedness
att$gender[att$sex == "male"] <- 1
att$gender[att$sex == "female"] <- 2
att$gender[is.na(att$sex) | att$sex == "" | 
      att$sex == " " | att$sex == "NA"] <- 3
att$father[att$father == 999 |att$father == 0|
              att$father == "empty" | att$father == "missing"] <- NA
att$mother[att$mother == 999 |att$mother == 0|
              att$mother == "empty" |att$mother == "missing"] <- NA

kin_dat <- select(att, personid, father, mother, gender)

# Create pedigree data
# Add missing father ids
kin_dat$mother == "missing"
dads <- unique(att$father[!att$father %in% att$personid & !is.na(att$father) & !att$father == 0])
mums <- unique(att$mother[!att$mother %in% att$personid & !is.na(att$mother) & !att$mother == 0])
new_dads <- data.frame(personid = dads, father = NA, mother = NA, gender = rep(1))
new_mums <- data.frame(personid = mums, father = NA, mother = NA, gender = rep(2))
kin_dat <- rbind(kin_dat, new_dads, new_mums)
kin_dat <- kin_dat[!is.na(kin_dat$personid),]
kin_dat$mother[is.na(kin_dat$father)] <- NA
kin_dat$father[is.na(kin_dat$mother)] <- NA

k <- kinship(pedigree(id = kin_dat$personid, 
  dadid = kin_dat$father, momid = kin_dat$mother, 
  sex = as.numeric(kin_dat$gender)))

# Format data to include only individuals in tenpatti
# Make sure ordering of rows/columns matches other data
k2 <- k[row.names(k) %in% att$personid, ]
k2 <- k2[, colnames(k) %in% att$personid ]
k2 <- k2[match(att$personid, rownames(k2)), match(att$personid, colnames(k2))]

# Create an edgelist from the kinship2 output
kedge <- data.frame(su_i = rownames(k2)[col(k2)], su_j = colnames(k2)[row(k2)], 
          val = c(t(k2)), stringsAsFactors = FALSE)
# Remove cases where the person id for i and j are the same
kedge <- kedge[!kedge$su_i == kedge$su_j, ]

# Bring in the su id for both i and j
kedge$su_i <- att$su_id_2[match(kedge$su_i, att$personid)]
kedge$su_j <- att$su_id_2[match(kedge$su_j, att$personid)]
# create a dyad id
kedge$dyad_id <- paste(kedge$su_i, "_", kedge$su_j, sep = "")

# Get average relatedness between households
dyads <- ddply(kedge, .(dyad_id), summarize, avg_r = mean(val))
# Separate out dyad ID again
dyads <- separate(data = dyads, col = dyad_id, into = c("su_i", "su_j"), sep = "_")

# Add su physical distance as a column in the dyads table
colnames(su_distances)[c(2,3)] <- c("su_i", "su_j")
su_distances$su_i <- att$su_id_2[match(su_distances$su_i, att$su_id_1)]
su_distances$su_j <- att$su_id_2[match(su_distances$su_j, att$su_id_1)]

dyads <- left_join(dyads, select(su_distances[su_distances$su_i %in% att$su_id_2 & su_distances$su_j %in% att$su_id_2,], su_i, su_j, distance), by = c('su_i', 'su_j'))  
dyads$distance[dyads$su_i == dyads$su_j] <- 0

# create and SU x SU relatedness matrix
sur_m <- matrix(0, nrow = length(unique(dyads$su_i)), ncol = length(unique(dyads$su_i)), dimnames = list(unique(dyads$su_i), unique(dyads$su_i)))
for(i in 1:nrow(dyads)){
  sur_m[rownames(sur_m)==dyads$su_i[i],colnames(sur_m)==dyads$su_j[i]] <- dyads$avg_r[i]
}
# Multiply by 2 because kinship2 is allelic
sur_m <- sur_m*2 
sur_m <- sur_m[match(unique(att$su_id_2), rownames(sur_m)), match(unique(att$su_id_2), colnames(sur_m))]

sud_m <- matrix(0, nrow = length(unique(dyads$su_i)), ncol = length(unique(dyads$su_i)), dimnames = list(unique(dyads$su_i), unique(dyads$su_i)))

for(i in 1:nrow(dyads)){
  sud_m[rownames(sud_m)==dyads$su_i[i],colnames(sud_m)==dyads$su_j[i]] <- dyads$distance[i]
}

sud_m <- sud_m[match(unique(att$su_id_2), rownames(sud_m)), match(unique(att$su_id_2), colnames(sud_m))]

# Write out the matrices
write.csv(sur_m, "./2-processed-data/su_relatedness.csv")
write.csv(sud_m, "./2-processed-data/su_distance.csv")

# housekeeping
rm(dyads, kedge, k, k2)

# Wealth distance

w <- expand.grid(su_i = wealth$SharingUnit2, su_j = wealth$SharingUnit2)
w$su_i_wealth <- wealth$CashValue_ssu[match(w$su_i, wealth$SharingUnit2)]
w$su_j_wealth <- wealth$CashValue_ssu[match(w$su_j, wealth$SharingUnit2)]
w$dist <- abs(w$su_i_wealth - w$su_j_wealth)


wealth_dist <- acast(w[which(colnames(w) %in% c("su_i","su_j", "dist"))],
       su_i ~ su_j, value.var = "dist")

wealth_dist <- wealth_dist[match(unique(att$su_id_2), rownames(wealth_dist)), match(unique(att$su_id_2), colnames(wealth_dist))]

write.csv(sur_m, "./2-processed-data/su_wealth_dist.csv")

######################################################################################################
#
#   Specify the social support networks
#
######################################################################################################

# Remove externals and subset to the two household sharing questions
net <- networks[networks$personid %in% att$personid & networks$alterid %in% att$personid ,]
net$su_i <- att$su_id_2[match(net$personid, att$personid)]
net$su_j <- att$su_id_2[match(net$alterid, att$personid)]

sharing1 <- select(net[net$tie == 3, ], ego = su_i, alter = su_j)
su_s1 <- sharing1[!duplicated(sharing1), ]
sharing2 <- select(net[net$tie == 4, ], ego = su_i, alter = su_j)
su_s2 <- sharing2[!duplicated(sharing2), ]

share_mat1 <- matrix(10, nrow = length(unique(att$su_id_2)), ncol = length(unique(att$su_id_2)), dimnames = list(unique(att$su_id_2), unique(att$su_id_2)))

for(i in 1:nrow(share_mat1)){
  for(j in 1:ncol(share_mat1)){
      if(nrow(sharing1[sharing1$ego == rownames(share_mat1)[i] & sharing1$alter == colnames(share_mat1)[j],]) > 0){
        share_mat1[i,j] <- 1
      } else {
        share_mat1[i,j] <- 0
      }
  }
}

share_mat2 <- matrix(10, nrow = length(unique(att$su_id_2)), ncol = length(unique(att$su_id_2)), dimnames = list(unique(att$su_id_2), unique(att$su_id_2)))

for(i in 1:nrow(share_mat2)){
  for(j in 1:ncol(share_mat2)){
      if(nrow(sharing2[sharing2$ego == rownames(share_mat2)[i] & sharing2$alter == colnames(share_mat2)[j],]) > 0){
        share_mat2[i,j] <- 1
      } else {
        share_mat2[i,j] <- 0
      }
  }
}

# Sanity checks
all(rownames(share_mat2) == unique(att$su_id_2))
all(colnames(share_mat2) == unique(att$su_id_2))
all(rownames(share_mat2) == rownames(share_mat1))
all(colnames(share_mat2) == colnames(share_mat1))
all(rownames(share_mat2) == rownames(sur_m))
all(colnames(share_mat2) == colnames(sur_m))
all(rownames(share_mat2) == rownames(sud_m))
all(colnames(share_mat2) == colnames(sud_m))
all(rownames(share_mat2) == rownames(wealth_dist))
all(colnames(share_mat2) == colnames(wealth_dist))

write.csv(share_mat1, "./2-processed-data/su_sharing1.csv")
write.csv(share_mat2, "./2-processed-data/su_sharing2.csv")
