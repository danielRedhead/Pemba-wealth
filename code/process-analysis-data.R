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

get_item_value <- function(item_name) { #take dummy from sharing unit and multiply by price
  return(su[,grep(item_name,colnames(su))] * pc[grep(item_name,pc$Item),"Cost"]) 
}

# Load libraries (install them if you don't have them)
library(tidyverse)
library(kinship2)
library(geosphere)
library(reshape2)
library(plyr)
library('readxl')

options(scipen=999)

# Load the primary sources 
database <- list()
database[[1]] <- read.csv("./1-raw-data/people.csv", stringsAsFactors = F)
database[[2]] <- read.csv("./1-raw-data/people_observations.csv", stringsAsFactors = F)
database[[3]] <- read.csv("./1-raw-data/networks.csv", stringsAsFactors = F)
database[[4]] <- read.csv("./1-raw-data/residents.csv", stringsAsFactors = F)
database[[5]] <- read.csv("./1-raw-data/su_observations.csv", stringsAsFactors = F)
database[[6]] <- read.csv("./1-raw-data/su_distances.csv", stringsAsFactors = F)
database[[7]] <- read.csv("./1-raw-data/partnerships.csv", stringsAsFactors = F)
database[[8]] <- read.csv("./1-raw-data/possession_costs.csv", stringsAsFactors = F)

su_notes <- read_excel("./1-raw-data/PE_documentation for SharingUnit.xlsx")

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
poss_cost <- database[[8]]

# Prepare attributes data
#add polygenous males su IDs 
people_observations$su_id_1[people_observations$personid == "pe0101"] <- "pesu101" # Also in pesu108
people_observations$su_id_1[people_observations$personid == "pe2401"] <- "pesu123" # Also in pesu124
people_observations$su_id_1[people_observations$personid == "pe3101"] <- "pesu131" # Also in pesu132
people_observations$su_id_1[people_observations$personid == "pe4501"] <- "pesu145" # Also in pesu157
people_observations$su_id_1[people_observations$personid == "pe5201"] <- "pesu152" # Also in pesu154
people_observations$su_id_1[people_observations$personid == "pe6101"] <- "pesu104" # Also in pesu161
 
#colnames(people)[1] <- "personid"
att <- left_join(people_observations, select(people,-site_code,-dob, -dod), by = "personid")
#att <- att[!att$su_id_1 == "", ]
att$su_id_1[att$su_id_1 == ""] <- NA
######################################################################################################
#
#   Specify kinship & proximity 
#
######################################################################################################

# Prepare kinship/relatedness
att$gender[att$gender == "male"] <- 1
att$gender[att$gender == "female"] <- 2
att$gender[is.na(att$gender) | att$gender == "" | 
      att$gender == " " | att$gender == "NA"] <- 3
att$father[att$father == 999 |att$father == 0|
              att$father == "empty" | att$father == "missing"] <- NA
att$mother[att$mother == 999 |att$mother == 0|
              att$mother == "empty" |att$mother == "missing"] <- NA


att2  <- att[!is.na(att$su_id_1),]


kin_dat <- select(att, personid, father, mother, gender)

# Create pedigree data
# Add missing father ids
#dads <- unique(att$father[!att$father %in% att$personid & !is.na(att$father) & !att$father == 0])
#mums <- unique(att$mother[!att$mother %in% att$personid & !is.na(att$mother) & !att$mother == 0])
#new_dads <- data.frame(personid = dads, father = NA, mother = NA, gender = rep(1))
#new_mums <- data.frame(personid = mums, father = NA, mother = NA, gender = rep(2))
#kin_dat <- rbind(kin_dat, new_dads, new_mums)
#kin_dat <- kin_dat[!is.na(kin_dat$personid),]
#kin_dat$mother[is.na(kin_dat$father)] <- NA
#kin_dat$father[is.na(kin_dat$mother)] <- NA
kin_dat$father <- ifelse(is.na(kin_dat$father) & !is.na(kin_dat$mother), 
                         paste0(kin_dat$personid, "_dad"), 
                         kin_dat$father)

kin_dat$mother <- ifelse(is.na(kin_dat$mother) & !is.na(kin_dat$father), 
                         paste0(kin_dat$personid, "_mum"), 
                         kin_dat$mother)
dads <- unique(kin_dat$father[!kin_dat$father %in% kin_dat$personid & !is.na(kin_dat$father) & !kin_dat$father == 0])
mums <- unique(kin_dat$mother[!kin_dat$mother %in% kin_dat$personid & !is.na(kin_dat$mother) & !kin_dat$mother == 0])
if(length(dads) > 0) new_dads <- data.frame(personid = dads, 
                                            father = NA, 
                                            mother = NA, 
                                            gender = rep(1))
if(length(dads) > 0) kin_dat <- rbind(kin_dat, new_dads)
if(length(mums) > 0) new_mums <- data.frame(personid = mums, 
                                            father = NA, 
                                            mother = NA, 
                                            gender = rep(2))
if(length(mums) > 0) kin_dat <- rbind(kin_dat, new_mums)

k <- kinship(pedigree(id = kin_dat$personid, 
  dadid = kin_dat$father, momid = kin_dat$mother, 
  sex = as.numeric(kin_dat$gender)))

# Format data to include only individuals in tenpatti
# Make sure ordering of rows/columns matches other data
k2 <- k[row.names(k) %in% att$personid, ]
k2 <- k2[, colnames(k) %in% att$personid ]
k2 <- k2[match(att$personid, rownames(k2)), match(att$personid, colnames(k2))]

# Create an edgelist from the kinship2 output
kedge <- data.frame(su_i = rownames(k2)[col(k2)], 
                    su_j = colnames(k2)[row(k2)], 
                    val = c(t(k2)), 
                    stringsAsFactors = FALSE)

# Add polygynous men to all of their associated SU's
poly_men <- c("pe0101",
				"pe2401",
				"pe3101",
				"pe4501",
				"pe5201",
				"pe6101")

add_kin <- kedge[kedge$su_i %in% poly_men | kedge$su_j %in% poly_men ,]

#add polygenous males second su IDs 
add_kin$su_i[add_kin$su_i == "pe0101"] <- "pesu108"
add_kin$su_i[add_kin$su_i == "pe2401"] <- "pesu124"
add_kin$su_i[add_kin$su_i == "pe3101"] <- "pesu132"
add_kin$su_i[add_kin$su_i == "pe4501"] <- "pesu157"
add_kin$su_i[add_kin$su_i == "pe5201"] <- "pesu154"
add_kin$su_i[add_kin$su_i == "pe6101"] <- "pesu161"
add_kin$su_j[add_kin$su_j == "pe0101"] <- "pesu108"
add_kin$su_j[add_kin$su_j == "pe2401"] <- "pesu124"
add_kin$su_j[add_kin$su_j == "pe3101"] <- "pesu132"
add_kin$su_j[add_kin$su_j == "pe4501"] <- "pesu157"
add_kin$su_j[add_kin$su_j == "pe5201"] <- "pesu154"
add_kin$su_j[add_kin$su_j == "pe6101"] <- "pesu161"


# Remove cases where the person id for i and j are the same
kedge <- kedge[!kedge$su_i == kedge$su_j, ]
kedge <- kedge[!kedge$su_i == "", ]
kedge <- kedge[!kedge$su_j == "", ]
kedge <- kedge[!is.na(kedge$su_i), ]
kedge <- kedge[!is.na(kedge$su_j), ]

# Limit to only residents
kedge <- kedge[kedge$su_i %in% att$personid[att$is_resident == 1],]
kedge <- kedge[kedge$su_j %in% att$personid[att$is_resident == 1],]

# Bring in the su id for both i and j
kedge$su_i <- att$su_id_1[match(kedge$su_i, att$personid)]
kedge$su_j <- att$su_id_1[match(kedge$su_j, att$personid)]

# Append to full networks table
kedge <- add_row(kedge, add_kin)
 
kedge <- kedge[!is.na(kedge$su_i), ]
kedge <- kedge[!is.na(kedge$su_j), ]

# create a dyad id
kedge$dyad_id <- paste(kedge$su_i, "_", kedge$su_j, sep = "")

# Get average relatedness between households
dyads <- ddply(kedge, .(dyad_id), summarize, avg_r = mean(val))
# Separate out dyad ID again
dyads <- separate(data = dyads, col = dyad_id, into = c("su_i", "su_j"), sep = "_")

# Add su physical distance as a column in the dyads table
colnames(su_distances)[c(2,3)] <- c("su_i", "su_j")
su_distances$su_i <- att$su_id_1[match(su_distances$su_i, att$su_id_1)]
su_distances$su_j <- att$su_id_1[match(su_distances$su_j, att$su_id_1)]

dyads <- left_join(dyads, select(su_distances[su_distances$su_i %in% att$su_id_1 & su_distances$su_j %in% att$su_id_1,], su_i, su_j, distance), by = c('su_i', 'su_j'))  
dyads$distance[dyads$su_i == dyads$su_j] <- 0

# create and SU x SU relatedness matrix
sur_m <- matrix(0, nrow = length(unique(dyads$su_i)), ncol = length(unique(dyads$su_i)), dimnames = list(unique(dyads$su_i), unique(dyads$su_i)))
for(i in 1:nrow(dyads)){
  sur_m[rownames(sur_m)==dyads$su_i[i],colnames(sur_m)==dyads$su_j[i]] <- dyads$avg_r[i]
}

# Multiply by 2 because kinship2 is allelic
sur_m <- sur_m*2 
sur_m <- sur_m[match(unique(att2$su_id_1), rownames(sur_m)), match(unique(att2$su_id_1), colnames(sur_m))]

sud_m <- matrix(0, nrow = length(unique(dyads$su_i)), ncol = length(unique(dyads$su_i)), dimnames = list(unique(dyads$su_i), unique(dyads$su_i)))

for(i in 1:nrow(dyads)){
  sud_m[rownames(sud_m)==dyads$su_i[i],colnames(sud_m)==dyads$su_j[i]] <- dyads$distance[i]
}

sud_m <- sud_m[match(unique(att2$su_id_1), rownames(sud_m)), match(unique(att2$su_id_1), colnames(sud_m))]

# Write out the matrices
write.csv(sur_m, "./2-processed-data/su_relatedness.csv")
write.csv(sud_m, "./2-processed-data/su_distance.csv")

# housekeeping
rm(dyads, kedge, k, k2)

######################################################################################################
#
#   Specify the social support networks
#
######################################################################################################

# Remove externals and subset to the two household sharing questions
net <- networks[networks$personid %in% att2$personid & networks$alterid %in% att2$personid ,]
net$su_i <- att$su_id_1[match(net$personid, att$personid)]
net$su_j <- att$su_id_1[match(net$alterid, att$personid)]

# Add polygynous men to all of their associated SU's

add_net <- net[net$personid %in% poly_men | net$alterid %in% poly_men ,]

#add polygenous males second su IDs 
add_net$su_i[add_net$personid == "pe0101"] <- "pesu108"
add_net$su_i[add_net$personid == "pe2401"] <- "pesu124"
add_net$su_i[add_net$personid == "pe3101"] <- "pesu132"
add_net$su_i[add_net$personid == "pe4501"] <- "pesu157"
add_net$su_i[add_net$personid == "pe5201"] <- "pesu154"
add_net$su_i[add_net$personid == "pe6101"] <- "pesu161"
add_net$su_j[add_net$alterid == "pe0101"] <- "pesu108"
add_net$su_j[add_net$alterid == "pe2401"] <- "pesu124"
add_net$su_j[add_net$alterid == "pe3101"] <- "pesu132"
add_net$su_j[add_net$alterid == "pe4501"] <- "pesu157"
add_net$su_j[add_net$alterid == "pe5201"] <- "pesu154"
add_net$su_j[add_net$alterid == "pe6101"] <- "pesu161"

# Append to full networks table
net <- add_row(net, add_net)
 
sharing1 <- select(net[net$tie == 3, ], ego = su_i, alter = su_j)
su_s1 <- sharing1[!duplicated(sharing1), ]
sharing2 <- select(net[net$tie == 4, ], ego = su_i, alter = su_j)
su_s2 <- sharing2[!duplicated(sharing2), ]

f_q <- select(net[net$tie == 5, ], ego = su_i, alter = su_j)
fem_q <- f_q[!duplicated(f_q), ]
m_q <- select(net[net$tie == 6, ], ego = su_i, alter = su_j)
men_q <- m_q[!duplicated(m_q), ]

f_k <- select(net[net$tie == 7, ], ego = su_i, alter = su_j)
fem_k <- f_k[!duplicated(f_k), ]
m_k <- select(net[net$tie == 8, ], ego = su_i, alter = su_j)
men_k <- m_k[!duplicated(m_k), ]

share_mat1 <- matrix(NA, nrow = length(unique(att2$su_id_1)), ncol = length(unique(att2$su_id_1)), dimnames = list(unique(att2$su_id_1), unique(att2$su_id_1)))

for(i in 1:nrow(share_mat1)){
  for(j in 1:ncol(share_mat1)){
      if(nrow(sharing1[sharing1$ego == rownames(share_mat1)[i] & sharing1$alter == colnames(share_mat1)[j],]) > 0){
        share_mat1[i,j] <- 1
      } else {
        share_mat1[i,j] <- 0
      }
  }
}

share_mat2 <- matrix(NA, nrow = length(unique(att2$su_id_1)), ncol = length(unique(att2$su_id_1)), dimnames = list(unique(att2$su_id_1), unique(att2$su_id_1)))

for(i in 1:nrow(share_mat2)){
  for(j in 1:ncol(share_mat2)){
      if(nrow(sharing2[sharing2$ego == rownames(share_mat2)[i] & sharing2$alter == colnames(share_mat2)[j],]) > 0){
        share_mat2[i,j] <- 1
      } else {
        share_mat2[i,j] <- 0
      }
  }
}

female_q <- matrix(NA, nrow = length(unique(att2$su_id_1)), ncol = length(unique(att2$su_id_1)), dimnames = list(unique(att2$su_id_1), unique(att2$su_id_1)))

for(i in 1:nrow(female_q)){
  for(j in 1:ncol(female_q)){
      if(nrow(fem_q[fem_q$ego == rownames(female_q)[i] & fem_q$alter == colnames(female_q)[j],]) > 0){
        female_q[i,j] <- 1
      } else {
        female_q[i,j] <- 0
      }
  }
}

male_q <- matrix(NA, nrow = length(unique(att2$su_id_1)), ncol = length(unique(att2$su_id_1)), dimnames = list(unique(att2$su_id_1), unique(att2$su_id_1)))

for(i in 1:nrow(male_q)){
  for(j in 1:ncol(male_q)){
      if(nrow(men_q[men_q$ego == rownames(male_q)[i] & men_q$alter == colnames(male_q)[j],]) > 0){
        male_q[i,j] <- 1
      } else {
        male_q[i,j] <- 0
      }
  }
}

female_k <- matrix(NA, nrow = length(unique(att2$su_id_1)), ncol = length(unique(att2$su_id_1)), dimnames = list(unique(att2$su_id_1), unique(att2$su_id_1)))

for(i in 1:nrow(female_k)){
  for(j in 1:ncol(female_k)){
      if(nrow(fem_q[fem_k$ego == rownames(female_k)[i] & fem_k$alter == colnames(female_k)[j],]) > 0){
        female_k[i,j] <- 1
      } else {
        female_k[i,j] <- 0
      }
  }
}

male_k <- matrix(NA, nrow = length(unique(att2$su_id_1)), ncol = length(unique(att2$su_id_1)), dimnames = list(unique(att2$su_id_1), unique(att2$su_id_1)))

for(i in 1:nrow(male_k)){
  for(j in 1:ncol(male_k)){
      if(nrow(men_k[men_k$ego == rownames(male_k)[i] & men_k$alter == colnames(male_k)[j],]) > 0){
        male_k[i,j] <- 1
      } else {
        male_k[i,j] <- 0
      }
  }
}


# Sanity checks
all(rownames(share_mat2) == unique(att2$su_id_1))
all(colnames(share_mat2) == unique(att2$su_id_1))
all(rownames(share_mat2) == rownames(share_mat1))
all(colnames(share_mat2) == colnames(share_mat1))
all(rownames(share_mat2) == rownames(sur_m))
all(colnames(share_mat2) == colnames(sur_m))
all(rownames(share_mat2) == rownames(sud_m))
all(colnames(share_mat2) == colnames(sud_m))

#################################
# WEALTH CALCULATIONS
#################################

pc <- poss_cost
colnames(pc) <- c("Item","Cost", "cost_USD", "meh") #cost 
pc$Cost <- as.numeric(gsub(",", "", pc$Cost))
pc$Item <- trimws(pc$Item)

su <- su_observations

not_wealth <- c("waveid",
				"smaller_meals",                          
				"sleep_hungry",
				"no_food",  
				"fewer_meals",                            
				"without_eating",                       
				"malehead",                             
				"femalehead",                           
				"unit",                                 
				"sharingunit2",
				"x.yearly_average_money_loaned_out",   
				"x.yearly_average_money_borrowed", 
				"x.yearly_average_received_remittances"
				) 

su <- su[!colnames(su) %in% not_wealth]

wealth_items <- colnames(su)[!colnames(su) %in% c("su_id", "nwives2018")]
wealth_items[!wealth_items %in% pc$Item] 

pc$Item[pc$Item == "base wealth"] <- "base.wealth" 
pc$Item[pc$Item == "house type 1"] <- "house.type.1"                         
pc$Item[pc$Item == "house type 2"] <-"house.type.2"                         
pc$Item[pc$Item == "house type 3"] <-"house.type.3"                         
pc$Item[pc$Item == "house type 4"] <-"house.type.4"                         
pc$Item[pc$Item == "house type 5"] <-"house.type.5"                         
pc$Item[pc$Item == "house type 6"] <-"house.type.6"                         
pc$Item[pc$Item == "house type 7"] <-"house.type.7"                         
pc$Item[pc$Item == "house type 8"] <-"house.type.8"                         
pc$Item[pc$Item == "house type 9"] <-"house.type.9"                         
pc$Item[pc$Item == "house type 10"] <-"house.type.10"                        
pc$Item[pc$Item == "house type 11"] <-"house.type.11"                        
pc$Item[pc$Item == "house type 12"] <-"house.type.12"                        
pc$Item[pc$Item == "house type 13"] <-"house.type.13"                        
pc$Item[pc$Item == "house type 14"] <-"house.type.14"                        
pc$Item[pc$Item == "house type 15"] <-"house.type.15"                        
pc$Item[pc$Item == "house type 16"] <-"house.type.16"                        
pc$Item[pc$Item == "gas cooker"] <-"gas.cooker"                           
pc$Item[pc$Item == "satelite dish"] <-"satelite.dish"                        
pc$Item[pc$Item == "2_inch_net_vol"] <-"x2_inch_net_vol"                      
pc$Item[pc$Item == "mask & snorkel pipe_vol"] <-"mask...snorkel.pipe_vol"              
pc$Item[pc$Item == "4_inch_net_vol"] <-"x4_inch_net_vol"                      
pc$Item[pc$Item == "1_inch_net_vol"] <-"x1_inch_net_vol"                      
pc$Item[pc$Item == "1.5_inch_net_vol"] <-"x1.5_inch_net_vol"                    
pc$Item[pc$Item == "iron bar (mtarimbo)_vol"] <-"iron.bar..mtarimbo._vol"              
 
all(wealth_items %in% pc$Item)       


# supra-unit items so should be divided by Nwives2018
supra <- list(	"car",    
				"cloves_amount",	
				"palms_amount",	
				"planted_trees_amount",	
				"oranges_amount_vol",	
				"cinnamon_amount_vol",	
				"cyprus_amount_vol",	
				"acacia_amount_vol",	
				"jackfruit_amount_vol",	
				"mango_amount_vol",	
				"hook_line",	 
				"drag_net", 	
				"seine_net",	 
				"dugout", 	
				"cloth_sail",	 
				"two_person_saw", 	
				"chainsaw",	
				"shop",	
				"x2_inch_net_vol",	
				"mask...snorkel.pipe_vol",
				"x4_inch_net_vol",
				"x1_inch_net_vol",
				"metal_saw_vol",
				"level_vol",
				"tape_vol",
				"hand_saw_vol",
				"plank_vol",
				"flippers_vol",
				"x1.5_inch_net_vol",
				"plane_vol",
				"iron.bar..mtarimbo._vol"
				)

su[colnames(su) %in% supra] <- su[colnames(su) %in% supra]/su$nwives2018

su$su_direct_wealth <- 0

for (su_item in wealth_items){
    #print(su_item)
    su$su_direct_wealth <- su$su_direct_wealth + get_item_value(paste0("^",su_item,"$"))
		}

sharing_unit <- select(su, su_id, nwives2018, su_direct_wealth)

sharing_unit$log_wealth <- log(sharing_unit$su_direct_wealth)

w <- expand.grid(su_i = sharing_unit$su_id, su_j = sharing_unit$su_id)
w$su_i_wealth <- sharing_unit$su_direct_wealth[match(w$su_i, sharing_unit$su_id)]
w$su_j_wealth <- sharing_unit$su_direct_wealth[match(w$su_j, sharing_unit$su_id)]
w$dist <- abs(w$su_i_wealth - w$su_j_wealth)

wealth_dist <- acast(w[which(colnames(w) %in% c("su_i","su_j", "dist"))],
       su_i ~ su_j, value.var = "dist")

all(rownames(share_mat2) == rownames(wealth_dist))
all(colnames(share_mat2) == colnames(wealth_dist))

w <- expand.grid(su_i = sharing_unit$su_id, su_j = sharing_unit$su_id)
w$su_i_wealth <- sharing_unit$log_wealth[match(w$su_i, sharing_unit$su_id)]
w$su_j_wealth <- sharing_unit$log_wealth[match(w$su_j, sharing_unit$su_id)]
w$dist <- abs(w$su_i_wealth - w$su_j_wealth)

log_wealth_dist <- acast(w[which(colnames(w) %in% c("su_i","su_j", "dist"))],
       su_i ~ su_j, value.var = "dist")

all(rownames(share_mat2) == rownames(log_wealth_dist))
all(colnames(share_mat2) == colnames(log_wealth_dist))

#bring in sharing unit status, 
sharing_unit$status <- as.vector(ifelse(aggregate(status ~ su_id_1, att2, sum)[2]
										 >0,1,0))

att2$group_id[att2$group_id == "member"] <- 1
att2$group_id[att2$group_id == "spouse of member"] <- 1
att2$group_id[att2$group_id == "no"] <- 0
att2$group_id <- as.numeric(att2$group_id)

sharing_unit$khazi <- as.vector(ifelse(aggregate(group_id ~ su_id_1, att2, sum)[2]
                     >0,1,0))

write.csv(log_wealth_dist, "./2-processed-data/su_log_wealth_dist.csv")
write.csv(wealth_dist, "./2-processed-data/su_wealth_dist.csv")
write.csv(share_mat1, "./2-processed-data/su_sharing1.csv")
write.csv(share_mat2, "./2-processed-data/su_sharing2.csv")
write.csv(female_k, "./2-processed-data/su_female_k.csv")
write.csv(female_q, "./2-processed-data/su_female_q.csv")
write.csv(male_k, "./2-processed-data/su_male_k.csv")
write.csv(male_q, "./2-processed-data/su_male_q.csv")
write.csv(sharing_unit, "./2-processed-data/sharing_unit.csv", row.names = FALSE)
