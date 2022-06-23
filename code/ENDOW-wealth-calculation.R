#Base Wealth included in PossCost

rm(list = ls())
library('dplyr')
library('readxl')

#FUNCTIONS: for net value (price * qty). make extensive use of grep() to grab price from pc and multiply by qty from su

#the main function. price * qty for a single item 
get_item_value <- function(item_name) { #take dummy from sharing unit and multiply by price
  return(su[,grep(item_name,colnames(su))] * pc[grep(item_name,pc$Item),"Cost"]) 
}

site_name <- "PE"
site_dir <- paste0("/Users/anirudhs/Documents/ENDOW/endow-",site_name)
data_dir <- paste0(site_dir, "/primary-sources/wave-1/data/")
metadata_dir <- paste0(site_dir,"/primary-sources/wave-1/metadata/")

wealth_dir <- paste0(site_dir, "/analyses/wealth/")

su<-read.csv(paste0(data_dir, paste0(site_name,"_SharingUnit.csv")), as.is=TRUE,header=TRUE, check.names = FALSE)
su$SharingUnitID <- su$SharingUnit1

pc <- read.csv(paste0(data_dir, paste0(site_name,"_PossCost.csv")), as.is=TRUE,header=TRUE)
colnames(pc) <- c("Item","Cost") #cost in USD
pc$Cost <- as.numeric(gsub(",", "", pc$Cost))
pc$Item <- trimws(pc$Item)
# pc$Item <- gsub("\\(", "", pc$Item)
# pc$Item <- gsub("\\)", "", pc$Item)



indiv <- read.csv(paste0(data_dir, paste0(site_name,"_Indiv.csv")), as.is=TRUE,header=TRUE)
indiv <- indiv %>% filter(X.source.of.data. == "focals")

su_sizes_unambiguous <- indiv %>% group_by(SharingUnit1) %>% summarise(size_su_unambiguous = n())
colnames(su_sizes_unambiguous) <- c("SharingUnitID","size_su_unambiguous")

su_sizes_share1 <- indiv %>% group_by(SharingUnit1.1) %>% summarise(size_su_share1 = n())
colnames(su_sizes_share1) <- c("SharingUnitID","size_su_share1")

su_sizes_share2 <- indiv %>% group_by(SharingUnit1.2) %>% summarise(size_su_share2 = n())
colnames(su_sizes_share2) <- c("SharingUnitID","size_su_share2")

su<- merge(su, su_sizes_unambiguous, all.x = TRUE)
su<- merge(su, su_sizes_share1, all.x = TRUE)
su<- merge(su, su_sizes_share2, all.x = TRUE)

su$size_su_share1 <- ifelse(is.na(su$size_su_share1),0,su$size_su_share1)
su$size_su_share2 <- ifelse(is.na(su$size_su_share2),0,su$size_su_share2)


su$size_su <- su$size_su_unambiguous + 0.5*su$size_su_share1 + 0.5*su$size_su_share2


  
aggregation_notes <- read_excel(paste0(metadata_dir,"PE_documentation for SharingUnit.xlsx"))
aggregation_notes <- data.frame(aggregation_notes)

colnames(aggregation_notes) <- c("Item", "Sharing_Level", "Unit", "Note")

#SU level, SSU level, and don't forget DEBTS

#su level first

su_level_notes <- aggregation_notes %>% filter(Sharing_Level == "su")
su_level_items <- su_level_notes$Item

unaccounted_su_level <- setdiff(su_level_items,pc$Item) #all food security stuff

su$su_direct_wealth <- 0

for (su_item in su_level_items) {
  if (!(su_item %in% unaccounted_su_level)) {
    #print(su_item)
    su$su_direct_wealth <- su$su_direct_wealth + get_item_value(paste0("^",su_item,"$"))
  }
}

ssu_level_notes <- aggregation_notes %>% filter(Sharing_Level == "ssu")
ssu_level_items <-ssu_level_notes$Item

ssu_level_items <- gsub("\\(", "\\\\(", ssu_level_items)
ssu_level_items <- gsub("\\)", "\\\\)", ssu_level_items)


unaccounted_ssu_level <- setdiff(ssu_level_items, gsub("\\)", "\\\\)",gsub("\\(", "\\\\(", pc$Item)))

su$su_indirect_wealth <- 0

for (ssu_item in ssu_level_items) {
  if (!(ssu_item %in% unaccounted_ssu_level)) {
    #print(ssu_item)
    #print(get_item_value(paste0("^",ssu_item,"$")))
    su$su_indirect_wealth <- su$su_indirect_wealth + get_item_value(paste0("^",ssu_item,"$"))
  }
}

su$su_indirect_wealth <- su$su_indirect_wealth / su$Nwives2018

su$net_credit <- (as.numeric(su$`$yearly_average_money_loaned_out`) + as.numeric(su$`$yearly_average_received_remittances`)- as.numeric(su$`$yearly_average_money_borrowed`)) / su$Nwives2018
#exactly 1 where debt details are missing, just impute 0

su$net_credit[is.na(su$net_credit)] <- 0

su$CashValue <- su$su_direct_wealth + su$su_indirect_wealth + su$net_credit

ssu_info <- su %>% group_by(SharingUnit2) %>%  summarise(size_ssu = sum(size_su), CashValue_ssu = sum(CashValue))

write.csv(ssu_info, paste0(wealth_dir,paste0(site_name,"_SU_wealth.csv")), row.names=FALSE)
