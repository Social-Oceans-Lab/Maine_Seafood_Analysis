# Seafood Production 
# 2.d. Trends by Landing Volume (Focal Species)
# Version 1.0. March 1, 2022


#loading packages 
library(dplyr)   
library(tidyr)
library(ggplot2) 
library(broom)
library(purrr)
library(RColorBrewer)
library(hrbrthemes)
library(scales)
library(cowplot)
library(gt)
library(ggpubr)

#reading in data
lands <- read.csv("/Users/jstoll/OneDrive - University of Maine System/Social_Coasts_Lab/seafood_systems/Data/Processed_Data/ACCSP_Landings_Nonconfidential_Commercial_2007_2020_clean.csv", stringsAsFactors = FALSE)

### Tables and heatmap of 5 and 10-year trends

### Calculate Z-scores

lands <- as_tibble(lands)                                              #making the df a tibble


### Nesting landing by state and species to calculate z-score for each species independently ####  calculate z-score for each combo of species and state. One score for each year chunk

lands_z <- lands %>%
  filter(Year >= 2010) %>%
  group_by(State, Common.Name) %>%                               #group by state and spp
  mutate(zscores = (Pounds - mean(Pounds))/sd(Pounds)) %>%    #calculate z-scores 
  arrange(Common.Name, State, Year)                              #arranging nicely

## Splitting out states
lands_z_me <- filter(lands_z, State == "MAINE")
lands_z_ma <- filter(lands_z, State == "MASSACHUSETTS")
lands_z_nh <- filter(lands_z, State == "NEW HAMPSHIRE")
lands_z_ct <- filter(lands_z, State == "CONNECTICUT")
lands_z_ri <- filter(lands_z, State == "RHODE ISLAND")

## Prepping zscores graph
#ordering heatmap y-axis by 2020 species z-scores

lands_20 <- filter(lands_z, Year == "2020")                      #using 2020 for lbs or value for ordering
lands_20$Common.Name <- as.factor(lands_20$Common.Name)          #making the names a factor


#getting list of species not present in 2020 for each state, finding most recent lbs/value, and then adding that info onto the "sorted" df so I can have one df with lbs/value for every species, can sort that, and then order the y-axis. oof.
years_select <- function(yfile, zfile, state){                     #yfile is lands_20, zfile is lands_z, state is "STATE"
  yfile_state <- filter(yfile, State == state)                     #splitting lands_20 by state
  
  zfile_state <- filter(zfile, State == state)                     #all spp caught in that state
  afile <- zfile_state[!duplicated(zfile_state$Common.Name), ]     #unique species present in heatmap (= all spp caught in state, but once per species)
  bfile <- anti_join(afile, yfile_state, by = "Common.Name")       #these are the species in the heatmap but w/o landings value in 2020 - need to get numbers for them!
  
  cfile <- zfile_state %>%                                         #pulling most recent year of landings >0 for all state spp
    filter(Year > 2009) %>%                                        #keeping only the species with landings after 2010. I want to get the spp with 0 landings in all years
    group_by(Common.Name) %>%                                      #grouping by common name
    slice_max(Pounds, n = 1)                                       #pulling top lbs for each species (if 0 and a tie, all years show up)
  
  yfile_state <- bind_rows(cfile, yfile_state)                     #merging together. will have duplicates since 2020 0s show up in both files
  
  yfile_state <- yfile_state %>%
    mutate(indy = ifelse(Year == 2020 & is.na(zscores), 1, 0))       #creating column to indicate where 2020 zscores are 0. In these cases, there are always results for a different year
  
  yfile_state <- yfile_state %>%
    filter(indy < 1)                                               #removing the 2020 0 zscores columns. This way I can keep years where 2020 was 0 but there were documented landings in other years
  
  yfile_state <- yfile_state %>%
    group_by(Common.Name) %>%
    slice_max(Year, n = 1)                                         #in cases of 0s or where 2020 had lower landings than other years, I'm keeping the most recent year
  
  yfile_state <- yfile_state %>%                                   #some duplicates where 2020 = 0 and it was carried over from both files
    distinct()                                                     #this removes the duplicates
  
  return(yfile_state)                                              #now I have 2020 landings if they existed, OR the most recent year where landings were publicly available. If data were confidential in all years, they will be 0 here  
}

#running function
lands_z_me_order <- years_select(lands_20, lands_z, "MAINE")
lands_z_ma_order <- years_select(lands_20, lands_z, "MASSACHUSETTS")
lands_z_ri_order <- years_select(lands_20, lands_z, "RHODE ISLAND")
lands_z_ct_order <- years_select(lands_20, lands_z, "CONNECTICUT")
lands_z_nh_order <- years_select(lands_20, lands_z, "NEW HAMPSHIRE")

#Maine
lands_z_me_order <- arrange(lands_z_me_order, zscores, Common.Name)           #ordering df by zscore for comparison     
lands_z_me_order$Common.Name <- as.factor(lands_z_me_order$Common.Name)       #making names a factor
lands_z_me$Common.Name <- factor(lands_z_me$Common.Name, 
                                 levels = lands_z_me_order$Common.Name)       #ordering original data by the order in the new df. 

#Mass
lands_z_ma_extra <- tibble(Year = c(2020, 2020, 2020),                        #adding in spp with 0 in 2020 and no other landings in other years
                           State = c("MASSACHUSETTS", "MASSACHUSETTS", 
                                     "MASSACHUSETTS"),
                           Common.Name = c("BASS, BANK SEA", "CRAB, SNOW",
                                           "HERRING, ATLANTIC THREAD"),
                           Pounds = c(0, 0, 0),
                           Live.Pounds = c(0, 0, 0),
                           Dollars = c(0, 0, 0),
                           zscores = c(NA, NA, NA),
                           indy = c(0, 0, 0))
lands_z_ma_order <- bind_rows(lands_z_ma_order, lands_z_ma_extra)            #merging the extra spp onto the original
lands_z_ma_order <- arrange(lands_z_ma_order, zscores, Common.Name)          #ordering df by value for comparison     
lands_z_ma_order$Common.Name <- as.factor(lands_z_ma_order$Common.Name)      #making names a factor
lands_z_ma$Common.Name <- factor(lands_z_ma$Common.Name, 
                                 levels = lands_z_ma_order$Common.Name)      #ordering original data by the order in the new df.

#nh
lands_z_nh_extra <- tibble(Year = c(2020, 2020, 2020),                        #adding in spp with 0 in 2020 and no other landings in other years
                           State = c("NEW HAMPSHIRE", "NEW HAMPSHIRE", 
                                     "NEW HAMPSHIRE"),
                           Common.Name = c("MACKEREL, ATLANTIC CHUB", 
                                           "SKATE, SMOOTH",
                                           "WHELK, WAVED"),                  #got these species by comparing lands_z_state_order with lands_z_state
                           Pounds = c(0, 0, 0),
                           Live.Pounds = c(0, 0, 0),
                           Dollars = c(0, 0, 0),
                           zscores = c(NA, NA, NA),
                           indy = c(0, 0, 0))
lands_z_nh_order <- bind_rows(lands_z_nh_order, lands_z_nh_extra)            #merging the extra spp onto the original
lands_z_nh_order <- arrange(lands_z_nh_order, zscores, Common.Name)          #ordering df by zscore for comparison   
lands_z_nh_order$Common.Name <- as.factor(lands_z_nh_order$Common.Name)      #making names a factor
lands_z_nh$Common.Name <- factor(lands_z_nh$Common.Name, 
                                 levels = lands_z_nh_order$Common.Name)      #ordering original data by the order in the new df.

#CT
lands_z_ct_extra <- tibble(Year = c(2020, 2020, 2020, 2020),                 #adding in spp with 0 in 2020 and no other landings in other years
                           State = c("CONNECTICUT", "CONNECTICUT", 
                                     "CONNECTICUT", "CONNECTICUT"),
                           Common.Name = c("CATFISH, WHITE", "OPAH", 
                                           "SHARK, MAKO, SHORTFIN",
                                           "SKATE, CLEARNOSE"),              #got these species by comparing lands_z_state_order with lands_z_state
                           Pounds = c(0, 0, 0, 0),
                           Live.Pounds = c(0, 0, 0, 0),
                           Dollars = c(0, 0, 0, 0),
                           zscores = c(NA, NA, NA, NA),
                           indy = c(0, 0, 0, 0))
lands_z_ct_order <- bind_rows(lands_z_ct_order, lands_z_ct_extra)            #merging the extra spp onto the original
lands_z_ct_order <- arrange(lands_z_ct_order, zscores, Common.Name)          #ordering df by zscore for comparison   
lands_z_ct_order$Common.Name <- as.factor(lands_z_ct_order$Common.Name)      #making names a factor
lands_z_ct$Common.Name <- factor(lands_z_ct$Common.Name, 
                                 levels = lands_z_ct_order$Common.Name)      #ordering original data by the order in the new df.

#RI
lands_z_ri_extra <- tibble(Year = c(2020, 2020, 2020,
                                    2020, 2020),                             #adding in spp with 0 in 2020 and no other landings in other years
                           State = c("RHODE ISLAND", "RHODE ISLAND", 
                                     "RHODE ISLAND", "RHODE ISLAND", 
                                     "RHODE ISLAND"),
                           Common.Name = c("BARRACUDAS, SPHYRAENIDAE (FAMILY)", 
                                           "HALIBUT, GREENLAND", 
                                           "HERRING, BLUEBACK",
                                           "HERRING, ROUND",
                                           "TUNA, BLACKFIN"),              #got these species by comparing lands_z_state_order with lands_z_state
                           Pounds = c(0, 0, 0, 0, 0),
                           Live.Pounds = c(0, 0, 0, 0, 0),
                           Dollars = c(0, 0, 0, 0, 0),
                           zscores = c(NA, NA, NA, NA, NA),
                           indy = c(0, 0, 0, 0, 0))
lands_z_ri_order <- bind_rows(lands_z_ri_order, lands_z_ri_extra)            #merging the extra spp onto the original
lands_z_ri_order <- arrange(lands_z_ri_order, zscores, Common.Name)          #ordering df by zscore for comparison   
lands_z_ri_order$Common.Name <- as.factor(lands_z_ri_order$Common.Name)      #making names a factor
lands_z_ri$Common.Name <- factor(lands_z_ri$Common.Name, 
                                 levels = lands_z_ri_order$Common.Name)      #ordering original data by the order in the new df.


####separating out conf and nonconf spp####
####Maine
lands_z_me_order_nonconf <- lands_z_me_order %>%                              #pulling nonconfidential spp
  filter(!is.na(zscores))
lands_z_me_order_conf <- lands_z_me_order %>%                                 #pulling confidential spp (for later)
  filter(is.na(zscores))

#list of non-confidential spp
lands_z_me_nonconf <- semi_join(lands_z_me, lands_z_me_order_nonconf, 
                                by = "Common.Name")                           #filtering lands_z_me so only the nonconf spp are present. I LOVE semi_join!

####mass
lands_z_ma_order_nonconf <- lands_z_ma_order %>%                              #pulling nonconfidential spp
  filter(!is.na(zscores))
lands_z_ma_order_conf <- lands_z_ma_order %>%                                 #pulling confidential spp (for later)
  filter(is.na(zscores))

#list of non-confidential spp for heatmap 
lands_z_ma_nonconf <- semi_join(lands_z_ma, lands_z_ma_order_nonconf, 
                                by = "Common.Name")                           #filtering lands_z_ma so only the nonconf spp are present. I LOVE semi_join!


#New Hampshire
lands_z_nh_order_nonconf <- lands_z_nh_order %>%                              #pulling nonconfidential spp
  filter(!is.na(zscores))
lands_z_nh_order_conf <- lands_z_nh_order %>%                                 #pulling confidential spp (for later)
  filter(is.na(zscores))

#list of non-confidential spp for heatmap 
lands_z_nh_nonconf <- semi_join(lands_z_nh, lands_z_nh_order_nonconf, 
                                by = "Common.Name")                           #filtering lands_z_nh so only the nonconf spp are present. I LOVE semi_join!

#CT
lands_z_ct_order_nonconf <- lands_z_ct_order %>%                              #pulling nonconfidential spp
  filter(!is.na(zscores))
lands_z_ct_order_conf <- lands_z_ct_order %>%                                 #pulling confidential spp (for later)
  filter(is.na(zscores))

#list of non-confidential spp for heatmap 
lands_z_ct_nonconf <- semi_join(lands_z_ct, lands_z_ct_order_nonconf, 
                                by = "Common.Name")                           #filtering lands_z_ct so only the nonconf spp are present. I LOVE semi_join!

#RI
lands_z_ri_order_nonconf <- lands_z_ri_order %>%                              #pulling nonconfidential spp
  filter(!is.na(zscores))
lands_z_ri_order_conf <- lands_z_ri_order %>%                                 #pulling confidential spp (for later)
  filter(is.na(zscores))

#list of non-confidential spp for heatmap 
lands_z_ri_nonconf <- semi_join(lands_z_ri, lands_z_ri_order_nonconf, 
                                by = "Common.Name")                           #filtering lands_z_ri so only the nonconf spp are present. I LOVE semi_join!



### Maine z-scores (all)
me_zs <- ggplot(lands_z_me_nonconf, aes(factor(Year), Common.Name, fill = zscores)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "#e37a55", high = "#233749",
                      limits = c(-3, 3.1), 
                      breaks = c(-3, -2, -1, 0, 1, 2, 3.1),   
                      labels = c(-3, -2, -1, 0, 1, 2, 3)) +    #CHECK THESE FOR EACH STATE! 
  theme_classic() +
  theme(legend.key.width = unit(10, "mm"),
        panel.border = element_rect(colour = "black", fill=NA),
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black"),
        text = element_text(size = 8),
        legend.title = element_text(face = "bold"),
        axis.text.y = element_text(size = 8),
        axis.text.x = element_text(angle = 90, vjust = 0.5, 
                                   hjust = 1, size = 8)) +
  labs(fill = "z score", x = "Year", y = "Species") +
  coord_fixed(ratio = 1/2)

### Maine z-scores (Key species of focus)

# Atlantic salmon (confidential)
# American eel
# Seaweeds (unspecified)
# Eastern oyster
# Sea scallop
# Soft-shell clam
# Quahog (Ocean, Northern)
# Atlantic cod 
# Atlatic halibut
# Pollock
# Haddock
# Monkfish (Goosefish)  
# Bluefin tuna 
# American lobster
# Jonah crab
# Atlantic herring 
# Menhadens
# Mussel

lands_z_me_nonconf_focus <- lands_z_me_nonconf[lands_z_me_nonconf$Common.Name == 'EEL, AMERICAN'|
                                                 lands_z_me_nonconf$Common.Name == 'SEAWEED'|
                                                 lands_z_me_nonconf$Common.Name == 'OYSTER, EASTERN'|
                                                 lands_z_me_nonconf$Common.Name == 'SCALLOP, SEA'|
                                                 lands_z_me_nonconf$Common.Name == 'CLAM, SOFT'|
                                                 lands_z_me_nonconf$Common.Name == 'CLAM, QUAHOG, NORTHERN'|
                                                 lands_z_me_nonconf$Common.Name == 'CLAM, QUAHOG, OCEAN'|
                                                 lands_z_me_nonconf$Common.Name == 'COD, ATLANTIC'|
                                                 lands_z_me_nonconf$Common.Name == 'HALIBUT, ATLANTIC'|
                                                 lands_z_me_nonconf$Common.Name == 'POLLOCK'|
                                                 lands_z_me_nonconf$Common.Name == 'HADDOCK'|
                                                 lands_z_me_nonconf$Common.Name == 'GOOSEFISH'|
                                                 lands_z_me_nonconf$Common.Name == 'TUNA, BLUEFIN'|
                                                 lands_z_me_nonconf$Common.Name == 'LOBSTER, AMERICAN'|
                                                 lands_z_me_nonconf$Common.Name == 'CRAB, JONAH'|
                                                 lands_z_me_nonconf$Common.Name == 'HERRING, ATLANTIC'|
                                                 lands_z_me_nonconf$Common.Name == 'MENHADENS' |
                                                 lands_z_me_nonconf$Common.Name == 'MUSSEL, SEA'
                                               
                                               , ]


me_zs_focus <- ggplot(lands_z_me_nonconf_focus, aes(factor(Year), Common.Name, fill = zscores)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "#e37a55", high = "#233749",
                      limits = c(-3, 3.1), 
                      breaks = c(-3, -2, -1, 0, 1, 2, 3.1),   
                      labels = c(-3, -2, -1, 0, 1, 2, 3)) +    #CHECK THESE FOR EACH STATE! 
  theme_classic() +
  theme(legend.key.width = unit(10, "mm"),
        panel.border = element_rect(colour = "black", fill=NA),
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black"),
        text = element_text(size = 8),
        legend.title = element_text(face = "bold"),
        axis.text.y = element_text(size = 8),
        axis.text.x = element_text(angle = 90, vjust = 0.5, 
                                   hjust = 1, size = 8)) +
  labs(fill = "z score", x = "Year", y = "Species") +
  coord_fixed(ratio = 1/2)

### Line plot

ggplot(lands_z_me_nonconf_focus, aes(x=Year, y=zscores, color = zscores)) +
  #geom_line(size = 1)+
  geom_hline(yintercept=0, size = 2) +
  #stat_smooth(aes(color=..y..), formula = y ~ s(x, k = 7), method = "gam", se = FALSE) +
  stat_smooth(aes(color=..y..), method = "lm", formula = y ~ x, size = 1) +
  scale_colour_gradient2(low = "#062847", high = "#e03f07", 
                         midpoint=0) +
  geom_point(size = 3) +
  ggtitle("10-Year Trend in Landings by Volume") +
  ylim(-3,3) +
  xlab(" ") + 
  ylab("Annual Variability in Landings Volume (normalized z-score)") +
  labs(color = "Z score") +
  scale_x_continuous(breaks=seq(2010,2020,1)) +
  theme_dark() +
  theme(axis.text.x = element_text(angle = 90, vjust = .5)) +
  facet_wrap(~ Common.Name)

# Save figure 

ggsave("2.c.2.Trends_Landing.pdf", width = 15, height = 9)

# Save data

write.csv(score, "me_lic_2021_strength.csv")


# Species specific plots

# American lobster 
lobster <- lands_z_me_nonconf_focus[which(lands_z_me_nonconf_focus$Common.Name == 'LOBSTER, AMERICAN'),]

p1 <- ggplot(lobster, aes(x=Year, y=zscores, color = zscores)) +
  #geom_line(size = 1)+
  geom_hline(yintercept=0, size = 2) +
  stat_smooth(aes(color=..y..), method = "lm", formula = y ~ x, size = 2) +
  scale_colour_gradient2(low = "#062847", high = "#e03f07", 
                         midpoint=0) +
  geom_point(size = 3) +
  ggtitle("10-Year Trend in Landings by Volume (American Lobster)") +
  ylim(-3,3) +
  xlab(" ") + 
  ylab("Annual Variability in Landings Volume (normalized z-score)") +
  labs(color = "Z score") +
  scale_x_continuous(breaks=seq(2010,2020,1)) +
  theme_dark() +
  theme(axis.text.x = element_text(angle = 90, vjust = .5)) 

ggsave("2.c.2.a.Trends_Landing.pdf", width = 15, height = 9)

# Clam, Quahog, Ocean  
quahog <- lands_z_me_nonconf_focus[which(lands_z_me_nonconf_focus$Common.Name == 'CLAM, QUAHOG, OCEAN'),]

p2 <- ggplot(quahog, aes(x=Year, y=zscores, color = zscores)) +
  #geom_line(size = 1)+
  geom_hline(yintercept=0, size = 2) +
  stat_smooth(aes(color=..y..), method = "lm", formula = y ~ x, size = 2) +
  scale_colour_gradient2(low = "#062847", high = "#e03f07", 
                         midpoint=0) +
  geom_point(size = 3) +
  ggtitle("10-Year Trend in Landings by Volume (Ocean Quahog)") +
  ylim(-3,3) +
  xlab(" ") + 
  ylab("Annual Variability in Landings Volume (normalized z-score)") +
  labs(color = "Z score") +
  scale_x_continuous(breaks=seq(2010,2020,1)) +
  theme_dark() +
  theme(axis.text.x = element_text(angle = 90, vjust = .5)) 

ggsave("2.c.2.b.Trends_Landing.pdf", width = 15, height = 9)

# Goosefish
monkfish <- lands_z_me_nonconf_focus[which(lands_z_me_nonconf_focus$Common.Name == 'GOOSEFISH'),]

p3 <- ggplot(monkfish, aes(x=Year, y=zscores, color = zscores)) +
  #geom_line(size = 1)+
  geom_hline(yintercept=0, size = 2) +
  stat_smooth(aes(color=..y..), method = "lm", formula = y ~ x, size = 2) +
  scale_colour_gradient2(low = "#062847", high = "#e03f07", 
                         midpoint=0) +
  geom_point(size = 3) +
  ggtitle("10-Year Trend in Landings by Volume (Monkfish)") +
  ylim(-3,3) +
  xlab(" ") + 
  ylab("Annual Variability in Landings Volume (normalized z-score)") +
  labs(color = "Z score") +
  scale_x_continuous(breaks=seq(2010,2020,1)) +
  theme_dark() +
  theme(axis.text.x = element_text(angle = 90, vjust = .5)) 

ggsave("2.c.2.c.Trends_Landing.pdf", width = 15, height = 9)


# Pollock
pollock <- lands_z_me_nonconf_focus[which(lands_z_me_nonconf_focus$Common.Name == 'POLLOCK'),]

p4 <- ggplot(pollock, aes(x=Year, y=zscores, color = zscores)) +
  #geom_line(size = 1)+
  geom_hline(yintercept=0, size = 2) +
  stat_smooth(aes(color=..y..), method = "lm", formula = y ~ x, size = 2) +
  scale_colour_gradient2(low = "#062847", high = "#e03f07", 
                         midpoint=0) +
  geom_point(size = 3) +
  ggtitle("10-Year Trend in Landings by Volume (Pollock)") +
  ylim(-3,3) +
  xlab(" ") + 
  ylab("Annual Variability in Landings Volume (normalized z-score)") +
  labs(color = "Z score") +
  scale_x_continuous(breaks=seq(2010,2020,1)) +
  theme_dark() +
  theme(axis.text.x = element_text(angle = 90, vjust = .5)) 

ggsave("2.c.2.d.Trends_Landing.pdf", width = 15, height = 9)


# Atlantic Halibut
halibut <- lands_z_me_nonconf_focus[which(lands_z_me_nonconf_focus$Common.Name == 'HALIBUT, ATLANTIC'),]

p5 <- ggplot(halibut, aes(x=Year, y=zscores, color = zscores)) +
  #geom_line(size = 1)+
  geom_hline(yintercept=0, size = 2) +
  stat_smooth(aes(color=..y..), method = "lm", formula = y ~ x, size = 2) +
  scale_colour_gradient2(low = "#062847", high = "#e03f07", 
                         midpoint=0) +
  geom_point(size = 3) +
  ggtitle("10-Year Trend in Landings by Volume (Atlantic halibut)") +
  ylim(-3,3) +
  xlab(" ") + 
  ylab("Annual Variability in Landings Volume (normalized z-score)") +
  labs(color = "Z score") +
  scale_x_continuous(breaks=seq(2010,2020,1)) +
  theme_dark() +
  theme(axis.text.x = element_text(angle = 90, vjust = .5)) 

ggsave("2.c.2.e.Trends_Landing.pdf", width = 15, height = 9)


# Atlantic cod
cod <- lands_z_me_nonconf_focus[which(lands_z_me_nonconf_focus$Common.Name == 'COD, ATLANTIC'),]

p6 <- ggplot(cod, aes(x=Year, y=zscores, color = zscores)) +
  #geom_line(size = 1)+
  geom_hline(yintercept=0, size = 2) +
  stat_smooth(aes(color=..y..), method = "lm", formula = y ~ x, size = 2) +
  scale_colour_gradient2(low = "#062847", high = "#e03f07", 
                         midpoint=0) +
  geom_point(size = 3) +
  ggtitle("10-Year Trend in Landings by Volume (Atlantic cod)") +
  ylim(-3,3) +
  xlab(" ") + 
  ylab("Annual Variability in Landings Volume (normalized z-score)") +
  labs(color = "Z score") +
  scale_x_continuous(breaks=seq(2010,2020,1)) +
  theme_dark() +
  theme(axis.text.x = element_text(angle = 90, vjust = .5)) 

ggsave("2.c.2.f.Trends_Landing.pdf", width = 15, height = 9)


# American eel
eel <- lands_z_me_nonconf_focus[which(lands_z_me_nonconf_focus$Common.Name == 'EEL, AMERICAN'),]

p7 <- ggplot(eel, aes(x=Year, y=zscores, color = zscores)) +
  #geom_line(size = 1)+
  geom_hline(yintercept=0, size = 2) +
  stat_smooth(aes(color=..y..), method = "lm", formula = y ~ x, size = 2) +
  scale_colour_gradient2(low = "#062847", high = "#e03f07", 
                         midpoint=0) +
  geom_point(size = 3) +
  ggtitle("10-Year Trend in Landings by Volume (American eel)") +
  ylim(-3,3) +
  xlab(" ") + 
  ylab("Annual Variability in Landings Volume (normalized z-score)") +
  labs(color = "Z score") +
  scale_x_continuous(breaks=seq(2010,2020,1)) +
  theme_dark() +
  theme(axis.text.x = element_text(angle = 90, vjust = .5)) 

ggsave("2.c.2.g.Trends_Landing.pdf", width = 15, height = 9)


# Haddock 
haddock <- lands_z_me_nonconf_focus[which(lands_z_me_nonconf_focus$Common.Name == 'HADDOCK'),]

p8 <- ggplot(haddock, aes(x=Year, y=zscores, color = zscores)) +
  #geom_line(size = 1)+
  geom_hline(yintercept=0, size = 2) +
  stat_smooth(aes(color=..y..), method = "lm", formula = y ~ x, size = 2) +
  scale_colour_gradient2(low = "#062847", high = "#e03f07", 
                         midpoint=0) +
  geom_point(size = 3) +
  ggtitle("10-Year Trend in Landings by Volume (Haddock)") +
  ylim(-3,3) +
  xlab(" ") + 
  ylab("Annual Variability in Landings Volume (normalized z-score)") +
  labs(color = "Z score") +
  scale_x_continuous(breaks=seq(2010,2020,1)) +
  theme_dark() +
  theme(axis.text.x = element_text(angle = 90, vjust = .5)) 

ggsave("2.c.2.h.Trends_Landing.pdf", width = 15, height = 9)


# softshell clam 

clam_soft <- lands_z_me_nonconf_focus[which(lands_z_me_nonconf_focus$Common.Name == 'CLAM, SOFT'),]

p9 <- ggplot(clam_soft, aes(x=Year, y=zscores, color = zscores)) +
  #geom_line(size = 1)+
  geom_hline(yintercept=0, size = 2) +
  stat_smooth(aes(color=..y..), method = "lm", formula = y ~ x, size = 2) +
  scale_colour_gradient2(low = "#062847", high = "#e03f07", 
                         midpoint=0) +
  geom_point(size = 3) +
  ggtitle("10-Year Trend in Landings by Volume (Softshell clam)") +
  ylim(-3,3) +
  xlab(" ") + 
  ylab("Annual Variability in Landings Volume (normalized z-score)") +
  labs(color = "Z score") +
  scale_x_continuous(breaks=seq(2010,2020,1)) +
  theme_dark() +
  theme(axis.text.x = element_text(angle = 90, vjust = .5)) 

ggsave("2.c.2.i.Trends_Landing.pdf", width = 15, height = 9)

# bluefin tuna

tuna_bluefin <- lands_z_me_nonconf_focus[which(lands_z_me_nonconf_focus$Common.Name == 'TUNA, BLUEFIN'),]

p10 <- ggplot(tuna_bluefin, aes(x=Year, y=zscores, color = zscores)) +
  #geom_line(size = 1)+
  geom_hline(yintercept=0, size = 2) +
  stat_smooth(aes(color=..y..), method = "lm", formula = y ~ x, size = 2) +
  scale_colour_gradient2(low = "#062847", high = "#e03f07", 
                         midpoint=0) +
  geom_point(size = 3) +
  ggtitle("10-Year Trend in Landings by Volume (Bluefin tuna)") +
  ylim(-3,3) +
  xlab(" ") + 
  ylab("Annual Variability in Landings Volume (normalized z-score)") +
  labs(color = "Z score") +
  scale_x_continuous(breaks=seq(2010,2020,1)) +
  theme_dark() +
  theme(axis.text.x = element_text(angle = 90, vjust = .5)) 

ggsave("2.c.2.j.Trends_Landing.pdf", width = 15, height = 9)

# Jonah crab

crab_jonah <- lands_z_me_nonconf_focus[which(lands_z_me_nonconf_focus$Common.Name == 'CRAB, JONAH'),]

p11 <- ggplot(crab_jonah, aes(x=Year, y=zscores, color = zscores)) +
  #geom_line(size = 1)+
  geom_hline(yintercept=0, size = 2) +
  stat_smooth(aes(color=..y..), method = "lm", formula = y ~ x, size = 2) +
  scale_colour_gradient2(low = "#062847", high = "#e03f07", 
                         midpoint=0) +
  geom_point(size = 3) +
  ggtitle("10-Year Trend in Landings by Volume (Jonah crab)") +
  ylim(-3,3) +
  xlab(" ") + 
  ylab("Annual Variability in Landings Volume (normalized z-score)") +
  labs(color = "Z score") +
  scale_x_continuous(breaks=seq(2010,2020,1)) +
  theme_dark() +
  theme(axis.text.x = element_text(angle = 90, vjust = .5)) 

ggsave("2.c.2.k.Trends_Landing.pdf", width = 15, height = 9)


# Sea scallop

scallop <- lands_z_me_nonconf_focus[which(lands_z_me_nonconf_focus$Common.Name == 'SCALLOP, SEA'),]

p12 <- ggplot(scallop, aes(x=Year, y=zscores, color = zscores)) +
  #geom_line(size = 1)+
  geom_hline(yintercept=0, size = 2) +
  stat_smooth(aes(color=..y..), method = "lm", formula = y ~ x, size = 2) +
  scale_colour_gradient2(low = "#062847", high = "#e03f07", 
                         midpoint=0) +
  geom_point(size = 3) +
  ggtitle("10-Year Trend in Landings by Volume (Sea scallop)") +
  ylim(-3,3) +
  xlab(" ") + 
  ylab("Annual Variability in Landings Volume (normalized z-score)") +
  labs(color = "Z score") +
  scale_x_continuous(breaks=seq(2010,2020,1)) +
  theme_dark() +
  theme(axis.text.x = element_text(angle = 90, vjust = .5)) 

ggsave("2.c.2.l.Trends_Landing.pdf", width = 15, height = 9)


# Eastern oyster 

oyster_eastern <- lands_z_me_nonconf_focus[which(lands_z_me_nonconf_focus$Common.Name == 'OYSTER, EASTERN'),]

p13 <- ggplot(oyster_eastern, aes(x=Year, y=zscores, color = zscores)) +
  #geom_line(size = 1)+
  geom_hline(yintercept=0, size = 2) +
  stat_smooth(aes(color=..y..), method = "lm", formula = y ~ x, size = 2) +
  scale_colour_gradient2(low = "#062847", high = "#e03f07", 
                         midpoint=0) +
  geom_point(size = 3) +
  ggtitle("10-Year Trend in Landings by Volume (Eastern oyster)") +
  ylim(-3,3) +
  xlab(" ") + 
  ylab("Annual Variability in Landings Volume (normalized z-score)") +
  labs(color = "Z score") +
  scale_x_continuous(breaks=seq(2010,2020,1)) +
  theme_dark() +
  theme(axis.text.x = element_text(angle = 90, vjust = .5)) 

ggsave("2.c.2.m.Trends_Landing.pdf", width = 15, height = 9)

# Ocean quahogs (northern)  

quahog_northern <- lands_z_me_nonconf_focus[which(lands_z_me_nonconf_focus$Common.Name == 'QUAHOG, OCEAN, NORTHERN'),]

p14 <- ggplot(quahog_northern, aes(x=Year, y=zscores, color = zscores)) +
  #geom_line(size = 1)+
  geom_hline(yintercept=0, size = 2) +
  stat_smooth(aes(color=..y..), method = "lm", formula = y ~ x, size = 2) +
  scale_colour_gradient2(low = "#062847", high = "#e03f07", 
                         midpoint=0) +
  geom_point(size = 3) +
  ggtitle("10-Year Trend in Landings by Volume (Northern Quahog)") +
  ylim(-3,3) +
  xlab(" ") + 
  ylab("Annual Variability in Landings Volume (normalized z-score)") +
  labs(color = "Z score") +
  scale_x_continuous(breaks=seq(2010,2020,1)) +
  theme_dark() +
  theme(axis.text.x = element_text(angle = 90, vjust = .5)) 

ggsave("2.c.2.n.Trends_Landing.pdf", width = 15, height = 9)

# Seaweed 

seaweed <- lands_z_me_nonconf_focus[which(lands_z_me_nonconf_focus$Common.Name == 'SEAWEED'),]

p14 <- ggplot(seaweed, aes(x=Year, y=zscores, color = zscores)) +
  #geom_line(size = 1)+
  geom_hline(yintercept=0, size = 2) +
  stat_smooth(aes(color=..y..), method = "lm", formula = y ~ x, size = 2) +
  scale_colour_gradient2(low = "#062847", high = "#e03f07", 
                         midpoint=0) +
  geom_point(size = 3) +
  ggtitle("10-Year Trend in Landings by Volume (Seaweed)") +
  ylim(-3,3) +
  xlab(" ") + 
  ylab("Annual Variability in Landings Volume (normalized z-score)") +
  labs(color = "Z score") +
  scale_x_continuous(breaks=seq(2010,2020,1)) +
  theme_dark() +
  theme(axis.text.x = element_text(angle = 90, vjust = .5)) 

ggsave("2.c.2.o.Trends_Landing.pdf", width = 15, height = 9)


# Menhadens 

menhadens <- lands_z_me_nonconf_focus[which(lands_z_me_nonconf_focus$Common.Name == 'MENHADENS'),]

p15 <- ggplot(menhadens, aes(x=Year, y=zscores, color = zscores)) +
  #geom_line(size = 1)+
  geom_hline(yintercept=0, size = 2) +
  stat_smooth(aes(color=..y..), method = "lm", formula = y ~ x, size = 2) +
  scale_colour_gradient2(low = "#062847", high = "#e03f07", 
                         midpoint=0) +
  geom_point(size = 3) +
  ggtitle("10-Year Trend in Landings by Volume (Menhadens)") +
  ylim(-3,3) +
  xlab(" ") + 
  ylab("Annual Variability in Landings Volume (normalized z-score)") +
  labs(color = "Z score") +
  scale_x_continuous(breaks=seq(2010,2020,1)) +
  theme_dark() +
  theme(axis.text.x = element_text(angle = 90, vjust = .5)) 

ggsave("2.c.2.p.Trends_Landing.pdf", width = 15, height = 9)

# Mussel 

mussel <- lands_z_me_nonconf_focus[which(lands_z_me_nonconf_focus$Common.Name == 'MUSSEL, SEA'),]

p16 <- ggplot(mussel, aes(x=Year, y=zscores, color = zscores)) +
  #geom_line(size = 1)+
  geom_hline(yintercept=0, size = 2) +
  stat_smooth(aes(color=..y..), method = "lm", formula = y ~ x, size = 2) +
  scale_colour_gradient2(low = "#062847", high = "#e03f07", 
                         midpoint=0) +
  geom_point(size = 3) +
  ggtitle("10-Year Trend in Landings by Volume (Blue Mussels)") +
  ylim(-3,3) +
  xlab(" ") + 
  ylab("Annual Variability in Landings Volume (normalized z-score)") +
  labs(color = "Z score") +
  scale_x_continuous(breaks=seq(2010,2020,1)) +
  theme_dark() +
  theme(axis.text.x = element_text(angle = 90, vjust = .5)) 

ggsave("2.c.2.q.Trends_Landing.pdf", width = 15, height = 9)



