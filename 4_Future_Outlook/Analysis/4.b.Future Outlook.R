# Future Outlook 
# 4.b.Future Outlook
# Version 1.0. March 1, 2022


#loading packages
library(dplyr)   
library(tidyr)
library(ggplot2) 
library(RColorBrewer)
library(scales)
library(ggrepel)


# reading in data
lands <- read.csv("/Users/jstoll/OneDrive - University of Maine System/Social_Coasts_Lab/seafood_systems/Data/Processed_Data/ACCSP_Landings_Nonconfidential_Commercial_2007_2020_clean.csv", stringsAsFactors = FALSE)

cats <- read.csv("/Users/jstoll/OneDrive - University of Maine System/Social_Coasts_Lab/seafood_systems/Data/Processed_Data/Species_Categories_Climate_Vulnerability_clean.csv", stringsAsFactors = FALSE)

### data prep
#replacing NA vulnerability with "Unknown"
cats <- cats %>%
  mutate(Vulnerability = ifelse(is.na(Vulnerability), "Unknown", Vulnerability))

# change common names

for(i in 1:nrow(lands)) {
  if(lands$Common.Name[i] == 'MUSSEL, SEA') lands$Common.Name[i] <- 'MUSSEL'  
}

for(i in 1:nrow(lands)) {
  if(lands$Common.Name[i] == 'URCHIN, GREEN SEA, (S. DROEBACHIENSIS)') lands$Common.Name[i] <- 'URCHIN, GREEN SEA'  
}


###################################################
#####Tables and heatmap of 5 and 10-year trends####             
###################################################

#removing one of the duplicated dolphinfish spp.
cats <- cats[c(1:244, 246:289), ]


####zscore####
lands <- as_tibble(lands)                                              #making the df a tibble

#merging cats (NOAA climate vulnerability) onto lands
lands <- left_join(lands, cats, by = "Common.Name")

####nesting lands by state and species to calc z-score for each species independently####  calculate z-score for each combo of species and state. One score for each year chunk
lands_z <- lands %>%
  filter(Year >= 2010) %>%
  group_by(State, Common.Name) %>%                               #group by state and spp
  mutate(zscores = (Dollars - mean(Dollars))/sd(Dollars)) %>%    #calculate z-scores 
  arrange(Common.Name, State, Year)                              #arranging nicely


##splitting out states
lands_z_me <- filter(lands_z, State == "MAINE")
lands_z_ma <- filter(lands_z, State == "MASSACHUSETTS")
lands_z_nh <- filter(lands_z, State == "NEW HAMPSHIRE")
lands_z_ct <- filter(lands_z, State == "CONNECTICUT")
lands_z_ri <- filter(lands_z, State == "RHODE ISLAND")


##############################
####prepping zscores graph####
##############################
#ordering heatmap y-axis by 2020 species zscores
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
    slice_max(Year, n = 1)                                         #in cases of 0s or where 2020 had lower landings than other years, I'm keeping the year with highest value!
  
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




####arranging df by zscore####
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


####separating out nonconf spp####
####Maine
lands_z_me_order_nonconf <- lands_z_me_order %>%                              #THIS IS WHAT i WANT FOR THE GRAPH! 
  filter(!is.na(zscores))

####mass
lands_z_ma_order_nonconf <- lands_z_ma_order %>%                              #ordered list of nonconfidential spp, with 2020 or highest non-2020 values
  filter(!is.na(zscores))

#New Hampshire
lands_z_nh_order_nonconf <- lands_z_nh_order %>%                              #
  filter(!is.na(zscores))

#CT
lands_z_ct_order_nonconf <- lands_z_ct_order %>%                              #
  filter(!is.na(zscores))

#RI
lands_z_ri_order_nonconf <- lands_z_ri_order %>%                              #
  filter(!is.na(zscores))


############################
####calculating rsquared####
############################

#pivoting all of the data wide so I can compare across years
lands_wide <- pivot_wider(lands_z, 
                          id_cols = c(State, Common.Name),
                          values_from = c(Dollars, zscores),
                          names_from = Year)


#convert NA to 0
lands_wide <- lands_wide %>% replace(is.na(.), 0)


#calculating R^2 for 2010-2020 zscores
lands_wide$sumxy_z1020 <- (2010*lands_wide$zscores_2010) + (2011*lands_wide$zscores_2011) + (2012*lands_wide$zscores_2012) + (2013*lands_wide$zscores_2013) + (2014*lands_wide$zscores_2014) + (2015*lands_wide$zscores_2015) + (2016*lands_wide$zscores_2016) + (2017*lands_wide$zscores_2017) + (2018*lands_wide$zscores_2018) + (2019*lands_wide$zscores_2019) + (2020*lands_wide$zscores_2020) 
lands_wide$sumx1020 <- 2010 + 2011 + 2012 + 2013 + 2014 + 2015 + 2016 + 2017 + 2018 + 2019 + 2020
lands_wide$sumy_z1020 <- lands_wide$zscores_2010 + lands_wide$zscores_2011 + lands_wide$zscores_2012 + lands_wide$zscores_2013 + lands_wide$zscores_2014 + lands_wide$zscores_2015 + lands_wide$zscores_2016 + lands_wide$zscores_2017 + lands_wide$zscores_2018 + lands_wide$zscores_2019 + lands_wide$zscores_2020
lands_wide$sumxsquared1020 <- (2010^2) + (2011^2) + (2012^2) + (2013^2) + (2014^2) + (2015^2) + (2016^2) + (2017^2) + (2018^2) + (2019^2) + (2020^2) 
lands_wide$sumysquared1020 <- (lands_wide$zscores_2010^2) + (lands_wide$zscores_2011^2) + (lands_wide$zscores_2012^2) + (lands_wide$zscores_2013^2) + (lands_wide$zscores_2014^2) + (lands_wide$zscores_2015^2) + (lands_wide$zscores_2016^2) + (lands_wide$zscores_2017^2) + (lands_wide$zscores_2018^2) + (lands_wide$zscores_2019^2) + (lands_wide$zscores_2020^2)

lands_wide$rsquared_z1020 <- (((11*lands_wide$sumxy_z1020)-(lands_wide$sumx1020 * lands_wide$sumy_z1020))/
                                (sqrt((11*lands_wide$sumxsquared1020) - (lands_wide$sumx1020^2)) * 
                                   sqrt((11*lands_wide$sumysquared1020) - (lands_wide$sumy_z1020^2))))^2


#trimming to only keep state, spp, and r2
lands_wide_trim <- lands_wide %>%
  select(State, Common.Name, rsquared_z1020)


#####splitting out states####
lands_wide_trim_me <- filter(lands_wide_trim, State == "MAINE")
lands_wide_trim_ma <- filter(lands_wide_trim, State == "MASSACHUSETTS")
lands_wide_trim_nh <- filter(lands_wide_trim, State == "NEW HAMPSHIRE")
lands_wide_trim_ct <- filter(lands_wide_trim, State == "CONNECTICUT")
lands_wide_trim_ri <- filter(lands_wide_trim, State == "RHODE ISLAND")



####keeping only the nonconf spp####

#ME
lands_wide_trim_me_nonconf <- semi_join(lands_wide_trim_me, lands_z_me_order_nonconf,    #using "lands_z_me_order_nonconf" from above - it is a list of the nonconfidential sppecies for that state
                                        by = "Common.Name")                                   #filtering lands_z_me so only the nonconf spp are present. I LOVE semi_join!

#MA
lands_wide_trim_ma_nonconf <- semi_join(lands_wide_trim_ma, lands_z_ma_order_nonconf,    #using "lands_z_ma_order_nonconf" from above - it is a list of the nonconfidential sppecies for that state
                                        by = "Common.Name")                                   #filtering lands_z_ma so only the nonconf spp are present. I LOVE semi_join!

#NH
lands_wide_trim_nh_nonconf <- semi_join(lands_wide_trim_nh, lands_z_nh_order_nonconf,    #using "lands_z_nh_order_nonconf" from above - it is a list of the nonconfidential sppecies for that state
                                        by = "Common.Name")                                   #filtering lands_z_nh so only the nonconf spp are present. I LOVE semi_join!

#ct
lands_wide_trim_ct_nonconf <- semi_join(lands_wide_trim_ct, lands_z_ct_order_nonconf,    #using "lands_z_ct_order_nonconf" from above - it is a list of the nonconfidential sppecies for that state
                                        by = "Common.Name")                                   #filtering lands_z_ct so only the nonconf spp are present. I LOVE semi_join!

#ri
lands_wide_trim_ri_nonconf <- semi_join(lands_wide_trim_ri, lands_z_ri_order_nonconf,    #using "lands_z_ri_order_nonconf" from above - it is a list of the nonconfidential sppecies for that state
                                        by = "Common.Name")                                   #filtering lands_z_ri so only the nonconf spp are present. I LOVE semi_join!


##########################################
####merging zscore and r2 dfs together####
##########################################

#Maine
me_zr <- full_join(lands_z_me_order_nonconf, lands_wide_trim_me_nonconf,              #merging the two dfs together
                   by = c("State", "Common.Name"))

tot_val <- sum(me_zr$Dollars)

me_zr <- me_zr %>%
  select(Year, State, Common.Name, Scientific.Name, 
         Dollars, zscores, rsquared_z1020, NOAA.category,
         Vulnerability) %>%                                                                 #keeping the useful columns
  mutate(prop_val = (Dollars/tot_val)*100, .after = rsquared_z1020)                         #creating new column with percent of total value for species

#NH
nh_zr <- full_join(lands_z_nh_order_nonconf, lands_wide_trim_nh_nonconf,              #merging the two dfs together
                   by = c("State", "Common.Name"))

tot_val <- sum(nh_zr$Dollars)

nh_zr <- nh_zr %>%
  select(Year, State, Common.Name, Scientific.Name, 
         Dollars, zscores, rsquared_z1020, NOAA.category,
         Vulnerability) %>%                                                                 #keeping the useful columns
  mutate(prop_val = (Dollars/tot_val)*100, .after = rsquared_z1020)                         #creating new column with percent of total value for species

#MA
ma_zr <- full_join(lands_z_ma_order_nonconf, lands_wide_trim_ma_nonconf,              #merging the two dfs together
                   by = c("State", "Common.Name"))

tot_val <- sum(ma_zr$Dollars)

ma_zr <- ma_zr %>%
  select(Year, State, Common.Name, Scientific.Name, 
         Dollars, zscores, rsquared_z1020, NOAA.category,
         Vulnerability) %>%                                                                 #keeping the useful columns
  mutate(prop_val = (Dollars/tot_val)*100, .after = rsquared_z1020)                         #creating new column with percent of total value for species

#CT
ct_zr <- full_join(lands_z_ct_order_nonconf, lands_wide_trim_ct_nonconf,              #merging the two dfs together
                   by = c("State", "Common.Name"))

tot_val <- sum(ct_zr$Dollars)

ct_zr <- ct_zr %>%
  select(Year, State, Common.Name, Scientific.Name, 
         Dollars, zscores, rsquared_z1020, NOAA.category,
         Vulnerability) %>%                                                                 #keeping the useful columns
  mutate(prop_val = (Dollars/tot_val)*100, .after = rsquared_z1020)                         #creating new column with percent of total value for species

#RI
ri_zr <- full_join(lands_z_ri_order_nonconf, lands_wide_trim_ri_nonconf,              #merging the two dfs together
                   by = c("State", "Common.Name"))

tot_val <- sum(ri_zr$Dollars)

ri_zr <- ri_zr %>%
  select(Year, State, Common.Name, Scientific.Name, 
         Dollars, zscores, rsquared_z1020, NOAA.category,
         Vulnerability) %>%                                                                 #keeping the useful columns
  mutate(prop_val = (Dollars/tot_val)*100, .after = rsquared_z1020)                         #creating new column with percent of total value for species

################
####graphing####
################

####maine####
#adding color
#4.4 range for x-axis. 4.4/3 = ~1.47. So -2.2, -0.73, 0.74, 2.2
#1 range for y. So 0.3, 0.6, 1

me_zr <- me_zr %>%
  mutate(color = ifelse(rsquared_z1020 <= 0.33 & zscores <= -0.73, "red", "yellow"),
         color = ifelse(rsquared_z1020 >= 0.33 & zscores <= -0.73, "orange", color),
         color = ifelse(rsquared_z1020 <= 0.33 & zscores >= -0.73, "orange", color),
         color = ifelse(rsquared_z1020 >= 0.66 & zscores >= 0.74, "green", color))

#writing csv of this so I have a list of species and their category
#write.csv(me_zr, "me_nonconf_vulnerability_v2.csv", row.names = FALSE)

#making vulnerability a factor and setting the order so it turns out how I want it in the legend
me_zr$Vulnerability <- factor(me_zr$Vulnerability, 
                              levels = c("Very High", "High", "Moderate", "Low", "Unknown"))

#making blobs
blobs <- tibble(x_min = rep(c(-2.2, -0.73, 0.74), each = 3),                        #created a sequence for xmins, the "each" means "repeat each number 3 times"
                x_max = rep(c(-0.73, 0.74, 2.2), each = 3),                         #ditto
                y_min = rep(c(0, 0.33, 0.66), 3),                                   #created a sequence for ymins, the "3" means "repeat the sequence 3 times" - need this so each box is represented
                y_max = rep(c(0.33, 0.66, 1), 3),                                   #ditto
                colrs = c(1, 2, 2, 2, 3, 3,                                         #colors indicating the colors I want in the final boxes. Probably a cleaner way to do this... 
                          2, 3, 4))

colrs_new <- tibble(colrs = c(2, 4, 3, 1),                                          #indicator column to match with blobs
                    outlk = c("Moderately Low", "High", "Moderately High", "Low"),  #labels
                    hex = c("#FFA500", "#00FF00","#FFFF00", "#FF0000"))             #hexes of the colors - transparent colors: c("#FFCC66", "#66FF66","#FFFF66", "#FF6666")

#merging together
blobs_colrs <- left_join(blobs, colrs_new, by = "colrs")


#color blobs
blob_plot_me <- ggplot(blobs_colrs, aes(xmin = x_min, xmax = x_max, 
                                        ymin = y_min, ymax = y_max, fill = outlk)) +              #creating the colored blobs - here is the data
  geom_rect() +                                                                                   #here I create the blobs
  geom_segment(aes(x = 0, y = 0, xend = 0, yend = 1)) +                                           #adding vertical line at 0
  scale_fill_manual(values = colrs_new$hex[c(2, 3, 1, 4)], 
                    breaks = colrs_new$outlk[c(2, 3, 1, 4)]) +                                    #coloring, based on colrs_new. Difficult to get the order right... hence the ([()]) nonsense. 
  geom_point(data = me_zr, aes(zscores, rsquared_z1020,
                               size = prop_val), inherit.aes = FALSE) +    #scatterplot here
  geom_text_repel(data=me_zr %>% filter(prop_val>0.5), # Filter data first
                  aes(zscores, rsquared_z1020, label=Common.Name, xmin = NULL, xmax = NULL, ymin = NULL, ymax = NULL, fill = NULL), size= 5, nudge_y = .05) +
  scale_shape_manual(values = c(21, 22, 23, 24, 25)) +                                            #scatterplot shapes here
  scale_size_continuous(limits = c(0, 86), 
                        breaks = c(0, 1, 5, 50), 
                        labels = c(0, 1, 5, 50)) +                                 #scaling points
  scale_y_continuous(limits = c(-0.02, 1),
                     breaks = c(0, 0.33, 0.66, 1.00),
                     labels = c(0, 0.33, 0.66, 1.00)) +                                           #scaling y-axis so it is nice
  scale_x_continuous(limits = c(-2.2, 2.2),
                     breaks = c(-2.2, -0.73, 0, 0.74, 2.2),
                     labels = c(-2.2, -0.73, 0, 0.74, 2.2)) +                                     #scaling x-axis so it is nice
  labs(x = "Trend in Landings Value (z score)", 
       y = expression(paste("Stability in Landings Value (r "^{2},")")),                                                 
       size = "% Total \nValue", 
       fill = "Future \nFishery Outlook",
       shape = "Climate \nVulnerability") +                                                       #relabelling
  annotate("text", x = 1, y = -0.02, 
           label = "Landings Increasing", fontface = "italic") +                                  #annotation to label increasing landings
  annotate("text", x = -1, y = -0.02, 
           label = "Landings Decreasing", fontface = "italic") +                                  #annotation to label decreasing landings
  ggtitle("Future Outlook based Trends in Landings Value & Stability (2010-2020)") +
  labs(caption="Note: If no data are available for a given year, analysis defaults to more recent year(s)")+
  theme_bw() 
  
ggsave("4.b.Future_Outlook.pdf", width = 15, height = 9)

