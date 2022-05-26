# Seafood Production 
# 2.d.1 - 2.d.15
# Version 1.0. March 1, 2022

# Key species of focus

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
# Mussels

### packages ####
library(ggplot2)
library(ggpubr)
library(dplyr)


#### data loading ####

production <- read.csv("/Users/jstoll/OneDrive - University of Maine System/Social_Coasts_Lab/seafood_systems/Data/Processed_Data/ACCSP_Landings_Nonconfidential_Commercial_2007_2020_clean.csv", stringsAsFactors = FALSE)


cats <- read.csv("/Users/jstoll/OneDrive - University of Maine System/Social_Coasts_Lab/seafood_systems/Data/Processed_Data/Species_Categories_Climate_Vulnerability_clean.csv", stringsAsFactors = FALSE)

#adding in confidential values so we get a more accurate picture of total landings for the region
NOAA_conf <- read.csv("/Users/jstoll/OneDrive - University of Maine System/Social_Coasts_Lab/seafood_systems/Data/Processed_Data/NOAA_Landings_Summarized_Confidential_lbs_value_2007_2020_clean.csv", stringsAsFactors = FALSE)

#### data merge ####

#merging cats and production together
production <- full_join(production, cats, by = "Common.Name")

#merging NOAA and production together
production <- bind_rows(production, NOAA_conf)

#keeping 2010-2020
production <- filter(production, Year >= 2010)

### Create plots 

#e37a55 - Orange
#233749 - Blue

cbbPalette <- c("#7abfff", "#5f96c9", "#476f94", "#233749", "#e37a55")

# Menhadens 

menhaden <- production[which(production$Common.Name == 'MENHADENS'),]
menhaden$State <- factor(menhaden$State, levels = c("CONNECTICUT","RHODE ISLAND", "MASSACHUSETTS","NEW HAMPSHIRE","MAINE"))

p1 <- ggplot(menhaden, aes(x=Year, y=Pounds/1000000, fill=State)) + 
  geom_bar(position="stack", stat="identity") + xlab("Year") + ylab("Lbs (millions)") + 
  ggtitle("MENHADENS LANDINGS") +
  scale_x_continuous(name="Year", breaks=c(2010:2020)) +
  scale_fill_manual(values=cbbPalette) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90)) +
  theme(axis.line = element_line(colour = "black", 
                                 size = 1, linetype = "solid")) 

p2 <- ggplot(menhaden, aes(x=Year, y=Dollars/1000000, fill=State)) + 
  geom_bar(position="stack", stat="identity") + xlab("Year") + ylab("Dollars (millions)") + 
  ggtitle("MENHADENS VALUE") +
  scale_x_continuous(name="Year", breaks=c(2010:2020)) +
  scale_fill_manual(values=cbbPalette) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90)) +
  theme(axis.line = element_line(colour = "black", 
                                 size = 1, linetype = "solid")) 

plot_menhadens <- 
  ggarrange(p1, p2, labels = c("A", "B"),
            common.legend = TRUE, legend = "right")
ggsave("2.d.5.plot_menhadens.pdf", width = 15, height = 6)


# Atlantic herring


herring <- production[which(production$Common.Name == 'HERRING, ATLANTIC'),]
herring$State <- factor(herring$State, levels = c("CONNECTICUT","RHODE ISLAND", "MASSACHUSETTS","NEW HAMPSHIRE","MAINE"))

h1 <- ggplot(herring, aes(x=Year, y=Pounds/1000000, fill=State)) + 
  geom_bar(position="stack", stat="identity") + xlab("Year") + ylab("Lbs (millions)") + 
  ggtitle("ATLANTIC HERRING LANDINGS") +
  scale_x_continuous(name="Year", breaks=c(2010:2020)) +
  scale_fill_manual(values=cbbPalette) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90)) +
  theme(axis.line = element_line(colour = "black", 
                                 size = 1, linetype = "solid")) 

h2 <- ggplot(herring, aes(x=Year, y=Dollars/1000000, fill=State)) + 
  geom_bar(position="stack", stat="identity") + xlab("Year") + ylab("Dollars (millions)") + 
  ggtitle("ATLANTIC HERRING VALUE") +
  scale_x_continuous(name="Year", breaks=c(2010:2020)) +
  scale_fill_manual(values=cbbPalette) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90)) +
  theme(axis.line = element_line(colour = "black", 
                                 size = 1, linetype = "solid")) 

plot_herring <- 
  ggarrange(h1, h2, labels = c("A", "B"),
          common.legend = TRUE, legend = "right")

ggsave("2.d.4.plot_herring.pdf", width = 15, height = 6)



# Jonah Crab

crab_jonah <- production[which(production$Common.Name == 'CRAB, JONAH'),]
crab_jonah$State <- factor(crab_jonah$State, levels = c("CONNECTICUT","RHODE ISLAND", "MASSACHUSETTS","NEW HAMPSHIRE","MAINE"))

j1 <- ggplot(crab_jonah, aes(x=Year, y=Pounds/1000000, fill=State)) + 
  geom_bar(position="stack", stat="identity") + xlab("Year") + ylab("Lbs (millions)") + 
  ggtitle("JONAH CRAB LANDINGS") +
  scale_x_continuous(name="Year", breaks=c(2010:2020)) +
  scale_fill_manual(values=cbbPalette) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90)) +
  theme(axis.line = element_line(colour = "black", 
                                 size = 1, linetype = "solid")) 

j2 <- ggplot(crab_jonah, aes(x=Year, y=Dollars/1000000, fill=State)) + 
  geom_bar(position="stack", stat="identity") + xlab("Year") + ylab("Dollars (millions)") + 
  ggtitle("JONAH CRAB VALUE") +
  scale_x_continuous(name="Year", breaks=c(2010:2020)) +
  scale_fill_manual(values=cbbPalette) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90)) +
  theme(axis.line = element_line(colour = "black", 
                                 size = 1, linetype = "solid")) 

plot_crab_jonah <- 
ggarrange(j1, j2, labels = c("A", "B"),
          common.legend = TRUE, legend = "right")
ggsave("2.d.10.plot_crab_jonah.pdf", width = 15, height = 6)


# American lobster

lobster <- production[which(production$Common.Name == 'LOBSTER, AMERICAN'),]
lobster$State <- factor(lobster$State, levels = c("CONNECTICUT","RHODE ISLAND", "MASSACHUSETTS","NEW HAMPSHIRE","MAINE"))

l1 <- ggplot(lobster, aes(x=Year, y=Pounds/1000000, fill=State)) + 
  geom_bar(position="stack", stat="identity") + xlab("Year") + ylab("Lbs (millions)") + 
  ggtitle("AMERICAN LOBSTER LANDINGS") +
  scale_x_continuous(name="Year", breaks=c(2010:2020)) +
  scale_fill_manual(values=cbbPalette) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90)) +
  theme(axis.line = element_line(colour = "black", 
                                 size = 1, linetype = "solid")) 

l2 <- ggplot(lobster, aes(x=Year, y=Dollars/1000000, fill=State)) + 
  geom_bar(position="stack", stat="identity") + xlab("Year") + ylab("Dollars (millions)") + 
  ggtitle("AMERICAN LOBSTER VALUE") +
  scale_x_continuous(name="Year", breaks=c(2010:2020)) +
  scale_fill_manual(values=cbbPalette) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90)) +
  theme(axis.line = element_line(colour = "black", 
                                 size = 1, linetype = "solid")) 

plot_lobster <- 
ggarrange(l1, l2, labels = c("A", "B"),
          common.legend = TRUE, legend = "right")
ggsave("2.d.1.plot_lobster.pdf", width = 15, height = 6)


# Bluefin tuna

bluefin <- production[which(production$Common.Name == 'TUNA, BLUEFIN'),]
bluefin$State <- factor(bluefin$State, levels = c("CONNECTICUT","RHODE ISLAND", "MASSACHUSETTS","NEW HAMPSHIRE","MAINE"))

b1 <- ggplot(bluefin, aes(x=Year, y=Pounds/1000000, fill=State)) + 
  geom_bar(position="stack", stat="identity") + xlab("Year") + ylab("Lbs (millions)") + 
  ggtitle("BLUEFIN TUNA LANDINGS") +
  scale_x_continuous(name="Year", breaks=c(2010:2020)) +
  scale_fill_manual(values=cbbPalette) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90)) +
  theme(axis.line = element_line(colour = "black", 
                                 size = 1, linetype = "solid")) 

b2 <- ggplot(bluefin, aes(x=Year, y=Dollars/1000000, fill=State)) + 
  geom_bar(position="stack", stat="identity") + xlab("Year") + ylab("Dollars (millions)") + 
  ggtitle("BLUEFIN TUNA VALUE") +
  scale_x_continuous(name="Year", breaks=c(2010:2020)) +
  scale_fill_manual(values=cbbPalette) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90)) +
  theme(axis.line = element_line(colour = "black", 
                                 size = 1, linetype = "solid")) 

plot_bluefin <- 
ggarrange(b1, b2, labels = c("A", "B"),
          common.legend = TRUE, legend = "right")
ggsave("2.d.8.plot_bluefin.pdf", width = 15, height = 6)


# Monkfish (Goosefish)

monkfish <- production[which(production$Common.Name == 'GOOSEFISH'),]
monkfish$State <- factor(monkfish$State, levels = c("CONNECTICUT","RHODE ISLAND", "MASSACHUSETTS","NEW HAMPSHIRE","MAINE"))

m1 <- ggplot(monkfish, aes(x=Year, y=Pounds/1000000, fill=State)) + 
  geom_bar(position="stack", stat="identity") + xlab("Year") + ylab("Lbs (millions)") + 
  ggtitle("MONKFISH LANDINGS") +
  scale_x_continuous(name="Year", breaks=c(2010:2020)) +
  scale_fill_manual(values=cbbPalette) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90)) +
  theme(axis.line = element_line(colour = "black", 
                                 size = 1, linetype = "solid")) 

m2 <- ggplot(monkfish, aes(x=Year, y=Dollars/1000000, fill=State)) + 
  geom_bar(position="stack", stat="identity") + xlab("Year") + ylab("Dollars (millions)") + 
  ggtitle("MONKFISH VALUE") +
  scale_x_continuous(name="Year", breaks=c(2010:2020)) +
  scale_fill_manual(values=cbbPalette) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90)) +
  theme(axis.line = element_line(colour = "black", 
                                 size = 1, linetype = "solid")) 
plot_monkfish <- 
ggarrange(m1, m2, labels = c("A", "B"),
          common.legend = TRUE, legend = "right")
ggsave("2.d.13.plot_monkfish.pdf", width = 15, height = 6)


# Haddock

haddock <- production[which(production$Common.Name == 'HADDOCK'),]
haddock$State <- factor(haddock$State, levels = c("CONNECTICUT","RHODE ISLAND", "MASSACHUSETTS","NEW HAMPSHIRE","MAINE"))

ha1 <- ggplot(haddock, aes(x=Year, y=Pounds/1000000, fill=State)) + 
  geom_bar(position="stack", stat="identity") + xlab("Year") + ylab("Lbs (millions)") + 
  ggtitle("HADDOCK LANDINGS") +
  scale_x_continuous(name="Year", breaks=c(2010:2020)) +
  scale_fill_manual(values=cbbPalette) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90)) +
  theme(axis.line = element_line(colour = "black", 
                                 size = 1, linetype = "solid")) 

ha2 <- ggplot(haddock, aes(x=Year, y=Dollars/1000000, fill=State)) + 
  geom_bar(position="stack", stat="identity") + xlab("Year") + ylab("Dollars (millions)") + 
  ggtitle("HADDOCK VALUE") +
  scale_x_continuous(name="Year", breaks=c(2010:2020)) +
  scale_fill_manual(values=cbbPalette) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90)) +
  theme(axis.line = element_line(colour = "black", 
                                 size = 1, linetype = "solid")) 

plot_haddock <- 
ggarrange(ha1, ha2, labels = c("A", "B"),
          common.legend = TRUE, legend = "right")
ggsave("2.d.12.plot_haddock.pdf", width = 15, height = 6)


# Pollock

pollock <- production[which(production$Common.Name == 'POLLOCK'),]
pollock$State <- factor(pollock$State, levels = c("CONNECTICUT","RHODE ISLAND", "MASSACHUSETTS","NEW HAMPSHIRE","MAINE"))

po1 <- ggplot(pollock, aes(x=Year, y=Pounds/1000000, fill=State)) + 
  geom_bar(position="stack", stat="identity") + xlab("Year") + ylab("Lbs (millions)") + 
  ggtitle("POLLOCK LANDINGS") +
  scale_x_continuous(name="Year", breaks=c(2010:2020)) +
  scale_fill_manual(values=cbbPalette) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90)) +
  theme(axis.line = element_line(colour = "black", 
                                 size = 1, linetype = "solid")) 

po2 <- ggplot(pollock, aes(x=Year, y=Dollars/1000000, fill=State)) + 
  geom_bar(position="stack", stat="identity") + xlab("Year") + ylab("Dollars (millions)") + 
  ggtitle("POLLOCK VALUE") +
  scale_x_continuous(name="Year", breaks=c(2010:2020)) +
  scale_fill_manual(values=cbbPalette) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90)) +
  theme(axis.line = element_line(colour = "black", 
                                 size = 1, linetype = "solid")) 

plot_pollock <- 
ggarrange(po1, po2, labels = c("A", "B"),
          common.legend = TRUE, legend = "right")
ggsave("2.d.6.plot_pollock.pdf", width = 15, height = 6)



# Halibut

halibut <- production[which(production$Common.Name == 'HALIBUT, ATLANTIC'),]
halibut$State <- factor(halibut$State, levels = c("CONNECTICUT","RHODE ISLAND", "MASSACHUSETTS","NEW HAMPSHIRE","MAINE"))

hal1 <- ggplot(halibut, aes(x=Year, y=Pounds/1000000, fill=State)) + 
  geom_bar(halibut="stack", stat="identity") + xlab("Year") + ylab("Lbs (millions)") + 
  ggtitle("ATLANTIC HALIBUT LANDINGS") +
  scale_x_continuous(name="Year", breaks=c(2010:2020)) +
  scale_fill_manual(values=cbbPalette) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90)) +
  theme(axis.line = element_line(colour = "black", 
                                 size = 1, linetype = "solid")) 

hal2 <- ggplot(halibut, aes(x=Year, y=Dollars/1000000, fill=State)) + 
  geom_bar(position="stack", stat="identity") + xlab("Year") + ylab("Dollars (millions)") + 
  ggtitle("ATLANTIC HALIBUT VALUE") +
  scale_x_continuous(name="Year", breaks=c(2010:2020)) +
  scale_fill_manual(values=cbbPalette) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90)) +
  theme(axis.line = element_line(colour = "black", 
                                 size = 1, linetype = "solid")) 

plot_halibut <- 
ggarrange(hal1, hal2, labels = c("A", "B"),
          common.legend = TRUE, legend = "right")
ggsave("2.d.3.plot_halibut.pdf", width = 15, height = 6)


# Atlantic cod 

cod <- production[which(production$Common.Name == 'COD, ATLANTIC'),]
cod$State <- factor(cod$State, levels = c("CONNECTICUT","RHODE ISLAND", "MASSACHUSETTS","NEW HAMPSHIRE","MAINE"))

cod1 <- ggplot(cod, aes(x=Year, y=Pounds/1000000, fill=State)) + 
  geom_bar(cod="stack", stat="identity") + xlab("Year") + ylab("Lbs (millions)") + 
  ggtitle("ATLANTIC COD LANDINGS") +
  scale_x_continuous(name="Year", breaks=c(2010:2020)) +
  scale_fill_manual(values=cbbPalette) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90)) +
  theme(axis.line = element_line(colour = "black", 
                                 size = 1, linetype = "solid")) 

cod2 <- ggplot(cod, aes(x=Year, y=Dollars/1000000, fill=State)) + 
  geom_bar(position="stack", stat="identity") + xlab("Year") + ylab("Dollars (millions)") + 
  ggtitle("ATLANTIC COD VALUE") +
  scale_x_continuous(name="Year", breaks=c(2010:2020)) +
  scale_fill_manual(values=cbbPalette) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90)) +
  theme(axis.line = element_line(colour = "black", 
                                 size = 1, linetype = "solid")) 
plot_cod <- 
ggarrange(cod1, cod2, labels = c("A", "B"),
          common.legend = TRUE, legend = "right")
ggsave("2.d.2.plot_cod.pdf", width = 15, height = 6)


# Ocean Quahog  

quahog_ocean <- production[which(production$Common.Name == 'CLAM, QUAHOG, OCEAN'),]
quahog_ocean$State <- factor(quahog_ocean$State, levels = c("CONNECTICUT","RHODE ISLAND", "MASSACHUSETTS","NEW HAMPSHIRE","MAINE"))

qo1 <- ggplot(quahog_ocean, aes(x=Year, y=Pounds/1000000, fill=State)) + 
  geom_bar(quahog_ocean="stack", stat="identity") + xlab("Year") + ylab("Lbs (millions)") + 
  ggtitle("OCEAN QUAHOG LANDINGS") +
  scale_x_continuous(name="Year", breaks=c(2010:2020)) +
  scale_fill_manual(values=cbbPalette) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90)) +
  theme(axis.line = element_line(colour = "black", 
                                 size = 1, linetype = "solid")) 

qo2 <- ggplot(quahog_ocean, aes(x=Year, y=Dollars/1000000, fill=State)) + 
  geom_bar(position="stack", stat="identity") + xlab("Year") + ylab("Dollars (millions)") + 
  ggtitle("OCEAN QUAHOG VALUE") +
  scale_x_continuous(name="Year", breaks=c(2010:2020)) +
  scale_fill_manual(values=cbbPalette) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90)) +
  theme(axis.line = element_line(colour = "black", 
                                 size = 1, linetype = "solid")) 
plot_quahog_ocean <- 
ggarrange(qo1, qo2, labels = c("A", "B"),
          common.legend = TRUE, legend = "right")
ggsave("plot_quahog_ocean.pdf", width = 15, height = 6)



# Northern Quahog  

quahog_northern <- production[which(production$Common.Name == 'CLAM, QUAHOG, NORTHERN'),]
quahog_northern$State <- factor(quahog_northern$State, levels = c("CONNECTICUT","RHODE ISLAND", "MASSACHUSETTS","NEW HAMPSHIRE","MAINE"))

qo_n1 <- ggplot(quahog_northern, aes(x=Year, y=Pounds/1000000, fill=State)) + 
  geom_bar(quahog_northern="stack", stat="identity") + xlab("Year") + ylab("Lbs (millions)") + 
  ggtitle("NORTHERN QUAHOG LANDINGS") +
  scale_x_continuous(name="Year", breaks=c(2010:2020)) +
  scale_fill_manual(values=cbbPalette) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90)) +
  theme(axis.line = element_line(colour = "black", 
                                 size = 1, linetype = "solid")) 

qo_n2 <- ggplot(quahog_northern, aes(x=Year, y=Dollars/1000000, fill=State)) + 
  geom_bar(position="stack", stat="identity") + xlab("Year") + ylab("Dollars (millions)") + 
  ggtitle("NORTHERN QUAHOG VALUE") +
  scale_x_continuous(name="Year", breaks=c(2010:2020)) +
  scale_fill_manual(values=cbbPalette) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90)) +
  theme(axis.line = element_line(colour = "black", 
                                 size = 1, linetype = "solid")) 

plot_quahog_northern <-
ggarrange(qo_n1, qo_n2, labels = c("A", "B"),
          common.legend = TRUE, legend = "right")
ggsave("plot_quahog_northern.pdf", width = 15, height = 6)


# Soft-shell clam  

cbbPalette2 <- c("#5f96c9","#476f94","#e37a55")


clam_soft <- production[which(production$Common.Name == 'CLAM, SOFT'),]
clam_soft$State <- factor(clam_soft$State, levels = c("CONNECTICUT","RHODE ISLAND", "MASSACHUSETTS","NEW HAMPSHIRE","MAINE"))

sc1 <- ggplot(clam_soft, aes(x=Year, y=Pounds/1000000, fill=State)) + 
  geom_bar(clam_soft="stack", stat="identity") + xlab("Year") + ylab("Lbs (millions)") + 
  ggtitle("SOFT-SHELL CLAM LANDINGS") +
  scale_x_continuous(name="Year", breaks=c(2010:2020)) +
  scale_fill_manual(values=cbbPalette2) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90)) +
  theme(axis.line = element_line(colour = "black", 
                                 size = 1, linetype = "solid")) 

sc2 <- ggplot(clam_soft, aes(x=Year, y=Dollars/1000000, fill=State)) + 
  geom_bar(position="stack", stat="identity") + xlab("Year") + ylab("Dollars (millions)") + 
  ggtitle("SOFT-SHELL CLAM VALUE") +
  scale_x_continuous(name="Year", breaks=c(2010:2020)) +
  scale_fill_manual(values=cbbPalette2) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90)) +
  theme(axis.line = element_line(colour = "black", 
                                 size = 1, linetype = "solid")) 

plot_clams_soft <-
ggarrange(sc1, sc2, labels = c("A", "B"),
          common.legend = TRUE, legend = "right")
ggsave("2.d.9.plot_clams_soft.pdf", width = 15, height = 6)


# Sea Scallop  

sea_scallop <- production[which(production$Common.Name == 'SCALLOP, SEA'),]
sea_scallop$State <- factor(sea_scallop$State, levels = c("CONNECTICUT","RHODE ISLAND", "MASSACHUSETTS","NEW HAMPSHIRE","MAINE"))

sea1 <- ggplot(sea_scallop, aes(x=Year, y=Pounds/1000000, fill=State)) + 
  geom_bar(sea_scallop="stack", stat="identity") + xlab("Year") + ylab("Lbs (millions)") + 
  ggtitle("SEA SCALLOP LANDINGS") +
  scale_x_continuous(name="Year", breaks=c(2010:2020)) +
  scale_fill_manual(values=cbbPalette) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90)) +
  theme(axis.line = element_line(colour = "black", 
                                 size = 1, linetype = "solid")) 

sea2 <- ggplot(sea_scallop, aes(x=Year, y=Dollars/1000000, fill=State)) + 
  geom_bar(position="stack", stat="identity") + xlab("Year") + ylab("Dollars (millions)") + 
  ggtitle("SEA SCALLOP VALUE") +
  scale_x_continuous(name="Year", breaks=c(2010:2020)) +
  scale_fill_manual(values=cbbPalette) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90)) +
  theme(axis.line = element_line(colour = "black", 
                                 size = 1, linetype = "solid")) 

plot_scallop <-
ggarrange(sea1, sea2, labels = c("A", "B"),
          common.legend = TRUE, legend = "right")
ggsave("2.d.7.plot_scallop.pdf", width = 15, height = 6)


# Eastern Oyster 

oyster <- production[which(production$Common.Name == 'OYSTER, EASTERN'),]
oyster$State <- factor(oyster$State, levels = c("CONNECTICUT","RHODE ISLAND", "MASSACHUSETTS","NEW HAMPSHIRE","MAINE"))

o1 <- ggplot(oyster, aes(x=Year, y=Pounds/1000000, fill=State)) + 
  geom_bar(oyster="stack", stat="identity") + xlab("Year") + ylab("Lbs (millions)") + 
  ggtitle("EASTERN OYSTER LANDINGS") +
  scale_x_continuous(name="Year", breaks=c(2010:2020)) +
  scale_fill_manual(values=cbbPalette2) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90)) +
  theme(axis.line = element_line(colour = "black", 
                                 size = 1, linetype = "solid")) 

o2 <- ggplot(oyster, aes(x=Year, y=Dollars/1000000, fill=State)) + 
  geom_bar(position="stack", stat="identity") + xlab("Year") + ylab("Dollars (millions)") + 
  ggtitle("EASTERN OYSTER VALUE") +
  scale_x_continuous(name="Year", breaks=c(2010:2020)) +
  scale_fill_manual(values=cbbPalette2) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90)) +
  theme(axis.line = element_line(colour = "black", 
                                 size = 1, linetype = "solid")) 

plot_oyster <-
ggarrange(o1, o2, labels = c("A", "B"),
          common.legend = TRUE, legend = "right")
ggsave("2.d.14.plot_oyster.pdf", width = 15, height = 6)


# Seaweeds


seaweed <- production[which(production$NOAA.category == 'Seaweed'),]

seaweed$State <- factor(seaweed$State, levels = c("CONNECTICUT","RHODE ISLAND", "MASSACHUSETTS","NEW HAMPSHIRE","MAINE"))

seaweed1 <- ggplot(seaweed[which(seaweed$State == 'MAINE'),], aes(x=Year, y=Pounds/1000000, fill=State)) + 
  geom_bar(seaweed="stack", stat="identity") + xlab("Year") + ylab("Lbs (millions)") + 
  ggtitle("SEAWEED LANDINGS") +
  scale_x_continuous(name="Year", breaks=c(2010:2020)) +
  scale_fill_manual(values=cbbPalette) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90)) +
  theme(axis.line = element_line(colour = "black", 
                                 size = 1, linetype = "solid")) 

seaweed2 <- ggplot(seaweed[which(seaweed$State == 'MAINE'),], aes(x=Year, y=Dollars/1000000, fill=State)) + 
  geom_bar(position="stack", stat="identity") + xlab("Year") + ylab("Dollars (millions)") + 
  ggtitle("SEAWEED VALUE") +
  scale_x_continuous(name="Year", breaks=c(2010:2020)) +
  scale_fill_manual(values=cbbPalette) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90)) +
  theme(axis.line = element_line(colour = "black", 
                                 size = 1, linetype = "solid")) 

plot_seaweed <-
ggarrange(seaweed1, seaweed2, labels = c("A", "B"),
          common.legend = TRUE, legend = "right")
ggsave("2.d.15.plot_seaweed.pdf", width = 15, height = 6)


# Eel  

eel <- production[which(production$Common.Name == 'EEL, AMERICAN'),]

eel$State <- factor(eel$State, levels = c("CONNECTICUT","RHODE ISLAND", "MASSACHUSETTS","NEW HAMPSHIRE","MAINE"))

eel1 <- ggplot(eel, aes(x=Year, y=Pounds/1000000, fill=State)) + 
  geom_bar(eel="stack", stat="identity") + xlab("Year") + ylab("Lbs (millions)") + 
  ggtitle("AMERICAN EEL LANDINGS") +
  scale_x_continuous(name="Year", breaks=c(2010:2020)) +
  scale_fill_manual(values=cbbPalette) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90)) +
  theme(axis.line = element_line(colour = "black", 
                                 size = 1, linetype = "solid")) 

eel2 <- ggplot(eel, aes(x=Year, y=Dollars/1000000, fill=State)) + 
  geom_bar(position="stack", stat="identity") + xlab("Year") + ylab("Dollars (millions)") + 
  ggtitle("AMERICAN EEL VALUE") +
  scale_x_continuous(name="Year", breaks=c(2010:2020)) +
  scale_fill_manual(values=cbbPalette) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90)) +
  theme(axis.line = element_line(colour = "black", 
                                 size = 1, linetype = "solid")) 

plot_eel <-
  ggarrange(eel1, eel2, labels = c("A", "B"),
            common.legend = TRUE, legend = "right")
ggsave("2.d.11.plot_eel.pdf", width = 15, height = 6)

# Mussel

mussel <- production[which(production$Common.Name == 'MUSSEL, SEA'),]
mussel$State <- factor(mussel$State, levels = c("CONNECTICUT","RHODE ISLAND", "MASSACHUSETTS","NEW HAMPSHIRE","MAINE"))

m1 <- ggplot(mussel, aes(x=Year, y=Pounds/1000000, fill=State)) + 
  geom_bar(position="stack", stat="identity") + xlab("Year") + ylab("Lbs (millions)") + 
  ggtitle("BLUE MUSSEL LANDINGS") +
  scale_x_continuous(name="Year", breaks=c(2010:2020)) +
  scale_fill_manual(values=cbbPalette2) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90)) +
  theme(axis.line = element_line(colour = "black", 
                                 size = 1, linetype = "solid")) 

m2 <- ggplot(mussel, aes(x=Year, y=Dollars/1000000, fill=State)) + 
  geom_bar(position="stack", stat="identity") + xlab("Year") + ylab("Dollars (millions)") + 
  ggtitle("BLUE MUSSEL VALUE") +
  scale_x_continuous(name="Year", breaks=c(2010:2020)) +
  scale_fill_manual(values=cbbPalette2) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90)) +
  theme(axis.line = element_line(colour = "black", 
                                 size = 1, linetype = "solid")) 

plot_mussel <-
  ggarrange(m1, m2, labels = c("A", "B"),
            common.legend = TRUE, legend = "right")
ggsave("2.d.16.plot_mussel.pdf", width = 15, height = 6)




