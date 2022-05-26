# Seafood Production 
# 2.e. Harvest Value / Volume by Maine Region
# Version 1.0. March 1, 2022

### packages ####
library(ggplot2)

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

### Load data

production <- read.csv("/Users/jstoll/OneDrive - University of Maine System/Social_Coasts_Lab/seafood_systems/Data/Processed_Data/production.csv", 
                       stringsAsFactors = FALSE) # Note: this file is created in 2.a.Seafood_Production.R

### plot data (by weight and value)

cbbPalette <- c("#7abfff", "#5f96c9", "#476f94", "#233749", "#e37a55")


# American eel


eel <- production[which(production$species == 'Elver'),]
eel$region <- factor(eel$region, levels = c("Downeast","Southern", "Midcoast","Unidentified"))


p1 <- ggplot(eel, aes(x=Year, y=weight/1000000, fill=region)) + 
  geom_bar(position="stack", stat="identity") + xlab("Year") + ylab("Lbs (million)") + 
  ggtitle("AMERICAN EEL LANDINGS") +
  scale_x_continuous(name="Year", breaks=c(2010:2020)) +
  scale_fill_manual(values=cbbPalette) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90)) +
  theme(axis.line = element_line(colour = "black", 
                                 size = 1, linetype = "solid")) 

p2 <- ggplot(eel, aes(x=Year, y=Dollars/1000000, fill=region)) + 
  geom_bar(position="stack", stat="identity") + xlab("Year") + ylab("Dollars (million)") + 
  ggtitle("AMERICAN EEL VALUE") +
  scale_x_continuous(name="Year", breaks=c(2010:2020)) +
  scale_fill_manual(values=cbbPalette) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90)) +
  theme(axis.line = element_line(colour = "black", 
                                 size = 1, linetype = "solid")) 

plot_eel <- 
  ggarrange(p1, p2, labels = c("A", "B"),
            common.legend = TRUE, legend = "right")
ggsave("2.e.11.plot_eel.pdf", width = 15, height = 6)

# Seaweeds (unspecified)

Seaweed <- production[which(production$NOAA.category == 'Seaweed'),]
Seaweed$region <- factor(Seaweed$region, levels = c("Downeast","Southern", "Midcoast","Unidentified"))


p1 <- ggplot(Seaweed, aes(x=Year, y=weight/1000000, fill=region)) + 
  geom_bar(position="stack", stat="identity") + xlab("Year") + ylab("Lbs (million)") + 
  ggtitle("SEAWEED LANDINGS") +
  scale_x_continuous(name="Year", breaks=c(2010:2020)) +
  scale_fill_manual(values=cbbPalette) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90)) +
  theme(axis.line = element_line(colour = "black", 
                                 size = 1, linetype = "solid")) 

p2 <- ggplot(Seaweed, aes(x=Year, y=Dollars/1000000, fill=region)) + 
  geom_bar(position="stack", stat="identity") + xlab("Year") + ylab("Dollars (million)") + 
  ggtitle("SEAWEED VALUE") +
  scale_x_continuous(name="Year", breaks=c(2010:2020)) +
  scale_fill_manual(values=cbbPalette) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90)) +
  theme(axis.line = element_line(colour = "black", 
                                 size = 1, linetype = "solid")) 

plot_seaweed <- 
  ggarrange(p1, p2, labels = c("A", "B"),
            common.legend = TRUE, legend = "right")
ggsave("2.e.14.plot_seaweed.pdf", width = 15, height = 6)

# Eastern oyster


oysters <- production[which(production$species == 'Oyster Eastern / American'),]
oysters$region <- factor(oysters$region, levels = c("Downeast","Southern", "Midcoast","Unidentified"))


p1 <- ggplot(oysters, aes(x=Year, y=weight/1000000, fill=region)) + 
  geom_bar(position="stack", stat="identity") + xlab("Year") + ylab("Lbs (million)") + 
  ggtitle("EASTERN OYSTER LANDINGS") +
  scale_x_continuous(name="Year", breaks=c(2010:2020)) +
  scale_fill_manual(values=cbbPalette) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90)) +
  theme(axis.line = element_line(colour = "black", 
                                 size = 1, linetype = "solid")) 

p2 <- ggplot(oysters, aes(x=Year, y=Dollars/1000000, fill=region)) + 
  geom_bar(position="stack", stat="identity") + xlab("Year") + ylab("Dollars (million)") + 
  ggtitle("EASTERN OYSTER VALUE") +
  scale_x_continuous(name="Year", breaks=c(2010:2020)) +
  scale_fill_manual(values=cbbPalette) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90)) +
  theme(axis.line = element_line(colour = "black", 
                                 size = 1, linetype = "solid")) 

plot_oysters <- 
  ggarrange(p1, p2, labels = c("A", "B"),
            common.legend = TRUE, legend = "right")
ggsave("2.e.15.plot_oysters.pdf", width = 15, height = 6)


# Sea scallop


scallop <- production[which(production$species == 'Scallop Sea'),]
scallop$region <- factor(scallop$region, levels = c("Downeast","Southern", "Midcoast","Unidentified"))


p1 <- ggplot(scallop, aes(x=Year, y=weight/1000000, fill=region)) + 
  geom_bar(position="stack", stat="identity") + xlab("Year") + ylab("Lbs (million)") + 
  ggtitle("SEA SCALLOP LANDINGS") +
  scale_x_continuous(name="Year", breaks=c(2010:2020)) +
  scale_fill_manual(values=cbbPalette) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90)) +
  theme(axis.line = element_line(colour = "black", 
                                 size = 1, linetype = "solid")) 

p2 <- ggplot(scallop, aes(x=Year, y=Dollars/1000000, fill=region)) + 
  geom_bar(position="stack", stat="identity") + xlab("Year") + ylab("Dollars (million)") + 
  ggtitle("SEA SCALLOP VALUE") +
  scale_x_continuous(name="Year", breaks=c(2010:2020)) +
  scale_fill_manual(values=cbbPalette) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90)) +
  theme(axis.line = element_line(colour = "black", 
                                 size = 1, linetype = "solid")) 

plot_scallop <- 
  ggarrange(p1, p2, labels = c("A", "B"),
            common.legend = TRUE, legend = "right")
ggsave("2.e.7.plot_scallop.pdf", width = 15, height = 6)

# Soft shell clam

clam <- production[which(production$species == 'Clam Soft'),]
clam$region <- factor(clam$region, levels = c("Downeast","Southern", "Midcoast","Unidentified"))

p1 <- ggplot(clam, aes(x=Year, y=weight/1000000, fill=region)) + 
  geom_bar(position="stack", stat="identity") + xlab("Year") + ylab("Lbs (million)") + 
  ggtitle("SOFT-SHELL CLAM LANDINGS") +
  scale_x_continuous(name="Year", breaks=c(2010:2020)) +
  scale_fill_manual(values=cbbPalette) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90)) +
  theme(axis.line = element_line(colour = "black", 
                                 size = 1, linetype = "solid")) 

p2 <- ggplot(clam, aes(x=Year, y=Dollars/1000000, fill=region)) + 
  geom_bar(position="stack", stat="identity") + xlab("Year") + ylab("Dollars (million)") + 
  ggtitle("SOFT-SHELL CLAM VALUE") +
  scale_x_continuous(name="Year", breaks=c(2010:2020)) +
  scale_fill_manual(values=cbbPalette) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90)) +
  theme(axis.line = element_line(colour = "black", 
                                 size = 1, linetype = "solid")) 

plot_clam <- 
  ggarrange(p1, p2, labels = c("A", "B"),
            common.legend = TRUE, legend = "right")
ggsave("2.e.9.plot_softclam.pdf", width = 15, height = 6)

# Quahog (Northern)


northern_quahog <- production[which(production$species == 'Clam Northern Quahog / Hard'),]
northern_quahog$region <- factor(northern_quahog$region, levels = c("Downeast","Southern", "Midcoast","Unidentified"))

p1 <- ggplot(northern_quahog, aes(x=Year, y=weight/1000000, fill=region)) + 
  geom_bar(position="stack", stat="identity") + xlab("Year") + ylab("Lbs (million)") + 
  ggtitle("NORTHERN QUAHOG LANDINGS") +
  scale_x_continuous(name="Year", breaks=c(2010:2020)) +
  scale_fill_manual(values=cbbPalette) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90)) +
  theme(axis.line = element_line(colour = "black", 
                                 size = 1, linetype = "solid")) 

p2 <- ggplot(northern_quahog, aes(x=Year, y=Dollars/1000000, fill=region)) + 
  geom_bar(position="stack", stat="identity") + xlab("Year") + ylab("Dollars (million)") + 
  ggtitle("NORTHERN QUAHOG VALUE") +
  scale_x_continuous(name="Year", breaks=c(2010:2020)) +
  scale_fill_manual(values=cbbPalette) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90)) +
  theme(axis.line = element_line(colour = "black", 
                                 size = 1, linetype = "solid")) 

plot_northern_quahog<- 
  ggarrange(p1, p2, labels = c("A", "B"),
            common.legend = TRUE, legend = "right")
ggsave("2.e.16.plot_quahog_northern.pdf", width = 15, height = 6)


# Clam Ocean Quahog

ocean_quahog <- production[which(production$species == 'Clam Ocean Quahog'),]
ocean_quahog$region <- factor(ocean_quahog$region, levels = c("Downeast","Southern", "Midcoast","Unidentified"))


p1 <- ggplot(ocean_quahog, aes(x=Year, y=weight/1000000, fill=region)) + 
  geom_bar(position="stack", stat="identity") + xlab("Year") + ylab("Lbs (million)") + 
  ggtitle("OCEAN QUAHOG LANDINGS") +
  scale_x_continuous(name="Year", breaks=c(2010:2020)) +
  scale_fill_manual(values=cbbPalette) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90)) +
  theme(axis.line = element_line(colour = "black", 
                                 size = 1, linetype = "solid")) 

p2 <- ggplot(ocean_quahog, aes(x=Year, y=Dollars/1000000, fill=region)) + 
  geom_bar(position="stack", stat="identity") + xlab("Year") + ylab("Dollars (million)") + 
  ggtitle("OCEAN QUAHOG VALUE") +
  scale_x_continuous(name="Year", breaks=c(2010:2020)) +
  scale_fill_manual(values=cbbPalette) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90)) +
  theme(axis.line = element_line(colour = "black", 
                                 size = 1, linetype = "solid")) 

plot_ocean_quahog <- 
  ggarrange(p1, p2, labels = c("A", "B"),
            common.legend = TRUE, legend = "right")
ggsave("2.e.17.plot_ocean_quahog.pdf", width = 15, height = 6)


# Atlantic cod 


cod <- production[which(production$species == 'Cod Atlantic'),]
cod$region <- factor(cod$region, levels = c("Downeast","Southern", "Midcoast","Unidentified"))


p1 <- ggplot(cod, aes(x=Year, y=weight/1000000, fill=region)) + 
  geom_bar(position="stack", stat="identity") + xlab("Year") + ylab("Lbs (million)") + 
  ggtitle("ATLANTIC COD LANDINGS") +
  scale_x_continuous(name="Year", breaks=c(2010:2020)) +
  scale_fill_manual(values=cbbPalette) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90)) +
  theme(axis.line = element_line(colour = "black", 
                                 size = 1, linetype = "solid")) 

p2 <- ggplot(cod, aes(x=Year, y=Dollars/1000000, fill=region)) + 
  geom_bar(position="stack", stat="identity") + xlab("Year") + ylab("Dollars (million)") + 
  ggtitle("ATLANTIC COD VALUE") +
  scale_x_continuous(name="Year", breaks=c(2010:2020)) +
  scale_fill_manual(values=cbbPalette) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90)) +
  theme(axis.line = element_line(colour = "black", 
                                 size = 1, linetype = "solid")) 

plot_cod <- 
  ggarrange(p1, p2, labels = c("A", "B"),
            common.legend = TRUE, legend = "right")
ggsave("2.e.2.plot_cod.pdf", width = 15, height = 6)


# Atlantic halibut

halibut <- production[which(production$species == 'Halibut Atlantic'),]
halibut$region <- factor(halibut$region, levels = c("Downeast","Southern", "Midcoast","Unidentified"))


p1 <- ggplot(halibut, aes(x=Year, y=weight/1000000, fill=region)) + 
  geom_bar(position="stack", stat="identity") + xlab("Year") + ylab("Lbs (million)") + 
  ggtitle("ATLANTIC HALIBUT LANDINGS") +
  scale_x_continuous(name="Year", breaks=c(2010:2020)) +
  scale_fill_manual(values=cbbPalette) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90)) +
  theme(axis.line = element_line(colour = "black", 
                                 size = 1, linetype = "solid")) 

p2 <- ggplot(halibut, aes(x=Year, y=Dollars/1000000, fill=region)) + 
  geom_bar(position="stack", stat="identity") + xlab("Year") + ylab("Dollars (million)") + 
  ggtitle("ATLANTIC HALIBUT VALUE") +
  scale_x_continuous(name="Year", breaks=c(2010:2020)) +
  scale_fill_manual(values=cbbPalette) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90)) +
  theme(axis.line = element_line(colour = "black", 
                                 size = 1, linetype = "solid")) 

plot_halibut <- 
  ggarrange(p1, p2, labels = c("A", "B"),
            common.legend = TRUE, legend = "right")
ggsave("2.e.3.plot_halibut.pdf", width = 15, height = 6)


# Pollock


pollock <- production[which(production$species == 'Pollock'),]
pollock$region <- factor(pollock$region, levels = c("Downeast","Southern", "Midcoast","Unidentified"))


p1 <- ggplot(pollock, aes(x=Year, y=weight/1000000, fill=region)) + 
  geom_bar(position="stack", stat="identity") + xlab("Year") + ylab("Lbs (million)") + 
  ggtitle("POLLOCK LANDINGS") +
  scale_x_continuous(name="Year", breaks=c(2010:2020)) +
  scale_fill_manual(values=cbbPalette) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90)) +
  theme(axis.line = element_line(colour = "black", 
                                 size = 1, linetype = "solid")) 

p2 <- ggplot(pollock, aes(x=Year, y=Dollars/1000000, fill=region)) + 
  geom_bar(position="stack", stat="identity") + xlab("Year") + ylab("Dollars (million)") + 
  ggtitle("POLLOCK VALUE") +
  scale_x_continuous(name="Year", breaks=c(2010:2020)) +
  scale_fill_manual(values=cbbPalette) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90)) +
  theme(axis.line = element_line(colour = "black", 
                                 size = 1, linetype = "solid")) 

plot_pollock <- 
  ggarrange(p1, p2, labels = c("A", "B"),
            common.legend = TRUE, legend = "right")
ggsave("2.e.6.plot_pollock.pdf", width = 15, height = 6)


# Haddock


Haddock <- production[which(production$species == 'Haddock'),]
Haddock$region <- factor(Haddock$region, levels = c("Downeast","Southern", "Midcoast","Unidentified"))


p1 <- ggplot(Haddock, aes(x=Year, y=weight/1000000, fill=region)) + 
  geom_bar(position="stack", stat="identity") + xlab("Year") + ylab("Lbs (million)") + 
  ggtitle("HADDOCK LANDINGS") +
  scale_x_continuous(name="Year", breaks=c(2010:2020)) +
  scale_fill_manual(values=cbbPalette) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90)) +
  theme(axis.line = element_line(colour = "black", 
                                 size = 1, linetype = "solid")) 

p2 <- ggplot(Haddock, aes(x=Year, y=Dollars/1000000, fill=region)) + 
  geom_bar(position="stack", stat="identity") + xlab("Year") + ylab("Dollars (million)") + 
  ggtitle("HADDOCK VALUE") +
  scale_x_continuous(name="Year", breaks=c(2010:2020)) +
  scale_fill_manual(values=cbbPalette) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90)) +
  theme(axis.line = element_line(colour = "black", 
                                 size = 1, linetype = "solid")) 

plot_Haddock <- 
  ggarrange(p1, p2, labels = c("A", "B"),
            common.legend = TRUE, legend = "right")
ggsave("2.e.12.plot_Haddock.pdf", width = 15, height = 6)


# Monkfish (Goosefish) 


Monkfish <- production[which(production$species == 'Monkfish'),]
Monkfish$region <- factor(Monkfish$region, levels = c("Downeast","Southern", "Midcoast","Unidentified"))


p1 <- ggplot(Monkfish, aes(x=Year, y=weight/1000000, fill=region)) + 
  geom_bar(position="stack", stat="identity") + xlab("Year") + ylab("Lbs (million)") + 
  ggtitle("MONKFISH LANDINGS") +
  scale_x_continuous(name="Year", breaks=c(2010:2020)) +
  scale_fill_manual(values=cbbPalette) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90)) +
  theme(axis.line = element_line(colour = "black", 
                                 size = 1, linetype = "solid")) 

p2 <- ggplot(Monkfish, aes(x=Year, y=Dollars/1000000, fill=region)) + 
  geom_bar(position="stack", stat="identity") + xlab("Year") + ylab("Dollars (million)") + 
  ggtitle("MONKFISH VALUE") +
  scale_x_continuous(name="Year", breaks=c(2010:2020)) +
  scale_fill_manual(values=cbbPalette) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90)) +
  theme(axis.line = element_line(colour = "black", 
                                 size = 1, linetype = "solid")) 

plot_Monkfish <- 
  ggarrange(p1, p2, labels = c("A", "B"),
            common.legend = TRUE, legend = "right")
ggsave("2.e.13.plot_monkfish.pdf", width = 15, height = 6)


# Bluefin tuna 

Bluefin <- production[which(production$species == 'Tuna Atlantic Bluefin'),]
Bluefin$region <- factor(Monkfish$region, levels = c("Downeast","Southern", "Midcoast","Unidentified"))


p1 <- ggplot(Bluefin, aes(x=Year, y=weight/1000000, fill=region)) + 
  geom_bar(position="stack", stat="identity") + xlab("Year") + ylab("Lbs (million)") + 
  ggtitle("ATLANTIC BLUEFIN TUNA LANDINGS") +
  scale_x_continuous(name="Year", breaks=c(2010:2020)) +
  scale_fill_manual(values=cbbPalette) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90)) +
  theme(axis.line = element_line(colour = "black", 
                                 size = 1, linetype = "solid")) 

p2 <- ggplot(Bluefin, aes(x=Year, y=Dollars/1000000, fill=region)) + 
  geom_bar(position="stack", stat="identity") + xlab("Year") + ylab("Dollars (million)") + 
  ggtitle("ATLANTIC BLUEFIN TUNA VALUE") +
  scale_x_continuous(name="Year", breaks=c(2010:2020)) +
  scale_fill_manual(values=cbbPalette) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90)) +
  theme(axis.line = element_line(colour = "black", 
                                 size = 1, linetype = "solid")) 

plot_Bluefin <- 
  ggarrange(p1, p2, labels = c("A", "B"),
            common.legend = TRUE, legend = "right")
ggsave("2.e.8.plot_bluefin.pdf", width = 15, height = 6)



# American lobster 
lobster <- production[which(production$species == 'Lobster American'),]
lobster$region <- factor(lobster$region, levels = c("Downeast","Southern", "Midcoast","Unidentified"))


a1 <- ggplot(lobster, aes(x=Year, y=weight/1000000, fill=region)) + 
  geom_bar(position="stack", stat="identity") + xlab("Year") + ylab("Lbs (million)") + 
  ggtitle("AMERICAN LOBSTER LANDINGS") +
  scale_x_continuous(name="Year", breaks=c(2010:2020)) +
  scale_fill_manual(values=cbbPalette) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90)) +
  theme(axis.line = element_line(colour = "black", 
                                 size = 1, linetype = "solid")) 

a2 <- ggplot(lobster, aes(x=Year, y=Dollars/1000000, fill=region)) + 
  geom_bar(position="stack", stat="identity") + xlab("Year") + ylab("Dollars (million)") + 
  ggtitle("AMERICAN LOBSTER VALUE") +
  scale_x_continuous(name="Year", breaks=c(2010:2020)) +
  scale_fill_manual(values=cbbPalette) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90)) +
  theme(axis.line = element_line(colour = "black", 
                                 size = 1, linetype = "solid")) 

plot_lobster <- 
  ggarrange(a1, a2, labels = c("A", "B"),
            common.legend = TRUE, legend = "right")
ggsave("2.e.1.plot_lobsters.pdf", width = 15, height = 6)

# Jonah crab


jonah <- production[which(production$species == 'Crab Jonah'),]
jonah$region <- factor(jonah$region, levels = c("Downeast","Southern", "Midcoast","Unidentified"))


b1 <- ggplot(jonah, aes(x=Year, y=weight/1000000, fill=region)) + 
  geom_bar(position="stack", stat="identity") + xlab("Year") + ylab("Lbs (million)") + 
  ggtitle("JONAH CRAB LANDINGS") +
  scale_x_continuous(name="Year", breaks=c(2010:2020)) +
  scale_fill_manual(values=cbbPalette) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90)) +
  theme(axis.line = element_line(colour = "black", 
                                 size = 1, linetype = "solid")) 

b2 <- ggplot(jonah, aes(x=Year, y=Dollars/1000000, fill=region)) + 
  geom_bar(position="stack", stat="identity") + xlab("Year") + ylab("Dollars (million)") + 
  ggtitle("JONAH CRAB VALUE") +
  scale_x_continuous(name="Year", breaks=c(2010:2020)) +
  scale_fill_manual(values=cbbPalette) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90)) +
  theme(axis.line = element_line(colour = "black", 
                                 size = 1, linetype = "solid")) 

plot_jonah <- 
  ggarrange(b1, b2, labels = c("A", "B"),
            common.legend = TRUE, legend = "right")
ggsave("2.e.10.plot_jonah.pdf", width = 15, height = 6)


# Atlantic herring 

herring <- production[which(production$species == 'Herring Atlantic'),]
herring$region <- factor(herring$region, levels = c("Downeast","Southern", "Midcoast","Unidentified"))


b1 <- ggplot(herring, aes(x=Year, y=weight/1000000, fill=region)) + 
  geom_bar(position="stack", stat="identity") + xlab("Year") + ylab("Lbs (million)") + 
  ggtitle("ATLANTIC HERRING LANDINGS") +
  scale_x_continuous(name="Year", breaks=c(2010:2020)) +
  scale_fill_manual(values=cbbPalette) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90)) +
  theme(axis.line = element_line(colour = "black", 
                                 size = 1, linetype = "solid")) 

b2 <- ggplot(herring, aes(x=Year, y=Dollars/1000000, fill=region)) + 
  geom_bar(position="stack", stat="identity") + xlab("Year") + ylab("Dollars (million)") + 
  ggtitle("ATLANTIC HERRING VALUE") +
  scale_x_continuous(name="Year", breaks=c(2010:2020)) +
  scale_fill_manual(values=cbbPalette) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90)) +
  theme(axis.line = element_line(colour = "black", 
                                 size = 1, linetype = "solid")) 

plot_herring <- 
  ggarrange(b1, b2, labels = c("A", "B"),
            common.legend = TRUE, legend = "right")
ggsave("2.e.4.plot_herring.pdf", width = 15, height = 6)


# Menhadens


menhadens <- production[which(production$species == 'Menhaden Atlantic'),]
menhadens$region <- factor(menhadens$region, levels = c("Downeast","Southern", "Midcoast","Unidentified"))


b1 <- ggplot(menhadens, aes(x=Year, y=weight/1000000, fill=region)) + 
  geom_bar(position="stack", stat="identity") + xlab("Year") + ylab("Lbs (million)") + 
  ggtitle("MENHADENS LANDINGS") +
  scale_x_continuous(name="Year", breaks=c(2010:2020)) +
  scale_fill_manual(values=cbbPalette) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90)) +
  theme(axis.line = element_line(colour = "black", 
                                 size = 1, linetype = "solid")) 

b2 <- ggplot(menhadens, aes(x=Year, y=Dollars/1000000, fill=region)) + 
  geom_bar(position="stack", stat="identity") + xlab("Year") + ylab("Dollars (million)") + 
  ggtitle("MENHADENS VALUE") +
  scale_x_continuous(name="Year", breaks=c(2010:2020)) +
  scale_fill_manual(values=cbbPalette) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90)) +
  theme(axis.line = element_line(colour = "black", 
                                 size = 1, linetype = "solid")) 

plot_menhadens <- 
  ggarrange(b1, b2, labels = c("A", "B"),
            common.legend = TRUE, legend = "right")
ggsave("2.e.5.plot_menhadens.pdf", width = 15, height = 6)


# Menhadens


mussel <- production[which(production$species == 'Mussel Blue Sea'),]
mussel$region <- factor(mussel$region, levels = c("Downeast","Southern", "Midcoast","Unidentified"))


m1 <- ggplot(mussel, aes(x=Year, y=weight/1000000, fill=region)) + 
  geom_bar(position="stack", stat="identity") + xlab("Year") + ylab("Lbs (million)") + 
  ggtitle("BLUE MUSSEL LANDINGS") +
  scale_x_continuous(name="Year", breaks=c(2010:2020)) +
  scale_fill_manual(values=cbbPalette) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90)) +
  theme(axis.line = element_line(colour = "black", 
                                 size = 1, linetype = "solid")) 

m2 <- ggplot(mussel, aes(x=Year, y=Dollars/1000000, fill=region)) + 
  geom_bar(position="stack", stat="identity") + xlab("Year") + ylab("Dollars (million)") + 
  ggtitle("BLUE MUSSEL VALUE") +
  scale_x_continuous(name="Year", breaks=c(2010:2020)) +
  scale_fill_manual(values=cbbPalette) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90)) +
  theme(axis.line = element_line(colour = "black", 
                                 size = 1, linetype = "solid")) 

plot_mussel <- 
  ggarrange(m1, m2, labels = c("A", "B"),
            common.legend = TRUE, legend = "right")
ggsave("2.e.18.plot_mussels.pdf", width = 15, height = 6)



