# Seafood Production 
# 2.e. Harvest Value / Volume by Maine Region
# Version 1.0. March 1, 2022

### packages ####
library(ggplot2)

### Load data

production <- read.csv("/Users/jstoll/OneDrive - University of Maine System/Social_Coasts_Lab/seafood_systems/Data/Processed_Data/production.csv", 
                       stringsAsFactors = FALSE)

### plot data (by weight and value)

cbbPalette <- c("#7abfff", "#5f96c9", "#476f94", "#233749", "#e37a55")


# American lobster 
lobster <- production[which(production$species == 'Lobster American'),]
lobster$region <- factor(lobster$region, levels = c("Downeast","Southern", "Midcoast","Unidentified"))


a1 <- ggplot(lobster, aes(x=Year, y=weight/100000, fill=region)) + 
  geom_bar(position="stack", stat="identity") + xlab("Year") + ylab("Lbs") + 
  ggtitle("AMERICAN LOBSTER LANDINGS") +
  scale_x_continuous(name="Year", breaks=c(2010:2020)) +
  scale_fill_manual(values=cbbPalette) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90)) +
  theme(axis.line = element_line(colour = "black", 
                                 size = 1, linetype = "solid")) 

a2 <- ggplot(lobster, aes(x=Year, y=Dollars/100000, fill=region)) + 
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

# Atlantic herring 

herring <- production[which(production$species == 'Herring Atlantic'),]
herring$region <- factor(herring$region, levels = c("Downeast","Southern", "Midcoast","Unidentified"))


b1 <- ggplot(herring, aes(x=Year, y=weight/100000, fill=region)) + 
  geom_bar(position="stack", stat="identity") + xlab("Year") + ylab("Lbs") + 
  ggtitle("ATLANTIC HERRING LANDINGS") +
  scale_x_continuous(name="Year", breaks=c(2010:2020)) +
  scale_fill_manual(values=cbbPalette) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90)) +
  theme(axis.line = element_line(colour = "black", 
                                 size = 1, linetype = "solid")) 

b2 <- ggplot(herring, aes(x=Year, y=Dollars/100000, fill=region)) + 
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
ggsave("2.e.2.plot_herring.pdf", width = 15, height = 6)



# Clam Ocean Quahog

ocean_quahog <- production[which(production$species == 'Clam Ocean Quahog'),]
ocean_quahog$region <- factor(ocean_quahog$region, levels = c("Downeast","Southern", "Midcoast","Unidentified"))


c1 <- ggplot(ocean_quahog, aes(x=Year, y=weight/100000, fill=region)) + 
  geom_bar(position="stack", stat="identity") + xlab("Year") + ylab("Lbs") + 
  ggtitle("OCEAN QUAHOG LANDINGS") +
  scale_x_continuous(name="Year", breaks=c(2010:2020)) +
  scale_fill_manual(values=cbbPalette) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90)) +
  theme(axis.line = element_line(colour = "black", 
                                 size = 1, linetype = "solid")) 

c2 <- ggplot(ocean_quahog, aes(x=Year, y=Dollars/100000, fill=region)) + 
  geom_bar(position="stack", stat="identity") + xlab("Year") + ylab("Dollars (million)") + 
  ggtitle("OCEAN QUAHOG VALUE") +
  scale_x_continuous(name="Year", breaks=c(2010:2020)) +
  scale_fill_manual(values=cbbPalette) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90)) +
  theme(axis.line = element_line(colour = "black", 
                                 size = 1, linetype = "solid")) 

plot_ocean_quahog <- 
  ggarrange(c1, c2, labels = c("A", "B"),
            common.legend = TRUE, legend = "right")
ggsave("2.e.3.plot_ocean_quahog.pdf", width = 15, height = 6)



# Monkfish

Monkfish <- production[which(production$species == 'Monkfish'),]
Monkfish$region <- factor(Monkfish$region, levels = c("Downeast","Southern", "Midcoast","Unidentified"))


d1 <- ggplot(Monkfish, aes(x=Year, y=weight/100000, fill=region)) + 
  geom_bar(position="stack", stat="identity") + xlab("Year") + ylab("Lbs") + 
  ggtitle("MONKFISH LANDINGS") +
  scale_x_continuous(name="Year", breaks=c(2010:2020)) +
  scale_fill_manual(values=cbbPalette) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90)) +
  theme(axis.line = element_line(colour = "black", 
                                 size = 1, linetype = "solid")) 

d2 <- ggplot(Monkfish, aes(x=Year, y=Dollars/100000, fill=region)) + 
  geom_bar(position="stack", stat="identity") + xlab("Year") + ylab("Dollars (million)") + 
  ggtitle("MONKFISH VALUE") +
  scale_x_continuous(name="Year", breaks=c(2010:2020)) +
  scale_fill_manual(values=cbbPalette) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90)) +
  theme(axis.line = element_line(colour = "black", 
                                 size = 1, linetype = "solid")) 

plot_Monkfish <- 
  ggarrange(d1, d2, labels = c("A", "B"),
            common.legend = TRUE, legend = "right")
ggsave("2.e.4.plot_monkfish.pdf", width = 15, height = 6)


