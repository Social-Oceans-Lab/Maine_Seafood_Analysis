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


a1 <- ggplot(lobster, aes(x=Year, y=weight, fill=region)) + 
  geom_bar(position="stack", stat="identity") + xlab("Year") + ylab("Lbs") + 
  ggtitle("AMERICAN LOBSTER LANDINGS") +
  scale_x_continuous(name="Year", breaks=c(2010:2020)) +
  scale_fill_manual(values=cbbPalette) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90)) +
  theme(axis.line = element_line(colour = "black", 
                                 size = 1, linetype = "solid")) 

a2 <- ggplot(lobster, aes(x=Year, y=Dollars, fill=region)) + 
  geom_bar(position="stack", stat="identity") + xlab("Year") + ylab("Lbs") + 
  ggtitle("AMERICAN LOBSTER LANDINGS") +
  scale_x_continuous(name="Year", breaks=c(2010:2020)) +
  scale_fill_manual(values=cbbPalette) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90)) +
  theme(axis.line = element_line(colour = "black", 
                                 size = 1, linetype = "solid")) 