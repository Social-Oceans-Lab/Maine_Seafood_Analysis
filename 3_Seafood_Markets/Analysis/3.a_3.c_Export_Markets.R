# Export Markets
# 3a - 3c
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

### packages ####
library(ggplot2)
library(ggpubr)
library(dplyr)


#### data loading ####

exportcountries <- read.csv("/Users/jstoll/OneDrive - University of Maine System/Social_Coasts_Lab/seafood_systems/Data/Processed_Data/USATradeData4DigitExportCountries2010_2021.csv", stringsAsFactors = FALSE)
# https://usatrade.census.gov/index.php
#### Fill commodities column labels ####

comlabels <- unique(exportcountries$Commodity)[c(1,3:14)] ##2nd value in list appears blank therefore removed

comcolumn <- function(x){
    column<-NULL
    for (i in 1:length(x)){
    column<-c(column,rep(x[i],241))
    }
    return(column)
}
exportcountries$Commodity<-comcolumn(comlabels)

####Turn Multiple columns for each year into one column variable titled Year

junk1<-rep(exportcountries$Commodity,12)
junk2<-rep(exportcountries$Country,12)
junk3<-c(rep(2010,3133),rep(2011,3133),rep(2012,3133),rep(2013,3133),rep(2014,3133),rep(2015,3133),rep(2016,3133),rep(2017,3133),rep(2018,3133),rep(2019,3133),rep(2020,3133),rep(2021,3133))
junk4<-rbind(matrix(exportcountries[,3],ncol=1),matrix(exportcountries[,4],ncol=1),matrix(exportcountries[,5],ncol=1),matrix(exportcountries[,6],ncol=1),matrix(exportcountries[,7],ncol=1),matrix(exportcountries[,8],ncol=1),
matrix(exportcountries[,9],ncol=1),matrix(exportcountries[,10],ncol=1),matrix(exportcountries[,11],ncol=1),matrix(exportcountries[,12],ncol=1),matrix(exportcountries[,13],ncol=1),matrix(exportcountries[,14],ncol=1))
junkcomb<-cbind(junk1,junk2,junk3,junk4)

exportcountries <- data.frame(junkcomb, stringsAsFactors = TRUE)
names(exportcountries)<-c("Commodity","Country","Year","ExportValue")
exportcountries$Year <- as.numeric(as.character(exportcountries$Year))

exportcountries$ExportValue <- as.numeric(gsub(",","",exportcountries$ExportValue))
  

#### Stacked ten-year world exports by commodity group (2011 to 2020 aligns with initial landings data, but could do 2012 to 2021 as export data will always be a year ahead of landings data) ####

#keeping 2011-2020

exportcountries <- exportcountries[which(exportcountries$Year >= 2010), ]


### Create plots 

cbbPalette <- c("#7abfff", "#5f96c9", "#476f94", "#233749", "#e37a55")

# World exports by commodity group 

worldsub_all <- exportcountries[which(exportcountries$Country == 'World Total'),]
worldsub <- worldsub_all[which(worldsub_all$Commodity != '03 Fish, Crustaceans & Aquatic Invertebrates'),]

## Stacked bar plot of world totals stacking 4-digit and 6-digit commodities 

p0 <- ggplot(worldsub, aes(x=Year, y=ExportValue/1000000, fill=Commodity)) + 
  geom_bar(position="stack", stat="identity") + xlab("Year") + ylab("Dollars (million)") + 
  ggtitle("Total Maine Seafood Exports (4-digit and 6-digit HS commodities)") +
  scale_x_continuous(name="Year", breaks=c(2010:2021)) +
  #scale_fill_manual(values=cbbPalette) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90)) +
  theme(axis.line = element_line(colour = "black", 
                                 size = 1, linetype = "solid")) 

ggsave("3.a.seafood_markets.pdf", width = 15, height = 6)

## Bar plot of world totals based on specific 4-digit and 6-digit commodities 

p1 <- ggplot(worldsub[which(worldsub$Commodity == '0301 Fish, Live'),], aes(x=Year, y=ExportValue/1000000, fill=Commodity)) + 
  geom_bar(position="stack", stat="identity") + xlab("Year") + ylab("Dollars (million)") + 
  ggtitle("Export of 0301 Fish, Live") +
  geom_text(aes(label = round(ExportValue/1000000, 2)), vjust = -0.2)+
  scale_x_continuous(name="Year", breaks=c(2010:2021)) +
  scale_fill_manual(values="#5f96c9") +
  theme_minimal() +
  theme(legend.position="none") +
  theme(axis.text.x = element_text(angle = 90)) +
  theme(axis.line = element_line(colour = "black", 
                                 size = 1, linetype = "solid")) 

ggsave("3.a.1.seafood_markets.pdf", width = 15, height = 6)

p2 <- ggplot(worldsub[which(worldsub$Commodity == '0302 Fish, Fresh Or Chilled (no Fillets Or Other Meat)'),], aes(x=Year, y=ExportValue/1000000, fill=Commodity)) + 
  geom_bar(position="stack", stat="identity") + xlab("Year") + ylab("Dollars (million)") + 
  ggtitle("Export of 0302 Fish, Fresh Or Chilled (no Fillets Or Other Meat)") +
  geom_text(aes(label = round(ExportValue/1000000, 2)), vjust = -0.2)+
  scale_x_continuous(name="Year", breaks=c(2010:2021)) +
  scale_fill_manual(values="#5f96c9") +
  theme_minimal() +
  theme(legend.position="none") +
  theme(axis.text.x = element_text(angle = 90)) +
  theme(axis.line = element_line(colour = "black", 
                                 size = 1, linetype = "solid")) 

ggsave("3.a.2.seafood_markets.pdf", width = 15, height = 6)

p3 <- ggplot(worldsub[which(worldsub$Commodity == '0303 Fish, Frozen (no Fish Fillets Or Other Fish Meat)'),], aes(x=Year, y=ExportValue/1000000, fill=Commodity)) + 
  geom_bar(position="stack", stat="identity") + xlab("Year") + ylab("Dollars (million)") + 
  ggtitle("Export of 0303 Fish, Frozen (no Fish Fillets Or Other Fish Meat)") +
  geom_text(aes(label = round(ExportValue/1000000, 2)), vjust = -0.2)+
  scale_x_continuous(name="Year", breaks=c(2010:2021)) +
  scale_fill_manual(values="#5f96c9") +
  theme_minimal() +
  theme(legend.position="none") +
  theme(axis.text.x = element_text(angle = 90)) +
  theme(axis.line = element_line(colour = "black", 
                                 size = 1, linetype = "solid")) 

ggsave("3.a.3.seafood_markets.pdf", width = 15, height = 6)

p4 <- ggplot(worldsub[which(worldsub$Commodity == '0304 Fish Fillets & Oth Fish Meat, Fresh, Chill Or Froz'),], aes(x=Year, y=ExportValue/1000000, fill=Commodity)) + 
  geom_bar(position="stack", stat="identity") + xlab("Year") + ylab("Dollars (million)") + 
  ggtitle("Export of 0304 Fish Fillets & Oth Fish Meat, Fresh, Chill Or Froz") +
  geom_text(aes(label = round(ExportValue/1000000, 2)), vjust = -0.2)+
  scale_x_continuous(name="Year", breaks=c(2010:2021)) +
  scale_fill_manual(values="#5f96c9") +
  theme_minimal() +
  theme(legend.position="none") +
  theme(axis.text.x = element_text(angle = 90)) +
  theme(axis.line = element_line(colour = "black", 
                                 size = 1, linetype = "solid")) 

ggsave("3.a.4.seafood_markets.pdf", width = 15, height = 6)

p5 <- ggplot(worldsub[which(worldsub$Commodity == '0305 Fish, Dried, Salted Or In Brine; Smoked Fish'),], aes(x=Year, y=ExportValue/1000000, fill=Commodity)) + 
  geom_bar(position="stack", stat="identity") + xlab("Year") + ylab("Dollars (million)") + 
  ggtitle("Export of 0305 Fish, Dried, Salted Or In Brine; Smoked Fish") +
  geom_text(aes(label = round(ExportValue/1000000, 2)), vjust = -0.2)+
  scale_x_continuous(name="Year", breaks=c(2010:2021)) +
  scale_fill_manual(values="#5f96c9") +
  theme_minimal() +
  theme(legend.position="none") +
  theme(axis.text.x = element_text(angle = 90)) +
  theme(axis.line = element_line(colour = "black", 
                                 size = 1, linetype = "solid")) 

ggsave("3.a.5.seafood_markets.pdf", width = 15, height = 6)

p6 <- ggplot(worldsub[which(worldsub$Commodity == '0306 Crustaceans'),], aes(x=Year, y=ExportValue/1000000, fill=Commodity)) + 
  geom_bar(position="stack", stat="identity") + xlab("Year") + ylab("Dollars (million)") + 
  ggtitle("Export of 0306 Crustaceans") +
  geom_text(aes(label = round(ExportValue/1000000, 2)), vjust = -0.2)+
  scale_x_continuous(name="Year", breaks=c(2010:2021)) +
  scale_fill_manual(values="#5f96c9") +
  theme_minimal() +
  theme(legend.position="none") +
  theme(axis.text.x = element_text(angle = 90)) +
  theme(axis.line = element_line(colour = "black", 
                                 size = 1, linetype = "solid")) 

ggsave("3.a.6.seafood_markets.pdf", width = 15, height = 6)

p7 <- ggplot(worldsub[which(worldsub$Commodity == '0307 Molluscs'),], aes(x=Year, y=ExportValue/1000000, fill=Commodity)) + 
  geom_bar(position="stack", stat="identity") + xlab("Year") + ylab("Dollars (million)") + 
  ggtitle("Export of 0307 Molluscs") +
  geom_text(aes(label = round(ExportValue/1000000, 2)), vjust = -0.2)+
  scale_x_continuous(name="Year", breaks=c(2010:2021)) +
  scale_fill_manual(values="#5f96c9") +
  theme_minimal() +
  theme(legend.position="none") +
  theme(axis.text.x = element_text(angle = 90)) +
  theme(axis.line = element_line(colour = "black", 
                                 size = 1, linetype = "solid")) 

ggsave("3.a.7.seafood_markets.pdf", width = 15, height = 6)

p8 <- ggplot(worldsub[which(worldsub$Commodity == '0308 Aquatic Invertebrates, Nesoi'),], aes(x=Year, y=ExportValue/1000000, fill=Commodity)) + 
  geom_bar(position="stack", stat="identity") + xlab("Year") + ylab("Dollars (million)") + 
  ggtitle("Export of 0308 Aquatic Invertebrates, Nesoi") +
  geom_text(aes(label = round(ExportValue/1000000, 2)), vjust = -0.2)+
  scale_x_continuous(name="Year", breaks=c(2010:2021)) +
  scale_fill_manual(values="#5f96c9") +
  theme_minimal() +
  theme(legend.position="none") +
  theme(axis.text.x = element_text(angle = 90)) +
  theme(axis.line = element_line(colour = "black", 
                                 size = 1, linetype = "solid")) 

ggsave("3.a.8.seafood_markets.pdf", width = 15, height = 6)

# Note: No export of 0309 Flours, Meals And Pellets Of Fish, Crustaceans Etc

p9 <- ggplot(worldsub[which(worldsub$Commodity == '121221 Seaweeds And Other Algae Fit For Human Consumption'),], aes(x=Year, y=ExportValue/1000000, fill=Commodity)) + 
  geom_bar(position="stack", stat="identity") + xlab("Year") + ylab("Dollars (million)") + 
  ggtitle("Export of 121221 Seaweeds And Other Algae Fit For Human Consumption") +
  geom_text(aes(label = round(ExportValue/1000000, 2)), vjust = -0.2)+
  scale_x_continuous(name="Year", breaks=c(2010:2021)) +
  scale_fill_manual(values="#5f96c9") +
  theme_minimal() +
  theme(legend.position="none") +
  theme(axis.text.x = element_text(angle = 90)) +
  theme(axis.line = element_line(colour = "black", 
                                 size = 1, linetype = "solid")) 

ggsave("3.a.9.seafood_markets.pdf", width = 15, height = 6)


p10 <- ggplot(worldsub[which(worldsub$Commodity == '121229 Seaweeds And Other Algae, Nesoi'),], aes(x=Year, y=ExportValue/1000000, fill=Commodity)) + 
  geom_bar(position="stack", stat="identity") + xlab("Year") + ylab("Dollars (million)") + 
  ggtitle("Export of 121229 Seaweeds And Other Algae, Nesoi") +
  geom_text(aes(label = round(ExportValue/1000000, 2)), vjust = -0.2)+
  scale_x_continuous(name="Year", breaks=c(2010:2021)) +
  scale_fill_manual(values="#5f96c9") +
  theme_minimal() +
  theme(legend.position="none") +
  theme(axis.text.x = element_text(angle = 90)) +
  theme(axis.line = element_line(colour = "black", 
                                 size = 1, linetype = "solid")) 

ggsave("3.a.10.seafood_markets.pdf", width = 15, height = 6)

p11 <- ggplot(worldsub[which(worldsub$Commodity == '1605 Crustaceans, Molluscs Etc. Prepared Or Preserved'),], aes(x=Year, y=ExportValue/1000000, fill=Commodity)) + 
  geom_bar(position="stack", stat="identity") + xlab("Year") + ylab("Dollars (million)") + 
  ggtitle("Export of 1605 Crustaceans, Molluscs Etc. Prepared Or Preserved") +
  geom_text(aes(label = round(ExportValue/1000000, 2)), vjust = -0.2)+
  scale_x_continuous(name="Year", breaks=c(2010:2021)) +
  scale_fill_manual(values="#5f96c9") +
  theme_minimal() +
  theme(legend.position="none") +
  theme(axis.text.x = element_text(angle = 90)) +
  theme(axis.line = element_line(colour = "black", 
                                 size = 1, linetype = "solid")) 

ggsave("3.a.11.seafood_markets.pdf", width = 15, height = 6)

## Percentage Change 

# 03 Fish, Crustaceans & Aquatic Invertebrates

trade <- worldsub_all[which(worldsub_all$Commodity == '03 Fish, Crustaceans & Aquatic Invertebrates'),]
mean <- mean(trade$ExportValue)
worldsub_all_change <- trade %>% 
  mutate(year_per_change = ((ExportValue) - (mean))/(mean) *100)

f0 <- ggplot(worldsub_all_change, aes(x=Year, y=year_per_change, fill=Commodity)) + 
  geom_bar(position="stack", stat="identity") + xlab("Year") + ylab("% Change from 12-year mean") + 
  ggtitle("Change in trade for 03 Fish, Crustaceans & Aquatic Invertebrates") +
  geom_text(aes(label = round(ExportValue/1000000, 2)), vjust = -0.2)+
  scale_x_continuous(name="Year", breaks=c(2010:2021)) +
  scale_fill_manual(values="#5f96c9") +
  theme_minimal() +
  geom_hline(yintercept=0) +
  theme(legend.position="none") +
  theme(axis.text.x = element_text(angle = 90)) +
  theme(axis.line = element_line(colour = "black", 
                                 size = 1, linetype = "solid")) 

ggsave("3.b.seafood_markets.pdf", width = 15, height = 6)

# 0301 Fish, Live

trade <- worldsub[which(worldsub$Commodity == "0301 Fish, Live"), ]
mean <- mean(trade$ExportValue)
worldsub_change <- trade %>% 
  mutate(year_per_change = ((ExportValue) - (mean))/(mean) *100)

f1 <- ggplot(worldsub_change, aes(x=Year, y=year_per_change, fill=Commodity)) + 
  geom_bar(position="stack", stat="identity") + xlab("Year") + ylab("% Change from 12-year mean") + 
  ggtitle("Change in trade for 0301 Fish, Live") +
  geom_text(aes(label = round(year_per_change, 1)), vjust = -0.2)+
  scale_x_continuous(name="Year", breaks=c(2010:2021)) +
  scale_fill_manual(values="#5f96c9") +
  theme_minimal() +
  geom_hline(yintercept=0) +
  theme(legend.position="none") +
  theme(axis.text.x = element_text(angle = 90)) +
  theme(axis.line = element_line(colour = "black", 
                                 size = 1, linetype = "solid")) 

ggsave("3.b.1.seafood_markets.pdf", width = 15, height = 6)

# 0302 Fish, Fresh Or Chilled (no Fillets Or Other Meat)

trade <- worldsub[which(worldsub$Commodity == "0302 Fish, Fresh Or Chilled (no Fillets Or Other Meat)"), ]
mean <- mean(trade$ExportValue)
worldsub_change <- trade %>% 
  mutate(year_per_change = ((ExportValue) - (mean))/(mean) *100)

f2 <- ggplot(worldsub_change, aes(x=Year, y=year_per_change, fill=Commodity)) + 
  geom_bar(position="stack", stat="identity") + xlab("Year") + ylab("% Change from 12-year mean") + 
  ggtitle("Change in trade for 0302 Fish, Fresh Or Chilled (no Fillets Or Other Meat)") +
  geom_text(aes(label = round(year_per_change, 1)), vjust = -0.2)+
  scale_x_continuous(name="Year", breaks=c(2010:2021)) +
  scale_fill_manual(values="#5f96c9") +
  theme_minimal() +
  geom_hline(yintercept=0) +
  theme(legend.position="none") +
  theme(axis.text.x = element_text(angle = 90)) +
  theme(axis.line = element_line(colour = "black", 
                                 size = 1, linetype = "solid")) 

ggsave("3.b.2.seafood_markets.pdf", width = 15, height = 6)

# 0303 Fish, Frozen (no Fish Fillets Or Other Fish Meat)

trade <- worldsub[which(worldsub$Commodity == "0303 Fish, Frozen (no Fish Fillets Or Other Fish Meat)"), ]
mean <- mean(trade$ExportValue)
worldsub_change <- trade %>% 
  mutate(year_per_change = ((ExportValue) - (mean))/(mean) *100)

f3 <- ggplot(worldsub_change, aes(x=Year, y=year_per_change, fill=Commodity)) + 
  geom_bar(position="stack", stat="identity") + xlab("Year") + ylab("% Change from 12-year mean") + 
  ggtitle("Change in trade for 0303 Fish, Frozen (no Fish Fillets Or Other Fish Meat)") +
  geom_text(aes(label = round(year_per_change, 1)), vjust = -0.2)+
  scale_x_continuous(name="Year", breaks=c(2010:2021)) +
  scale_fill_manual(values="#5f96c9") +
  theme_minimal() +
  geom_hline(yintercept=0) +
  theme(legend.position="none") +
  theme(axis.text.x = element_text(angle = 90)) +
  theme(axis.line = element_line(colour = "black", 
                                 size = 1, linetype = "solid")) 

ggsave("3.b.3.seafood_markets.pdf", width = 15, height = 6)

# 0304 Fish Fillets & Oth Fish Meat, Fresh, Chill Or Froz

trade <- worldsub[which(worldsub$Commodity == "0304 Fish Fillets & Oth Fish Meat, Fresh, Chill Or Froz"), ]
mean <- mean(trade$ExportValue)
worldsub_change <- trade %>% 
  mutate(year_per_change = ((ExportValue) - (mean))/(mean) *100)

f4 <- ggplot(worldsub_change, aes(x=Year, y=year_per_change, fill=Commodity)) + 
  geom_bar(position="stack", stat="identity") + xlab("Year") + ylab("% Change from 12-year mean") + 
  ggtitle("Change in trade for 0304 Fish Fillets & Oth Fish Meat, Fresh, Chill Or Froz") +
  geom_text(aes(label = round(year_per_change, 1)), vjust = -0.2)+
  scale_x_continuous(name="Year", breaks=c(2010:2021)) +
  scale_fill_manual(values="#5f96c9") +
  theme_minimal() +
  geom_hline(yintercept=0) +
  theme(legend.position="none") +
  theme(axis.text.x = element_text(angle = 90)) +
  theme(axis.line = element_line(colour = "black", 
                                 size = 1, linetype = "solid")) 

ggsave("3.b.4.seafood_markets.pdf", width = 15, height = 6)


# 0305 Fish, Dried, Salted Or In Brine; Smoked Fish

trade <- worldsub[which(worldsub$Commodity == "0305 Fish, Dried, Salted Or In Brine; Smoked Fish"), ]
mean <- mean(trade$ExportValue)
worldsub_change <- trade %>% 
  mutate(year_per_change = ((ExportValue) - (mean))/(mean) *100)

f5 <- ggplot(worldsub_change, aes(x=Year, y=year_per_change, fill=Commodity)) + 
  geom_bar(position="stack", stat="identity") + xlab("Year") + ylab("% Change from 12-year mean") + 
  ggtitle("Change in trade for 0305 Fish, Dried, Salted Or In Brine; Smoked Fish") +
  geom_text(aes(label = round(year_per_change, 1)), vjust = -0.2)+
  scale_x_continuous(name="Year", breaks=c(2010:2021)) +
  scale_fill_manual(values="#5f96c9") +
  theme_minimal() +
  geom_hline(yintercept=0) +
  theme(legend.position="none") +
  theme(axis.text.x = element_text(angle = 90)) +
  theme(axis.line = element_line(colour = "black", 
                                 size = 1, linetype = "solid")) 

ggsave("3.b.5.seafood_markets.pdf", width = 15, height = 6)

# 0306 Crustaceans

trade <- worldsub[which(worldsub$Commodity == "0306 Crustaceans"), ]
mean <- mean(trade$ExportValue)
worldsub_change <- trade %>% 
  mutate(year_per_change = ((ExportValue) - (mean))/(mean) *100)

f6 <- ggplot(worldsub_change, aes(x=Year, y=year_per_change, fill=Commodity)) + 
  geom_bar(position="stack", stat="identity") + xlab("Year") + ylab("% Change from 12-year mean") + 
  ggtitle("Change in trade for 0306 Crustaceans") +
  geom_text(aes(label = round(year_per_change, 1)), vjust = -0.2)+
  scale_x_continuous(name="Year", breaks=c(2010:2021)) +
  scale_fill_manual(values="#5f96c9") +
  theme_minimal() +
  geom_hline(yintercept=0) +
  theme(legend.position="none") +
  theme(axis.text.x = element_text(angle = 90)) +
  theme(axis.line = element_line(colour = "black", 
                                 size = 1, linetype = "solid")) 

ggsave("3.b.6.seafood_markets.pdf", width = 15, height = 6)

# 0307 Molluscs

trade <- worldsub[which(worldsub$Commodity == "0307 Molluscs"), ]
mean <- mean(trade$ExportValue)
worldsub_change <- trade %>% 
  mutate(year_per_change = ((ExportValue) - (mean))/(mean) *100)

f7 <- ggplot(worldsub_change, aes(x=Year, y=year_per_change, fill=Commodity)) + 
  geom_bar(position="stack", stat="identity") + xlab("Year") + ylab("% Change from 12-year mean") + 
  ggtitle("Change in trade for 0307 Molluscs") +
  geom_text(aes(label = round(year_per_change, 1)), vjust = -0.2)+
  scale_x_continuous(name="Year", breaks=c(2010:2021)) +
  scale_fill_manual(values="#5f96c9") +
  theme_minimal() +
  geom_hline(yintercept=0) +
  theme(legend.position="none") +
  theme(axis.text.x = element_text(angle = 90)) +
  theme(axis.line = element_line(colour = "black", 
                                 size = 1, linetype = "solid")) 

ggsave("3.b.7.seafood_markets.pdf", width = 15, height = 6)

# 0308 Aquatic Invertebrates, Nesoi

trade <- worldsub[which(worldsub$Commodity == "0308 Aquatic Invertebrates, Nesoi"), ]
mean <- mean(trade$ExportValue, na.rm=TRUE)
worldsub_change <- trade %>% 
  mutate(year_per_change = ((ExportValue) - (mean))/(mean) *100)

f8 <- ggplot(worldsub_change, aes(x=Year, y=year_per_change, fill=Commodity)) + 
  geom_bar(position="stack", stat="identity") + xlab("Year") + ylab("% Change from 12-year mean") + 
  ggtitle("Change in trade for 0308 Aquatic Invertebrates, Nesoi") +
  geom_text(aes(label = round(year_per_change, 1)), vjust = -0.2)+
  scale_x_continuous(name="Year", breaks=c(2010:2021)) +
  scale_fill_manual(values="#5f96c9") +
  theme_minimal() +
  geom_hline(yintercept=0) +
  theme(legend.position="none") +
  theme(axis.text.x = element_text(angle = 90)) +
  theme(axis.line = element_line(colour = "black", 
                                 size = 1, linetype = "solid")) 

ggsave("3.b.8.seafood_markets.pdf", width = 15, height = 6)


# 121221 Seaweeds And Other Algae Fit For Human Consumption

trade <- worldsub[which(worldsub$Commodity == "121221 Seaweeds And Other Algae Fit For Human Consumption"), ]
mean <- mean(trade$ExportValue, na.rm=TRUE)
worldsub_change <- trade %>% 
  mutate(year_per_change = ((ExportValue) - (mean))/(mean) *100)

f9 <- ggplot(worldsub_change, aes(x=Year, y=year_per_change, fill=Commodity)) + 
  geom_bar(position="stack", stat="identity") + xlab("Year") + ylab("% Change from 12-year mean") + 
  ggtitle("Change in trade for 121221 Seaweeds And Other Algae Fit For Human Consumption") +
  geom_text(aes(label = round(year_per_change, 1)), vjust = -0.2)+
  scale_x_continuous(name="Year", breaks=c(2010:2021)) +
  scale_fill_manual(values="#5f96c9") +
  theme_minimal() +
  geom_hline(yintercept=0) +
  theme(legend.position="none") +
  theme(axis.text.x = element_text(angle = 90)) +
  theme(axis.line = element_line(colour = "black", 
                                 size = 1, linetype = "solid")) 

ggsave("3.b.9.seafood_markets.pdf", width = 15, height = 6)

# 121229 Seaweeds And Other Algae, Nesoi

trade <- worldsub[which(worldsub$Commodity == "121229 Seaweeds And Other Algae, Nesoi"), ]
mean <- mean(trade$ExportValue, na.rm=TRUE)
worldsub_change <- trade %>% 
  mutate(year_per_change = ((ExportValue) - (mean))/(mean) *100)

f10 <- ggplot(worldsub_change, aes(x=Year, y=year_per_change, fill=Commodity)) + 
  geom_bar(position="stack", stat="identity") + xlab("Year") + ylab("% Change from 12-year mean") + 
  ggtitle("Change in trade for 121229 Seaweeds And Other Algae, Nesoi") +
  geom_text(aes(label = round(year_per_change, 1)), vjust = -0.2)+
  scale_x_continuous(name="Year", breaks=c(2010:2021)) +
  scale_fill_manual(values="#5f96c9") +
  theme_minimal() +
  geom_hline(yintercept=0) +
  theme(legend.position="none") +
  theme(axis.text.x = element_text(angle = 90)) +
  theme(axis.line = element_line(colour = "black", 
                                 size = 1, linetype = "solid")) 

ggsave("3.b.10.seafood_markets.pdf", width = 15, height = 6)

# 1605 Crustaceans, Molluscs Etc. Prepared Or Preserved

trade <- worldsub[which(worldsub$Commodity == "1605 Crustaceans, Molluscs Etc. Prepared Or Preserved"), ]
mean <- mean(trade$ExportValue, na.rm=TRUE)
worldsub_change <- trade %>% 
  mutate(year_per_change = ((ExportValue) - (mean))/(mean) *100)

f11 <- ggplot(worldsub_change, aes(x=Year, y=year_per_change, fill=Commodity)) + 
  geom_bar(position="stack", stat="identity") + xlab("Year") + ylab("% Change from 12-year mean") + 
  ggtitle("Change in trade for 1605 Crustaceans, Molluscs Etc. Prepared Or Preserved") +
  geom_text(aes(label = round(year_per_change, 1)), vjust = -0.2)+
  scale_x_continuous(name="Year", breaks=c(2010:2021)) +
  scale_fill_manual(values="#5f96c9") +
  theme_minimal() +
  geom_hline(yintercept=0) +
  theme(legend.position="none") +
  theme(axis.text.x = element_text(angle = 90)) +
  theme(axis.line = element_line(colour = "black", 
                                 size = 1, linetype = "solid")) 

ggsave("3.b.11.seafood_markets.pdf", width = 15, height = 6)

# Top-10 export countries 

# 03 Fish, Crustaceans & Aquatic Invertebrates
exportcountries_no_world <- exportcountries[which(exportcountries$Country != 'World Total'),]
exportcountries_no_world <- exportcountries_no_world[which(exportcountries_no_world$Commodity == '03 Fish, Crustaceans & Aquatic Invertebrates' & exportcountries_no_world$Year == 2021) ,]
exportcountries_no_world <- na.omit(exportcountries_no_world[order(-exportcountries_no_world$ExportValue),])
exportcountries_no_world <- head(exportcountries_no_world,10)

t0 <- ggplot(exportcountries_no_world, aes(x=Country, y=ExportValue/1000000, fill=Commodity)) + 
  geom_bar(position="stack", stat="identity") + xlab("Year") + ylab("Dollars (million)") + 
  ggtitle("Top-10 Export Countries (03 Fish, Crustaceans & Aquatic Invertebrates)") +
  #scale_x_continuous(name="Year", breaks=c(2010:2021)) +
  geom_text(aes(label = round(ExportValue/1000000, 2)), vjust = -0.2)+
  scale_fill_manual(values="#5f96c9") +
  theme_minimal() +
  geom_hline(yintercept=0) +
  theme(legend.position="none") +
  theme(axis.text.x = element_text(angle = 90)) +
  theme(axis.line = element_line(colour = "black", 
                                 size = 1, linetype = "solid")) 

ggsave("3.c.seafood_markets.pdf", width = 15, height = 6)

# 0301 Fish, Live
exportcountries_no_world <- exportcountries[which(exportcountries$Country != 'World Total'),]
exportcountries_no_world <- exportcountries_no_world[which(exportcountries_no_world$Commodity == '0301 Fish, Live' & exportcountries_no_world$Year == 2021) ,]
exportcountries_no_world <- na.omit(exportcountries_no_world[order(-exportcountries_no_world$ExportValue),])
exportcountries_no_world <- head(exportcountries_no_world,10)

t1 <- ggplot(exportcountries_no_world, aes(x=Country, y=ExportValue/1000000, fill=Commodity)) + 
  geom_bar(position="stack", stat="identity") + xlab("Year") + ylab("Dollars (million)") + 
  ggtitle("Top-10 Export Countries (0301 Fish, Live)") +
  #scale_x_continuous(name="Year", breaks=c(2010:2021)) +
  geom_text(aes(label = round(ExportValue/1000000, 2)), vjust = -0.2)+
  scale_fill_manual(values="#5f96c9") +
  theme_minimal() +
  geom_hline(yintercept=0) +
  theme(legend.position="none") +
  theme(axis.text.x = element_text(angle = 90)) +
  theme(axis.line = element_line(colour = "black", 
                                 size = 1, linetype = "solid")) 

ggsave("3.c.1.seafood_markets.pdf", width = 15, height = 6)


# 0302 Fish, Fresh Or Chilled (no Fillets Or Other Meat)
exportcountries_no_world <- exportcountries[which(exportcountries$Country != 'World Total'),]
exportcountries_no_world <- exportcountries_no_world[which(exportcountries_no_world$Commodity == '0302 Fish, Fresh Or Chilled (no Fillets Or Other Meat)' & exportcountries_no_world$Year == 2021) ,]
exportcountries_no_world <- na.omit(exportcountries_no_world[order(-exportcountries_no_world$ExportValue),])
exportcountries_no_world <- head(exportcountries_no_world,10)

t2 <- ggplot(exportcountries_no_world, aes(x=Country, y=ExportValue/1000000, fill=Commodity)) + 
  geom_bar(position="stack", stat="identity") + xlab("Year") + ylab("Dollars (million)") + 
  ggtitle("Top-10 Export Countries (0302 Fish, Fresh Or Chilled (no Fillets Or Other Meat))") +
  #scale_x_continuous(name="Year", breaks=c(2010:2021)) +
  geom_text(aes(label = round(ExportValue/1000000, 2)), vjust = -0.2)+
  scale_fill_manual(values="#5f96c9") +
  theme_minimal() +
  geom_hline(yintercept=0) +
  theme(legend.position="none") +
  theme(axis.text.x = element_text(angle = 90)) +
  theme(axis.line = element_line(colour = "black", 
                                 size = 1, linetype = "solid")) 

ggsave("3.c.2.seafood_markets.pdf", width = 15, height = 6)


# 0303 Fish, Frozen (no Fish Fillets Or Other Fish Meat)
exportcountries_no_world <- exportcountries[which(exportcountries$Country != 'World Total'),]
exportcountries_no_world <- exportcountries_no_world[which(exportcountries_no_world$Commodity == '0303 Fish, Frozen (no Fish Fillets Or Other Fish Meat)' & exportcountries_no_world$Year == 2021) ,]
exportcountries_no_world <- na.omit(exportcountries_no_world[order(-exportcountries_no_world$ExportValue),])
exportcountries_no_world <- head(exportcountries_no_world,10)

t3 <- ggplot(exportcountries_no_world, aes(x=Country, y=ExportValue/1000000, fill=Commodity)) + 
  geom_bar(position="stack", stat="identity") + xlab("Year") + ylab("Dollars (million)") + 
  ggtitle("Top-10 Export Countries (0303 Fish, Frozen (no Fish Fillets Or Other Fish Meat))") +
  #scale_x_continuous(name="Year", breaks=c(2010:2021)) +
  geom_text(aes(label = round(ExportValue/1000000, 2)), vjust = -0.2)+
  scale_fill_manual(values="#5f96c9") +
  theme_minimal() +
  geom_hline(yintercept=0) +
  theme(legend.position="none") +
  theme(axis.text.x = element_text(angle = 90)) +
  theme(axis.line = element_line(colour = "black", 
                                 size = 1, linetype = "solid")) 

ggsave("3.c.3.seafood_markets.pdf", width = 15, height = 6)


# 0304 Fish Fillets & Oth Fish Meat, Fresh, Chill Or Froz
exportcountries_no_world <- exportcountries[which(exportcountries$Country != 'World Total'),]
exportcountries_no_world <- exportcountries_no_world[which(exportcountries_no_world$Commodity == '0304 Fish Fillets & Oth Fish Meat, Fresh, Chill Or Froz' & exportcountries_no_world$Year == 2021) ,]
exportcountries_no_world <- na.omit(exportcountries_no_world[order(-exportcountries_no_world$ExportValue),])
exportcountries_no_world <- head(exportcountries_no_world,10)

t4 <- ggplot(exportcountries_no_world, aes(x=Country, y=ExportValue/1000000, fill=Commodity)) + 
  geom_bar(position="stack", stat="identity") + xlab("Year") + ylab("Dollars (million)") + 
  ggtitle("Top-10 Export Countries (0304 Fish Fillets & Oth Fish Meat, Fresh, Chill Or Froz)") +
  #scale_x_continuous(name="Year", breaks=c(2010:2021)) +
  geom_text(aes(label = round(ExportValue/1000000, 2)), vjust = -0.2)+
  scale_fill_manual(values="#5f96c9") +
  theme_minimal() +
  geom_hline(yintercept=0) +
  theme(legend.position="none") +
  theme(axis.text.x = element_text(angle = 90)) +
  theme(axis.line = element_line(colour = "black", 
                                 size = 1, linetype = "solid")) 

ggsave("3.c.4.seafood_markets.pdf", width = 15, height = 6)

# 0305 Fish, Dried, Salted Or In Brine; Smoked Fish
exportcountries_no_world <- exportcountries[which(exportcountries$Country != 'World Total'),]
exportcountries_no_world <- exportcountries_no_world[which(exportcountries_no_world$Commodity == '0305 Fish, Dried, Salted Or In Brine; Smoked Fish' & exportcountries_no_world$Year == 2021) ,]
exportcountries_no_world <- na.omit(exportcountries_no_world[order(-exportcountries_no_world$ExportValue),])
exportcountries_no_world <- head(exportcountries_no_world,10)

t5 <- ggplot(exportcountries_no_world, aes(x=Country, y=ExportValue/1000000, fill=Commodity)) + 
  geom_bar(position="stack", stat="identity") + xlab("Year") + ylab("Dollars (million)") + 
  ggtitle("Top-10 Export Countries (0305 Fish, Dried, Salted Or In Brine; Smoked Fish)") +
  #scale_x_continuous(name="Year", breaks=c(2010:2021)) +
  geom_text(aes(label = round(ExportValue/1000000, 2)), vjust = -0.2)+
  scale_fill_manual(values="#5f96c9") +
  theme_minimal() +
  geom_hline(yintercept=0) +
  theme(legend.position="none") +
  theme(axis.text.x = element_text(angle = 90)) +
  theme(axis.line = element_line(colour = "black", 
                                 size = 1, linetype = "solid")) 

ggsave("3.c.5.seafood_markets.pdf", width = 15, height = 6)

# 0306 Crustaceans
exportcountries_no_world <- exportcountries[which(exportcountries$Country != 'World Total'),]
exportcountries_no_world <- exportcountries_no_world[which(exportcountries_no_world$Commodity == '0306 Crustaceans' & exportcountries_no_world$Year == 2021) ,]
exportcountries_no_world <- na.omit(exportcountries_no_world[order(-exportcountries_no_world$ExportValue),])
exportcountries_no_world <- head(exportcountries_no_world,10)

t6 <- ggplot(exportcountries_no_world, aes(x=Country, y=ExportValue/1000000, fill=Commodity)) + 
  geom_bar(position="stack", stat="identity") + xlab("Year") + ylab("Dollars (million)") + 
  ggtitle("Top-10 Export Countries (0306 Crustaceans)") +
  #scale_x_continuous(name="Year", breaks=c(2010:2021)) +
  geom_text(aes(label = round(ExportValue/1000000, 2)), vjust = -0.2)+
  scale_fill_manual(values="#5f96c9") +
  theme_minimal() +
  geom_hline(yintercept=0) +
  theme(legend.position="none") +
  theme(axis.text.x = element_text(angle = 90)) +
  theme(axis.line = element_line(colour = "black", 
                                 size = 1, linetype = "solid")) 

ggsave("3.c.6.seafood_markets.pdf", width = 15, height = 6)

# 0307 Molluscs
exportcountries_no_world <- exportcountries[which(exportcountries$Country != 'World Total'),]
exportcountries_no_world <- exportcountries_no_world[which(exportcountries_no_world$Commodity == '0307 Molluscs' & exportcountries_no_world$Year == 2021) ,]
exportcountries_no_world <- na.omit(exportcountries_no_world[order(-exportcountries_no_world$ExportValue),])
exportcountries_no_world <- head(exportcountries_no_world,10)

t7 <- ggplot(exportcountries_no_world, aes(x=Country, y=ExportValue/1000000, fill=Commodity)) + 
  geom_bar(position="stack", stat="identity") + xlab("Year") + ylab("Dollars (million)") + 
  ggtitle("Top-10 Export Countries (0307 Molluscs)") +
  #scale_x_continuous(name="Year", breaks=c(2010:2021)) +
  geom_text(aes(label = round(ExportValue/1000000, 2)), vjust = -0.2)+
  scale_fill_manual(values="#5f96c9") +
  theme_minimal() +
  geom_hline(yintercept=0) +
  theme(legend.position="none") +
  theme(axis.text.x = element_text(angle = 90)) +
  theme(axis.line = element_line(colour = "black", 
                                 size = 1, linetype = "solid")) 

ggsave("3.c.7.seafood_markets.pdf", width = 15, height = 6)

# 0308 Aquatic Invertebrates, Nesoi
exportcountries_no_world <- exportcountries[which(exportcountries$Country != 'World Total'),]
exportcountries_no_world <- exportcountries_no_world[which(exportcountries_no_world$Commodity == '0308 Aquatic Invertebrates, Nesoi' & exportcountries_no_world$Year == 2021) ,]
exportcountries_no_world <- na.omit(exportcountries_no_world[order(-exportcountries_no_world$ExportValue),])
exportcountries_no_world <- head(exportcountries_no_world,10)

t8 <- ggplot(exportcountries_no_world, aes(x=Country, y=ExportValue/1000000, fill=Commodity)) + 
  geom_bar(position="stack", stat="identity") + xlab("Year") + ylab("Dollars (million)") + 
  ggtitle("Top-10 Export Countries (0308 Aquatic Invertebrates, Nesoi)") +
  #scale_x_continuous(name="Year", breaks=c(2010:2021)) +
  geom_text(aes(label = round(ExportValue/1000000, 2)), vjust = -0.2)+
  scale_fill_manual(values="#5f96c9") +
  theme_minimal() +
  geom_hline(yintercept=0) +
  theme(legend.position="none") +
  theme(axis.text.x = element_text(angle = 90)) +
  theme(axis.line = element_line(colour = "black", 
                                 size = 1, linetype = "solid")) 

ggsave("3.c.8.seafood_markets.pdf", width = 15, height = 6)


# 121221 Seaweeds And Other Algae Fit For Human Consumption
exportcountries_no_world <- exportcountries[which(exportcountries$Country != 'World Total'),]
exportcountries_no_world <- exportcountries_no_world[which(exportcountries_no_world$Commodity == '121221 Seaweeds And Other Algae Fit For Human Consumption' & exportcountries_no_world$Year == 2021) ,]
exportcountries_no_world <- na.omit(exportcountries_no_world[order(-exportcountries_no_world$ExportValue),])
exportcountries_no_world <- head(exportcountries_no_world,10)

t9 <- ggplot(exportcountries_no_world, aes(x=Country, y=ExportValue/1000000, fill=Commodity)) + 
  geom_bar(position="stack", stat="identity") + xlab("Year") + ylab("Dollars (million)") + 
  ggtitle("Top-10 Export Countries (121221 Seaweeds And Other Algae Fit For Human Consumption)") +
  #scale_x_continuous(name="Year", breaks=c(2010:2021)) +
  geom_text(aes(label = round(ExportValue/1000000, 2)), vjust = -0.2)+
  scale_fill_manual(values="#5f96c9") +
  theme_minimal() +
  geom_hline(yintercept=0) +
  theme(legend.position="none") +
  theme(axis.text.x = element_text(angle = 90)) +
  theme(axis.line = element_line(colour = "black", 
                                 size = 1, linetype = "solid")) 

ggsave("3.c.9.seafood_markets.pdf", width = 15, height = 6)


# 121229 Seaweeds And Other Algae, Nesoi
exportcountries_no_world <- exportcountries[which(exportcountries$Country != 'World Total'),]
exportcountries_no_world <- exportcountries_no_world[which(exportcountries_no_world$Commodity == '121229 Seaweeds And Other Algae, Nesoi' & exportcountries_no_world$Year == 2021) ,]
exportcountries_no_world <- na.omit(exportcountries_no_world[order(-exportcountries_no_world$ExportValue),])
exportcountries_no_world <- head(exportcountries_no_world,10)

t10 <- ggplot(exportcountries_no_world, aes(x=Country, y=ExportValue/1000000, fill=Commodity)) + 
  geom_bar(position="stack", stat="identity") + xlab("Year") + ylab("Dollars (million)") + 
  ggtitle("Top-10 Export Countries (121229 Seaweeds And Other Algae, Nesoi)") +
  #scale_x_continuous(name="Year", breaks=c(2010:2021)) +
  geom_text(aes(label = round(ExportValue/1000000, 2)), vjust = -0.2)+
  scale_fill_manual(values="#5f96c9") +
  theme_minimal() +
  geom_hline(yintercept=0) +
  theme(legend.position="none") +
  theme(axis.text.x = element_text(angle = 90)) +
  theme(axis.line = element_line(colour = "black", 
                                 size = 1, linetype = "solid")) 

ggsave("3.c.10.seafood_markets.pdf", width = 15, height = 6)

# 1605 Crustaceans, Molluscs Etc. Prepared Or Preserved
exportcountries_no_world <- exportcountries[which(exportcountries$Country != 'World Total'),]
exportcountries_no_world <- exportcountries_no_world[which(exportcountries_no_world$Commodity == '1605 Crustaceans, Molluscs Etc. Prepared Or Preserved' & exportcountries_no_world$Year == 2021) ,]
exportcountries_no_world <- na.omit(exportcountries_no_world[order(-exportcountries_no_world$ExportValue),])
exportcountries_no_world <- head(exportcountries_no_world,10)

t11 <- ggplot(exportcountries_no_world, aes(x=Country, y=ExportValue/1000000, fill=Commodity)) + 
  geom_bar(position="stack", stat="identity") + xlab("Year") + ylab("Dollars (million)") + 
  ggtitle("Top-10 Export Countries (1605 Crustaceans, Molluscs Etc. Prepared Or Preserved)") +
  #scale_x_continuous(name="Year", breaks=c(2010:2021)) +
  geom_text(aes(label = round(ExportValue/1000000, 2)), vjust = -0.2)+
  scale_fill_manual(values="#5f96c9") +
  theme_minimal() +
  geom_hline(yintercept=0) +
  theme(legend.position="none") +
  theme(axis.text.x = element_text(angle = 90)) +
  theme(axis.line = element_line(colour = "black", 
                                 size = 1, linetype = "solid")) 

ggsave("3.c.11.seafood_markets.pdf", width = 15, height = 6)






