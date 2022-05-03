# Export Markets
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

### packages ####
library(ggplot2)
library(ggpubr)
library(dplyr)


#### data loading ####

exportcountries <- read.csv("/Users/jstoll/OneDrive - University of Maine System/Social_Coasts_Lab/seafood_systems/Data/Processed_Data/USATradeData4DigitExportCountries2010_2021.csv", stringsAsFactors = FALSE)

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

exportcountries <- filter(exportcountries, Year <= 2020 && Year > 2010)

### Create plots 

#e37a55 - Orange
#233749 - Blue

cbbPalette <- c("#7abfff", "#5f96c9", "#476f94", "#233749", "#e37a55")

# World exports by commodity group 

worldsub_all <- exportcountries[which(exportcountries$Country == 'World Total'),]
worldsub <- worldsub[which(worldsub_all$Commodity != '03 Fish, Crustaceans & Aquatic Invertebrates'),]

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
  ggtitle("121221 Seaweeds And Other Algae Fit For Human Consumption") +
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
  ggtitle("121229 Seaweeds And Other Algae, Nesoi") +
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
  ggtitle("1605 Crustaceans, Molluscs Etc. Prepared Or Preserved") +
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

t0 <- ggplot(worldsub_change, aes(x=Year, y=ExportValue/1000000, fill=Commodity)) + 
  geom_bar(position="stack", stat="identity") + xlab("Year") + ylab("Dollars (million)") + 
  ggtitle("03 Fish, Crustaceans & Aquatic Invertebrates") +
  scale_x_continuous(name="Year", breaks=c(2010:2021)) +
  scale_fill_manual(values="#5f96c9") +
  theme_minimal() +
  geom_hline(yintercept=0) +
  theme(legend.position="none") +
  theme(axis.text.x = element_text(angle = 90)) +
  theme(axis.line = element_line(colour = "black", 
                                 size = 1, linetype = "solid")) 


#Stopped coding here, but left Seafood Production code below in case it can be adapted for the following

### Commodity Plots (focusing on latest ten year range (Yr 1...Yr10))
Done # Plot 1 - stacked bar plot of world totals stacking 4-digit and 6-digit commodities (y-axis)  by Yr1 to Yr10 (x-axis) Note: this means 2-digit commodity gets dropped (note: remove 2-digit)
Done # (#12) Plots 2 to 14 - one line graph for each commodity world totals by 4-digit and 6-digit commodity (y-axis) by year (x-axis) Y1 to Yr10
Done # (#12) Summary Stats - Percentage changes from Yr1 to Yr10 (% change by mean - total exports) (for each HS code)

### Countries Plots 
# Plots 15 to 23 - side by side Yr1 and Yr10 plots for each of the 12 4-digit and 6-digit commodities showing the top ten export destination countries + "other countries" as an aggregate of remaining country exports
# Note - Canada will be on a different scale for some commodities and we can discuss whether to separate it, or show it on a different scale if possible
# Summary Stats - Percentage shares of countries in each of the plots 15 to 23 **or changes from 1st 3 or 5 years versus last 3 or 5 years (more stable and more data to work with)

### Other topics
# In future the commodity labels can change and/or product gets re-classified and we will have to watch for this in future
# Could the landings and climate vulnerability data be used together with the export data to say more about future expectations? Perhaps only addressed qualitatively in the report

# I can get the same csv file for other states, should we do this to make comparisons?
# There is price information for air-freight only (i.e. high value exports) and we could plot some of these





worldsub$Commodity <- factor(worldsub$Commodity, levels = comlabels)

p1 <- ggplot(worldsub, aes(x=Year, y=Exports($)/1000000, fill=Commodity)) + 
  geom_bar(position="stack", stat="identity") + xlab("Year") + ylab("Exports ($millions)") + 
  ggtitle("MENHADENS LANDINGS") +
  scale_x_continuous(name="Year", breaks=c(2010:2020)) +
  scale_fill_manual(values=cbbPalette) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90)) +
  theme(axis.line = element_line(colour = "black", 
                                 size = 1, linetype = "solid")) 








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


