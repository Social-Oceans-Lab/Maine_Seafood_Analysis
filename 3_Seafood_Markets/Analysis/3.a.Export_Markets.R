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

### packages ####
library(ggplot2)
library(ggpubr)
library(dplyr)


#### data loading ####

exportcountries <- read.csv("~/Gregor/GP/Projects/Fisheries/SeaMaine2021/BenchmarkingDashboard/USATradeData4DigitExportCountries2010_2021.csv", stringsAsFactors = FALSE)

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

# Next line doesn't work, lost values and NAs introduced
exportcountries$ExportValue <- as.numeric(as.character(exportcountries$ExportValue))

#### Stacked ten-year world exports by commodity group (2011 to 2020 aligns with initial landings data, but could do 2012 to 2021 as export data will always be a year ahead of landings data) ####

#keeping 2011-2020

exportcountries <- filter(exportcountries, Year <= 2020 && Year > 2010)

### Create plots 

#e37a55 - Orange
#233749 - Blue

cbbPalette <- c("#7abfff", "#5f96c9", "#476f94", "#233749", "#e37a55")

# World exports by commodity group 

worldsub <- exportcountries[which(exportcountries$Country == 'World Total'),]



#Stopped coding here, but left Seafood Production code below in case it can be adapted for the following

### Commodity Plots (focusing on latest ten year range (Yr 1...Yr10))
# Plot 1 - stacked bar plot of world totals stacking 4-digit and 6-digit commodities (y-axis)  by Yr1 to Yr10 (x-axis) Note: this means 2-digit commodity gets dropped
# Plots 2 to 14 - one line graph for each commodity world totals by 4-digit and 6-digit commodity (y-axis) by year (x-axis) Y1 to Yr10
# Summary Stats - Percentage changes from Yr1 to Yr10
# Consider Stats - Percentage changes from 1st 3 or 5 years versus last 3 or 5 years (more stable and more data to work with)

### Countries Plots 
# Plots 15 to 23 - side by side Yr1 and Yr10 plots for each of the 12 4-digit and 6-digit commodities showing the top ten export destination countries + "other countries" as an aggregate of remaining country exports
# Note - Canada will be on a different scale for some commodities and we can discuss whether to separate it, or show it on a different scale if possible
# Summary Stats - Percentage shares of countries in each of the plots 15 to 23 **or changes from 1st 3 or 5 years versus last 3 or 5 years (more stable and more data to work with)

### Other topics
# We could assign better commodity labels that align more with labels for priority species, but let's discuss
# Feel free to consider other forms of plots that may be useful or better to communicate this data
# In future the commodity labels can change and/or product gets re-classified and we will have to watch for this in future
# Could the landings and climate vulnerability data be used together with the export data to say more about future expectations? Perhaps only addressed qualitatively in the report

# Consider a graph of percentage changes for commodities and/or countries?
# I can get the same csv file for other states, should we do this to make comparisons?
# Could go to 6-digit level of detail, but it starts to get unwieldy - post only as a data file on the website?
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


