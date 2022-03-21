# Fisheries Access
# 1.b. Maine fishing licenses 
# Version 1.0. March 1, 2022

# Load packages 
library(dplyr)   
library(tidyr)
library(stringr)
library(tibble)
library(lubridate)
library(eeptools)
library(igraph)
library(data.table)
library(ggplot2)
library(forcats)

# Reading in data

lic <- read.csv("/Users/jstoll/OneDrive - University of Maine System/Social_Coasts_Lab/seafood_systems/Data/processed_data/Fisheries_Access/me_lic_2016_2021.csv", stringsAsFactors = FALSE)

# Select 2021 data

lic21 <- lic[which(lic$License.Year == 2021),]

# Data processing


LicTypes <- unique(lic21$License.Type) # 98 type original 
LicTypes <- as.data.frame(LicTypes)
LicTypes <- LicTypes[order(LicTypes$LicTypes),]
LicTypes <- as.data.frame(LicTypes)

OWNER <- unique(lic21$Landings.Number)
VRN <- length(unique(lic21$Landings.Number)) # 16824 unique in 2021


# create vector

AL <- vector(length=VRN, mode="numeric")
CAR <- vector(length=VRN, mode="numeric")
CFC <- vector(length=VRN, mode="numeric")
CFS <- vector(length=VRN, mode="numeric")
CPC <- vector(length=VRN, mode="numeric")
CPS <- vector(length=VRN, mode="numeric")
CS <- vector(length=VRN, mode="numeric")
CSO <- vector(length=VRN, mode="numeric")
CSU <- vector(length=VRN, mode="numeric")
DL <- vector(length=VRN, mode="numeric")
DS <- vector(length=VRN, mode="numeric")
E0 <- vector(length=VRN, mode="numeric")
E0C <- vector(length=VRN, mode="numeric")
E1 <- vector(length=VRN, mode="numeric")
E1C <- vector(length=VRN, mode="numeric")
E2 <- vector(length=VRN, mode="numeric")
E2C <- vector(length=VRN, mode="numeric")
E6 <- vector(length=VRN, mode="numeric")
E6C <- vector(length=VRN, mode="numeric")
EP <- vector(length=VRN, mode="numeric")
GC <- vector(length=VRN, mode="numeric")
GCN <- vector(length=VRN, mode="numeric")
LA <- vector(length=VRN, mode="numeric")
LAO <- vector(length=VRN, mode="numeric")
LAU <- vector(length=VRN, mode="numeric")
LC1 <- vector(length=VRN, mode="numeric")
LC2 <- vector(length=VRN, mode="numeric")
LC2O <- vector(length=VRN, mode="numeric")
LC3 <- vector(length=VRN, mode="numeric")
LC3O <- vector(length=VRN, mode="numeric")
LCO <- vector(length=VRN, mode="numeric")
LCS <- vector(length=VRN, mode="numeric")
LCU <- vector(length=VRN, mode="numeric")
LNC <- vector(length=VRN, mode="numeric")
MAED <- vector(length=VRN, mode="numeric")
MAEF <- vector(length=VRN, mode="numeric")
MD <- vector(length=VRN, mode="numeric")
MENC <- vector(length=VRN, mode="numeric")
MENNR <- vector(length=VRN, mode="numeric")
MENR <- vector(length=VRN, mode="numeric")
MH <- vector(length=VRN, mode="numeric")
MIDF <- vector(length=VRN, mode="numeric")
MIDFC <- vector(length=VRN, mode="numeric")
MISD <- vector(length=VRN, mode="numeric")
MISDT <- vector(length=VRN, mode="numeric")
MWD <- vector(length=VRN, mode="numeric")
NBAL <- vector(length=VRN, mode="numeric")
NBCFS <- vector(length=VRN, mode="numeric")
NBCS <- vector(length=VRN, mode="numeric")
NBED <- vector(length=VRN, mode="numeric")
NBEDF <- vector(length=VRN, mode="numeric")
NBEF <- vector(length=VRN, mode="numeric")
NBEF2 <- vector(length=VRN, mode="numeric")
NBEF2C <- vector(length=VRN, mode="numeric")
NBMD <- vector(length=VRN, mode="numeric")
NBSC <- vector(length=VRN, mode="numeric")
NBSD <- vector(length=VRN, mode="numeric")
NBSF <- vector(length=VRN, mode="numeric")
NBSL <- vector(length=VRN, mode="numeric")
NBSS <- vector(length=VRN, mode="numeric")
NCF <- vector(length=VRN, mode="numeric")
NCP <- vector(length=VRN, mode="numeric")
NCS <- vector(length=VRN, mode="numeric")
NLC1 <- vector(length=VRN, mode="numeric")
NRS <- vector(length=VRN, mode="numeric")
NRSS <- vector(length=VRN, mode="numeric")
NTP <- vector(length=VRN, mode="numeric")
PAL <- vector(length=VRN, mode="numeric")
PCS <- vector(length=VRN, mode="numeric")
PELD <- vector(length=VRN, mode="numeric")
PELDC <- vector(length=VRN, mode="numeric")
PELF <- vector(length=VRN, mode="numeric")
PLC3 <- vector(length=VRN, mode="numeric")
PSCF <- vector(length=VRN, mode="numeric")
PSD <- vector(length=VRN, mode="numeric")
PSSH <- vector(length=VRN, mode="numeric")
PSSU <- vector(length=VRN, mode="numeric")
PSUB <- vector(length=VRN, mode="numeric")
PSUH <- vector(length=VRN, mode="numeric")
PSUL <- vector(length=VRN, mode="numeric")
PSUS <- vector(length=VRN, mode="numeric")
QM <- vector(length=VRN, mode="numeric")
SCD <- vector(length=VRN, mode="numeric")
SD <- vector(length=VRN, mode="numeric")
SDI <- vector(length=VRN, mode="numeric")
SDT <- vector(length=VRN, mode="numeric")
SPT <- vector(length=VRN, mode="numeric")
SUB <- vector(length=VRN, mode="numeric")
SUH <- vector(length=VRN, mode="numeric")
SUR <- vector(length=VRN, mode="numeric")
SURF <- vector(length=VRN, mode="numeric")
SUWT <- vector(length=VRN, mode="numeric")
SW <- vector(length=VRN, mode="numeric")
SWR <- vector(length=VRN, mode="numeric")
SWRO <- vector(length=VRN, mode="numeric")
SWS <- vector(length=VRN, mode="numeric")
TEN <- vector(length=VRN, mode="numeric")
VH <- vector(length=VRN, mode="numeric")

FishTable <- cbind(OWNER, AL, CAR, CFC, CFS, CPC, CPS, CS, CSO, CSU, DL, 
                   DS, E0, E0C, E1, E1C, E2, E2C, E6, E6C, EP, GC, GCN, LA, 
                   LAO, LAU, LC1, LC2, LC2O, LC3, LC3O, LCO, LCS, LCU, 
                   LNC, MAED, MAEF, MD, MENC, MENNR, MENR, MH, MIDF, 
                   MIDFC, MISD, MISDT, MWD, NBAL, NBCFS, NBCS, NBED, 
                   NBEDF, NBEF, NBEF2, NBEF2C, NBMD, NBSC, NBSD, NBSF, NBSL, 
                   NBSS, NCF, NCP, NCS, NLC1, NRS, NRSS, NTP, PAL, PCS, 
                   PELD, PELDC, PELF, PLC3, PSCF, PSD, PSSH, PSSU, PSUB, 
                   PSUH, PSUL, PSUS, QM, SCD, SD, SDI, SDT, SPT, SUB, 
                   SUH, SUR, SURF, SUWT, SW, SWR, SWRO, SWS, TEN, VH)   

FishTable <- as.data.frame(FishTable, stringsAsFactors=FALSE)

for(i in 1:nrow(FishTable)) {
  lic <- subset(lic21$License.Type, lic21$Landings.Number==OWNER[i])
  FishTable[i,2:99] <- (LicTypes == lic)
}

write.csv(FishTable, "me_lic_2021_matrix.csv")
### FishTable$Number.Licenses <- rowSums(FishTable[2:99]) NOT WORKING, WHY DO SOME PEOPLE HAVE ZERO LICENSES? 


## Diversification among harvesters

lic21_no_duplications <- select(lic21, -X)
lic21_no_duplications <- select(lic21_no_duplications, -License.Number)

lic21_no_duplications <- lic21_no_duplications[!duplicated(lic21_no_duplications), ] # Remove duplicate rows

lic21_no_duplications <- lic21_no_duplications %>% count(Landings.Number)

ggplot(lic21_no_duplications, aes(x=n)) + 
  geom_histogram(color="black", fill="white", binwidth=1) +
  scale_x_continuous(breaks = seq(1, 12, 1)) +
  xlab("Licence Count") +
  ylab("Number of Individuals") +
  theme_dark() +
  theme(axis.text.x = element_text(angle = 0, vjust = .5)) 

# plot by region

select_columns <- lic21[, c(3, 30)]
lic21_no_duplications_regions <- merge(lic21_no_duplications, select_columns, by="Landings.Number", all = T)
lic21_no_duplications_regions <- lic21_no_duplications_regions[!duplicated(lic21_no_duplications_regions), ] # Remove duplicate rows

color_region <- colorRampPalette(c("#233749", "#e37a55"))(3)  # set colors

lic_by_region <- ggplot(lic21_no_duplications_regions, aes(x=n, fill = Region)) + 
  geom_histogram(bins = 30) +
  scale_x_continuous(breaks = seq(1, 12, 1)) +
  xlab("Licence Count") +
  ylab("Number of Individuals") +
  ggtitle("License diversification among harvesters (2021)") +
  scale_fill_manual(values = color_region) +  
  theme_classic() +
  theme(axis.text.x = element_text(angle = 0, vjust = .5))

ggsave("1.b.1.Fisheries_Access.pdf", width = 15, height = 9)

# Role of Aquaculture in Diversification 

select_FishTable <- FishTable[, c(2:99)]

lic2021 = as.matrix(select_FishTable)
lic21row = lic2021 %*% t(lic2021)
lic21col = t(lic2021) %*% lic2021

licnet = graph.adjacency(lic21col,mode="undirected",weighted=TRUE,diag=FALSE)

score <- strength(licnet, vids = V(licnet), mode = c("all", "out", "in", "total"),
                  loops = TRUE, weights = NULL)

score <- as.data.frame(score, stringsAsFactors=FALSE)
score <- rename(score, Strength = score) # Change column name 

score$Type <- rownames(score)

#score <- setDT(score, keep.rownames = TRUE)[] # Make row name a column 

score$Strength <- score[order(-score$Strength),] 
score <- as.data.frame(score, stringsAsFactors=FALSE)

score <- score[, c(1)]
score <- as.data.frame(score, stringsAsFactors=FALSE)

#fill=factor(score$Type, levels=unique(score$Type))
#the_factor <- fill=factor(score$Type, levels=unique(score$Type))
#fill=factor(Type, levels=order(Type))

score$Type <- factor(score$Type, levels = score$Type[order(-score$Strength)]) # order 

ggplot(score[which(score$Strength > 0),], aes(x=Type, y = Strength)) + 
  geom_bar(stat="identity", fill = '#233749') +
  xlab("Licence Type") +
  ylab("Network strength") +
  ggtitle("Role of License Type in Creating Diverisification (2021)") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 0, vjust = .5)) +
  theme(axis.text.x = element_text(angle = 90)) 

ggsave("1.b.2.Fisheries_Access.pdf", width = 15, height = 9)

