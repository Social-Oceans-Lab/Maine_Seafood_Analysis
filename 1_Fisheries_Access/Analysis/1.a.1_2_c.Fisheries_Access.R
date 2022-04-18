# Fisheries Access
# 1.a.1, 1.a.2, 1.c.  Maine fishing licenses 
# Version 1.0. March 1, 2022

# Load packages 
library(dplyr)   
library(tidyr)
library(stringr)
library(tibble)
library(lubridate)
library(eeptools)

# Reading in data
lic1 <- read.csv("/Users/jstoll/OneDrive - University of Maine System/Social_Coasts_Lab/seafood_systems/Data/original_data/License_data/2016 All lobster commercial and noncommercial.csv", stringsAsFactors = FALSE)
lic2 <- read.csv("/Users/jstoll/OneDrive - University of Maine System/Social_Coasts_Lab/seafood_systems/Data/original_data/License_data/2016 All other licenses rec_comm_tribal.csv", stringsAsFactors = FALSE)
lic3 <- read.csv("/Users/jstoll/OneDrive - University of Maine System/Social_Coasts_Lab/seafood_systems/Data/original_data/License_data/2016-2020 Comm Fish_Pelagic_Menhaden.csv", stringsAsFactors = FALSE)
lic4 <- read.csv("/Users/jstoll/OneDrive - University of Maine System/Social_Coasts_Lab/seafood_systems/Data/original_data/License_data/2017 All lobster commercial and noncommercial.csv", stringsAsFactors = FALSE)
lic5 <- read.csv("/Users/jstoll/OneDrive - University of Maine System/Social_Coasts_Lab/seafood_systems/Data/original_data/License_data/2017 All other licenses rec_comm_tribal.csv", stringsAsFactors = FALSE)
lic6 <- read.csv("/Users/jstoll/OneDrive - University of Maine System/Social_Coasts_Lab/seafood_systems/Data/original_data/License_data/2018 All lobster commercial and noncommercial.csv", stringsAsFactors = FALSE)
lic7 <- read.csv("/Users/jstoll/OneDrive - University of Maine System/Social_Coasts_Lab/seafood_systems/Data/original_data/License_data/2018 All other licenses rec_comm_tribal.csv", stringsAsFactors = FALSE)
lic8 <- read.csv("/Users/jstoll/OneDrive - University of Maine System/Social_Coasts_Lab/seafood_systems/Data/original_data/License_data/2019 All lobster commercial and noncommercial.csv", stringsAsFactors = FALSE)
lic9 <- read.csv("/Users/jstoll/OneDrive - University of Maine System/Social_Coasts_Lab/seafood_systems/Data/original_data/License_data/2019 All other licenses rec_comm_tribal.csv", stringsAsFactors = FALSE)
lic10 <- read.csv("/Users/jstoll/OneDrive - University of Maine System/Social_Coasts_Lab/seafood_systems/Data/original_data/License_data/2020 All lobster commercial and noncommercial.csv", stringsAsFactors = FALSE)
lic11 <- read.csv("/Users/jstoll/OneDrive - University of Maine System/Social_Coasts_Lab/seafood_systems/Data/original_data/License_data/2020 All other licenses rec_comm_tribal.csv", stringsAsFactors = FALSE)
lic12 <- read.csv("/Users/jstoll/OneDrive - University of Maine System/Social_Coasts_Lab/seafood_systems/Data/original_data/License_data/2021 All lobster commercial and noncommercial.csv", stringsAsFactors = FALSE)
lic13 <- read.csv("/Users/jstoll/OneDrive - University of Maine System/Social_Coasts_Lab/seafood_systems/Data/original_data/License_data/2021 All other licenses rec_comm_tribal.csv", stringsAsFactors = FALSE)
lic14 <- read.csv("/Users/jstoll/OneDrive - University of Maine System/Social_Coasts_Lab/seafood_systems/Data/original_data/License_data/2021 Comm Fish_Pelagic_Menhaden.csv", stringsAsFactors = FALSE)

### Data cleaning & Data merge 

l1 <- as.data.frame(colnames(lic1)) # same as # 4,6
l4 <- as.data.frame(colnames(lic4)) 
l6 <- as.data.frame(colnames(lic6)) 
l8 <- as.data.frame(colnames(lic8)) 
l10 <- as.data.frame(colnames(lic10)) 
l12 <- as.data.frame(colnames(lic12)) 

l2 <- as.data.frame(colnames(lic2)) # same as # 3, 5, 7
l3 <- as.data.frame(colnames(lic3)) # same as above, but missing "Landings.Number" 
l5 <- as.data.frame(colnames(lic5)) 
l7 <- as.data.frame(colnames(lic7)) 
l9 <- as.data.frame(colnames(lic9)) # same as above, but missing "Permit.String" 
l11 <- as.data.frame(colnames(lic11)) 
l13 <- as.data.frame(colnames(lic13)) # same as above, but missing "Permit.String" 
l14 <- as.data.frame(colnames(lic14))

# Add missing columns 

lic3 <- lic3 %>%
  add_column(Landings.Number = NA) # Add "Landings.Number" column

lic9 <- lic9 %>%
  add_column(Permit.String = NA) # Add "Permit.String" column

lic13 <- lic13 %>%
  add_column(Permit.String = NA) # Add "Permit.String" column

lob_lic <- rbind(lic1, lic4, lic6, lic8, lic10, lic12)
other_lic <- rbind(lic2, lic3, lic5, lic7, lic9, lic11, lic13, lic14)

lob_lic_colnames <- as.data.frame(colnames(lob_lic)) 
other_lic_colnames <- as.data.frame(colnames(other_lic)) 

# Rename columns and add missing ones 

lob_lic <- rename(lob_lic, Address = ADDRESSLINE1..address) # Change column name 
lob_lic <- rename(lob_lic, Landings.Number = Associate.ID) # Change column name 
other_lic <- select(other_lic, -Address.type) # Remove column 
lob_lic <- rename(lob_lic, City = CITY..address) # Change column name 
other_lic <- other_lic %>%
  add_column(County = NA) # Add "County" column
other_lic <- other_lic %>%
  add_column(Declared.Zone = NA) # Add "Declared.Zone" column
other_lic <- other_lic %>%
  add_column(FederalPermit = NA) # Add "FederalPermit" column
other_lic <- other_lic %>%
  add_column(Group.Tag.Status = NA) # Add "Group.Tag.Status" column
lob_lic <- rename(lob_lic, First.name = FirstName) # Change column name 
other_lic <- rename(other_lic, Gender = Gender.Property) # Change column name 
lob_lic <- lob_lic %>%
  add_column(Landline = NA) # Add "Landline" column
lob_lic <- rename(lob_lic, Last.name = LastName) # Change column name 
lob_lic <- rename(lob_lic, Middle.name = MiddleName) # Change column name 
other_lic <- rename(other_lic, Middle.name = MI) # Change column name 
lob_lic <- lob_lic %>%
  add_column(Permit.String = NA) # Add "Permit.String" column
other_lic <- other_lic %>%
  add_column(Quantity = NA) # Add "Quantity" column
lob_lic <- rename(lob_lic, State = STATE.CODE) # Change column name 
lob_lic <- rename(lob_lic, Zip = ZIP.CODE..address) # Change column name 

# Merge data 

me_lic_2016_2021 <-rbind(lob_lic, other_lic)

# Clean data 

me_lic_2016_2021$First.name <- str_to_title(me_lic_2016_2021$First.name) 
me_lic_2016_2021$Middle.name <- str_to_title(me_lic_2016_2021$Middle.name) 
me_lic_2016_2021$Last.name <- str_to_title(me_lic_2016_2021$Last.name) 
me_lic_2016_2021$Address <- str_to_title(me_lic_2016_2021$Address) 
me_lic_2016_2021$City <- str_to_title(me_lic_2016_2021$City) 
me_lic_2016_2021$County <- str_to_title(me_lic_2016_2021$County) 

# Fix DMR formatting issue for DOB

me_lic_2016_2021 <- me_lic_2016_2021 %>%
  add_column(Age = NA) # Add "Quantity" column

me_lic_2016_2021$DOB <- as.Date(me_lic_2016_2021$DOB, format =  "%m/%d/%y")

for(i in 1:nrow(me_lic_2016_2021)) {
  if(! is.na(me_lic_2016_2021$DOB[i])) {
  if(me_lic_2016_2021$DOB[i] >= "2019-01-01") me_lic_2016_2021$DOB[i] <- me_lic_2016_2021$DOB[i] %m-% years(100)
  }
}

# Calculate age 

me_lic_2016_2021$Age <- as.numeric(difftime(Sys.Date(),me_lic_2016_2021$DOB, units = "weeks"))/52.25

### Fill in missing county data 

select_city <- me_lic_2016_2021[!duplicated(me_lic_2016_2021[,c('City')]),]
select_city <- select_city[, c(13, 14, 16)]

for(i in 1:nrow(select_city)) {
  if(select_city$City[i] == 'Waite') select_city$County[i] <- 'Washington'  
}

for(i in 1:nrow(select_city)) {
  if(select_city$City[i] == 'Pleasant Point') select_city$County[i] <- 'Washington'  
}

for(i in 1:nrow(select_city)) {
  if(select_city$City[i] == 'Houlton') select_city$County[i] <- 'Aroostook'  
}

for(i in 1:nrow(select_city)) {
  if(select_city$City[i] == 'Norridgewock') select_city$County[i] <- 'Somerset'  
}

for(i in 1:nrow(select_city)) {
  if(select_city$City[i] == 'Deblois') select_city$County[i] <- 'Washington'  
}

for(i in 1:nrow(select_city)) {
  if(select_city$City[i] == 'North Vassalboro') select_city$County[i] <- 'Kennebec'  
}

for(i in 1:nrow(select_city)) {
  if(select_city$City[i] == 'Meddybemps') select_city$County[i] <- 'Washington'  
}

for(i in 1:nrow(select_city)) {
  if(select_city$City[i] == 'Livermore') select_city$County[i] <- 'Androscoggin'  
}

for(i in 1:nrow(select_city)) {
  if(select_city$City[i] == 'Indian Island') select_city$County[i] <- 'Penobscot'  
}

for(i in 1:nrow(select_city)) {
  if(select_city$City[i] == 'Marion Township') select_city$County[i] <- 'Washington'  
}

for(i in 1:nrow(select_city)) {
  if(select_city$City[i] == 'Livermore Falls') select_city$County[i] <- 'Androscoggin'  
}

for(i in 1:nrow(select_city)) {
  if(select_city$City[i] == 'Stoneham') select_city$County[i] <- 'Oxford'  
}

for(i in 1:nrow(select_city)) {
  if(select_city$City[i] == 'Brownville') select_city$County[i] <- 'Piscataquis'  
}

for(i in 1:nrow(select_city)) {
  if(select_city$City[i] == 'China Village') select_city$County[i] <- 'Kennebec'  
}

for(i in 1:nrow(select_city)) {
  if(select_city$City[i] == 'Monson') select_city$County[i] <- 'Piscataquis'  
}

for(i in 1:nrow(select_city)) {
  if(select_city$City[i] == 'Rome') select_city$County[i] <- 'Kennebec'  
}

for(i in 1:nrow(select_city)) {
  if(select_city$City[i] == 'Ludlow') select_city$County[i] <- 'Aroostook'  
}

for(i in 1:nrow(select_city)) {
  if(select_city$City[i] == 'Clinton') select_city$County[i] <- 'Kennebec'  
}

for(i in 1:nrow(select_city)) {
  if(select_city$City[i] == 'Saint Albans') select_city$County[i] <- 'Somerset'  
}

for(i in 1:nrow(select_city)) {
  if(select_city$City[i] == 'Sumner') select_city$County[i] <- 'Oxford'  
}

for(i in 1:nrow(select_city)) {
  if(select_city$City[i] == 'Plymouth') select_city$County[i] <- 'Penobscot'  
}

for(i in 1:nrow(select_city)) {
  if(select_city$City[i] == 'East Dixfield') select_city$County[i] <- 'Oxford'  
}

for(i in 1:nrow(select_city)) {
  if(select_city$City[i] == 'Patten') select_city$County[i] <- 'Penobscot'  
}

for(i in 1:nrow(select_city)) {
  if(select_city$City[i] == 'Kents Hill') select_city$County[i] <- 'Kennebec'  
}

for(i in 1:nrow(select_city)) {
  if(select_city$City[i] == 'Detroit') select_city$County[i] <- 'Somerset'  
}

for(i in 1:nrow(select_city)) {
  if(select_city$City[i] == 'Coopers Mills') select_city$County[i] <- 'Lincoln'  
}

for(i in 1:nrow(select_city)) {
  if(select_city$City[i] == 'Rockwood') select_city$County[i] <- 'Somerset'  
}

for(i in 1:nrow(select_city)) {
  if(select_city$City[i] == 'Bradley') select_city$County[i] <- 'Penobscot'  
}

for(i in 1:nrow(select_city)) {
  if(select_city$City[i] == 'Fairfield') select_city$County[i] <- 'Somerset'  
}

for(i in 1:nrow(select_city)) {
  if(select_city$City[i] == 'Leeds') select_city$County[i] <- 'Androscoggin'  
}


for(i in 1:nrow(select_city)) {
  if(select_city$City[i] == 'Stetson') select_city$County[i] <- 'Penobscot'  
}

for(i in 1:nrow(select_city)) {
  if(select_city$City[i] == 'West Kennebunk') select_city$County[i] <- 'Kennebunk'  
}

for(i in 1:nrow(select_city)) {
  if(select_city$City[i] == 'Alexander') select_city$County[i] <- 'Washington'  
}

for(i in 1:nrow(select_city)) {
  if(select_city$City[i] == 'Danforth') select_city$County[i] <- 'Washington'  
}

for(i in 1:nrow(select_city)) {
  if(select_city$City[i] == 'West Forks') select_city$County[i] <- 'Somerset'  
}

for(i in 1:nrow(select_city)) {
  if(select_city$City[i] == 'Lexington') select_city$County[i] <- 'Somerset'  
}

for(i in 1:nrow(select_city)) {
  if(select_city$City[i] == 'Lincolnville Center') select_city$County[i] <- 'Waldo'  
}

for(i in 1:nrow(select_city)) {
  if(select_city$City[i] == 'Woodland') select_city$County[i] <- 'Aroostook'  
}

for(i in 1:nrow(select_city)) {
  if(select_city$City[i] == 'China') select_city$County[i] <- 'Kennebec'  
}

for(i in 1:nrow(select_city)) {
  if(select_city$City[i] == 'Madison') select_city$County[i] <- 'Somerset'  
}

for(i in 1:nrow(select_city)) {
  if(select_city$City[i] == 'Jay') select_city$County[i] <- 'Franklin'  
}

for(i in 1:nrow(select_city)) {
  if(select_city$City[i] == 'Chesterville') select_city$County[i] <- 'Franklin'  
}

for(i in 1:nrow(select_city)) {
  if(select_city$City[i] == 'Madawaska') select_city$County[i] <- 'Aroostook'  
}

for(i in 1:nrow(select_city)) {
  if(select_city$City[i] == 'Dover Foxcroft') select_city$County[i] <- 'Penobscot'  
}

for(i in 1:nrow(select_city)) {
  if(select_city$City[i] == 'Cambridge') select_city$County[i] <- 'Somerset'  
}

for(i in 1:nrow(select_city)) {
  if(select_city$City[i] == 'Pigeon Hill') select_city$County[i] <- 'Washington'  
}
for(i in 1:nrow(select_city)) {
  if(select_city$City[i] == 'Fort Kent') select_city$County[i] <- 'Aroostook'  
}

for(i in 1:nrow(select_city)) {
  if(select_city$City[i] == 'Springfield') select_city$County[i] <- 'Penobscot'  
}

for(i in 1:nrow(select_city)) {
  if(select_city$City[i] == 'Mexico') select_city$County[i] <- 'Oxford'  
}

for(i in 1:nrow(select_city)) {
  if(select_city$City[i] == 'Topsfield') select_city$County[i] <- 'Washington'  
}
for(i in 1:nrow(select_city)) {
  if(select_city$City[i] == 'Ocean Park') select_city$County[i] <- 'York'  
}

for(i in 1:nrow(select_city)) {
  if(select_city$City[i] == 'Sherman') select_city$County[i] <- 'Aroostook'  
}

for(i in 1:nrow(select_city)) {
  if(select_city$City[i] == 'Atkinson') select_city$County[i] <- 'Piscataquis'  
}

for(i in 1:nrow(select_city)) {
  if(select_city$City[i] == 'North Anson') select_city$County[i] <- 'Somerset'  
}
for(i in 1:nrow(select_city)) {
  if(select_city$City[i] == 'Limestone') select_city$County[i] <- 'Aroostook'  
}

for(i in 1:nrow(select_city)) {
  if(select_city$City[i] == 'Carrabasset Valley') select_city$County[i] <- 'Franklin'  
}

for(i in 1:nrow(select_city)) {
  if(select_city$City[i] == 'Athens') select_city$County[i] <- 'Somerset'  
}
for(i in 1:nrow(select_city)) {
  if(select_city$City[i] == 'Belgrade Lakes') select_city$County[i] <- 'Kennebec'  
}

for(i in 1:nrow(select_city)) {
  if(select_city$City[i] == 'Eustis') select_city$County[i] <- 'Franklin'  
}

for(i in 1:nrow(select_city)) {
  if(select_city$City[i] == 'Stratton') select_city$County[i] <- 'Franklin'  
}
for(i in 1:nrow(select_city)) {
  if(select_city$City[i] == 'Millinocket') select_city$County[i] <- 'Penobscot'  
}

for(i in 1:nrow(select_city)) {
  if(select_city$City[i] == 'East Wilton') select_city$County[i] <- 'Franklin'  
}

for(i in 1:nrow(select_city)) {
  if(select_city$City[i] == 'Sangerville') select_city$County[i] <- 'Piscataquis'  
}
for(i in 1:nrow(select_city)) {
  if(select_city$City[i] == 'Mapleton') select_city$County[i] <- 'Aroostook'  
}

for(i in 1:nrow(select_city)) {
  if(select_city$City[i] == 'Harmony') select_city$County[i] <- 'Somerset'  
}

for(i in 1:nrow(select_city)) {
  if(select_city$City[i] == 'Wales') select_city$County[i] <- 'Androscoggin'  
}


for(i in 1:nrow(me_lic_2016_2021)) {
  if(!is.na(me_lic_2016_2021$City[i])) {
    if(is.na(me_lic_2016_2021$County[i])) {
      city_temp <- me_lic_2016_2021$City[i]
      if(!is.na(select_city$County[which(select_city$City == city_temp)])) me_lic_2016_2021$County[i] <- select_city$County[which(select_city$City == me_lic_2016_2021$City[i])]
      if(is.na(select_city$County[which(select_city$City == city_temp)])) me_lic_2016_2021$County[i] <- NA
    }
  }
} 


### Define Regions

#Eastern
#Washington
#Hancock

#Midcoast
#Waldo
#Knox
#Lincoln
#Sagadahoc

#Southern 
#Cumberland
#York

for(i in 1:nrow(me_lic_2016_2021)) {
  if(!is.na(me_lic_2016_2021$County[i])) {
  if(me_lic_2016_2021$County[i] == 'Washington' | me_lic_2016_2021$County[i] == 'Hancock') me_lic_2016_2021$Region[i] <- 'Eastern'  
  }
}

for(i in 1:nrow(me_lic_2016_2021)) {
  if(!is.na(me_lic_2016_2021$County[i])) {
    if(me_lic_2016_2021$County[i] == 'Waldo' | me_lic_2016_2021$County[i] == 'Knox' | me_lic_2016_2021$County[i] == 'Lincoln' | me_lic_2016_2021$County[i] == 'Sagadahoc') me_lic_2016_2021$Region[i] <- 'Midcoaast'  
  }
}

for(i in 1:nrow(me_lic_2016_2021)) {
  if(!is.na(me_lic_2016_2021$County[i])) {
    if(me_lic_2016_2021$County[i] == 'Cumberland' | me_lic_2016_2021$County[i] == 'York') me_lic_2016_2021$Region[i] <- 'Southern'  
  }
}

# Remove duplicate rows

me_lic_2016_2021 <- me_lic_2016_2021[!duplicated(me_lic_2016_2021), ]

# Merge licenses into fisheries categories

ln <- nrow(me_lic_2016_2021)
Lic_Category	<- vector(length=ln, mode="numeric")

for(i in 1:nrow(me_lic_2016_2021)) {
  if(me_lic_2016_2021$License.Type[i] == 'LA' | 
     me_lic_2016_2021$License.Type[i] == 'LAO' | 
     me_lic_2016_2021$License.Type[i] == 'LAU' |
     me_lic_2016_2021$License.Type[i] == 'LC1' |
     me_lic_2016_2021$License.Type[i] == 'LC2' |
     me_lic_2016_2021$License.Type[i] == 'LC2O' |
     me_lic_2016_2021$License.Type[i] == 'LC3' |
     me_lic_2016_2021$License.Type[i] == 'LC3O' |
     me_lic_2016_2021$License.Type[i] == 'LCO' |
     me_lic_2016_2021$License.Type[i] == 'LCS' |
     me_lic_2016_2021$License.Type[i] == 'LCU' |
     me_lic_2016_2021$License.Type[i] == 'MILC1' |
     me_lic_2016_2021$License.Type[i] == 'NBAL' |
     me_lic_2016_2021$License.Type[i] == 'NBSL' |
     me_lic_2016_2021$License.Type[i] == 'PSUL' |
     me_lic_2016_2021$License.Type[i] == 'PLC3' |
     me_lic_2016_2021$License.Type[i] == 'PAL' 
     ) me_lic_2016_2021$Lic_Category[i] <- 'Lobster'
  
}

for(i in 1:nrow(me_lic_2016_2021)) {
  if(me_lic_2016_2021$License.Type[i] == 'E0' | 
     me_lic_2016_2021$License.Type[i] == 'E1' | 
     me_lic_2016_2021$License.Type[i] == 'E1C' |
     me_lic_2016_2021$License.Type[i] == 'E2' |
     me_lic_2016_2021$License.Type[i] == 'E6' |
     me_lic_2016_2021$License.Type[i] == 'E0C' |
     me_lic_2016_2021$License.Type[i] == 'E2C' |
     me_lic_2016_2021$License.Type[i] == 'E6C' | 
     me_lic_2016_2021$License.Type[i] == 'EP' |
     me_lic_2016_2021$License.Type[i] == 'PELD' |
     me_lic_2016_2021$License.Type[i] == 'NBED' |
     me_lic_2016_2021$License.Type[i] == 'NBEF' |
     me_lic_2016_2021$License.Type[i] == 'PELF' |
     me_lic_2016_2021$License.Type[i] == 'NBEF2' |
     me_lic_2016_2021$License.Type[i] == 'NBEF2C' |
     me_lic_2016_2021$License.Type[i] == 'MAEF' |
     me_lic_2016_2021$License.Type[i] == 'MAED' |
     me_lic_2016_2021$License.Type[i] == 'MIDF' |
     me_lic_2016_2021$License.Type[i] == 'MIDFC' |
     me_lic_2016_2021$License.Type[i] == 'MIEF' |
     me_lic_2016_2021$License.Type[i] == 'NBEDF' |
     me_lic_2016_2021$License.Type[i] == 'NBEDF2C'|
     me_lic_2016_2021$License.Type[i] == 'PELFC' |
     me_lic_2016_2021$License.Type[i] == 'PELDC'
  ) me_lic_2016_2021$Lic_Category[i] <- 'Elver & Eel'
  
}


for(i in 1:nrow(me_lic_2016_2021)) {
  if(me_lic_2016_2021$License.Type[i] == 'CFS' | 
     me_lic_2016_2021$License.Type[i] == 'CFC' |
     me_lic_2016_2021$License.Type[i] == 'NBCFS' |
     me_lic_2016_2021$License.Type[i] == 'NBSF' |
     me_lic_2016_2021$License.Type[i] == 'PSCF' |
     me_lic_2016_2021$License.Type[i] == 'PCFS' |
     me_lic_2016_2021$License.Type[i] == 'PCFC' 
  ) me_lic_2016_2021$Lic_Category[i] <- 'General'
  
}

for(i in 1:nrow(me_lic_2016_2021)) {
  if(me_lic_2016_2021$License.Type[i] == 'GC' 
  ) me_lic_2016_2021$Lic_Category[i] <- 'Green Crab'
  
}

for(i in 1:nrow(me_lic_2016_2021)) {
  if(me_lic_2016_2021$License.Type[i] == 'MD' |
     me_lic_2016_2021$License.Type[i] == 'MH' |
     me_lic_2016_2021$License.Type[i] == 'NBMD' |
     me_lic_2016_2021$License.Type[i] == 'NBMH' 
  ) me_lic_2016_2021$Lic_Category[i] <- 'Blue Mussel'
  
}


for(i in 1:nrow(me_lic_2016_2021)) {
  if(me_lic_2016_2021$License.Type[i] == 'SURF' |
     me_lic_2016_2021$License.Type[i] == 'MH' 
  ) me_lic_2016_2021$Lic_Category[i] <- 'Surf Clam'
  
}

for(i in 1:nrow(me_lic_2016_2021)) {
  if(me_lic_2016_2021$License.Type[i] == 'CPC' |
     me_lic_2016_2021$License.Type[i] == 'CPS' |
     me_lic_2016_2021$License.Type[i] == 'PCPS' 
  ) me_lic_2016_2021$Lic_Category[i] <- 'Anadromous & Pelagic'
  
}


for(i in 1:nrow(me_lic_2016_2021)) {
  if(me_lic_2016_2021$License.Type[i] == 'QM' 
  ) me_lic_2016_2021$Lic_Category[i] <- 'Quahog'
  
}

for(i in 1:nrow(me_lic_2016_2021)) {
  if(me_lic_2016_2021$License.Type[i] == 'SD' |
     me_lic_2016_2021$License.Type[i] == 'SDT' |
     me_lic_2016_2021$License.Type[i] == 'SDI' |
     me_lic_2016_2021$License.Type[i] == 'MISD' |
     me_lic_2016_2021$License.Type[i] == 'MISDT' |
     me_lic_2016_2021$License.Type[i] == 'NBSD' |     
     me_lic_2016_2021$License.Type[i] == 'NBSC' |
     me_lic_2016_2021$License.Type[i] == 'PSUS' |
     me_lic_2016_2021$License.Type[i] == 'PSDI' |
     me_lic_2016_2021$License.Type[i] == 'PSD'
  ) me_lic_2016_2021$Lic_Category[i] <- 'Scallop'
  
}

for(i in 1:nrow(me_lic_2016_2021)) {
  if(me_lic_2016_2021$License.Type[i] == 'SUH' |
     me_lic_2016_2021$License.Type[i] == 'SUWT' |
     me_lic_2016_2021$License.Type[i] == 'SUB' |
     me_lic_2016_2021$License.Type[i] == 'SUR' |
     me_lic_2016_2021$License.Type[i] == 'SUB' |
     me_lic_2016_2021$License.Type[i] == 'PSUH' |
     me_lic_2016_2021$License.Type[i] == 'PSUB' |
     me_lic_2016_2021$License.Type[i] == 'PSSU' 
  ) me_lic_2016_2021$Lic_Category[i] <- 'Green Urchin'
  
}

for(i in 1:nrow(me_lic_2016_2021)) {
  if(me_lic_2016_2021$License.Type[i] == 'SW' |
     me_lic_2016_2021$License.Type[i] == 'PSW'
  ) me_lic_2016_2021$Lic_Category[i] <- 'Seaweed'
  
}

for(i in 1:nrow(me_lic_2016_2021)) {
  if(me_lic_2016_2021$License.Type[i] == 'SCD' 
  ) me_lic_2016_2021$Lic_Category[i] <- 'Sea Cucumber'
  
}

for(i in 1:nrow(me_lic_2016_2021)) {
  if(me_lic_2016_2021$License.Type[i] == 'CS' |
     me_lic_2016_2021$License.Type[i] == 'CSO' |
     me_lic_2016_2021$License.Type[i] == 'CSU' |
     me_lic_2016_2021$License.Type[i] == 'MICS' |
     me_lic_2016_2021$License.Type[i] == 'NBCS' |
     me_lic_2016_2021$License.Type[i] == 'NBSS' |
     me_lic_2016_2021$License.Type[i] == 'PCS' |
     me_lic_2016_2021$License.Type[i] == 'PSSH'
  ) me_lic_2016_2021$Lic_Category[i] <- 'Shellfish'
  
}

for(i in 1:nrow(me_lic_2016_2021)) {
  if(me_lic_2016_2021$License.Type[i] == 'MWD' |
     me_lic_2016_2021$License.Type[i] == 'MIMWD' 
  ) me_lic_2016_2021$Lic_Category[i] <- 'Worm'
  
}


for(i in 1:nrow(me_lic_2016_2021)) {
  if(me_lic_2016_2021$License.Type[i] == 'MENC' 
  ) me_lic_2016_2021$Lic_Category[i] <- 'Menhaden'
  
}

for(i in 1:nrow(me_lic_2016_2021)) {
  if(me_lic_2016_2021$License.Type[i] == 'AL' 
  ) me_lic_2016_2021$Lic_Category[i] <- 'Aquaculture'
  
}

for(i in 1:nrow(me_lic_2016_2021)) {
  if(me_lic_2016_2021$License.Type[i] == 'TEN' 
  ) me_lic_2016_2021$Lic_Category[i] <- 'Tender'
  
}

for(i in 1:nrow(me_lic_2016_2021)) {
  if(me_lic_2016_2021$License.Type[i] == 'SWRO' 
  ) me_lic_2016_2021$Lic_Category[i] <- 'Recreational Saltwater Fishing Operation'
  
}

for(i in 1:nrow(me_lic_2016_2021)) {
  if(me_lic_2016_2021$License.Type[i] == 'CAR' |
    me_lic_2016_2021$License.Type[i] == 'DL' |
    me_lic_2016_2021$License.Type[i] == 'DS' 
  ) me_lic_2016_2021$Lic_Category[i] <- 'Other'
  
}

#for(i in 1:nrow(FishTable)) {
#  FishTable$SHRIMP[i] <- if (FishTable$CSC[i] == 1) 1 else if (FishTable$CSS[i] == 1) 1 else 0
#}


# Remove non-commercial licenses 
me_lic_2016_2021 <- me_lic_2016_2021[which(me_lic_2016_2021$License.Type != 'DL'), ]
me_lic_2016_2021 <- me_lic_2016_2021[which(me_lic_2016_2021$License.Type != 'SWR'), ]
me_lic_2016_2021 <- me_lic_2016_2021[which(me_lic_2016_2021$License.Type != 'LNC'), ]
me_lic_2016_2021 <- me_lic_2016_2021[which(me_lic_2016_2021$License.Type != 'GCN'), ]
me_lic_2016_2021 <- me_lic_2016_2021[which(me_lic_2016_2021$License.Type != 'MENNR'), ]
me_lic_2016_2021 <- me_lic_2016_2021[which(me_lic_2016_2021$License.Type != 'MENR'), ]
me_lic_2016_2021 <- me_lic_2016_2021[which(me_lic_2016_2021$License.Type != 'NCF'), ]
me_lic_2016_2021 <- me_lic_2016_2021[which(me_lic_2016_2021$License.Type != 'NCS'), ]
me_lic_2016_2021 <- me_lic_2016_2021[which(me_lic_2016_2021$License.Type != 'NCP'), ]
me_lic_2016_2021 <- me_lic_2016_2021[which(me_lic_2016_2021$License.Type != 'NLC1'), ]
me_lic_2016_2021 <- me_lic_2016_2021[which(me_lic_2016_2021$License.Type != 'NRS'), ]
me_lic_2016_2021 <- me_lic_2016_2021[which(me_lic_2016_2021$License.Type != 'NRSS'), ]
me_lic_2016_2021 <- me_lic_2016_2021[which(me_lic_2016_2021$License.Type != 'NTP'), ]
me_lic_2016_2021 <- me_lic_2016_2021[which(me_lic_2016_2021$License.Type != 'SPT'), ]
me_lic_2016_2021 <- me_lic_2016_2021[which(me_lic_2016_2021$License.Type != 'SWS'), ]
me_lic_2016_2021 <- me_lic_2016_2021[which(me_lic_2016_2021$License.Type != 'VH'), ]



### Create table of the number of license per year

Y21 <- me_lic_2016_2021[which(me_lic_2016_2021$License.Year == 2021), ]
Y21 <- Y21 %>% count(License.Year, License.Name, sort = TRUE)
Y21 <- rename(Y21, Y2021 = n) # Change column name 

Y20 <- me_lic_2016_2021[which(me_lic_2016_2021$License.Year == 2020), ]
Y20 <- Y20 %>% count(License.Year, License.Name, sort = TRUE)
Y20 <- rename(Y20, Y2020 = n) # Change column name 

Y19 <- me_lic_2016_2021[which(me_lic_2016_2021$License.Year == 2019), ]
Y19 <- Y19 %>% count(License.Year, License.Name, sort = TRUE)
Y19 <- rename(Y19, Y2019 = n) # Change column name 

Y18 <- me_lic_2016_2021[which(me_lic_2016_2021$License.Year == 2018), ]
Y18 <- Y18 %>% count(License.Year, License.Name, sort = TRUE)
Y18 <- rename(Y18, Y2018 = n) # Change column name 

Y17 <- me_lic_2016_2021[which(me_lic_2016_2021$License.Year == 2017), ]
Y17 <- Y17 %>% count(License.Year, License.Name, sort = TRUE)
Y17 <- rename(Y17, Y2017 = n) # Change column name 

Y16 <- me_lic_2016_2021[which(me_lic_2016_2021$License.Year == 2016), ]
Y16 <- Y16 %>% count(License.Year, License.Name, sort = TRUE)
Y16 <- rename(Y16, Y2016 = n) # Change column name 


working <- merge(Y21, Y20, by="License.Name", all = T)
working <- select(working, -License.Year.x)
working <- select(working, -License.Year.y)
working <- merge(working, Y19, by="License.Name", all = T)
working <- select(working, -License.Year)
working <- merge(working, Y18, by="License.Name", all = T)
working <- select(working, -License.Year)
working <- merge(working, Y17, by="License.Name", all = T)
working <- select(working, -License.Year)
working <- merge(working, Y16, by="License.Name", all = T)
LicTypes_2016_2021 <- select(working, -License.Year)

LicTypes_2016_2021 <- LicTypes_2016_2021[-c(1), ]   # Remove blank License.Name row
LicTypes_2016_2021 <- rename(LicTypes_2016_2021, License = License.Name) # Change column name 


write.csv(LicTypes_2016_2021, "LicTypes_2016_2021.csv") # write summary table to csv

# Plot numbers of licenses by region

color_region <- colorRampPalette(c("#233749", "#e37a55"))(3)  # set colors

me_lic_2021 <- me_lic_2016_2021[which(me_lic_2016_2021$License.Year == 2021), ]

lic_count <- ggplot(me_lic_2021, aes(fill=Region, x=License.Type, y= License.Year/2021)) + 
  theme_classic() +
  ylab("License Count") +
  xlab(" ") +
  ggtitle("Number of Licenses by Region (2021)") +
  labs(caption="Note: Plot does not include non-residential or recreational license categories.")+
  scale_fill_manual(values = color_region) +  
  scale_y_continuous(labels = scales::comma) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 8)) +
  geom_bar(stat="identity")

ggsave("1.a.1.Fisheries_Access.pdf", width = 15, height = 9)

# Plot numbers of licenses by region and fisheries category

lic_count_category <- ggplot(me_lic_2021, aes(fill=Region, x= Lic_Category, y= License.Year/2021)) + 
  theme_classic() +
  ylab("License Count") +
  xlab(" ") +
  ggtitle("Number of Licenses by Region (2021)") +
  labs(caption="Note: Plot does not include non-residential or recreational license categories.")+
  scale_fill_manual(values = color_region) +  
  scale_y_continuous(labels = scales::comma) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 8)) +
  geom_bar(stat="identity")

ggsave("1.a.3.Fisheries_Access.pdf", width = 15, height = 9)


### Age Distribution

# Age by year
p1 <- ggplot(me_lic_2016_2021, aes(x=as.factor(License.Year), y=Age)) + 
  ggtitle("Age of License Holders by Year (2016 - 2021)") +
  geom_violin(draw_quantiles = c(0.25, 0.5, 0.75)) +
  xlab("Year") +
  theme_dark()
  
# Age by region
p2 <- ggplot(me_lic_2016_2021[which(me_lic_2016_2021$License.Year == 2021),], aes(x=Region, y=Age)) + 
  ggtitle("Age of License Holders by Region (2021)") +
  geom_violin(draw_quantiles = c(0.25, 0.5, 0.75)) +
  xlab("Region") +
  theme_dark()

# Age by gender
p3 <- ggplot(me_lic_2016_2021[which(me_lic_2016_2021$License.Year == 2021),], aes(x=Gender, y=Age)) + 
  ggtitle("Age of License Holders by Gender (2021)") +
  geom_violin(draw_quantiles = c(0.25, 0.5, 0.75)) +
  xlab("Gender") +
  theme_dark()

# Age by AQ vs Lobster 

p4 <- ggplot(me_lic_2016_2021[which(me_lic_2016_2021$License.Year == 2021 & me_lic_2016_2021$License.Type == 'AL' | 
                                me_lic_2016_2021$License.Type == 'LC1' | 
                                me_lic_2016_2021$License.Type == 'LC2' |
                                me_lic_2016_2021$License.Type == 'LC3' 
                                ),], aes(x=as.factor(License.Type), y=Age)) + 
  ggtitle("Age of Aquaculture & Lobster License Holders (2021)") +
  geom_violin(draw_quantiles = c(0.25, 0.5, 0.75)) +
  xlab("License Type") +
  theme_dark()

plot_age <-
  ggarrange(p1, p2, p3, p4, labels = c("A", "B", "C", "D"),
            common.legend = TRUE, legend = "right")

ggsave("1.c.Fisheries_Access.pdf", width = 15, height = 9)


### Write data file

write.csv(me_lic_2016_2021, "me_lic_2016_2021.csv")




