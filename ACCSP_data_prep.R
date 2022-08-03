#loading packages 
library(dplyr)   
library(tidyr)


#reading in data
lands <- read.csv("/Users/jstoll/tmp/seafood_systems/New_England_Food_Project/Data/original/ACCSP_Landings_Nonconfidential_Commercial_2007_2020.csv", stringsAsFactors = FALSE)



#####################
####data cleaning####
#####################

#removing coast, region, and % display
lands <- lands[ , c(1, 4:8)]

# 


### Fill in data gaps

# Add alewife data 
lands$Pounds[11]<- "1151395" # 2011
lands$Pounds[13]<- "1606535" # 2012
lands$Pounds[14]<- "1423773" # 2013
lands$Pounds[16]<- "1720285" # 2014
lands$Pounds[17]<- "1295998" # 2015
lands$Pounds[18]<- "1292662" # 2016
lands$Pounds[19]<- "1682174" # 2017
lands$Pounds[20]<- "1963269" # 2018
lands$Pounds[21]<- "2154678" # 2019
lands$Pounds[22]<- "1576645" # 2020

lands$Live.Pounds[11]<- "1151395" # 2011
lands$Live.Pounds[13]<- "1606535" # 2012
lands$Live.Pounds[14]<- "1423773" # 2013
lands$Live.Pounds[16]<- "1720285" # 2014
lands$Live.Pounds[17]<- "1295998" # 2015
lands$Live.Pounds[18]<- "1292662" # 2016
lands$Live.Pounds[19]<- "1682174" # 2017
lands$Live.Pounds[20]<- "1963269" # 2018
lands$Live.Pounds[21]<- "2154678" # 2019
lands$Live.Pounds[22]<- "1576645" # 2020

lands$Dollars[11]<- "290046" # 2011
lands$Dollars[13]<- "426320" # 2012
lands$Dollars[14]<- "350785" # 2013
lands$Dollars[16]<- "482212" # 2014
lands$Dollars[17]<- "415433" # 2015
lands$Dollars[18]<- "398861" # 2016
lands$Dollars[19]<- "569623" # 2017
lands$Dollars[20]<- "677530" # 2018
lands$Dollars[21]<- "818193" # 2019
lands$Dollars[22]<- "586668" # 2020

# Add eel data

lands$Pounds[1385]<- "9400" # 2016
lands$Pounds[1389]<- "9343" # 2017
lands$Pounds[1393]<- "9194" # 2018
lands$Pounds[1397]<- "9620" # 2019
lands$Pounds[1400]<- "9652" # 2020

lands$Live.Pounds[1385]<- "9400" # 2016
lands$Live.Pounds[1389]<- "9343" # 2017
lands$Live.Pounds[1393]<- "9194" # 2018
lands$Live.Pounds[1397]<- "9620" # 2019
lands$Live.Pounds[1400]<- "9652" # 2020

lands$Dollars[1385]<- "13446828" # 2016
lands$Dollars[1389]<- "12166417" # 2017
lands$Dollars[1393]<- "21753350" # 2018
lands$Dollars[1397]<- "20119194" # 2019
lands$Dollars[1400]<- "5067521" # 2020

# Add bloodworm data

lands$Pounds[3264]<- "393509" # 2019
lands$Live.Pounds[3264]<- "393509" # 2019
lands$Dollars[3264]<- "6286131" # 2019

# Add sandworms data

lands$Pounds[3272]<- "255440" # 2013
lands$Pounds[3276]<- "201542" # 2017
lands$Pounds[3278]<- "208676" # 2019
lands$Pounds[3279]<- "235998" # 2020

lands$Live.Pounds[3272]<- "255440" # 2013
lands$Live.Pounds[3276]<- "201542" # 2017
lands$Live.Pounds[3278]<- "208676" # 2019
lands$Live.Pounds[3279]<- "235998" # 2020

lands$Dollars[3272]<- "1371262" # 2013
lands$Dollars[3276]<- "1533774" # 2017
lands$Dollars[3278]<- "2102155" # 2019
lands$Dollars[3279]<- "1873423" # 2020

# Add salmon data

lands$Pounds[3472]<- "24530940" # 2010
lands$Live.Pounds[3472]<- "24530940" # 2010
lands$Dollars[3472]<- "76781842" # 2010

# Add seaweed data
lands$Pounds[3750]<- "1151395" # 2010
lands$Pounds[3751]<- "1151395" # 2011
#lands$Pounds[]<- "1606535" # 2012
#lands$Pounds[]<- "1423773" # 2013
lands$Pounds[3752]<- "1720285" # 2014
#lands$Pounds[]<- "1295998" # 2015
lands$Pounds[3753]<- "1292662" # 2016
lands$Pounds[3754]<- "1682174" # 2017
lands$Pounds[3755]<- "1963269" # 2018
lands$Pounds[3756]<- "2154678" # 2019
lands$Pounds[3757]<- "1576645" # 2020

lands$Live.Pounds[3750]<- "1151395" # 2010
lands$Live.Pounds[3751]<- "1151395" # 2011
#lands$Live.Pounds[]<- "1606535" # 2012
#lands$Live.Pounds[]<- "1423773" # 2013
lands$Live.Pounds[3752]<- "1720285" # 2014
#lands$Live.Pounds[]<- "1295998" # 2015
lands$Live.Pounds[3753]<- "1292662" # 2016
lands$Live.Pounds[3754]<- "1682174" # 2017
lands$Live.Pounds[3755]<- "1963269" # 2018
lands$Live.Pounds[3756]<- "2154678" # 2019
lands$Live.Pounds[3757]<- "1576645" # 2020

lands$Dollars[3750]<- "290046" # 2010
lands$Dollars[3751]<- "290046" # 2011
#lands$Dollars[]<- "426320" # 2012
#lands$Dollars[]<- "350785" # 2013
lands$Dollars[3752]<- "482212" # 2014
#lands$Dollars[]<- "415433" # 2015
lands$Dollars[3753]<- "398861" # 2016
lands$Dollars[3754]<- "569623" # 2017
lands$Dollars[3755]<- "677530" # 2018
lands$Dollars[3756]<- "818193" # 2019
lands$Dollars[3757]<- "586668" # 2020

lands[nrow(lands) + 1,] <- c(2012, "MAINE", "SEAWEED", 1606535, 1606535, 426320)
lands[nrow(lands) + 1,] <- c(2013, "MAINE", "SEAWEED", 1423773, 1423773, 350785)
lands[nrow(lands) + 1,] <- c(2015, "MAINE", "SEAWEED", 1295998, 1295998, 415433) 

# Revise Maine's menhaden data 

lands$Pounds[2907]<- "6133104" # 2016
lands$Pounds[2910]<- "14816783" # 2017
lands$Pounds[2917]<- "25298442" # 2018
lands$Pounds[2921]<- "26972192" # 2019
lands$Pounds[2926]<- "22129430" # 2020

lands$Dollars[2907]<- "1938050" # 2016
lands$Dollars[2910]<- "3230476" # 2017
lands$Dollars[2917]<- "4445032" # 2018
lands$Dollars[2921]<- "6855802" # 2019
lands$Dollars[2926]<- "7093687" # 2020

lands$Live.Pounds[2907]<- "6133104" # 2016
lands$Live.Pounds[2910]<- "14816783" # 2017
lands$Live.Pounds[2917]<- "25298442" # 2018
lands$Live.Pounds[2921]<- "26972192" # 2019
lands$Live.Pounds[2926]<- "22129430" # 2020


#converting * to 0
lands <- lands %>%                               
  mutate(Pounds = replace(Pounds, Pounds == "*", 0),
         Live.Pounds = replace(Live.Pounds, Live.Pounds == "*", 0),
         Dollars = replace(Dollars, Dollars == "*", 0))

#making lbs, live.lbs, and dollars numeric
lands$Pounds <- as.numeric(lands$Pounds)
lands$Live.Pounds <- as.numeric(lands$Live.Pounds)
lands$Dollars <- as.numeric(lands$Dollars)


###################
####writing csv####
###################

#the clean file lives in seafood_systems -> New_England_Food_Project -> Data -> Processed_Data

write.csv(lands, "ACCSP_Landings_Nonconfidential_Commercial_2007_2020_clean.csv", row.names = FALSE)
