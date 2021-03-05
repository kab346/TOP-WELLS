#TEST

#K.Brosky

#The purpose of this script is to optimize the creation of data frames

#measure time
start_time <- Sys.time()

#references libraries
library(tidyverse)
library(dslabs)
library(readxl)
library(dplyr)


#load data 
gasprods <-read_excel("C:/Users/KATIE BROSKY/Desktop/Super Senior Year Spring/R/OG_Production_Unconventional - original.xlsx")

#create data frame of necessary elements
gasprod<-data.frame("WELL_PERMIT_NUM"=gasprods$WELL_PERMIT_NUM, "GAS_QUANTITY"=gasprods$GAS_QUANTITY)

#omit na
gasprod[is.na(x = gasprod)] <- 0


#Create col vector of ID
WellID <- matrix(unique(gasprod$WELL_PERMIT_NUM), nrow = length(unique(gasprod$WELL_PERMIT_NUM)), ncol = 1)

#create col vector of zeros
zeros <- matrix(0, nrow = length(unique(gasprod$WELL_PERMIT_NUM)), ncol = 1)

#Create Matrix of col ID and totals

  for (x in 1:length(WellID)) {
    
    for (y in 1:length(gasprod$WELL_PERMIT_NUM)) {
      
      if (WellID[x] == gasprod$WELL_PERMIT_NUM[y]){
        
        zeros[x] = zeros[x] + gasprod$GAS_QUANTITY[y]
      }
      
     }
  }

#create data frame
Totals <- data.frame("WellID" = WellID, "Totals" = zeros)


#find high producing operators - 2 std above the mean
#omit na
zero<-na.omit(zeros)

#Define mean and Std Dev
c<-mean(zero)
d<-2*sd(zero)

#Define Data Frame
Top <- data.frame("Top Wells" = WellID, "Top Totals" = 0)


#print Well ID's and Gas Totals for those 2 std away from the mean - assume gaussian distribution

for (x in 1:length(WellID)) {
    
    if (zero[x] > (c+d)){
      
      Top$Top.Wells[x] <-  WellID[x]
      
      Top$Top.Totals[x] <-  zero[x] + Top$Top.Totals[x]
    
  }
}

#Remove zeros
Max <- Top[apply(Top!=0, 1, all),]

#export data
write.table(Max, "c:/Users/KATIE BROSKY/Desktop/Super Senior Year Spring/R/mydata.txt", sep="\t")

#measure time
end_time <- Sys.time()

print("TIME FOR CODE TO RUN")
print(end_time-start_time)
