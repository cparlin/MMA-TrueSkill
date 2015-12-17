#Set Libraries
#library(plyr)
library(dplyr)
source("SupportFunctions.R")

#Global Default values
StartDate = as.Date("2003-07-06")
EndDate = as.Date("2015-11-26")
initialMu = 25
initialSigma = initialMu / 3
Beta = 13
Tau = initialSigma / 100
DrawProbability = 0.0166
DrawMargin = InverseCumulative((DrawProbability+1)/2) * sqrt(2) * Beta

#initialize empty table for fight predictions
CompletePredictions = data.frame(matrix(vector(), 0, 15,
              dimnames=list(c(), c("Event", "Organization", "Date", "FightCount", "Fighter1", "Fighter2", "Result1",
                                   "Result2", "mu1","sigma1","mu2","sigma2", "FightPrediction", "NewMu", "NewSigma"))),
                stringsAsFactors=F)

#import bout data
BoutData <- read.csv("~/UCL Classwork/Programming for Business Analytics/Elo_MMA/data/MMA_raw_Complete.csv", header=FALSE)
#Rename variables
names(BoutData)[1]="Event"
names(BoutData)[2]="Organization"
names(BoutData)[3]="Date"
names(BoutData)[4]="Fighter1"
names(BoutData)[5]="Result1"
names(BoutData)[6]="Fighter2"
names(BoutData)[7]="Result2"

#Remove bad results
BoutData <- BoutData[BoutData$Result1 != "NC" & BoutData$Result1 != "yet to come",]
BoutData <- BoutData[BoutData$Fighter1 != "Unknown Fighter" & BoutData$Fighter2 != "Unknown Fighter",]

#Format date properly
BoutData$Date <- as.Date(BoutData$Date, "%b %d %Y")

#Create mirror set for fighter in 2nd row
BoutDataReversed <- data.frame(BoutData$Event, BoutData$Organization, BoutData$Date, BoutData$Fighter2, BoutData$Result2, BoutData$Fighter1, BoutData$Result1)
names(BoutDataReversed)[1]="Event"
names(BoutDataReversed)[2]="Organization"
names(BoutDataReversed)[3]="Date"
names(BoutDataReversed)[4]="Fighter1"
names(BoutDataReversed)[5]="Result1"
names(BoutDataReversed)[6]="Fighter2"
names(BoutDataReversed)[7]="Result2"

#Join all fights together
AllFights <- rbind(BoutData, BoutDataReversed)

#Generate Current Ratings
CurrentRatings <- data.frame(unique(AllFights$Fighter1))
names(CurrentRatings)[1]="Name"
CurrentRatings$mu <- initialMu
CurrentRatings$sigma <- initialSigma

#Set analysis date
today <- StartDate

while (today < EndDate) {

#Subset data for this date
TodaysFights <- AllFights[AllFights$Date == today,]

#TodaysFights <- ddply(TodaysFights, .(Fighter1), function(X) data.frame(X, FightCount=1:nrow(X)))
#TodaysFights$FightCount <- ave(TodaysFights$Date, TodaysFights$Fighter1,  FUN = seq_along)


if (nrow(TodaysFights)==0) {
  today <- today+1
  next
}

#Incremental counter for multiple fighters
TodaysFights <- (TodaysFights %>% group_by(Fighter1) %>% mutate(FightCount = 1:n()))

#Loop for fight counter
for (i in 1:max(TodaysFights$FightCount)) {
  #Split by counter
  PartialTodaysFights <- TodaysFights[TodaysFights$FightCount ==i,]
  
  #Get current fighter ratings
  TodaysFighterRatings <- CurrentRatings[CurrentRatings$Name %in% PartialTodaysFights$Fighter1 | CurrentRatings$Name %in% PartialTodaysFights$Fighter2,]
  PartialTodaysFights <- merge(PartialTodaysFights, TodaysFighterRatings, by.x=c("Fighter1"), by.y=c("Name"))
  names(PartialTodaysFights)[9]="mu1"
  names(PartialTodaysFights)[10]="sigma1"
  PartialTodaysFights <- merge(PartialTodaysFights,TodaysFighterRatings, by.x=c("Fighter2"), by.y=c("Name"))
  names(PartialTodaysFights)[11]="mu2"
  names(PartialTodaysFights)[12]="sigma2"
  
  #Calculate result prediction
  PartialTodaysFights$FightPrediction <- sapply(1:nrow(PartialTodaysFights), function(n) predictOutcome(PartialTodaysFights$mu1[n], PartialTodaysFights$mu2[n], PartialTodaysFights$sigma1[n], PartialTodaysFights$sigma2[n]))
  
  #Update ratings
  PartialTodaysFights$NewMu <- sapply(1:nrow(PartialTodaysFights), function(n) {
    GetNewMu(PartialTodaysFights$mu1[n], 
             PartialTodaysFights$mu2[n], 
             PartialTodaysFights$sigma1[n], 
             PartialTodaysFights$sigma2[n], 
             PartialTodaysFights$Result1[n])})
  PartialTodaysFights$NewSigma <- sapply(1:nrow(PartialTodaysFights), function(n) {
    GetNewSigma(PartialTodaysFights$mu1[n], 
             PartialTodaysFights$mu2[n], 
             PartialTodaysFights$sigma1[n], 
             PartialTodaysFights$sigma2[n], 
             PartialTodaysFights$Result1[n])})
  
  #Store predictions
  CompletePredictions <- rbind(CompletePredictions, PartialTodaysFights)
  
  #Update Ratings
  UpdatedRatings <- PartialTodaysFights[,c("Fighter1","NewMu","NewSigma")]
  names(UpdatedRatings)[1]="Name"
  names(UpdatedRatings)[2]="mu"
  names(UpdatedRatings)[3]="sigma"
  CurrentRatings <- CurrentRatings[!(CurrentRatings$Name %in% UpdatedRatings$Name),]
  CurrentRatings <- rbind(CurrentRatings, UpdatedRatings)
}
today <- today+1
print(today)
}

write.csv(CompletePredictions, file = "CompletePredictions.csv")

