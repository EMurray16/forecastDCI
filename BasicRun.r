#This reads the data and runs the 2018 DCI Model

#Load the libraries
Libraries = c("tictoc")
lapply(Libraries, library, character.only=T)
source("go/src/github.com/EMurray16/forecastDCI/DCI_Library_FullScoreOnly.r")

#Start by reading the data
ScoreList = ScoreParse("ScoreHistory/2008.csv")
#Also read the csv into an external object to get the most recent day 
ScoreTable = read.csv("ScoreHistory/2008.csv")
MaxDay = max(ScoreTable$Day)

#Now find the exponentials for all corps
AllExponentials = lapply(ScoreList, ExpFitter, BaseDay=MaxDay-3, PrelimsDay=MaxDay-2)
#Clean the exponential list
CleanExp = AllExponentials[!sapply(AllExponentials, is.null)]

#Predict World Class Finals, Day 52 and Open Class Finals (Day 48)
tic(); Pred = Predictor(CleanExp, MaxDay-1, 5000, 44, 0.317); toc()

#Reduce each prediction to a readable data frame
Frame = PredictionReduce(Pred)
