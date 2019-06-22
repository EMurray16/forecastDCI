#This reads the data and runs the 2018 DCI Model

#Load the libraries
Libraries = c("tictoc")
lapply(Libraries, library, character.only=T)
source("DCI_Library_CaptionScore.r")

#Start by reading the data
ScoreList = ScoreParse("DCI_Scores_2018.csv")
#Also read the csv into an external object to get the most recent day 
ScoreTable = read.csv("DCI_Scores_2018.csv")
MaxDay = max(ScoreTable$Day)

#Now find the exponentials for all corps
AllExponentials = lapply(ScoreList, ExpFitter, BaseDay=MaxDay)
#Clean the exponential list
CleanExp = AllExponentials[!sapply(AllExponentials, is.null)]

#Separate the Open Class corps from World for predicting OC Finals
OpenExp = AllExponentials[OpenClass]

#Predict World Class Finals, Day 52 and Open Class Finals (Day 48)
tic(); Pred52 = Predictor(CleanExp, 52, 5000, 45); toc()
#tic(); Pred48 = Predictor(OpenExp, 48, 5000, MaxDay, Damp=FALSE); toc()


#Reduce each prediction to a readable data frame
Frame52 = PredictionReduce_WorldClass(Pred52)
#Frame48 = PredictionReduce_OpenClass(Pred48)