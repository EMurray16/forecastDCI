#This contains all of the functions used in the model

ScoreParse <- function(filename) {
	#Start by loading the file
	BigTable = read.csv(filename, stringsAsFactors=F)
	
	#Adjust the table to have a minimum day of 0
	BigTable$Day = (BigTable$Day - min(BigTable$Day)) + 1
	
	#Get a list of all the corps in the file
	CorpsNames = unique(BigTable$Corps)
	
	#This function creates a data frame for a given corps
	FrameCreate <- function(CorpsName, Table) {
		#Start by finding all the rows which have data
		CorpsRows = which(Table$Corps == CorpsName)
		if (length(CorpsRows) == 0) {
			return(NULL)
		}
		
		#Get vectors for the scores and Day
		ScoreVec = Table[CorpsRows,"Score"]
		DayVec = Table[CorpsRows,"Day"]
		
		#Now make the data frame
		OutFrame = data.frame(Day=DayVec, Score=ScoreVec)
		return(OutFrame)
	}
	
	#Now run the function on all the world class and open class corps
	AllFrames = lapply(CorpsNames, FrameCreate, Table=BigTable)
	
	#Add the corps names to the frames
	names(AllFrames) = CorpsNames
	return(AllFrames)
}

#Create a function that fits the exponential curve to a data frame
ExpFitter <- function(CorpsFrame, BaseDay) {
	#Start by checking the number of shows
	if (is.null(CorpsFrame)) { return(NULL) }
	if (nrow(CorpsFrame) < 6) { return(NULL) }
	
	#The weight vector reduces weights of recent scores, based on their correlation with finals week scores
	## TODO: Find a reliable way to make this dynamic
	WeightVec = 0.65 + 3.6e-6 * (1:70)^2.86
	WeightVec = c(WeightVec, 1,1,1)
	
	#Create a vector for the corps-specific day weights
	BaseWeight = WeightVec[BaseDay]
	#print(c(BaseDay, BaseWeight))
	#Create the linear increase in weights from BaseWeight to 1 over the 1.5-week window
	Wline = seq(from=1, to=BaseWeight, length.out=10)
	#All weights are 1 before the 1.5-week window
	SpecWeights = c(rep(1,BaseDay-10), Wline)
	
	#Pull out the individual vectors to match nls input better
	N = sum(CorpsFrame$Day <= BaseDay)
	Score = CorpsFrame$Score[CorpsFrame$Day <= BaseDay]
	Day = CorpsFrame$Day[CorpsFrame$Day <= BaseDay]
	
	#Now fit the curve for each caption, wrapped in a try to avoid catastrophic errors
	#This tryCatch won't make the objects if we're not successful
	tryCatch({ 
		ScoreModel = nls(Score ~ a + Day^b, data=data.frame(Score, Day), start=list(a=Score[1], b=0.8), 
			control=nls.control(warnOnly=TRUE), weights=SpecWeights[Day])
	}, warning = function(w) { #This makes it so scenarios of non-convergence don't return bad coefficients
		#print("Can't run corps due to non-convergence")
	}, error = function(e) {
		#print("Can't run corps due to an error in curve fitting")
	}) 
	
	#Check to make sure the model exists (trycatch won't write to the variable if it doesn't)
	ExistBool = exists("ScoreModel")
	
	#Create a copy of CorpsFrame we can modify
	CorpsFrame2 = CorpsFrame
	TrimmedResult = NA #This tracks if we're successful in the if loop
	
	if (!ExistBool) { #This means we didn't get a good fit
		#print("Nonconvergence!")
		#Try removing the most recent scores until we get convergence or too small a sample
		CorpsFrame2 = CorpsFrame2[1:(nrow(CorpsFrame2)-1) ,]
		
		if (nrow(CorpsFrame) < 6) { #Check for sample size
			break
		} else {
			#Recursion!
			TrimmedResult = ExpFitter(CorpsFrame2, BaseDay)
			#No matter what, we can return TrimmedResult because this is the top level of the recursion
			return(TrimmedResult)
		}
	}
	
	#Pull out the summary of the models
	Scoef = summary(ScoreModel)$coefficients
	
	#Now pull out the coefficients
	a = Scoef[1,1]
	b = Scoef[2,1]
	
	#And now pull out the standard errors
	aSE = Scoef[1,2]
	bSE = Scoef[2,2]
	
	#Put the coefficients and standard errors into a 1-row data frame
	#	That way, we can refer to the coefficients by name
	OutFrame = data.frame(a=a, b=b, aSE=aSE, bSE=bSE, N=N)
	
	#return the vector
	return(OutFrame)
}

#Now make a function that predicts a day for all corps
Predictor <- function(CorpsList, PredictDay, Nmonte, RankDay, ExpWeight) {
	#Start by making a function that predicts N scores based on the exponential uncertainty
	ExpPredict <- function(CorpsCoefList, PredictDay, Nmonte) {
		#Start by gathering basic information
		Ncorps = length(CorpsCoefList)
		
		#This function returns a list of N predictions
		ScoreList = vector(mode='list', length=Ncorps)
		
		#Loop through each corps and make the random vectors
		for (C in 1:Ncorps) {
			CorpsCoefs = CorpsCoefList[[C]]
			#Recall the exponential is of the form Score = a + Day^b
			
			#Make a vector of random a's and b's for each caption
			randa = rnorm(Nmonte, mean=as.numeric(CorpsCoefs["a"]), sd=as.numeric(CorpsCoefs["aSE"]))
			randb = rnorm(Nmonte, mean=as.numeric(CorpsCoefs["b"]), sd=as.numeric(CorpsCoefs["bSE"])) 
			
			#Now create the score for each caption
			PredScore = randa + PredictDay ^ randb
			
			#Now correct each score to make sure they're actually possible
			PredScore[PredScore > 100] = 100
			
			#Now get the total score list
			ScoreList[[C]] = PredScore
		}
		
		#Convert each simulation score to gaps
		BaseScores = vector(mode='double', length=Nmonte)
		for (i in 1:Nmonte) {
			BaseScores[i] = max(sapply(ScoreList, '[[', i))
		}
		#Now loop through each corps and subtract the base score
		for (C in 1:length(ScoreList)) {
			ScoreList[[C]] = ScoreList[[C]] - BaseScores
		}		
		return(ScoreList)
	}
	
	#Now create a function that predicts N scores based on the random error
	RandPredict <- function(CorpsCoefList, PredictDay, Nmonte, RankDay, Damp) {
		library(MASS)
		
		#Get the basic information
		Ncorps = length(CorpsCoefList)
		#The output will be the score list
		ScoreList = vector(mode='list', length=Ncorps)
		
		#Make a correlation matrix for the errors
		CorrMat = matrix(nrow=Ncorps, ncol=Ncorps)
		for (Row in 1:Ncorps) {
			for (Col in 1:Ncorps) {
				IndexDiff = abs(Row - Col)
				#Use IndexDiff to determine the correlation
				if (IndexDiff == 0) { 
					#This is a diag
					CorrMat[Row,Col] = 1
				} else if (IndexDiff > 14) { 
					#This is a background correlation
					CorrMat[Row,Col] = 0.263
				} else { 
					#This is an off-diag. The adjancent correlation should by 0.514
					CorrMat[Row,Col] = 0.513+(0.25/14) - IndexDiff*(0.25/14)
				}
			}
		}
		
		#2 is the historical noise magnitude (variance) in overall scores uncertainty
		ScoreCov = CorrMat * 2
		
		#Draw the random numbers using the covariance matrices, 
		ScoreRand = mvrnorm(Nmonte, mu=rep(0,Ncorps), Sigma=ScoreCov, empirical=F)
		
		#We choose the column of random numbers for each corps based on their rank
		ScoreNonrand = vector(mode='double', length=Ncorps)
		
		#Fill in the vectors
		for (C in 1:Ncorps) {
			ScoreNonrand[C] = as.numeric(CorpsCoefList[[C]]["a"]) + RankDay ^ as.numeric(CorpsCoefList[[C]]["b"])
		}
		#Convert these to gaps
		ScoreNonrand = ScoreNonrand - max(ScoreNonrand) 
		
		#Get vectors of rank for each caption
		Ranks = match(ScoreNonrand, sort(ScoreNonrand, decreasing=T)) #Ranks from highest to lowest
		
		#Now loop through each corps and fill in ScoreList
		for (C in 1:Ncorps) {
			#Get the scores and put them into the list
			ScoreVec = ScoreNonrand[C] + ScoreRand[,Ranks[C]]
			ScoreList[[C]] = ScoreVec
		}
		
		return(ScoreList)
	}
	
	#Get the number of corps
	Ncorps = length(CorpsList)
	
	#Run both score predictors
	ExpScoreList = ExpPredict(CorpsList, PredictDay, Nmonte)
	RandScoreList = RandPredict(CorpsList, PredictDay, Nmonte, RankDay, Damp)
	
	#Now create the overall score and rank lists to return
	ScoreList = vector(mode='list', length=Ncorps)
	RankList = vector(mode='list', length=Ncorps)
	
	#Fill in the final ScoreList
	for (C in 1:Ncorps) {
		ScoreList[[C]] = ExpWeight*ExpScoreList[[C]] + (1-ExpWeight)*RandScoreList[[C]]
	}
	
	#Fill in RankList by sorting the scores
	for (n in 1:Nmonte) {
		#Get the scores and rank them
		scores = sapply(ScoreList, '[[', n)
		ranks = match(scores, sort(scores, decreasing=T))
		#Now put the ranks into the list
		for (C in 1:Ncorps) {
			RankList[[C]][n] = ranks[C]
		}
	}
	
	#Retain the names in the lists
	names(ScoreList) = names(CorpsList)
	names(RankList) = names(CorpsList)
	
	#return the lists
	return(list(ScoreList, RankList))
}

#this function returns a vector of information for each corps 
	#mean score,percent odds of being in 1st, second, thrid, top12
PredictionReduce <- function(PredictionList) {
	ScoreList = PredictionList[[1]]
	RankList = PredictionList[[2]]
	
	#Get the simple information
	Ncorps = length(PredictionList[[1]])
	Nmonte = length(PredictionList[[1]][[1]])
	CorpsNames = names(PredictionList[[1]])
	
	#create a vector for each returned value
	MeanScores = vector(mode='double', length=Ncorps)
	PercGold = vector(mode='double', length=Ncorps)
	PercSilver = vector(mode='double', length=Ncorps)
	PercBronze = vector(mode='double', length=Ncorps)
	PercFinals = vector(mode='double', length=Ncorps)
	
	#Loop through each corps and fill the vectors in
	for (C in 1:Ncorps) {
		MeanScores[C] = mean(ScoreList[[C]])
		PercGold[C] = sum(RankList[[C]] == 1) / Nmonte
		PercSilver[C] = sum(RankList[[C]] == 2) / Nmonte
		PercBronze[C] = sum(RankList[[C]] == 3) / Nmonte
		PercFinals[C] = sum(RankList[[C]] <= 12) / Nmonte
	}
	
	#Adjust the gaps to make sure 1st place is a 0
	MeanScores = MeanScores - max(MeanScores)
	
	#Now format these into a data frame
	OutFrame = data.frame(Mean=MeanScores, Gold=PercGold, Silver=PercSilver, Bronze=PercBronze, Finals=PercFinals)
	row.names(OutFrame) = CorpsNames
	
	#Sort the data frame by percent chance of gold
	OutFrame = OutFrame[order(OutFrame$Mean, decreasing=T),]
	
	#Now return the data frame
	return(OutFrame)
}