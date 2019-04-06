#This contains all of the functions used in the model

#Hardcode the corps names by class as generally accessible variables
WorldClass = c("The Academy","Blue Devils","Blue Knights","Blue Stars","Bluecoats","Boston Crusaders","The Cadets","Carolina Crown",
	"The Cavaliers","Colts","Crossmen","Genesis","Jersey Surf","Madison Scouts","Mandarins","Music City","Oregon Crusaders",
	"Pacific Crest","Phantom Regiment","Pioneer","Santa Clara Vanguard","Seattle Cascades","Spirit of Atlanta","Troopers"
)
#Eventually this will only include 
OpenClass = c("7th Regiment","The Battalion","Blue Devils B","Blue Devils C","Colt Cadets","Columbians","Gold","Golden Empire",
	"Guardians","Heat Wave","Impulse","Incognito","Legends","Louisiana Stars","Raiders","River City Rhythm","Shadow",
	"Southwind","Spartans","Vanguard Cadets","Vessel","Watchmen"
)


ScoreParse <- function(filename, WorldNames=WorldClass, OpenNames=OpenClass) {
	#Start by loading the file
	BigTable = read.csv(filename, stringsAsFactors=F)
	
	#Eventually, we'll get rid of Open Class names that aren't going to Finals Week
	
	#This function creates a data frame for a given corps
	FrameCreate <- function(CorpsName, Table) {
		#Start by finding all the rows which have data
		CorpsRows = which(Table$Corps == CorpsName)
		if (length(CorpsRows) == 0) {
			return(NULL)
		}
		
		#Get vectors for the scores and Day
		GEvec = Table[CorpsRows,"GE"]
		VisVec = Table[CorpsRows,"Visual"]
		MusVec = Table[CorpsRows,"Music"]
		DayVec = Table[CorpsRows,"Day"]
		
		#Now make the data frame
		OutFrame = data.frame(Day=DayVec, GE=GEvec, Vis=VisVec, Mus=MusVec)
		return(OutFrame)
	}
	
	#Now run the function on all the world class and open class corps
	WorldFrames = lapply(WorldNames, FrameCreate, Table=BigTable)
	OpenFrames = lapply(OpenNames, FrameCreate, Table=BigTable)
	#Combine them into a single frame
	AllFrames = c(WorldFrames, OpenFrames)
	
	#Add the corps names to the frames
	names(AllFrames) = c(WorldNames, OpenNames)
	return(AllFrames)
}

#Create a function that fits the exponential curve to a data frame
ExpFitter <- function(CorpsFrame, BaseDay) {
	#Start by checking the number of shows
	if (is.null(CorpsFrame)) { return(NULL) }
	if (nrow(CorpsFrame) < 6) { return(NULL) }
	
	#The weight vector reduces weights of recent scores, based on their correlation with finals week scores
	WeightVec = 0.65 + 3.6e-6 * (1:49)^2.86
	WeightVec = c(WeightVec, 1,1,1)
	
	#Create a vector for the corps-specific day weights
	BaseWeight = WeightVec[BaseDay]
	#Create the linear increase in weights from BaseWeight to 1 over the 1.5-week window
	Wline = seq(from=1, to=BaseWeight, length.out=10)
	#All weights are 1 before the 1.5-week window
	SpecWeights = c(rep(1,BaseDay-10), Wline)
	
	#Pull out the individual vectors to match nls input better
	N = sum(CorpsFrame$Day <= BaseDay)
	GE = CorpsFrame$GE[CorpsFrame$Day <= BaseDay]
	Vis = CorpsFrame$Vis[CorpsFrame$Day <= BaseDay]
	Mus = CorpsFrame$Mus[CorpsFrame$Day <= BaseDay]
	Day = CorpsFrame$Day[CorpsFrame$Day <= BaseDay]
	
	#Now fit the curve for each caption, wrapped in a try to avoid catastrophic errors
	#This tryCatch won't make the objects if we're not successful
	tryCatch({ 
		GEModel = nls(GE ~ a + Day^b, data=data.frame(GE, Day), start=list(a=GE[1], b=0.5), 
			control=nls.control(warnOnly=TRUE), weights=SpecWeights[Day])
		VisModel = nls(Vis ~ a + Day^b, data=data.frame(Vis, Day), start=list(a=Vis[1], b=0.5), 
			control=nls.control(warnOnly=TRUE), weights=SpecWeights[Day])
		MusModel = nls(Mus ~ a + Day^b, data=data.frame(Mus, Day), start=list(a=Mus[1], b=0.5), 
			control=nls.control(warnOnly=TRUE), weights=SpecWeights[Day])
	}, warning = function(w) { #This makes it so scenarios of non-convergence don't return bad coefficients
		#print("Can't run corps due to non-convergence")
	}, error = function(e) {
		#print("Can't run corps due to an error in curve fitting")
	}) 
	
	#Check to see how many of the 3 models exist
	ExistVec = c(exists('GEModel'), exists('VisModel'), exists('MusModel'))
	
	#Create a copy of CorpsFrame we can modify
	CorpsFrame2 = CorpsFrame
	TrimmedResult = NA #This tracks if we're successful in the if loop
	
	if (sum(ExistVec) < 3) { #This means we didn't get a good fit
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
	GEcoef = summary(GEModel)$coefficients
	Vcoef = summary(VisModel)$coefficients
	Mcoef = summary(MusModel)$coefficients
	
	#Now pull out the coefficients
	aVec = c(GEcoef[1,1], Vcoef[1,1], Mcoef[1,1])
	bVec = c(GEcoef[2,1], Vcoef[2,1], Mcoef[2,1])
	
	#And now pull out the standard errors
	aseVec = c(GEcoef[1,2], Vcoef[1,2], Mcoef[1,2])
	bseVec = c(GEcoef[2,2], Vcoef[2,2], Mcoef[2,2])
	
	#In 2017, we imposed limits on the ceofficients so as not to produce impossible scores
	#In this version, we do that later but not here, so that corps with impossible coefficients can still be ranked
	
	#Put the coefficients and standard errors into a data frame
	OutFrame = data.frame(a=aVec, b=bVec, aSE=aseVec, bSE=bseVec, N=rep(N,3))
	row.names(OutFrame) = c('GE','Vis','Mus')
	#return the data frame
	return(OutFrame)
}

#Now make a function that predicts a day for all corps
Predictor <- function(CorpsList, PredictDay, Nmonte, RankDay, Damp=TRUE) {
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
			
			#Make a vector of random a's for each caption
			GEa = rnorm(Nmonte, mean=CorpsCoefs["GE","a"], sd=CorpsCoefs["GE","aSE"])
			Va = rnorm(Nmonte, mean=CorpsCoefs["Vis","a"], sd=CorpsCoefs["Vis","aSE"]) 
			Ma = rnorm(Nmonte, mean=CorpsCoefs["Mus","a"], sd=CorpsCoefs["Mus","aSE"]) 
			#Make a vector of b's for each caption
			GEb = rnorm(Nmonte, mean=CorpsCoefs["GE","b"], sd=CorpsCoefs["GE","bSE"]) 
			Vb = rnorm(Nmonte, mean=CorpsCoefs["Vis","b"], sd=CorpsCoefs["Vis","bSE"])
			Mb = rnorm(Nmonte, mean=CorpsCoefs["Mus","b"], sd=CorpsCoefs["Mus","bSE"])
			
			#Now create the score for each caption
			GEscore = GEa + PredictDay ^ GEb
			Vscore = Va + PredictDay ^ Vb
			Mscore = Ma + PredictDay ^ Mb
			
			#Now correct each score to make sure they're actually possible
			GEscore[GEscore > 40] = 40
			Vscore[Vscore > 30] = 30
			Mscore[Mscore > 30] = 30
			
			#Now get the total score list
			ScoreList[[C]] = GEscore + Vscore + Mscore
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
				} else if (IndexDiff > 8) { 
					#This is a background correlation
					CorrMat[Row,Col] = 0.1
				} else { 
					#This is an off-diag
					CorrMat[Row,Col] = 0.77 - IndexDiff*0.07
				}
			}
		}
		
		#Now multiply the correlation matrix by the standard deviations for each captions 
		#The sqrt(1.2) is the historical noise magnitude in caption scores, GE is scaled to 40 points instead of 30
		GEcov = CorrMat * sqrt(1.2*4/3)
		CapCov = CorrMat * sqrt(1.2)
		
		#Draw the random numbers using the covariance matrices, 
		GErand = mvrnorm(Nmonte, mu=rep(0,Ncorps), Sigma=GEcov, empirical=F)
		Mrand = mvrnorm(Nmonte, mu=rep(0,Ncorps), Sigma=CapCov, empirical=F)
		Vrand = mvrnorm(Nmonte, mu=rep(0,Ncorps), Sigma=CapCov, empirical=F)
		
		#We choose the column of random numbers for each corps based on their rank
		GEscores = vector(mode='double', length=Ncorps)
		Vscores = vector(mode='double', length=Ncorps)
		Mscores = vector(mode='double', length=Ncorps)
		
		#Fill in the vectors
		for (C in 1:Ncorps) {
			GEscores[C] = CorpsCoefList[[C]]["GE","a"] + RankDay ^ CorpsCoefList[[C]]["GE","b"]
			Vscores[C] = CorpsCoefList[[C]]["Vis","a"] + RankDay ^ CorpsCoefList[[C]]["Vis","b"]
			Mscores[C] = CorpsCoefList[[C]]["Mus","a"] + RankDay ^ CorpsCoefList[[C]]["Mus","b"]
		}
		#Convert these to gaps
		GEscores = GEscores - max(GEscores) 
		Vscores = Vscores - max(Vscores)
		Mscores = Mscores - max(Mscores)
		
		#Get vectors of rank for each caption
		GEranks = match(GEscores, sort(GEscores, decreasing=T)) #Ranks from highest to lowest
		Vranks = match(Vscores, sort(Vscores, decreasing=T)) #Ranks from highest to lowest
		Mranks = match(Mscores, sort(Mscores, decreasing=T)) #Ranks from highest to lowest
		
		#Now loop through each corps and fill in ScoreList
		for (C in 1:Ncorps) {
			#Damp the noise for top 15 if the PredictDay is >38 and Damp is true
			if (Damp & PredictDay > 38) {
				#Adjust the uncertainty for slotting in the top 12 later in the season by reducing the magnitude of the noise
				if (GEranks[C] < 15) {
					GErand[,GEranks[C]] = GErand[,GEranks[C]] * (1 - 0.03*(15-GEranks[C])) * (RankDay-38)/14
				}
				if (Vranks[C] < 15) {
					Vrand[,Vranks[C]] = Vrand[,Vranks[C]] * (1 - 0.03*(15-GEranks[C])) * (RankDay-38)/14
				}
				if (Mranks[C] < 15) {
					Mrand[,Mranks[C]] = Mrand[,Mranks[C]] * (1 - 0.03*(15-Mranks[C])) * (RankDay-38)/14
				}
			}
			
			#Get the scores for each caption
			GEvec = GEscores[C] + GErand[,GEranks[C]]
			Mvec = Mscores[C] + Mrand[,Mranks[C]]
			Vvec = Vscores[C] + Vrand[,Vranks[C]]
			
			#Put the summed scores into the list
			ScoreList[[C]] = GEvec + Mvec + Vvec
		}
		
		return(ScoreList)
	}
	
	#Get the number of corps
	Ncorps = length(CorpsList)
	
	#Run both score predictors
	ExpScoreList = ExpPredict(CorpsList, PredictDay, Nmonte)
	RandScoreList = RandPredict(CorpsList, PredictDay, Nmonte, RankDay, Damp)
	#print(RandScoreList)
	
	#Now create the overall score and rank lists to return
	ScoreList = vector(mode='list', length=Ncorps)
	RankList = vector(mode='list', length=Ncorps)
	
	#Fill in the final ScoreList
	for (C in 1:Ncorps) {
		ScoreList[[C]] = 0.275*ExpScoreList[[C]] + 0.725*RandScoreList[[C]]
		#ScoreList[[C]] = RandScoreList[[C]]
	}
	print(ScoreList)
	
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
	#mean score,percent odds of being in 1st, second, thrid, top12, and top25
PredictionReduce_WorldClass <- function(PredictionList) {
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
	PercSemis = vector(mode='double', length=Ncorps)
	
	#Loop through each corps and fill the vectors in
	for (C in 1:Ncorps) {
		MeanScores[C] = mean(ScoreList[[C]])
		PercGold[C] = sum(RankList[[C]] == 1) / Nmonte
		PercSilver[C] = sum(RankList[[C]] == 2) / Nmonte
		PercBronze[C] = sum(RankList[[C]] == 3) / Nmonte
		PercFinals[C] = sum(RankList[[C]] <= 12) / Nmonte
		PercSemis[C] = sum(RankList[[C]] <= 25) / Nmonte
	}
	
	#Adjust the gaps to make sure 1st place is a 0
	MeanScores = MeanScores - max(MeanScores)
	
	#Now format these into a data frame
	OutFrame = data.frame(Mean=MeanScores, Gold=PercGold, Silver=PercSilver, Bronze=PercBronze, Finals=PercFinals, Semis=PercSemis)
	row.names(OutFrame) = CorpsNames
	
	#Sort the data frame by percent chance of gold
	OutFrame = OutFrame[order(OutFrame$Mean, decreasing=T),]
	
	#Now return the data frame
	return(OutFrame)
}

#This function returns a vector of information for each corps, based on open class
	#that's mean score, percent odds of being in 1st, 2nd, 3rd, and making OC Finals
PredictionReduce_OpenClass <- function(PredictionList) {
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
	
	#Make sure 1st place's gap is 0
	MeanScores = MeanScores - max(MeanScores)
	
	#Now format these into a data frame
	OutFrame = data.frame(Mean=MeanScores, Gold=PercGold, Silver=PercSilver, Bronze=PercBronze, Finals=PercFinals)
	row.names(OutFrame) = CorpsNames
	
	#Sort the data frame by percent chance of gold
	OutFrame = OutFrame[order(OutFrame$Mean, decreasing=T),]
	
	#Now return the data frame
	return(OutFrame)
}

