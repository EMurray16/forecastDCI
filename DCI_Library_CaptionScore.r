#This contains all of the functions used in the model
library(data.table)
#Hardcode the corps names by class as generally accessible variables
WorldClass = c("The Academy","Blue Devils","Blue Knights","Blue Stars","Bluecoats","Boston Crusaders","The Cadets","Carolina Crown",
	"The Cavaliers","Colts","Crossmen","Genesis","Jersey Surf","Madison Scouts","Mandarins","Music City",
	"Pacific Crest","Phantom Regiment","Santa Clara Vanguard","Seattle Cascades","Spirit of Atlanta","Troopers"
)
OpenClass = c("7th Regiment","The Battalion","Blue Devils B","Blue Devils C","Colt Cadets","Columbians","Encorps","Gold","Golden Empire",
	"Guardians","Heat Wave","Impulse","Incognito","Legends","Les Stentors","Louisiana Stars","Raiders","River City Rhythm","Shadow",
	"Southwind","Spartans","Vanguard Cadets","Vessel","Watchmen"
)
OC_NotAttending = c("The Battalion","Blue Devils B","Blue Devils C","Columbians","Encorps","Impulse","Incognito",
	"Vanguard Cadets","Vessel","Watchmen"
)


ScoreParse <- function(filename, WorldNames=WorldClass, OpenNames=OpenClass) {
	#Start by loading the file
	BigTable = data.table(read.csv(filename, stringsAsFactors=F))
	
	# Eliminate any rows with null scores
	BigTable = BigTable[!is.na(BigTable$GE) & !is.na(BigTable$Visual) & !is.na(BigTable$Music) ,]
	
	#This function creates a data frame for a given corps
	FrameCreate <- function(CorpsName, Table) {
		#Start by finding all the rows which have data
		CorpsRows = which(Table$Corps == CorpsName)
		if (length(CorpsRows) == 0) {
			return(NULL)
		}
		
		#Get vectors for the scores and Day
		GEvec = Table$GE[CorpsRows]
		VisVec = Table$Visual[CorpsRows]
		MusVec = Table$Music[CorpsRows]
		DayVec = Table$Day[CorpsRows]
		SourceVec = Table$Source[CorpsRows]
		
		#Now make the data frame
		OutFrame = data.table(Day=DayVec, GE=GEvec, Vis=VisVec, Mus=MusVec, Source=SourceVec)
		# Before returning the frame, we want to sort by day
		OutFrame = OutFrame[order(OutFrame$Day),]
		return(OutFrame)
		#print(OutFrame)
	}
	
	#Now run the function on all the world class and open class corps
	WorldFrames = lapply(WorldNames, FrameCreate, Table=BigTable)
	OpenFrames = lapply(OpenNames, FrameCreate, Table=BigTable)
	#Combine them into a single frame
	AllFrames = c(WorldFrames, OpenFrames)
	# AllFrames = c(WorldFrames)
	
	#Add the corps names to the frames
	names(AllFrames) = c(WorldNames, OpenNames)
	# names(AllFrames) = c(WorldNames)
	return(AllFrames)
}

#Create a function that fits the exponential curve to a data frame
ExpFitter <- function(CorpsFrame, BaseDay, PrelimsDay=50) {
	#50 is the prelims day for 2019, which is why it's defaulted here
	
	#Start by checking the number of shows
	if (is.null(CorpsFrame)) { return(NULL) }
	if (nrow(CorpsFrame) < 6) { return(NULL) }
	
	#Order the frame by day
	CorpsFrame = CorpsFrame[order(CorpsFrame$Day),]
	
	#The weight vector reduces weights of recent scores, based on their correlation with finals week scores
	DayVec = seq(from=0, to=PrelimsDay+2, by=1)
	WeightVec = 1 - (PrelimsDay-DayVec)*0.00224
	DiscountOrig = 0.25 + (0.75-0.25)/PrelimsDay * BaseDay
	DiscountVec = seq(from=1, to=DiscountOrig, length.out=6)
	
	#Create a vector for the corps-specific day weights
	ShowWeights = WeightVec[CorpsFrame$Day]
	#Now discount the most recent 5
	Nshow = length(ShowWeights)
	ShowWeights[(Nshow-5):Nshow] = ShowWeights[(Nshow-5):Nshow] * DiscountVec
	
	# print("ExpFitter:")
	# print(data.table(Day=CorpsFrame$Day, WeightsOld=ShowWeights))
	
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
			control=nls.control(warnOnly=TRUE), weights=ShowWeights)
		VisModel = nls(Vis ~ a + Day^b, data=data.frame(Vis, Day), start=list(a=Vis[1], b=0.5), 
			control=nls.control(warnOnly=TRUE), weights=ShowWeights)
		MusModel = nls(Mus ~ a + Day^b, data=data.frame(Mus, Day), start=list(a=Mus[1], b=0.5), 
			control=nls.control(warnOnly=TRUE), weights=ShowWeights)
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
	
	if (sum(ExistVec) < 3) { #This means we didn't get a good fit in at least 1 caption
		#print("Nonconvergence!")
		#Try removing the most recent scores until we get convergence or too small a sample
		CorpsFrame2 = CorpsFrame2[1:(nrow(CorpsFrame2)-1) ,]
		
		if (nrow(CorpsFrame) < 6) { #Check for sample size
			print("NONCONVERGENCE")
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
	
	#Put the coefficients and standard errors into a data frame
	OutFrame = data.frame(a=aVec, b=bVec, aSE=aseVec, bSE=bseVec, N=rep(N,3))
	row.names(OutFrame) = c('GE','Vis','Mus')
	#return the data frame
	return(OutFrame)
}

#Create a function that fits the exponential curve to a data frame
# This one always includes all scores, but doubles the weight for those before the base day
ExpFitterMod <- function(CorpsFrame, BaseDay, PrelimsDay=50) {
	#50 is the prelims day for 2019, which is why it's defaulted here
	
	#Start by checking the number of shows
	if (is.null(CorpsFrame)) { return(NULL) }
	if (nrow(CorpsFrame) < 6) { return(NULL) }
	
	#Order the frame by day
	CorpsFrame = CorpsFrame[order(CorpsFrame$Day),]
	
	#The weight vector reduces weights of recent scores, based on their correlation with finals week scores
	DayVec = seq(from=0, to=PrelimsDay+2, by=1)
	WeightVec = 1 - (PrelimsDay-DayVec)*0.00224
	DiscountOrig = 0.25 + (0.75-0.25)/PrelimsDay * BaseDay
	DiscountVec = seq(from=1, to=DiscountOrig, length.out=6)
	
	#Create a vector for the corps-specific day weights
	ShowWeights = WeightVec[CorpsFrame$Day]
	#Now discount the most recent 5
	Nshow = sum(CorpsFrame$Day <= BaseDay)
	# We can't assume there are 5 shows to weight, so we have to loop backwards from the show
	if (Nshow > 0) {
		discountInd = 7
		for (ind in Nshow:(max(1,Nshow-5))) {
			discountInd = discountInd - 1
			#print(c(Nshow, ind, discountInd))
			ShowWeights[ind] = ShowWeights[ind] * DiscountVec[discountInd]
		}
	}
	
	# Double the weights for simulated days
	ShowWeights[CorpsFrame$Source == "sim"] = ShowWeights[CorpsFrame$Source == "sim"]*5
	
	# print("ExpFitterMod:")
	# print(data.table(Day=CorpsFrame$Day, WeightsOld=ShowWeights, WeightsNew = ShowWeights2))
	
	#Pull out the individual vectors to match nls input better
	N = sum(CorpsFrame$Day < PrelimsDay)
	GE = CorpsFrame$GE#[CorpsFrame$Day < PrelimsDay]
	Vis = CorpsFrame$Vis#[CorpsFrame$Day < PrelimsDay]
	Mus = CorpsFrame$Mus#[CorpsFrame$Day < PrelimsDay]
	Day = CorpsFrame$Day#[CorpsFrame$Day < PrelimsDay]
	
	#Now fit the curve for each caption, wrapped in a try to avoid catastrophic errors
	#This tryCatch won't make the objects if we're not successful
	tryCatch({ 
		GEModel = nls(GE ~ a + Day^b, data=data.frame(GE, Day), start=list(a=GE[1], b=0.5), 
			control=nls.control(warnOnly=TRUE), weights=ShowWeights)
		VisModel = nls(Vis ~ a + Day^b, data=data.frame(Vis, Day), start=list(a=Vis[1], b=0.5), 
			control=nls.control(warnOnly=TRUE), weights=ShowWeights)
		MusModel = nls(Mus ~ a + Day^b, data=data.frame(Mus, Day), start=list(a=Mus[1], b=0.5), 
			control=nls.control(warnOnly=TRUE), weights=ShowWeights)
	}, warning = function(w) { #This makes it so scenarios of non-convergence don't return bad coefficients
		print("Can't run corps due to non-convergence")
	}, error = function(e) {
		print("Can't run corps due to an error in curve fitting")
		print(e)
		print(c(length(ShowWeights), length(Day), length(GE), ))
	}) 
	
	#Check to see how many of the 3 models exist
	ExistVec = c(exists('GEModel'), exists('VisModel'), exists('MusModel'))
	
	#Create a copy of CorpsFrame we can modify
	CorpsFrame2 = CorpsFrame
	TrimmedResult = NA #This tracks if we're successful in the if loop
	
	if (sum(ExistVec) < 3) { #This means we didn't get a good fit in at least 1 caption
		#print("Nonconvergence!")
		#Try removing the most recent scores until we get convergence or too small a sample
		CorpsFrame2 = CorpsFrame2[1:(nrow(CorpsFrame2)-1) ,]
		
		if (nrow(CorpsFrame) < 6) { #Check for sample size
			print("NONCONVERGENCE")
			break
		} else {
			#Recursion!
			TrimmedResult = ExpFitterMod(CorpsFrame2, BaseDay)
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
	
	#Put the coefficients and standard errors into a data frame
	OutFrame = data.frame(a=aVec, b=bVec, aSE=aseVec, bSE=bseVec, N=rep(N,3))
	row.names(OutFrame) = c('GE','Vis','Mus')
	#return the data frame
	return(OutFrame)
}

#Now make a function that predicts a day for all corps
Predictor <- function(CorpsList, PredictDay, Nmonte, RankDay) {
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
			ScoreList[[C]] = data.table(GE=GEscore, Vis=Vscore, Mus=Mscore)
		}
		
		return(ScoreList)
	}
	
	#Now create a function that predicts N scores based on the random error
	RandPredict <- function(CorpsCoefList, PredictDay, Nmonte, RankDay) {
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
					#This is an off-diag
					CorrMat[Row,Col] = 0.513 - (IndexDiff-1)*(0.25/14)
				}
			}
		}
		#To account for the effect of doing caption specific correlations, we increase the correlation
		CorrMat = CorrMat * 1.25
		diag(CorrMat) = 1
		
		#Now multiply the correlation matrix by the variances for each captions 
		#the historical noise magnitude is 4, so the captions are adjusted to add to that:
			# 40^2 * GEvar + 30^2 * Mvar + 30^2 Vvar = 4
			# 1600GEvar + 900Mvar + 900Vvar = 4
			# Mvar = Vvar = Cvar - assuming the music and visual errors are the same
			# 1600GEvar + 1800Cvar = 4
			# GEvar = 4/3 * Cvar - assuming error is scaled by total caption score
			# (4/3)1600Cvar + 1800Cvar = 4
			# with some rounding...
			# 3900Cvar = 4
			# Cvar = 4 / 3900
		# We now need to apply the variance to each individual caption 
		# At some point the error needs to be scaled by total points, so we'll do it  here
		GEcov = CorrMat * 40 * sqrt(4/3900)
		CapCov = CorrMat * 30 * sqrt(4/3900)
		
		#Draw the random numbers using the covariance matrices, 
		GErand = mvrnorm(Nmonte, mu=rep(0,Ncorps), Sigma=GEcov, empirical=F)
		Mrand = mvrnorm(Nmonte, mu=rep(0,Ncorps), Sigma=CapCov, empirical=F)
		Vrand = mvrnorm(Nmonte, mu=rep(0,Ncorps), Sigma=CapCov, empirical=F)
		
		#We choose the column of random numbers for each corps based on their rank
		# In 2018, we ranked by caption but now we use total score as it better indicates performance order
		RankScores = vector(mode='double', length=Ncorps)
		
		#We still need the caption scores to do the predictions
		GEscores = vector(mode='double', length=Ncorps)
		Vscores = vector(mode='double', length=Ncorps)
		Mscores = vector(mode='double', length=Ncorps)
		
		#Fill in the vectors
		for (C in 1:Ncorps) {
			GEscores[C] = CorpsCoefList[[C]]["GE","a"] + RankDay ^ CorpsCoefList[[C]]["GE","b"]
			Vscores[C] = CorpsCoefList[[C]]["Vis","a"] + RankDay ^ CorpsCoefList[[C]]["Vis","b"]
			Mscores[C] = CorpsCoefList[[C]]["Mus","a"] + RankDay ^ CorpsCoefList[[C]]["Mus","b"]
			RankScores = GEscores + Vscores + Mscores
		}
		
		#Get the rank vector
		CorpsRanks = match(RankScores, sort(RankScores, decreasing=T)) #Ranks from highest to lowest
		
		#Now loop through each corps and fill in ScoreList
		for (C in 1:Ncorps) {
			# In 2018 we had a damping effect to account for slotting, but that effect is now captured
			# 	by other parts in the model.
			GEvec = GEscores[C] + GErand[,CorpsRanks[C]]
			Mvec = Mscores[C] + Mrand[,CorpsRanks[C]]
			Vvec = Vscores[C] + Vrand[,CorpsRanks[C]]
			
			#Put the summed scores into the list
			ScoreList[[C]] = data.table(GE=GEvec, Vis=Vvec, Mus=Mvec)
		}
		return(ScoreList)
	}
	
	#Get the number of corps
	Ncorps = length(CorpsList)
	
	#Run both score predictors
	ExpScoreList = ExpPredict(CorpsList, PredictDay, Nmonte)
	RandScoreList = RandPredict(CorpsList, PredictDay, Nmonte, RankDay)
	#print(RandScoreList)
	
	#Now create the overall score and rank lists to return
	ScoreList = vector(mode='list', length=Ncorps)
	RankList = vector(mode='list', length=Ncorps)
	
	#Fill in the final ScoreList
	for (C in 1:Ncorps) {
		ScoreList[[C]] = 0.317*ExpScoreList[[C]] + 0.683*RandScoreList[[C]]
	}
	
	# Round everything to the 3rd digit
	
	
	#Retain the names in the lists
	names(ScoreList) = names(CorpsList)
	
	#return the lists
	return(ScoreList)
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

