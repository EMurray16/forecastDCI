#This reads the data and runs the 2018 DCI Model

#Load the libraries
Libraries = c("tictoc","data.table")
lapply(Libraries, library, character.only=T)
source("DCI_Library_CaptionScore.r")

# Retrieve the 2019 scores
scores2019 = ScoreParse("DCI_Scores_2019.csv")
# We will exlclude corps that didn't attend DCI Championship Week in 2019 due to data quality concerns
scores2019 = scores2019[!(names(scores2019) %in% OC_NotAttending)]

# Get the 2020 schedule
schedule2020 = data.table(read.csv("schedule2020_byCorps.csv", stringsAsFactors=F))
# Once again, we eliminate corps with small sample size
schedule2020 = schedule2020[!(Corps %in% OC_NotAttending),]

# Create an editable copy of the original data
simData = copy(scores2019)

# Now we loop through each day of the season
# This is slow, but I'm not concerned about speed in this example
for (d in 1:max(schedule2020$CompetitionDay)) {
	# Drop the scores up to this day from all the corps' tables
	for (C in names(simData)) {
		oldFrame = simData[[C]]
		# Note that this drops all current-day scores
		newFrame = oldFrame[(Day > d & Source == "real") | (Day < d & Source == "sim") ,]
		simData[[C]] = newFrame
	}
	
	# Fit the skill curves for this day
	skillCurves = lapply(simData, ExpFitterMod, BaseDay=d)
	# Clean out the nonconvergent curves
	goodCurves = !as.vector(sapply(skillCurves, is.null))
	skillCurves = skillCurves[goodCurves]
	
	# Get the list of shows performing this day
	daySchedule = schedule2020[CompetitionDay == d, c("CompetitionDay","showID","Corps")]
	showIDs = unique(daySchedule$showID)
	#print(showIDs)
	
	# Run 100 simulations
	results = Predictor(skillCurves, PredictDay=d, Nmonte=100, RankDay=d)
	
	# For each show, grab the simulation for the corps and add it
	for (sID in showIDs) {
		showResults = lapply(results, "[", sID) # extracts the results matching the showID
		
		corpsPerforming = daySchedule$Corps[daySchedule$showID == sID]
		for (corps in corpsPerforming) {
			corpsTable = simData[[corps]]
			
			# Create a 1-row data table with the result
			corpsRes = showResults[[corps]]
			corpsRes$Source = "sim"
			corpsRes$Day = d
			setcolorder(corpsRes, neworder=c("Day","GE","Vis","Mus","Source"))
			
			simData[[corps]] = rbind(corpsRes, corpsTable)
			print(paste(d, sID, corps))
			#print(simData[[corps]])
		}
	}
}