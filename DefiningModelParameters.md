# Defining the Parameters of the DCI Model
DCI_DataExploration.r

The DCI model has the following parameters:
1. The distribution of judge-based error 
2. The correlation of judge-based error between corps
3. The weighting between the exponential prediction model and the random error model
4. The weighting of shows in the fitting algorithm as we progress through the season

## Data Selection
Only corps which were scored on World Class (or Division 1) equivalent scoring sheets can be included in the data. Using dcxmuseum, World-Class equivalent corps were assumed to have performed at quarterfinals (or prelims) during Finals Week. 
However, DCI has a historical precedent of granting performance slots at World Class Quarterfinals (or Division I Quaterfinals) to successful corps in the lower divisions. Those corps are typically scored on different sheets during the season and can't be included. To account for this, all corps with a maximum score that is more than 10 points greater than their final score were excluded from the sample. This resulted in a sample of about 20 corps for each season except 2014 and on, when all DCI corps started being scored on the same sheets. 

## Judge-based Error
The magnitude of judge-based error was estimated by fitting the a + x^b curve to their full sample of scores. For each year, the residual distribution was made by pooling the residuals of all corps together into one distribution. This resulted in having a distribution for each year that was very consistent - with a mean of 0 (good) and a standard deviation of 1.41305. Therefore, we will approximate the distribution of judge-based errors as normal with a mean of 0 and a variance of 2.

## Error Correlation
The show-specific residuals (aka judging error) is correlated from corps to corps, so the model needs to account for the historic correlation. However, doing this for each pair of corps within a year is impractical becuase the sample size for pairs of corps is too small. But because the residual distribution is the same for every year in the sample, we can pool all the years together. Then the pairs are defined by placement - for example, one pair is the correlation between first place and second place, regardless of the year. This results in a sample size of greater than 200 for many pairs, which allows us to resolve the correlation.

The correlation matrix is built by finding the mean correlation between adjacent pairs, then second-adjacent, third-adjacent, and so on until the background correlations is met. The background correlation is the average of all "greater than N" adjacent pairs, weighted by sample size. 

This results in a correlation dropoff from 0.513 at adjacency to 0.263 at 15 places away (from first to 16th, for example), which is the overall background correlation.

## Model Weighting
Model weighting will be found two ways. It'd be nice if they point to the same weight.

The first way is based on variance. With all of the model skill curves fit, we can find the variance of the residuals, which adds up to the total uncertainty in the scores. We can also find the variance that would result just from the uncertainty in the fit of the curves itself - this is the uncertainty in the ExpPredicit portion of the model. Using these two, we can solve for the uncertainty in RandPredict using the algebraic properties of error propaagation and a few simple assumptions.

The second way is to test the model at various weights from 0% random and 100% exponential to the exact opposite. Whichever way is the most accurate and calibrated is the best weighting to use. 

### Algebraic Error Propagation
The historical variance of the residuals is 2.09. This is straightforward to find. The historical variance of the exponential fits is slightly more forward because finding the error propagation of a + x^b is nontrivial. Instead, we use a Monte Carlo approach. 

By definition, the distribution of coefficients for the nonlinear fit is normal, and the standard deviation of the distribution is the standard error given by the fitting algorithm. However, each score, while an individual data point, technically provides information for the individual captions, whose errors are uncorrelated (at least, we assume. We don't have the data to test this claim directly). Therefore, we actually use each point three times in the fitting, which brings the standard error into a range more representative of the caption-specific use case for this model.

The Monte Carlo is done by calculating the score distribution on Finals day for each corps 1000 times. The variance of that, averaged across all corps, is the typical ExpPredict variance. That turns out to be 0.6637662, which is 31.7% of the total variance. 

Lastly, we need to scale the models so that the post-weight variance adds up to 2.1. This is done by only increasing the random variance, as the exponential fit variance is defined by the fit itself. This number turns out to be 5.

### Historic Prediction Fit

## Day Weighting
Days are weighted according to their distance from Prelims day, according to their literal correlation with prelims scores for all corps. Prelims day is assumed to be two days before Finals. 

As with earlier, all years are collapsed together to produce a sample of 494 corps. For each day, the correlation is calculated between the day and prelims day for all corps that performed that day, using both the spearman and pearson methods. They perform similarly. 

Given a list of days from 1 to 50 before prelims, a fitted linear model is calcuated, weighted by the sample size. Both correlation methods produce a correlation reduction (from 1 at 0 days away) of 0.00224 per day. This is used to get the weights of all the scores over the full season. 

However, the model should also discount scores that are recent as the season progresses, to limit the impact of a recent show on the fit curve. This is because the model should be somewhat skeptical about recent scores - if a corps get a score 4 points above the prior, we don't want it to overreact and assume that's the new normal. Therefore, recent shows should be weighted less than their normal weighting to start, but be weighted closer to normall as more data is accumulated.

Unfortunately, there's no clear methodological way to figure out the weighting reduction (or "skepticism factor") that's not convoluted, computationally difficuly, and hard to interpret. Cases like this are where the phrase "forecasting is both science and art" comes in, becuase I'm setting the skepticims factors more or less arbitratily. I am setting the most recent show to half it's normal weighting, and it increases linearly over 5 shows until it's up to the normal weight. However, this "discounting" will decrease through the season from 0.5 to 0.75. I think this is well within the range of defensibility. 