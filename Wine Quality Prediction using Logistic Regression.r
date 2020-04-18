## Assignment 3 R File By Dhaval Thakur, Tejas Pandit and Rushi Bhuva
### All the steps including EDA, graph plots are included in this R file

library(tidyverse)
library(ggplot2)
library(corrgram)
library(corrplot)
library(car)
library(lmtest)
library(gvlma)
library(dotwhisker)

df_redwine <- read.csv('https://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-red.csv',header = TRUE, sep = ";")
df_whitewine <- read.csv('https://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-white.csv',header = TRUE, sep = ";")
red <- df_redwine[!duplicated(df_redwine), ]
white <- df_whitewine[!duplicated(df_whitewine), ]
finaldataset<- rbind(red,white)
corrplot(cor(red))
#correlation matrix for white wine
corrplot(cor(white))
#Creating a new Factored Variable called 'Rating'
#Plotting of univariate graphs just to check 
ggplot(data=finaldataset, aes(x=quality)) +
  geom_bar() +
  geom_text(stat='count', aes(label=..count..), vjust=-1)
## found out that there are outliers of 5.

#plotting of univariate graph : rating
ggplot(data=finaldataset, aes(x=rating)) +
  geom_bar() +
  geom_text(stat='count', aes(label=..count..), vjust=-1)

#plotting histogram for pH (its normally distributed)
ggplot(finaldataset, aes(x=pH)) + geom_histogram()


# plotting historgram for density (its normally distributed)
ggplot(finaldataset, aes(x=density)) + geom_histogram()

finaldataset$rating <- ifelse(finaldataset$quality <= 5, 'below average', 'above average')

finaldataset$rating <- ordered(finaldataset$rating,levels = c('below average','above average'))
str(finaldataset)
summary(finaldataset)

ggplot(mapping = aes(x = finaldataset$rating)) + geom_bar() + theme_bw() + xlab("Quality")
wineModel.1 <- glm(rating ~ fixed.acidity+ volatile.acidity + citric.acid + residual.sugar+chlorides+free.sulfur.dioxide +total.sulfur.dioxide +density +pH+ sulphates+alcohol, data=finaldataset, family=binomial(link = 'logit'))
summary(wineModel.1)

wineModel.2 <- glm(rating ~ fixed.acidity+ volatile.acidity + residual.sugar+chlorides+free.sulfur.dioxide +total.sulfur.dioxide +density +pH+ sulphates+alcohol, data=finaldataset, family=binomial(link = 'logit'))
summary(wineModel.2)

wineModel.3 <- glm(rating ~ volatile.acidity + residual.sugar+chlorides+free.sulfur.dioxide +total.sulfur.dioxide +density +pH+ sulphates+alcohol, data=finaldataset, family=binomial(link = 'logit'))
summary(wineModel.3)

wineModel.4 <- glm(rating ~ volatile.acidity + residual.sugar+chlorides+free.sulfur.dioxide +total.sulfur.dioxide +pH+ sulphates+alcohol, data=finaldataset, family=binomial(link = 'logit'))
summary(wineModel.4)

wineModel.5 <- glm(rating ~ volatile.acidity + residual.sugar+free.sulfur.dioxide +total.sulfur.dioxide +pH+ sulphates+alcohol, data=finaldataset, family=binomial(link = 'logit'))
summary(wineModel.5)
BIC(wineModel.4)
BIC(wineModel.5)
var(finaldataset[names(finaldataset) != 'rating'])
sum(var(finaldataset[names(finaldataset) != 'rating']) == 0)
#VIF Calculation
vif(wineModel.4)

#tolerance
1/vif(wineModel.4)
plot(wineModel.4)

durbinWatsonTest(wineModel.4)

#test for the linearity of logit
#finaldataset$fixed.acidity.log <- log(finaldataset$fixed.acidity) * finaldataset$fixed.acidity
finaldataset$volatile.acidity.log <- log(finaldataset$volatile.acidity) * finaldataset$volatile.acidity
#finaldataset$citric.acid.log <- log(finaldataset$citric.acid) * finaldataset$citric.acid
finaldataset$residual.sugar.log <- log(finaldataset$residual.sugar) * finaldataset$residual.sugar
finaldataset$chlorides.log <- log(finaldataset$chlorides) * finaldataset$chlorides
finaldataset$free.sulfur.dioxide.log <- log(finaldataset$free.sulfur.dioxide) * finaldataset$free.sulfur.dioxide
finaldataset$total.sulfur.dioxide.log <- log(finaldataset$total.sulfur.dioxide) * finaldataset$total.sulfur.dioxide
#finaldataset$density.log <- log(finaldataset$density) * finaldataset$density
finaldataset$pH.log <- log(finaldataset$pH) * finaldataset$pH
finaldataset$sulphates.log <- log(finaldataset$sulphates) * finaldataset$sulphates
finaldataset$alcohol.log <- log(finaldataset$alcohol) * finaldataset$alcohol

wineModel.6 <- glm(rating ~ volatile.acidity + free.sulfur.dioxide + total.sulfur.dioxide + sulphates + alcohol + chlorides + pH  + residual.sugar+ volatile.acidity.log + volatile.acidity.log + free.sulfur.dioxide.log + total.sulfur.dioxide.log + sulphates.log + alcohol.log + chlorides.log + pH.log + residual.sugar.log, data=finaldataset, family=binomial(link = 'logit'))
summary(wineModel.6) #5364.6

wineModel.61 <- glm(rating ~ volatile.acidity + free.sulfur.dioxide + total.sulfur.dioxide + sulphates + alcohol + chlorides  + residual.sugar+ volatile.acidity.log + volatile.acidity.log + free.sulfur.dioxide.log + total.sulfur.dioxide.log + sulphates.log + alcohol.log + chlorides.log + residual.sugar.log, data=finaldataset, family=binomial(link = 'logit'))
summary(wineModel.61) #5364.6

wineModel.7 <- glm(rating ~ volatile.acidity + total.sulfur.dioxide + sulphates + alcohol + chlorides + pH  + residual.sugar+ volatile.acidity.log + volatile.acidity.log + total.sulfur.dioxide.log + sulphates.log + alcohol.log + chlorides.log + pH.log + residual.sugar.log, data=finaldataset, family=binomial(link = 'logit'))
summary(wineModel.7) #5490.9

wineModel.8 <- glm(rating ~ total.sulfur.dioxide + sulphates + alcohol + chlorides + pH  + residual.sugar+ volatile.acidity.log + total.sulfur.dioxide.log + sulphates.log + alcohol.log + chlorides.log + pH.log + residual.sugar.log, data=finaldataset, family=binomial(link = 'logit'))
summary(wineModel.8) #5837

wineModel.9 <- glm(rating ~ total.sulfur.dioxide + sulphates + alcohol + pH  + residual.sugar+ volatile.acidity.log + total.sulfur.dioxide.log + sulphates.log + alcohol.log + pH.log + residual.sugar.log, data=finaldataset, family=binomial(link = 'logit'))
summary(wineModel.9) #5870.3

wineModel.10 <- glm(rating ~ sulphates + alcohol + pH  + residual.sugar+ volatile.acidity.log + sulphates.log + alcohol.log + pH.log + residual.sugar.log, data=finaldataset, family=binomial(link = 'logit'))
summary(wineModel.10) #5870.3

wineModel.11 <- glm(rating ~ sulphates + alcohol + pH  + residual.sugar + sulphates.log + alcohol.log + pH.log + residual.sugar.log, data=finaldataset, family=binomial(link = 'logit'))
summary(wineModel.11) #5870.3

wineModel.11 <- glm(rating ~ sulphates + alcohol + pH  + residual.sugar + sulphates.log + alcohol.log + pH.log + residual.sugar.log, data=finaldataset, family=binomial(link = 'logit'))
summary(wineModel.11) #5870.3
finaldataset$fitted <- wineModel.4$fitted
finaldataset$residuals <- wineModel.4$residuals
finaldataset$standardized.residuals <- rstandard(wineModel.4)
possible.outliers <- subset(finaldataset, standardized.residuals < -1.96 | standardized.residuals > 1.96)
#possible.outliers
percent_outlier <-nrow(possible.outliers)/nrow(finaldataset)*100
#percent_outlier
wineModel.4$cooks <- cooks.distance(wineModel.4)
plot(sort(wineModel.4$cooks, decreasing=TRUE))

max(wineModel.4$cooks) #Cook's Distance
exp(wineModel.4$coefficients)
exp(confint(wineModel.4)) #Confidence Interval
dwplot(list(wineModel.4)) + theme_bw() + xlab("Coeffcient") + theme(legend.position = c(0.85,0.8))