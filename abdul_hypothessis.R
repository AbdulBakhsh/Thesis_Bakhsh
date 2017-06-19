

# Hypothesis Testing -----------------------------

#------------------------------------------------------------------------------- 
# Load the relevant model into R's working memory:
source("preliminary.R")

###################### Hypothesis 1 ########################

## H1: it is expected that a lack of emotional clarity and absence of strategies for managing emotions will be significant predictor of binge eating behavior.

# 1. Need to average 7 and 8 
abdul.df$edds78 <- (abdul.df$edds7 + abdul.df$edds8)/2

# 2. Need to select only people with EDDS 5 and 6
abdul.df$edds5f <- as.factor(abdul.df$edds5) # make it a factor
abdul.df$edds6f <- as.factor(abdul.df$edds6) # make it a factor

# make a new variable combining the results from the two variables
abdul.df$edds56 <- ifelse((abdul.df$edds5 == 2) & (abdul.df$edds6 == 2), 0, 
                           +    ifelse((abdul.df$edds5 ==1) & (abdul.df$edds6 == 1), 1, 99))

abdul.df$edds56f <- factor(abdul.df$edds56, levels = c(0:1), labels = c("No-BED", "BED"))

bed.df <- subset(abdul.df, edds56f == "BED")
nobed.df <- subset(abdul.df, edds56f == "No-BED")

# regression ------------------------------
library(arm)

hyp1.lm <- lm(edds78 ~ clarity + strategy, data= bed.df)
display(hyp1.lm) # strategy seems to have an effect

hyp1.lm1 <- lm(edds78 ~ clarity*edds56f + strategy*edds56f, data= na.df)
display(hyp1.lm1)

View(na.df)
na.df <- na.omit(abdul.df)
names(na.df)

graph <- ggplot(data = na.df, aes(y=edds78, x=strategy, colour=edds56f)) +
  geom_point() + geom_smooth(method="lm", se=TRUE)
graph
## Question: Since there are missing answers for some measures, what should i do with it?
## question: how do i include the EEDS if they only said yes? I want EDDS5 and EDDS6 to be only yes




######################################## Hypothesis 2 ######################################## 
##Hypothesis 2: gender, food restriction, weight-shape over-evaluation, and overall difficulties with emotion regulation are expected to be significant predictors of binge-eating behavior.

## To test for hypothesis 2, a hierarchical linear regression will be used with variables entered consistent with Whiteside’s model including: 1) gender 2) gender + food restriction 3) gender + food restriction + weight-shape over-evaluation 4) gender + food restriction + weight-shape over-evaluation + DERS total. Consistent with (Whiteside et al., 2007), no interaction terms will be included, no variables centered, and gender coded 0 for female. Also, as an extension of (Whiteside et al., 2007), analyses will be re-run with all variables centered to improve interpretability of the coefficients.

## Questions: I dont know which item from the EEDS is food restriction, weight-shape

## General regression analysis using the lm() function, Linear model
newModel<-lm(outcome ~ predictor(s), data= dataframe, na.function= anaction)

## general format for hypothesis 2 using lm 

## 1. BED and (gender)
Hypo2A.df<-lm(edds ~ gender, data= ThesisData.df, na.function= na.fail)

## 2. BED and (gender + food restriction)

newmodel2<-lm(edds ~ gender, foodrestriction, data= ThesisData.df, na.function= na.fail)

## 3. BED and (gender + food restriction+ weight-shape over-evaluation )

newmodel3<-lm(edds ~ gender, foodrestriction, weight-shape, data= ThesisData.df, na.function= na.fail)

## 4. BED and (gender + food restriction+ weight-shape over-evaluation + DERS total)

newmodel4<-lm(edds ~ gender, foodrestriction, weight-shape, ThesisData$derstot, data= ThesisData.df, na.function= na.fail)

###### OR Run the multiple regression model

## 1. BED and (gender)
as.1 <- lm(edds ~ gender, data = ThesisData.df, na.function= na.fail)
summary(as.1) ##shows us Min, 1Q, Median, 3Q, Max
lm.beta(as.1) ## shows us the number of standard deviation by which the outcome will change as a result of one standard deviation in the predictor

## 2. BED and (gender + food restriction)
as.2 <- lm(edds ~ gender,foodrestriction, data = ThesisData.df, na.function= na.fail)
summary(as.2)
lm.beta(as.2)

## 3. BED and (gender + food restriction+ weight-shape over-evaluation )
as.3 <- lm(edds ~ gender,foodrestriction, weight-shape, data = ThesisData.df, na.function= na.fail)
summary(as.3)
lm.beta(as.3)


## 4. BED and (gender + food restriction+ weight-shape over-evaluation + DERS total)
as.4 <- lm(edds ~ gender,foodrestriction, weight-shape, ThesisData$derstot, data = ThesisData.df, na.function= na.fail)
summary(as.4)
lm.beta(as.4)

anova(as.1, as.2, as.3, as.4)

lm.beta(ThesisData.df)


######################################## Hypothesis 3 ######################################## 
##Hypothesis 3: Lower levels of self-concept affiliation and lower levels of affiliation and higher autonomy in a close relationship will be associated with increased binge-eating behavior regardless of overall positive or negative coping and overall emotion regulation.

##To test for hypothesis 3, a linear regression will be conducted Intrex SASB variables to examine their association with binge-eating behavior. 

## regression analysis using the lm() function, Linear model
newModel<-lm(outcome ~ predictor(s), data= dataframe, na.function= anaction)

## general format for hypothesis 3 using lm 

## Step 1: BED and Intrex SASB variables

newmodel1<-lm(edds ~ SASB, data= ThesisData.df, na.function= na.fail)

## Step 2: Intrex SASB variables, overall DERS, positive coping (ThesisData$DSS), and negative coping (ThesisData$DCS3) ?
newmodel1<-lm(edds ~ SASB, DERSTotal, PositiveCoping(WCLL), NegativeCoping(DSS),  data= ThesisData.df, na.function= na.fail)

ThesisData$DCS3
##Step 3: Intrex SASB variables, overall DERS, positive coping, and negative coping, gender, food restriction, and weight-shape over evaluation.

newmodel1<-lm(edds ~ SASB, DERSTotal, PositiveCoping(WCLL), NegativeCoping(DSS), gender, foodrestriction,weight-shape,DERSTotal,  data= ThesisData.df, na.function= na.fail)






#################### Regression general codes ####################:
#R Code for Chapter 7 of:

## regression analysis using the lm() function, Linear model
## general
newModel<-lm(outcome ~ predictor(s), data= dataframe, na.function= anaction)


########### histoframs
##general, number of X? - Histogram with Density Line
histoX <- ggplot (dataframe) , aex(predictor) + geom_histogram(aes(y=..density..), binwidth = 0.4, colour="black", fill="white") + labs(x="number of advertisments", y = "frequency", title="Histogram")
histox

# number of X? - Histogram with Density Line
histoAdver <- ggplot(ThesisData, aes(X?) ) + geom_histogram(aes(y=..density..), binwidth = 0.4, colour="black", fill="white") + labs(x="number of advertisments", y = "frequency", title="Histogram")
histoAdver

########### Scatterplots
#general simple scatter for factor1 and factor2
scatter <- ggplot(dataframe, aes (factor1, factor2))
scatter1 <-scatter + geom_point() + labs(x = "factor1", y = "factor2") # add points and labels
scatter1 # interesting effect for those with low anxiety. 

#Simple scatter for X1 and X2
scatter <- ggplot(ThesisData, aes(X1, X2))
scatter1 <-scatter + geom_point() + labs(x = "X1", y = "X2") # add points and labels
scatter1 # interesting effect for those with low anxiety.  

########### Regression
as.1 <- lm(predictor1 ~ predictor2, data = ThesisData) # what's the DV and IV here? 'as' refers to 'album sales'
summary(as.1)

as.1 <- lm(predictor1 ~ predictor2, data = ThesisData) # what's the DV and IV here? 'as' refers to 'album sales'
summary(as.1)

#### ---- We can obtain standardized parameter estimates with the lm.beta() function---#need library(QuantPsyc)
lm.beta(as.1) 

#### ---Confidence intervals are obtained with the confint() function----
confint(as.1)


#########################---Run the multiple regression model----############################ 
## general 

as.2 <- lm(predictor1 ~ predictor2 + predictor3 + predictor4, data = dataframe)
summary(as.3)
lm.beta(as.3)

as.3 <- lm(predictor1 ~ predictor2 + predictor3 , data = dataframe)

anova(as.2, as.3)