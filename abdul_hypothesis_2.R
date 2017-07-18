
## July, 8, 2017

#-----------------#Hypothesis Testing -----------------------------


# Load the relevant model into R's working memory:
source("preliminary.R")


sd(abdul.df$age)
#----------------- Hypothesis 1 ---------------------------------------
# it is expected that a lack of emotional clarity and absence of strategies for managing emotions will be significant predictor of binge eating behavior.
## A linear regression will be conducted with DERS subscales as predictors of binge-eating behavior. 

############### fix the data up
# 1. Need to average 7 and 8 (number of binge)
abdul.df$edds78 <- (abdul.df$edds7 + abdul.df$edds8)/2

# 2. Need to select only people with EDDS 5 and 6
abdul.df$edds5f <- as.factor(abdul.df$edds5) # make it a factor
abdul.df$edds6f <- as.factor(abdul.df$edds6) # make it a factor

# make a new variable combining the results from the two variables
abdul.df$edds56 <- ifelse((abdul.df$edds5 == 2) & (abdul.df$edds6 == 2), 0, +ifelse((abdul.df$edds5 ==1) & (abdul.df$edds6 == 1), 1, 99))

# label the new factor
abdul.df$edds56f <- factor(abdul.df$edds56, levels = c(0:1), labels = c("No-BED", "BED"))

bed.df <- subset(abdul.df, edds56f == "BED")
nobed.df <- subset(abdul.df, edds56f == "No-BED")

############### test the hypotheses (check chapter 7, p. 258 and chapter 10, p. 424)

# regression for everyone regardless of with both 5 and 6 only
hyp1.lm <- lm(edds78 ~ clarity + strategy, data= bed.df)
summary(hyp1.lm) # not significant
confint(hyp1.lm)
lm.beta(hyp1.lm)

## Multiple R-squared:  0.05501,	Adjusted R-squared:  0.01866 
## F-statistic: 1.513 on 2 and 52 DF,  p-value: 0.2297
    ## Interpretation: Model accounts for 0.1178 of the model.   
    ## not significant because F is 0.2297, which is not significant at p < 0.2147.
    ## for it to be significant F has to be less than p 0.05 and our P is p < 0.2147, thus not significant

## confint(hyp1.lm), 
    ## Emotional clarity (B = .09, CI = -.14, .33) and strategy (B = 0.5, CI = -.05, .15)

## Emotional clarity and absence of strategies did not significantly predict binge-eating, R2 = 0.06, F = (2, 52) = 1.51, p = 0.23.  Emotional clarity (B = .09, CI = -.14, .33) and strategy (B = 0.5, CI = -.05, .15) were both positively, but not significantly, associated with binge-eating.
  
  ## R2 is "Multiple R-squared", F is "F-statistic: 1.513 on 2 and 52 DF", P is "p-value: 0.2297", B is "estimate", CI is "confint"


## The results tells us that there is less than 11.78%  chance that an F-ratio this large would happen if the null hupothesis were true. Therefore, we CANNOT conclude that our regression model results in significiantly better prediction of binge eating than if we used the mean valuse of binge eating. In short, the regression model (emotional clarity and absence of strategies) overal does NOT predicts binge eating and is not significant. 


#test.lm <- glm(edds78 ~ clarity + strategy, data = bed.df, family = poisson(link = "log"))
# summary(test.lm)

# here, use all data and include whether or not meeting criteria interacts with predicted DERS Scales  
hyp1.lm1 <- lm(edds78 ~ clarity*edds56f + strategy*edds56f, data= abdul.df)
summary(hyp1.lm1) # not significant



# how about other DERS total - not hypothesized
hyp1.lm2 <- lm(edds78 ~ derstot, data= abdul.df)
summary(hyp1.lm2) # significant but 56 don't matter

## F-statistic: 7.434 on 1 and 80 DF,  p-value: 0.00786
    ## If P-value is less than 0.5, then it would be significant.
    ## Our results show that P= 0.00786 which is less than 0.05 thus, it is significant
    ## the total ders seems to be significant, thus ders total could be significant predictor of binge eating

# Graphs for hypothesis 1 --------------------------------------
abdulna.df <- na.omit(abdul.df) # get rid of NA for graphing

# graph strategy
graph <- ggplot(data = abdulna.df, aes(y=edds78, x=strategy, colour=edds56f)) + geom_point() + geom_smooth(method="lm", se=TRUE)
graph # no association between edds78 for strategy

#graph clarity
graph1 <- ggplot(data = abdulna.df, aes(y=edds78, x=clarity, colour=edds56f)) + geom_point() + geom_smooth(method="lm", se=TRUE)
graph1 # no association between edds78 for strategy

#total ders 
graph3 <- ggplot(data = abdulna.df, aes(y=edds78, x=derstot, colour=edds56f)) + geom_point() + geom_smooth(method="lm", se=TRUE)
graph3 # an association in right direction but edds56 is not inlcuded





#----------------- Hypothesis 2 ---------------------------------------

#hypothesis 2: Gender, food restriction, weight-shape over-evaluation, and overall difficulties with emotion regulation are expected to be significant predictors of binge-eating behavior.

## edds17 is food restriction
## (edds234)= edds2, edds3, edds4 is weight and shape

## 1. Make Fear varaiable - 2,3,4 (sum up) (wieght-shape)
abdulna.df$edds234 <- abdulna.df$edds2 + abdulna.df$edds3 + abdulna.df$edds4
bed.df$edds234 <- bed.df$edds2 + bed.df$edds3 + bed.df$edds4

hyp2.lm <- lm(edds78 ~ genderfactor + edds17 + edds234 + derstot, data= bed.df)
summary(hyp2.lm) #edds17 (restricting) significant
confint(hyp2.lm)
lm.beta(hyp2.lm)


## Multiple R-squared:  0.1876,	Adjusted R-squared:  0.1226 
## F-statistic: 2.887 on 4 and 50 DF,  p-value: 0.03158
    ##Multiple R-squared has a valuse of 0.03158, Interpretation: Model accounts for 0.03158 of the model.
    ## P value is is less than 0.05, thus it is significant, hypothesis 2 not rejected ?
    ## for Gender, food ristriction, weight-shape over-evaluation, and overall difficulties with emotion regulation are significant predictors of bed-behaviors
    
## Results show that gender, food restriction, weight-shape over-evaluation, and overall difficulties with emotion regulation significantly predicted binge-eating, R2= 0.19, F (4, 50) = 2.89, p = 0.03. 
# With gender (B= -0.25, CI= -1.49,0.99), food restriction "edds17" (B= 0.9, CI=0.004, 0.19), weight-shape over-evaluation "edds234" (B= 0.11, CI=-0.02, 0.22), overall difficulties with emotion regulation "derstot" (B= 0.010, CI= -0.02, 0.04)

    ## for gender, Pr(>|t|) = 0.6927 which is greater than P=0.05, thus NOT significant
    ## for edds17 (food restriction) Pr(>|t|) =  0.0400 which is less than P=0.05, thus it is significant
    ## for edds234 (fear) Pr(>|t|) =  0.0941 which is greater than P=0.05, thus it is NOT significant
    ## for derstot (overall difficulties with emotion regulation), Pr(>|t|) =  0.4863 which is greater than P=0.05, thus it is NOT significant

# graphs for hypothesis 2
# 1. fear (edds234)
graph4 <- ggplot(data = bed.df, aes(y=edds78, x=edds234)) + geom_point() + geom_smooth(method="lm", se=TRUE)
graph4 

# 2. restrict
graph5 <- ggplot(data = bed.df, aes(y=edds78, x=edds17)) + geom_point() + geom_smooth(method="lm", se=TRUE)
graph5 

## are we just making graphs only for edds234 and edds17 because there were significant?



#----------------- Hypothesis 3 ---------------------------------------

#Lower levels of self-concept affiliation and lower levels of affiliation and higher autonomy in a close relationship will be associated with increased binge-eating behavior regardless of overall positive or negative coping and overall emotion regulation.

# create overall affilaition in a close relationship
abdul.df$afftotal <- abdul.df$sostaffp + abdul.df$sostaffw + abdul.df$sosiaffp + abdul.df$sosiaffw + abdul.df$ssotaffp + abdul.df$ssotaffw + abdul.df$ssoiaffp + abdul.df$ssoiaffw

bed.df$afftotal <- bed.df$sostaffp + bed.df$sostaffw + bed.df$sosiaffp + bed.df$sosiaffw + bed.df$ssotaffp + bed.df$ssotaffw + bed.df$ssoiaffp + bed.df$ssoiaffw


hyp3.lm <- lm(edds78 ~  afftotal + intaffw + DCS1 + DCS2 + derstot, data=bed.df) ## why didnt we do DCS3 Instead ?
summary(hyp3.lm) # only DCS2 is significant
confint(hyp3.lm)
lm.beta(hyp3.lm)
    ## Multiple R-squared:  0.2111,	Adjusted R-squared:  0.1306 
    ## F-statistic: 2.623 on 5 and 49 DF,  p-value: 0.03528
    ## the p-value is 0.03528, which is lower than 0.05, thus, significant

  ## Results show that affiliation, autonomy, and DERS significantly predict binge-eating R2= 0.21, F (4, 49) = 2.62, p = 0.04.
  ## with affiliation (B= -6.237e-06, CI= -0.001, 0.002), autonomy (B= 8.828e-04 , CI= -0.02, 0.02), Total overall dysfunction coping subscale "DCS1" (B= 2.657e-02, CI= -0.12, 0.06), Total overall dysfunction coping subscale "DCS2" (B= 2.720e-01, CI= 0.08, 0.46), DERS total (B=  4.787e-03, CI= -0.26, 0.04)     

    ## for afftotal (Affiliation) Pr(>|t|) = 0.99404, which is greater than 0.05, NOT significant 
    ## for intaffw (autonomy) Pr(>|t|) = 0.92971, which is greater than 0.05, NOT significant 
    ## for DCS1 (Total overall dysfunction coping subscale) Pr(>|t|) = 0.54981, which is greater than 0.05, NOT significant 
    ## for DCS2 (Total overall dysfunction coping) Pr(>|t|) = 0.00578, which is smaller than 0.05, thus it is significant 
    ## for derstot Pr(>|t|) = 0.75919, which is greater than 0.05, NOT significant 
    ## reject hypothesis three, the p-value is 0.03528, which is smaller than 0.05, thus, significant. ?

## I removed "DCS1" and "DCS2" with DCS3, and removed the "derstot" with DSS. because we are comparing the total of both positive and negative DERS, right?
hyp3b.lm <- lm(edds78 ~  afftotal + intaffw + DCS3 + DSS , data=bed.df) 
summary(hyp3b.lm) # Not significant 
confint(hyp3b.lm)
lm.beta(hyp3b.lm)

    ## Multiple R-squared:  0.1251,	Adjusted R-squared:  0.05516 
    ## F-statistic: 1.788 on 4 and 50 DF,  p-value: 0.1459
    ## the p-value is 0.1459, which is greater than 0.05, thus, NOT significant
## Results show that affiliation, autonomy, and DERS significantly predict binge-eating,  R2= 0.13, F (4, 50) = 1.788, p = 0.15.
## with affiliation (B= -0.001, CI= -0.002, 0.001), autonomy (B= 0.01 , CI= -0.01, 0.03), total overall negative coping subscale "DCS3" (B= 0.05, CI= -0.01, 0.11), total overall positive coping subscale "DSS" (B= 0.02, CI= -0.02, 0.05), 


## should i make a graph, if they are all not significant?

# 1. DCS2
graph6 <- ggplot(data = bed.df, aes(y=edds78, x=DCS2)) + geom_point() + geom_smooth(method="lm", se=TRUE)
graph6 



# 1. DCS2
graph6 <- ggplot(data = bed.df, aes(y=edds78, x=DCS2)) + geom_point() + geom_smooth(method="lm", se=TRUE)
graph6 


## does this mean that Hypothesis 1 and hypothesis 3 are rejected ?
## does this mean that only hypothesis 2 is accepted?


######### Demographics

## changing to factors
# Change 1 and 2 to male and female for the general dataframe

ThesisData.df$genderfactor <- factor(ThesisData.df$gender, levels = c(1:2), labels = c("Male", "Female"))

## changing 1-7 to education factors
ThesisData.df$educationfactor <- factor(ThesisData.df$education, levels = c(1:7), labels = c("No schooling completed,", "High school graduate or GED", "Associate degree", "Bachelor’s degree", "Master’s degree", "Professional degree", "Doctorate degree"))
ThesisData.df$educationfactor

## changing 1,2,3, 4 to marital factor

ThesisData.df$maritalfactor <- factor(ThesisData.df$marital, levels = c(1:5), labels = c("Single,", "Married", "Widowed", "Divorced", "Separated"))
ThesisData.df$maritalfactor

## changing from 1-9 to employment factor
ThesisData.df$employmentfactor <- factor(ThesisData.df$employment, levels = c(1:9), labels = c("Employed for wages,", "Self- employed", "No work and looking for work", "No work and not looking for work", "homemaker", "student", "Military", "Retired", "Unable to work" ))
ThesisData.df$employmentfactor
# check

## chaanging from to income factor
ThesisData.df$incomefactor <- factor(ThesisData.df$income, levels = c(1:4), labels = c("Less than $24,999", "$25,000 to $49,999", "$50,000 to $99,999", "$100,000 or more"))
ThesisData.df$incomefactor

###### Demographics
nrow(ThesisData.df)
## there are 93 participants
summary(ThesisData.df)
summary(ThesisData.df$genderfactor) 
#Gender:
## 43 male
## 50 female
summary(ThesisData.df)
sd(abdul.df$age)
#Age:
## mean age is 38.01
## Max age is 73.00
## min age is 21
## sb= 11.59
## Ethnicity:
## 42 white
## 38 asian
## 6 African American
## 3 latin
## 4 non-specified /  No answer

summary(ThesisData.df$maritalfactor)
ThesisData.df$maritalfactor
## Marital status
## 25 single
## 59 married or in a relationship
## 2 widowed
## 5 divorced
## 1 separated
## 2 NA
summary(ThesisData.df$educationfactor)
## Education
## 0 no schooling
## 17 high school or GED
## 9 associate degree
## 53 bachelors
## 13 Master's
## 0 Professional degree
## 0 doctorate
## 2 NA
summary(ThesisData.df$employmentfactor)
## Employment
## 69 employed for wages
## 14 self-employed
## 0 not working
## 6 homemaker
## 2 students
## 0 Military 
## 0 Retired
## 1  Unable to work 
## 2 NA
summary(ThesisData.df$incomefactor)
##Income
## 23 individuals less than Less than $24,999
## 32 individuals $25,000 to $49,999
## 33 individuals $50,000 to $99,999
## 4 individuals $100,000 or more
## 2 NA



## chaanging from edds7 to number of binge in last 6 months factor
ThesisData.df$edds7factor <- factor(ThesisData.df$edds7, levels = c(1:8), labels = c("1", "2", "3", "4", "5", "6", "7", "8"))
summary(ThesisData.df$edds7factor)
## 24 at least 1 in the last 6 months
## 22 at least 2
## 14 at least 3
## 8 at least 4
## 9 at least 5
## 3 at least 6
## 2 at least 7
## 0 at least 8
## 12 NA

## changing from edds8 to number of binge in last 3 months factor
ThesisData.df$edds8factor <- factor(ThesisData.df$edds8, levels = c(1:14), labels = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14"))
summary(ThesisData.df$edds8factor)
## 23 at least 1 time in the last 3 months
## 22 at least 2
## 10 at least 3
## 5 at least 4
## 2 at least 5
## 4 at least 6
## 3 at least 7
## 5 at least 8
## 1 at least 9
## 2 at least 10
## 1 at least 11
## 2 at least 12
## 2 at least 13
## 0 at least 14
## 12 NA
