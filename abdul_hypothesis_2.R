
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
lm.beta(hyp1.lm)
confint(hyp1.lm)

test.lm <- glm(edds78 ~ clarity + strategy, data = bed.df, family = poisson(link = "log"))
summary(test.lm)
# Multiple R-squared:  0.1178,	Adjusted R-squared:  0.03765
    ## Multiple R-squared has a valuse of 0.1178
    ## Interpretation: Model accounts for 0.1178 of the model. 

##sqrt(0.1178)
    ## the pearson correlation coedicient is 0.34

## F-statistic:  1.47 on 5 and 55 DF,  p-value: 0.2147
    ## not significant because F is 1.45, which is not significant at p < 0.2147.
    ## for it to be significant F has to be less than p 0.05 and our P is p < 0.2147, thus not significant
    ## if this observed significance is less than 0.05, then it would have an effect. 

## The results tells us that there is less than 21.47%  chance that an F-ratio this large would happen if the null hupothesis were true. Therefore, we CANNOT conclude that our regression model results in significiantly better prediction of binge eating than if we used the mean valuse of binge eating. In short, the regression model (emotional clarity and absence of strategies) overal does NOT predicts binge eating and is not significant. 

    ## reject hypotheis 1, emotional clarity and absence of strategies for managing emotions are NOT significant predictor of binge eating behavior??


# here, use all data and include whether or not meeting criteria interacts with predicted DERS Scales  
    ##(WHAT is the diffrence between hyp1.lm and hyp1.lm1 ) ??
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


## why dont we remove all non-BED participants?? 



#----------------- Hypothesis 2 ---------------------------------------

#hypothesis 2: Gender, food restriction, weight-shape over-evaluation, and overall difficulties with emotion regulation are expected to be significant predictors of binge-eating behavior.

## edds17 is food restriction
## (edds234)= edds2, edds3, edds4 is weight and shape

## 1. Make Fear varaiable - 2,3,4 (sum up) (wieght-shape)
abdulna.df$edds234 <- abdulna.df$edds2 + abdulna.df$edds3 + abdulna.df$edds4
bed.df$edds234 <- bed.df$edds2 + bed.df$edds3 + bed.df$edds4

hyp2.lm <- lm(edds78 ~ genderfactor + edds17 + edds234 + derstot, data= bed.df)
summary(hyp2.lm) #edds17 (restricting) significant

#hyp2.lma <- lm(edds78 ~ genderfactor*edds17, data= bed.df)
#summary(hyp2.lma)
## F-statistic: 2.887 on 4 and 50 DF,  p-value: 0.03158
    ##Multiple R-squared has a valuse of 0.03158, Interpretation: Model accounts for 0.03158 of the model.
    ## P value is is less than 0.05, thus it is significant, hypothesis 2 not rejected ???
    ## for Gender, food ristriction, weight-shape over-evaluation, and overall difficulties with emotion regulation are significant predictors of bed-behaviors
    
    ## for gender, Pr(>|t|) = 0.6927 which is greater than P=0.05, thus NOT significant
    ## for edds17 (food restriction) Pr(>|t|) =  0.0400 which is less than P=0.05, thus it is significant
    ## for edds234 (fear) Pr(>|t|) =  0.0941 which is greater than P=0.05, thus it is NOT significant
    ## for derstot (overall difficulties with emotion regulation), Pr(>|t|) =  0.4863 which is greater than P=0.05, thus it is NOT significant
    ## I dont understand how edds17 (restricting) and edds234 (fear) are significant??

## Below i did how regressions in 4 level as Whiteside did, is this correct??

## A. testing edds78 with gender 
hyp2a.lm <- lm(edds78 ~ genderfactor, data= bed.df)
summary(hyp2a.lm) 
  ## Multiple R-squared:  0.01795,  p-value: 0.3295
  ## gender at step 1 explained 1.79% of binge eating variance
  ## NOT significant p= 0.3295 is bigger than 0.05


## B. testing eeds78 with gender + food restriction 
hyp2b.lm <- lm(edds78 ~ genderfactor + edds17, data= bed.df)
summary(hyp2b.lm)
  ## Multiple R-squared:  0.1238,  p-value: 0.03216
  ## For level 2, the avarage times the participants skipped meals in order to change his or her weight (food restriction) accounted for 12.38% of binge eating   variance
  #Gender and food restriction at step 2 explained 12.38% of binge eating varaince. 
  ## Significant p= 0.03216 is lower than 0.05

## C. testing edds78 with gender + food restriction + weight-shape over-evaluation
hyp2c.lm <- lm(edds78 ~ genderfactor + edds17 + edds234, data= bed.df)
summary(hyp2c.lm)
  ## Multiple R-squared:  0.1796, p-value: 0.01699
  ## for level 3 of the hierarchical linear regression, the degree to which the participants evaluated themselve based on weight-shape over-evaluation explained 17.96% of variance over the gender variance 
  ## significant because p= 0.01699 which is less than 0.05


## D. tetsing gender + food restriction + weight-shape over-evaluation + DERS total.
hyp2d.lm <- lm(edds78 ~ genderfactor + edds17 + edds234 + derstot, data= bed.df)
summary(hyp2d.lm) 
  ##Multiple R-squared: 0.1876, p-value: 0.03158
  #consitant with our expectation, level 4 DERS accounted for 18.76% of variance in binge eating. 

## testing edds234
hyp2e.lm <- lm(edds78 ~ edds234, data= bed.df)
summary(hyp2e.lm) ## here it is significant, but not in the previous one, WHY??????
  ## F-statistic: 4.054 on 1 and 53 DF,  p-value: 0.04916

lm.beta(hyp2.lm)
    ## these estimates tell us the number of standard devenation by which the outcome will change as a result of one standard deviation change in the predictor
    ## the standarized beta values for edds17 and edds234 are almost identical and close to each other (0.2855 and 0.2234), indicating that both variables have comparable degree of importance in the model. 


# graphs for hypothesis 2
# 1. fear
graph4 <- ggplot(data = bed.df, aes(y=edds78, x=edds234)) + geom_point() + geom_smooth(method="lm", se=TRUE)
graph4 

# 2. restrict
graph5 <- ggplot(data = bed.df, aes(y=edds78, x=edds17)) + geom_point() + geom_smooth(method="lm", se=TRUE)
graph5 

## are we just making graphs only for edds234 and edds17 because there were significant??



#----------------- Hypothesis 3 ---------------------------------------

#Lower levels of self-concept affiliation and lower levels of affiliation and higher autonomy in a close relationship will be associated with increased binge-eating behavior regardless of overall positive or negative coping and overall emotion regulation.

# create overall affilaition in a close relationship
abdul.df$afftotal <- abdul.df$sostaffp + abdul.df$sostaffw + abdul.df$sosiaffp + abdul.df$sosiaffw + abdul.df$ssotaffp + abdul.df$ssotaffw + abdul.df$ssoiaffp + abdul.df$ssoiaffw

bed.df$afftotal <- bed.df$sostaffp + bed.df$sostaffw + bed.df$sosiaffp + bed.df$sosiaffw + bed.df$ssotaffp + bed.df$ssotaffw + bed.df$ssoiaffp + bed.df$ssoiaffw


hyp3.lm <- lm(edds78 ~  afftotal + intaffw + DCS1 + DCS2 + derstot, data=bed.df)
summary(hyp3.lm) #
    ## Multiple R-squared:  0.2111, p-value: 0.03528
    ## the p-value is 0.1306, which is greater than 0.05, thus, NOT significant
    ## for afftotal (Affiliation) Pr(>|t|) = 0.99404, which is greater than 0.05, NOT significant 
    ## for intaffw (autonomy) Pr(>|t|) = 0.92971, which is greater than 0.05, NOT significant 
    ## for DCS1 (Total overall dysfunction coping subscake) Pr(>|t|) = 0.54981, which is greater than 0.05, NOT significant 
    ## for DCS2 (Total overall dysfunction coping) Pr(>|t|) = 0.00578, which is smaller than 0.05, thus it is significant 
    ## for derstot Pr(>|t|) = 0.75919, which is greater than 0.05, NOT significant 
    ## reject hypothesis three, the p-value is 0.1306, which is greater than 0.05, thus, NOT significant. ?

lm.beta(hyp3.lm)
    ##  dont know what this means?
 
hyp3a.lm <- lm(edds78 ~  afftotal + intaffw + DCS1 + DCS2 , data=bed.df)
summary(hyp3a.lm) #
    ## Multiple R-squared:  0.2096, p-value: 0.01744


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
