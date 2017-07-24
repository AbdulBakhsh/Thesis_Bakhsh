
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

summary(nobed.df) ## five participants did not complete the full servey (edds5, and edds6)?
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
abdulna.df <- na.omit(bed.df) # get rid of NA for graphing

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

abdul.df$edds234 <- abdul.df$edds2 + abdul.df$edds3 + abdul.df$edds4

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

#A more hostile self-concept, more hostile levels of affiliation in a close relationship, and higher autonomy in a close relationship will be associated with increased binge-eating behavior regardless of overall positive or negative coping and overall emotion regulation.
# higher score on affiliation the more friendly; lower less friendly (hostile)

# create overall affilaition in a close relationship
bed.df$afftotal <- bed.df$sostaffp + bed.df$sostaffw + bed.df$sosiaffp + bed.df$sosiaffw + bed.df$ssotaffp + bed.df$ssotaffw + bed.df$ssoiaffp + bed.df$ssoiaffw

bed.df$auttot <- bed.df$sostautp + bed.df$sostautw + bed.df$sosiautp + bed.df$sosiautw + bed.df$ssotautp + bed.df$ssotautw + bed.df$ssoiautp + bed.df$ssoiautw

# introject
bed.df$intafftot <- bed.df$intaffp + bed.df$intaffw # self concept (introject) affiliation
bed.df$intauttot <- bed.df$intautp + bed.df$intautw # self concept (introject) autonomy

## Close relationship
hyp3.lm <- lm(edds78 ~  afftotal + auttot + DCS3 +DSS +  derstot, data=bed.df)
summary(hyp3.lm) # only DCS2 is significant
confint(hyp3.lm)
lm.beta(hyp3.lm)
   
# introject (self-concept)
hyp3a.lm <- lm(edds78 ~  intafftot + intauttot + DCS3 +DSS +  derstot, data=bed.df) 
summary(hyp3a.lm) # only DCS2 is significant
confint(hyp3a.lm)
lm.beta(hyp3a.lm)


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



## should i make a graph, if they are all not significant?

# 1. DCS2
graph6 <- ggplot(data = bed.df, aes(y=edds78, x=DCS2)) + geom_point() + geom_smooth(method="lm", se=TRUE)
graph6 



# 1. DCS2
graph6 <- ggplot(data = bed.df, aes(y=edds78, x=DCS2)) + geom_point() + geom_smooth(method="lm", se=TRUE)
graph6 


## does this mean that Hypothesis 1 and hypothesis 3 are rejected ?
## does this mean that only hypothesis 2 is accepted?
