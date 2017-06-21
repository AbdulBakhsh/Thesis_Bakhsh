

#-----------------#Hypothesis Testing -----------------------------


# Load the relevant model into R's working memory:
source("preliminary.R")

#----------------- Hypothesis 1 ---------------------------------------
# it is expected that a lack of emotional clarity and absence of strategies for managing emotions will be significant predictor of binge eating behavior.

############### fix the data up
# 1. Need to average 7 and 8 
abdul.df$edds78 <- (abdul.df$edds7 + abdul.df$edds8)/2

# 2. Need to select only people with EDDS 5 and 6
abdul.df$edds5f <- as.factor(abdul.df$edds5) # make it a factor
abdul.df$edds6f <- as.factor(abdul.df$edds6) # make it a factor

# make a new variable combining the results from the two variables
abdul.df$edds56 <- ifelse((abdul.df$edds5 == 2) & (abdul.df$edds6 == 2), 0, 
                           +    ifelse((abdul.df$edds5 ==1) & (abdul.df$edds6 == 1), 1, 99))

# label the new factor
abdul.df$edds56f <- factor(abdul.df$edds56, levels = c(0:1), labels = c("No-BED", "BED"))

bed.df <- subset(abdul.df, edds56f == "BED")
nobed.df <- subset(abdul.df, edds56f == "No-BED")

############### test the hypotheses

# regression for everyone regardless of with both 5 and 6 only
hyp1.lm <- lm(edds78 ~ clarity*edds56f + strategy*edds56f, data= abdul.df)
summary(hyp1.lm) # not significant


# here, use all data and include whether or not meeting criteria interacts with predicted DERS Scales
hyp1.lm1 <- lm(edds78 ~ clarity*edds56f + strategy*edds56f, data= abdul.df)
summary(hyp1.lm1) # not significant


# how about other DERS total? - not hypothesized
hyp1.lm2 <- lm(edds78 ~ derstot, data= abdul.df)
summary(hyp1.lm2) # significant but 56 don't matter


# Graph for H1 --------------------------------------
abdulna.df <- na.omit(abdul.df) # get rid of NA for graphing

# graph strategy
graph <- ggplot(data = abdulna.df, aes(y=edds78, x=strategy, colour=edds56f)) +
  geom_point() + geom_smooth(method="lm", se=TRUE)
graph # no association between edds78 for strategy

#graph clarity
graph1 <- ggplot(data = abdulna.df, aes(y=edds78, x=clarity, colour=edds56f)) +
  geom_point() + geom_smooth(method="lm", se=TRUE)
graph1 # no association between edds78 for strategy

#total ders 
graph3 <- ggplot(data = abdulna.df, aes(y=edds78, x=derstot)) +
  geom_point() + geom_smooth(method="lm", se=TRUE)
graph3 # an association in right direction but edds56 is not inlcuded



#----------------- Hypothesis 2 ---------------------------------------

#Gender, food restriction, weight-shape over-evaluation, and overall difficulties with emotion regulation are expected to be significant predictors of binge-eating behavior.

## To test for hypothesis 2, a hierarchical linear regression will be used with variables entered consistent with Whiteside’s model including: 1) gender 2) gender + food restriction 3) gender + food restriction + weight-shape over-evaluation 4) gender + food restriction + weight-shape over-evaluation + DERS total. Consistent with (Whiteside et al., 2007), no interaction terms will be included, no variables centered, and gender coded 0 for female. Also, as an extension of (Whiteside et al., 2007), analyses will be re-run with all variables centered to improve interpretability of the coefficients.

## Questions: I dont know which item from the EEDS is food restriction, weight-shape
 
## 1. Make Fear varaiable - 2,3,4 (sum up)
abdulna.df$edds234 <- abdulna.df$edds2 + abdulna.df$edds3 + abdulna.df$edds4
bed.df$edds234 <- bed.df$edds2 + bed.df$edds3 + bed.df$edds4

hyp2.lm <- lm(edds78 ~ genderfactor + edds17 + edds234 + derstot, data= bed.df)
summary(hyp2.lm) #edds17 (restricting) and edds234 (fear) significant
lm.beta(hyp2.lm)

# graph
# 1. fear
graph4 <- ggplot(data = bed.df, aes(y=edds78, x=edds234)) +
  geom_point() + geom_smooth(method="lm", se=TRUE)
graph4 

# 2. restrict
graph5 <- ggplot(data = bed.df, aes(y=edds78, x=edds17)) +
  geom_point() + geom_smooth(method="lm", se=TRUE)
graph5 

#----------------- Hypothesis 3 ---------------------------------------

#Lower levels of self-concept affiliation and lower levels of affiliation and higher autonomy in a close relationship will be associated with increased binge-eating behavior regardless of overall positive or negative coping and overall emotion regulation.

# create overall affilaition in a close relationship
abdul.df$afftotal <- abdul.df$sostaffp + abdul.df$sostaffw + abdul.df$sosiaffp + abdul.df$sosiaffw + abdul.df$ssotaffp + abdul.df$ssotaffw + abdul.df$ssoiaffp + abdul.df$ssoiaffw

bed.df$afftotal <- bed.df$sostaffp + bed.df$sostaffw + bed.df$sosiaffp + bed.df$sosiaffw + bed.df$ssotaffp + bed.df$ssotaffw + bed.df$ssoiaffp + bed.df$ssoiaffw


hyp3.lm <- lm(edds78 ~  afftotal + intaffw + DCS1 + DCS2 + derstot, data=bed.df)
summary(hyp3.lm) #
lm.beta(hyp3.lm)

# 1. DCS2
graph6 <- ggplot(data = bed.df, aes(y=edds78, x=DCS2)) +
  geom_point() + geom_smooth(method="lm", se=TRUE)
graph6 
