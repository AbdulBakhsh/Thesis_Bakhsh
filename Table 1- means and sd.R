
## 7/20/2017
# Means and standard deviations for all variables

## abdul.df (all participants)
## bed.df only bed



######### Demographics

## changing to factors
# Change 1 and 2 to male and female for the general dataframe

abdul.df$genderfactor <- factor(abdul.df$gender, levels = c(1:2), labels = c("Male", "Female"))

## changing 1-7 to education factors
abdul.df$educationfactor <- factor(abdul.df$education, levels = c(1:7), labels = c("No schooling completed,", "High school graduate or GED", "Associate degree", "Bachelor’s degree", "Master’s degree", "Professional degree", "Doctorate degree"))
abdul.df$educationfactor

## changing 1,2,3, 4 to marital factor

abdul.df$maritalfactor <- factor(abdul.df$marital, levels = c(1:5), labels = c("Single,", "Married", "Widowed", "Divorced", "Separated"))
abdul.df$maritalfactor

## changing from 1-9 to employment factor
abdul.df$employmentfactor <- factor(abdul.df$employment, levels = c(1:9), labels = c("Employed for wages,", "Self- employed", "No work and looking for work", "No work and not looking for work", "homemaker", "student", "Military", "Retired", "Unable to work" ))
abdul.df$employmentfactor
# check

## chaanging from to income factor
abdul.df$incomefactor <- factor(abdul.df$income, levels = c(1:4), labels = c("Less than $24,999", "$25,000 to $49,999", "$50,000 to $99,999", "$100,000 or more"))
abdul.df$incomefactor

###### Demographics
nrow(abdul.df)
## there are 93 participants
summary(abdul.df)
summary(abdul.df$genderfactor) 
#Gender:
## 43 male
## 50 female
summary(abdul.df)
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

summary(abdul.df$maritalfactor)
ThesisData.df$maritalfactor
## Marital status
## 25 single
## 59 married or in a relationship
## 2 widowed
## 5 divorced
## 1 separated

summary(abdul.df$educationfactor)
## Education
## 0 no schooling
## 17 high school or GED
## 9 associate degree
## 53 bachelors
## 13 Master's
## 0 Professional degree
## 0 doctorate

summary(abdul.df$employmentfactor)
## Employment
## 69 employed for wages
## 14 self-employed
## 0 not working
## 6 homemaker
## 2 students
## 0 Military 
## 0 Retired
## 1  Unable to work 

summary(abdul.df$incomefactor)
##Income
## 23 individuals less than Less than $24,999
## 32 individuals $25,000 to $49,999
## 33 individuals $50,000 to $99,999
## 4 individuals $100,000 or more


## chaanging from edds7 to number of binge in last 6 months factor
abdul.df$edds7factor <- factor(abdul.df$edds7, levels = c(1:8), labels = c("1", "2", "3", "4", "5", "6", "7", "8"))
summary(abdul.df$edds7factor)
## 24 at least 1 in the last 6 months
## 22 at least 2
## 14 at least 3
## 8 at least 4
## 9 at least 5
## 3 at least 6
## 2 at least 7
## 0 at least 8
## 10 NA

## changing from edds8 to number of binge in last 3 months factor
abdul.df$edds8factor <- factor(abdul.df$edds8, levels = c(1:14), labels = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14"))
summary(abdul.df$edds8factor)
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
## 10 NA



## Demographics means and sd
  ## age all age
summary(abdul.df$age)
sd(abdul.df$age)
    ## Mean= 38.01, 11.59

## age BED
  summary(bed.df$age)
  sd(bed.df$age)
## DERS
  ## Impulse
  summary(abdul.df$impulse)
  sd(abdul.df$impulse)
    ## mean= 13.96 Sd= 5.23
  summary(bed.df$impulse)
  sd(bed.df$impulse)
  
  ## aware
  summary(abdul.df$aware)
  sd(abdul.df$aware)
    ## mean= 21.26 Sd= 3.97
  summary(bed.df$aware)
  sd(bed.df$aware)
  
  ## strategy
  summary(abdul.df$strategy)
  sd(abdul.df$strategy)
    ## Mean= 20.58, sd= 6.91
  summary(bed.df$strategy)
  sd(bed.df$strategy)
  
  ## clarity
  summary(abdul.df$clarity)
  sd(abdul.df$clarity)
    ## Mean= 13.45, sd= 2.79
  summary(bed.df$clarity)
  sd(bed.df$clarity)
  
  ## nonaccept
  summary(abdul.df$nonaccept)
  sd(abdul.df$nonaccept)
  ## mean= 14.87, sd= 6.01
  summary(bed.df$nonaccept)
  sd(bed.df$nonaccept)
  
  ## goals
  summary(abdul.df$goals)
  sd(abdul.df$goals)
    ## mean= 14.12, sd= 4.02
  summary(bed.df$goals)
  sd(bed.df$goals)
  
  ## derstot
  summary(abdul.df$derstot)
  sd(abdul.df$derstot)
    ## mean= 98.23, sd= 21.10  
  summary(bed.df$derstot)
  sd(bed.df$derstot)
  
 
## SASB

  ## overall affilaition in a close relationship for BED only 
  summary(abdul.df$afftotal)
  sd(abdul.df$afftotal)
  ## mean= 385.60 sd= NA
  
  summary(bed.df$afftotal)
  sd(bed.df$afftotal)
  ## Mean= 345.70m sd= 391.49 
  
## intaffw (Introject, Affiliation) ?? or autonomy
  summary(bed.df$afftotal)
  sd(bed.df$afftotal)
  
  
  ## intaffp
  ## intautp (Autonomy)

  ## intautw (Introject, Autonomy)
  ## sostaffp 
  ## sostautp 
  ## sostaffw 
  ## sostautw 
  ## sosiaffp 
  ## sosiautp 
  ## sosiaffw 
  ## sosiautw 
  ## ssotaffp 
  ## ssotautp 
  ## ssotaffw 
  ## ssotautw 
  ## ssoiaffp 
  ## ssoiautp 
  ## ssoiaffw 
  ## ssoiautw 
  ## DSS 
  ## DCS1 
  ## DCS2 
  ## DCS3

## eddsX
  
  ## average edds7 and 8 (number of binge)
  summary(bed.df$edds78)
  sd(bed.df$edds78)
  ## mean= 3.527, sd= 2.29
  
  summary(abdul.df$edds78)
  sd(abdul.df$edds78)

  
  ## weight-shape over-evaluation,
  summary(bed.df$edds234)
  sd(bed.df$edds234)
  ## M= 15.13, sd= 4.76
  
  summary(abdul.df$edds234)
  sd(abdul.df$edds234)
  
  ## Food restriction
  summary(bed.df$edds17)
  sd(bed.df$edds17)
  
  
  summary(abdul.df$edds17)
  sd(abdul.df$edds17)


  ## Dialectical Behavioral Therapy Ways of Coping Checklist (wcclX)
  
  #Total overall positive coping 
  summary(abdul.df$DSS)
  sd(abdul.df$DSS)
  ## mean= 99.09, sd= NA
  summary(bed.df$DSS)
  sd(bed.df$DSS)  
  ## mean = 98.13, sd= 20.97
  
  
  ##  DCS1 (general dysfunctional coping)
  summary(abdul.df$DCS1)
  sd(abdul.df$DCS1)
  ## mean= 38.44, sd= NA
  
  summary(bed.df$DCS1)
  sd(bed.df$DCS1)
  ## mean= 40.67, sd= 8.06
  
  
  ## DCS2 (Total overall dysfunction coping) blaming others factor
  summary(abdul.df$DCS2)
  sd(abdul.df$DCS2)
  ## mean= 38.44, sd= NA
  
  summary(bed.df$DCS2)
  sd(bed.df$DCS2)
  
  ## DCS3 (Total overall dysfunction coping)
  summary(abdul.df$DCS3)
  sd(abdul.df$DCS3)
  ## mean= 38.44, sd= NA
  
  summary(bed.df$DCS3)
  sd(bed.df$DCS3)