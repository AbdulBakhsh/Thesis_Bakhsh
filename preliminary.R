library(QuantPsyc)
library(car)
library(pastecs)
library(ggplot2)
library(multcomp)
library(compute.es)
library(psych)
library(Hmisc)
library(reshape)

# Clean the data -----------------------------------------------------------------------------
graphics.off() # This closes all of R's graphics windows.
rm(list=ls()) 

ThesisData <-read.csv ("ThesisData.csv", header = TRUE)


#----------------Renaming----------------------#

## renaming Questions 2.2 - 3.6 to age + demographics
ThesisData <- rename(ThesisData, c(Q2.2="age", Q3.1="gender", Q3.2="ethnicity", Q3.3="education", Q3.4="marital", Q3.5="employment", Q3.6="income"))

## renaming Questions 4.2 – 4.2.36 to dersX
ThesisData <- rename(ThesisData, c(Q4.2_1="ders1", Q4.2_2="ders2", Q4.2_3="ders3", Q4.2_4="ders4", Q4.2_5="ders5", Q4.2_6="ders6", Q4.2_7="ders7", Q4.2_8="ders8", Q4.2_9="ders9", Q4.2_10="ders10", Q4.2_11="ders11", Q4.2_12="ders12",Q4.2_13="ders13", Q4.2_14="ders14", Q4.2_15="ders15", Q4.2_16="ders16", Q4.2_17="ders17", Q4.2_18="ders18", Q4.2_19="ders19", Q4.2_20="ders20", Q4.2_21="ders21", Q4.2_22="ders22", Q4.2_23="ders23",Q4.2_24="ders24", Q4.2_25="ders25", Q4.2_26="ders26", Q4.2_27="ders27", Q4.2_28="ders28", Q4.2_28="ders28", Q4.2_29="ders29", Q4.2_30="ders30", Q4.2_31="ders31", Q4.2_32="ders32", Q4.2_33="ders33", Q4.2_34="ders34", Q4.2_35="ders35", Q4.2_36="ders36"))

## renaming Questions 5.2.1 – 5.7.32 to sasbx
ThesisData <- rename(ThesisData, c(Q5.2_1="intcl1b", Q5.2_2="intcl2b", Q5.2_3="intcl3b", Q5.2_4="intcl4b", Q5.2_5="intcl5b", Q5.2_6="intcl6b", Q5.2_7="intcl7b", Q5.2_8="intcl8b", Q5.4_1="intcl1w", Q5.4_2="intcl2w", Q5.4_3="intcl3w", Q5.4_4="intcl4w", Q5.4_5="intcl5w", Q5.4_6="intcl6w", Q5.4_7="intcl7w", Q5.4_8="intcl8w", Q5.6_1="sost2b", Q5.6_2="sosi8b", Q5.6_3="sost6b", Q5.6_4="sost8b", Q5.6_5="sosi4b", Q5.6_6="sost3b", Q5.6_7="sosi6b", Q5.6_8="sosi2b", Q5.6_9="sost5b", Q5.6_10="sosi5b", Q5.6_11="sosi1b", Q5.6_12="sost7b", Q5.6_13="sost4b", Q5.6_14="sost1b", Q5.6_15="sosi3b", Q5.6_16="sosi7b", Q5.6_17="ssot2b", Q5.6_18="ssoi8b", Q5.6_19="ssot6b", Q5.6_20="ssot8b", Q5.6_21="ssoi4b", Q5.6_22="ssot3b", Q5.6_23="ssoi6b", Q5.6_24="ssoi2b", Q5.6_25="ssot5b", Q5.6_26="ssoi5b", Q5.6_27="ssoi1b", Q5.6_28="ssot7b", Q5.6_29="ssot4b", Q5.6_30="ssot1b", Q5.6_31="ssoi3b", Q5.6_32="ssoi7b", Q5.7_1="sost2w", Q5.7_2="sosi8w", Q5.7_3="sost6w", Q5.7_4="sost8w", Q5.7_5="sosi4w", Q5.7_6="sost3w", Q5.7_7="sosi6w", Q5.7_8="sosi2w", Q5.7_9="sost5w", Q5.7_10="sosi5w", Q5.7_11="sosi1w", Q5.7_12="sost7w", Q5.7_13="sost4w", Q5.7_14="sost1w", Q5.7_15="sosi3w", Q5.7_16="sosi7w", Q5.7_17="ssot2w", Q5.7_18="ssoi8w", Q5.7_19="ssot6w", Q5.7_20="ssot8w", Q5.7_21="ssoi4w", Q5.7_22="ssot3w", Q5.7_23="ssoi6w", Q5.7_24="ssoi2w", Q5.7_25="ssot5w", Q5.7_26="ssoi5w", Q5.7_27="ssoi1w", Q5.7_28="ssot7w", Q5.7_29="ssot4w", Q5.7_30="ssot1w", Q5.7_31="ssoi3w", Q5.7_32="ssoi7w"))

## Renaming Q6.1- 6.20 to eddsX
ThesisData <- rename(ThesisData, c(Q6.1_1="edds1", Q6.1_2="edds2", Q6.1_3="edds3", Q6.1_4="edds4", Q6.2="edds5", Q6.3= "edds6", Q6.4="edds7", Q6.5="edds8", Q6.6="edds9",  Q6.7="edds10" , Q6.8="edds11" , Q6.9="edds12" , Q6.10="edds13" , Q6.11= "edds14", Q6.12= "edds15", Q6.13="edds16" , Q6.14="edds17" , Q6.15="edds18" , Q6.16="edds19" , Q6.17="edds20" , Q6.18="edds21" , Q6.19="edds22" , Q6.20="edds23"))

## renaming Questions 7.2_1 – 7.2.59 to wcclX
ThesisData <- rename(ThesisData, c(Q7.2_1="wccl1", Q7.2_2="wccl2", Q7.2_3="wccl3", Q7.2_4="wccl4", Q7.2_5="wccl5", Q7.2_6="wccl6", Q7.2_7="wccl7", Q7.2_8="wccl8", Q7.2_9="wccl9", Q7.2_10="wccl10", Q7.2_11="wccl11", Q7.2_12="wccl12",Q7.2_13="wccl13", Q7.2_14="wccl14", Q7.2_15="wccl15", Q7.2_16="wccl16", Q7.2_17="wccl17", Q7.2_18="wccl18", Q7.2_19="wccl19", Q7.2_20="wccl20", Q7.2_21="wccl21", Q7.2_22="wccl22", Q7.2_23="wccl23",Q7.2_24="wccl24", Q7.2_25="wccl25", Q7.2_26="wccl26", Q7.2_27="wccl27", Q7.2_28="wccl28", Q7.2_29="wccl29", Q7.2_30="wccl30", Q7.2_31="wccl31", Q7.2_32="wccl32", Q7.2_33="wccl33", Q7.2_34="wccl34", Q7.2_35="wccl35", Q7.2_36="wccl36", Q7.2_37="wccl37", Q7.2_38="wccl38", Q7.2_39="wccl39", Q7.2_40="wccl40", Q7.2_41="wccl41", Q7.2_42="wccl42", Q7.2_43="wccl43", Q7.2_44="wccl44", Q7.2_45="wccl45", Q7.2_46="wccl46", Q7.2_47="wccl47", Q7.2_48="wccl48", Q7.2_49="wccl49", Q7.2_50="wccl50", Q7.2_51="wccl51", Q7.2_52="wccl52", Q7.2_53="wccl53", Q7.2_54="wccl54", Q7.2_55="wccl55", Q7.2_56="wccl56", Q7.2_57="wccl57", Q7.2_58="wccl58", Q7.2_59="wccl59"))

# Change 1 and 2 to male and female for DERS dataframe
ThesisData$genderfactor <- factor(ThesisData$gender, levels = c(1:2), labels = c("Male", "Female"))

#################### Recoding DERS ####################:
#making the DERS scales and recoding
## Ders
## --- 
library(car)
ThesisData$ders1r <- recode(ThesisData$ders1, "1=5;  2=4;  3=3 ; 4=2;  5=1")
ThesisData$ders2r <- recode(ThesisData$ders2, "1=5;  2=4;  3=3 ; 4=2;  5=1")
ThesisData$ders6r <- recode(ThesisData$ders6, "1=5;  2=4;  3=3 ; 4=2;  5=1")
ThesisData$ders7r <- recode(ThesisData$ders7, "1=5;  2=4;  3=3 ; 4=2;  5=1")
ThesisData$ders8r <- recode(ThesisData$ders8, "1=5;  2=4;  3=3 ; 4=2;  5=1")
ThesisData$ders10r <- recode(ThesisData$ders10, "1=5;  2=4;  3=3 ; 4=2;  5=1")
ThesisData$ders17r <- recode(ThesisData$ders17, "1=5;  2=4;  3=3 ; 4=2;  5=1")
ThesisData$ders20r <- recode(ThesisData$ders20, "1=5;  2=4;  3=3 ; 4=2;  5=1")
ThesisData$ders22r <- recode(ThesisData$ders22, "1=5;  2=4;  3=3 ; 4=2;  5=1")
ThesisData$ders24r <- recode(ThesisData$ders24, "1=5;  2=4;  3=3 ; 4=2;  5=1")
ThesisData$ders34r <- recode(ThesisData$ders34, "1=5;  2=4;  3=3 ; 4=2;  5=1")

### DERS Scales
ThesisData$impulse <- ThesisData$ders32 + ThesisData$ders27 + ThesisData$ders14 + ThesisData$ders19 + ThesisData$ders3 + ThesisData$ders24r
ThesisData$aware <- ThesisData$ders6 + ThesisData$ders2 + ThesisData$ders10 + ThesisData$ders17 + ThesisData$ders8 + ThesisData$ders34r
ThesisData$strategy <- ThesisData$ders16 + ThesisData$ders15 + ThesisData$ders31 + ThesisData$ders35 + ThesisData$ders28 + ThesisData$ders22 + ThesisData$ders36 + ThesisData$ders30
ThesisData$clarity <- ThesisData$ders5 + ThesisData$ders4 + ThesisData$ders9 + ThesisData$ders7 + ThesisData$ders1
ThesisData$nonaccept <- ThesisData$ders25 + ThesisData$ders21 + ThesisData$ders12 + ThesisData$ders11 + ThesisData$ders29 + ThesisData$ders23
ThesisData$goals <- ThesisData$ders26 + ThesisData$ders18 + ThesisData$ders13 + ThesisData$ders33 + ThesisData$ders20
ThesisData$derstot <- ThesisData$impulse + ThesisData$aware + ThesisData$strategy + ThesisData$clarity + ThesisData$nonaccept + ThesisData$goals


####### SASB Summarizing after renaming ######

################################# Recoding SASB ##########################################
###### 1. Introject Pre (introject)
## Affiliation

# change v.df to ThesisData
# change "p" at the end to "b"
# change "a" at the end to "w"
# change "pre" to "p"
# change "post" to "w"


ThesisData$intaffp <- (0*ThesisData$intcl1b) + (4.5*ThesisData$intcl2b) + (7.8*ThesisData$intcl3b) + (4.5*ThesisData$intcl4b) + (0*ThesisData$intcl5b) + (-4.5*ThesisData$intcl6b) + (-7.8*ThesisData$intcl7b) + (-4.5*ThesisData$intcl8b)


## Autonomy
ThesisData$intautp <- (7.8*ThesisData$intcl1b) + (4.5*ThesisData$intcl2b) + (0*ThesisData$intcl3b) + (-4.5*ThesisData$intcl4b) + (-7.8*ThesisData$intcl5b) + (-4.5*ThesisData$intcl6b) + (0*ThesisData$intcl7b) + (4.5*ThesisData$intcl8b)


###### 2. Introject w  (introject)
## Affiliation
ThesisData$intaffw <- (0*ThesisData$intcl1w) + (4.5*ThesisData$intcl2w) + (7.8*ThesisData$intcl3w) + (4.5*ThesisData$intcl4w) + (0*ThesisData$intcl5w) + (-4.5*ThesisData$intcl6w) + (-7.8*ThesisData$intcl7w) + (-4.5*ThesisData$intcl8w)


## Autonomy
ThesisData$intautw <- (7.8*ThesisData$intcl1w) + (4.5*ThesisData$intcl2w) + (0*ThesisData$intcl3w) + (-4.5*ThesisData$intcl4w) + (-7.8*ThesisData$intcl5w) + (-4.5*ThesisData$intcl6w) + (0*ThesisData$intcl7w) + (4.5*ThesisData$intcl8w)

####################################### SOST and SOSI #############################
###### 3. SOST b (signficant to subject transitive - acting - top surface)
## Affiliation
ThesisData$sostaffp <- (0*ThesisData$sost1b) + (4.5*ThesisData$sost2b) + (7.8*ThesisData$sost3b) + (4.5*ThesisData$sost4b) + (0*ThesisData$sost5b) +
  (-4.5*ThesisData$sost6b) + (-7.8*ThesisData$sost7b) + (-4.5*ThesisData$sost8b)


## Autonomy
ThesisData$sostautp <- (7.8*ThesisData$sost1b) + (4.5*ThesisData$sost2b) + (0*ThesisData$sost3b) + (-4.5*ThesisData$sost4b) + (-7.8*ThesisData$sost5b) +
  (-4.5*ThesisData$sost6b) + (0*ThesisData$sost7b) + (4.5*ThesisData$sost8b)

###### 4. SOST w (signficant to subject transitive - acting - top surface)
## Affiliation
ThesisData$sostaffw <- (0*ThesisData$sost1w) + (4.5*ThesisData$sost2w) + (7.8*ThesisData$sost3w) + (4.5*ThesisData$sost4w) + (0*ThesisData$sost5w) +
  (-4.5*ThesisData$sost6w) + (-7.8*ThesisData$sost7w) + (-4.5*ThesisData$sost8w)

## Autonomy
ThesisData$sostautw <- (7.8*ThesisData$sost1w) + (4.5*ThesisData$sost2w) + (0*ThesisData$sost3w) + (-4.5*ThesisData$sost4w) + (-7.8*ThesisData$sost5w) +(-4.5*ThesisData$sost6w) + (0*ThesisData$sost7w) + (4.5*ThesisData$sost8w)

###### 5. SOSI b (significant other to subject intransitive - reacting - second surface)
## Affiliation
ThesisData$sosiaffp <- (0*ThesisData$sosi1b) + (4.5*ThesisData$sosi2b) + (7.8*ThesisData$sosi3b) + (4.5*ThesisData$sosi4b) + (0*ThesisData$sosi5b) +
  (-4.5*ThesisData$sosi6b) + (-7.8*ThesisData$sosi7b) + (-4.5*ThesisData$sosi8b)


## Autonomy
ThesisData$sosiautp <- (7.8*ThesisData$sosi1b) + (4.5*ThesisData$sosi2b) + (0*ThesisData$sosi3b) + (-4.5*ThesisData$sosi4b) + (-7.8*ThesisData$sosi5b) + (-4.5*ThesisData$sosi6b) + (0*ThesisData$sosi7b) + (4.5*ThesisData$sosi8b)

###### 6. SOSI w (significant other to subject intransitive - reacting - second surface)
## Affiliation
ThesisData$sosiaffw <- (0*ThesisData$sosi1w) + (4.5*ThesisData$sosi2w) + (7.8*ThesisData$sosi3w) + (4.5*ThesisData$sosi4w) + (0*ThesisData$sosi5w) +
  (-4.5*ThesisData$sosi6w) + (-7.8*ThesisData$sosi7w) + (-4.5*ThesisData$sosi8w)

## Autonomy
ThesisData$sosiautw <- (7.8*ThesisData$sosi1w) + (4.5*ThesisData$sosi2w) + (0*ThesisData$sosi3w) + (-4.5*ThesisData$sosi4w) + (-7.8*ThesisData$sosi5w) +
  (-4.5*ThesisData$sosi6w) + (0*ThesisData$sosi7w) + (4.5*ThesisData$sosi8w)

####################################### SSOT and SSOI #############################
###### SSOT b (subject to significant other transitive - acting - top surface)
## Affiliation
ThesisData$ssotaffp <- (0*ThesisData$ssot1b) + (4.5*ThesisData$ssot2b) + (7.8*ThesisData$ssot3b) + (4.5*ThesisData$ssot4b) + (0*ThesisData$ssot5b) +
  (-4.5*ThesisData$ssot6b) + (-7.8*ThesisData$ssot7b) + (-4.5*ThesisData$ssot8b)


## Autonomy
ThesisData$ssotautp <- (7.8*ThesisData$ssot1b) + (4.5*ThesisData$ssot2b) + (0*ThesisData$ssot3b) + (-4.5*ThesisData$ssot4b) + (-7.8*ThesisData$ssot5b) +
  (-4.5*ThesisData$ssot6b) + (0*ThesisData$ssot7b) + (4.5*ThesisData$ssot8b)


###### 8. SSOT w (subject to significant other transitive - acting - top surface)
## Affiliation
ThesisData$ssotaffw <- (0*ThesisData$ssot1w) + (4.5*ThesisData$ssot2w) + (7.8*ThesisData$ssot3w) + (4.5*ThesisData$ssot4w) + (0*ThesisData$ssot5w) +
  (-4.5*ThesisData$ssot6w) + (-7.8*ThesisData$ssot7w) + (-4.5*ThesisData$ssot8w)


## Autonomy
ThesisData$ssotautw <- (7.8*ThesisData$ssot1w) + (4.5*ThesisData$ssot2w) + (0*ThesisData$ssot3w) + (-4.5*ThesisData$ssot4w) + (-7.8*ThesisData$ssot5w) +
  (-4.5*ThesisData$ssot6w) + (0*ThesisData$ssot7w) + (4.5*ThesisData$ssot8w)


###### 9. SSOI b (subject to significant other intransitive - reacting - second surface)
## Affiliation
ThesisData$ssoiaffp <- (0*ThesisData$ssoi1b) + (4.5*ThesisData$ssoi2b) + (7.8*ThesisData$ssoi3b) + (4.5*ThesisData$ssoi4b) + (0*ThesisData$ssoi5b) +
  (-4.5*ThesisData$ssoi6b) + (-7.8*ThesisData$ssoi7b) + (-4.5*ThesisData$ssoi8b)


## Autonomy
ThesisData$ssoiautp <- (7.8*ThesisData$ssoi1b) + (4.5*ThesisData$ssoi2b) + (0*ThesisData$ssoi3b) + (-4.5*ThesisData$ssoi4b) + (-7.8*ThesisData$ssoi5b) +
  (-4.5*ThesisData$ssoi6b) + (0*ThesisData$ssoi7b) + (4.5*ThesisData$ssoi8b)



###### 10. SSOI w (subject to significant other intransitive - reacting - second surface)
## Affiliation
ThesisData$ssoiaffw <- (0*ThesisData$ssoi1w) + (4.5*ThesisData$ssoi2w) + (7.8*ThesisData$ssoi3w) + (4.5*ThesisData$ssoi4w) + (0*ThesisData$ssoi5w) +
  (-4.5*ThesisData$ssoi6w) + (-7.8*ThesisData$ssoi7w) + (-4.5*ThesisData$ssoi8w)


## Autonomy
ThesisData$ssoiautw <- (7.8*ThesisData$ssoi1w) + (4.5*ThesisData$ssoi2w) + (0*ThesisData$ssoi3w) + (-4.5*ThesisData$ssoi4w) + (-7.8*ThesisData$ssoi5w) +  (-4.5*ThesisData$ssoi6w) + (0*ThesisData$ssoi7w) + (4.5*ThesisData$ssoi8w)


################################## Recoding WCCL  ##############################
### Total overall positive coping 
ThesisData$DSS <- ThesisData$wccl1+ ThesisData$wccl2 + ThesisData$wccl4 + ThesisData$wccl6 + ThesisData$wccl9 + ThesisData$wccl10+ ThesisData$wccl11 + ThesisData$wccl13   + ThesisData$wccl16  + ThesisData$wccl18  + ThesisData$wccl19  + ThesisData$wccl21 + ThesisData$wccl22  + ThesisData$wccl23  + ThesisData$wccl26  + ThesisData$wccl27  + ThesisData$wccl29  + ThesisData$wccl31  + ThesisData$wccl33 + ThesisData$wccl34  + ThesisData$wccl35+ ThesisData$wccl36  + ThesisData$wccl38 + ThesisData$wccl39  + ThesisData$wccl40  + ThesisData$wccl42  + ThesisData$wccl43  + ThesisData$wccl44  + ThesisData$wccl47  + ThesisData$wccl49  + ThesisData$wccl50  + ThesisData$wccl51  + ThesisData$wccl53  + ThesisData$wccl54  + ThesisData$wccl56  + ThesisData$wccl57  + ThesisData$wccl58+ ThesisData$wccl59


### Total overall dysfunction coping subscake
ThesisData$DCS1 <- ThesisData$wccl3 + ThesisData$wccl5+ ThesisData$wccl8 + ThesisData$wccl12 + ThesisData$wccl14 + ThesisData$wccl17 + ThesisData$wccl20 + ThesisData$wccl25 + ThesisData$wccl32 + ThesisData$wccl37+ ThesisData$wccl41+ ThesisData$wccl45 + ThesisData$wccl46 + ThesisData$wccl52 + ThesisData$wccl55


### Total overall dysfunction coping 
ThesisData$DCS2 <- ThesisData$wccl7 + ThesisData$wccl15 + ThesisData$wccl24 + ThesisData$wccl28 + ThesisData$wccl30 + ThesisData$wccl48
ThesisData$DCS3 <-c(ThesisData$DCS1 +ThesisData$DCS2)


#################### EED Part 1 ####################:
##Item 5, 6, 7,  8 (edds5, eeds6, edds7, edds8)


# Change 1 and 2 to male and female for DERS dataframe
ThesisData$genderfactor <- factor(ThesisData$gender, levels = c(1:2), labels = c("Male", "Female"))

## I dont know how to remove the first row cz it has the questions
## when i do remove it from excel it messs the data very much, it combains all to one factor. 

## Stice, E., Fisher, M. & Martinez, E. (2004) provided the codes but it is in SPSS

### Z score z scores of each item before averaging the items according to Stice, E., Fisher, M. & Martinez, E.

## 1. To do this, one must first save out the standardized scores for the EDDS items (zedds1–zedds21).






## make a new data frame for gender + age+  DERS + SASB + EED(4-8) + WCCL 
#str(ThesisData$age)

abdul.df <- ThesisData[, c("age", "gender", "genderfactor", "ethnicity", "education", "marital", "employment", "income", "impulse", "aware", "strategy", "clarity", "nonaccept", "goals", "derstot", "intaffp", "intautp", "intaffw", "intautw", "sostaffp", "sostautp", "sostaffw", "sostautw", "sosiaffp", "sosiautp", "sosiaffw", "sosiautw", "ssotaffp", "ssotautp", "ssotaffw", "ssotautw", "ssoiaffp", "ssoiautp", "ssoiaffw", "ssoiautw", "DSS", "DCS1", "DCS2", "DCS3", "edds1", "edds2", "edds3", "edds4", "edds5", "edds6", "edds7", "edds8","edds9", "edds10" , "edds11" , "edds12" , "edds13" ,  "edds14",  "edds15", "edds16" , "edds17" , "edds18" , "edds19" , "edds20" , "edds21" , "edds22" , "edds23")]


