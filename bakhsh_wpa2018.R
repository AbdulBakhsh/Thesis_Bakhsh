---
  title: "R Code WPA Presentation"
author: "Abdulilah BAkhsh"
date: "3/14/2018"
output: html_document
---
  
  
  ## Preliminary Setup
  #{r setup, include=FALSE}
  # This code will remove all warnings and messages from your Knitted RMD
  # Remember, everything inside a chunk will be read by R unless you use # to block like I've done here. 
  # The # outside of a chunk is a header. The numberof #s determines the size of the header
  knitr::opts_chunk$set(echo = TRUE, warning = FALSE, message = FALSE)

## 1. Open libraries necessary for the data analysis
library(QuantPsyc)
library(car)
library(pastecs)
library(ggplot2)
library(multcomp)
library(compute.es)
library(psych)
library(Hmisc)
library(reshape)


### 2. Get the data

ThesisData <-read.csv ("ThesisData.csv", header = TRUE)
ThesisData #testing the data



## 3. Data Wrangling (Renaming and recoding the scales)


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

############ Recoding DERS
#making the DERS scales and recoding
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

##### Recoding SASB
### 1. Introject Pre (introject)
## Affiliation

ThesisData$intaffp <- (0*ThesisData$intcl1b) + (4.5*ThesisData$intcl2b) + (7.8*ThesisData$intcl3b) + (4.5*ThesisData$intcl4b) + (0*ThesisData$intcl5b) + (-4.5*ThesisData$intcl6b) + (-7.8*ThesisData$intcl7b) + (-4.5*ThesisData$intcl8b)


## Autonomy
ThesisData$intautp <- (7.8*ThesisData$intcl1b) + (4.5*ThesisData$intcl2b) + (0*ThesisData$intcl3b) + (-4.5*ThesisData$intcl4b) + (-7.8*ThesisData$intcl5b) + (-4.5*ThesisData$intcl6b) + (0*ThesisData$intcl7b) + (4.5*ThesisData$intcl8b)


### 2. Introject w  (introject)
## Affiliation
ThesisData$intaffw <- (0*ThesisData$intcl1w) + (4.5*ThesisData$intcl2w) + (7.8*ThesisData$intcl3w) + (4.5*ThesisData$intcl4w) + (0*ThesisData$intcl5w) + (-4.5*ThesisData$intcl6w) + (-7.8*ThesisData$intcl7w) + (-4.5*ThesisData$intcl8w)


## Autonomy
ThesisData$intautw <- (7.8*ThesisData$intcl1w) + (4.5*ThesisData$intcl2w) + (0*ThesisData$intcl3w) + (-4.5*ThesisData$intcl4w) + (-7.8*ThesisData$intcl5w) + (-4.5*ThesisData$intcl6w) + (0*ThesisData$intcl7w) + (4.5*ThesisData$intcl8w)

##### SOST and SOSI
### 3. SOST b (signficant to subject transitive - acting - top surface)
## Affiliation
ThesisData$sostaffp <- (0*ThesisData$sost1b) + (4.5*ThesisData$sost2b) + (7.8*ThesisData$sost3b) + (4.5*ThesisData$sost4b) + (0*ThesisData$sost5b) +
  (-4.5*ThesisData$sost6b) + (-7.8*ThesisData$sost7b) + (-4.5*ThesisData$sost8b)


## Autonomy
ThesisData$sostautp <- (7.8*ThesisData$sost1b) + (4.5*ThesisData$sost2b) + (0*ThesisData$sost3b) + (-4.5*ThesisData$sost4b) + (-7.8*ThesisData$sost5b) +
  (-4.5*ThesisData$sost6b) + (0*ThesisData$sost7b) + (4.5*ThesisData$sost8b)

### 4. SOST w (signficant to subject transitive - acting - top surface)
## Affiliation
ThesisData$sostaffw <- (0*ThesisData$sost1w) + (4.5*ThesisData$sost2w) + (7.8*ThesisData$sost3w) + (4.5*ThesisData$sost4w) + (0*ThesisData$sost5w) +
  (-4.5*ThesisData$sost6w) + (-7.8*ThesisData$sost7w) + (-4.5*ThesisData$sost8w)

## Autonomy
ThesisData$sostautw <- (7.8*ThesisData$sost1w) + (4.5*ThesisData$sost2w) + (0*ThesisData$sost3w) + (-4.5*ThesisData$sost4w) + (-7.8*ThesisData$sost5w) +(-4.5*ThesisData$sost6w) + (0*ThesisData$sost7w) + (4.5*ThesisData$sost8w)

### 5. SOSI b (significant other to subject intransitive - reacting - second surface)
## Affiliation
ThesisData$sosiaffp <- (0*ThesisData$sosi1b) + (4.5*ThesisData$sosi2b) + (7.8*ThesisData$sosi3b) + (4.5*ThesisData$sosi4b) + (0*ThesisData$sosi5b) +
  (-4.5*ThesisData$sosi6b) + (-7.8*ThesisData$sosi7b) + (-4.5*ThesisData$sosi8b)


## Autonomy
ThesisData$sosiautp <- (7.8*ThesisData$sosi1b) + (4.5*ThesisData$sosi2b) + (0*ThesisData$sosi3b) + (-4.5*ThesisData$sosi4b) + (-7.8*ThesisData$sosi5b) + (-4.5*ThesisData$sosi6b) + (0*ThesisData$sosi7b) + (4.5*ThesisData$sosi8b)

### 6. SOSI w (significant other to subject intransitive - reacting - second surface)
## Affiliation
ThesisData$sosiaffw <- (0*ThesisData$sosi1w) + (4.5*ThesisData$sosi2w) + (7.8*ThesisData$sosi3w) + (4.5*ThesisData$sosi4w) + (0*ThesisData$sosi5w) +
  (-4.5*ThesisData$sosi6w) + (-7.8*ThesisData$sosi7w) + (-4.5*ThesisData$sosi8w)

## Autonomy
ThesisData$sosiautw <- (7.8*ThesisData$sosi1w) + (4.5*ThesisData$sosi2w) + (0*ThesisData$sosi3w) + (-4.5*ThesisData$sosi4w) + (-7.8*ThesisData$sosi5w) +
  (-4.5*ThesisData$sosi6w) + (0*ThesisData$sosi7w) + (4.5*ThesisData$sosi8w)

##### SSOT and SSOI
### SSOT b (subject to significant other transitive - acting - top surface)
## Affiliation
ThesisData$ssotaffp <- (0*ThesisData$ssot1b) + (4.5*ThesisData$ssot2b) + (7.8*ThesisData$ssot3b) + (4.5*ThesisData$ssot4b) + (0*ThesisData$ssot5b) +
  (-4.5*ThesisData$ssot6b) + (-7.8*ThesisData$ssot7b) + (-4.5*ThesisData$ssot8b)


## Autonomy
ThesisData$ssotautp <- (7.8*ThesisData$ssot1b) + (4.5*ThesisData$ssot2b) + (0*ThesisData$ssot3b) + (-4.5*ThesisData$ssot4b) + (-7.8*ThesisData$ssot5b) +
  (-4.5*ThesisData$ssot6b) + (0*ThesisData$ssot7b) + (4.5*ThesisData$ssot8b)


### 8. SSOT w (subject to significant other transitive - acting - top surface)
## Affiliation
ThesisData$ssotaffw <- (0*ThesisData$ssot1w) + (4.5*ThesisData$ssot2w) + (7.8*ThesisData$ssot3w) + (4.5*ThesisData$ssot4w) + (0*ThesisData$ssot5w) +
  (-4.5*ThesisData$ssot6w) + (-7.8*ThesisData$ssot7w) + (-4.5*ThesisData$ssot8w)


## Autonomy
ThesisData$ssotautw <- (7.8*ThesisData$ssot1w) + (4.5*ThesisData$ssot2w) + (0*ThesisData$ssot3w) + (-4.5*ThesisData$ssot4w) + (-7.8*ThesisData$ssot5w) +
  (-4.5*ThesisData$ssot6w) + (0*ThesisData$ssot7w) + (4.5*ThesisData$ssot8w)


### 9. SSOI b (subject to significant other intransitive - reacting - second surface)
## Affiliation
ThesisData$ssoiaffp <- (0*ThesisData$ssoi1b) + (4.5*ThesisData$ssoi2b) + (7.8*ThesisData$ssoi3b) + (4.5*ThesisData$ssoi4b) + (0*ThesisData$ssoi5b) +
  (-4.5*ThesisData$ssoi6b) + (-7.8*ThesisData$ssoi7b) + (-4.5*ThesisData$ssoi8b)


## Autonomy
ThesisData$ssoiautp <- (7.8*ThesisData$ssoi1b) + (4.5*ThesisData$ssoi2b) + (0*ThesisData$ssoi3b) + (-4.5*ThesisData$ssoi4b) + (-7.8*ThesisData$ssoi5b) +
  (-4.5*ThesisData$ssoi6b) + (0*ThesisData$ssoi7b) + (4.5*ThesisData$ssoi8b)



### 10. SSOI w (subject to significant other intransitive - reacting - second surface)
## Affiliation
ThesisData$ssoiaffw <- (0*ThesisData$ssoi1w) + (4.5*ThesisData$ssoi2w) + (7.8*ThesisData$ssoi3w) + (4.5*ThesisData$ssoi4w) + (0*ThesisData$ssoi5w) +
  (-4.5*ThesisData$ssoi6w) + (-7.8*ThesisData$ssoi7w) + (-4.5*ThesisData$ssoi8w)


## Autonomy
ThesisData$ssoiautw <- (7.8*ThesisData$ssoi1w) + (4.5*ThesisData$ssoi2w) + (0*ThesisData$ssoi3w) + (-4.5*ThesisData$ssoi4w) + (-7.8*ThesisData$ssoi5w) +  (-4.5*ThesisData$ssoi6w) + (0*ThesisData$ssoi7w) + (4.5*ThesisData$ssoi8w)


### Recoding WCCL
## Total overall positive coping 
ThesisData$DSS <- ThesisData$wccl1+ ThesisData$wccl2 + ThesisData$wccl4 + ThesisData$wccl6 + ThesisData$wccl9 + ThesisData$wccl10+ ThesisData$wccl11 + ThesisData$wccl13   + ThesisData$wccl16  + ThesisData$wccl18  + ThesisData$wccl19  + ThesisData$wccl21 + ThesisData$wccl22  + ThesisData$wccl23  + ThesisData$wccl26  + ThesisData$wccl27  + ThesisData$wccl29  + ThesisData$wccl31  + ThesisData$wccl33 + ThesisData$wccl34  + ThesisData$wccl35+ ThesisData$wccl36  + ThesisData$wccl38 + ThesisData$wccl39  + ThesisData$wccl40  + ThesisData$wccl42  + ThesisData$wccl43  + ThesisData$wccl44  + ThesisData$wccl47  + ThesisData$wccl49  + ThesisData$wccl50  + ThesisData$wccl51  + ThesisData$wccl53  + ThesisData$wccl54  + ThesisData$wccl56  + ThesisData$wccl57  + ThesisData$wccl58+ ThesisData$wccl59


## Total overall dysfunction coping subscale
ThesisData$DCS1 <- ThesisData$wccl3 + ThesisData$wccl5+ ThesisData$wccl8 + ThesisData$wccl12 + ThesisData$wccl14 + ThesisData$wccl17 + ThesisData$wccl20 + ThesisData$wccl25 + ThesisData$wccl32 + ThesisData$wccl37+ ThesisData$wccl41+ ThesisData$wccl45 + ThesisData$wccl46 + ThesisData$wccl52 + ThesisData$wccl55


## Total overall dysfunction coping  (blaming others factors)
ThesisData$DCS2 <- ThesisData$wccl7 + ThesisData$wccl15 + ThesisData$wccl24 + ThesisData$wccl28 + ThesisData$wccl30 + ThesisData$wccl48
ThesisData$DCS3 <-c(ThesisData$DCS1 +ThesisData$DCS2)


############ EED
##Item 5, 6, 7,  8 (edds5, eeds6, edds7, edds8)

# Change 1 and 2 to male and female for DERS dataframe
ThesisData$genderfactor <- factor(ThesisData$gender, levels = c(1:2), labels = c("Male", "Female"))

## Stice, E., Fisher, M. & Martinez, E. (2004) provided the codes but it is in SPSS

### Z score z scores of each item before averaging the items according to Stice, E., Fisher, M. & Martinez, E.

## 1. To do this, one must first save out the standardized scores for the EDDS items (zedds1–zedds21).


## make a new data frame for gender + age+  DERS + SASB + EED(4-8) + WCCL 
#str(ThesisData$age)

abdul.df <- ThesisData[, c("age", "gender", "genderfactor", "ethnicity", "education", "marital", "employment", "income", "impulse", "aware", "strategy", "clarity", "nonaccept", "goals", "derstot", "intaffp", "intautp", "intaffw", "intautw", "sostaffp", "sostautp", "sostaffw", "sostautw", "sosiaffp", "sosiautp", "sosiaffw", "sosiautw", "ssotaffp", "ssotautp", "ssotaffw", "ssotautw", "ssoiaffp", "ssoiautp", "ssoiaffw", "ssoiautw", "DSS", "DCS1", "DCS2", "DCS3", "edds1", "edds2", "edds3", "edds4", "edds5", "edds6", "edds7", "edds8","edds9", "edds10" , "edds11" , "edds12" , "edds13" ,  "edds14",  "edds15", "edds16" , "edds17" , "edds18" , "edds19" , "edds20" , "edds21" , "edds22" , "edds23")]

abdul.df <- abdul.df[-c(93,94),]

# . Need to average 7 and 8 (number of binge)
abdul.df$edds78 <- (abdul.df$edds7 + abdul.df$edds8)/2

# Need to select only people with EDDS 5 and 6
abdul.df$edds5f <- as.factor(abdul.df$edds5) # make it a factor
abdul.df$edds6f <- as.factor(abdul.df$edds6) # make it a factor

# make a new variable combining the results from the two variables
abdul.df$edds56 <- ifelse((abdul.df$edds5 == 2) & (abdul.df$edds6 == 2), 0, +ifelse((abdul.df$edds5 ==1) & (abdul.df$edds6 == 1), 1, 99))

# label the new factor
abdul.df$edds56f <- factor(abdul.df$edds56, levels = c(0:1), labels = c("No-BED", "BED"))

bed.df <- subset(abdul.df, edds56f == "BED")
nobed.df <- subset(abdul.df, edds56f == "No-BED")



############ 4. Data Modeling: Hypothesis Testing

#### List of Study Hypotheses
1. #hypothesis 1: It is expected that a lack of emotional clarity and absence of strategies for managing emotions will be significant predictor of binge eating behavior.

2. #hypothesis 2: Gender, food restriction, weight-shape over-evaluation, and overall difficulties with emotion regulation are expected to be significant predictors of binge-eating behavior.

3. #hypothesis 3: A more hostile self-concept, more hostile levels of affiliation in a close relationship, and higher autonomy in a close relationship will be associated with increased binge-eating behavior regardless of overall positive or negative coping and overall emotion regulation.




############ 5. Exploratory Data Analysis and Descriptive Statistics

######### means and SD for data

## Demographics

## changing to factors
# Change 1 and 2 to male and female for the general dataframe

bed.df$genderfactor <- factor(bed.df$gender, levels = c(1:2), labels = c("Male", "Female"))

## changing 1-7 to education factors
bed.df$educationfactor <- factor(bed.df$education, levels = c(1:7), labels = c("No schooling completed,", "High school graduate or GED", "Associate degree", "Bachelor’s degree", "Master’s degree", "Professional degree", "Doctorate degree"))
bed.dff$educationfactor

## changing 1,2,3, 4 to marital factor

bed.df$maritalfactor <- factor(bed.df$marital, levels = c(1:5), labels = c("Single,", "Married", "Widowed", "Divorced", "Separated"))
abdul.df$maritalfactor

## changing from 1-9 to employment factor
bed.df$employmentfactor <- factor(bed.df$employment, levels = c(1:9), labels = c("Employed for wages,", "Self- employed", "No work and looking for work", "No work and not looking for work", "homemaker", "student", "Military", "Retired", "Unable to work" ))
bed.df$employmentfactor
# check

## chaanging from to income factor
bed.df$incomefactor <- factor(bed.df$income, levels = c(1:4), labels = c("Less than $24,999", "$25,000 to $49,999", "$50,000 to $99,999", "$100,000 or more"))
bed.df$incomefactor

## chaanging from edds7 to number of binge in last 6 months factor
bed.df$edds7factor <- factor(bed.df$edds7, levels = c(1:8), labels = c("1", "2", "3", "4", "5", "6", "7", "8"))

## changing from edds8 to number of binge in last 3 months factor
bed.df$edds8factor <- factor(bed.df$edds8, levels = c(1:14), labels = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14"))

###### Demographics means and SD
# number of participants 
nrow(bed.df) ## there are 93 participants

# Gender:
summary(bed.df$genderfactor) # 25 male, 30 female

# Age:
summary(bed.df)
sd(bed.df$age) #Age: mean age is 39.71, Max age is 73.00, min age is 21, sb= 12.1

## Ethnicity:
summary(bed.df$ethnicity) ## Ethnicity: 30 white, 18 asian, 3 African American, 1 latin, 3 non-specified

## Marital status
summary(bed.df$maritalfactor) ## Marital status: 12 single, 35 married or in a relationship, 2 widowed, 5 divorced, 1 separated

## Education
summary(bed.df$educationfactor) ## Education: 0 no schooling, 14 high school or GED, 6 associate degree, 27 bachelors, 8 Master's, 0 Professional degree or doctorate

## Employment
summary(bed.df$employmentfactor) ## Employment: 35 employed for wages, 12 self-employed, 0 not working, 5 homemaker, 2 students, 0 Military, 0 Retired, 1 Unable to work 

## Income
summary(bed.df$incomefactor) ##Income: 13 individuals less than Less than $24,999, 18 individuals $25,000 to $49,999, 20 individuals $50,000 to $99,999, 4 individuals $100,000 or more


## Number of binge in last 6 months factor
summary(bed.df$edds7factor) # 14 at least 1 time in the last 6 months, 13 at least 2 times, 11 at least 3 times, 6 at least 4 times, 6 at least 5 times, 3 at least 6 times, 2 at least 7 times, 0 at least 8 times

## Number of binge in last 3 months factor
summary(bed.df$edds8factor) #15 at least 1 time in the last 3 months, 11 at least 2, 7 at least 3, 5 at least 4, 1 at least 5, 3 at least 6, 2 at least 7, 3 at least 8, 1 at least 9, 2 at least 10, 1 at least 11, 2 at least 12, 2 at least 13, 0 at least 14


###### DERS means and SD
## Impulse
summary(bed.df$impulse)
sd(bed.df$impulse)
## mean= 15.00 Sd= 5.30, 

## aware
summary(bed.df$aware)
sd(bed.df$aware) ## mean= 22.2600 Sd= 4.20

## strategy
summary(bed.df$strategy)
sd(bed.df$strategy) ## Mean= 21.00, sd= 6.88

## clarity
summary(bed.df$clarity)
sd(bed.df$clarity) ## Mean= 13.00, sd= 3.02

## nonaccept
summary(bed.df$nonaccept)
sd(bed.df$nonaccept) ## mean= 15.00, sd= 6.03

## goals
summary(bed.df$goals)
sd(bed.df$goals)
## mean= 15.00, sd= 4.03

## derstot
summary(bed.df$derstot)
sd(bed.df$derstot) ## mean= 101.0, sd= 21.70 


###### eddsX means and SD

## average edds7 and 8 (number of binge)
summary(bed.df$edds78)
sd(bed.df$edds78) ## mean= 3.527, sd= 2.29

## weight-shape over-evaluation,
summary(bed.df$edds234)
sd(bed.df$edds234) ## mean= 15.13, sd= 4.76

## Food restriction
summary(bed.df$edds17)
sd(bed.df$edds17) ## mean= 4.70, sd= 6.89

###### Dialectical Behavioral Therapy Ways of Coping Checklist (wcclX) means and SD

#Total overall positive coping 
summary(bed.df$DSS)
sd(bed.df$DSS) ## mean = 98.13, sd= 20.97

##  DCS1 (general dysfunctional coping)
summary(bed.df$DCS1)
sd(bed.df$DCS1)## mean= 40.67, sd= 8.06

## DCS2 (Total overall dysfunction coping) blaming others factor
summary(bed.df$DCS2)
sd(bed.df$DCS2) ## mean= 13.33.67, sd= 3.99

## DCS3 (Total overall dysfunction coping)
summary(bed.df$DCS3)
sd(bed.df$DCS3) ## mean= 54.09.44, sd= 10.59


####### 6. hypothesis testing

#----------------- Hypothesis 1
#hypothesis 1: It is expected that a lack of emotional clarity and absence of strategies for managing emotions will be significant predictor of binge eating behavior.
## A linear regression will be conducted with DERS subscales as predictors of binge-eating behavior. 


# regression for everyone regardless of with both 5 and 6 only
hyp1.lm <- lm(edds78 ~ clarity + strategy, data= bed.df)
summary(hyp1.lm) # not significant
confint(hyp1.lm)
lm.beta(hyp1.lm)

## Multiple R-squared:  0.05501,	Adjusted R-squared:  0.01866 
## F-statistic: 1.513 on 2 and 52 DF,  p-value: 0.2297
## Interpretation: Model accounts for 0.1178 of the model.   
## not significant because F is 0.2297, which is not significant at p < 0.2147.
## Emotional clarity (B = .09, CI = -.14, .33) and strategy (B = 0.5, CI = -.05, .15)

# how about other DERS total - not hypothesized
hyp1a.lm <- lm(edds78 ~ derstot, data= abdul.df)
summary(hyp1a.lm) # significant but 56 don't matter
confint(hyp1a.lm)

## Results show that the overal DERS significantly predicted binge-eating, R2 = 0.08, F = (1, 80) = 7.43, p = 0.007.  DERS total  (B = 0.03, CI = 0.001, 0.05). 



#----------------- Hypothesis 2
#hypothesis 2: Gender, food restriction, weight-shape over-evaluation, and overall difficulties with emotion regulation are expected to be significant predictors of binge-eating behavior.
## edds17 is food restriction
## (edds234)= edds2, edds3, edds4 is weight and shape

## 1. Make Fear varaiable - 2,3,4 (sum up) (wieght-shape)

bed.df$edds234 <- bed.df$edds2 + bed.df$edds3 + bed.df$edds4

hyp2.lm <- lm(edds78 ~ genderfactor + edds17 + edds234 + derstot, data= bed.df)
summary(hyp2.lm) #edds17 (restricting) significant
confint(hyp2.lm)
lm.beta(hyp2.lm)

## Multiple R-squared:  0.1876,	Adjusted R-squared:  0.1226 
## F-statistic: 2.887 on 4 and 50 DF,  p-value: 0.03158
## Multiple R-squared has a valuse of 0.03158, Interpretation: Model accounts for 0.03158 of the model.



#----------------- Hypothesis 3
#hypothesis 3: A more hostile self-concept, more hostile levels of affiliation in a close relationship, and higher autonomy in a close relationship will be associated with increased binge-eating behavior regardless of overall positive or negative coping and overall emotion regulation.

## afftotal= total autonomy, auttot= overal autonomy, intafftot= self-concept affiliation, intauttot = self-concept autonomy, DCS3= total negative coping , DSS= total positive coping

## overal affiliation
# create overall affilaition in a close relationship
bed.df$afftotal <- bed.df$sostaffp + bed.df$sostaffw + bed.df$sosiaffp + bed.df$sosiaffw + bed.df$ssotaffp + bed.df$ssotaffw + bed.df$ssoiaffp + bed.df$ssoiaffw
## overal autonomy
bed.df$auttot <- bed.df$sostautp + bed.df$sostautw + bed.df$sosiautp + bed.df$sosiautw + bed.df$ssotautp + bed.df$ssotautw + bed.df$ssoiautp + bed.df$ssoiautw

# introject
bed.df$intafftot <- bed.df$intaffp + bed.df$intaffw # self concept (introject) affiliation
bed.df$intauttot <- bed.df$intautp + bed.df$intautw # self concept (introject) autonomy

## Close relationship + introject (self-concept)
hyp3.lm <- lm(edds78 ~  afftotal + auttot + DCS3 + DSS + derstot + intafftot + intauttot, data=bed.df)
summary(hyp3.lm) # Not significant
confint(hyp3.lm)
lm.beta(hyp3.lm)

## Results show that affiliation, autonomy, postive and negative coping, DERS, self-concept affiliation, self-concept autonomy did NOT significantly predict binge-eating R2= 0.16, F (7, 47) = 1.29, p = 0.28.


## Close relationship
hyp3a.lm <- lm(edds78 ~  afftotal + auttot + DCS3 +DSS +  derstot, data=bed.df)
summary(hyp3a.lm) # Not significant
confint(hyp3a.lm)
lm.beta(hyp3a.lm)

## Results show that affiliation, autonomy, postive and negative coping, and DERS did NOT significantly predict binge-eating R2= 0.13, F (5, 49) = 1.40, p = 0.24.


# introject (self-concept)
hyp3b.lm <- lm(edds78 ~  intafftot + intauttot + DCS3 + DSS +  derstot, data=bed.df) 
summary(hyp3b.lm) # Not significant
confint(hyp3b.lm)
lm.beta(hyp3b.lm)
## Results show that self-concept affiliation, self-concept autonomy, postive and negative coping, and DERS did NOT significantly predict binge-eating R2= 0.14, F (5, 49) = 1.58, p = 0.18.



############ 7.  Data Visualization 

# Graphs for hypothesis 1 --------------------------------------
abdulna.df <- na.omit(bed.df) # get rid of NA for graphing

# graph strategy
graph <- ggplot(data = abdulna.df, aes(y=edds78, x=strategy, colour=edds56f)) + labs(x="Strategy", y = "Binge eating average") + geom_point() + geom_smooth(method="lm", se=TRUE)
graph # no association between edds78 for strategy
## rename legend in ggplot 
#graph clarity
graph1 <- ggplot(data = abdulna.df, aes(y=edds78, x=clarity, colour=edds56f)) + labs(x="Clarity", y = "Binge eating average ") + geom_point() + geom_smooth(method="lm", se=TRUE)
graph1 # no association between edds78 for strategy

#total ders 
graph3 <- ggplot(data = abdulna.df, aes(y=edds78, x=derstot, colour=edds56f)) + labs(x="Total Difficulties in Emotion Regulation", y = "Binge eating average ") + geom_point() + geom_smooth(method="lm", se=TRUE)
graph3 # an association in right direction but edds56 is not inlcuded




# graphs for hypothesis 2
# 1. fear (edds234)
graph4 <- ggplot(data = bed.df, aes(y=edds78, x=edds234, colour=edds56f)) + labs(x="Weight-shape over-evaluation", y = "Binge eating average", title="Density Plot") + geom_point() + geom_smooth(method="lm", se=TRUE)
graph4 


# 2. restrict
graph5 <- ggplot(data = bed.df, aes(y=edds78, x=edds17, colour=edds56f)) +labs(x="Food restriction", y = "Binge eating average", title="Density Plot")+ geom_point() + geom_smooth(method="lm", se=TRUE)
graph5 


######## 8. Summary 
#### The first hypothesis attempted to replicate the findings of Whiteside and colleagues (2007).  It was predicted that an increase in lack of emotional clarity and absence of strategies for managing emotions would be associated with an increase in binge eating.  A linear regression was conducted with the DERS emotional clarity and absence of strategies for managing emotions subscales as predictors of binge-eating behavior.  Emotional clarity and absence of strategies did not significantly predict binge eating (R2 = 0.06, F = (2, 52) = 1.51, p = 0.23).  Emotional clarity (B = .09, CI = -0.14, 0.33) and strategy (B = 0.5, CI = -0.05, 0.15) were both positively, but not significantly, associated with binge eating.  

#### The second hypothesis also attempted to replicate the findings of Whiteside and colleagues (2007).  It was predicted that binge-eating behavior has a significant association with the variables of gender, food restriction, weight and shape over-evaluation, and overall difficulties with emotion regulation.  A hierarchical linear regression was used with level variables entered consistent with Whiteside’s et al.  (2007) model including: 1) gender, 2) gender + food restriction, 3) gender + food restriction + weight and shape over-evaluation 4) gender + food restriction + weight and shape over-evaluation + DERS total.  The results show that gender, food restriction, weight and shape over-evaluation, and overall difficulties with emotion regulation significantly predicted binge eating (R2 = 0.19, F (4, 50) = 2.89, p = 0.03), with gender (B = -0.25, CI = -1.49, 0.99) negatively but not significantly associated with binge eating.  Food restriction (B = 0.9, CI = 0.004, 0.19), weight and shape over-evaluation (B = 0.11, CI =-0.02, 0.22), and overall difficulties with emotion regulation (B = 0.010, CI = -0.02, 0.04) were positively associated with binge eating, with only food restriction significantly correlated.  


#### Finally, the third hypothesis predicted that low levels of self-concept affiliation, low levels of affiliation, and increased autonomy in close relationships would be associated with increased binge-eating behaviors, regardless of overall positive or negative coping and overall emotion regulation.  A linear regression was conducted with DERS and Intrex SASB affiliation and autonomy subscales as predictors of binge-eating behavior.  The results show that overall relationship affiliation, self-concept affiliation, overall relationship autonomy, positive and negative coping skills, and overall difficulties with emotion regulation did not significantly predict binge eating (R2 = 0.16, F (7, 47) = 1.29, p = 0.28), with relationship affiliation negatively but not significantly correlated with binge-eating behavior (B = -0.0008, CI = -0.0002, 0.001).  Relationship autonomy (B = 0.0003, CI = -0.004, 0.004), overall dysfunctional coping (B = 0.08, CI = -0.03, 0.11), overall positive coping (B = 0.02, CI = -0.01, 0.05), and overall difficulties with emotion regulation (B = 0.01, CI = -0.02, 0.04) were not significantly correlated with binge-eating behavior.  