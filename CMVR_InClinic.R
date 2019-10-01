library(readr)
library(tidyr)
library(dplyr)
library(skimr)
library(rlang)
library(lubridate)
 
##Import 

inclinic_data <- read_csv("In-Clinic Screening Photographer Worksheet 4.4.16-JK2_10.1.csv")
View(inclinic_data)

cd4_data <- read_csv("CD4consolidated_updated_10.1.csv")
View(cd4_data)

##Review 
skim(inclinic_data)
skim(cd4_data)

##########################
#Clean data at Nakornping#
##########################

## The below is no longer relavent only kept as a record for housekeeping

## Below Corrected - After data cleaning from Nakornping, this looks much better. No missing data except for two fields that seem potentially imporatant:
# cd4countdate      50 
# patientrefertochoeng      24

## Some of these cd4 dates were FU1 and FU2 visits, 
## I found these cd4 dates for all patients but 1 for first visits: 5568056 - needs to be excluded
## Found and corrected missing data for patientrefertochoeng 


#filter(inclinic_data, is.na(cd4count))

#5910819
#5840459
# Both of these patients had missing cd4 counts. These were retreived and entered

#filter(inclinic_data, is.na(dilation))

#5736865
#5551837
# Both of these patients never received photo or exam. Documented in the paper chart that they did not
# have time for photo and would get at a later time then were lost to follow up
# Both patients should be excluded. Deleted from inclinic worksheet 

#filter(cd4_data, is.na(Sex))

#5674322
#5706200
# Both of these patients had missing sex. These were retreived and entered

##Reorganize and combine 

inclinic_data2 <- inclinic_data %>%
  rename(HN = hospitalnumber,
         patientrefertochoeng = patientrefertochoeng) %>%
  mutate(photodate = mdy(dateofphotography)) %>%
  select(-patientname) %>%
  select(HN, photodate, patientrefertochoeng, photonumber:cd4countdate) %>%
  filter(visit == "First") %>%
  arrange(photodate)

cd4_data2  <- cd4_data %>%
  # JK: cleaner to me to just use the command once, and then commas...
  rename(include = "Include?",
         hivclinic = "HIV clinic?",
         dr.p = "Dr. P",
         dr.n = "Dr. N",
         age = "Age",
         sex = "Sex",
         cd4_cell_ws = "CD4 cell") %>%
  # JK: you don't need to do the data$variable thing when you've specified your data up-front and are then in a pipe
  mutate(cd4_ws_date = mdy(D)) %>%
  select(HN, cd4_ws_date, age, sex, D:include) %>%
  arrange(cd4_ws_date)

skim(cd4_data2)
skim(inclinic_data2)

###################################
# More data cleaning at Nakornping#
###################################

# No need to remove patients that should not be included, from
# I deleted them directly from the csv per your instruction, 
# The below is only kept as a record for who was removed from inclinic
# for housekeeping
# 
# 
# inclinic_data3 <- inclinic_data2 %>%
#   filter(HN != 5658168 &
#            HN != 5704433 &
#            HN != 5604947 &
#            HN != 5636895 &
#            HN != 4843446 &
#            HN != 5737820 &
#            HN != 5768271 &
#            HN != 5740689 &
#            HN != 5774166 &
#            HN != 5417027 &
#            HN != 5803679 &
#            HN != 5806744 &
#            HN != 5808445 &
#            HN != 5803515 &
#            HN != 5815335 &
#            HN != 5815996 &
#            HN != 5819144 &
#            HN != 5826676 &
#            HN != 5828233 &
#            HN != 5829827 &
#            HN != 5829884 &
#            HN != 5844196 &
#            HN != 5856139 &
#            HN != 5821670 &
#            HN != 5846888 &
#            HN != 5383354 &
#            HN != 5861941 &
#            HN != 5878326 &
#            HN != 5868329 &
#            HN != 5824039 &
#            HN != 5862344 &
#            HN != 5912754 &
#            HN != 5915117)

# All of is only for housekeeping and really can be removed

# tsmissing1 <- inclinic_data %>%
#   filter(is.na(cd4count))
# 
# tsmissing2 <- inclinic_data %>%
#   filter(is.na(dilation))
# 
# tsmissing3 <- cd4_data %>%
#   filter(is.na(sex))
# 
# xtabs(data=combineddata, ~ (!is.na(photonumber)) + (!is.na(age)), addNA=TRUE)
# 
# tsmissing4 <- combineddata %>%
#   filter(is.na(age))
# 
# tsmissing5 <- combineddata %>%
#   filter(is.na(D))
# 
# View(tsmissing5)

## 5605483 had a follow up visit incorrectly labeled as First, I corrected this to FU1. 

#A lot of these photo same day as cd4, ok changed code to >= instead of > 


## All of the below were corrected 
# nakornpingcheck <- combineddata2 %>%
#   filter(HN == 5605483)
# # duplicate one should be FU, updated 
# nakornpingcheck <- combineddata2 %>%
#   filter(HN == 5655271)
# incorrect cd4 date, updated to 4/30/15
# nakornpingcheck <- combineddata2 %>%
#   filter(HN == 5640994)
# # duplicate one should be FU, also date entered incorrectly by Ying, cd4 was on 3/12/14
# nakornpingcheck <- combineddata2 %>%
#   filter(HN == 5568056)
# incorrect cd4 date, updated to 4/29/14
# nakornpingcheck <- combineddata2 %>%
#   filter(HN == 5736562)
# incorrect cd4 date, updated to 9/2/14
# nakornpingcheck <- combineddata2 %>%
#   filter(HN == 5748792)
# nakornpingcheck <- combineddata2 %>%
#   filter(HN == 5465272)
# nakornpingcheck <- combineddata2 %>%
#   filter(HN == 5718327)
# #photo same day as cd4
# nakornpingcheck <- combineddata2 %>%
#   filter(HN == 5803734)


# 1	5736562	NA
# 2	5655271	NA
# 3	5568056	NA
# 
# ts2 <- combineddata3 %>%
#   filter(HN.x == 5736562 | HN.x == 5655271 | HN.x == 5568056)
# View (ts2)

## Above had cd4 incorrectly after photo date. All looked like typos.  Went back and corrected from Nakronping review

# Note we want TRUE/TRUE (not missing in either file); but in this case we know we wouldn't have full data for the photos, 
# So what we really want is none false for age, which would mean the person is not listed in the CD4 file
# I did a little digging and we knew about this issue and Pbing, the research assistant at Nakornping, looked up these people:
# This file has the most updated data: "CMVR screening--not on CD4 list (1)_up19May16.xlsx"
# Blake the easiest thing will probably be to make a new CD4 consolidated file, with a new name (maybe add the date?) where you add the info from this file.

## Corrected and now the imported csv. 

inclinic_data3 <- inclinic_data2 %>%
  group_by(HN) %>%
  mutate(dup=n()) %>%
  group_by(HN, visit) %>%
  mutate(dup2=n())

xtabs(data=inclinic_data3, ~dup+dup2, addNA=TRUE)

ts_inclinic_data3 <- inclinic_data3 %>% 
  filter(dup>1)

## Now corrected 

skim(cd4_data2)
skim(inclinic_data2)

# Yay, data cleaning seems to be completed 

combineddata <- full_join(inclinic_data2, cd4_data2, by = "HN") %>%
  arrange(cd4_ws_date)

skim(combineddata)

#cd4_data2 has 1027 obs and combined has 1027

#View(combineddata)

## Ok this is now 0, I think data cleaning is potentially completed now. Your thoughts?

###? BMS - Jeremy, I think that we finally have a cleaned dataset. Let me know your thoughts on the above, 
# if so we can move forward with analysis.

## Define a positive case for each dataset (getting exam by Choeng in non-photo, and photo in photo)
## Are the above definitions correct?

# JK: this is an OK idea though I would make a variable for whether the patient was screened, not whether they had a photo.
# Knowing that all patients screened before the photo period were with a Choeng exam, and all patients screened during the photo period were with photos.

#? should we use an innerjoin instead since one file has x4 the cases. I thought not. Let me know if I am wrong
#combineddata <- inner_join(cd4_data, inclinic_data, by = "HN")

## create two datasets for nonphoto period and photo period 

beforephotoperiod <-combineddata %>%
  arrange(cd4_ws_date) %>%
  filter(cd4_ws_date < as.Date("2014-03-28"))

afterphotoperiod <-combineddata %>%
  arrange(cd4_ws_date) %>%
  filter(cd4_ws_date > as.Date("2014-03-28"))

#View(beforephotoperiod)
#View(afterphotoperiod)

## Define a positive case for each dataset (getting exam by Choeng in non-photo, and photo in photo)

##----- BMS stop here 

afterphotoperiod  <- afterphotoperiod %>%
  mutate(phototaken = case_when(!is.na(photonumber) ~ 1,
                                 is.na(photonumber) ~ 0))

#? I am unsure if the below defintion is correct, because I don't know the data well, 
# but these patients all have a Y dor dr. p or Dr. N, so I am assuming this means exam

beforephotoperiod  <- beforephotoperiod %>%
  mutate(examperformed = case_when(include == "Y" ~ 1,
                                   include == "N" ~ 0))

## Returning to data:
# my confusion is that it seems like "patientrefertochoeng" would be the right way to do this,
# however this variable is missing for most cases in the before, 
# maybe this is the point and I will define separately as refertochoeng

beforephotoperiod  <- beforephotoperiod %>%
  mutate(refertochoeng = case_when(patientrefertochoeng == "Yes" ~ 1,
                                   TRUE ~ 0))

##create column if CD4 is <100

afterphotoperiod  <- afterphotoperiod %>%
  mutate(cd4less100 = case_when(cd4_cell_ws <100 ~ 1,
                                cd4_cell_ws >100 ~ 0))

beforephotoperiod  <- beforephotoperiod %>%
  mutate(cd4less100 = case_when(cd4_cell_ws <100 ~ 1,
                                cd4_cell_ws >100 ~ 0))

afterphotoperiod  <- afterphotoperiod %>%
  mutate(cd4less100.2 = case_when(cd4count <100 ~ 1,
                                cd4count >100 ~ 0))

beforephotoperiod  <- beforephotoperiod %>%
  mutate(cd4less100.2 = case_when(cd4count <100 ~ 1,
                                cd4count >100 ~ 0))

## Tally numbers in each group and divide by total 

count(beforephotoperiod, examperformed)
#348/486
count(afterphotoperiod, phototaken)
#228/595
count(beforephotoperiod, cd4less100)
#10/486
count(afterphotoperiod, cd4less100)
#21/595
count(beforephotoperiod, cd4less100.2)
#2/486 with 433 NA
count(afterphotoperiod, cd4less100.2)
#6/595 with 371 NA

### Hmm, to my understanding this is the opposite of our hypothesis, correct?
# I feel that I may not have made the definitions. Are these correct? 
# I may stop here to clarify before moving forward...
# Also the amount of participants with a cd4 count <100 is way smaller than what is listed in the .do file
### Returning to data:
count(beforephotoperiod, refertochoeng)
#8/486

## Ok, perhaps this is the real number ~2% before camera to ~33% after camera, this seems right now, correct?
##Let's look at % with cd4 <100

beforecd4less100 <- beforephotoperiod %>%
  filter(cd4less100 ==1)

count(beforecd4less100, refertochoeng)
# 0/10

aftercd4less100 <- afterphotoperiod %>%
  filter(cd4less100 ==1)

count(aftercd4less100, phototaken)
# 7/21

#From 0 to 33% in this group as well. 

## Let me know any forward steps
