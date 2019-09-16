library(readr)
library(tidyr)
library(dplyr)
library(skimr)
library(rlang)
library(lubridate)
 
##Import 

inclinic_data <- read_csv("In-Clinic Screening Photographer Worksheet 4.4.16-JK2.csv")
View(inclinic_data)

cd4_data <- read_csv("CD4consolidated.csv")
View(cd4_data)

##Review 
skim(inclinic_data)
skim(cd4_data)

##Reorganize and combine 

inclinic_data <- rename(inclinic_data, HN = hospitalnumber) %>%
  mutate(photodate = mdy(inclinic_data$dateofphotography)) %>%
  select(-patientname) %>%
  rename(patientrefertochoeng = patientrefertochoeng)
  select(HN, photodate, patientrefertochoeng, photonumber:cd4countdate) %>%
  arrange(photodate)

cd4_data <- rename(cd4_data, include = "Include?") %>%
  rename(hivclinic = "HIV clinic?") %>%
  rename(dr.p = "Dr. P") %>%
  rename(dr.n = "Dr. N") %>%
  rename(age = "Age") %>%
  rename(sex = "Sex") %>%
  rename(cd4_cell_ws = "CD4 cell") %>%
  mutate(cd4_ws_date = mdy(cd4_data$D)) %>%
  select(HN, cd4_ws_date, age, sex, D:include) %>%
  arrange(cd4_ws_date)

combineddata <- full_join(inclinic_data, cd4_data, by = "HN")

combineddata <- arrange(combineddata, cd4_ws_date)

#View(combineddata)

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
## Are the above definitions correct?

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