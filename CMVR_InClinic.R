library(readr)
library(tidyr)
library(dplyr)
library(skimr)
library(rlang)
library(lubridate)
 
##Import 

#inclinic_data <- read_csv("In-Clinic Screening Photographer Worksheet 4.4.16-JK2.csv")

##Import updated data

inclinic_data <- read_csv("In-Clinic Screening Photographer Worksheet 4.4.16-JK2_9.23_FIRSTVISITONLY.csv")
View(inclinic_data)

cd4_data <- read_csv("CD4consolidated.csv")
View(cd4_data)

##Review 
skim(inclinic_data)
skim(cd4_data)

##Reorganize and combine 
# JK: I tend not to replace but instead make a new object. But I am not sure if that is a best practice. 
# I guess if when you are troubleshooting you always run it from the very top then you're probably fine replacing
inclinic_data2 <- inclinic_data %>%
  rename(HN = hospitalnumber,
         patientrefertochoeng = patientrefertochoeng) %>%
  mutate(photodate = mdy(dateofphotography)) %>%
  select(-patientname) %>%
  select(HN, photodate, patientrefertochoeng, photonumber:cd4countdate) %>%
  arrange(photodate)

## Need to remove patients that should not be included form sheet that you sent me

# # No need to remove as I deleted them directly from the csv per your instruction, 
# # but I will keep in the file as a record with # for housekeeping purposes
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

combineddata <- full_join(inclinic_data2, cd4_data2, by = "HN") %>%
  arrange(cd4_ws_date)
# JK: note that the resulting file has more observations than either file alone, meaning some did not merge.
# Looking for variables unique to each file that are not missing
skim(inclinic_data2)
skim(cd4_data2)

tsmissing1 <- inclinic_data2 %>%
  filter(is.na(cd4count))

tsmissing2 <- inclinic_data2 %>%
  filter(is.na(dilation))

tsmissing3 <- cd4_data2 %>%
  filter(is.na(sex))

xtabs(data=combineddata, ~ (!is.na(photonumber)) + (!is.na(age)), addNA=TRUE)

## Only 1 case after updating 

tsmissing4 <- combineddata %>%
  filter(is.na(age))

# Will update 5748792	from paper chart 

# Note we want TRUE/TRUE (not missing in either file); but in this case we know we wouldn't have full data for the photos, 
# So what we really want is none false for age, which would mean the person is not listed in the CD4 file
# I did a little digging and we knew about this issue and Pbing, the research assistant at Nakornping, looked up these people:
# This file has the most updated data: "CMVR screening--not on CD4 list (1)_up19May16.xlsx"
# Blake the easiest thing will probably be to make a new CD4 consolidated file, with a new name (maybe add the date?) where you add the info from this file.

## Completed 

#View(combineddata)

#? should we use an innerjoin instead since one file has x4 the cases. I thought not. Let me know if I am wrong
# JK: No, you want all records from the CD4 consolidated, since that defines the population at risk. Only some of them will have photos.
# We will eventually ignore the photos that don't match up (most of them from different hospitals, a couple were TB patients)

# JK: We wouldn't want 2 separate dataframes. We want a single dataframe, with a variable for before/after.
combineddata2 <- combineddata %>%
  # JK: Ultimately you are only eligible for the study if you had a CD4 count and you were seen at Nakornping Hospital
  filter(!is.na(cd4_ws_date) & include=="Y") %>%
  mutate(photoperiod=if_else(cd4_ws_date < as.Date("2014-03-28"),0, 
                     if_else(cd4_ws_date >= as.Date("2014-03-28"),1, NA_real_)),
         screened=if_else(!is.na(photonumber),1,999))
xtabs(data=combineddata2, ~photoperiod+screened, addNA=TRUE)

# JK: Realizing that we need to merge in the data from the Redcap project "Cytomegalovirus Retinitis Incidence Data Collection Form - Nakornping Hospital v4"
# The table above highlights that some data cleaning needs to be done 
# because theoretically there should not be any people yet who were screened in the pre-photo period since we haven't merged in the data yet
# Just spot checking: 5614050 --> this one should have 2 photo period visits (look at photonumber and visit) and 1 pre-photo period visit. 
                          # But they actually have 6 records in the combineddata2
                          # This highlights that we made a mistake in not looking to see if the HN was unique before merging.
                          # What we should have done:
inclinic_data3 <- inclinic_data2 %>%
  group_by(HN) %>%
  mutate(dup=n()) %>%
  group_by(HN, visit) %>%
  mutate(dup2=n())

xtabs(data=inclinic_data3, ~dup+dup2, addNA=TRUE)
# So there are definitely duplicates. But generally HN, visit will identify unique observations.

tsduplicates <- inclinic_data2 %>%
 filter(dup2==2)

# Except for 5605483, but this one it looks like the visit field is wrong (one of them should be FU; I would either change on the original csv or in the R script when you import.)
# We should investigate the others...
ts_inclinic_data3 <- inclinic_data3 %>% 
  filter(dup>1)

##? No more duplicates except for 5605483, should I jyust change this one manually in the csv so the the Ying visit is excluded?

# Can you look through these a little? Note field visit, which says if it was first or F/U. I could see two plans of attack: 
# easiest would be to just keep only the first visit. This would make some sense because 
# my memory is that we had them come back every 3 mos regardless of CD4 count. 
# And given that we are basing all of this on CD4 count then probably cleanest just to look only at the first visit.
# We could always report how many had F/U visits and whether any CMVR was detected during F/U visits.
# The other way would be to see whether any of the second visits seem to have been triggered by a new CD4 count.
# In that case we would want to match up the photo that corresponds to the correct CD4 count. 
# Actually now that I think about it I wonder if we have to do this second way, because we want to allow people to be eligible
# In both the pre-photo period and also the photo period. So we might need to match these people up, but then allow for the possibility 
# That we can't match some of them. (because maybe they didn't have a new CD4, or the CD4 was above 100, and so they were getting screened just because it was 3 months later)
# Please take a look and see if you can figure out a good plan of attack here.

## Completed, only keeping first visit eliminates all duplicates except for the one error of entry above. 
## This seems like the cleanest way, but can go back and do differently if you would like.

cd4_data3 <- cd4_data2 %>%
  group_by(HN) %>%
  mutate(dup=n())
xtabs(data=cd4_data3, ~dup, addNA=TRUE)

# So there are definitely duplicates. In line with the above reasoning, I think we need to actually match up each photo visit and exam visit to each CD4 count.
# To simplify this:
inclinic_data3_small <- inclinic_data3 %>%
  select(HN, photodate, photonumber, visit)

cd4_data4 <- cd4_data3 %>%
  group_by(HN) %>%
  mutate(daterank=rank(cd4_ws_date, ties.method = "random")) %>%
  # Below just checking to make sure this is doing what I want it to in terms of the ties...
  # group_by(HN, daterank) %>%
  # mutate(duphnduprank=n()) # xtabs(data=cd4_data4, ~duphnduprank)
#? Just to make sure that I understand, if there are two cd4_ws_date that are the same then it will randomly select one 
#? to be first ie. daterank == 1 and put a 2 or 3 etc. into the daterank for the other identical cd4_ws_date rows?
  mutate(hn_daterank=paste(HN, daterank, sep="_")) %>%
  select(-daterank)

dating <- full_join(cd4_data4,inclinic_data3_small, by="HN") %>%
  mutate(datediff=ifelse(photodate>cd4_ws_date, photodate-cd4_ws_date, NA)) %>%
  group_by(HN, visit) %>%
  mutate(mindatediff=min(datediff, na.rm=TRUE)) %>% # Note the min, max etc doesn't work if there are missing values unless you add the na.rm=TRUE
  filter(datediff==mindatediff) %>%
  group_by(hn_daterank) %>%
  mutate(dupdating=n()) %>% # xtabs(data=filter(dating, dupdating>1), ~hn_daterank+visit, addNA=TRUE)
                      # OK so this shows that generally we are OK just taking the "First" visit except 5736562
# 5736562 is removed in most recent run of the file
  filter(dupdating==1 | ((dupdating==2 & visit=="First"))) %>%
  select(HN, visit, hn_daterank)

inclinic_data4 <- full_join(inclinic_data3, dating, by=c("HN", "visit"))
combineddata3 <- full_join(inclinic_data4, cd4_data4, by = "hn_daterank")

ts <- combineddata3 %>% 
  filter(is.na(hn_daterank) & !is.na(photonumber)) %>%
  group_by(HN.x, HN.y) %>%
  distinct(HN.x, HN.y)

# OK I need to stop this for the day but we next step is to find out why there are 69 people on that list. I could imagine 45 or so, since that is the number
# from above. But I think something did not happen correctly.
# An example is 5640994 --  for this one the first photo was taken before a CD4 count. Either the photo date is wrong 
# (totally possible--perhaps we can doublecheck by looking at the metadata from the photos themselves)
# or somehow had a screening visit before the first recorded CD4 count. This could be added to the list of ones to investigate on the EMR of Nakornping during a site visit
# I assume you don't have access to these photos; I am not even sure if I do. I will look.

## Updated sheet only has 8 listed here.  I do not have access to these photos. I am a little confused on exactly what happened
## at the end. I understand each individual step, but I don't undertsand how HN.x and HN.y can be distinct 


-----# BMS Stop here on 9.23

## Define a positive case for each dataset (getting exam by Choeng in non-photo, and photo in photo)
## Are the above definitions correct?

# JK: this is an OK idea though I would make a variable for whether the patient was screened, not whether they had a photo.
# Knowing that all patients screened before the photo period were with a Choeng exam, and all patients screened during the photo period were with photos.
# afterphotoperiod  <- afterphotoperiod %>%
#   mutate(phototaken = case_when(!is.na(photonumber) ~ 1,
#                                  is.na(photonumber) ~ 0))

#? I am unsure if the below defintion is correct, because I don't know the data well, 
# but these patients all have a Y dor dr. p or Dr. N, so I am assuming this means exam
# JK: include means that they were eligible for inclusion in the study. Not sure we need the examperformed variable.
# beforephotoperiod  <- beforephotoperiod %>%
#   mutate(examperformed = case_when(include == "Y" ~ 1,
#                                    include == "N" ~ 0))

## Returning to data:
# my confusion is that it seems like "patientrefertochoeng" would be the right way to do this,
# however this variable is missing for most cases in the before, 
# maybe this is the point and I will define separately as refertochoeng
# JK: patientrefertochoeng should mean that a photo was taken during the photo period, the med student or Ying thought it looked suspicious, so they wanted Choeng to examine the patient
# beforephotoperiod  <- beforephotoperiod %>%
#   mutate(refertochoeng = case_when(patientrefertochoeng == "Yes" ~ 1,
#                                    TRUE ~ 0))

##create column if CD4 is <100
# JK: Not really sure what is going on here. All patients had a CD4 < 100 on the CD4 data, right? So this seems unnecessary
# afterphotoperiod  <- afterphotoperiod %>%
#   mutate(cd4less100 = case_when(cd4_cell_ws <100 ~ 1,
#                                 cd4_cell_ws >100 ~ 0))
# 
# beforephotoperiod  <- beforephotoperiod %>%
#   mutate(cd4less100 = case_when(cd4_cell_ws <100 ~ 1,
#                                 cd4_cell_ws >100 ~ 0))
# 
# afterphotoperiod  <- afterphotoperiod %>%
#   mutate(cd4less100.2 = case_when(cd4count <100 ~ 1,
#                                 cd4count >100 ~ 0))
# 
# beforephotoperiod  <- beforephotoperiod %>%
#   mutate(cd4less100.2 = case_when(cd4count <100 ~ 1,
#                                 cd4count >100 ~ 0))

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
