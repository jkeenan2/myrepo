#Clear existing redcapexport and graphics
rm(list=ls())
graphics.off()

library(readr)
#Load exploratory redcapexport analysis packages
library(tidyverse)
library(scales)   # date/time scales for plots
library(lubridate)
library(epiR)
library(dplyr)

##Read Digicards Data
# JK: looks like cataract not recorded separately for each eye so not really sure how this will be useful 
#LL: Digicards didn't distinguish btwn OD/OS cataracts if I remember correctly (or else I assume I
#     would have separated them). I guess the data can be included in summary characteristics of eyes in 
#     Table 1.
# JK: missing data for 2 of them, I am filtering them out // LL: OK (complete set of photos not taken for these patients)
redcapexport <- read_csv("SmartphoneRetinalScr_DATA_2019-09-10_0855.csv") %>%
  filter(study_id!=3807366 & study_id!=3746321) %>%
  rename(otherretinalpath_right=other_retinal_path,
         otherretinalpath_left=other_retinal_path_left,
         dxdr_right=diagnosis_dr,
         dxdr_left=diagnosis_dr_left,
         dxamd_right=diagnosis_amd,
         dxamd_left=diagnosis_amd_left,
         dxglaucoma_right=diagnosis_glaucoma,
         dxglaucoma_left=diagnosis_glaucoma_left) %>%
  mutate(cam_a=recode_factor(cam_a, `1`="peek", `2`="inview", `3`="pictor"),
         cam_b=recode_factor(cam_b, `1`="peek", `2`="inview", `3`="pictor"),
         cam_c=recode_factor(cam_c, `1`="peek", `2`="inview", `3`="pictor")) %>%
  gather(field, value, cam_a:cam_c) %>%
  mutate(valueorder=paste(value, "order", sep="_"),
         field=case_when(field=="cam_a" ~ 1,
                         field=="cam_b" ~ 2,
                         field=="cam_c" ~ 3,
                         TRUE ~ NA_real_)) %>%
  select(-value) %>%
  spread(valueorder, field, convert=TRUE)

hospitalandphotoid <- read_csv("Hospital ID - Photo ID.csv")

#re-add leading 0's to studyid's
#wide -> long data; each photoid in separate row
hospitalandphotoid_long <- hospitalandphotoid %>%
  filter(study_id!=3807366 & study_id!=3746321) %>%
  mutate(study_id=formatC(as.integer(study_id), width = 7, format = "d", flag = "0")) %>%
  gather(key = camera, value = photoid, peek:pictor) %>%
  group_by(study_id, eye, camera) %>%
  mutate(duph=n())
# select(-duph) 
# xtabs(data=hospitalandphotoid_long, ~duph, addNA=TRUE)

# JK: There are 258 duplicates combinations of study_id, eye, and camera in this file. 
# JK: There should be no duplicates before merging.
# JK: Louisa, do you know what is going on here? Are these duplicates on purpose for intra-grader reliability or something?
# JK: If so we need to designate which one is the official photo and which one is the duplicate.
# LL: these duplicates are patients who had their photos taken on 2 separate clinic visits.
# LL: perhaps we can select photos from just the last visit for these patients?
hdups <-   hospitalandphotoid_long %>%
  filter(duph>1)

# JK: note changes; when you reshape you should do all the variables that have a right and left eye. Not just a single variable.
#long data for RE/LE diagnoses
redcapexport_long <- redcapexport %>%
  gather(key = field, value = value, dr_right:dxglaucoma_left) %>%
  separate(field, into=c("disease", "eye"), sep="_") %>%
  mutate(eye=case_when(eye=="right" ~ "RE",
                       eye=="left" ~ "LE",
                       TRUE ~ NA_character_)) %>%
  spread(disease, value, convert=TRUE) %>%
  select(peek_time:pictor_discomfort, inview_order:pictor_order, everything()) %>%
  gather(field, value, peek_time:pictor_order) %>%
  separate(field, into=c("camera", "var"), sep="_") %>%
  spread(var, value, convert=TRUE) %>%
  select(study_id, eye, camera, everything()) %>%
  group_by(study_id, eye, camera)
# mutate(dupr=n()) %>% # xtabs(data=redcapexport_long, ~dupr, addNA=TRUE)
# select(-dupr) 
# NO DUPLICATES

# JK: Please notice that we now have unique id of study_id, eye, and camera for both the redcapexport_long and hospitalandphotoid_long objects.
# JK: And the format of all 3 variables is the same. (eg, not right and RE but rather RE and RE)

# JK: I don't really understand this part. I don't think this is correct.
# redcapexport_long <- redcapexport_long %>%
#   mutate(eye = if_else(grepl("right", redcapexport_long$dr_eye), "RE", "LE")) %>%
#   select(-(dr_eye)) %>%
#   mutate(dme = if_else(eye=="RE", dme_right, dme_left)) %>%
#   mutate(amd = if_else(eye=="RE", amd_right, amd_left)) %>%
#   mutate(glaucoma = if_else(eye=="RE", glaucoma_right, glaucoma_left)) %>%  
#   mutate(otherdx = if_else(eye=="RE", otherdx_right, otherdx_left)) %>%  
#   mutate(other_retinal_path_new = if_else(eye=="RE", other_retinal_path, other_retinal_path_left)) %>%    
#   mutate(diagnosis_dr_new = if_else(eye=="RE", diagnosis_dr, diagnosis_dr_left)) %>%    
#   mutate(diagnosis_amd_new = if_else(eye=="RE", diagnosis_amd, diagnosis_amd_left)) %>%     
#   mutate(diagnosis_glaucoma_new = if_else(eye=="RE", diagnosis_glaucoma, diagnosis_glaucoma_left)) %>%
#   select(-(dme_right:diagnosis_glaucoma_left)) %>%
#   select(study_id, eye, everything())


#long-er data for cameras
# JK: Not sure this is correct...
# redcapexport_longer <- redcapexport_long %>%
#   gather(key = camera_time, value = seconds, peek_time:pictor_time)

# redcapexport_longer <- redcapexport_longer %>%
#   mutate(camera = if_else(grepl("peek", redcapexport_longer$camera_time), "peek",
#                   if_else(grepl("inview", redcapexport_longer$camera_time), "inview",    
#                   if_else(grepl("pictor", redcapexport_longer$camera_time), "pictor", "NA")))) %>%
#   select(-(camera_time)) %>%
#   mutate(discomfort = if_else(camera=="peek", peek_discomfort,
#                       if_else(camera=="inview", inview_discomfort,        
#                       if_else(camera=="pictor", pictor_discomfort, 999))))

# redcapexport_to_merge <- redcapexport_longer %>%
#   select(study_id, eye, camera, photo_date, age, gender, cataract, cataract_deg, dr:seconds, discomfort)


digicards_data <- full_join(redcapexport_long, hospitalandphotoid_long, by=c("study_id", "eye", "camera")) %>%
  mutate(photoid=as.character(photoid)) %>%
  filter(!is.na(photoid))
# JK: Note that the digicards_data includes the 258 duplicates from the hospitalandphotoid_long file; these need to be fixed in some way

# *** filtered out duplicate photos (keeping only photos taken on the last clinic visit; unfortunately I kept only the last clinic date
#       in Digicards, so couldn't automate this with code and had to do this manually via photoid's from my patient log spreadsheet)
digicards_data <- digicards_data %>%
  filter(!(photoid == "1943" | photoid == "1118" | photoid == "3240" | photoid == "2079" | photoid == "1229" | photoid == "1034")) %>%
  filter(!(photoid == "3103" | photoid == "1308" | photoid == "3108" | photoid == "3624" | photoid == "1784" | photoid == "1493")) %>%
  filter(!(photoid == "3439" | photoid == "1174" | photoid == "2368" | photoid == "1947" | photoid == "3218" | photoid == "1215")) %>%
  filter(!(photoid == "3903" | photoid == "2229" | photoid == "3130" | photoid == "1287" | photoid == "1506" | photoid == "3686")) %>%
  filter(!(photoid == "2895" | photoid == "2920" | photoid == "1966" | photoid == "1804" | photoid == "3115" | photoid == "1149")) %>%
  filter(!(photoid == "1008" | photoid == "1586" | photoid == "3505" | photoid == "2884" | photoid == "1455" | photoid == "3443")) %>%
  filter(!(photoid == "1943" | photoid == "1118" | photoid == "3240" | photoid == "2079" | photoid == "1229" | photoid == "1034")) %>%
  filter(!(photoid == "2858" | photoid == "3808" | photoid == "2297" | photoid == "1078" | photoid == "3274" | photoid == "2896")) %>%
  filter(!(photoid == "3763" | photoid == "3201" | photoid == "3970" | photoid == "3668" | photoid == "2429" | photoid == "2588")) %>%
  filter(!(photoid == "1582" | photoid == "2806" | photoid == "2616" | photoid == "2056" | photoid == "2631" | photoid == "2590")) %>%
  filter(!(photoid == "2058" | photoid == "2419" | photoid == "1988" | photoid == "2992" | photoid == "2213" | photoid == "3135")) %>%
  filter(!(photoid == "2556" | photoid == "3577" | photoid == "1870" | photoid == "2583" | photoid == "3395" | photoid == "2527")) %>%
  filter(!(photoid == "1713" | photoid == "1967" | photoid == "3587" | photoid == "3347" | photoid == "3464" | photoid == "3422")) %>%
  filter(!(photoid == "3383" | photoid == "2469" | photoid == "1672" | photoid == "1684" | photoid == "1237" | photoid == "1540")) %>%
  filter(!(photoid == "1810" | photoid == "1680" | photoid == "2488" | photoid == "1635" | photoid == "3194" | photoid == "2796")) %>%
  filter(!(photoid == "3266" | photoid == "2909" | photoid == "1838" | photoid == "1616" | photoid == "1222" | photoid == "2242")) %>%
  filter(!(photoid == "3364" | photoid == "3681" | photoid == "1550" | photoid == "3716" | photoid == "3898" | photoid == "3811")) %>%
  filter(!(photoid == "1946" | photoid == "3015" | photoid == "2517" | photoid == "2809" | photoid == "1969" | photoid == "3528")) %>%
  filter(!(photoid == "2547" | photoid == "2679" | photoid == "2550" | photoid == "1245" | photoid == "3209" | photoid == "3592")) %>%
  filter(!(photoid == "3442" | photoid == "1138" | photoid == "1407" | photoid == "2833" | photoid == "1249" | photoid == "3434")) %>%
  filter(!(photoid == "2050" | photoid == "2510" | photoid == "2413" | photoid == "2205" | photoid == "2246" | photoid == "2981")) %>%
  filter(!(photoid == "2363" | photoid == "2496" | photoid == "3913" | photoid == "2255" | photoid == "1218" | photoid == "3032")) %>%
  filter(!(photoid == "1809" | photoid == "1137" | photoid == "2198" | photoid == "2296" | photoid == "3562" | photoid == "2761")) %>%
  select(-duph)


# JK: Bad merges: Any idea what is going on with these?
# LL: these patients had photos taken in only one eye (but I recorded Digicards diagnoses for both eyes)
# LL: filtered these out from digicards_data (above)

# badmerge_no_h <- digicards_data %>%
#   filter(is.na(photoid))
# badmerge_no_r <- digicards_data %>%
#   filter(is.na(photo_date))

##################

# PHOTO ADJUDICATION FOR TEAM 1

# JK: Note, export the Redcap project "Smartphone Retinal Screening-Team 1"
# JK: It seems like it would be way more efficient to first combine all the data and then perform the manipulations
team1_import <- read_csv("SmartphoneRetinalScr_DATA_2019-09-10_0846_T1.csv") %>%  mutate(team=1)
team2_import <- read_csv("SmartphoneRetinalScr_DATA_2019-09-10_0851_T2.csv") %>%  mutate(team=2)
team3_import <- read_csv("SmartphoneRetinalScr_DATA_2019-09-10_0852_T3.csv") %>%  mutate(team=3)
team4_import <- read_csv("SmartphoneRetinalScr_DATA_2019-09-10_0852_T4.csv") %>%  mutate(team=4)

teams_import <- bind_rows(team1_import, team2_import, team3_import, team4_import)

# JK: Now repeat append process for the adjudicated grades
# JK: I ADDED THE MUTATE TEAM HERE TO PREVENT AN ERROR IN YOUR GRADING_DATA OBJECT BELOW
adj_team1 <- read_csv("SmartphoneAdjudicati_DATA_2019-09-10_0852_T1.csv") %>%  mutate(team=1)
adj_team2 <- read_csv("SmartphoneAdjudicati_DATA_2019-09-10_0853_T2.csv") %>%  mutate(team=2)
adj_team3 <- read_csv("SmartphoneAdjudicati_DATA_2019-09-10_0853_T3.csv") %>%  mutate(team=3)
adj_team4 <- read_csv("SmartphoneAdjudicati_DATA_2019-09-10_0853_T4.csv") %>%  mutate(team=4)

adj_teams <- bind_rows(adj_team1, adj_team2, adj_team3, adj_team4) %>%
  mutate(study_id=paste0(record_id, "--3")) %>%
  select(-record_id)


grading_data_allgrades <- bind_rows(teams_import, adj_teams) %>%
  select(-new_photo_grading_form_complete) %>%
  # JK: PREFERRED TO USE IF_ELSE NOT IFELSE
  mutate(dr_yesno = if_else((dr == 0 | dr == 1), 0, 
                            if_else((dr == 2 | dr == 3), 1, NA_real_)),
         dme_yesno = if_else((dme == 0 | dme == 1), 0, 
                             if_else((dme == 2 | dme == 3), 1, NA_real_)),
         amd_yesno = if_else((amd == 0 | amd == 1), 0, 
                             # JK: THE XTABS NEEDS TO USE THE CORRECT DATA...
                             if_else((amd == 2 | amd == 3), 1, NA_real_))) %>% # xtabs(data=grading_data, ~dr + dr_yesno, addNA=TRUE)
  # xtabs(data=grading_data, ~amd + amd_yesno, addNA=TRUE)
  separate(study_id, c("photoid", "grader"), sep = "--", remove = TRUE, convert = FALSE)

grading_data_consensus <- grading_data_allgrades %>%
  group_by(photoid) %>%
  # JK: CHANGE THIS BACK to mutate to PROBLEMSOLVE
  summarize(numgraders=n(),
            dr_final=median(dr_yesno),
            numdrgrades=sum(!is.na(dr_yesno)),
            dme_final=median(dme_yesno),
            numdmegrades=sum(!is.na(dme_yesno)),
            amd_final=median(amd_yesno),
            numamdgrades=sum(!is.na(amd_yesno)),
            cdr_final=median(cdr),
            glaucoma_final=if_else(cdr_final>=0.7,1,0),
            numcdrgrades=sum(!is.na(cdr)),
            # JK: JUST DEFINING THE SEVERITY AS THE LEAST SEVERE; THIS SEEMS MOST CONSERVATIVE?
            # JK: NOTE THAT IF DR==0 THEN SEVERITY IS MISSING NOT ZERO
            # JK: I PLAYED AROUND WITH THE BEST WAY TO DEAL WITH VARIABLES WITH 3 OR MORE LEVELS 
            # JK: THIS IS MEDIAN BUT TAKES LOWER VALUE IF A TIE BETWEN EVEN NUMBER
            # NOTE THAT THIS DOES not RESTRICT ONLY TO DR YES; WE MAY WANT TO DO THAT
            # drsev_final1=if_else(median(dr_yesno)==0,NA_real_,min(dr_severity)),
            # drsev_final2=if_else(median(dr_yesno)==0,NA_real_,median(dr_severity)),
            # (LL notes to self: https://www.rdocumentation.org/packages/stats/versions/3.6.1/topics/quantile)
            drsev_final=if_else(median(dr_yesno)==0,NA_real_,quantile(dr_severity, probs = 0.5, type=3, na.rm=TRUE)),
            amdsev_final=if_else(median(amd_yesno)==0,NA_real_,quantile(amd_severity, probs = 0.5, type=3, na.rm=TRUE)),
            # amdsev_final=if_else(median(amd_yesno)==0,NA_real_,min(amd_severity)),
            nervecov_final=recode_factor(quantile(nerve, probs = 0.5, type=3, na.rm=TRUE), `0`="None", `1`="Some", `2`="All"),
            maculacov_final=recode_factor(quantile(macula, probs = 0.5, type=3, na.rm=TRUE), `0`="None", `1`="Some", `2`="All"),
            imageclarity_final=quantile(image_clarity, probs = 0.5, type=3, na.rm=TRUE),
            vcdrconfidence_final=quantile(vcdr_confidence, probs = 0.5, type=3, na.rm=TRUE))
# JK: GENERALLY WE NEED AT LEAST 2 GRADES TO HAVE A CONSENSUS; INVESTIGATE WHEN only 1 GRADE...
xtabs(data=grading_data_consensus, ~numdrgrades + dr_final, addNA=TRUE)
xtabs(data=grading_data_consensus, ~numdmegrades + dme_final, addNA=TRUE)
xtabs(data=grading_data_consensus, ~numamdgrades + amd_final, addNA=TRUE)
xtabs(data=grading_data_consensus, ~numcdrgrades + cdr_final, addNA=TRUE)
xtabs(data=grading_data_consensus, ~drsev_final + dr_final, addNA=TRUE)
xtabs(data=grading_data_consensus, ~amdsev_final + amd_final, addNA=TRUE)

ts_needdme <- grading_data_consensus %>%
  filter(dme_final==0.5)
# THere are 48 images that would need to be graded before a consensus DME grade could be given
# LL: I don’t think grading for DME ended up being high yield at all, so I would be happy to just exclude DME from the analysis


# ONCE WE ARE DONE CLEANING WE CAN USE SUMMARIZE INSTEAD OF MUTATE...

# JK: THE FOLLOWING CAN BE DELETED BUT LEAVING THE NOTES FOR YOU IN CASE YOU ARE INTERESTED...
# gather(field, value, image_clarity:notes, dr_yesno:amd_yesno) %>%
# mutate(field.grader=paste(field, grader, sep=".")) %>%
# select(-field, -grader) %>%
# # JK: YOUR SPREAD DIDN'T WORK CORRECTLY. YOU NEED TO CHECK IT TO MAKE SURE. ONE EASY WAY IS TO LOOK AT THE NUMBER
# # OF OBSERVATIONS. IT SHOULD HAVE HAD SOMEWHERE ON THE ORDER OF 1200 BUT YOURS HAD 1720. SO NEED TO INVESTIGATE.
# # I QUICKLY NOTED THAT THERE WERE DUPLICATE PHOTOIDs WHICH YOU DO NOT WANT. AND THAT THESE SEEM TO RESULT FROM A 
# # PROBLEM WITH THE --3 RECORDS

# LL: Thank you for leaving these notes in - helpful for learning!

# # IF YOU JUST SEARCH FOR 1001 YOU SEE THAT THE --3 RECORDS HAVE A NA FOR "TEAM". SO I ADDED A NEW VARIABLE TEAM TO YOUR ADJUDICATED OBJECTS ABOVE
# spread(field.grader, value, convert=TRUE) %>%
# mutate(dr_final = if_else((dr_yesno.1 != dr_yesno.2 & !is.na(dr_yesno.3)), dr_yesno.3, dr_yesno.1),
#        amd_final = if_else((amd_yesno.1 != amd_yesno.2 & !is.na(amd_yesno.3)), amd_yesno.3, amd_yesno.1),
#        # JK: I THINK WE DEFINED DISCREPANT AS >0.2, RIGHT? 
#        # JK: YOU COULD CONFIRM ON THE PREVIOUS CODE WE USED TO DECIDE WHICH PHOTOS TO INCLUDE IN THE ADJUDICATION PROJECT
#        cdr_final = if_else((abs(cdr.1-cdr.2)>.2) & !is.na(cdr.3), cdr.3, 
#                    if_else((abs(cdr.1-cdr.2)<=.2) & !is.na(cdr.3), ((cdr.1 + cdr.2 + cdr.3)/ 3), 
#                    if_else((abs(cdr.1-cdr.2)<=.2) & is.na(cdr.3), ((cdr.1 + cdr.2)/ 2), 999)))) # xtabs(data=grading_data, ~cdr_final, addNA=TRUE)


# JK: note there are 2 types of records with missing dr_adj data: first, those with nonmissing grader 1 and 2 but missing grader3, and also those with missing grader 1 and 2 and 3
# Probably worth a double-check of the data to make sure this data is actually missing? Then we can drop the rows with completely missing data
# LL: ^NA's for all 3 graders are for photoid's for photos that weren't taken (actually missing) or mistaken entries (i.e. 1153a, b, c, d)


#adj_discrep <- grading_data %>% filter(dr_yesno.3!=dr_adj)
# JK: I am not sure how these people got an adjudicated grade since the grader 1 and 2 grades matched. 
# But I guess we take the consensus grade, which would be the grader 1 and 2 grade.
# But worth looking into what happened here. (Look at a few of the photos...)
# ^ LL: grader 1 & grader 2 disagreed on either AMD or CDR for these cases


# JK: for severity and other variables that are not dichotomous (eg image_clarity): we need to be careful. 
# For severity, I think being conservative here means that it's harder to pick up milder disease
# So we would make severe disease trump mild disease
# So then need to take the max of the severity grades. Best would be to take the consensus but not sure that is possible.
# Maybe you can take a stab at making consensus grades for the categorical variables


# JK: DON'T UNDERSTAND WHY YOU ARE DOING THIS...
#make amd_severity == 4 → 0.4
# grading_data$amd_severity.1[grading_data$amd_severity.1 == 4] <- 0.4
# grading_data$amd_severity.2[grading_data$amd_severity.2 == 4] <- 0.4
# grading_data$amd_severity.3[grading_data$amd_severity.3 == 4] <- 0.4
# 
# #make dr_severity == 5 → 0.5
# grading_data$dr_severity.1[grading_data$dr_severity.1 == 5] <- 0.5
# grading_data$dr_severity.2[grading_data$dr_severity.2 == 5] <- 0.5
# grading_data$dr_severity.3[grading_data$dr_severity.3 == 5] <- 0.5

#final values for categorical/other variables
# grading_data <- grading_data %>%
#   mutate(amd_severity_final = if_else(amd_final == 0, 0,
#                               if_else(amd_final == 1 & (amd_severity.1>amd_severity.2), amd_severity.1, amd_severity.2))) %>%
#   mutate(dme_final = if_else(dme.1>dme.2, dme.1, dme.2)) %>%
#   mutate(dr_severity_final = if_else(dr_final == 0, 0,
#                              if_else(dr_final == 1 & (dr_severity.1>dr_severity.2), dr_severity.1, dr_severity.2))) %>%
#   mutate(image_clarity_final = if_else(image_clarity.1>image_clarity.2, image_clarity.1, image_clarity.2)) %>%
#   mutate(macula_final = if_else(macula.1>macula.2, macula.1, macula.2)) %>%
#   mutate(nerve_final = if_else(nerve.1>nerve.2, nerve.1, nerve.2)) %>%
#   mutate(notes_final = paste(notes.1, notes.2, notes.3, sep="/")) %>%
#   mutate(other_dx_detail_final = paste(other_dx_detail.1, other_dx_detail.2, other_dx_detail.3, sep="/")) %>%
#   mutate(other_dx_final = if_else(other_dx.1>other_dx.2, other_dx.1, other_dx.2)) %>%
#   mutate(vcdr_confidence_final = if_else(!is.na(vcdr_confidence.3), vcdr_confidence.3,
#                                  if_else(vcdr_confidence.1>vcdr_confidence.2, vcdr_confidence.1, vcdr_confidence.2))) %>%
#   mutate(zone2_final = if_else(zone2.1>zone2.2, zone2.1, zone2.2)) %>%
#   mutate(glaucoma_final = if_else(!is.na(cdr_final) & cdr_final >=0.7, 1, 0)) %>%
#   mutate(other_dx_yesno = if_else((other_dx_final == 0 | other_dx_final == 1), 0,
#                           if_else((other_dx_final == 2 | other_dx_final == 3), 1, 999)))


clean_grading_data <- grading_data_consensus %>%
  select(photoid, imageclarity_final, dr_final, drsev_final, dme_final, amd_final, amdsev_final, vcdrconfidence_final, cdr_final, glaucoma_final)


#xtabs(data=grading_data, ~dr_yesno.1+dr_yesno.2, addNA=TRUE)
#xtabs(data=grading_data, ~amd_yesno.1+amd_yesno.2, addNA=TRUE)

# class(digicards_data$photoid)
# class(clean_grading_data$photoid)
# digicards_data$photoid <- as.character(digicards_data$photoid)

alldata <- full_join(digicards_data, clean_grading_data, by="photoid") %>%
  arrange(study_id, eye, camera)

# removing duplicate patient sets of photos
alldata <- alldata %>%
  filter(!(photoid == "1943" | photoid == "1118" | photoid == "3240" | photoid == "2079" | photoid == "1229" | photoid == "1034")) %>%
  filter(!(photoid == "3103" | photoid == "1308" | photoid == "3108" | photoid == "3624" | photoid == "1784" | photoid == "1493")) %>%
  filter(!(photoid == "3439" | photoid == "1174" | photoid == "2368" | photoid == "1947" | photoid == "3218" | photoid == "1215")) %>%
  filter(!(photoid == "3903" | photoid == "2229" | photoid == "3130" | photoid == "1287" | photoid == "1506" | photoid == "3686")) %>%
  filter(!(photoid == "2895" | photoid == "2920" | photoid == "1966" | photoid == "1804" | photoid == "3115" | photoid == "1149")) %>%
  filter(!(photoid == "1008" | photoid == "1586" | photoid == "3505" | photoid == "2884" | photoid == "1455" | photoid == "3443")) %>%
  filter(!(photoid == "1943" | photoid == "1118" | photoid == "3240" | photoid == "2079" | photoid == "1229" | photoid == "1034")) %>%
  filter(!(photoid == "2858" | photoid == "3808" | photoid == "2297" | photoid == "1078" | photoid == "3274" | photoid == "2896")) %>%
  filter(!(photoid == "3763" | photoid == "3201" | photoid == "3970" | photoid == "3668" | photoid == "2429" | photoid == "2588")) %>%
  filter(!(photoid == "1582" | photoid == "2806" | photoid == "2616" | photoid == "2056" | photoid == "2631" | photoid == "2590")) %>%
  filter(!(photoid == "2058" | photoid == "2419" | photoid == "1988" | photoid == "2992" | photoid == "2213" | photoid == "3135")) %>%
  filter(!(photoid == "2556" | photoid == "3577" | photoid == "1870" | photoid == "2583" | photoid == "3395" | photoid == "2527")) %>%
  filter(!(photoid == "1713" | photoid == "1967" | photoid == "3587" | photoid == "3347" | photoid == "3464" | photoid == "3422")) %>%
  filter(!(photoid == "3383" | photoid == "2469" | photoid == "1672" | photoid == "1684" | photoid == "1237" | photoid == "1540")) %>%
  filter(!(photoid == "1810" | photoid == "1680" | photoid == "2488" | photoid == "1635" | photoid == "3194" | photoid == "2796")) %>%
  filter(!(photoid == "3266" | photoid == "2909" | photoid == "1838" | photoid == "1616" | photoid == "1222" | photoid == "2242")) %>%
  filter(!(photoid == "3364" | photoid == "3681" | photoid == "1550" | photoid == "3716" | photoid == "3898" | photoid == "3811")) %>%
  filter(!(photoid == "1946" | photoid == "3015" | photoid == "2517" | photoid == "2809" | photoid == "1969" | photoid == "3528")) %>%
  filter(!(photoid == "2547" | photoid == "2679" | photoid == "2550" | photoid == "1245" | photoid == "3209" | photoid == "3592")) %>%
  filter(!(photoid == "3442" | photoid == "1138" | photoid == "1407" | photoid == "2833" | photoid == "1249" | photoid == "3434")) %>%
  filter(!(photoid == "2050" | photoid == "2510" | photoid == "2413" | photoid == "2205" | photoid == "2246" | photoid == "2981")) %>%
  filter(!(photoid == "2363" | photoid == "2496" | photoid == "3913" | photoid == "2255" | photoid == "1218" | photoid == "3032")) %>%
  filter(!(photoid == "1809" | photoid == "1137" | photoid == "2198" | photoid == "2296" | photoid == "3562" | photoid == "2761")) 

alldataofallgraders <- full_join(digicards_data, grading_data_allgrades, by="photoid") %>%
  arrange(study_id, eye, camera)

# JK For final analysis we need to make sure that we have nonmissing data for gold standard and adjudicated grade for all 3 cameras. 
# Only include those observations that meet this criteria.
# But confirm that we have incomplete data for these
# LL: checked patient log; confirmed these patients didn't have photos taken for 1+ cameras or an entire eye
missingdata <- as.data.frame(xtabs(data=filter(alldata, !is.na(dr_final) & !is.na(dr)), ~study_id+camera+eye)) %>%
  mutate(studyideye=paste(study_id, eye, sep="_")) %>%
  select(-study_id, -eye) %>%
  spread(camera, Freq) %>%
  mutate(sum=inview+pictor+peek,
         product=inview*pictor*peek) %>%
  filter(!(sum==3 & product==1)) %>%
  filter(!(sum==6 & product==8))
incompletedatavector <- missingdata[["studyideye"]]
write_csv(missingdata, "missingdata.csv")

#take out rows with incomplete data for digicards/gold standard data & graded/adjudicated data for all 3 cameras
alldata_final <- alldata %>%
  ungroup() %>%
  mutate(studyid_eye=paste(study_id, eye, sep="_"),
         dr=as.factor(dr),
         dr_final=as.factor(dr_final)) %>%
  filter(!(studyid_eye %in% incompletedatavector)) %>%
  group_by(studyid_eye) %>%
  mutate(num_of_obs=n()) %>%
  filter(num_of_obs == 3) 
# xtabs(data=alldata_final, ~num_of_obs+camera, addNA=TRUE)
# Should only be 3 observations per studyid_eye. So figure out what is going on.
# LL: these photoid's with 12 obs correspond to patients with missing photos. Removed!
a <- alldata_final %>% ungroup() %>% filter(num_of_obs==3) %>% distinct(study_id) 
b <- alldata_final %>% ungroup() %>% filter(num_of_obs==3) %>% distinct(studyid_eye)


################################
##          ANALYSES          ##
################################

# Inter-rater reliability, comparing 2 photo-graders for each camera
grading_data_allgrades_kappadata <- alldataofallgraders %>%
  filter(grader %in% c(1,2)) %>%
  select(photoid, grader, camera, dr_yesno, amd_yesno) %>%
  gather(field, value, dr_yesno:amd_yesno) %>%
  mutate(fieldgrader=paste(field, grader, sep="__")) %>%
  select(-field, -grader) %>%
  spread(fieldgrader, value, convert=TRUE)

library(irr)
kappa2(grading_data_allgrades_kappadata[,5:6])
# JK: I guess I don't love this syntax because hard to know what 5:6 is, whereas when you use the actual variable name then clearer.
# Plus it doesn't give confidence intervals.
# Trying out alternatives, then just picking one...
library(fmsb)
Kappa.test(x=grading_data_allgrades_kappadata$amd_yesno__1, y=grading_data_allgrades_kappadata$amd_yesno__2, conf.level=0.95)
library(DescTools)
CohenKappa(x=grading_data_allgrades_kappadata$amd_yesno__1, y=grading_data_allgrades_kappadata$amd_yesno__2, conf.level=0.95)
library(psych)
cohen.kappa(x=cbind(filter(grading_data_allgrades_kappadata, camera=="peek")$amd_yesno__1, filter(grading_data_allgrades_kappadata, camera=="peek")$amd_yesno__2), alpha=0.05)
cohen.kappa(x=cbind(filter(grading_data_allgrades_kappadata, camera=="pictor")$amd_yesno__1, filter(grading_data_allgrades_kappadata, camera=="pictor")$amd_yesno__2), alpha=0.05)
cohen.kappa(x=cbind(filter(grading_data_allgrades_kappadata, camera=="inview")$amd_yesno__1, filter(grading_data_allgrades_kappadata, camera=="inview")$amd_yesno__2), alpha=0.05)
# JK: Note that different numbers for each comparison. That is not fair. Need to make sure you are doing it on the same population.
grading_data_allgrades_kappadata2 <- grading_data_allgrades_kappadata %>%
  mutate(studyid_eye=paste(study_id, eye, sep="_")) %>%
  filter(!(studyid_eye %in% incompletedatavector)) %>%
  group_by(studyid_eye) %>%
  mutate(num_perstudyideye=n()) %>% # xtabs(data=grading_data_allgrades_kappadata2, ~numperstudyideye+camera)
  # xtabs(data=grading_data_allgrades_kappadata2, ~study_id+camera)
  filter(num_perstudyideye==3 & !is.na(amd_yesno__1) & !is.na(amd_yesno__2) & !is.na(dr_yesno__1) & !is.na(dr_yesno__2))
# Kappas on the same population (156 eyes)
cohen.kappa(x=cbind(filter(grading_data_allgrades_kappadata2, camera=="peek")$amd_yesno__1, filter(grading_data_allgrades_kappadata2, camera=="peek")$amd_yesno__2), alpha=0.05)
cohen.kappa(x=cbind(filter(grading_data_allgrades_kappadata2, camera=="pictor")$amd_yesno__1, filter(grading_data_allgrades_kappadata2, camera=="pictor")$amd_yesno__2), alpha=0.05)
cohen.kappa(x=cbind(filter(grading_data_allgrades_kappadata2, camera=="inview")$amd_yesno__1, filter(grading_data_allgrades_kappadata2, camera=="inview")$amd_yesno__2), alpha=0.05)


# JK: Trying out the yardstick package from Louisa's email
# JK: Note that the variable names are not all that self-evident...
# JK: the gold standard grades are dr, amd, dme, etc.
# JK: The index tests are dr_final, amd_final, etc.
#https://stackoverflow.com/questions/57146785/how-to-perform-bootstrapping-to-a-diagnostic-test-in-r
library(yardstick)
options(yardstick.event_first = FALSE)

addmargins(xtabs(data=alldata_final, ~dr+dr_final))
alldata_final %>% filter(!is.na(camera)) %>% group_by(camera) %>% sens(., truth = as.factor(dr), estimate = as.factor(dr_final))
alldata_final %>% filter(!is.na(camera)) %>% group_by(camera) %>% spec(., truth = as.factor(dr), estimate = as.factor(dr_final))

# I guess you can get multiple metrics at once with this "metric_set" command
class_metrics <- metric_set(sens, spec, ppv, npv)

library(skimr)
skim(alldata_final %>% ungroup())
# For the actual numbers...
confmatrixtable_dr <- alldata_final %>%
  filter(!is.na(camera)) %>%
  group_by(camera) %>%
  conf_mat(dr, dr_final) %>%
  mutate(tidied = map(conf_mat, tidy)) %>%
  unnest(tidied) %>%
  spread(name, value) %>%
  rename(bothpos=cell_1_1,
         bothneg=cell_2_2,
         testpos_gsneg=cell_1_2,
         testneg_gspos=cell_2_1)

confmat_peek_dr <- alldata_final %>%
  filter(camera=="peek") %>%
  conf_mat(dr, dr_final)
summary(confmat_peek_dr)

confmat_inview_dr <- alldata_final %>%
  filter(camera=="inview") %>%
  conf_mat(dr, dr_final)
summary(confmat_inview_dr)

confmat_pictor_dr <- alldata_final %>%
  filter(camera=="pictor") %>%
  conf_mat(dr, dr_final)
summary(confmat_pictor_dr)



# Now to get estimates and CIs, need to do separately. First an object with the estimates
sensspec_estimates_dr <- alldata_final %>%
  filter(!is.na(camera)) %>%
  group_by(camera) %>%
  class_metrics(., truth = dr, estimate = dr_final) %>%
  select(-.estimator) %>%
  spread(.metric, .estimate, convert=TRUE) %>%
  rename(sens_est=sens,
         spec_est=spec,
         npv_est=npv,
         ppv_est=ppv)
# Check
xtabs(data=filter(alldata_final, camera=="peek"), ~dr+dr_final)

###########################################################
## BOOTSTRAPPED 95% CI ACCOUNTING FOR CLUSTERING OF EYES ##
###########################################################

# This creates a nested data frame, where all data with same study id get put on the same line
# So if we resample, we will automatically resample all data from the same person
D <- alldata_final %>% filter(!is.na(camera)) %>% nest(-studyid_eye)
head(D)
library(rsample)
set.seed(154234)
# The bs object is the boostrap object; we are creating separate populations with resampling
# You could alter the "times" option; usually use small number of replications as testing code because faster
# But then change to a larger number (9999?) for the final analysis
bs <- bootstraps(D, times = 9)
library(purrr)

# Need the purrr package for the map function
# The map function applies a function iteratively to each element of a list or vector
# For example, let's apply "p" to everything on the list after the comma
p <- c(0.025, 0.975)
p
pmap1 <- map(p, function(x) 2 + x )
# For a shortcut you can use the ~ to signify "function" and . to signify where to apply the "p"
pmap2 <- map(p, ~ 2 + . )
pmap1
pmap2
# (The pmap1 and pmap2 are just examples, not used in analysis below. But the "p" is used)
# So here, we can make a list of both the 2.5% and 97.5% by aplying the p vector above to this partial function
# Will use this below to apply to all the metrics to get the 2.5% and 97.5% of the confidence interval.
p_funs <- map(p, ~partial(quantile, probs = .x, na.rm = TRUE)) %>% 
  set_names(nm = c("lower95", "upper95"))

# So finally here is the dataframe with the CIs
bs_sensspec_dr <- map(bs$splits, ~as_tibble(.) %>% unnest %>% 
                        filter(!is.na(camera)) %>%
                        group_by(camera) %>% 
                        class_metrics(., truth = dr, estimate = dr_final)) %>% 
  bind_rows(.id = 'boots') %>%
  select(-.estimator) %>%
  spread(.metric, .estimate, convert=TRUE) %>%
  group_by(camera) %>%
  summarize_at(vars(sens, spec, ppv, npv), lst(!!!p_funs))


# Now merge together the estimates and CIs and reshape into more useful format
sensspectable_dr <- full_join(sensspec_estimates_dr, bs_sensspec_dr, by="camera")
sensspectablelong_dr <- sensspectable_dr %>%
  gather(field, value, npv_est:npv_upper95) %>%
  separate(field, into=c("metric", "stat"), sep="_") %>%
  spread(stat, value, convert=TRUE)
write_csv(sensspectablelong_dr, "sensspectablelong_dr.csv")

## END JK CODE HERE


# numbers for AMD
addmargins(xtabs(data=alldata_final, ~amd+amd_final))
alldata_final %>% filter(!is.na(camera)) %>% group_by(camera) %>% sens(., truth = as.factor(amd), estimate = as.factor(amd_final))
alldata_final %>% filter(!is.na(camera)) %>% group_by(camera) %>% spec(., truth = as.factor(amd), estimate = as.factor(amd_final))

alldata_final$amd <- as.factor(alldata_final$amd)
alldata_final$amd_final <- as.factor(alldata_final$amd_final)
class(alldata_final$amd)
class(alldata_final$amd_final)

confmatrixtable_amd <- alldata_final %>%
  filter(!is.na(camera)) %>%
  group_by(camera) %>%
  conf_mat(amd, amd_final) %>%
  mutate(tidied = map(conf_mat, tidy)) %>%
  unnest(tidied) %>%
  spread(name, value) %>%
  rename(bothpos=cell_1_1,
         bothneg=cell_2_2,
         testpos_gsneg=cell_1_2,
         testneg_gspos=cell_2_1)

confmat_peek_amd <- alldata_final %>%
  filter(camera=="peek") %>%
  conf_mat(amd, amd_final)
summary(confmat_peek_amd)

confmat_inview_amd <- alldata_final %>%
  filter(camera=="inview") %>%
  conf_mat(amd, amd_final)
summary(confmat_inview_amd)

confmat_pictor_amd <- alldata_final %>%
  filter(camera=="pictor") %>%
  conf_mat(amd, amd_final)
summary(confmat_pictor_amd)


sensspec_estimates_amd <- alldata_final %>%
  filter(!is.na(camera)) %>%
  group_by(camera) %>%
  class_metrics(., truth = amd, estimate = amd_final) %>%
  select(-.estimator) %>%
  spread(.metric, .estimate, convert=TRUE) %>%
  rename(sens_est=sens,
         spec_est=spec,
         npv_est=npv,
         ppv_est=ppv)
# Check
xtabs(data=filter(alldata_final, camera=="peek"), ~amd+amd_final)


## bootstrapped 95% CI accounting for clustering of eyes 
bs_sensspec_amd <- map(bs$splits, ~as_tibble(.) %>% unnest %>% 
                         filter(!is.na(camera)) %>%
                         group_by(camera) %>% 
                         class_metrics(., truth = as.factor(amd), estimate = as.factor(amd_final))) %>% 
  bind_rows(.id = 'boots') %>%
  select(-.estimator) %>%
  spread(.metric, .estimate, convert=TRUE) %>%
  group_by(camera) %>%
  summarize_at(vars(sens, spec, ppv, npv), lst(!!!p_funs))


# Now merge together the estimates and CIs and reshape into more useful format
sensspectable_amd <- full_join(sensspec_estimates_amd, bs_sensspec_amd, by="camera")
sensspectablelong_amd <- sensspectable_amd %>%
  gather(field, value, npv_est:npv_upper95) %>%
  separate(field, into=c("metric", "stat"), sep="_") %>%
  spread(stat, value, convert=TRUE) %>%
  write.csv("senspectablelong_amd.csv")




# numbers for glaucoma

addmargins(xtabs(data=alldata_final, ~glaucoma+glaucoma_final))
alldata_final %>% filter(!is.na(camera)) %>% group_by(camera) %>% sens(., truth = as.factor(glaucoma), estimate = as.factor(glaucoma_final))
alldata_final %>% filter(!is.na(camera)) %>% group_by(camera) %>% spec(., truth = as.factor(glaucoma), estimate = as.factor(glaucoma_final))

alldata_final$glaucoma <- as.factor(alldata_final$glaucoma)
alldata_final$glaucoma_final <- as.factor(alldata_final$glaucoma_final)
class(alldata_final$glaucoma)
class(alldata_final$glaucoma_final)

confmatrixtable_glaucoma <- alldata_final %>%
  filter(!is.na(camera)) %>%
  group_by(camera) %>%
  conf_mat(glaucoma, glaucoma_final) %>%
  mutate(tidied = map(conf_mat, tidy)) %>%
  unnest(tidied) %>%
  spread(name, value) %>%
  rename(bothpos=cell_1_1,
         bothneg=cell_2_2,
         testpos_gsneg=cell_1_2,
         testneg_gspos=cell_2_1)

confmat_peek_glaucoma <- alldata_final %>%
  filter(camera=="peek") %>%
  conf_mat(glaucoma, glaucoma_final)
summary(confmat_peek_glaucoma)

confmat_inview_glaucoma <- alldata_final %>%
  filter(camera=="inview") %>%
  conf_mat(glaucoma, glaucoma_final)
summary(confmat_inview_glaucoma)

confmat_pictor_glaucoma <- alldata_final %>%
  filter(camera=="pictor") %>%
  conf_mat(glaucoma, glaucoma_final)
summary(confmat_pictor_glaucoma)


sensspec_estimates_glaucoma <- alldata_final %>%
  filter(!is.na(camera)) %>%
  group_by(camera) %>%
  class_metrics(., truth = glaucoma, estimate = glaucoma_final) %>%
  select(-.estimator) %>%
  spread(.metric, .estimate, convert=TRUE) %>%
  rename(sens_est=sens,
         spec_est=spec,
         npv_est=npv,
         ppv_est=ppv)
# Check
xtabs(data=filter(alldata_final, camera=="peek"), ~glaucoma+glaucoma_final)



## bootstrapped 95% CI accounting for clustering of eyes 
bs_sensspec_glaucoma <- map(bs$splits, ~as_tibble(.) %>% unnest %>% 
                              filter(!is.na(camera)) %>%
                              group_by(camera) %>% 
                              class_metrics(., truth = as.factor(glaucoma), estimate = as.factor(glaucoma_final))) %>% 
  bind_rows(.id = 'boots') %>%
  select(-.estimator) %>%
  spread(.metric, .estimate, convert=TRUE) %>%
  group_by(camera) %>%
  summarize_at(vars(sens, spec, ppv, npv), lst(!!!p_funs))


# Now merge together the estimates and CIs and reshape into more useful format
sensspectable_glaucoma <- full_join(sensspec_estimates_glaucoma, bs_sensspec_glaucoma, by="camera")
sensspectablelong_glaucoma <- sensspectable_glaucoma %>%
  gather(field, value, npv_est:npv_upper95) %>%
  separate(field, into=c("metric", "stat"), sep="_") %>%
  spread(stat, value, convert=TRUE) %>%
  write.csv("senspectablelong_glaucoma.csv")



# numbers for other retinal diagnosis

addmargins(xtabs(data=alldata_final, ~otherdx+other_dx_yesno))
alldata_final %>% filter(!is.na(camera)) %>% group_by(camera) %>% sens(., truth = as.factor(otherdx), estimate = as.factor(other_dx_yesno))
alldata_final %>% filter(!is.na(camera)) %>% group_by(camera) %>% spec(., truth = as.factor(otherdx), estimate = as.factor(other_dx_yesno))

alldata_final$otherdx <- as.factor(alldata_final$otherdx)
alldata_final$other_dx_yesno <- as.factor(alldata_final$other_dx_yesno)
class(alldata_final$otherdx)
class(alldata_final$other_dx_yesno)

confmatrixtable_otherdx <- alldata_final %>%
  filter(!is.na(camera)) %>%
  group_by(camera) %>%
  conf_mat(otherdx, other_dx_yesno) %>%
  mutate(tidied = map(conf_mat, tidy)) %>%
  unnest(tidied) %>%
  spread(name, value) %>%
  rename(bothpos=cell_1_1,
         bothneg=cell_2_2,
         testpos_gsneg=cell_1_2,
         testneg_gspos=cell_2_1)

confmat_peek_otherdx <- alldata_final %>%
  filter(camera=="peek") %>%
  conf_mat(otherdx, other_dx_yesno)
summary(confmat_peek_otherdx)

confmat_inview_otherdx <- alldata_final %>%
  filter(camera=="inview") %>%
  conf_mat(otherdx, other_dx_yesno)
summary(confmat_inview_otherdx)

confmat_pictor_otherdx<- alldata_final %>%
  filter(camera=="pictor") %>%
  conf_mat(otherdx, other_dx_yesno)
summary(confmat_pictor_otherdx)


sensspec_estimates_otherdx <- alldata_final %>%
  filter(!is.na(camera)) %>%
  group_by(camera) %>%
  class_metrics(., truth = otherdx, estimate = other_dx_yesno) %>%
  select(-.estimator) %>%
  spread(.metric, .estimate, convert=TRUE) %>%
  rename(sens_est=sens,
         spec_est=spec,
         npv_est=npv,
         ppv_est=ppv)
# Check
xtabs(data=filter(alldata_final, camera=="peek"), ~otherdx+other_dx_yesno)



## bootstrapped 95% CI accounting for clustering of eyes
bs_sensspec_otherdx <- map(bs$splits, ~as_tibble(.) %>% unnest %>%
                             filter(!is.na(camera)) %>%
                             group_by(camera) %>%
                             class_metrics(., truth = as.factor(otherdx), estimate = as.factor(other_dx_yesno))) %>%
  bind_rows(.id = 'boots') %>%
  select(-.estimator) %>%
  spread(.metric, .estimate, convert=TRUE) %>%
  group_by(camera) %>%
  summarize_at(vars(sens, spec, ppv, npv), lst(!!!p_funs))


# Now merge together the estimates and CIs and reshape into more useful format
sensspectable_otherdx <- full_join(sensspec_estimates_otherdx, bs_sensspec_otherdx, by="camera")
sensspectablelong_otherdx <- sensspectable_otherdx %>%
  gather(field, value, npv_est:npv_upper95) %>%
  separate(field, into=c("metric", "stat"), sep="_") %>%
  spread(stat, value, convert=TRUE) %>%
  write.csv("senspectablelong_otherdx.csv")


################################################

# SN for screening for non-proliferative DR

nonprolif_dr_subset <- alldata_final %>%
  filter(diagnosis_dr == 1 | diagnosis_dr == 2)

addmargins(xtabs(data=nonprolif_dr_subset, ~dr+dr_final))
nonprolif_dr_subset %>% filter(!is.na(camera)) %>% group_by(camera) %>% sens(., truth = as.factor(dr), estimate = as.factor(dr_final))

confmatrixtable_nonprolif_dr_subset <- nonprolif_dr_subset %>%
  filter(!is.na(camera)) %>%
  group_by(camera) %>%
  conf_mat(dr, dr_final) %>%
  mutate(tidied = map(conf_mat, tidy)) %>%
  unnest(tidied) %>%
  spread(name, value) %>%
  rename(bothpos=cell_1_1,
         bothneg=cell_2_2,
         testpos_gsneg=cell_1_2,
         testneg_gspos=cell_2_1)

confmat_peek_nonprolif_dr_subset <- nonprolif_dr_subset %>%
  filter(camera=="peek") %>%
  conf_mat(dr, dr_final)
summary(confmat_peek_nonprolif_dr_subset)

confmat_inview_nonprolif_dr_subset <- nonprolif_dr_subset %>%
  filter(camera=="inview") %>%
  conf_mat(dr, dr_final)
summary(confmat_inview_nonprolif_dr_subset)

confmat_pictor_nonprolif_dr_subset <- nonprolif_dr_subset %>%
  filter(camera=="pictor") %>%
  conf_mat(dr, dr_final)
summary(confmat_pictor_nonprolif_dr_subset)


# Now to get estimates and CIs, need to do separately. First an object with the estimates
sensspec_estimates_nonprolif_dr_subset <- nonprolif_dr_subset %>%
  filter(!is.na(camera)) %>%
  group_by(camera) %>%
  class_metrics(., truth = dr, estimate = dr_final) %>%
  select(-.estimator) %>%
  spread(.metric, .estimate, convert=TRUE) %>%
  rename(sens_est=sens,
         spec_est=spec,
         npv_est=npv,
         ppv_est=ppv)
# Check
xtabs(data=filter(nonprolif_dr_subset, camera=="peek"), ~dr+dr_final)

# So finally here is the dataframe with the CIs
bs_sensspec_nonprolif_dr_subset <- map(bs$splits, ~as_tibble(.) %>% unnest %>% 
                                         filter(!is.na(camera)) %>%
                                         group_by(camera) %>% 
                                         class_metrics(., truth = dr, estimate = dr_final)) %>% 
  bind_rows(.id = 'boots') %>%
  select(-.estimator) %>%
  spread(.metric, .estimate, convert=TRUE) %>%
  group_by(camera) %>%
  summarize_at(vars(sens, spec, ppv, npv), lst(!!!p_funs))

# Now merge together the estimates and CIs and reshape into more useful format
sensspectable_nonprolif_dr_subset <- full_join(sensspec_estimates_nonprolif_dr_subset, bs_sensspec_nonprolif_dr_subset, by="camera")
sensspectablelong_nonprolif_dr_subset <- sensspectable_nonprolif_dr_subset %>%
  gather(field, value, npv_est:npv_upper95) %>%
  separate(field, into=c("metric", "stat"), sep="_") %>%
  spread(stat, value, convert=TRUE) %>%
  write.csv("senspectablelong_nonprolif_dr_subset.csv")





# SN for screening for early/intermed AMD

earlyint_amd_subset <- alldata_final %>%
  filter(diagnosis_amd == 1 | diagnosis_amd == 2)

addmargins(xtabs(data=earlyint_amd_subset, ~amd+amd_final))
earlyint_amd_subset %>% filter(!is.na(camera)) %>% group_by(camera) %>% sens(., truth = as.factor(amd), estimate = as.factor(amd_final))
earlyint_amd_subset %>% filter(!is.na(camera)) %>% group_by(camera) %>% spec(., truth = as.factor(amd), estimate = as.factor(amd_final))

earlyint_amd_subset$amd <- as.factor(earlyint_amd_subset$amd)
earlyint_amd_subset$amd_final <- as.factor(earlyint_amd_subset$amd_final)
class(earlyint_amd_subset$amd)
class(earlyint_amd_subset$amd_final)

confmatrixtable_earlyint_amd_subset <- earlyint_amd_subset %>%
  filter(!is.na(camera)) %>%
  group_by(camera) %>%
  conf_mat(amd, amd_final) %>%
  mutate(tidied = map(conf_mat, tidy)) %>%
  unnest(tidied) %>%
  spread(name, value) %>%
  rename(bothpos=cell_1_1,
         bothneg=cell_2_2,
         testpos_gsneg=cell_1_2,
         testneg_gspos=cell_2_1)

confmat_peek_earlyint_amd_subset <- earlyint_amd_subset %>%
  filter(camera=="peek") %>%
  conf_mat(amd, amd_final)
summary(confmat_peek_amd)

confmat_inview_earlyint_amd_subset <- earlyint_amd_subset %>%
  filter(camera=="inview") %>%
  conf_mat(amd, amd_final)
summary(confmat_inview_amd)

confmat_pictor_earlyint_amd_subset <- earlyint_amd_subset %>%
  filter(camera=="pictor") %>%
  conf_mat(amd, amd_final)
summary(confmat_pictor_amd)


sensspec_estimates_earlyint_amd_subset <- earlyint_amd_subset %>%
  filter(!is.na(camera)) %>%
  group_by(camera) %>%
  class_metrics(., truth = amd, estimate = amd_final) %>%
  select(-.estimator) %>%
  spread(.metric, .estimate, convert=TRUE) %>%
  rename(sens_est=sens,
         spec_est=spec,
         npv_est=npv,
         ppv_est=ppv)
# Check
xtabs(data=filter(earlyint_amd_subset, camera=="peek"), ~amd+amd_final)


## bootstrapped 95% CI accounting for clustering of eyes 
bs_sensspec_earlyint_amd_subset <- map(bs$splits, ~as_tibble(.) %>% unnest %>% 
                                         filter(!is.na(camera)) %>%
                                         group_by(camera) %>% 
                                         class_metrics(., truth = as.factor(amd), estimate = as.factor(amd_final))) %>% 
  bind_rows(.id = 'boots') %>%
  select(-.estimator) %>%
  spread(.metric, .estimate, convert=TRUE) %>%
  group_by(camera) %>%
  summarize_at(vars(sens, spec, ppv, npv), lst(!!!p_funs))


# Now merge together the estimates and CIs and reshape into more useful format
sensspectable_earlyint_amd_subset <- full_join(sensspec_estimates_earlyint_amd_subset, bs_sensspec_earlyint_amd_subset, by="camera")
sensspectablelong_earlyint_amd_subset <- sensspectable_earlyint_amd_subset %>%
  gather(field, value, npv_est:npv_upper95) %>%
  separate(field, into=c("metric", "stat"), sep="_") %>%
  spread(stat, value, convert=TRUE) %>%
  write.csv("senspectablelong_earlyint_amd_subset.csv")



# no use calculating SN screening for mild/moderate glaucoma bc only 3 cases of moderate and 0 cases of mild




####### Table 1 #####

# filter data → one eye per line
eyes_data <- alldata_final %>%
  filter(camera == "pictor")

#filter data → one patient per line
pt_data <- alldata_final %>%
  group_by(study_id) %>%
  mutate(max_photoid = max(photoid, na.rm = TRUE)) %>%
  filter(max_photoid == photoid)

# number of unique EYES
summary(unique(eyes_data$studyid_eye))

# number of unique PATIENTS
summary(unique(pt_data$study_id))


# age: median 
summary(pt_data$age)

# gender (1 = female; 0 = male)
sum(pt_data$gender, na.rm = TRUE)

# number of pts with cataracts
sum(pt_data$cataract == 1 | pt_data$cataract == 2 | pt_data$cataract == 3 | pt_data$cataract == 4, na.rm = TRUE)
#NS cataract
sum(pt_data$cataract == 1, na.rm = TRUE)
#cortical cataract
sum(pt_data$cataract == 2, na.rm = TRUE)
#PSC cataract
sum(pt_data$cataract == 3, na.rm = TRUE)
#unspec cataract
sum(pt_data$cataract == 4 | pt_data$cataract == 5, na.rm = TRUE)
#degree of cataract
sum(pt_data$cataract_deg == 1, na.rm = TRUE)
sum(pt_data$cataract_deg == 2, na.rm = TRUE)
sum(pt_data$cataract_deg == 3, na.rm = TRUE)
sum(pt_data$cataract_deg == 4, na.rm = TRUE)
sum(pt_data$cataract_deg == 5, na.rm = TRUE)


#camera-specific info
#peek time & discomfort
peek_only <- alldata_final %>%
  filter(camera == "peek")
summary(peek_only$seconds)
summary(peek_only$discomfort)
#inview time & discomfort
inview_only <- alldata_final %>%
  filter(camera == "inview")
summary(inview_only$seconds)
summary(inview_only$discomfort)
#pictor time & discomfort
pictor_only <- alldata_final %>%
  filter(camera == "pictor")
summary(pictor_only$seconds)
summary(pictor_only$discomfort)

#image_clarity...number of eyes with:
#...excellent clarity
sum(peek_only$image_clarity_final == 4, na.rm = TRUE)
sum(inview_only$image_clarity_final == 4, na.rm = TRUE)
sum(pictor_only$image_clarity_final == 4, na.rm = TRUE)
#...good clarity
sum(peek_only$image_clarity_final == 3, na.rm = TRUE)
sum(inview_only$image_clarity_final == 3, na.rm = TRUE)
sum(pictor_only$image_clarity_final == 3, na.rm = TRUE)
#...fair clarity
sum(peek_only$image_clarity_final == 2, na.rm = TRUE)
sum(inview_only$image_clarity_final == 2, na.rm = TRUE)
sum(pictor_only$image_clarity_final == 2, na.rm = TRUE)
#...poor clarity
sum(peek_only$image_clarity_final == 1, na.rm = TRUE)
sum(inview_only$image_clarity_final == 1, na.rm = TRUE)
sum(pictor_only$image_clarity_final == 1, na.rm = TRUE)


#photo quality...coverage of the optic nerve
#fully vis
sum(peek_only$nerve_final == 2, na.rm = TRUE)
sum(inview_only$nerve_final == 2, na.rm = TRUE)
sum(pictor_only$nerve_final == 2, na.rm = TRUE)
#partly vis
sum(peek_only$nerve_final == 1, na.rm = TRUE)
sum(inview_only$nerve_final == 1, na.rm = TRUE)
sum(pictor_only$nerve_final == 1, na.rm = TRUE)
#absent
sum(peek_only$nerve_final == 0, na.rm = TRUE)
sum(inview_only$nerve_final == 0, na.rm = TRUE)
sum(pictor_only$nerve_final == 0, na.rm = TRUE)


#photo quality...coverage of the macula
#fully vis
sum(peek_only$macula_final == 2, na.rm = TRUE)
sum(inview_only$macula_final == 2, na.rm = TRUE)
sum(pictor_only$macula_final == 2, na.rm = TRUE)
#partly vis
sum(peek_only$macula_final == 1, na.rm = TRUE)
sum(inview_only$macula_final == 1, na.rm = TRUE)
sum(pictor_only$macula_final == 1, na.rm = TRUE)
#absent
sum(peek_only$macula_final == 0, na.rm = TRUE)
sum(inview_only$macula_final == 0, na.rm = TRUE)
sum(pictor_only$macula_final == 0, na.rm = TRUE)


#photo quality...coverage of zone 2
#fully vis
sum(peek_only$zone2_final == 2, na.rm = TRUE)
sum(inview_only$zone2_final == 2, na.rm = TRUE)
sum(pictor_only$zone2_final == 2, na.rm = TRUE)
#partly vis
sum(peek_only$zone2_final == 1, na.rm = TRUE)
sum(inview_only$zone2_final == 1, na.rm = TRUE)
sum(pictor_only$zone2_final == 1, na.rm = TRUE)
#absent
sum(peek_only$zone2_final == 0, na.rm = TRUE)
sum(inview_only$zone2_final == 0, na.rm = TRUE)
sum(pictor_only$zone2_final == 0, na.rm = TRUE)


#confidence in grading vcdr 
#able, confident
sum(peek_only$vcdr_confidence_final == 1, na.rm = TRUE)
sum(inview_only$vcdr_confidence_final == 1, na.rm = TRUE)
sum(pictor_only$vcdr_confidence_final == 1, na.rm = TRUE)
#able, NOT confident
sum(peek_only$vcdr_confidence_final == 2, na.rm = TRUE)
sum(inview_only$vcdr_confidence_final == 2, na.rm = TRUE)
sum(pictor_only$vcdr_confidence_final == 2, na.rm = TRUE)
#unable to grade
sum(peek_only$vcdr_confidence_final == 3, na.rm = TRUE)
sum(inview_only$vcdr_confidence_final == 3, na.rm = TRUE)
sum(pictor_only$vcdr_confidence_final == 3, na.rm = TRUE)


#total eyes with DR
eyes_data <- eyes_data %>%
  mutate(dr=as.numeric(dr))
eyes_data$dr[eyes_data$dr == 1] <- 0
eyes_data$dr[eyes_data$dr == 2] <- 1
sum(eyes_data$dr, na.rm = TRUE)

#eyes with mild or moderate NPDR
sum(eyes_data$diagnosis_dr == 1, na.rm = TRUE)
#eyes with severe NPDR
sum(eyes_data$diagnosis_dr == 2, na.rm = TRUE)
#eyes with PDR
sum(eyes_data$diagnosis_dr == 3, na.rm = TRUE)
#eyes with unspecified DR
sum(eyes_data$diagnosis_dr == 4, na.rm = TRUE)

#eyes with DME
sum(eyes_data$dme, na.rm = TRUE)


#total eyes with AMD
eyes_data <- eyes_data %>%
  mutate(amd=as.numeric(amd))
eyes_data$amd[eyes_data$amd == 1] <- 0
eyes_data$amd[eyes_data$amd == 2] <- 1
sum(eyes_data$amd, na.rm = TRUE)

#eyes with early amd
sum(eyes_data$diagnosis_amd == 1, na.rm = TRUE)
#eyes with intermed amd
sum(eyes_data$diagnosis_amd == 2, na.rm = TRUE)
#eyes with advanced amd
sum(eyes_data$diagnosis_amd == 3, na.rm = TRUE)
#eyes with unspec amd
sum(eyes_data$diagnosis_amd == 4, na.rm = TRUE)


#total eyes with glaucoma
eyes_data <- eyes_data %>%
  mutate(glaucoma=as.numeric(glaucoma))
eyes_data$glaucoma[eyes_data$glaucoma == 1] <- 0
eyes_data$glaucoma[eyes_data$glaucoma == 2] <- 1
sum(eyes_data$glaucoma, na.rm = TRUE)

#eyes with mild/early glauc
sum(eyes_data$diagnosis_glaucoma == 1, na.rm = TRUE)
#eyes with mod glauc
sum(eyes_data$diagnosis_glaucoma == 2, na.rm = TRUE)
#eyes with severe glauc
sum(eyes_data$diagnosis_glaucoma == 3, na.rm = TRUE)
#eyes with unspec glauc
sum(eyes_data$diagnosis_glaucoma == 4, na.rm = TRUE)
#eyes with NA info on glauc
sum(eyes_data$diagnosis_glaucoma == 5, na.rm = TRUE)


#total eyes with other dx
eyes_data <- eyes_data %>%
  mutate(otherdx=as.numeric(otherdx))
eyes_data$otherdx[eyes_data$otherdx == 1] <- 0
eyes_data$otherdx[eyes_data$otherdx == 2] <- 1
sum(eyes_data$otherdx, na.rm = TRUE)

list_otherdx <- eyes_data %>% 
  select(other_retinal_path) %>%
  filter(!(is.na(other_retinal_path))) %>%
  write.csv("list_otherdx.csv")














#################################################################################################
#################################################################################################
#################################################################################################
#################################################################################################
#################################################################################################
# PHOTO ADJUDICATION FOR TEAM 1

team1_discrep <- team1 %>%
  filter((amd_yesno.1!=amd_yesno.2) | (dr_yesno.1!=dr_yesno.2) | (abs(cdr.1-cdr.2)>.2)) %>%
  select(id, amd_yesno.1, amd_yesno.2, dr_yesno.1, dr_yesno.2, cdr.1, cdr.2) %>%
  write.csv("team1_discrep.csv")


# PHOTO ADJUDICATION FOR TEAM 2

team2_import <- read_csv("Team2_DATA_2019-05-13_0820.csv")

# CONFIRM LEVELS OF AMD AND DR:
xtabs(data=team2_import, ~dr+amd, addNA=TRUE)

team2 <- team2_import %>%
  mutate(dr_yesno = ifelse((dr == 0 | dr == 1), 0, 1)) %>%
  mutate(amd_yesno = ifelse((amd == 0 | amd == 1), 0, 1)) %>%
  select(study_id, dr_yesno, dr, amd_yesno, amd, amd_severity, cdr) %>%
  separate(study_id, c("id", "grader"), sep = "--", remove = TRUE, convert = FALSE) %>%
  gather(field, value, dr_yesno:cdr) %>%
  mutate(field.grader=paste(field, grader, sep=".")) %>%
  select(-field, -grader) %>%
  spread(field.grader, value, convert=TRUE)

xtabs(data=team2, ~dr_yesno.1+dr_yesno.2, addNA=TRUE)
xtabs(data=team2, ~amd_yesno.1+amd_yesno.2, addNA=TRUE)
xtabs(data=filter(team2, amd_yesno.1==1 & amd_yesno.2==0), ~amd_severity.1)

team2_discrep <- team2 %>%
  filter((amd_yesno.1!=amd_yesno.2) | (dr_yesno.1!=dr_yesno.2) | (abs(cdr.1-cdr.2)>.2)) %>%
  select(id, amd_yesno.1, amd_yesno.2, dr_yesno.1, dr_yesno.2, cdr.1, cdr.2) %>%
  write.csv("team2_discrep.csv")


# PHOTO ADJUDICATION FOR TEAM 3

team3_import <- read_csv("Team3_DATA_2019-06-23_1055.csv")

# CONFIRM LEVELS OF AMD AND DR:
xtabs(data=team3_import, ~dr+amd, addNA=TRUE)

team3 <- team3_import %>%
  mutate(dr_yesno = ifelse((dr == 0 | dr == 1), 0, 1)) %>%
  mutate(amd_yesno = ifelse((amd == 0 | amd == 1), 0, 1)) %>%
  select(study_id, dr_yesno, dr, amd_yesno, amd, amd_severity, cdr) %>%
  separate(study_id, c("id", "grader"), sep = "--", remove = TRUE, convert = FALSE) %>%
  gather(field, value, dr_yesno:cdr) %>%
  mutate(field.grader=paste(field, grader, sep=".")) %>%
  select(-field, -grader) %>%
  spread(field.grader, value, convert=TRUE)

xtabs(data=team3, ~dr_yesno.1+dr_yesno.2, addNA=TRUE)
xtabs(data=team3, ~amd_yesno.1+amd_yesno.2, addNA=TRUE)
xtabs(data=filter(team3, amd_yesno.1==1 & amd_yesno.2==0), ~amd_severity.1)

team3_discrep <- team3 %>%
  filter((amd_yesno.1!=amd_yesno.2) | (dr_yesno.1!=dr_yesno.2) | (abs(cdr.1-cdr.2)>.2)) %>%
  select(id, amd_yesno.1, amd_yesno.2, dr_yesno.1, dr_yesno.2, cdr.1, cdr.2) %>%
  write.csv("team3_discrep.csv")


# PHOTO ADJUDICATION FOR TEAM 4

team4_import <- read_csv("Team4_DATA_2019-06-23_1056.csv")

# CONFIRM LEVELS OF AMD AND DR:
xtabs(data=team4_import, ~dr+amd, addNA=TRUE)

team4 <- team4_import %>%
  mutate(dr_yesno = ifelse((dr == 0 | dr == 1), 0, 1)) %>%
  mutate(amd_yesno = ifelse((amd == 0 | amd == 1), 0, 1)) %>%
  select(study_id, dr_yesno, dr, amd_yesno, amd, amd_severity, cdr) %>%
  separate(study_id, c("id", "grader"), sep = "--", remove = TRUE, convert = FALSE) %>%
  gather(field, value, dr_yesno:cdr) %>%
  mutate(field.grader=paste(field, grader, sep=".")) %>%
  select(-field, -grader) %>%
  spread(field.grader, value, convert=TRUE)

xtabs(data=team4, ~dr_yesno.1+dr_yesno.2, addNA=TRUE)
xtabs(data=team4, ~amd_yesno.1+amd_yesno.2, addNA=TRUE)
xtabs(data=filter(team4, amd_yesno.1==1 & amd_yesno.2==0), ~amd_severity.1)

team4_discrep <- team4 %>%
  filter((amd_yesno.1!=amd_yesno.2) | (dr_yesno.1!=dr_yesno.2) | (abs(cdr.1-cdr.2)>.2)) %>%
  select(id, amd_yesno.1, amd_yesno.2, dr_yesno.1, dr_yesno.2, cdr.1, cdr.2) %>%
  write.csv("team4_discrep.csv")


#TEAM 1: replace discrepant grading with adjudicated grades

# import adjudication dataset -> adj_team1
adj_team1 <- read_csv("Team1_Adjudic_2019-07-12_1539.csv")

# rename record_id column to "study_id", then add "--3"
colnames(adj_team1)[1] <- "study_id"
adj_team1 <- adj_team1[,-2]
adj_team1$study_id <- paste0(adj_team1$study_id, "--3")

# merge team1_import with adj_team 1
final_team1 <- rbind(team1_import, adj_team1)

final_team1 <- final_team1 %>%
  separate(study_id, c("id", "grader"), sep = "--", remove = TRUE, convert = FALSE) %>%
  gather(field, value, image_clarity:notes) %>%
  mutate(field.grader=paste(field, grader, sep=".")) %>%
  select(-field, -grader) %>%
  spread(field.grader, value, convert=TRUE)

#remove column "new_photo_grading_form_complete"
final_team1 <- final_team1[,-2]


# if variable.3 != NA, then variable.final = variable.3. if variable.3 = NA, then variable.final = variable.1
final_team1 <- final_team1 %>%
  mutate(amd_severity = if_else(is.na(amd_severity.3)==FALSE, amd_severity.3, amd_severity.1),
         amd = if_else(is.na(amd.3)==FALSE, amd.3, amd.1),
         cdr = if_else(is.na(cdr.3)==FALSE, cdr.3, cdr.1),
         dme = if_else(is.na(dme.3)==FALSE, dme.3, dme.1),
         dr_severity = if_else(is.na(dr_severity.3)==FALSE, dr_severity.3, dr_severity.1),
         dr = if_else(is.na(dr.3)==FALSE, dr.3, dr.1),
         image_clarity = if_else(is.na(image_clarity.3)==FALSE, image_clarity.3, image_clarity.1),
         macula = if_else(is.na(macula.3)==FALSE, macula.3, macula.1),
         nerve = if_else(is.na(nerve.3)==FALSE, nerve.3, nerve.1),
         notes = if_else(is.na(notes.3)==FALSE, notes.3, notes.1),
         other_dx_detail = if_else(is.na(other_dx_detail.3)==FALSE, other_dx_detail.3, other_dx_detail.1),
         other_dx = if_else(is.na(other_dx.3)==FALSE, other_dx.3, other_dx.1),
         vcdr_confidence = if_else(is.na(vcdr_confidence.3)==FALSE, vcdr_confidence.3, vcdr_confidence.1),
         zone2 = if_else(is.na(zone2.3)==FALSE, zone2.3, zone2.1))

final_team1 <- final_team1 %>%
  select(id,amd_severity:zone2)


#TEAM 2: replace discrepant grading with adjudicated grades

# import adjudication dataset -> adj_team2
adj_team2 <- read_csv("Team2_Adjudic_2019-07-15_1702.csv")

# rename record_id column to "study_id", then add "--3"
colnames(adj_team2)[1] <- "study_id"
adj_team2 <- adj_team2[,-2]
adj_team2$study_id <- paste0(adj_team2$study_id, "--3")

# merge team1_import with adj_team 1
final_team2 <- rbind(team2_import, adj_team2)

final_team2 <- final_team2 %>%
  separate(study_id, c("id", "grader"), sep = "--", remove = TRUE, convert = FALSE) %>%
  gather(field, value, image_clarity:notes) %>%
  mutate(field.grader=paste(field, grader, sep=".")) %>%
  select(-field, -grader) %>%
  spread(field.grader, value, convert=TRUE)

#remove column "new_photo_grading_form_complete"
final_team2 <- final_team2[,-2]


# if variable.3 != NA, then variable.final = variable.3. if variable.3 = NA, then variable.final = variable.1
final_team2 <- final_team2 %>%
  mutate(amd_severity = if_else(is.na(amd_severity.3)==FALSE, amd_severity.3, amd_severity.1),
         amd = if_else(is.na(amd.3)==FALSE, amd.3, amd.1),
         cdr = if_else(is.na(cdr.3)==FALSE, cdr.3, cdr.1),
         dme = if_else(is.na(dme.3)==FALSE, dme.3, dme.1),
         dr_severity = if_else(is.na(dr_severity.3)==FALSE, dr_severity.3, dr_severity.1),
         dr = if_else(is.na(dr.3)==FALSE, dr.3, dr.1),
         image_clarity = if_else(is.na(image_clarity.3)==FALSE, image_clarity.3, image_clarity.1),
         macula = if_else(is.na(macula.3)==FALSE, macula.3, macula.1),
         nerve = if_else(is.na(nerve.3)==FALSE, nerve.3, nerve.1),
         notes = if_else(is.na(notes.3)==FALSE, notes.3, notes.1),
         other_dx_detail = if_else(is.na(other_dx_detail.3)==FALSE, other_dx_detail.3, other_dx_detail.1),
         other_dx = if_else(is.na(other_dx.3)==FALSE, other_dx.3, other_dx.1),
         vcdr_confidence = if_else(is.na(vcdr_confidence.3)==FALSE, vcdr_confidence.3, vcdr_confidence.1),
         zone2 = if_else(is.na(zone2.3)==FALSE, zone2.3, zone2.1))

final_team2 <- final_team2 %>%
  select(id,amd_severity:zone2)



#TEAM 3: replace discrepant grading with adjudicated grades

# import adjudication dataset -> adj_team3
adj_team3 <- read_csv("Team3_Adjudic_2019-07-17_1624.csv")

# rename record_id column to "study_id", then add "--3"
colnames(adj_team3)[1] <- "study_id"
adj_team3 <- adj_team3[,-2]
adj_team3$study_id <- paste0(adj_team3$study_id, "--3")

# merge team3_import with adj_team 1
final_team3 <- rbind(team3_import, adj_team3)

final_team3 <- final_team3 %>%
  separate(study_id, c("id", "grader"), sep = "--", remove = TRUE, convert = FALSE) %>%
  gather(field, value, image_clarity:notes) %>%
  mutate(field.grader=paste(field, grader, sep=".")) %>%
  select(-field, -grader) %>%
  spread(field.grader, value, convert=TRUE)

#remove column "new_photo_grading_form_complete"
final_team3 <- final_team3[,-2]


# if variable.3 != NA, then variable.final = variable.3. if variable.3 = NA, then variable.final = variable.1
final_team3 <- final_team3 %>%
  mutate(amd_severity = if_else(is.na(amd_severity.3)==FALSE, amd_severity.3, amd_severity.1),
         amd = if_else(is.na(amd.3)==FALSE, amd.3, amd.1),
         cdr = if_else(is.na(cdr.3)==FALSE, cdr.3, cdr.1),
         dme = if_else(is.na(dme.3)==FALSE, dme.3, dme.1),
         dr_severity = if_else(is.na(dr_severity.3)==FALSE, dr_severity.3, dr_severity.1),
         dr = if_else(is.na(dr.3)==FALSE, dr.3, dr.1),
         image_clarity = if_else(is.na(image_clarity.3)==FALSE, image_clarity.3, image_clarity.1),
         macula = if_else(is.na(macula.3)==FALSE, macula.3, macula.1),
         nerve = if_else(is.na(nerve.3)==FALSE, nerve.3, nerve.1),
         notes = if_else(is.na(notes.3)==FALSE, notes.3, notes.1),
         other_dx_detail = if_else(is.na(other_dx_detail.3)==FALSE, other_dx_detail.3, other_dx_detail.1),
         other_dx = if_else(is.na(other_dx.3)==FALSE, other_dx.3, other_dx.1),
         vcdr_confidence = if_else(is.na(vcdr_confidence.3)==FALSE, vcdr_confidence.3, vcdr_confidence.1),
         zone2 = if_else(is.na(zone2.3)==FALSE, zone2.3, zone2.1))

final_team3 <- final_team3 %>%
  select(id,amd_severity:zone2)




#sensitivity for DR
alldata_final <- alldata_final %>%
  mutate(dr_true_pos_peek = ifelse(((dr_final==1) & (dr==1)) & camera=="peek", 1, 0)) %>%
  mutate(dr_false_neg_peek = ifelse(((dr_final==0) & (dr==1)) & camera=="peek", 1, 0)) %>%  
  mutate(dr_true_pos_inview = ifelse(((dr_final==1) & (dr==1)) & camera=="inview", 1, 0)) %>%
  mutate(dr_false_neg_inview = ifelse(((dr_final==0) & (dr==1)) & camera=="inview", 1, 0)) %>%
  mutate(dr_true_pos_pictor = ifelse(((dr_final==1) & (dr==1)) & camera=="pictor", 1, 0)) %>%
  mutate(dr_false_neg_pictor = ifelse(((dr_final==0) & (dr==1)) & camera=="pictor", 1, 0))

sensitivity_dr_peek <- sum(alldata_final$dr_true_pos_peek, na.rm=TRUE)/(sum(alldata_final$dr_true_pos_peek, na.rm=TRUE)+sum(alldata_final$dr_false_neg_peek, na.rm=TRUE))
sensitivity_dr_inview <- sum(alldata_final$dr_true_pos_inview, na.rm=TRUE)/(sum(alldata_final$dr_true_pos_inview, na.rm=TRUE)+sum(alldata_final$dr_false_neg_inview, na.rm=TRUE))
sensitivity_dr_pictor <- sum(alldata_final$dr_true_pos_pictor, na.rm=TRUE)/(sum(alldata_final$dr_true_pos_pictor, na.rm=TRUE)+sum(alldata_final$dr_false_neg_pictor, na.rm=TRUE))

sensitivity_dr_peek
sensitivity_dr_inview
sensitivity_dr_pictor


#sensitivity for DME
alldata <- alldata %>%
  mutate(dme_true_pos_peek = ifelse(((dme_right==1 | dme_left==1) & (dme==2|dme==3)) & camera=="peek", 1, 0)) %>%
  mutate(dme_false_neg_peek = ifelse(((dme_right==1 | dme_left==1) & (dme==0|dme==1)) & camera=="peek", 1, 0)) %>%  
  mutate(dme_true_pos_inview = ifelse(((dme_right==1 | dme_left==1) & (dme==2|dme==3)) & camera=="inview", 1, 0)) %>%
  mutate(dme_false_neg_inview = ifelse(((dme_right==1 | dme_left==1) & (dme==0|dme==1)) & camera=="inview", 1, 0)) %>%
  mutate(dme_true_pos_pictor = ifelse(((dme_right==1 | dme_left==1) & (dme==2|dme==3)) & camera=="pictor", 1, 0)) %>%
  mutate(dme_false_neg_pictor = ifelse(((dme_right==1 | dme_left==1) & (dme==0|dme==1)) & camera=="pictor", 1, 0))

sensitivity_dme_peek <- sum(alldata$dme_true_pos_peek, na.rm=TRUE)/(sum(alldata$dme_true_pos_peek, na.rm=TRUE)+sum(alldata$dme_false_neg_peek, na.rm=TRUE))
sensitivity_dme_inview <- sum(alldata$dme_true_pos_inview, na.rm=TRUE)/(sum(alldata$dme_true_pos_inview, na.rm=TRUE)+sum(alldata$dme_false_neg_inview, na.rm=TRUE))
sensitivity_dme_pictor <- sum(alldata$dme_true_pos_pictor, na.rm=TRUE)/(sum(alldata$dme_true_pos_pictor, na.rm=TRUE)+sum(alldata$dme_false_neg_pictor, na.rm=TRUE))

sensitivity_dme_peek
sensitivity_dme_inview
sensitivity_dme_pictor

#sensitivity for amd
alldata <- alldata %>%
  mutate(amd_true_pos_peek = ifelse(((amd_right==1 | amd_left==1) & (amd==2|amd==3)) & camera=="peek", 1, 0)) %>%
  mutate(amd_false_neg_peek = ifelse(((amd_right==1 | amd_left==1) & (amd==0|amd==1)) & camera=="peek", 1, 0)) %>%  
  mutate(amd_true_pos_inview = ifelse(((amd_right==1 | amd_left==1) & (amd==2|amd==3)) & camera=="inview", 1, 0)) %>%
  mutate(amd_false_neg_inview = ifelse(((amd_right==1 | amd_left==1) & (amd==0|amd==1)) & camera=="inview", 1, 0)) %>%
  mutate(amd_true_pos_pictor = ifelse(((amd_right==1 | amd_left==1) & (amd==2|amd==3)) & camera=="pictor", 1, 0)) %>%
  mutate(amd_false_neg_pictor = ifelse(((amd_right==1 | amd_left==1) & (amd==0|amd==1)) & camera=="pictor", 1, 0))

sensitivity_amd_peek <- sum(alldata$amd_true_pos_peek, na.rm=TRUE)/(sum(alldata$amd_true_pos_peek, na.rm=TRUE)+sum(alldata$amd_false_neg_peek, na.rm=TRUE))
sensitivity_amd_inview <- sum(alldata$amd_true_pos_inview, na.rm=TRUE)/(sum(alldata$amd_true_pos_inview, na.rm=TRUE)+sum(alldata$amd_false_neg_inview, na.rm=TRUE))
sensitivity_amd_pictor <- sum(alldata$amd_true_pos_pictor, na.rm=TRUE)/(sum(alldata$amd_true_pos_pictor, na.rm=TRUE)+sum(alldata$amd_false_neg_pictor, na.rm=TRUE))

sensitivity_amd_peek
sensitivity_amd_inview
sensitivity_amd_pictor

#sensitivity for glaucoma
alldata <- alldata %>%
  mutate(glaucoma_true_pos_peek = ifelse((glaucoma_right==1 | glaucoma_left==1) & cdr>=0.7 & camera=="peek", 1, 0)) %>%
  mutate(glaucoma_false_neg_peek = ifelse((glaucoma_right==1 | glaucoma_left==1) & cdr<0.7 & camera=="peek", 1, 0)) %>%  
  mutate(glaucoma_true_pos_inview = ifelse((glaucoma_right==1 | glaucoma_left==1) & cdr>=0.7 & camera=="inview", 1, 0)) %>%
  mutate(glaucoma_false_neg_inview = ifelse((glaucoma_right==1 | glaucoma_left==1) & cdr<0.7 & camera=="inview", 1, 0)) %>%
  mutate(glaucoma_true_pos_pictor = ifelse((glaucoma_right==1 | glaucoma_left==1) & cdr>=0.7 & camera=="pictor", 1, 0)) %>%
  mutate(glaucoma_false_neg_pictor = ifelse((glaucoma_right==1 | glaucoma_left==1) & cdr<0.7 & camera=="pictor", 1, 0))

sensitivity_glaucoma_peek <- sum(alldata$glaucoma_true_pos_peek, na.rm=TRUE)/(sum(alldata$glaucoma_true_pos_peek, na.rm=TRUE)+sum(alldata$glaucoma_false_neg_peek, na.rm=TRUE))
sensitivity_glaucoma_inview <- sum(alldata$glaucoma_true_pos_inview, na.rm=TRUE)/(sum(alldata$glaucoma_true_pos_inview, na.rm=TRUE)+sum(alldata$glaucoma_false_neg_inview, na.rm=TRUE))
sensitivity_glaucoma_pictor <- sum(alldata$glaucoma_true_pos_pictor, na.rm=TRUE)/(sum(alldata$glaucoma_true_pos_pictor, na.rm=TRUE)+sum(alldata$glaucoma_false_neg_pictor, na.rm=TRUE))

sensitivity_glaucoma_peek
sensitivity_glaucoma_inview
sensitivity_glaucoma_pictor

#sensitivity for other dx
alldata <- alldata %>%
  mutate(otherdx_true_pos_peek = ifelse(((otherdx_right==1 | otherdx_left==1) & (other_dx==2|other_dx==3)) & camera=="peek", 1, 0)) %>%
  mutate(otherdx_false_neg_peek = ifelse(((otherdx_right==1 | otherdx_left==1) & (other_dx==0|other_dx==1)) & camera=="peek", 1, 0)) %>%  
  mutate(otherdx_true_pos_inview = ifelse(((otherdx_right==1 | otherdx_left==1) & (other_dx==2|other_dx==3)) & camera=="inview", 1, 0)) %>%
  mutate(otherdx_false_neg_inview = ifelse(((otherdx_right==1 | otherdx_left==1) & (other_dx==0|other_dx==1)) & camera=="inview", 1, 0)) %>%
  mutate(otherdx_true_pos_pictor = ifelse(((otherdx_right==1 | otherdx_left==1) & (other_dx==2|other_dx==3)) & camera=="pictor", 1, 0)) %>%
  mutate(otherdx_false_neg_pictor = ifelse(((otherdx_right==1 | otherdx_left==1) & (other_dx==0|other_dx==1)) & camera=="pictor", 1, 0))

sensitivity_otherdx_peek <- sum(alldata$otherdx_true_pos_peek, na.rm=TRUE)/(sum(alldata$otherdx_true_pos_peek, na.rm=TRUE)+sum(alldata$otherdx_false_neg_peek, na.rm=TRUE))
sensitivity_otherdx_inview <- sum(alldata$otherdx_true_pos_inview, na.rm=TRUE)/(sum(alldata$otherdx_true_pos_inview, na.rm=TRUE)+sum(alldata$otherdx_false_neg_inview, na.rm=TRUE))
sensitivity_otherdx_pictor <- sum(alldata$otherdx_true_pos_pictor, na.rm=TRUE)/(sum(alldata$otherdx_true_pos_pictor, na.rm=TRUE)+sum(alldata$otherdx_false_neg_pictor, na.rm=TRUE))

sensitivity_otherdx_peek
sensitivity_otherdx_inview
sensitivity_otherdx_pictor


#specificity for DR
alldata <- alldata %>%
  mutate(dr_true_neg_peek = ifelse(((dr_right==0 | dr_left==0) & (dr==0|dr==1)) & camera=="peek", 1, 0)) %>%
  mutate(dr_false_pos_peek = ifelse(((dr_right==0 | dr_left==0) & (dr==2|dr==3)) & camera=="peek", 1, 0)) %>%  
  mutate(dr_true_neg_inview = ifelse(((dr_right==0 | dr_left==0) & (dr==0|dr==1)) & camera=="inview", 1, 0)) %>%
  mutate(dr_false_pos_inview = ifelse(((dr_right==0 | dr_left==0) & (dr==2|dr==3)) & camera=="inview", 1, 0)) %>%
  mutate(dr_true_neg_pictor = ifelse(((dr_right==0 | dr_left==0) & (dr==0|dr==1)) & camera=="pictor", 1, 0)) %>%
  mutate(dr_false_pos_pictor = ifelse(((dr_right==0 | dr_left==0) & (dr==2|dr==3)) & camera=="pictor", 1, 0))

specificity_dr_peek <- sum(alldata$dr_true_neg_peek, na.rm=TRUE)/(sum(alldata$dr_true_neg_peek, na.rm=TRUE)+sum(alldata$dr_false_pos_peek, na.rm=TRUE))
specificity_dr_inview <- sum(alldata$dr_true_neg_inview, na.rm=TRUE)/(sum(alldata$dr_true_neg_inview, na.rm=TRUE)+sum(alldata$dr_false_pos_inview, na.rm=TRUE))
specificity_dr_pictor <- sum(alldata$dr_true_neg_pictor, na.rm=TRUE)/(sum(alldata$dr_true_neg_pictor, na.rm=TRUE)+sum(alldata$dr_false_pos_pictor, na.rm=TRUE))

specificity_dr_peek
specificity_dr_inview
specificity_dr_pictor


#specificity for DME
alldata <- alldata %>%
  mutate(dme_true_neg_peek = ifelse(((dme_right==0 | dme_left==0) & (dme==0|dme==1)) & camera=="peek", 1, 0)) %>%
  mutate(dme_false_pos_peek = ifelse(((dme_right==0 | dme_left==0) & (dme==2|dme==3)) & camera=="peek", 1, 0)) %>%  
  mutate(dme_true_neg_inview = ifelse(((dme_right==0 | dme_left==0) & (dme==0|dme==1)) & camera=="inview", 1, 0)) %>%
  mutate(dme_false_pos_inview = ifelse(((dme_right==0 | dme_left==0) & (dme==2|dme==3)) & camera=="inview", 1, 0)) %>%
  mutate(dme_true_neg_pictor = ifelse(((dme_right==0 | dme_left==0) & (dme==0|dme==1)) & camera=="pictor", 1, 0)) %>%
  mutate(dme_false_pos_pictor = ifelse(((dme_right==0 | dme_left==0) & (dme==2|dme==3)) & camera=="pictor", 1, 0))

specificity_dme_peek <- sum(alldata$dme_true_neg_peek, na.rm=TRUE)/(sum(alldata$dme_true_neg_peek, na.rm=TRUE)+sum(alldata$dme_false_pos_peek, na.rm=TRUE))
specificity_dme_inview <- sum(alldata$dme_true_neg_inview, na.rm=TRUE)/(sum(alldata$dme_true_neg_inview, na.rm=TRUE)+sum(alldata$dme_false_pos_inview, na.rm=TRUE))
specificity_dme_pictor <- sum(alldata$dme_true_neg_pictor, na.rm=TRUE)/(sum(alldata$dme_true_neg_pictor, na.rm=TRUE)+sum(alldata$dme_false_pos_pictor, na.rm=TRUE))

specificity_dme_peek
specificity_dme_inview
specificity_dme_pictor


#specificity for AMD
alldata <- alldata %>%
  mutate(amd_true_neg_peek = ifelse(((amd_right==0 | amd_left==0) & (amd==0|amd==1)) & camera=="peek", 1, 0)) %>%
  mutate(amd_false_pos_peek = ifelse(((amd_right==0 | amd_left==0) & (amd==2|amd==3)) & camera=="peek", 1, 0)) %>%  
  mutate(amd_true_neg_inview = ifelse(((amd_right==0 | amd_left==0) & (amd==0|amd==1)) & camera=="inview", 1, 0)) %>%
  mutate(amd_false_pos_inview = ifelse(((amd_right==0 | amd_left==0) & (amd==2|amd==3)) & camera=="inview", 1, 0)) %>%
  mutate(amd_true_neg_pictor = ifelse(((amd_right==0 | amd_left==0) & (amd==0|amd==1)) & camera=="pictor", 1, 0)) %>%
  mutate(amd_false_pos_pictor = ifelse(((amd_right==0 | amd_left==0) & (amd==2|amd==3)) & camera=="pictor", 1, 0))

specificity_amd_peek <- sum(alldata$amd_true_neg_peek, na.rm=TRUE)/(sum(alldata$amd_true_neg_peek, na.rm=TRUE)+sum(alldata$amd_false_pos_peek, na.rm=TRUE))
specificity_amd_inview <- sum(alldata$amd_true_neg_inview, na.rm=TRUE)/(sum(alldata$amd_true_neg_inview, na.rm=TRUE)+sum(alldata$amd_false_pos_inview, na.rm=TRUE))
specificity_amd_pictor <- sum(alldata$amd_true_neg_pictor, na.rm=TRUE)/(sum(alldata$amd_true_neg_pictor, na.rm=TRUE)+sum(alldata$amd_false_pos_pictor, na.rm=TRUE))

specificity_amd_peek
specificity_amd_inview
specificity_amd_pictor


#specificity for glaucoma
alldata <- alldata %>%
  mutate(glaucoma_true_neg_peek = ifelse((glaucoma_right==0 | glaucoma_left==0) & cdr<0.7 & camera=="peek", 1, 0)) %>%
  mutate(glaucoma_false_pos_peek = ifelse((glaucoma_right==0 | glaucoma_left==0) & cdr>=0.7 & camera=="peek", 1, 0)) %>%  
  mutate(glaucoma_true_neg_inview = ifelse((glaucoma_right==0 | glaucoma_left==0) & cdr<0.7 & camera=="inview", 1, 0)) %>%
  mutate(glaucoma_false_pos_inview = ifelse((glaucoma_right==0 | glaucoma_left==0) & cdr>=0.7 & camera=="inview", 1, 0)) %>%
  mutate(glaucoma_true_neg_pictor = ifelse((glaucoma_right==0 | glaucoma_left==0) & cdr<0.7 & camera=="pictor", 1, 0)) %>%
  mutate(glaucoma_false_pos_pictor = ifelse((glaucoma_right==0 | glaucoma_left==0) & cdr>=0.7 & camera=="pictor", 1, 0))

specificity_glaucoma_peek <- sum(alldata$glaucoma_true_neg_peek, na.rm=TRUE)/(sum(alldata$glaucoma_true_neg_peek, na.rm=TRUE)+sum(alldata$glaucoma_false_pos_peek, na.rm=TRUE))
specificity_glaucoma_inview <- sum(alldata$glaucoma_true_neg_inview, na.rm=TRUE)/(sum(alldata$glaucoma_true_neg_inview, na.rm=TRUE)+sum(alldata$glaucoma_false_pos_inview, na.rm=TRUE))
specificity_glaucoma_pictor <- sum(alldata$glaucoma_true_neg_pictor, na.rm=TRUE)/(sum(alldata$glaucoma_true_neg_pictor, na.rm=TRUE)+sum(alldata$glaucoma_false_pos_pictor, na.rm=TRUE))

specificity_glaucoma_peek
specificity_glaucoma_inview
specificity_glaucoma_pictor

#specificity for otherdx
alldata <- alldata %>%
  mutate(otherdx_true_neg_peek = ifelse(((otherdx_right==0 | otherdx_left==0) & (other_dx==0|other_dx==1)) & camera=="peek", 1, 0)) %>%
  mutate(otherdx_false_pos_peek = ifelse(((otherdx_right==0 | otherdx_left==0) & (other_dx==2|other_dx==3)) & camera=="peek", 1, 0)) %>%  
  mutate(otherdx_true_neg_inview = ifelse(((otherdx_right==0 | otherdx_left==0) & (other_dx==0|other_dx==1)) & camera=="inview", 1, 0)) %>%
  mutate(otherdx_false_pos_inview = ifelse(((otherdx_right==0 | otherdx_left==0) & (other_dx==2|other_dx==3)) & camera=="inview", 1, 0)) %>%
  mutate(otherdx_true_neg_pictor = ifelse(((otherdx_right==0 | otherdx_left==0) & (other_dx==0|other_dx==1)) & camera=="pictor", 1, 0)) %>%
  mutate(otherdx_false_pos_pictor = ifelse(((otherdx_right==0 | otherdx_left==0) & (other_dx==2|other_dx==3)) & camera=="pictor", 1, 0))

specificity_otherdx_peek <- sum(alldata$otherdx_true_neg_peek, na.rm=TRUE)/(sum(alldata$otherdx_true_neg_peek, na.rm=TRUE)+sum(alldata$otherdx_false_pos_peek, na.rm=TRUE))
specificity_otherdx_inview <- sum(alldata$otherdx_true_neg_inview, na.rm=TRUE)/(sum(alldata$otherdx_true_neg_inview, na.rm=TRUE)+sum(alldata$otherdx_false_pos_inview, na.rm=TRUE))
specificity_otherdx_pictor <- sum(alldata$otherdx_true_neg_pictor, na.rm=TRUE)/(sum(alldata$otherdx_true_neg_pictor, na.rm=TRUE)+sum(alldata$otherdx_false_pos_pictor, na.rm=TRUE))

specificity_otherdx_peek
specificity_otherdx_inview
specificity_otherdx_pictor


#########EPI TABLES#####

## epi dr peek
table_dr_peek <- as.table(matrix(c(sum(alldata$dr_true_pos_peek, na.rm=TRUE),sum(alldata$dr_false_pos_peek, na.rm=TRUE),sum(alldata$dr_false_neg_peek, na.rm=TRUE),sum(alldata$dr_true_neg_peek, na.rm=TRUE)), nrow = 2, byrow = TRUE))
colnames(table_dr_peek) <- c("Dis+","Dis-")
rownames(table_dr_peek) <- c("Test+","Test-")
table_dr_peek

epi_dr_peek <- epi.tests(table_dr_peek, conf.level = 0.95)
epi_dr_peek


## epi dr inview
table_dr_inview <- as.table(matrix(c(sum(alldata$dr_true_pos_inview, na.rm=TRUE),sum(alldata$dr_false_pos_inview, na.rm=TRUE),sum(alldata$dr_false_neg_inview, na.rm=TRUE),sum(alldata$dr_true_neg_inview, na.rm=TRUE)), nrow = 2, byrow = TRUE))
colnames(table_dr_inview) <- c("Dis+","Dis-")
rownames(table_dr_inview) <- c("Test+","Test-")
table_dr_inview

epi_dr_inview <- epi.tests(table_dr_inview, conf.level = 0.95)
epi_dr_inview


## epi dr pictor
table_dr_pictor <- as.table(matrix(c(sum(alldata$dr_true_pos_pictor, na.rm=TRUE),sum(alldata$dr_false_pos_pictor, na.rm=TRUE),sum(alldata$dr_false_neg_pictor, na.rm=TRUE),sum(alldata$dr_true_neg_pictor, na.rm=TRUE)), nrow = 2, byrow = TRUE))
colnames(table_dr_pictor) <- c("Dis+","Dis-")
rownames(table_dr_pictor) <- c("Test+","Test-")
table_dr_pictor

epi_dr_pictor <- epi.tests(table_dr_pictor, conf.level = 0.95)
epi_dr_pictor


## epi dme peek
table_dme_peek <- as.table(matrix(c(sum(alldata$dme_true_pos_peek, na.rm=TRUE),sum(alldata$dme_false_pos_peek, na.rm=TRUE),sum(alldata$dme_false_neg_peek, na.rm=TRUE),sum(alldata$dme_true_neg_peek, na.rm=TRUE)), nrow = 2, byrow = TRUE))
colnames(table_dme_peek) <- c("Dis+","Dis-")
rownames(table_dme_peek) <- c("Test+","Test-")
table_dme_peek

epi_dme_peek <- epi.tests(table_dme_peek, conf.level = 0.95)
epi_dme_peek


## epi dme inview
table_dme_inview <- as.table(matrix(c(sum(alldata$dme_true_pos_inview, na.rm=TRUE),sum(alldata$dme_false_pos_inview, na.rm=TRUE),sum(alldata$dme_false_neg_inview, na.rm=TRUE),sum(alldata$dme_true_neg_inview, na.rm=TRUE)), nrow = 2, byrow = TRUE))
colnames(table_dme_inview) <- c("Dis+","Dis-")
rownames(table_dme_inview) <- c("Test+","Test-")
table_dme_inview

epi_dme_inview <- epi.tests(table_dme_inview, conf.level = 0.95)
epi_dme_inview


## epi dme pictor
table_dme_pictor <- as.table(matrix(c(sum(alldata$dme_true_pos_pictor, na.rm=TRUE),sum(alldata$dme_false_pos_pictor, na.rm=TRUE),sum(alldata$dme_false_neg_pictor, na.rm=TRUE),sum(alldata$dme_true_neg_pictor, na.rm=TRUE)), nrow = 2, byrow = TRUE))
colnames(table_dme_pictor) <- c("Dis+","Dis-")
rownames(table_dme_pictor) <- c("Test+","Test-")
table_dme_pictor

epi_dme_pictor <- epi.tests(table_dme_pictor, conf.level = 0.95)
epi_dme_pictor


## epi amd peek
table_amd_peek <- as.table(matrix(c(sum(alldata$amd_true_pos_peek, na.rm=TRUE),sum(alldata$amd_false_pos_peek, na.rm=TRUE),sum(alldata$amd_false_neg_peek, na.rm=TRUE),sum(alldata$amd_true_neg_peek, na.rm=TRUE)), nrow = 2, byrow = TRUE))
colnames(table_amd_peek) <- c("Dis+","Dis-")
rownames(table_amd_peek) <- c("Test+","Test-")
table_amd_peek

epi_amd_peek <- epi.tests(table_amd_peek, conf.level = 0.95)
epi_amd_peek


## epi amd inview
table_amd_inview <- as.table(matrix(c(sum(alldata$amd_true_pos_inview, na.rm=TRUE),sum(alldata$amd_false_pos_inview, na.rm=TRUE),sum(alldata$amd_false_neg_inview, na.rm=TRUE),sum(alldata$amd_true_neg_inview, na.rm=TRUE)), nrow = 2, byrow = TRUE))
colnames(table_amd_inview) <- c("Dis+","Dis-")
rownames(table_amd_inview) <- c("Test+","Test-")
table_amd_inview

epi_amd_inview <- epi.tests(table_amd_inview, conf.level = 0.95)
epi_amd_inview


## epi amd pictor
table_amd_pictor <- as.table(matrix(c(sum(alldata$amd_true_pos_pictor, na.rm=TRUE),sum(alldata$amd_false_pos_pictor, na.rm=TRUE),sum(alldata$amd_false_neg_pictor, na.rm=TRUE),sum(alldata$amd_true_neg_pictor, na.rm=TRUE)), nrow = 2, byrow = TRUE))
colnames(table_amd_pictor) <- c("Dis+","Dis-")
rownames(table_amd_pictor) <- c("Test+","Test-")
table_amd_pictor

epi_amd_pictor <- epi.tests(table_amd_pictor, conf.level = 0.95)
epi_amd_pictor



## epi glaucoma peek
table_glaucoma_peek <- as.table(matrix(c(sum(alldata$glaucoma_true_pos_peek, na.rm=TRUE),sum(alldata$glaucoma_false_pos_peek, na.rm=TRUE),sum(alldata$glaucoma_false_neg_peek, na.rm=TRUE),sum(alldata$glaucoma_true_neg_peek, na.rm=TRUE)), nrow = 2, byrow = TRUE))
colnames(table_glaucoma_peek) <- c("Dis+","Dis-")
rownames(table_glaucoma_peek) <- c("Test+","Test-")
table_glaucoma_peek

epi_glaucoma_peek <- epi.tests(table_glaucoma_peek, conf.level = 0.95)
epi_glaucoma_peek


## epi glaucoma inview
table_glaucoma_inview <- as.table(matrix(c(sum(alldata$glaucoma_true_pos_inview, na.rm=TRUE),sum(alldata$glaucoma_false_pos_inview, na.rm=TRUE),sum(alldata$glaucoma_false_neg_inview, na.rm=TRUE),sum(alldata$glaucoma_true_neg_inview, na.rm=TRUE)), nrow = 2, byrow = TRUE))
colnames(table_glaucoma_inview) <- c("Dis+","Dis-")
rownames(table_glaucoma_inview) <- c("Test+","Test-")
table_glaucoma_inview

epi_glaucoma_inview <- epi.tests(table_glaucoma_inview, conf.level = 0.95)
epi_glaucoma_inview


## epi glaucoma pictor
table_glaucoma_pictor <- as.table(matrix(c(sum(alldata$glaucoma_true_pos_pictor, na.rm=TRUE),sum(alldata$glaucoma_false_pos_pictor, na.rm=TRUE),sum(alldata$glaucoma_false_neg_pictor, na.rm=TRUE),sum(alldata$glaucoma_true_neg_pictor, na.rm=TRUE)), nrow = 2, byrow = TRUE))
colnames(table_glaucoma_pictor) <- c("Dis+","Dis-")
rownames(table_glaucoma_pictor) <- c("Test+","Test-")
table_glaucoma_pictor

epi_glaucoma_pictor <- epi.tests(table_glaucoma_pictor, conf.level = 0.95)
epi_glaucoma_pictor



## epi otherdx peek
table_otherdx_peek <- as.table(matrix(c(sum(alldata$otherdx_true_pos_peek, na.rm=TRUE),sum(alldata$otherdx_false_pos_peek, na.rm=TRUE),sum(alldata$otherdx_false_neg_peek, na.rm=TRUE),sum(alldata$otherdx_true_neg_peek, na.rm=TRUE)), nrow = 2, byrow = TRUE))
colnames(table_otherdx_peek) <- c("Dis+","Dis-")
rownames(table_otherdx_peek) <- c("Test+","Test-")
table_otherdx_peek

epi_otherdx_peek <- epi.tests(table_otherdx_peek, conf.level = 0.95)
epi_otherdx_peek


## epi otherdx inview
table_otherdx_inview <- as.table(matrix(c(sum(alldata$otherdx_true_pos_inview, na.rm=TRUE),sum(alldata$otherdx_false_pos_inview, na.rm=TRUE),sum(alldata$otherdx_false_neg_inview, na.rm=TRUE),sum(alldata$otherdx_true_neg_inview, na.rm=TRUE)), nrow = 2, byrow = TRUE))
colnames(table_otherdx_inview) <- c("Dis+","Dis-")
rownames(table_otherdx_inview) <- c("Test+","Test-")
table_otherdx_inview

epi_otherdx_inview <- epi.tests(table_otherdx_inview, conf.level = 0.95)
epi_otherdx_inview


## epi otherdx pictor
table_otherdx_pictor <- as.table(matrix(c(sum(alldata$otherdx_true_pos_pictor, na.rm=TRUE),sum(alldata$otherdx_false_pos_pictor, na.rm=TRUE),sum(alldata$otherdx_false_neg_pictor, na.rm=TRUE),sum(alldata$otherdx_true_neg_pictor, na.rm=TRUE)), nrow = 2, byrow = TRUE))
colnames(table_otherdx_pictor) <- c("Dis+","Dis-")
rownames(table_otherdx_pictor) <- c("Test+","Test-")
table_otherdx_pictor

epi_otherdx_pictor <- epi.tests(table_otherdx_pictor, conf.level = 0.95)
epi_otherdx_pictor