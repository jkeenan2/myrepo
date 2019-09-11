#Clear existing redcapexport and graphics
rm(list=ls())
graphics.off()

#Load Hmisc library
#library(Hmisc)
library(readr)
library(biostat3)
#Load exploratory redcapexport analysis packages
library(tidyverse)
library(tidyr)
library(scales)   # date/time scales for plots
library(lubridate)
library(survival)
library(survminer)
library(coxphf)
library(dplyr)
library(skimr)
library(broom)
library(eq5d)


#Read Data
redcapexport <- read_csv("RetinitisTreatedWith_DATA_2019-04-27_1548.csv", 
                         col_types = cols(mrn = col_character()),
                         guess_max=3000)
# cd4_date = col_date(format = "%Y/%m/%d"), 
# cd_12m_date_v2 = col_date(format = "%Y/%m/%d"), 
# cd_6m_date = col_date(format = "%Y/%m/%d"), 
# cd_3m_date = col_date(format = "%Y/%m/%d"),
# cd_enroll_date = col_date(format = "%Y/%m/%d"), 
# date1 = col_date(format = "%Y/%m/%d"), 
# date2 = col_date(format = "%Y/%m/%d"), 
# date3 = col_date(format = "%Y/%m/%d"), 
# fu_date = col_date(format = "%Y/%m/%d"), 
# haart_date = col_date(format = "%Y/%m/%d"), 
# reg_date = col_date(format = "%Y/%m/%d")))



############################
#     DATA CLEANING        #
############################

# Calculate EQ5 scores:
cmvr_eq5data <- redcapexport %>%
  separate(studyid, c("id", "dde"), sep = "--", remove = FALSE, convert = FALSE) %>%
  filter(is.na(dde) & studyid != "test" & studyid != "test3") %>%
  filter(!is.na(eq_1)) %>%
  #change variable names to match format for eq5d package
  rename(MO=eq_1,
         SC=eq_2,
         UA=eq_3,
         PD=eq_4,
         AD=eq_5) %>%
  select(studyid, redcap_event_name, MO, SC, UA, PD, AD) %>%
  arrange(studyid, redcap_event_name) # xtabs(data=cmvr_eq5data, ~dup)
cmvr_eq5 <- bind_cols(cmvr_eq5data, 
                      # THINK OF THIS AS MAKING A NEW DATA FRAME THAT YOU WANT TO BIND
                      # THE ONLY WAY I COULD FIGURE OUT HOW TO NAME THE NEW COLUMN WAS THE MATRIX AND DIMNAMES BUSINESS...
                      as.data.frame(matrix(cmvr_eq5data %>% select(MO, SC, UA, PD, AD) %>% eq5d(., country="Thailand", version="5L", type="VT"), dimnames=list(NULL, c("eq5score")))))
# Confirming it worked...
head(cmvr_eq5 %>%
       select(studyid, redcap_event_name, MO, SC, UA, PD, AD, eq5score))
eq5d(c(MO=2,SC=1,UA=5,PD=3,AD=1), country="Thailand", version="5L", type="VT")
eq5d(c(MO=1,SC=1,UA=1,PD=1,AD=2), country="Thailand", version="5L", type="VT")
# Looks OK
cmvr_eq5_formerge <- cmvr_eq5 %>%
  select(studyid, redcap_event_name, eq5score)
#change format of date recordings to number of weeks
cmvrdata <- redcapexport %>%
  separate(studyid, c("id", "dde"), sep = "--", remove = FALSE, convert = FALSE) %>%
  filter(is.na(dde) & studyid != "test" & studyid != "test3") %>%
  left_join(., cmvr_eq5_formerge, by=c("studyid", "redcap_event_name")) %>%
  mutate(hivdx_date = as.Date(hivdx_date, format = "%m/%d/%y"),
         redcap_week=NA,
         redcap_week=ifelse(redcap_event_name == "0_wk_arm_1", 0,
                     ifelse(redcap_event_name == "1_wk_arm_1", 1,
                     ifelse(redcap_event_name == "2_wk_arm_1", 2,
                     ifelse(redcap_event_name == "3_wk_arm_1", 3,
                     ifelse(redcap_event_name == "4_wk_arm_1", 4,
                     ifelse(redcap_event_name == "5_wk_arm_1", 5,
                     ifelse(redcap_event_name == "6_wk_arm_1", 6,
                     ifelse(redcap_event_name == "7_wk_arm_1", 7,
                     ifelse(redcap_event_name == "8_wk_arm_1", 8,
                     ifelse(redcap_event_name == "9_wk_arm_1", 9,
                     ifelse(redcap_event_name == "10_wk_arm_1", 10,
                     ifelse(redcap_event_name == "11_wk_arm_1", 11,
                     ifelse(redcap_event_name == "3_mo__12_wk_arm_1", 12,
                     ifelse(redcap_event_name == "13_wk_arm_1", 13,
                     ifelse(redcap_event_name == "14_wk_arm_1", 14,
                     ifelse(redcap_event_name == "15_wk_arm_1", 15,
                     ifelse(redcap_event_name == "16_wk_arm_1", 16,
                     ifelse(redcap_event_name == "17_wk_arm_1", 17,
                     ifelse(redcap_event_name == "18_wk_arm_1", 18,
                     ifelse(redcap_event_name == "19_wk_arm_1", 19,
                     ifelse(redcap_event_name == "20_wk_arm_1", 20,
                     ifelse(redcap_event_name == "21_wk_arm_1", 21,
                     ifelse(redcap_event_name == "22_wk_arm_1", 22,
                     ifelse(redcap_event_name == "23_wk_arm_1", 23,
                     ifelse(redcap_event_name == "6_mo__24_wk_arm_1", 24, 
                     ifelse(redcap_event_name == "25_wk_arm_1", 25, redcap_week))))))))))))))))))))))))))) %>%
  mutate(redcap_week=ifelse(redcap_event_name =="26_wk_arm_1", 26,
                     ifelse(redcap_event_name == "27_wk_arm_1", 27, 
                     ifelse(redcap_event_name =="28_wk_arm_1", 28,
                     ifelse(redcap_event_name =="29_wk_arm_1", 29,
                     ifelse(redcap_event_name =="30_wk_arm_1", 30,
                     ifelse(redcap_event_name =="31_wk_arm_1", 31, 
                     ifelse(redcap_event_name =="32_wk_arm_1", 32,
                     ifelse(redcap_event_name =="33_wk_arm_1", 33,
                     ifelse(redcap_event_name =="34_wk_arm_1", 34,
                     ifelse(redcap_event_name =="35_wk_arm_1", 35,
                     ifelse(redcap_event_name =="36_wk_arm_1", 36,
                     ifelse(redcap_event_name =="37_wk_arm_1", 37,
                     ifelse(redcap_event_name =="38_wk_arm_1", 38,
                     ifelse(redcap_event_name =="39_wk_arm_1", 39,
                     ifelse(redcap_event_name =="40_wk_arm_1", 40,
                     ifelse(redcap_event_name =="41_wk_arm_1", 41,
                     ifelse(redcap_event_name =="42_wk_arm_1", 42,
                     ifelse(redcap_event_name =="43_wk_arm_1", 43,
                     ifelse(redcap_event_name =="44_wk_arm_1", 44, redcap_week)))))))))))))))))))) %>% 
  mutate(redcap_week=ifelse(redcap_event_name == "45_wk_arm_1", 45,
                     ifelse(redcap_event_name == "46_wk_arm_1", 46,
                     ifelse(redcap_event_name == "47_wk_arm_1", 47,
                     ifelse(redcap_event_name == "48_wk_arm_1", 48,
                     ifelse(redcap_event_name == "49_wk_arm_1", 49,
                     ifelse(redcap_event_name == "50_wk_arm_1", 50,
                     ifelse(redcap_event_name == "51_wk_arm_1", 51,
                     ifelse(redcap_event_name == "12_mo__52_wk_arm_1", 52, redcap_week))))))))) %>%
  mutate(new_cd4_count = ifelse(!is.na(cd4_count), cd4_count,
                         ifelse(!is.na(cd_enroll_count), cd_enroll_count,
                         ifelse(!is.na(cd_3m_count), cd_3m_count,
                         ifelse(!is.na(cd_6m_count), cd_6m_count,
                         ifelse(!is.na(cd_12m_count_v2), cd_12m_count_v2, NA)))))) %>%
  mutate(new_cd4_date = if_else(!is.na(cd4_date), as_date(cd4_date),
                        if_else(!is.na(cd_enroll_date), cd_enroll_date,
                        if_else(!is.na(cd_3m_date), cd_3m_date,
                        if_else(!is.na(cd_6m_date), cd_6m_date,
                        if_else(!is.na(cd_12m_date_v2), cd_12m_date_v2, as_date(NA) )))))) %>%
  mutate(exam_date = if_else(!is.na(date1), as_date(date1),
                     if_else(!is.na(date2), date2,
                     if_else(!is.na(date3), date3,
                     if_else(!is.na(fu_date), fu_date, 
                     if_else(!is.na(new_cd4_date), new_cd4_date, as_date(NA) ))))),
         female=if_else(gender==1,1,if_else(gender==2,0,NA_real_))) %>%
  group_by(studyid) %>%
  mutate(timesincehiv = as.numeric((if_else(!is.na(hivdx_date), min(exam_date, na.rm=TRUE) - hivdx_date, 
                        if_else(hiv_dxdate3==1, hivdx_date2*(365/12), 
                        if_else(hiv_dxdate3==2, hivdx_date2*365, 999))))/(365/12)),
         fu_os_cmv=if_else(studyid=="C128" & redcap_week==44,2,fu_os_cmv)) %>%
  ungroup()


# Need Louisa to look these up
missingexamdate <- cmvrdata %>%
  filter(is.na(exam_date))
# write_csv(missingexamdate, "missingexamdate.csv")

# IMPORT PHOTO NAMES
library(readxl)
library(lubridate)
photonames <- read_excel("CMV_Photos_New-File-Names-27APR2019-JK.xlsx", sheet = "Sheet1") %>% 
  rename(originalname=`Original file name`,
         newname=`New file name`) %>%
  select(originalname, newname) %>%
  mutate(o.originalname=originalname,
         righteye=str_extract(originalname, "RE"),
         lefteye=str_extract(originalname, "LE"),
         eye=if_else(!is.na(righteye), righteye, lefteye)) %>%
  separate(originalname, into=c("newfield1", "re"), sep="RE_") %>%
  separate(newfield1, into=c("montage", "le"), sep="LE_") %>%
  separate(newname, into=c("studyid", "garbage1", "garbage2"), remove=FALSE) %>%
  mutate(date1=if_else(!is.na(re), re, le),
         date2=substr(date1, start=1, stop=8),
         date3=ymd(date2)) %>%
  separate(date1, into=c("x", "y"), sep="_", remove=FALSE) %>%
  mutate(x=if_else(x=="0" | x=="New", NA_character_, x),
         date4=if_else(is.na(date3) & !is.na(x), substr(x, start=1, stop=8), 
                       if_else(is.na(date3) & !is.na(y), substr(y, start=1, stop=8), date2)),
         date4=case_when(newname=="C26_RE_1.jpg" ~ "20130711",
                         newname=="C125_RE_5.jpg" ~ "20150528",
                         newname=="C4_RE_3.jpg" ~ "20130613",
                         newname=="C4_LE_15.jpg" ~ "20160204",
                         newname=="C4_LE_16.jpg" ~ "20160804",
                         newname=="C120_LE_1.jpg" ~ "20150115",
                         newname=="C120_RE_1.jpg" ~ "20150115",
                         newname=="C120_RE_4.jpg" ~ "20150226",
                         newname=="C137_RE_1.jpg" ~ "20150430",
                         newname=="C137_LE_1.jpg" ~ "20150430",
                         newname=="C85_RE_1.jpg" ~ "20140821",
                         newname=="C85_RE_2.jpg" ~ "20140904",
                         newname=="C150_RE_1.jpg" ~ "20150820",
                         newname=="C150_LE_1.jpg" ~ "20150820",
                         newname=="C182_RE_5.jpg" ~ "20141030",
                         newname=="C182_LE_5.jpg" ~ "20141030",
                         TRUE ~ date4),
         exam_date=ymd(date4),
         eye=case_when(eye=="RE" ~ "od_",
                       eye=="LE" ~ "os_",
                       TRUE ~ NA_character_)) %>%
  select(o.originalname, studyid, exam_date, eye, newname)


# IMPORT DATA FOR PROGRESSION
teamA <- read_csv("CMVRetinitisPhotoGra_DATA_2019-04-27_1547-TeamA.csv")
teamB <- read_csv("CMVRetinitisPhotoGra_DATA_2019-04-27_1547-TeamB.csv")
poay <- read_csv("CMVRetinitisPhotoGra_DATA_2019-04-27_1615-Poay.csv") %>%
  mutate(grader="poay")
jeremy <- read_csv("CMVRetinitisPhotoGra_DATA_2019-04-27_1548-Jeremy.csv") %>%
  mutate(grader="jeremy")
teamPJ <- bind_rows(poay, jeremy) # %>% group_by(recordid, redcap_repeat_instance) %>% mutate(dup=n()) # xtabs(data=teamPJ, ~dup, addNA=TRUE)


teamAB <- full_join(teamA, teamB, by=c("recordid", "redcap_repeat_instance"), suffix=c(".a", ".b"))
teamABP <- full_join(teamAB, teamPJ, by=c("recordid", "redcap_repeat_instance"))
glimpse(teamABP)

xtabs(data=teamABP, ~cmvr.a+cmvr.b+cmvr, addNA=TRUE)
xtabs(data=teamABP, ~prog_existing.a+prog_existing.b, addNA=TRUE)
xtabs(data=teamABP, ~prog_new.a+prog_new.b, addNA=TRUE)
xtabs(data=teamABP, ~pct_retina_involved.a+pct_retina_involved.b, addNA=TRUE)

progdata <- teamABP %>%
  mutate(cmv.cons=if_else((cmvr.a==1 & cmvr.b==1) | (cmvr.a==1 & cmvr==1) | (cmvr.b==1 & cmvr==1),1,0),
         prog.exist.cons=if_else((prog_existing.a==1 & prog_existing.b==1) | (prog_existing.a==1 & prog_existing==1) | (prog_existing.b==1 & prog_existing==1),1,0),
         newname=paste0(recordid, "_", redcap_repeat_instance,".jpg")) %>%
  select(newname, redcap_repeat_instance, cmvr.a, cmvr.b, cmvr, cmv.cons, prog_existing.a, prog_existing.b, prog_existing, prog.exist.cons, pct_retina_involved.a, pct_retina_involved.b, pct_retina_involved)
progdata2 <- full_join(progdata, photonames, by="newname")

#next step: make sure eyes are correlated...bootstrapping (observations aren't quite independent)
cmvrdatalong <- cmvrdata %>%
  select(studyid, redcap_week, exam_date, age, female, hivdx, timesincehiv, haart, new_cd4_count, new_cd4_date, date1, date2, date3, 
         fu_date, eq5score, od_symp:os_va_ph_other, od_cmv:os_tx_other, fu_od_va_unc:fu_os_va_ph_other, fu_od_cmv:fu_os_tx_other ) %>%
  gather(origfield, value, c(od_symp:os_va_ph_other, od_cmv:os_tx_other, fu_od_va_unc:fu_os_va_ph_other, fu_od_cmv:fu_os_tx_other)) %>%
  mutate(os=str_extract(origfield, "os_"),
         od=str_extract(origfield, "od_"),
         fu=str_extract(origfield, "fu"),
         eye=ifelse(!is.na(os), os,
                    ifelse(!is.na(od), od,NA)),
         origfieldminus=str_replace(origfield, "fu_os_", ""),
         origfieldminus=str_replace(origfieldminus, "fu_od_", ""),
         origfieldminus=str_replace(origfieldminus, "os_", ""),
         origfieldminus=str_replace(origfieldminus, "od_", ""),
         studyid_eye_week=paste(studyid, eye, redcap_week, sep="-")) %>%
  filter((redcap_week==0 & is.na(fu)) | (redcap_week!=0 & fu=="fu")) %>%
  select(-origfield, -od, -os, -fu) %>%
  spread(origfieldminus, value, convert = TRUE) %>%
  mutate(new_cmv = ifelse(cmv == 1, 1, 
                          ifelse(cmv %in% c(2, 3), 0, NA)), # xtabs(data=cmvrdatalong, ~new_cmv+cmv, addNA = TRUE)
         new_rd = ifelse(rd == 1, 1, 
                         ifelse(rd %in% c(2, 3), 0, NA)), # xtabs(data=cmvrdatalong, ~new_rd+rd, addNA = TRUE)
         new_maced = ifelse(maced == 1, 1, 
                            ifelse(maced %in% c(2, 3), 0, NA)),
         new_vithaze = ifelse(vithaze == 1, 1, 
                              ifelse(vithaze %in% c(2, 3), 0, NA)),
         va = ifelse(!is.na(va_ph), va_ph,
                     ifelse(!is.na(va_unc), va_unc, NA)),
         logmar = NA, 
         logmar = ifelse(va == 1, 3,
                  ifelse(va == 2, 2.5,
                  ifelse(va == 3, 2.28,
                  ifelse(va == 4, 1.98,
                  ifelse(va == 5, 1.7781512503836,
                  ifelse(va == 6, 1.4771212547197,
                  ifelse(va == 7, 1,
                  ifelse(va == 8, 0.77815125038364,
                  ifelse(va == 9, 0.60205999132796,
                  ifelse(va == 10, 0.47712125471966,     
                  ifelse(va == 11, 0.30102999566398, 
                  ifelse(va == 12, 0.17609125905568,
                  ifelse(va == 13, 0,
                  ifelse(va_ph==14 & va_ph_other=="1/60", 1.7781512503836, # xtabs(data = filter(cmvrdatalong, va==14), ~va_ph_other, addNA = TRUE)
                  ifelse(va_ph==14 & va_ph_other=="2/60", 1.4771212547197,
                  ifelse(va_ph==14 & va_ph_other=="PJ", 2.5, NA)))))))))))))))), # xtabs(data = cmvrdatalong, ~logmar+va, addNA = TRUE)
# troubleshoot: explain NA's for logmar? looks like NA's are @ cd4 count visits (i.e. @ 12wk, 24wk) when va was not recorded! 
# logmartroubleshoot <- cmvrdatalong %>%
#   filter(is.na(logmar)==TRUE) %>%
#   select(studyid, redcap_week, new_cd4_date, new_cd4_count, va, va_ph, va_unc, va_unc_other)
# REPLACING RD BASED ON CHART REVIEW...
         new_rd=replace(new_rd, studyid %in% c("C121"), 1),
         new_rd=replace(new_rd, studyid %in% c("C11") & eye %in% c("os"), 1),
         new_rd=replace(new_rd, studyid %in% c("C118", "C141", "C15", "C23", "C25", "C3", "C44", "C73"), 0),
         new_rd=replace(new_rd, studyid %in% c("C129") & redcap_week %in% ("21"), 0), 
         new_rd=replace(new_rd, studyid %in% c("C137") & eye %in% c("od") & redcap_week %in% ("52"), 1),
         new_rd=replace(new_rd, studyid %in% c("C145") & eye %in% c("os") & redcap_week %in% c("10"), 1),
         new_rd=replace(new_rd, studyid %in% c("C19") & redcap_week %in% c("6"), 0),
         new_rd=replace(new_rd, studyid %in% c("C4") & eye %in% c("od") & redcap_week %in% c("22", "35", "43", "52"), 1),
         new_rd=replace(new_rd, studyid %in% c("C42") & redcap_week %in% c("45"), 0),
         new_rd=replace(new_rd, studyid %in% c("C56") & redcap_week %in% c("12"), 0),
         new_rd=replace(new_rd, studyid %in% c("C74") & redcap_week %in% c("52"), 1), new_rd) %>%
  group_by(studyid, eye) %>%
  mutate(baseline_rd=min(ifelse(redcap_week == "0", new_rd, 999), na.rm = TRUE), # xtabs(data = cmvrdatalong, ~redcap_week+baseline_rd, addNA = TRUE)
                                                                                 # xtabs(data = filter(cmvrdatalong, redcap_week == 0 & baseline_rd == 1), ~studyid_eye_week+baseline_rd, addNA = TRUE)
                                                                                 # looks like 6 eyes have RD at baseline: C11-os, C115-od, C121-od, C121-os, C46-od, C46-os
         rd_developed=max(ifelse((baseline_rd == 0 & new_rd == 1), 1, 0), na.rm = TRUE),  # xtabs(data = filter(cmvrdatalong, rd_developed == 1), ~studyid+eye, addNA = TRUE)
                                                                                          # looks like 13 eyes (in 10 pts) developed RD! 
                                                                                          #C125 OD @ wk 26 (175 days)
                                                                                          #C134 OS @ wk 16 (119 days)
                                                                                          #C137 OD @ wk 26 (189 days)
                                                                                          #C145 OS @ wk 2 (14 days)
                                                                                          #C148 OD @ wk 11 (84 days)
                                                                                          #C148 OS @ wk 26 (189 days)
                                                                                          #C4 OD @ wk 2 (14 days)
                                                                                          #C5 OD @ wk 52 (343 days)
                                                                                          #C65 OD @ wk 2 (14 days)
                                                                                          #C65 OS @ wk 45 (315 days)
                                                                                          #C74 OD @ wk 34 (91 days)
                                                                                          #C74 OS @ wk 22 (147 days)
                                                                                          #C93 OD @ wk 20 (140 days)
         first_cmvr_date_working = if_else(new_cmv == 1, exam_date, as_date(NA)),
         first_cmvr_date_eye = min(first_cmvr_date_working, na.rm = TRUE),
         time_in_cmvfu_eye = max(as.integer(exam_date), na.rm = TRUE) - as.integer(first_cmvr_date_eye),
         max_exam_date=max(exam_date, na.rm=TRUE),
         time_to_rd = min(if_else(new_rd == 1, exam_date - first_cmvr_date_eye, 
                           if_else(new_rd == 0, max(exam_date, na.rm = TRUE) - first_cmvr_date_eye, NA_real_)), na.rm = TRUE),
         time_to_rd = replace(time_to_rd, is.infinite(time_to_rd), NA),
         time_to_rd = if_else(time_to_rd>400, as.difftime(400, units="days"), time_to_rd)) %>%
  group_by(studyid) %>%
  mutate(first_cmvr_date_patient = min(first_cmvr_date_working, na.rm = TRUE),
         time_in_cmvfu_patient = max(as.integer(exam_date), na.rm = TRUE) - as.integer(first_cmvr_date_patient),
         second_eye=if_else(!is.infinite(max(first_cmvr_date_eye, na.rm=TRUE)) & max(as.integer(first_cmvr_date_eye), na.rm=TRUE) != min(as.integer(first_cmvr_date_eye), na.rm=TRUE), 1, 0), # The warnings are just saying that it is returning -Inf
         diffpatienteye=max(first_cmvr_date_eye-first_cmvr_date_patient),
         time_to_secondeye = if_else(second_eye==1, min(diffpatienteye), as.numeric(max_exam_date-first_cmvr_date_patient)),
         time_to_secondeye = replace(time_to_secondeye, is.infinite(time_to_secondeye), NA),
         time_to_secondeye = if_else(time_to_secondeye>400, as.difftime(400, units="days"), time_to_secondeye),
         first_eye = ifelse(first_cmvr_date_eye == first_cmvr_date_patient, 1, 0),
         age=min(age, na.rm=TRUE),
         female=min(female, na.rm=TRUE),
         haart=max(haart, na.rm=TRUE),
         timesincehiv=min(timesincehiv, na.rm=TRUE),
         hivdx=max(hivdx, na.rm=TRUE),
         mostpostzone = if_else(cmv_zones___1==1, 1,
                        if_else(cmv_zones___2==1, 2,
                        if_else(cmv_zones___3==1, 3, NA_real_))),
         mostantzone = if_else(cmv_zones___3==1, 3,
                       if_else(cmv_zones___2==1, 2,
                       if_else(cmv_zones___1==1, 1, NA_real_))),
         mostpostzone1 = as.factor(if_else(mostpostzone==1,1, 0)),
         mostantzone2 = as.factor(if_else(mostantzone==1,2, mostantzone))) %>%
  rename(induction=tx___1,
         maintenance=tx___2,
         observation=tx___3,
         othertx=tx___4) %>%
  group_by(studyid, redcap_week) %>%
  mutate(induction=case_when((studyid=="C125" & redcap_week==26 & eye=="od_") ~ 1,
                             (studyid=="C145" & redcap_week==4 & eye=="os_") ~ 1,
                             (studyid=="C145" & redcap_week==10 & eye=="os_") ~ 1,
                             TRUE ~ as.numeric(induction)),
         visit_laterality=sum(new_cmv),
         uni_vi=if_else(max(logmar)>0.47712125471966,1,0),
         bi_vi=if_else(min(logmar)>0.47712125471966,1,0),
         uni_blind=if_else(max(logmar)>1.301029995664,1,0),
         bi_blind=if_else(min(logmar)>1.301029995664,1,0),
         starttime=0) %>%
  full_join(., progdata2, by=c("studyid", "eye", "exam_date")) %>% # xtabs(data=filter(cmvrdatalong, redcap_week==0 & new_cmv==1), ~studyid+visit_laterality)
  mutate(percentretina=if_else(!is.na(pct_retina_involved), pct_retina_involved, 
                       if_else(pct_retina_involved.a==pct_retina_involved.b, pct_retina_involved.a, NA_real_)),
         percentretina=case_when(newname=="C106_RE_1.jpg" ~ 1, # Assuming small; good photos, data says zone 3 only
                                 newname=="C107_LE_1.jpg" ~ 1,
                                 newname=="C113_LE_1.jpg" ~ 1,
                                 newname=="C120_LE_1.jpg" ~ 1, #  Based on diagram
                                 newname=="C120_RE_1.jpg" ~ 1,
                                 newname=="C121_LE_1.jpg" ~ 1,
                                 newname=="C139_RE_1.jpg" ~ 1,
                                 newname=="C40_RE_3.jpg" ~ 1, # This based on the diagram, not the photo
                                 newname=="C13_LE_1.jpg" ~ 1, # This based on the diagram, not the photo
                                 newname=="C13_RE_1.jpg" ~ 1, # This based on the diagram and later photos
                                 newname=="C65_LE_1.jpg" ~ 2, # This based on the diagram, not the photo
                                 newname=="C68_LE_1.jpg" ~ 1, # This based on the diagram, not the photo
                                 studyid=="C111" & redcap_week==2 & eye=="od_" ~ 1, # This based on the diagram, not the photo
                                 studyid=="C23" & redcap_week==0 & eye=="os_" ~ 4, # This based on the diagram, but photo consistent; looks like +VH
                                 studyid=="C80" & redcap_week==0 & eye=="od_" ~ 4, # This based on the diagram, not the photo
                                 studyid=="C82" & redcap_week==0 & eye=="os_" ~ 1, # This based on the diagram, not the photo
                                 studyid=="C19" & redcap_week==3 & eye=="od_" ~ 1, # This based on the diagram, not the photo
                                 studyid=="C30" & redcap_week==0 & eye=="od_" ~ 2, # This based on the diagram, not the photo
                                 studyid=="C29" & redcap_week==0 & eye=="od_" ~ 1, # This based on the diagram, not the photo
                                 studyid=="C46" & redcap_week==0 & eye=="od_" ~ 4, # This based on the diagram (total RD) and the photo--but difficult
                                 studyid=="C65" & redcap_week==0 & eye=="od_" ~ 4, # This based on the diagram and the photo
                                 studyid=="C115" & redcap_week==0 & eye=="od_" ~ 4, # This based on the diagram and the photo
                                 studyid=="C41" & redcap_week==0 & eye=="od_" ~ 2, # This based on the subsequent photos
                                 studyid=="C3" & redcap_week==0 & eye=="os_" ~ 2, # This based on the subsequent photos and diagram
                                 studyid=="C141" & redcap_week==0 & eye=="os_" ~ 4, # This based on single photo that Louisa found plus diagram
                                 studyid=="C148" & redcap_week==0 & eye=="os_" ~ 1, # Nothing in photos (zone 3 only), so assuming small
                                 TRUE ~ percentretina),
         percentretina2=as.factor(if_else(percentretina==4,3,if_else(percentretina==1,2,percentretina))))

# Check for inaccurate photo order:
cdl.wrongorder.rri <- cmvrdatalong %>%
  filter(!is.na(redcap_repeat_instance)) %>%
  group_by(studyid, eye) %>%
  arrange(exam_date) %>%
  mutate(redinstance.diff = redcap_repeat_instance - lag(redcap_repeat_instance, default = first(redcap_repeat_instance)),
         minrridiff=min(redinstance.diff)) %>%
  filter(minrridiff<0) %>%
    select(studyid, eye, exam_date, redcap_week, redcap_repeat_instance, redinstance.diff, minrridiff, o.originalname)
write_csv(cdl.wrongorder.rri, "cdl.wrongorder.rri.csv")
cmv.ts <- cmvrdata %>%
  filter(studyid=="C1") %>%
  select(studyid, redcap_event_name, exam_date, fu_date, redcap_week, fu_od_cmv, new_cd4_count, new_cd4_date, hk1)

xtabs(data=cdl.wrongorder.rri, ~studyid+eye)

#Attempt at tables for baseline characteristics
# Note that firstcmv.data.all has all eyes, regardless of whether they were a "first eye" or "second eye"
firstcmv.data.all <- cmvrdatalong %>%
  filter(hivdx==1 & new_cmv==1 & exam_date==first_cmvr_date_eye) %>%
  select(studyid, age, female, hivdx, timesincehiv, haart, new_cd4_count, uni_vi, bi_vi, uni_blind, bi_blind, 
         eye, exam_date, redcap_week, redcap_repeat_instance, new_cmv, second_eye, time_to_secondeye, first_eye, first_cmvr_date_patient, 
         rd_developed, time_to_rd, visit_laterality, percentretina, percentretina2, logmar, 
         mostpostzone, mostpostzone1, mostantzone, mostantzone2, new_vithaze, new_rd, induction, maintenance, observation, othertx, 
         time_in_cmvfu_eye, time_in_cmvfu_patient, starttime) %>%
  mutate(ltfu3=if_else(time_in_cmvfu_patient<70,1,0),
         ltfu6=if_else(time_in_cmvfu_patient<150,1,0),
         ltfu12=if_else(time_in_cmvfu_patient<300,1,0),
         time_to_rd_censor3m=if_else(time_to_rd>121, 121, as.numeric(time_to_rd)),
         rd_developed_censor3m=if_else(rd_developed==1 & time_to_rd<=121,1,0),
         time_to_secondeye_censor3m=if_else(time_to_secondeye>121, 121, as.numeric(time_to_secondeye)),
         second_eye_censor3m=if_else(second_eye==1 & time_to_secondeye<=121,1,0),
         cd4_lt50=if_else(new_cd4_count<50,1,0),
         cd4_lt50=if_else(is.na(new_cd4_count),0,cd4_lt50),
         cd4_lt100=if_else(new_cd4_count<100,1,0),
         cd4_lt100=if_else(is.na(new_cd4_count),0,cd4_lt100),
         va.2060=if_else(logmar>0.47712125471966,1,0),
         va.20400=if_else(logmar>1.301029995664,1,0)) %>%
  ungroup() 


  
#Then this just restricts to only the first eyes, which is what we want for 
firstcmv.data <- firstcmv.data.all %>%
  filter(first_eye==1)



tsactivity <- cmvrdatalong %>%
  filter(studyid %in% c("C135", "C23", "C148", "C44", "C145")) %>%
  select(redcap_week, eye, new_cmv, induction, maintenance, observation, othertx )
  

firstcmv.data.patient <- firstcmv.data.all %>%
  group_by(studyid) %>%
  mutate(time_to_firstrd=min(time_to_rd),
         anyrd=max(rd_developed),
         time_to_firstrd_censor3m=min(time_to_rd_censor3m),
         anyrd_censor3m=max(rd_developed_censor3m)) %>%
  filter((visit_laterality==2 & second_eye==0 & eye=="od_") | (visit_laterality==2 & second_eye==1 & first_eye==1) | visit_laterality==1) %>%
  ungroup()

# Overall population, for text only
firstcmv.data.patient %>% summarize(medianage=median(age),
                                    agep25=quantile(age, 1/4),
                                    agep75=quantile(age, 3/4))
xtabs(data=firstcmv.data.patient, ~female, addNA=TRUE)
xtabs(data=firstcmv.data.patient, ~haart, addNA=TRUE)
addmargins(xtabs(data=firstcmv.data.patient, ~ltfu3))
addmargins(xtabs(data=firstcmv.data.patient, ~ltfu6))
addmargins(xtabs(data=firstcmv.data.patient, ~ltfu12))
skim(firstcmv.data.patient$time_in_cmvfu_patient)

# Table 2: Baseline patient characteristics:
table2 <- firstcmv.data.patient %>%
  group_by(ltfu3) %>%
  summarize(count=n(),
            medianage=median(age),
            agep25=quantile(age, 1/4),
            agep75=quantile(age, 3/4),
            count.female=sum(female==1),
            prop.female=mean(female), 
            mediantimehiv=median(timesincehiv),
            timehivp25=quantile(timesincehiv, 1/4),
            timehivp75=quantile(timesincehiv, 3/4),           
            countcd4=sum(!is.na(new_cd4_count)),
            mediancd4=median(new_cd4_count, na.rm=TRUE),
            cd4p25=quantile(new_cd4_count, 1/4, na.rm=TRUE),
            cd4p75=quantile(new_cd4_count, 3/4, na.rm=TRUE),
            count.cd4lt50=sum(cd4_lt50==1, na.rm=TRUE),
            prop.cd4lt50=mean(cd4_lt50, na.rm=TRUE),
            count.cd4lt100=sum(cd4_lt100==1, na.rm=TRUE),
            prop.cd4lt100=mean(cd4_lt100, na.rm=TRUE),
            count.haart=sum(haart==1),
            prop.haart=mean(haart),
            count.unilateral=sum(visit_laterality==1),
            count.bilateral=sum(visit_laterality==2),
            count.univi=sum(uni_vi==1),
            prop.univi=mean(uni_vi==1),
            count.bivi=sum(bi_vi==1),
            prop.bivi=mean(bi_vi==1),
            count.uniblind=sum(uni_blind==1),
            prop.uniblind=mean(uni_blind==1),
            count.biblind=sum(bi_blind==1),
            prop.biblind=mean(bi_blind==1),
            count.rd=sum(new_rd==1),
            count.nord=sum(new_rd==0))


# P-values
firstcmv.data.notltfu.pt <- firstcmv.data.patient %>% filter(ltfu3==0)  
firstcmv.data.ltfu.pt <- firstcmv.data.patient %>% filter(ltfu3==1)  
wilcox.test(firstcmv.data.notltfu.pt$age, firstcmv.data.ltfu.pt$age)
wilcox.test(firstcmv.data.notltfu.pt$female, firstcmv.data.ltfu.pt$female)
wilcox.test(firstcmv.data.notltfu.pt$timesincehiv, firstcmv.data.ltfu.pt$timesincehiv)
wilcox.test(firstcmv.data.notltfu.pt$new_cd4_count, firstcmv.data.ltfu.pt$new_cd4_count)
wilcox.test(firstcmv.data.notltfu.pt$cd4_lt50, firstcmv.data.ltfu.pt$cd4_lt50)
wilcox.test(firstcmv.data.notltfu.pt$haart, firstcmv.data.ltfu.pt$haart)
wilcox.test(firstcmv.data.notltfu.pt$visit_laterality, firstcmv.data.ltfu.pt$visit_laterality)
wilcox.test(firstcmv.data.notltfu.pt$uni_vi, firstcmv.data.ltfu.pt$uni_vi)
wilcox.test(firstcmv.data.notltfu.pt$bi_vi, firstcmv.data.ltfu.pt$bi_vi)
wilcox.test(firstcmv.data.notltfu.pt$uni_blind, firstcmv.data.ltfu.pt$uni_blind)
wilcox.test(firstcmv.data.notltfu.pt$bi_blind, firstcmv.data.ltfu.pt$bi_blind)

# Table 3: Baseline eye characteristics
table3 <- firstcmv.data.all %>%
  group_by(ltfu3) %>%
  summarize(count=n(),
            active=sum(induction==1),
            maint=sum(maintenance==1),
            obs=sum(observation==1),
            postzone1=sum(mostpostzone==1),
            postzone2=sum(mostpostzone==2),
            postzone3=sum(mostpostzone==3),
            antzone1=sum(mostantzone==1),
            antzone2=sum(mostantzone==2),
            antzone3=sum(mostantzone==3),
            percentretina.lt10=sum(percentretina==1, na.rm=TRUE),
            percentretina.1025=sum(percentretina==2, na.rm=TRUE),
            percentretina.2650=sum(percentretina==3, na.rm=TRUE),
            percentretina.gt50=sum(percentretina==4, na.rm=TRUE),
            percentretina.cd=sum(percentretina==5, na.rm=TRUE),
            vithaze=sum(new_vithaze==1, na.rm=TRUE),
            novithaze=sum(new_vithaze==0, na.rm=TRUE),
            rd=sum(new_rd==1),
            nord=sum(new_rd==0),
            va.2060=sum(logmar>0.47712125471966),
            va.20400=sum(logmar>1.301029995664))

# Troubleshoot missing percent retina
# xtabs(data=firstcmv.data.all, ~percentretina, addNA=TRUE)
# xtabs(data=filter(cmvrdatalong, studyid=="C85" & eye=="od_"), ~exam_date + percentretina, addNA=TRUE)
prts <- cmvrdatalong %>%
  select(studyid, eye, exam_date, new_cmv, induction, maintenance, observation, cmv_zones___1, cmv_zones___2, cmv_zones___3, cmvr.a, cmvr.b, cmvr, pct_retina_involved.a, pct_retina_involved.b, pct_retina_involved, percentretina, newname) %>%
  filter(studyid=="C4")
# 
prts2 <- firstcmv.data.all %>%
  filter(is.na(percentretina))
# write_csv(prts2, "prts2.csv")

# P values
firstcmv.data.notltfu <- firstcmv.data.all %>% filter(ltfu3==0)  
firstcmv.data.ltfu <- firstcmv.data.all %>% filter(ltfu3==1)

fisher.test(xtabs(data=firstcmv.data.all, ~ va.2060+ltfu3))
fisher.test(xtabs(data=firstcmv.data.all, ~ va.20400+ltfu3))
fisher.test(xtabs(data=firstcmv.data.all, ~ induction+ltfu3))
fisher.test(xtabs(data=firstcmv.data.all, ~ mostpostzone+ltfu3))
fisher.test(xtabs(data=firstcmv.data.all, ~ mostantzone+ltfu3))
fisher.test(xtabs(data=firstcmv.data.all, ~ new_vithaze+ltfu3))
fisher.test(xtabs(data=firstcmv.data.all, ~ new_rd+ltfu3))
fisher.test(xtabs(data=firstcmv.data.all, ~ percentretina+ltfu3))



################################
###        SECOND EYE        ###
################################

# First check to see whether any of the second eyes are potential data error entry
# It's C128 was only a single visit and not treated. Assume data error entry but ask Louisa to check--> the next visit they said no CMV so assume charting error
# C74 happened after 3 months; has missing data for OS but not for OD--doublecheck. week 12-->LL confirmed no visit at week 12
# Need to doublecheck but assume this is a data error.
check <- cmvrdatalong %>%
  filter(studyid=="C128" | studyid=="C74" | second_eye==1) %>%
  select(studyid, eye, redcap_week, new_cmv, new_rd, second_eye, time_to_secondeye, induction, maintenance, observation, othertx) %>%
  arrange(studyid, eye, redcap_week)
firstcmv.data.all_censor3m <- firstcmv.data.all %>%
  filter()

# Second eye text
xtabs(data=firstcmv.data.patient, ~visit_laterality+second_eye, addNA=TRUE)

# Unilateral cases at baseline
firstcmv.data.patient.uni <- firstcmv.data.patient %>%
  filter(visit_laterality==1)
# Administrative censoring 3 months
secondeye_surv_object_censor3m <- with(firstcmv.data.patient.uni, Surv(time=starttime, time2 = as.numeric(time_to_secondeye_censor3m/30.4), event = second_eye_censor3m))
secondeye_surv_object_censor3m
summary(secondeye_surv_object_censor3m)
survRate(secondeye_surv_object_censor3m ~ 1, data=firstcmv.data.patient.uni)
# Table 5
# Regular Cox model
cox_secondeye_censor3m <- coxph(secondeye_surv_object_censor3m ~ age + female + (new_cd4_count < 100) + haart + induction + mostpostzone, data=firstcmv.data.patient.uni)
summary(cox_secondeye_censor3m)
cox_secondeye_censor3m.age <- coxph(secondeye_surv_object_censor3m ~ age, data=firstcmv.data.patient.uni)
summary(cox_secondeye_censor3m.age)
# Try firth, UNIVARIATE
# Setting up data
firstcmv.data.patient.uni.noltfu <- firstcmv.data.patient.uni %>% filter(time_to_secondeye>0)
secondeye_surv_object_censor3m.noltfu <- with(firstcmv.data.patient.uni.noltfu, Surv(time=starttime, time2 = as.numeric(time_to_secondeye_censor3m/30.4), event = second_eye_censor3m))
survRate(secondeye_surv_object_censor3m.noltfu ~ 1, data=firstcmv.data.patient.uni.noltfu)

# Univariable Models
# Age
cox_secondeye_censor3m.age.firth <- coxphf(firstcmv.data.patient.uni.noltfu, formula=secondeye_surv_object_censor3m.noltfu ~ age)
cbind(exp(cox_secondeye_censor3m.age.firth$coefficients), cox_secondeye_censor3m.age.firth$ci.lower, cox_secondeye_censor3m.age.firth$ci.upper, cox_secondeye_censor3m.age.firth$prob)
# Female
cox_secondeye_censor3m.sex.firth <- coxphf(firstcmv.data.patient.uni.noltfu, formula=secondeye_surv_object_censor3m.noltfu ~ female)
cbind(exp(cox_secondeye_censor3m.sex.firth$coefficients), cox_secondeye_censor3m.sex.firth$ci.lower, cox_secondeye_censor3m.sex.firth$ci.upper, cox_secondeye_censor3m.sex.firth$prob)
# CD4 count
cox_secondeye_censor3m.cd4_lt50.firth <- coxphf(firstcmv.data.patient.uni.noltfu, formula=secondeye_surv_object_censor3m.noltfu ~ cd4_lt50)
cbind(exp(cox_secondeye_censor3m.cd4_lt50.firth$coefficients), cox_secondeye_censor3m.cd4_lt50.firth$ci.lower, cox_secondeye_censor3m.cd4_lt50.firth$ci.upper, cox_secondeye_censor3m.cd4_lt50.firth$prob)
# HAART
cox_secondeye_censor3m.haart.firth <- coxphf(firstcmv.data.patient.uni.noltfu, formula=secondeye_surv_object_censor3m.noltfu ~ haart)
cbind(exp(cox_secondeye_censor3m.haart.firth$coefficients), cox_secondeye_censor3m.haart.firth$ci.lower, cox_secondeye_censor3m.haart.firth$ci.upper, cox_secondeye_censor3m.haart.firth$prob)
# Active retinitis
cox_secondeye_censor3m.induction.firth <- coxphf(firstcmv.data.patient.uni.noltfu, formula=secondeye_surv_object_censor3m.noltfu ~ induction)
cbind(exp(cox_secondeye_censor3m.induction.firth$coefficients), cox_secondeye_censor3m.induction.firth$ci.lower, cox_secondeye_censor3m.induction.firth$ci.upper, cox_secondeye_censor3m.induction.firth$prob)
# Most posterior zone
cox_secondeye_censor3m.mostpostzone.firth <- coxphf(firstcmv.data.patient.uni.noltfu, formula=secondeye_surv_object_censor3m.noltfu ~ mostpostzone1)
cbind(exp(cox_secondeye_censor3m.mostpostzone.firth$coefficients), cox_secondeye_censor3m.mostpostzone.firth$ci.lower, cox_secondeye_censor3m.mostpostzone.firth$ci.upper, cox_secondeye_censor3m.mostpostzone.firth$prob)
# Most anterior zone
cox_secondeye_censor3m.mostantzone.firth <- coxphf(firstcmv.data.patient.uni.noltfu, formula=secondeye_surv_object_censor3m.noltfu ~ mostantzone2)
cbind(exp(cox_secondeye_censor3m.mostantzone.firth$coefficients), cox_secondeye_censor3m.mostantzone.firth$ci.lower, cox_secondeye_censor3m.mostantzone.firth$ci.upper, cox_secondeye_censor3m.mostantzone.firth$prob)
# percent retina
cox_secondeye_censor3m.size.firth <- coxphf(firstcmv.data.patient.uni.noltfu, formula=secondeye_surv_object_censor3m.noltfu ~ percentretina2)
cbind(exp(cox_secondeye_censor3m.size.firth$coefficients), cox_secondeye_censor3m.size.firth$ci.lower, cox_secondeye_censor3m.size.firth$ci.upper, cox_secondeye_censor3m.size.firth$prob)
# VA2060
cox_secondeye_censor3m.va2060.firth <- coxphf(firstcmv.data.patient.uni.noltfu, formula=secondeye_surv_object_censor3m.noltfu ~ va.2060)
cbind(exp(cox_secondeye_censor3m.va2060.firth$coefficients), cox_secondeye_censor3m.va2060.firth$ci.lower, cox_secondeye_censor3m.va2060.firth$ci.upper, cox_secondeye_censor3m.va2060.firth$prob)
# VA20400
cox_secondeye_censor3m.va20400.firth <- coxphf(firstcmv.data.patient.uni.noltfu, formula=secondeye_surv_object_censor3m.noltfu ~ va.20400)
cbind(exp(cox_secondeye_censor3m.va20400.firth$coefficients), cox_secondeye_censor3m.va20400.firth$ci.lower, cox_secondeye_censor3m.va20400.firth$ci.upper, cox_secondeye_censor3m.va20400.firth$prob)

# FULL MODEL
cox_secondeye_censor3m.firth <- coxphf(firstcmv.data.patient.uni.noltfu, formula=secondeye_surv_object_censor3m.noltfu ~ age + female + cd4_lt50 + haart + induction + mostpostzone1 + mostantzone2 + percentretina2)
summary(cox_secondeye_censor3m.firth)
cbind(exp(cox_secondeye_censor3m.firth$coefficients), 
          cox_secondeye_censor3m.firth$ci.lower,
          cox_secondeye_censor3m.firth$ci.upper,
          cox_secondeye_censor3m.firth$prob)
# NUMBERS IN TABLE
table4 <- firstcmv.data.patient.uni %>%
  group_by(second_eye_censor3m) %>%
  summarize(count=n(),
            meanage=mean(age),
            sdage=sd(age),
            female.ct=sum(female==1),
            cd4_lt50.ct=sum(cd4_lt50==1),
            haart.ct=sum(haart==1),
            active.ct=sum(induction==1),
            mostpostz1=sum(mostpostzone1==1),
            mostpostz23=sum(mostpostzone1==0),
            mostantzone2z12=sum(mostantzone2==2),
            mostantzone2z3=sum(mostantzone2==3),
            percentretina12=sum(percentretina2==2),
            percentretina34=sum(percentretina2==3))


# Sensitivity without firth
cox_secondeye_censor3m.age.noltfu <- coxph(secondeye_surv_object_censor3m.noltfu ~ age, data=firstcmv.data.patient.uni.noltfu)
summary(cox_secondeye_censor3m.age.noltfu)
cox_secondeye_censor3m.ind.noltfu <- coxph(secondeye_surv_object_censor3m.noltfu ~ induction, data=firstcmv.data.patient.uni.noltfu)
summary(cox_secondeye_censor3m.ind.noltfu)

# Administrative censoring 12 months
secondeye_surv_object.noltfu <- with(firstcmv.data.patient.uni.noltfu, Surv(time=starttime, time2 = as.numeric(time_to_secondeye/30.4), event = second_eye))
secondeye_surv_object.noltfu
summary(secondeye_surv_object.noltfu)
survRate(secondeye_surv_object.noltfu ~ 1, data=firstcmv.data.patient.uni)
# time to second eye:
firstcmv.data.patient.uni.noltfu %>% filter(second_eye==1) %>% 
  summarize(medtime=median(time_to_secondeye),
            p25time=quantile(time_to_secondeye, 0.25),
            p75time=quantile(time_to_secondeye, 0.75))

# FULL MODEL
cox_secondeye_censor12m.firth <- coxphf(firstcmv.data.patient.uni.noltfu, formula=secondeye_surv_object.noltfu ~ age + female + cd4_lt50 + haart + induction + mostpostzone1 + mostantzone2 + percentretina2)
summary(cox_secondeye_censor12m.firth)
cbind(exp(cox_secondeye_censor12m.firth$coefficients), 
      cox_secondeye_censor12m.firth$ci.lower,
      cox_secondeye_censor12m.firth$ci.upper,
      cox_secondeye_censor12m.firth$prob)

# Full model, censored at 12 months
cox_secondeye.firth <- coxphf(firstcmv.data.patient.uni.noltfu, formula=secondeye_surv_object.noltfu ~ age + female + cd4_lt50 + haart + induction + as.factor(mostpostzone))
summary(cox_secondeye.firth)

table.s1.se <- firstcmv.data.patient.uni %>%
  group_by(second_eye) %>%
  summarize(count=n(),
            meanage=mean(age),
            sdage=sd(age),
            female.ct=sum(female==1),
            cd4_lt50.ct=sum(cd4_lt50==1),
            haart.ct=sum(haart==1),
            active.ct=sum(induction==1),
            mostpostz1=sum(mostpostzone1==1),
            mostpostz23=sum(mostpostzone1==0),
            mostantz12=sum(mostantzone2==2),
            mostantz3=sum(mostantzone2==3),
            percentretina12=sum(percentretina2==2),
            percentretina34=sum(percentretina2==3))

########################################
###        RETINAL DETACHMENT        ###
########################################

# RD text
xtabs(data=firstcmv.data.all, ~new_rd+first_eye, addNA=TRUE)
xtabs(data=firstcmv.data.all, ~new_rd+rd_developed, addNA=TRUE)

# RD check
# rdcheck <- cmvrdatalong %>% 
#   select(studyid, hivdx, new_cmv, exam_date, first_cmvr_date_eye, first_eye, eye, redcap_week, new_cmv, new_rd, rd_developed, time_to_rd) %>%
#   filter(studyid=="C74") %>%
#   arrange(studyid, eye, redcap_week)

# Administrative censoring 3 months
firstcmv.data.all.nord <- firstcmv.data.all %>%
  filter(new_rd==0 & time_to_rd_censor3m>0)
rd_surv_object_censor3m <- with(firstcmv.data.all.nord, Surv(time=starttime, time2 = as.numeric(time_to_rd_censor3m/30.4), event = rd_developed_censor3m))
rd_surv_object_censor3m
summary(rd_surv_object_censor3m)
survRate(rd_surv_object_censor3m ~ 1, data=firstcmv.data.all.nord)
# PATIENT-LEVEL INCIDENCE
# Administrative censoring 3 months
ptrd_surv_object_censor3m <- with(firstcmv.data.patient, Surv(time=starttime, time2 = as.numeric(time_to_firstrd_censor3m/30.4), event = anyrd_censor3m))
ptrd_surv_object_censor3m
summary(ptrd_surv_object_censor3m)
survRate(ptrd_surv_object_censor3m ~ 1, data=firstcmv.data.patient)
# Administrative censoring 12 months
ptrd_surv_object_censor12m <- with(firstcmv.data.patient, Surv(time=starttime, time2 = as.numeric(time_to_firstrd/30.4), event = anyrd))
ptrd_surv_object_censor12m
summary(ptrd_surv_object_censor12m)
survRate(ptrd_surv_object_censor12m ~ 1, data=firstcmv.data.patient)

# THESE COX MODELS ACCOUNT FOR CLUSTERED DATA
# Age
cox_rd_censor3m.age <- coxph(rd_surv_object_censor3m ~ age + cluster(studyid), data=firstcmv.data.all.nord)
cox_rd_censor3m.age.tidy <- tidy(cox_rd_censor3m.age)
cbind(exp(cox_rd_censor3m.age.tidy$estimate), exp(cox_rd_censor3m.age.tidy$conf.low), exp(cox_rd_censor3m.age.tidy$conf.high), cox_rd_censor3m.age.tidy$p.value)
# Female
cox_rd_censor3m.female <- coxph(rd_surv_object_censor3m ~ female + cluster(studyid), data=firstcmv.data.all.nord)
cox_rd_censor3m.female.tidy <- tidy(cox_rd_censor3m.female)
cbind(exp(cox_rd_censor3m.female.tidy$estimate), exp(cox_rd_censor3m.female.tidy$conf.low), exp(cox_rd_censor3m.female.tidy$conf.high), cox_rd_censor3m.female.tidy$p.value)
# CD4<50
cox_rd_censor3m.cd4_lt50 <- coxph(rd_surv_object_censor3m ~ cd4_lt50 + cluster(studyid), data=firstcmv.data.all.nord)
cox_rd_censor3m.cd4_lt50.tidy <- tidy(cox_rd_censor3m.cd4_lt50)
cbind(exp(cox_rd_censor3m.cd4_lt50.tidy$estimate), exp(cox_rd_censor3m.cd4_lt50.tidy$conf.low), exp(cox_rd_censor3m.cd4_lt50.tidy$conf.high), cox_rd_censor3m.cd4_lt50.tidy$p.value)
# HAART
cox_rd_censor3m.haart <- coxph(rd_surv_object_censor3m ~ haart + cluster(studyid), data=firstcmv.data.all.nord)
cox_rd_censor3m.haart.tidy <- tidy(cox_rd_censor3m.haart)
cbind(exp(cox_rd_censor3m.haart.tidy$estimate), exp(cox_rd_censor3m.haart.tidy$conf.low), exp(cox_rd_censor3m.haart.tidy$conf.high), cox_rd_censor3m.haart.tidy$p.value)
# MOST POST ZONE
cox_rd_censor3m.mostpost <- coxph(rd_surv_object_censor3m ~ mostpostzone1 + cluster(studyid), data=firstcmv.data.all.nord)
cox_rd_censor3m.mostpost.tidy <- tidy(cox_rd_censor3m.mostpost)
cbind(exp(cox_rd_censor3m.mostpost.tidy$estimate), exp(cox_rd_censor3m.mostpost.tidy$conf.low), exp(cox_rd_censor3m.mostpost.tidy$conf.high), cox_rd_censor3m.mostpost.tidy$p.value)
# MOST ANT ZONE
cox_rd_censor3m.mostant <- coxph(rd_surv_object_censor3m ~ mostantzone2 + cluster(studyid), data=firstcmv.data.all.nord)
cox_rd_censor3m.mostant.tidy <- tidy(cox_rd_censor3m.mostant)
cbind(exp(cox_rd_censor3m.mostant.tidy$estimate), exp(cox_rd_censor3m.mostant.tidy$conf.low), exp(cox_rd_censor3m.mostant.tidy$conf.high), cox_rd_censor3m.mostant.tidy$p.value)
# PERCENT RETINA
cox_rd_censor3m.size <- coxph(rd_surv_object_censor3m ~ percentretina2 + cluster(studyid), data=firstcmv.data.all.nord)
cox_rd_censor3m.size.tidy <- tidy(cox_rd_censor3m.size)
cbind(exp(cox_rd_censor3m.size.tidy$estimate), exp(cox_rd_censor3m.size.tidy$conf.low), exp(cox_rd_censor3m.size.tidy$conf.high), cox_rd_censor3m.size.tidy$p.value)
# THESE HAVE ZERO CELLS SO USED FIRTH WITHOUT CLUSTERING
# Active
cox_rd_censor3m.induction.firth <- coxphf(firstcmv.data.all.nord, formula=rd_surv_object_censor3m ~ induction)
cbind(exp(cox_rd_censor3m.induction.firth$coefficients), cox_rd_censor3m.induction.firth$ci.lower, cox_rd_censor3m.induction.firth$ci.upper, cox_rd_censor3m.induction.firth$prob)
# FULL MODEL
cox_rd_censor3m.mostpostzone.firth <- coxphf(firstcmv.data.all.nord, formula=rd_surv_object_censor3m ~ age + female + cd4_lt50 + haart + induction + mostpostzone1 + mostantzone2 + percentretina2)
cbind(exp(cox_rd_censor3m.mostpostzone.firth$coefficients), cox_rd_censor3m.mostpostzone.firth$ci.lower, cox_rd_censor3m.mostpostzone.firth$ci.upper, cox_rd_censor3m.mostpostzone.firth$prob)


# NUMBERS IN TABLE
table5 <- firstcmv.data.all %>%
  group_by(rd_developed_censor3m) %>%
  summarize(count=n(),
            meanage=mean(age),
            sdage=sd(age),
            female.ct=sum(female==1),
            cd4_lt50.ct=sum(cd4_lt50==1),
            haart.ct=sum(haart==1),
            active.ct=sum(induction==1),
            mostpostz1=sum(mostpostzone1==1),
            mostpostz23=sum(mostpostzone1==0),
            mostantz12=sum(mostantzone2==2),
            mostantz3=sum(mostantzone2==3),
            percentretina12=sum(percentretina2==2),
            percentretina34=sum(percentretina2==3))

# FULL MODEL - 12 month censoring
# Administrative censoring 3 months
rd_surv_object_censor12m <- with(firstcmv.data.all.nord, Surv(time=starttime, time2 = as.numeric(time_to_rd/30.4), event = rd_developed))
rd_surv_object_censor12m
summary(rd_surv_object_censor12m)
survRate(rd_surv_object_censor12m ~ 1, data=firstcmv.data.all.nord)
cox_rd_censor12m.mostpostzone.firth <- coxphf(firstcmv.data.all.nord, formula=rd_surv_object_censor12m ~ age + female + cd4_lt50 + haart + induction + mostpostzone1 + mostantzone2 + percentretina2)
cbind(exp(cox_rd_censor12m.mostpostzone.firth$coefficients), cox_rd_censor12m.mostpostzone.firth$ci.lower, cox_rd_censor12m.mostpostzone.firth$ci.upper, cox_rd_censor12m.mostpostzone.firth$prob)

# time to RD:
firstcmv.data.all.nord %>% filter(rd_developed==1) %>% 
  summarize(medtime=median(time_to_rd),
            p25time=quantile(time_to_rd, 0.25),
            p75time=quantile(time_to_rd, 0.75))

table.s1.rd <- firstcmv.data.all %>%
  group_by(rd_developed) %>%
  summarize(count=n(),
            meanage=mean(age),
            sdage=sd(age),
            female.ct=sum(female==1),
            cd4_lt50.ct=sum(cd4_lt50==1),
            haart.ct=sum(haart==1),
            active.ct=sum(induction==1),
            mostpostz1=sum(mostpostzone1==1),
            mostpostz23=sum(mostpostzone1==0),
            mostantz12=sum(mostantzone2==2),
            mostantz3=sum(mostantzone2==3),
            percentretina12=sum(percentretina2==2),
            percentretina34=sum(percentretina2==3))

# KAPLAN-MEIER CURVES
library(grid)
library(gridExtra)
library(ggplotify)
splots <- list()
secondeyecurve <- survfit(secondeye_surv_object.noltfu ~ 1, data = firstcmv.data.patient.uni.noltfu)
splots[[1]] <- ggsurvplot(secondeyecurve, data = firstcmv.data.patient.uni.noltfu, 
           conf.int = TRUE, 
           fun = "event",
           conf.int.style = "step",  # customize style of confidence intervals
           xlab = "Months",
           ylab = "Second eye involvement (%)",
           break.time.by = 3,     # break X axis in time intervals by 200.
           ggtheme = theme_classic())
rdcurve <- survfit(rd_surv_object_censor12m ~ 1, data = firstcmv.data.all.nord)
splots[[2]] <- ggsurvplot(rdcurve, data = firstcmv.data.all.nord, 
           conf.int = TRUE, 
           fun = "event",
           conf.int.style = "step",  # customize style of confidence intervals
           xlab = "Months",
           ylab = "Retinal detachment (%)",
           break.time.by = 3,     # break X axis in time intervals by 200.
           ggtheme = theme_classic())
fig <- arrange_ggsurvplots(splots, print = TRUE, ncol = 2, nrow = 1)
ggsave("CMV_KM.svg", fig)


# Figuring out people to adjudicate for percent retina
xtabs(data=filter(cmvrdatalong.prog, is.na(pct_retina_involved) & redcap_week==0 & pct_retina_involved.a!=pct_retina_involved.b), ~o.originalname)
addmargins(xtabs(data=filter(cmvrdatalong, studyid=="C136"), ~redcap_week+eye, addNA=TRUE))

# C124 (four) --> 2 AND 3 ARE THE SAME
# C32 (three)
# C113 (three)
# C136 (three)


###################################################
#########                                 #########
#########         QUALITY OF LIFE         #########
#########                                 #########
###################################################

addmargins(xtabs(data=filter(cmvrdatalong, eye=="od_" & redcap_week==0), ~studyid+ visit_laterality))
qol0data <- cmvrdatalong %>%
  filter(eye=="od_" & redcap_week==0) %>%
  mutate(anycmvr=if_else(visit_laterality>0, 1, 0))

qol_table1 <- qol0data %>%
  group_by(anycmvr) %>%
  summarize(mean_eq5=mean(eq5score),
            sd_eq5=sd(eq5score),
            count_eq5=sum(!is.na(eq5score)),
            mean_age=mean(age),
            sd_age=sd(age),
            count_age=sum(!is.na(age)),
            num_female=sum(female==1),
            count_female=sum(!is.na(female)),
            prop_female=num_female/count_female) %>%
  # PLEASE ADD TO THIS BASELINE TABLE; VISION? CD4? TIME SINCE HIV?
  gather(field, value, mean_eq5:prop_female) %>%
  separate(field, into=c("stat", "var"), sep="_") %>%
  mutate(groupstat=paste(anycmvr, stat, sep="_")) %>%
  select(-stat, -anycmvr) %>%
  spread(groupstat, value, convert=TRUE)

# GETTING CONFIDENCE INTERVALS, ANYCMVR
model1_nocmv <- lm(eq5score ~ anycmvr, data=qol0data)
# Note that you can get the mean and 95%CI for the anycmvr==0 group from the summary and confint, looking at intercept
summary(model1_nocmv)
confint(model1_nocmv)
# Easy way to get mean/CI of the anycmvr==1 group is to make the reference level be "1", then just read off the intercept:
model1_cmv <- lm(eq5score ~ factor(anycmvr, levels=c("1", "0")), data=qol0data)
summary(model1_cmv)
confint(model1_cmv)

# WHAT ABOUT LATERALITY?
# Note we need to specify it's a factor because otherwise treats as a continuous variable
model1_lat <- lm(eq5score ~ factor(visit_laterality), data=qol0data)
# To get omnibus p-value for the laterality categorical variable, use a likelihood ratio test
# Basically make 2 models. One with the categorical variable and one without. Then run an ANOVA comparing the 2 models.
# So this is the model without the visit_laterality term
model1_nolat <- lm(eq5score ~ 1, data=qol0data)
# And get the p-value from this ANOVA
anova(model1_lat, model1_nolat)
# And the means/CIs for the uni vs bilateral:
model1_lat_uni <- lm(eq5score ~ factor(visit_laterality, levels=c("1", "0", "2")), data=qol0data)
summary(model1_lat_uni)
model1_lat_bi <- lm(eq5score ~ factor(visit_laterality, levels=c("2", "0", "1")), data=qol0data)
summary(model1_lat_bi)
# So the bilateral ones didn't have worse QOL...

# LONGITUDINAL?
addmargins(xtabs(data=cmvrdatalong, ~eq5score+redcap_week))
# So month 6 seems best...

# THIS NOT CORRECT YET; NEED TO EXCLUDE PEOPLE WHO SUBSEQUENTLY DEVELOPED CMVR; OR MAYBE HAVE THEM SWITCH GROUPS OR SOMETHING
qol_longitudinaldata <- cmvrdatalong %>%
  filter(eye=="od_") %>%
  group_by(studyid) %>%
  mutate(baselinecmvr_work=if_else(visit_laterality>0 & redcap_week==0, 1, 0),
         baselinecmvr=max(baselinecmvr_work, na.rm=TRUE),
         baselineeq5_work=if_else(redcap_week==0, eq5score, NA_real_),
         baselineeq5=max(baselineeq5_work, na.rm=TRUE))

qol_longitudinaltable <- qol_longitudinaldata %>%
  filter(redcap_week %in% c(0,24)) %>%
  group_by(baselinecmvr, redcap_week) %>%
  summarize(mean_eq5=mean(eq5score, na.rm=TRUE),
            count_eq5=sum(!is.na(eq5score)))

library(lme4)  
model_long_int <- lmer(eq5score ~ redcap_week*baselinecmvr + (1|studyid), data=filter(qol_longitudinaldata, redcap_week %in% c(0,24)))
summary(model_long_int)
# Interaction term not significant; this would indicate that rate of change is different between the two groups
# So just do a simple linear regression adjusting for baseline values
model_long24 <- lm(eq5score ~ baselinecmvr + baselineeq5, data=filter(qol_longitudinaldata, redcap_week==24))
summary(model_long24)



##  JK STOPPED HERE  ##





| (studyid %in% c("C19", "C33", "C40", "C43", "C58", "C59", "C74", "C111")) 

         # logmar_firsteye_working = ifelse(first_eye == 1 & exam_date==first_cmvr_date_eye, logmar, NA),
         # logmar_firsteye = min(logmar_firsteye_working, na.rm = TRUE),
         # 
         # cmv_zones1_firsteye_working =ifelse(first_eye == 1 & exam_date==first_cmvr_date_eye, cmv_zones___1, NA),
         # cmv_zones1_firsteye = max(cmv_zones1_firsteye_working, na.rm = TRUE),
         # 
         # cmv_zones2_firsteye_working =ifelse(first_eye == 1 & exam_date==first_cmvr_date_eye, cmv_zones___2, NA),
         # cmv_zones2_firsteye = max(cmv_zones2_firsteye_working, na.rm = TRUE),
         # 
         # cmv_zones3_firsteye_working =ifelse(first_eye == 1 & exam_date==first_cmvr_date_eye, cmv_zones___3, NA),
         # cmv_zones3_firsteye = max(cmv_zones3_firsteye_working, na.rm = TRUE),
         # 
         # mostpostzone_firsteye = ifelse(cmv_zones1_firsteye==1, 1,
         #                                ifelse(cmv_zones2_firsteye==1, 2,
         #                                       ifelse(cmv_zones3_firsteye==1, 3, NA))),
         # 
         # activecmvr_firsteye_working = ifelse(first_eye == 1 & exam_date==first_cmvr_date_eye, new_cmv, NA),
         # activecmvr_firsteye = max(activecmvr_firsteye_working, na.rm = TRUE),
         # 
         # vithaze_firsteye_working = ifelse(first_eye == 1 & exam_date==first_cmvr_date_eye, new_vithaze, NA),
         # vithaze_firsteye = max(vithaze_firsteye_working, na.rm = TRUE),
         # 
         # maced_firsteye_working = ifelse(first_eye == 1 & exam_date==first_cmvr_date_eye, new_maced, NA),
         # maced_firsteye = max(maced_firsteye_working, na.rm = TRUE),
         # 
         # rd_firsteye_working = ifelse(first_eye == 1 & exam_date==first_cmvr_date_eye, new_rd, NA),
         # rd_firsteye = max(rd_firsteye_working, na.rm = TRUE),
         # 
         # cd4_firsteye_working = ifelse(first_eye == 1 & exam_date==first_cmvr_date_eye, new_cd4_count, NA),
         # cd4_firsteye = max(rd_firsteye_working, na.rm = TRUE),
         # 
         # tx1_firsteye_working = ifelse(first_eye == 1 & exam_date==first_cmvr_date_eye, tx___1, NA),
         # tx1_firsteye = max(tx1_firsteye_working, na.rm = TRUE),
         # 
         # tx2_firsteye_working = ifelse(first_eye == 1 & exam_date==first_cmvr_date_eye, tx___2, NA),
         # tx2_firsteye = max(tx2_firsteye_working, na.rm = TRUE),
         # 
         # tx3_firsteye_working = ifelse(first_eye == 1 & exam_date==first_cmvr_date_eye, tx___3, NA),
         # tx3_firsteye = max(tx3_firsteye_working, na.rm = TRUE),
         # 
         # tx4_firsteye_working = ifelse(first_eye == 1 & exam_date==first_cmvr_date_eye, tx___4, NA),
         # tx4_firsteye = max(tx4_firsteye_working, na.rm = TRUE))


xtabs(data=cmvrdatalong, ~time_to_rd, addNA=TRUE)
ts <- cmvrdatalong %>% filter(second_eye==0) %>% select(studyid, eye, new_cmv, new_rd, exam_date, 
                                                        first_cmvr_date_eye, second_eye, first_cmvr_date_working, first_cmvr_date_patient, 
                                                        time_to_secondeye, diffpatienteye, time_in_cmvfu_patient, first_eye)

############################
#     DATA PREPPING        #
############################

#VARIABLES NEEDED FOR RD SURVIVAL ANALYSIS: 
# 1) rd_developed (1 if yes, 0 if no)
# 2) time_to_rd (days between date of RD OR last exam date...& first_cmvr_date (starting point for time to rd) 

rdcheck <- cmvrdatalong %>%
  filter(studyid  == 'C79') %>%
  select(studyid_eye_week, studyid, eye, redcap_week, exam_date, new_cmv, first_cmvr_date_patient, first_cmvr_date_eye, baseline_rd, new_rd, rd_developed)

#time_to_rd (days between first_cmvr_date & date of RD OR last exam date if eye never dev'd RD
#(note: time_to_rd = 0 if eye that had RD at baseline)

view_data_by_studyid <- cmvrdatalong %>%
  filter(studyid=="C99") %>%
  arrange(redcap_week) %>%
  select(studyid_eye_week, studyid, eye, redcap_week, new_cmv, baseline_rd, new_rd, first_cmvr_date_eye, first_cmvr_date_patient, exam_date, time_to_rd)
view_data_by_studyid
#C99: neither eye w/CMV @ baseline or ever, no RD developed ever
#C125: 1 eye w/CMV @ baseline that eventually dev RD, 1 eye without CMV that does NOT dev RD
#C115: RD @ baseline 


#^pts with time_since_firstcmvr == 0 (& thus time_to_secondeyecmvr == 0) are pts who had NO f/u exam dates: C10, 102, 141, 2, 30, 52, 54


view_data_by_studyid <- cmvrdatalong %>%
  filter(studyid=="C99") %>%
  arrange(redcap_week) %>%
  select(studyid_eye_week, studyid, eye, redcap_week, new_cmv, first_cmvr_date_eye, first_cmvr_date_patient, exam_date, time_since_firstcmvr, time_to_secondeyecmvr)
view_data_by_studyid

# Making variables to help make an indicator for second eye involvement...
#od_first_cmvr_date & os_first_cmvr_date
#min_od_cmvr_date & min_os_cmvr_date
# cmvrdatalong <- cmvrdatalong %>%
#   mutate(od_first_cmvr_date_working = ifelse(eye == "od_", first_cmvr_date_eye, NA),
#          od_first_cmvr_date = max(od_first_cmvr_date_working, na.rm = TRUE),
#          os_first_cmvr_date_working = ifelse(eye == "os_", first_cmvr_date_eye, NA),
#          os_first_cmvr_date = max(os_first_cmvr_date_working, na.rm = TRUE),
#          min_od_cmvr_date = ifelse(is.finite(od_first_cmvr_date), min_od_cmvr_date, NA),
#          min_od_cmvr_date = min(od_first_cmvr_date, na.rm = TRUE),
#          min_os_cmvr_date = ifelse(is.finite(os_first_cmvr_date), min_os_cmvr_date, NA),
#          min_os_cmvr_date = min(os_first_cmvr_date, na.rm = TRUE))

# view_data_by_studyid <- cmvrdatalong %>%
#   filter(studyid=="C100") %>%
#   arrange(redcap_week) %>%
#   select(studyid_eye_week, studyid, eye, redcap_week, new_cmv, first_cmvr_date_eye, od_first_cmvr_date_working, od_first_cmvr_date, min_od_cmvr_date, os_first_cmvr_date_working, os_first_cmvr_date, min_os_cmvr_date)
# view_data_by_studyid
# 
# xtabs(data = cmvrdatalong, ~min_od_cmvr_date, addNA = TRUE)
# xtabs(data = cmvrdatalong, ~min_os_cmvr_date, addNA = TRUE)


# view_data_by_studyid <- cmvrdatalong %>%
#   filter(studyid=="C111") %>%
#   arrange(redcap_week) %>%
#   group_by(studyid, eye) %>%
#   select(studyid_eye_week, studyid, eye, redcap_week, new_cmv, secondeyecmvr_developed, min_od_cmvr_date, min_os_cmvr_date)
# view_data_by_studyid

# secondeye_check <- cmvrdatalong %>%
#   filter(secondeyecmvr_developed == 1) %>%
#   group_by(studyid, eye) %>%
#   arrange(redcap_week) %>%
#   select(studyid_eye_week, studyid, eye, redcap_week, new_cmv, secondeyecmvr_developed, min_od_cmvr_date, min_os_cmvr_date)



view_data_by_studyid <- cmvrdatalong %>%
  filter(studyid=="C33") %>%
  arrange(redcap_week) %>%
  select(studyid_eye_week, studyid, eye, redcap_week, new_cmv, secondeyecmvr_developed, first_cmvr_date_patient, first_cmvr_date_eye, exam_date, time_to_secondeyecmvr, time_since_firstcmvr)
view_data_by_studyid
#C112: no CMV @ baseline, then never develops CMV
#C33: no CMV @ baseline, then developed unilateral then bilateral CMV on sequential dates
#43: no CMV @ baseline, then developed unilateral CMV only
#no patients: No CMV @ baseline, then bilateral CMV on same date
#C29: unilateral CMV @ baseline, then remains unilateral
#C111: unilateral CMV @ baseline, then developed bilateral CMV
#C121: bilateral CMV @ baseline


#modify dataset so that eye level data for variables of interest are available on the one line for patient's firstcmvrdate
#single eye, fill in; if bilat involvement, just take first eye*****
#similar code as logmar_firsteye for other variables ...eg, zones (use most posterior; if zones 1 & 2, just make it zone 1), rd, etc...


  
# #JK checking if the 2 variables are the same
# ggplot(data=cmvrdatalong, aes(x=logmar_firsteye, y=logmar_firsteyejk)) + geom_point()
# xtabs(data=filter(cmvrdatalong,logmar_firsteye!=logmar_firsteyejk), ~studyid)
# xtabs(data=cmvrdatalong, ~cmv_zones1_firsteye+cmv_zones1_firsteyejk, addNA=TRUE)
# xtabs(data=filter(cmvrdatalong, cmv_zones1_firsteye==0 & cmv_zones1_firsteyejk==1), ~studyid, addNA=TRUE)





# view_data_by_studyid <- cmvrdatalong %>%
#   filter(studyid=="C103") %>%
#   arrange(redcap_week) %>%
#   select(studyid_eye_week, studyid, eye, redcap_week, secondeyecmvr_developed, time_to_secondeyecmvr, logmar_firsteye_working, logmar_firsteye, cmv_zones1_firsteye, cmv_zones2_firsteye, cmv_zones3_firsteye, cmv_maxzone, induction_tx_firsteye, maintenance_tx_firsteye, observation_tx_firsteye, other_tx_firsteye, symp_firsteye, blurry_symp_firsteye, flashes_symp_firsteye, floaters_symp_firsteye, scotoma_symp_firsteye, other_symp_firsteye, rd_firsteye)
# view_data_by_studyid



#DATASET NEEDED FOR SECONDEYECMV SURVIVAL ANALYSIS @ patient level...
#each patient has one line...
#patients are those with UNILATERAL cmv at baseline...

# #new dataset: cmvrdatalong_firstcmvrdate. each patient has 2 lines, one for each eye
# cmvrdatalong_firstcmvrdate <- cmvrdatalong %>%
#   filter(first_cmvr_date_eye == exam_date) 
# 
# view_data_by_studyid <- cmvrdatalong_firstcmvrdate %>%
#   filter(studyid=="C128") %>%
#   arrange(redcap_week) %>%
#   select(studyid_eye_week, studyid, eye, first_eye, redcap_week, new_cmv, first_cmvr_date_patient, first_cmvr_date_eye, exam_date, time_since_firstcmvr, secondeyecmvr_developed, time_to_secondeyecmvr)
# view_data_by_studyid
# 
# 
# #new dataset: cmvrdatalong_unilatcmv. only patients with unilateral CMVR (at firstcmvrdate) who then develop 2nd eye involvement
# cmvrdatalong_firstcmvrdate <- cmvrdatalong_firstcmvrdate %>%
#   group_by(studyid) %>%
#   mutate(mean_firstcmvrdate = mean(first_cmvr_date_eye))
# 
# xtabs(data = cmvrdatalong_firstcmvrdate, ~mean_firstcmvrdate+studyid, addNA = TRUE)
# 
# cmvrdatalong_unilatcmv <- cmvrdatalong_firstcmvrdate %>%
#   filter((!(mean_firstcmvrdate == first_cmvr_date_eye) & !(secondeyecmvr_developed == 0))) %>%
#   filter(first_eye == 1 | is.na(first_eye))



dataforsecondeye <- cmvrdatalong %>%
  filter(first_eye==1 & cmvlat=="Unilateral" & first_cmvr_date_patient == exam_date)



# view_data_by_studyid <- cmvrdatalong_unilatcmv %>%
#   filter(studyid=="C103") %>%
#   arrange(redcap_week) %>%
#   select(studyid_eye_week, studyid, eye, redcap_week, new_cmv, secondeyecmvr_developed, first_cmvr_date_patient, first_cmvr_date_eye, exam_date, time_to_secondeyecmvr, time_since_firstcmvr)
# view_data_by_studyid




# RD INCIDENCE RATE - JK attempt
library(biostat3)
rdsurvivaldata <- rdsurvivaldata %>%
  mutate(startime=0,
         active=tx___1==1 | tx___2==1)
# Note the following is probably different from what you have; 
#if you don’t include start and stop-time like this then it will include people with zero follow-up. 
#But in reality we don’t want to include people with zero follow-up. This may make your p-values different, for example, it makes zone 3 p-value 0.3 I believe. 
#Also, I express time in terms of months here in order to make it easier to interpret the incidence rates.
rd_surv_object <- with(rdsurvivaldata, Surv(time=startime, time2 = as.numeric(time_to_rd/30.4), event = anyrd))
rd_surv_object
summary(rd_surv_object)
survRate(rd_surv_object ~ 1, data=rdsurvivaldata)

rd_surv_object <- with(rdsurvivaldata, Surv(time=startime, time2 = as.numeric(time_to_rd/365), event = anyrd))
rd_surv_object
summary(rd_surv_object)
survRate(rd_surv_object ~ 1, data=rdsurvivaldata)



# Note can also do the Survival object in the code; here if didn't want to make a variable for the start time:
survRate(Surv(as.numeric(time_to_rd/30.4),anyrd)~1, data=filter(rdsurvivaldata, time_to_rd>0))

# Confirming manually; 13 cases and 695 person-months at risk:
xtabs(data=filter(rdsurvivaldata, time_to_rd>0), ~anyrd)
rdsurvivaldata %>%
  filter(time_to_rd>0) %>%
  group_by(startime) %>%
  summarize(meantimetord=sum(time_to_rd/30.4))

#JK cox examples
cox_z3 <- coxph(rd_surv_object ~ cmv_zones___3, rdsurvivaldata)
summary(cox_z3)
survRate(rd_surv_object ~ cmv_zones___3, data=rdsurvivaldata)

cox_active <- coxph(rd_surv_object ~ active, rdsurvivaldata)
summary(cox_active)
survRate(rd_surv_object ~ active, data=rdsurvivaldata)


#################### RD SURVIVAL ANALYSES ###################################

# https://www.datacamp.com/community/tutorials/survival-analysis-R

#rd_surv_object: Fit survival data using the Kaplan-Meier method
rd_surv_object <- with(rdsurvivaldata, Surv(time = time_to_rd, event = anyrd))
rd_surv_object                   


#RD factor: age (≥50, <50)
#p = 0.13
fit_rd_age_50 <- survfit(rd_surv_object ~ factor(age < 50), data = rdsurvivaldata)
summary(fit_rd_age_50)
ggsurvplot(fit_rd_age_50, data = rdsurvivaldata, pval = TRUE, conf.int = TRUE, risk.table = TRUE)

#RD factor: age (median age = 37; >37, ≤37)
#p = 0.049
summary(rdsurvivaldata$age)
fit_rd_age_median <- survfit(rd_surv_object ~ factor(age > 37), data = rdsurvivaldata)
summary(fit_rd_age_median)
ggsurvplot(fit_rd_age_median, data = rdsurvivaldata, pval = TRUE, conf.int = TRUE, risk.table = TRUE)

#Cox model: age 
#HR 0.99 per year of age, 95% CI 0.95-1.05, P = 0.90
cox_age <- coxph(rd_surv_object ~ age + new_cd4_count + haart, rdsurvivaldata)
summary(cox_age)
# JK: POTENTIAL ERROR: the above is multivariable. Should it be?


#RD factor: gender (1 = female)
#p = 0.51
fit_rd_gender <- survfit(rd_surv_object ~ factor(gender), data = rdsurvivaldata)
summary(fit_rd_gender)
ggsurvplot(fit_rd_gender, data = rdsurvivaldata, pval = TRUE, conf.int = TRUE, risk.table = TRUE)

#Cox model: gender (female sex)
#HR 1.35, 95% CI 0.53-3.44, P = 0.53
cox_gender <- coxph(rd_surv_object ~ gender + new_cd4_count + haart, rdsurvivaldata)
summary(cox_gender)


#RD factor: baseline CD4 count (≥50, <50)
#p = 0.38
fit_rd_cd4_50 <- survfit(rd_surv_object ~ factor(new_cd4_count < 50), data = rdsurvivaldata)
summary(fit_rd_cd4_50)
ggsurvplot(fit_rd_cd4_50, data = rdsurvivaldata, pval = TRUE, conf.int = TRUE, risk.table = TRUE)

#RD factor: baseline CD4 count (≥100, <100)
#p = 0.027
fit_rd_cd4_100 <- survfit(rd_surv_object ~ factor(new_cd4_count < 100), data = rdsurvivaldata)
summary(fit_rd_cd4_100)
ggsurvplot(fit_rd_cd4_100, data = rdsurvivaldata, pval = TRUE, conf.int = TRUE, risk.table = TRUE)

#Cox model: baseline CD4 count < 50
#HR 0.54, 95% CI 0.15-2.02, P = 0.36
cox_cd4_50 <- coxph(rd_surv_object ~ (new_cd4_count < 50) + new_cd4_count + haart, rdsurvivaldata)
summary(cox_cd4_50)

#Cox model: baseline CD4 count < 100
#HR 0.13, 95% CI 0.03-0.60, P = 0.009
cox_cd4_100 <- coxph(rd_surv_object ~ (new_cd4_count < 100) + new_cd4_count + haart, rdsurvivaldata)
summary(cox_cd4_100)


#RD factor: CMV zone 1 involvement
#p = 0.56
fit_rd_z1 <- survfit(rd_surv_object ~ cmv_zones___1, data = rdsurvivaldata)
summary(fit_rd_z1)
ggsurvplot(fit_rd_z1, data = rdsurvivaldata, pval = TRUE, conf.int = TRUE, risk.table = TRUE)

#Cox model: CMV zone 1 involvement
#HR 0.99, 95% CI 0.39-2.51, P = 0.98
cox_z1 <- coxph(rd_surv_object ~ cmv_zones___1 + new_cd4_count + haart, rdsurvivaldata)
summary(cox_z1)


#RD factor: CMV zone 2 involvement
#p = 0.25
fit_rd_z2 <- survfit(rd_surv_object ~ cmv_zones___2, data = rdsurvivaldata)
summary(fit_rd_z2)
ggsurvplot(fit_rd_z2, data = rdsurvivaldata, pval = TRUE, conf.int = TRUE, risk.table = TRUE)

#Cox model: CMV zone 2 involvement
#HR 2.6E7, 95% CI 0-Inf, P = 0.99 (???)
cox_z2 <- coxph(rd_surv_object ~ cmv_zones___2 + new_cd4_count + haart, rdsurvivaldata)
summary(cox_z2)



#RD factor: CMV zone 3 involvement
#p = 0.31
fit_rd_z3 <- survfit(rd_surv_object ~ cmv_zones___3, data = rdsurvivaldata)
summary(fit_rd_z3)
ggsurvplot(fit_rd_z3, data = rdsurvivaldata, pval = TRUE, conf.int = TRUE, risk.table = TRUE)

#Cox model: CMV zone 3 involvement
#HR 1.72, 95% CI 0.66-4.46, P = 0.27
cox_z3 <- coxph(rd_surv_object ~ cmv_zones___3 + new_cd4_count + haart, rdsurvivaldata)
summary(cox_z3)

survRate(rd_surv_object ~ cmv_zones___3, data=rdsurvivaldata)


#RD factor: haart (1: yes, 2: no)
#p = 0.74
fit_rd_haart <- survfit(rd_surv_object ~ haart, data = rdsurvivaldata)
summary(fit_rd_haart)
ggsurvplot(fit_rd_haart, data = rdsurvivaldata, pval = TRUE, conf.int = TRUE, risk.table = TRUE)

#Cox model: haart
#HR 0.72, 95% CI 0.20-2.66, P = 0.62
cox_haart <- coxph(rd_surv_object ~ haart + new_cd4_count + haart, rdsurvivaldata)
summary(cox_haart)


#RD factor: induction treatment 
#p = 0.17
fit_rd_induction_tx <- survfit(rd_surv_object ~ new_induction_tx, data = rdsurvivaldata)
summary(fit_rd_induction_tx)
ggsurvplot(fit_rd_induction_tx, data = rdsurvivaldata, pval = TRUE, conf.int = TRUE, risk.table = TRUE)

#Cox model: induction treatment 
cox_induction_tx <- coxph(rd_surv_object ~ new_induction_tx + new_cd4_count + haart, rdsurvivaldata)
summary(cox_induction_tx)


#RD factor: maintenance treatment 
#p = 0.54
fit_rd_maint_tx <- survfit(rd_surv_object ~ new_maint_tx, data = rdsurvivaldata)
summary(fit_rd_maint_tx)
ggsurvplot(fit_rd_maint_tx, data = rdsurvivaldata, pval = TRUE, conf.int = TRUE, risk.table = TRUE)

#Cox model: maintenance treatment 
cox_maint_tx <- coxph(rd_surv_object ~ new_maint_tx + new_cd4_count + haart, rdsurvivaldata)
summary(cox_maint_tx)


#RD factor: either induction OR maintenance treatment 
#p = 0.072
rdsurvivaldata <- rdsurvivaldata %>%
  mutate(active_tx = ifelse(new_induction_tx == 1 | new_maint_tx == 1, 1, 0))

fit_rd_active_tx <- survfit(rd_surv_object ~ active_tx, data = rdsurvivaldata)
summary(fit_rd_active_tx)
ggsurvplot(fit_rd_active_tx, data = rdsurvivaldata, pval = TRUE, conf.int = TRUE, risk.table = TRUE)

#Cox model: either induction OR maintenance treatment → proxy for ACTIVE retinitis
#HR 0.29, 95% CI 0.07-1.22, P = 0.09
cox_active_ret <- coxph(rd_surv_object ~ active_tx + new_cd4_count + haart, rdsurvivaldata)
summary(cox_active_ret)

survRate(rd_surv_object ~ active_tx, data=rdsurvivaldata)


#RD factor: observation treatment → proxy for INACTIVE retinitis
#p = 0.044
fit_rd_obs_tx <- survfit(rd_surv_object ~ new_obs_tx, data = rdsurvivaldata)
summary(fit_rd_obs_tx)
ggsurvplot(fit_rd_obs_tx, data = rdsurvivaldata, pval = TRUE, conf.int = TRUE, risk.table = TRUE)

#Cox model: observation treatment 
#HR 3.79, 95% CI 0.94-15.24, P = 0.06
cox_obs_tx <- coxph(rd_surv_object ~ new_obs_tx + new_cd4_count + haart, rdsurvivaldata)
summary(cox_obs_tx)




#second eye incidence, based on JK rd incidence code above
library(biostat3)
dataforsecondeye <- dataforsecondeye %>%
  mutate(startime=0,
         active=tx___1==1 | tx___2==1)
# Note the following is probably different from what you have; 
#if you don’t include start and stop-time like this then it will include people with zero follow-up. 
#But in reality we don’t want to include people with zero follow-up. This may make your p-values different, for example, it makes zone 3 p-value 0.3 I believe. 
#Also, I express time in terms of months here in order to make it easier to interpret the incidence rates.
secondeye_surv_object <- with(dataforsecondeye, Surv(time=startime, time2 = as.numeric(time_to_secondeyecmvr/30.4), event = secondeyecmvr_developed))
secondeye_surv_object
summary(secondeye_surv_object)
survRate(secondeye_surv_object ~ 1, data=dataforsecondeye)

secondeye_surv_object <- with(dataforsecondeye, Surv(time=startime, time2 = as.numeric(time_to_secondeyecmvr/365), event = secondeyecmvr_developed))
secondeye_surv_object
summary(secondeye_surv_object)
survRate(secondeye_surv_object ~ 1, data=dataforsecondeye)


#################### 2ND EYE SURVIVAL ANALYSES ###################################

#secondeye_surv_object
secondeye_surv_object <- with(dataforsecondeye, Surv(time = time_to_secondeyecmvr, event = secondeyecmvr_developed))
secondeye_surv_object

#second eye factor: age (≥50, <50)
#p = 0.32
fit_secondeye_age_50 <- survfit(secondeye_surv_object ~ factor(age < 50), data = dataforsecondeye)
summary(fit_secondeye_age_50)
ggsurvplot(fit_secondeye_age_50, data = dataforsecondeye, pval = TRUE, conf.int = TRUE, risk.table = TRUE)

#second eye factor: median age = 37; >37, ≤37
#p = 0.13
fit_secondeye_age_median <- survfit(secondeye_surv_object ~ factor(age > 37), data = dataforsecondeye)
summary(fit_secondeye_age_median)
ggsurvplot(fit_secondeye_age_median, data = dataforsecondeye, pval = TRUE, conf.int = TRUE, risk.table = TRUE)

#Cox model: age 
#HR 0.96, 95% CI 0.88-1.05, P = 0.36
cox_secondeye_age <- coxph(secondeye_surv_object ~ age + new_cd4_count + haart, dataforsecondeye)
summary(cox_secondeye_age)



#second eye factor: gender (1 = female)
#p = 0.28
fit_secondeye_gender <- survfit(secondeye_surv_object ~ gender, data = dataforsecondeye)
summary(fit_secondeye_gender)
ggsurvplot(fit_secondeye_gender, data = dataforsecondeye, pval = TRUE, conf.int = TRUE, risk.table = TRUE)

#Cox model: gender
#HR 0.34, 95% CI 0.07-1.57, P = 0.17
cox_secondeye_gender <- coxph(secondeye_surv_object ~ gender + new_cd4_count + haart, dataforsecondeye)
summary(cox_secondeye_gender)


#second eye factor: haart (1: yes, 2: no)
#p = 0.04
fit_secondeye_haart <- survfit(secondeye_surv_object ~ haart, data = dataforsecondeye)
summary(fit_secondeye_haart)
ggsurvplot(fit_secondeye_haart, data = dataforsecondeye, pval = TRUE, conf.int = TRUE, risk.table = TRUE)

#Cox model: haart
#HR 0.40, 95% CI 0.09-1.79, P = 0.23
cox_secondeye_haart <- coxph(secondeye_surv_object ~ haart + new_cd4_count + haart, dataforsecondeye)
summary(cox_secondeye_haart)


#second eye factor: baseline CD4 count (≥50, <50)
#p = 0.1
fit_secondeye_cd4_50 <- survfit(secondeye_surv_object ~ factor(new_cd4_count < 50), data = dataforsecondeye)
summary(fit_secondeye_cd4_50)
ggsurvplot(fit_secondeye_cd4_50, data = dataforsecondeye, pval = TRUE, conf.int = TRUE, risk.table = TRUE)

#Cox model: baseline CD4 count (≥50, <50)
#HR 0.82, 95% CI 0.06-10.53, P = 0.88
cox_secondeye_cd4_50 <- coxph(secondeye_surv_object ~ (new_cd4_count < 50) + new_cd4_count + haart, dataforsecondeye)
summary(cox_secondeye_cd4_50)


#second eye factor: baseline CD4 count (≥100, <100)
#p = 0.12
fit_secondeye_cd4_100 <- survfit(secondeye_surv_object ~ factor(new_cd4_count < 100), data = dataforsecondeye)
summary(fit_secondeye_cd4_100)
ggsurvplot(fit_secondeye_cd4_100, data = dataforsecondeye, pval = TRUE, conf.int = TRUE, risk.table = TRUE)

#Cox model: baseline CD4 count (≥100, <100)
#HR 0.47, 95% CI 0.01-20.09, P = 0.69
cox_secondeye_cd4_100 <- coxph(secondeye_surv_object ~ (new_cd4_count < 100) + new_cd4_count + haart, dataforsecondeye)
summary(cox_secondeye_cd4_100)


#second eye factor: CMV zone 1 involvement
#p = 0.35
fit_secondeye_z1 <- survfit(secondeye_surv_object ~ cmv_zones___1, data = dataforsecondeye)
summary(fit_secondeye_z1)
ggsurvplot(fit_secondeye_z1, data = dataforsecondeye, pval = TRUE, conf.int = TRUE, risk.table = TRUE)

#Cox model: CMV zone 1 involvement
#HR 2.9, 95% CI 0.55-15.26, P = 0.21
cox_secondeye_z1 <- coxph(secondeye_surv_object ~ cmv_zones___1 + new_cd4_count + haart, dataforsecondeye)
summary(cox_secondeye_z1)


#second eye factor: CMV zone 2 involvement
#p = 0.68
fit_secondeye_z2 <- survfit(secondeye_surv_object ~ cmv_zones___2, data = dataforsecondeye)
summary(fit_secondeye_z2)
ggsurvplot(fit_secondeye_z2, data = dataforsecondeye, pval = TRUE, conf.int = TRUE, risk.table = TRUE)

#Cox model: CMV zone 2 involvement
#HR 0.23, 95% CI 0.03-2.14, P = 0.20
cox_secondeye_z2 <- coxph(secondeye_surv_object ~ cmv_zones___2 + new_cd4_count + haart, dataforsecondeye)
summary(cox_secondeye_z2)


#second eye factor: CMV zone 3 involvement
#p = 0.13
fit_secondeye_z3 <- survfit(secondeye_surv_object ~ cmv_zones___3, data = dataforsecondeye)
summary(fit_secondeye_z3)
ggsurvplot(fit_secondeye_z3, data = dataforsecondeye, pval = TRUE, conf.int = TRUE, risk.table = TRUE)

#Cox model: CMV zone 3 involvement
#HR 0.22, 95% CI 0.03-1.81, P = 0.16
cox_secondeye_z3 <- coxph(secondeye_surv_object ~ cmv_zones___3 + new_cd4_count + haart, dataforsecondeye)
summary(cox_secondeye_z3)


dataforsecondeye <- dataforsecondeye %>%
  mutate(active_tx = ifelse(new_induction_tx == 1 | new_maint_tx == 1, 1, 0))

#second eye factor: either induction OR maintenance treatment → proxy for ACTIVE retinitis
#p = 0.37
fit_secondeye_active_tx <- survfit(secondeye_surv_object ~ active_tx, data = dataforsecondeye)
summary(fit_secondeye_active_tx)
ggsurvplot(fit_secondeye_active_tx, data = dataforsecondeye, pval = TRUE, conf.int = TRUE, risk.table = TRUE)

#Cox model: either induction OR maintenance treatment → proxy for ACTIVE retinitis
#HR 2.02E7, 95% CI 0-Inf, P = 0.99
cox_secondeye_active_tx <- coxph(secondeye_surv_object ~ active_tx + new_cd4_count + haart, dataforsecondeye)
summary(cox_secondeye_active_tx)







# template!
# #second eye factor: ***
# #p = 0.
# fit_secondeye_*** <- survfit(secondeye_surv_object ~ factor(***), data = dataforsecondeye)
# summary(fit_secondeye_***)
# ggsurvplot(fit_secondeye_***, data = dataforsecondeye, pval = TRUE, conf.int = TRUE, risk.table = TRUE)
# 
# #Cox model: *** 
# #HR , 95% CI , P = 0.
# cox_secondeye_*** <- coxph(secondeye_surv_object ~ *** + new_cd4_count + haart, dataforsecondeye)
# summary(cox_secondeye_***)