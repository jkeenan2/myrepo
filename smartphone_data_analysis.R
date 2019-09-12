#Clear existing redcapexport and graphics
rm(list=ls())
graphi=s.off()

library(readr)
#Load exploratory redcapexport analysis=20packages
library(tidyverse)
library(scales)   # date/time s=ales for plots
library(lubridate)
library(epiR)
library(dplyr)
=ibrary(irr)

##Read Digicards Data
redcapexport <- read_csv("=martphoneRetinalScr_DATA_2019-04-06_0348.csv")

hospitalandphotoid <-=20read_csv("Hospital ID - Photo ID.csv")

#re-add leading 0=s to studyid's
#wide -> long data; each photoid in sepa=ate row
hospitalandphotoid_long <- hospitalandphotoid %>%
  =utate(study_id=formatC(as.integer(study_id), width = 7, format =3D "d", flag = "0")) %>%
  gather(key = camera, v=lue = photoid, peek:pictor)


#long data for RE/LE di=gnoses
redcapexport_long <- redcapexport %>%
  gather(key ==20dr_eye, value = dr, dr_right:dr_left)


redcapexport_long=20<- redcapexport_long %>%
  mutate(eye = if_else(grepl("ri=ht", redcapexport_long$dr_eye), "RE", "LE")) %>%
  select(-(d=_eye)) %>%
  mutate(dme = if_else(eye=="RE", dme_right,=20dme_left)) %>%
  mutate(amd = if_else(eye=="RE", amd_=ight, amd_left)) %>%
  mutate(glaucoma = if_else(eye=="=E", glaucoma_right, glaucoma_left)) %>%  
  mutate(otherdx =3D if_else(eye=="RE", otherdx_right, otherdx_left)) %>%  
=20 mutate(other_retinal_path_new = if_else(eye=="RE", other_r=tinal_path, other_retinal_path_left)) %>%    
  mutate(di=gnosis_dr_new = if_else(eye=="RE", diagnosis_dr, diagnosis_dr=left)) %>%    
  mutate(diagnosis_amd_new = if_else(e=e=="RE", diagnosis_amd, diagnosis_amd_left)) %>%     
=20 mutate(diagnosis_glaucoma_new = if_else(eye=="RE", diagnos=s_glaucoma, diagnosis_glaucoma_left)) %>%
  select(-(dme_right:di=gnosis_glaucoma_left)) %>%
  select(study_id, eye, everything()=
  

#long-er data for cameras
redcapexport_longer <- =edcapexport_long %>%
  gather(key = camera_time, value ==20seconds, peek_time:pictor_time)

redcapexport_longer <- redcape=port_longer %>%
  mutate(camera = if_else(grepl("peek", red=apexport_longer$camera_time), "peek",
           =20      if_else(grepl("inview", redcapexport_longer$camera_ti=e), "inview",    
               =20  if_else(grepl("pictor", redcapexport_longer$camera_time), "pict=r", "NA")))) %>%
  select(-(camera_time)) %>%
  mutate(di=comfort = if_else(camera=="peek", peek_discomfort,
    =20                 if_else(camera=="i=view", inview_discomfort,        
       =20              if_else(camera=="pictor", =ictor_discomfort, 999))))

redcapexport_to_merge <- redcapexport_=onger %>%
  select(study_id, eye, camera, photo_date, age, =ender, cataract, cataract_deg, dr:seconds, discomfort)
  
 =20
digicards_data <- full_join(redcapexport_to_merge, hospitalandph=toid_long, by=c("study_id", "eye", "camera")) %>%
  select(=tudy_id:photo_date, photoid, age:discomfort) %>%
  rename(other=retinal_path = other_retinal_path_new) %>%
  rename(diagnosis=dr = diagnosis_dr_new) %>%  
  rename(diagnosis_amd ==20diagnosis_amd_new) %>%  
  rename(diagnosis_glaucoma = =iagnosis_glaucoma_new)




##################

# import o=iginal teams grading data
team1_import <- read_csv("Team1_DATA_=019-05-15_0712.csv") %>%  mutate(team=1)
team2_import <- read=csv("Team2_DATA_2019-05-13_0820.csv") %>%  mutate(team=2)
team3_i=port <- read_csv("Team3_DATA_2019-06-23_1055.csv") %>%  mutate(te=m=3)
team4_import <- read_csv("Team4_DATA_2019-06-23_1056.csv") %=%  mutate(team=4)

teams_import <- bind_rows(team1_import, =eam2_import, team3_import, team4_import)

# import adjudication=20grading data
adj_team1 <- read_csv("Team1_Adjudic_2019-07-12_1539=csv") %>%  mutate(team=1)
adj_team2 <- read_csv("Team2_Adjudi=_2019-07-15_1702.csv") %>%  mutate(team=2)
adj_team3 <- read_=sv("Team3_Adjudic_2019-07-17_1624.csv") %>%  mutate(team=3)
adj_t=am4 <- read_csv("Team4_Adjudic_2019-08-08_0143.csv") %>%  mutate(=eam=4)

adj_teams <- bind_rows(adj_team1, adj_team2, adj_team=, adj_team4) %>%
  mutate(study_id=paste0(record_id, "--3")) =>%
  select(-record_id)


grading_data <- bind_rows(teams_i=port, adj_teams) %>%
  select(-new_photo_grading_form_complete) =>%
  mutate(dr_yesno = ifelse((dr == 0 | dr ===201), 0, 
                    =felse((dr == 2 | dr == 3), 1, NA)),
     =20   amd_yesno = ifelse((amd == 0 | amd == =), 0, 
                     =felse((amd == 2 | amd == 3), 1, NA))) %>% # x=abs(data=teams, ~dr + dr_yesno, addNA=TRUE)
  separate(st=dy_id, c("photoid", "grader"), sep = "--", remove = TRU=, convert = FALSE) %>%
  gather(field, value, image_cla=ity:notes, dr_yesno:amd_yesno) %>%
  mutate(field.grader=paste(=ield, grader, sep=".")) %>%
  select(-field, -grader) %>%=0A  spread(field.grader, value, convert=TRUE) %>%
  mutat=(dr_final = if_else((dr_yesno.1 != dr_yesno.2 & !is.na(dr_y=sno.3)), dr_yesno.3, dr_yesno.1),
            =20       if_else(!is.na(dr_yesno.1) & is.na(dr_yesno.2) = is.na(dr_yesno.3), dr_yesno.1,
             =20      if_else(!is.na(dr_yesno.2) & is.na(dr_yesno.1) & =s.na(dr_yesno.3), dr_yesno.2, dr_yesno.3))) %>%
       =20                         =20                         =20               # addmargins(xtabs(data==ilter(grading_data, dr_yesno.1!=dr_yesno.2), ~dr_yesno.3+dr_adj, ad=NA=TRUE))
                     =20                         =20                         =20 # addmargins(xtabs(data=filter(grading_data, dr_yesno.1==dr_=esno.2), ~dr_yesno.3+dr_adj, addNA=TRUE))
         =20                         =20                         =20             # addmargins(xtabs(data=gradin=_data, ~dr_yesno.3+dr_adj, addNA=TRUE))
  mutate(amd_final ==20if_else((amd_yesno.1 != amd_yesno.2 & !is.na(amd_yesno.3)), a=d_yesno.3, amd_yesno.1),
                =20    if_else(!is.na(amd_yesno.1) & is.na(amd_yesno.2) & is=na(amd_yesno.3), amd_yesno.1,
              =20      if_else(!is.na(amd_yesno.2) & is.na(amd_yesno.1) = is.na(amd_yesno.3), amd_yesno.2, amd_yesno.3))) %>%
  mutate=cdr_final = if_else(!is.na(cdr.3), cdr.3,
        =20          if_else((is.na(cdr.1) & is.na(cdr.3)), =dr.2,
                   if_else((i=.na(cdr.2) & is.na(cdr.3)), cdr.1,
           =20       if_else((is.na(cdr.3) & !is.na(cdr.1) & !is.=a(cdr.2)) & (cdr.1>cdr.2), cdr.1, cdr.2))))) %>%
  select(-=eam, -(amd_yesno.1:cdr.3), -(dr_yesno.1:dr.3)) %>%
  select(pho=oid, amd_final, amd_severity.1:amd_severity.3, dme.1:dme.3, dr_fina=, dr_severity.1:dr_severity.3, image_clarity.1:other_dx.3, cdr_final,=20vcdr_confidence.1:zone2.3)


 # JK: note there are 2 =ypes of records with missing dr_adj data: first, those wi=h nonmissing grader 1 and 2 but missing grader3, and al=o those with missing grader 1 and 2 and 3
  # Pro=ably worth a double-check of the data to make sure this=20data is actually missing? Then we can drop the rows w=th completely missing data
# LL: ^NA's for all 3 grader= are for photoid's for photos that weren't taken (actuall= missing) or mistaken entries (i.e. 1153a, b, c, d)

=0A#adj_discrep <- grading_data %>% filter(dr_yesno.3!=dr_adj)
#=20JK: I am not sure how these people got an adjudicated=20grade since the grader 1 and 2 grades matched. 
# B=t I guess we take the consensus grade, which would be =he grader 1 and 2 grade.
# But worth looking into wha= happened here. (Look at a few of the photos...)
# ^ =L: grader 1 & grader 2 disagreed on either AMD or CDR=20for these cases


# JK: for severity and other vari=bles that are not dichotomous (eg image_clarity): we need =o be careful. 
# For severity, I think being conservati=e here means that it's harder to pick up milder disease=0A# So we would make severe disease trump mild disease
= So then need to take the max of the severity grades.=20Best would be to take the consensus but not sure that=20is possible.
# Maybe you can take a stab at making =onsensus grades for the categorical variables


#make amd=severity == 4 → 0.4
grading_data$amd_severity.1[gradi=g_data$amd_severity.1 == 4] <- 0.4
grading_data$amd_severity.=[grading_data$amd_severity.2 == 4] <- 0.4
grading_data$amd_se=erity.3[grading_data$amd_severity.3 == 4] <- 0.4

#make d=_severity == 5 → 0.5
grading_data$dr_severity.1[gradi=g_data$dr_severity.1 == 5] <- 0.5
grading_data$dr_severity.2[=rading_data$dr_severity.2 == 5] <- 0.5
grading_data$dr_severi=y.3[grading_data$dr_severity.3 == 5] <- 0.5

#final value= for categorical/other variables
grading_data <- grading_data =>%
  mutate(amd_severity_final = if_else(amd_final == 0= 0,
                       =20      if_else(amd_final == 1 & (amd_severity.1>am=_severity.2), amd_severity.1, amd_severity.2))) %>%
  mutate(dm=_final = if_else(dme.1>dme.2, dme.1, dme.2)) %>%
  mutate=dr_severity_final = if_else(dr_final == 0, 0,
    =20                        i=_else(dr_final == 1 & (dr_severity.1>dr_severity.2), dr_sever=ty.1, dr_severity.2))) %>%
  mutate(image_clarity_final = i=_else(image_clarity.1>image_clarity.2, image_clarity.1, image_clarity.2=) %>%
  mutate(macula_final = if_else(macula.1>macula.2, ma=ula.1, macula.2)) %>%
  mutate(nerve_final = if_else(nerve.=>nerve.2, nerve.1, nerve.2)) %>%
  mutate(notes_final = p=ste(notes.1, notes.2, notes.3, sep="/")) %>%
  mutate(other=dx_detail_final = paste(other_dx_detail.1, other_dx_detail.2, oth=r_dx_detail.3, sep="/")) %>%
  mutate(other_dx_final = if=else(other_dx.1>other_dx.2, other_dx.1, other_dx.2)) %>%
  muta=e(vcdr_confidence_final = if_else(!is.na(vcdr_confidence.3), vcdr_c=nfidence.3,
                     =20           if_else(vcdr_confidence.1>vcdr_confidenc=.2, vcdr_confidence.1, vcdr_confidence.2))) %>%
  mutate(zone2_=inal = if_else(zone2.1>zone2.2, zone2.1, zone2.2)) %>%
  =utate(glaucoma_final = if_else(!is.na(cdr_final) & cdr_final >==.7, 1, 0)) %>%
  mutate(other_dx_yesno = if_else((other_d=_final == 0 | other_dx_final == 1), 0,
    =20                     if_else((o=her_dx_final == 2 | other_dx_final == 3), 1, 999)))=0A


clean_grading_data <- grading_data %>%
  select(phot=id, image_clarity_final, nerve_final, macula_final, zone2_final, =r_final, dr_severity_final, dme_final, amd_final, amd_severity_fina=, vcdr_confidence_final, cdr_final, glaucoma_final, other_dx_yesno,=20other_dx_final, other_dx_detail_final, notes_final)


#xtabs(da=a=grading_data, ~dr_yesno.1+dr_yesno.2, addNA=TRUE)
#xtabs(data==rading_data, ~amd_yesno.1+amd_yesno.2, addNA=TRUE)

class(digicar=s_data$photoid)
class(clean_grading_data$photoid)
digicards_data$photoi= <- as.character(digicards_data$photoid)

alldata <- full_join(=igicards_data, clean_grading_data, by="photoid") %>%
  arrang=(study_id, eye, camera) %>%
  mutate(dr_final=as.factor(dr_fi=al),
         dr=as.factor(dr))


# INTERRATE= RELIABILITY
#merge non-clean grading data (with individual =rader grades) for interrater agreement 
# ****** take out=20line 118-9 that un-selects columns with individual grader =rades for dr/amd/etc...
for_irr <- full_join(digicards_data, gr=ding_data, by="photoid") 

peek_irr <- for_irr %>%
  =ilter(camera == "peek")
inview_irr <- for_irr %>%
  f=lter(camera == "inview")
pictor_irr <- for_irr %>%
  =ilter(camera == "pictor")

peek_dr_irr <- peek_irr[,40:41]
=appa2(peek_dr_irr, "squared")

inview_dr_irr <- inview_irr[,40:41=
kappa2(inview_dr_irr, "squared")

pictor_dr_irr <- pictor_irr[=40:41]
kappa2(pictor_dr_irr, "squared")
#
peek_amd_irr <- peek_=rr[,25:26]
kappa2(peek_amd_irr, "squared")

inview_amd_irr <- i=view_irr[,25:26]
kappa2(inview_amd_irr, "squared")

pictor_amd_irr =- pictor_irr[,25:26]
kappa2(pictor_amd_irr, "squared")
#
peek_cdr=icc <- peek_irr[,31:32]
icc(peek_cdr_icc, model="twoway", type==agreement")

inview_cdr_icc <- inview_irr[,31:32]
icc(inview_cdr_=cc, model="twoway", type="agreement")

pictor_cdr_icc <- pi=tor_irr[,31:32]
icc(pictor_cdr_icc, model="twoway", type="agreeme=t")
#
peek_clarity_irr <- peek_irr[,46:47]
kappa2(peek_clarity_ir=, "squared")

inview_clarity_irr <- inview_irr[,46:47]
kappa2(i=view_clarity_irr, "squared")

pictor_clarity_irr <- pictor_irr[,4=:47]
kappa2(pictor_clarity_irr, "squared")



# JK For fi=al analysis we need to make sure that we have nonmissin= data for gold standard and adjudicated grade for all 3=20cameras. 
# Only include those observations that meet t=is criteria.
missingdata <- as.data.frame(xtabs(data=filter(allda=a, !is.na(dr_final) & !is.na(dr)), ~study_id+camera+eye)) %>%
 =20mutate(studyideye=paste(study_id, eye, sep="_")) %>%
  se=ect(-study_id, -eye) %>%
  spread(camera, Freq) %>%
  m=tate(sum=inview+pictor+peek,
         product=invie=*pictor*peek) %>%
  filter(!(sum==3 & product==1)) %>=
  filter(!(sum==6 & product==8)) %>%
  write.csv="missingdata.csv")

#take out rows with incomplete data for=20digicards/gold standard data & graded/adjudicated data for =ll 3 cameras
alldata_final <- alldata %>%
  mutate(studyi=_eye=paste(study_id, eye, sep="_")) %>%
  select(study_id, =tudyid_eye, camera:notes_final) %>%
  filter(!(grepl("2376229_RE"= studyid_eye)), !(grepl("2694303_RE", studyid_eye)), !(grepl("28324=4_LE", studyid_eye)),
         !(grepl("3083909_LE", =tudyid_eye)), !(grepl("3092218_RE", studyid_eye)), !(grepl("3524639_L=", studyid_eye)), 
         !(grepl("3568975_RE", =tudyid_eye)), !(grepl("3690462_LE", studyid_eye)), !(grepl("3738618_L=", studyid_eye)), 
         !(grepl("3746321_LE", =tudyid_eye)), !(grepl("3746321_RE", studyid_eye)), !(grepl("3798184_L=", studyid_eye)), 
         !(grepl("3807366_LE", =tudyid_eye)), !(grepl("3807366_RE", studyid_eye)), !(grepl("3807979_L=", studyid_eye)), 
         !(grepl("3819672_LE", =tudyid_eye)), !(grepl("3819672_RE", studyid_eye)), !(grepl("3839955_L=", studyid_eye)), 
         !(grepl("3845086_RE", =tudyid_eye)), !(grepl("3845515_RE", studyid_eye)), !(grepl("3891932_L=", studyid_eye)), 
         !(grepl("3891932_RE", =tudyid_eye)), !(grepl("3922325_LE", studyid_eye)), !(grepl("1838", =hotoid)), 
         !(grepl("2909", photoid)), !(=repl("3266", photoid)), !(grepl("1153d", photoid)))
     =20     

# JK: Trying out the yardstick package =rom Louisa's email
#https://stackoverflow.com/questions/57146785/how-=o-perform-bootstrapping-to-a-diagnostic-test-in-r
library(yardstick)
op=ions(yardstick.event_first = FALSE)

addmargins(xtabs(data=alld=ta_final, ~dr+dr_final))
alldata_final %>% filter(!is.na(camera)) =>% group_by(camera) %>% sens(., truth = as.factor(dr), esti=ate = as.factor(dr_final))
alldata_final %>% filter(!is.na(came=a)) %>% group_by(camera) %>% spec(., truth = as.factor(dr),=20estimate = as.factor(dr_final))

# I guess you can ge= multiple metrics at once with this "metric_set" command
=lass_metrics <- metric_set(sens, spec, ppv, npv)

# For t=e actual numbers...
confmatrixtable_dr <- alldata_final %>%
 =20filter(!is.na(camera)) %>%
  group_by(camera) %>%
  conf_=at(dr, dr_final) %>%
  mutate(tidied = map(conf_mat, tidy=) %>%
  unnest(tidied) %>%
  spread(name, value) %>%
=20 rename(bothpos=cell_1_1,
         bothneg=cell=2_2,
         testpos_gsneg=cell_1_2,
     =20   testneg_gspos=cell_2_1)

confmat_peek_dr <- alldata_fi=al %>%
  filter(camera=="peek") %>%
  conf_mat(dr, dr=final)
summary(confmat_peek_dr)

confmat_inview_dr <- alldata_fin=l %>%
  filter(camera=="inview") %>%
  conf_mat(dr, d=_final)
summary(confmat_inview_dr)

confmat_pictor_dr <- alldata_=inal %>%
  filter(camera=="pictor") %>%
  conf_mat(dr, =r_final)
summary(confmat_pictor_dr)



# Now to get estim=tes and CIs, need to do separately. First an object wit= the estimates
sensspec_estimates_dr <- alldata_final %>%
 =20filter(!is.na(camera)) %>%
  group_by(camera) %>%
  class=metrics(., truth = dr, estimate = dr_final) %>%
  s=lect(-.estimator) %>%
  spread(.metric, .estimate, convert=TR=E) %>%
  rename(sens_est=sens,
         spec_=st=spec,
         npv_est=npv,
      =20  ppv_est=ppv)
# Check
xtabs(data=filter(alldata_final, c=mera=="peek"), ~dr+dr_final)

###################################=#######################
## BOOTSTRAPPED 95% CI ACCOUNTING FOR =LUSTERING OF EYES ##
##############################################=############

# This creates a nested data frame, where =ll data with same study id get put on the same line
= So if we resample, we will automatically resample all =ata from the same person
D <- alldata_final %>% filter(!i=.na(camera)) %>% nest(-studyid_eye)
head(D)
library(rsample)
set.=eed(154234)
# The bs object is the boostrap object; we =re creating separate populations with resampling
# You coul= alter the "times" option; usually use small number of =eplications as testing code because faster
# But then cha=ge to a larger number (9999?) for the final analysis
bs=20<- bootstraps(D, times = 9)
library(purrr)

# Need th= purrr package for the map function
# The map function =pplies a function iteratively to each element of a list =r vector
# For example, let's apply "p" to everything o= the list after the comma
p <- c(0.025, 0.975)
p
pmap= <- map(p, function(x) 2 + x )
# For a shortcut you=20can use the ~ to signify "function" and . to signify =here to apply the "p"
pmap2 <- map(p, ~ 2 + . )
p=ap1
pmap2
# (The pmap1 and pmap2 are just examples, not=20used in analysis below. But the "p" is used)
# So h=re, we can make a list of both the 2.5% and 97.5% b= aplying the p vector above to this partial function
# =ill use this below to apply to all the metrics to get=20the 2.5% and 97.5% of the confidence interval.
p_funs <= map(p, ~partial(quantile, probs = .x, na.rm = TRUE)) =>% 
  set_names(nm = c("lower95", "upper95"))

# So =inally here is the dataframe with the CIs
bs_sensspec_dr =- map(bs$splits, ~as_tibble(.) %>% unnest %>% 
     =20               filter(!is.na(camera)) %>%=0A                     group_by(c=mera) %>% 
                    =20class_metrics(., truth = dr, estimate = dr_final)) %>% =0A  bind_rows(.id = 'boots') %>%
  select(-.estimator) =>%
  spread(.metric, .estimate, convert=TRUE) %>%
  gro=p_by(camera) %>%
  summarize_at(vars(sens, spec, ppv, npv), =st(!!!p_funs))


# Now merge together the estimates and =Is and reshape into more useful format
sensspectable_dr <- =ull_join(sensspec_estimates_dr, bs_sensspec_dr, by="camera")
senssp=ctablelong_dr <- sensspectable_dr %>%
  gather(field, value, =pv_est:npv_upper95) %>%
  separate(field, into=c("metric", "s=at"), sep="_") %>%
  spread(stat, value, convert=TRUE) =>%
  write.csv("senspectablelong_dr.csv")

## END JK CODE =ERE


# numbers for AMD
addmargins(xtabs(data=alldata_final= ~amd+amd_final))
alldata_final %>% filter(!is.na(camera)) %>% =roup_by(camera) %>% sens(., truth = as.factor(amd), estimate =3D as.factor(amd_final))
alldata_final %>% filter(!is.na(camera)) =>% group_by(camera) %>% spec(., truth = as.factor(amd), est=mate = as.factor(amd_final))

alldata_final$amd <- as.factor(=lldata_final$amd)
alldata_final$amd_final <- as.factor(alldata_final$=md_final)
class(alldata_final$amd)
class(alldata_final$amd_final)

=onfmatrixtable_amd <- alldata_final %>%
  filter(!is.na(camera)= %>%
  group_by(camera) %>%
  conf_mat(amd, amd_final) =>%
  mutate(tidied = map(conf_mat, tidy)) %>%
  unnes=(tidied) %>%
  spread(name, value) %>%
  rename(bothpos==ell_1_1,
         bothneg=cell_2_2,
     =20   testpos_gsneg=cell_1_2,
         testneg_g=pos=cell_2_1)

confmat_peek_amd <- alldata_final %>%
  fi=ter(camera=="peek") %>%
  conf_mat(amd, amd_final)
summary(=onfmat_peek_amd)

confmat_inview_amd <- alldata_final %>%
  =ilter(camera=="inview") %>%
  conf_mat(amd, amd_final)
summ=ry(confmat_inview_amd)

confmat_pictor_amd <- alldata_final %>%
=20 filter(camera=="pictor") %>%
  conf_mat(amd, amd_final)
=ummary(confmat_pictor_amd)


sensspec_estimates_amd <- alldata_fi=al %>%
  filter(!is.na(camera)) %>%
  group_by(camera) %>=
  class_metrics(., truth = amd, estimate = amd_final= %>%
  select(-.estimator) %>%
  spread(.metric, .estimat=, convert=TRUE) %>%
  rename(sens_est=sens,
     =20   spec_est=spec,
         npv_est=npv,
 =20       ppv_est=ppv)
# Check
xtabs(data=filter(all=ata_final, camera=="peek"), ~amd+amd_final)


## bootstrapp=d 95% CI accounting for clustering of eyes 
bs_sensspec_a=d <- map(bs$splits, ~as_tibble(.) %>% unnest %>% 
   =20                    filter(!is.na=camera)) %>%
                    =20   group_by(camera) %>% 
            =20           class_metrics(., truth = as.fact=r(amd), estimate = as.factor(amd_final))) %>% 
  bind_row=(.id = 'boots') %>%
  select(-.estimator) %>%
  sprea=(.metric, .estimate, convert=TRUE) %>%
  group_by(camera) %=%
  summarize_at(vars(sens, spec, ppv, npv), lst(!!!p_funs))
=0A
# Now merge together the estimates and CIs and resha=e into more useful format
sensspectable_amd <- full_join(sens=pec_estimates_amd, bs_sensspec_amd, by="camera")
sensspectablelong_=md <- sensspectable_amd %>%
  gather(field, value, npv_est:=pv_upper95) %>%
  separate(field, into=c("metric", "stat"), =ep="_") %>%
  spread(stat, value, convert=TRUE) %>%
 =20write.csv("senspectablelong_amd.csv")




# numbers for g=aucoma

addmargins(xtabs(data=alldata_final, ~glaucoma+glaucoma_fin=l))
alldata_final %>% filter(!is.na(camera)) %>% group_by(camera)=20%>% sens(., truth = as.factor(glaucoma), estimate = as.=actor(glaucoma_final))
alldata_final %>% filter(!is.na(camera)) %>%=20group_by(camera) %>% spec(., truth = as.factor(glaucoma), e=timate = as.factor(glaucoma_final))

alldata_final$glaucoma <- =s.factor(alldata_final$glaucoma)
alldata_final$glaucoma_final <- as.f=ctor(alldata_final$glaucoma_final)
class(alldata_final$glaucoma)
class(=lldata_final$glaucoma_final)

confmatrixtable_glaucoma <- alldata_f=nal %>%
  filter(!is.na(camera)) %>%
  group_by(camera) %=%
  conf_mat(glaucoma, glaucoma_final) %>%
  mutate(tidied =3D map(conf_mat, tidy)) %>%
  unnest(tidied) %>%
  spre=d(name, value) %>%
  rename(bothpos=cell_1_1,
     =20   bothneg=cell_2_2,
         testpos_gsneg==ell_1_2,
         testneg_gspos=cell_2_1)

confma=_peek_glaucoma <- alldata_final %>%
  filter(camera=="peek"= %>%
  conf_mat(glaucoma, glaucoma_final)
summary(confmat_peek_=laucoma)

confmat_inview_glaucoma <- alldata_final %>%
  fi=ter(camera=="inview") %>%
  conf_mat(glaucoma, glaucoma_final=
summary(confmat_inview_glaucoma)

confmat_pictor_glaucoma <- all=ata_final %>%
  filter(camera=="pictor") %>%
  conf_mat=glaucoma, glaucoma_final)
summary(confmat_pictor_glaucoma)


sens=pec_estimates_glaucoma <- alldata_final %>%
  filter(!is.na(cam=ra)) %>%
  group_by(camera) %>%
  class_metrics(., truth =3D glaucoma, estimate = glaucoma_final) %>%
  select(-.es=imator) %>%
  spread(.metric, .estimate, convert=TRUE) %>%
=20 rename(sens_est=sens,
         spec_est=spec,
=20        npv_est=npv,
         ppv_e=t=ppv)
# Check
xtabs(data=filter(alldata_final, camera=="pe=k"), ~glaucoma+glaucoma_final)



## bootstrapped 95% CI =ccounting for clustering of eyes 
bs_sensspec_glaucoma <- m=p(bs$splits, ~as_tibble(.) %>% unnest %>% 
       =20                 filter(!is.na(camera))=20%>%
                       =20 group_by(camera) %>% 
              =20          class_metrics(., truth = as.factor(=laucoma), estimate = as.factor(glaucoma_final))) %>% 
  b=nd_rows(.id = 'boots') %>%
  select(-.estimator) %>%
  =pread(.metric, .estimate, convert=TRUE) %>%
  group_by(camera= %>%
  summarize_at(vars(sens, spec, ppv, npv), lst(!!!p_fu=s))


# Now merge together the estimates and CIs and =eshape into more useful format
sensspectable_glaucoma <- full=join(sensspec_estimates_glaucoma, bs_sensspec_glaucoma, by="camera")
=ensspectablelong_glaucoma <- sensspectable_glaucoma %>%
  gathe=(field, value, npv_est:npv_upper95) %>%
  separate(field, int==c("metric", "stat"), sep="_") %>%
  spread(stat, value, =onvert=TRUE) %>%
  write.csv("senspectablelong_glaucoma.csv")

=0A
# numbers for other retinal diagnosis

addmargins(xtabs(=ata=alldata_final, ~otherdx+other_dx_yesno))
alldata_final %>% fi=ter(!is.na(camera)) %>% group_by(camera) %>% sens(., truth = =s.factor(otherdx), estimate = as.factor(other_dx_yesno))
alldata_=inal %>% filter(!is.na(camera)) %>% group_by(camera) %>% spec(.= truth = as.factor(otherdx), estimate = as.factor(other_dx_=esno))

alldata_final$otherdx <- as.factor(alldata_final$otherdx)
=lldata_final$other_dx_yesno <- as.factor(alldata_final$other_dx_yesno)
=lass(alldata_final$otherdx)
class(alldata_final$other_dx_yesno)

conf=atrixtable_otherdx <- alldata_final %>%
  filter(!is.na(camera)= %>%
  group_by(camera) %>%
  conf_mat(otherdx, other_dx_=esno) %>%
  mutate(tidied = map(conf_mat, tidy)) %>%
 =20unnest(tidied) %>%
  spread(name, value) %>%
  rename(b=thpos=cell_1_1,
         bothneg=cell_2_2,
  =20      testpos_gsneg=cell_1_2,
         =estneg_gspos=cell_2_1)

confmat_peek_otherdx <- alldata_final %=%
  filter(camera=="peek") %>%
  conf_mat(otherdx, othe=_dx_yesno)
summary(confmat_peek_otherdx)

confmat_inview_otherdx <-=20alldata_final %>%
  filter(camera=="inview") %>%
  co=f_mat(otherdx, other_dx_yesno)
summary(confmat_inview_otherdx)

con=mat_pictor_otherdx<- alldata_final %>%
  filter(camera=="pict=r") %>%
  conf_mat(otherdx, other_dx_yesno)
summary(confmat_pic=or_otherdx)


sensspec_estimates_otherdx <- alldata_final %>%
=20 filter(!is.na(camera)) %>%
  group_by(camera) %>%
  cl=ss_metrics(., truth = otherdx, estimate = other_dx_yesno) =>%
  select(-.estimator) %>%
  spread(.metric, .estimate, =onvert=TRUE) %>%
  rename(sens_est=sens,
       =20 spec_est=spec,
         npv_est=npv,
   =20     ppv_est=ppv)
# Check
xtabs(data=filter(alldata_f=nal, camera=="peek"), ~otherdx+other_dx_yesno)



## boot=trapped 95% CI accounting for clustering of eyes
bs_sensspe=_otherdx <- map(bs$splits, ~as_tibble(.) %>% unnest %>%
  =20                         =20 filter(!is.na(camera)) %>%
             =20                group_by(camera) %>%
=20                         =20   class_metrics(., truth = as.factor(otherdx), estimate =3D as.factor(other_dx_yesno))) %>%
  bind_rows(.id = 'boots=) %>%
  select(-.estimator) %>%
  spread(.metric, .estima=e, convert=TRUE) %>%
  group_by(camera) %>%
  summarize=at(vars(sens, spec, ppv, npv), lst(!!!p_funs))


# Now me=ge together the estimates and CIs and reshape into more =seful format
sensspectable_otherdx <- full_join(sensspec_estimates_=therdx, bs_sensspec_otherdx, by="camera")
sensspectablelong_otherdx=20<- sensspectable_otherdx %>%
  gather(field, value, npv_est=npv_upper95) %>%
  separate(field, into=c("metric", "stat"), =ep="_") %>%
  spread(stat, value, convert=TRUE) %>%
 =20write.csv("senspectablelong_otherdx.csv")


#######################=########################

# SN for screening for non-prolifer=tive DR

nonprolif_dr_subset <- alldata_final %>%
  filte=(diagnosis_dr == 1 | diagnosis_dr == 2)

addmargins=xtabs(data=nonprolif_dr_subset, ~dr+dr_final))
nonprolif_dr_subset =>% filter(!is.na(camera)) %>% group_by(camera) %>% sens(., trut= = as.factor(dr), estimate = as.factor(dr_final))

confma=rixtable_nonprolif_dr_subset <- nonprolif_dr_subset %>%
  filte=(!is.na(camera)) %>%
  group_by(camera) %>%
  conf_mat(dr, =r_final) %>%
  mutate(tidied = map(conf_mat, tidy)) %>%
=20 unnest(tidied) %>%
  spread(name, value) %>%
  renam=(bothpos=cell_1_1,
         bothneg=cell_2_2,
 =20       testpos_gsneg=cell_1_2,
        =20testneg_gspos=cell_2_1)

confmat_peek_nonprolif_dr_subset <- no=prolif_dr_subset %>%
  filter(camera=="peek") %>%
  con=_mat(dr, dr_final)
summary(confmat_peek_nonprolif_dr_subset)

confm=t_inview_nonprolif_dr_subset <- nonprolif_dr_subset %>%
  filte=(camera=="inview") %>%
  conf_mat(dr, dr_final)
summary(con=mat_inview_nonprolif_dr_subset)

confmat_pictor_nonprolif_dr_subset <= nonprolif_dr_subset %>%
  filter(camera=="pictor") %>%
 =20conf_mat(dr, dr_final)
summary(confmat_pictor_nonprolif_dr_subset)
=0A
# Now to get estimates and CIs, need to do separat=ly. First an object with the estimates
sensspec_estimates_non=rolif_dr_subset <- nonprolif_dr_subset %>%
  filter(!is.na(came=a)) %>%
  group_by(camera) %>%
  class_metrics(., truth =3D dr, estimate = dr_final) %>%
  select(-.estimator) %=%
  spread(.metric, .estimate, convert=TRUE) %>%
  rena=e(sens_est=sens,
         spec_est=spec,
   =20     npv_est=npv,
         ppv_est=ppv)=0A# Check
xtabs(data=filter(nonprolif_dr_subset, camera=="peek"=, ~dr+dr_final)

# So finally here is the dataframe wit= the CIs
bs_sensspec_nonprolif_dr_subset <- map(bs$splits, ~as_=ibble(.) %>% unnest %>% 
              =20         filter(!is.na(camera)) %>%
     =20                  group_by(camera) =>% 
                       =20class_metrics(., truth = dr, estimate = dr_final)) %>% =0A  bind_rows(.id = 'boots') %>%
  select(-.estimator) =>%
  spread(.metric, .estimate, convert=TRUE) %>%
  gro=p_by(camera) %>%
  summarize_at(vars(sens, spec, ppv, npv), =st(!!!p_funs))

# Now merge together the estimates and CI= and reshape into more useful format
sensspectable_nonprolif_=r_subset <- full_join(sensspec_estimates_nonprolif_dr_subset, bs_sens=pec_nonprolif_dr_subset, by="camera")
sensspectablelong_nonprolif_dr_=ubset <- sensspectable_nonprolif_dr_subset %>%
  gather(field, =alue, npv_est:npv_upper95) %>%
  separate(field, into=c("metr=c", "stat"), sep="_") %>%
  spread(stat, value, convert==RUE) %>%
  write.csv("senspectablelong_nonprolif_dr_subset.csv")
=0A



# SN for screening for early/intermed AMD

ea=lyint_amd_subset <- alldata_final %>%
  filter(diagnosis_amd =3D= 1 | diagnosis_amd == 2)

addmargins(xtabs(data=ea=lyint_amd_subset, ~amd+amd_final))
earlyint_amd_subset %>% filter(!=s.na(camera)) %>% group_by(camera) %>% sens(., truth = as.f=ctor(amd), estimate = as.factor(amd_final))
earlyint_amd_subset =>% filter(!is.na(camera)) %>% group_by(camera) %>% spec(., trut= = as.factor(amd), estimate = as.factor(amd_final))

earl=int_amd_subset$amd <- as.factor(earlyint_amd_subset$amd)
earlyint_amd=subset$amd_final <- as.factor(earlyint_amd_subset$amd_final)
class(ea=lyint_amd_subset$amd)
class(earlyint_amd_subset$amd_final)

confmatri=table_earlyint_amd_subset <- earlyint_amd_subset %>%
  filter(!=s.na(camera)) %>%
  group_by(camera) %>%
  conf_mat(amd, =md_final) %>%
  mutate(tidied = map(conf_mat, tidy)) %>%
=20 unnest(tidied) %>%
  spread(name, value) %>%
  renam=(bothpos=cell_1_1,
         bothneg=cell_2_2,
 =20       testpos_gsneg=cell_1_2,
        =20testneg_gspos=cell_2_1)

confmat_peek_earlyint_amd_subset <- ea=lyint_amd_subset %>%
  filter(camera=="peek") %>%
  con=_mat(amd, amd_final)
summary(confmat_peek_amd)

confmat_inview_earl=int_amd_subset <- earlyint_amd_subset %>%
  filter(camera===inview") %>%
  conf_mat(amd, amd_final)
summary(confmat_inview_=md)

confmat_pictor_earlyint_amd_subset <- earlyint_amd_subset %>=
  filter(camera=="pictor") %>%
  conf_mat(amd, amd_fin=l)
summary(confmat_pictor_amd)


sensspec_estimates_earlyint_amd_su=set <- earlyint_amd_subset %>%
  filter(!is.na(camera)) %>%
=20 group_by(camera) %>%
  class_metrics(., truth = amd, =stimate = amd_final) %>%
  select(-.estimator) %>%
  =pread(.metric, .estimate, convert=TRUE) %>%
  rename(sens_est=3Dsens,
         spec_est=spec,
       =20 npv_est=npv,
         ppv_est=ppv)
# Check=0Axtabs(data=filter(earlyint_amd_subset, camera=="peek"), ~amd+am=_final)


## bootstrapped 95% CI accounting for clusterin= of eyes 
bs_sensspec_earlyint_amd_subset <- map(bs$splits, ~=s_tibble(.) %>% unnest %>% 
             =20           filter(!is.na(camera)) %>%
   =20                     group_by(c=mera) %>% 
                    =20    class_metrics(., truth = as.factor(amd), estimate =3D as.factor(amd_final))) %>% 
  bind_rows(.id = 'boots')=20%>%
  select(-.estimator) %>%
  spread(.metric, .estimate= convert=TRUE) %>%
  group_by(camera) %>%
  summarize_a=(vars(sens, spec, ppv, npv), lst(!!!p_funs))


# Now merg= together the estimates and CIs and reshape into more u=eful format
sensspectable_earlyint_amd_subset <- full_join(sensspec=estimates_earlyint_amd_subset, bs_sensspec_earlyint_amd_subset, by="c=mera")
sensspectablelong_earlyint_amd_subset <- sensspectable_earlyin=_amd_subset %>%
  gather(field, value, npv_est:npv_upper95) %=%
  separate(field, into=c("metric", "stat"), sep="_") %>=
  spread(stat, value, convert=TRUE) %>%
  write.csv("s=nspectablelong_earlyint_amd_subset.csv")



# no use calculat=ng SN screening for mild/moderate glaucoma bc only 3 case= of moderate and 0 cases of mild




####### Tabl= 1 #####

# number of eyes & patients before taking =ut missingdata
orig_eyes_data <- alldata %>%
  mutate(study=d_eye=paste(study_id, eye, sep="_")) %>%
  filter(camera =3D= "pictor")
orig_pt_data <- alldata %>%
  group_by(stud=_id) %>%
  mutate(max_photoid = max(photoid, na.rm = =RUE)) %>%
  filter(max_photoid == photoid)
summary(unique=orig_eyes_data$studyid_eye))
summary(unique(orig_pt_data$study_id))

= filter data → one eye per line
eyes_data <- al=data_final %>%
  filter(camera == "pictor")

#filter =ata → one patient per line
pt_data <- alldata_final=20%>%
  group_by(study_id) %>%
  mutate(max_photoid = m=x(photoid, na.rm = TRUE)) %>%
  filter(max_photoid ===20photoid)

# number of unique EYES
summary(unique(eyes_data$=tudyid_eye))

# number of unique PATIENTS
summary(unique(pt_d=ta$study_id))


# age: median 
summary(orig_pt_data$age)
su=mary(pt_data$age)

# gender (1 = female; 0 = male)
=um(orig_pt_data$gender, na.rm = TRUE)
sum(pt_data$gender, na.rm=20= TRUE)

# number of pts with cataracts
sum(pt_data$c=taract == 1 | pt_data$cataract == 2 | pt_data$catar=ct == 3 | pt_data$cataract == 4, na.rm = TRUE)
=NS cataract
sum(pt_data$cataract == 1, na.rm = TRUE)
=cortical cataract
sum(pt_data$cataract == 2, na.rm = TR=E)
#PSC cataract
sum(pt_data$cataract == 3, na.rm = T=UE)
#unspec cataract
sum(pt_data$cataract == 4 | pt_data$=ataract == 5, na.rm = TRUE)
#degree of cataract
sum=pt_data$cataract_deg == 1, na.rm = TRUE)
sum(pt_data$cata=act_deg == 2, na.rm = TRUE)
sum(pt_data$cataract_deg ==3D 3, na.rm = TRUE)
sum(pt_data$cataract_deg == 4, na=rm = TRUE)
sum(pt_data$cataract_deg == 5, na.rm = T=UE)


#camera-specific info
#peek time & discomfort
peek_=nly <- alldata_final %>%
  filter(camera == "peek")
s=mmary(peek_only$seconds)
summary(peek_only$discomfort)
#inview time = discomfort
inview_only <- alldata_final %>%
  filter(camer= == "inview")
summary(inview_only$seconds)
summary(inview_only$=iscomfort)
#pictor time & discomfort
pictor_only <- alldata_f=nal %>%
  filter(camera == "pictor")
summary(pictor_only$=econds)
summary(pictor_only$discomfort)

#image_clarity...number of=20eyes with:
#...excellent clarity
sum(peek_only$image_clarity_fina= == 4, na.rm = TRUE)
sum(inview_only$image_clarity_final =3D= 4, na.rm = TRUE)
sum(pictor_only$image_clarity_final ==3D 4, na.rm = TRUE)
#...good clarity
sum(peek_only$image_cl=rity_final == 3, na.rm = TRUE)
sum(inview_only$image_clar=ty_final == 3, na.rm = TRUE)
sum(pictor_only$image_clarit=_final == 3, na.rm = TRUE)
#...fair clarity
sum(peek_=nly$image_clarity_final == 2, na.rm = TRUE)
sum(inview_on=y$image_clarity_final == 2, na.rm = TRUE)
sum(pictor_only=image_clarity_final == 2, na.rm = TRUE)
#...poor clarit=
sum(peek_only$image_clarity_final == 1, na.rm = TRUE)
=um(inview_only$image_clarity_final == 1, na.rm = TRUE)
su=(pictor_only$image_clarity_final == 1, na.rm = TRUE)


=photo quality...coverage of the optic nerve
#fully vis
sum(=eek_only$nerve_final == 2, na.rm = TRUE)
sum(inview_only$=erve_final == 2, na.rm = TRUE)
sum(pictor_only$nerve_fina= == 2, na.rm = TRUE)
#partly vis
sum(peek_only$nerve_=inal == 1, na.rm = TRUE)
sum(inview_only$nerve_final ==3D 1, na.rm = TRUE)
sum(pictor_only$nerve_final == 1, =a.rm = TRUE)
#absent
sum(peek_only$nerve_final == 0, na=rm = TRUE)
sum(inview_only$nerve_final == 0, na.rm = =RUE)
sum(pictor_only$nerve_final == 0, na.rm = TRUE)

=0A#photo quality...coverage of the macula
#fully vis
sum(peek=only$macula_final == 2, na.rm = TRUE)
sum(inview_only$mac=la_final == 2, na.rm = TRUE)
sum(pictor_only$macula_final=20== 2, na.rm = TRUE)
#partly vis
sum(peek_only$macula_=inal == 1, na.rm = TRUE)
sum(inview_only$macula_final ==3D 1, na.rm = TRUE)
sum(pictor_only$macula_final == 1, =a.rm = TRUE)
#absent
sum(peek_only$macula_final == 0, n=.rm = TRUE)
sum(inview_only$macula_final == 0, na.rm ==20TRUE)
sum(pictor_only$macula_final == 0, na.rm = TRUE)
=0A
#photo quality...coverage of zone 2
#fully vis
sum(peek_=nly$zone2_final == 2, na.rm = TRUE)
sum(inview_only$zone2=final == 2, na.rm = TRUE)
sum(pictor_only$zone2_final ==3D 2, na.rm = TRUE)
#partly vis
sum(peek_only$zone2_final =3D= 1, na.rm = TRUE)
sum(inview_only$zone2_final == 1= na.rm = TRUE)
sum(pictor_only$zone2_final == 1, na.rm =3D TRUE)
#absent
sum(peek_only$zone2_final == 0, na.rm ==20TRUE)
sum(inview_only$zone2_final == 0, na.rm = TRUE)
=um(pictor_only$zone2_final == 0, na.rm = TRUE)


#con=idence in grading vcdr 
#able, confident
sum(peek_only$vcdr_c=nfidence_final == 1, na.rm = TRUE)
sum(inview_only$vcdr_c=nfidence_final == 1, na.rm = TRUE)
sum(pictor_only$vcdr_c=nfidence_final == 1, na.rm = TRUE)
#able, NOT confide=t
sum(peek_only$vcdr_confidence_final == 2, na.rm = TRUE)=0Asum(inview_only$vcdr_confidence_final == 2, na.rm = TRUE)=0Asum(pictor_only$vcdr_confidence_final == 2, na.rm = TRUE)=0A#unable to grade
sum(peek_only$vcdr_confidence_final == 3, =a.rm = TRUE)
sum(inview_only$vcdr_confidence_final == 3, =a.rm = TRUE)
sum(pictor_only$vcdr_confidence_final == 3, =a.rm = TRUE)


#total eyes with DR
eyes_data <- eye=_data %>%
  mutate(dr=as.numeric(dr))
eyes_data$dr[eyes_data$dr=20== 1] <- 0
eyes_data$dr[eyes_data$dr == 2] <- 1
=um(eyes_data$dr, na.rm = TRUE)

#eyes with mild or mode=ate NPDR
sum(eyes_data$diagnosis_dr == 1, na.rm = TRUE)=0A#eyes with severe NPDR
sum(eyes_data$diagnosis_dr == 2, =a.rm = TRUE)
#eyes with PDR
sum(eyes_data$diagnosis_dr ===203, na.rm = TRUE)
#eyes with unspecified DR
sum(eyes_dat=$diagnosis_dr == 4, na.rm = TRUE)

#eyes with DME
=um(eyes_data$dme, na.rm = TRUE)


#total eyes with AMD
=yes_data <- eyes_data %>%
  mutate(amd=as.numeric(amd))
eye=_data$amd[eyes_data$amd == 1] <- 0
eyes_data$amd[eyes_data$am= == 2] <- 1
sum(eyes_data$amd, na.rm = TRUE)

#ey=s with early amd
sum(eyes_data$diagnosis_amd == 1, na.rm =3D TRUE)
#eyes with intermed amd
sum(eyes_data$diagnosis_amd =3D= 2, na.rm = TRUE)
#eyes with advanced amd
sum(eyes=data$diagnosis_amd == 3, na.rm = TRUE)
#eyes with uns=ec amd
sum(eyes_data$diagnosis_amd == 4, na.rm = TRUE)
=0A
#total eyes with glaucoma
eyes_data <- eyes_data %>%
 =20mutate(glaucoma=as.numeric(glaucoma))
eyes_data$glaucoma[eyes_data$gl=ucoma == 1] <- 0
eyes_data$glaucoma[eyes_data$glaucoma ===202] <- 1
sum(eyes_data$glaucoma, na.rm = TRUE)

#eyes =ith mild/early glauc
sum(eyes_data$diagnosis_glaucoma == 1, =a.rm = TRUE)
#eyes with mod glauc
sum(eyes_data$diagnosis_g=aucoma == 2, na.rm = TRUE)
#eyes with severe glauc
=um(eyes_data$diagnosis_glaucoma == 3, na.rm = TRUE)
#eyes=20with unspec glauc
sum(eyes_data$diagnosis_glaucoma == 4, =a.rm = TRUE)
#eyes with NA info on glauc
sum(eyes_data$=iagnosis_glaucoma == 5, na.rm = TRUE)


#total eyes=20with other dx
eyes_data <- eyes_data %>%
  mutate(other=x=as.numeric(otherdx))
eyes_data$otherdx[eyes_data$otherdx == 1= <- 0
eyes_data$otherdx[eyes_data$otherdx == 2] <- 1
su=(eyes_data$otherdx, na.rm = TRUE)

list_otherdx <- eyes_dat= %>% 
  select(other_retinal_path) %>%
  filter(!(is.na(o=her_retinal_path))) %>%
  write.csv("list_otherdx.csv")



=0A









###########################################=#####################################################
###################=###########################################################################=#
#######################################################################=#########################
###############################################=#################################################
#######################=#########################################################################
= PHOTO ADJUDICATION FOR TEAM 1

team1_discrep <- team1 =>%
  filter((amd_yesno.1!=amd_yesno.2) | (dr_yesno.1!=dr_yesn=.2) | (abs(cdr.1-cdr.2)>.2)) %>%
  select(id, amd_yesno.1, =md_yesno.2, dr_yesno.1, dr_yesno.2, cdr.1, cdr.2) %>%
  wri=e.csv("team1_discrep.csv")


# PHOTO ADJUDICATION FOR TEAM =

team2_import <- read_csv("Team2_DATA_2019-05-13_0820.csv")

#=20CONFIRM LEVELS OF AMD AND DR:
xtabs(data=team2_import, ~d=+amd, addNA=TRUE)

team2 <- team2_import %>%
  mutate(d=_yesno = ifelse((dr == 0 | dr == 1), 0, 1)) =>%
  mutate(amd_yesno = ifelse((amd == 0 | amd ==3D 1), 0, 1)) %>%
  select(study_id, dr_yesno, dr, amd_=esno, amd, amd_severity, cdr) %>%
  separate(study_id, c("i=", "grader"), sep = "--", remove = TRUE, convert = =ALSE) %>%
  gather(field, value, dr_yesno:cdr) %>%
  mu=ate(field.grader=paste(field, grader, sep=".")) %>%
  selec=(-field, -grader) %>%
  spread(field.grader, value, convert==RUE)

xtabs(data=team2, ~dr_yesno.1+dr_yesno.2, addNA=TRUE)
x=abs(data=team2, ~amd_yesno.1+amd_yesno.2, addNA=TRUE)
xtabs(data==ilter(team2, amd_yesno.1==1 & amd_yesno.2==0), ~amd_severit=.1)

team2_discrep <- team2 %>%
  filter((amd_yesno.1!=am=_yesno.2) | (dr_yesno.1!=dr_yesno.2) | (abs(cdr.1-cdr.2)>.2)) %=%
  select(id, amd_yesno.1, amd_yesno.2, dr_yesno.1, dr_yesno=2, cdr.1, cdr.2) %>%
  write.csv("team2_discrep.csv")


#=20PHOTO ADJUDICATION FOR TEAM 3

team3_import <- read_csv("=eam3_DATA_2019-06-23_1055.csv")

# CONFIRM LEVELS OF AMD AND =R:
xtabs(data=team3_import, ~dr+amd, addNA=TRUE)

team3 <- =eam3_import %>%
  mutate(dr_yesno = ifelse((dr == 0 = dr == 1), 0, 1)) %>%
  mutate(amd_yesno = ifel=e((amd == 0 | amd == 1), 0, 1)) %>%
  selec=(study_id, dr_yesno, dr, amd_yesno, amd, amd_severity, cdr) %=%
  separate(study_id, c("id", "grader"), sep = "--", r=move = TRUE, convert = FALSE) %>%
  gather(field, v=lue, dr_yesno:cdr) %>%
  mutate(field.grader=paste(field, gra=er, sep=".")) %>%
  select(-field, -grader) %>%
  spr=ad(field.grader, value, convert=TRUE)

xtabs(data=team3, ~dr_=esno.1+dr_yesno.2, addNA=TRUE)
xtabs(data=team3, ~amd_yesno.1+amd=yesno.2, addNA=TRUE)
xtabs(data=filter(team3, amd_yesno.1==1 = amd_yesno.2==0), ~amd_severity.1)

team3_discrep <- team3 =>%
  filter((amd_yesno.1!=amd_yesno.2) | (dr_yesno.1!=dr_yesn=.2) | (abs(cdr.1-cdr.2)>.2)) %>%
  select(id, amd_yesno.1, =md_yesno.2, dr_yesno.1, dr_yesno.2, cdr.1, cdr.2) %>%
  wri=e.csv("team3_discrep.csv")


# PHOTO ADJUDICATION FOR TEAM =

team4_import <- read_csv("Team4_DATA_2019-06-23_1056.csv")

#=20CONFIRM LEVELS OF AMD AND DR:
xtabs(data=team4_import, ~d=+amd, addNA=TRUE)

team4 <- team4_import %>%
  mutate(d=_yesno = ifelse((dr == 0 | dr == 1), 0, 1)) =>%
  mutate(amd_yesno = ifelse((amd == 0 | amd ==3D 1), 0, 1)) %>%
  select(study_id, dr_yesno, dr, amd_=esno, amd, amd_severity, cdr) %>%
  separate(study_id, c("i=", "grader"), sep = "--", remove = TRUE, convert = =ALSE) %>%
  gather(field, value, dr_yesno:cdr) %>%
  mu=ate(field.grader=paste(field, grader, sep=".")) %>%
  selec=(-field, -grader) %>%
  spread(field.grader, value, convert==RUE)

xtabs(data=team4, ~dr_yesno.1+dr_yesno.2, addNA=TRUE)
x=abs(data=team4, ~amd_yesno.1+amd_yesno.2, addNA=TRUE)
xtabs(data==ilter(team4, amd_yesno.1==1 & amd_yesno.2==0), ~amd_severit=.1)

team4_discrep <- team4 %>%
  filter((amd_yesno.1!=am=_yesno.2) | (dr_yesno.1!=dr_yesno.2) | (abs(cdr.1-cdr.2)>.2)) %=%
  select(id, amd_yesno.1, amd_yesno.2, dr_yesno.1, dr_yesno=2, cdr.1, cdr.2) %>%
  write.csv("team4_discrep.csv")


#=EAM 1: replace discrepant grading with adjudicated grades

= import adjudication dataset -> adj_team1
adj_team1 <- read=csv("Team1_Adjudic_2019-07-12_1539.csv")

# rename record_id colu=n to "study_id", then add "--3"
colnames(adj_team1)[1] <- "=tudy_id"
adj_team1 <- adj_team1[,-2]
adj_team1$study_id <- past=0(adj_team1$study_id, "--3")

# merge team1_import with adj_t=am 1
final_team1 <- rbind(team1_import, adj_team1)

final_tea=1 <- final_team1 %>%
  separate(study_id, c("id", "grader")= sep = "--", remove = TRUE, convert = FALSE) %>%
=20 gather(field, value, image_clarity:notes) %>%
  mutate(fie=d.grader=paste(field, grader, sep=".")) %>%
  select(-field= -grader) %>%
  spread(field.grader, value, convert=TRUE)
=0A#remove column "new_photo_grading_form_complete"
final_team1 <- =inal_team1[,-2]


# if variable.3 != NA, then variable.=inal = variable.3. if variable.3 = NA, then variable.fi=al = variable.1
final_team1 <- final_team1 %>%
  mutate=amd_severity = if_else(is.na(amd_severity.3)==FALSE, amd_severi=y.3, amd_severity.1),
         amd = if_else(is=na(amd.3)==FALSE, amd.3, amd.1),
         cdr =3D if_else(is.na(cdr.3)==FALSE, cdr.3, cdr.1),
      =20  dme = if_else(is.na(dme.3)==FALSE, dme.3, dme.1),
 =20       dr_severity = if_else(is.na(dr_severity.3)===ALSE, dr_severity.3, dr_severity.1),
         dr =3D if_else(is.na(dr.3)==FALSE, dr.3, dr.1),
       =20 image_clarity = if_else(is.na(image_clarity.3)==FALSE, ima=e_clarity.3, image_clarity.1),
         macula = =f_else(is.na(macula.3)==FALSE, macula.3, macula.1),
     =20   nerve = if_else(is.na(nerve.3)==FALSE, nerve.3, ne=ve.1),
         notes = if_else(is.na(notes.3)==3DFALSE, notes.3, notes.1),
         other_dx_detai= = if_else(is.na(other_dx_detail.3)==FALSE, other_dx_detail.3, =ther_dx_detail.1),
         other_dx = if_else(is=na(other_dx.3)==FALSE, other_dx.3, other_dx.1),
      =20  vcdr_confidence = if_else(is.na(vcdr_confidence.3)==FALSE= vcdr_confidence.3, vcdr_confidence.1),
         zo=e2 = if_else(is.na(zone2.3)==FALSE, zone2.3, zone2.1))

f=nal_team1 <- final_team1 %>%
  select(id,amd_severity:zone2)
=0A
#TEAM 2: replace discrepant grading with adjudicated gra=es

# import adjudication dataset -> adj_team2
adj_team2 =- read_csv("Team2_Adjudic_2019-07-15_1702.csv")

# rename record_=d column to "study_id", then add "--3"
colnames(adj_team2)[1]=20<- "study_id"
adj_team2 <- adj_team2[,-2]
adj_team2$study_id =- paste0(adj_team2$study_id, "--3")

# merge team1_import wit= adj_team 1
final_team2 <- rbind(team2_import, adj_team2)

=inal_team2 <- final_team2 %>%
  separate(study_id, c("id", =grader"), sep = "--", remove = TRUE, convert = FALS=) %>%
  gather(field, value, image_clarity:notes) %>%
  =utate(field.grader=paste(field, grader, sep=".")) %>%
  sel=ct(-field, -grader) %>%
  spread(field.grader, value, convert=3DTRUE)

#remove column "new_photo_grading_form_complete"
final_t=am2 <- final_team2[,-2]


# if variable.3 != NA, then=20variable.final = variable.3. if variable.3 = NA, then =ariable.final = variable.1
final_team2 <- final_team2 %>%
 =20mutate(amd_severity = if_else(is.na(amd_severity.3)==FALSE, a=d_severity.3, amd_severity.1),
         amd = i=_else(is.na(amd.3)==FALSE, amd.3, amd.1),
        =20cdr = if_else(is.na(cdr.3)==FALSE, cdr.3, cdr.1),
   =20     dme = if_else(is.na(dme.3)==FALSE, dme.3, dm=.1),
         dr_severity = if_else(is.na(dr_seve=ity.3)==FALSE, dr_severity.3, dr_severity.1),
       =20 dr = if_else(is.na(dr.3)==FALSE, dr.3, dr.1),
   =20     image_clarity = if_else(is.na(image_clarity.3)===ALSE, image_clarity.3, image_clarity.1),
         m=cula = if_else(is.na(macula.3)==FALSE, macula.3, macula.1),
=20        nerve = if_else(is.na(nerve.3)==FALSE, =erve.3, nerve.1),
         notes = if_else(is.n=(notes.3)==FALSE, notes.3, notes.1),
         o=her_dx_detail = if_else(is.na(other_dx_detail.3)==FALSE, other_=x_detail.3, other_dx_detail.1),
         other_dx ==20if_else(is.na(other_dx.3)==FALSE, other_dx.3, other_dx.1),
  =20      vcdr_confidence = if_else(is.na(vcdr_confidence.3===FALSE, vcdr_confidence.3, vcdr_confidence.1),
      =20  zone2 = if_else(is.na(zone2.3)==FALSE, zone2.3, zone2=1))

final_team2 <- final_team2 %>%
  select(id,amd_severit=:zone2)



#TEAM 3: replace discrepant grading with adj=dicated grades

# import adjudication dataset -> adj_team3
=dj_team3 <- read_csv("Team3_Adjudic_2019-07-17_1624.csv")

# rena=e record_id column to "study_id", then add "--3"
colnames(a=j_team3)[1] <- "study_id"
adj_team3 <- adj_team3[,-2]
adj_team3=study_id <- paste0(adj_team3$study_id, "--3")

# merge team3_=mport with adj_team 1
final_team3 <- rbind(team3_import, adj_=eam3)

final_team3 <- final_team3 %>%
  separate(study_id, =("id", "grader"), sep = "--", remove = TRUE, convert =3D FALSE) %>%
  gather(field, value, image_clarity:notes) %=%
  mutate(field.grader=paste(field, grader, sep=".")) %>%
=20 select(-field, -grader) %>%
  spread(field.grader, value, =onvert=TRUE)

#remove column "new_photo_grading_form_complete"
=inal_team3 <- final_team3[,-2]


# if variable.3 != NA,=20then variable.final = variable.3. if variable.3 = NA, =hen variable.final = variable.1
final_team3 <- final_team3 =>%
  mutate(amd_severity = if_else(is.na(amd_severity.3)==FA=SE, amd_severity.3, amd_severity.1),
         amd =3D if_else(is.na(amd.3)==FALSE, amd.3, amd.1),
      =20  cdr = if_else(is.na(cdr.3)==FALSE, cdr.3, cdr.1),
 =20       dme = if_else(is.na(dme.3)==FALSE, dme.3= dme.1),
         dr_severity = if_else(is.na(d=_severity.3)==FALSE, dr_severity.3, dr_severity.1),
     =20   dr = if_else(is.na(dr.3)==FALSE, dr.3, dr.1),
 =20       image_clarity = if_else(is.na(image_clarity.3)=3D=FALSE, image_clarity.3, image_clarity.1),
        =20macula = if_else(is.na(macula.3)==FALSE, macula.3, macula.1=,
         nerve = if_else(is.na(nerve.3)==FA=SE, nerve.3, nerve.1),
         notes = if_el=e(is.na(notes.3)==FALSE, notes.3, notes.1),
       =20 other_dx_detail = if_else(is.na(other_dx_detail.3)==FALSE, =ther_dx_detail.3, other_dx_detail.1),
         other_=x = if_else(is.na(other_dx.3)==FALSE, other_dx.3, other_dx.1)=
         vcdr_confidence = if_else(is.na(vcdr_co=fidence.3)==FALSE, vcdr_confidence.3, vcdr_confidence.1),
   =20     zone2 = if_else(is.na(zone2.3)==FALSE, zone2.3= zone2.1))

final_team3 <- final_team3 %>%
  select(id,am=_severity:zone2)




#sensitivity for DR
alldata_final <-=20alldata_final %>%
  mutate(dr_true_pos_peek = ifelse(((dr_f=nal==1) & (dr==1)) & camera=="peek", 1, 0)) %>%
=20 mutate(dr_false_neg_peek = ifelse(((dr_final==0) & (dr==3D1)) & camera=="peek", 1, 0)) %>%  
  mutate(dr_tr=e_pos_inview = ifelse(((dr_final==1) & (dr==1)) & cam=ra=="inview", 1, 0)) %>%
  mutate(dr_false_neg_inview ==20ifelse(((dr_final==0) & (dr==1)) & camera=="inview", =, 0)) %>%
  mutate(dr_true_pos_pictor = ifelse(((dr_final==3D1) & (dr==1)) & camera=="pictor", 1, 0)) %>%
  =utate(dr_false_neg_pictor = ifelse(((dr_final==0) & (dr===)) & camera=="pictor", 1, 0))

sensitivity_dr_peek <- s=m(alldata_final$dr_true_pos_peek, na.rm=TRUE)/(sum(alldata_final$dr_tru=_pos_peek, na.rm=TRUE)+sum(alldata_final$dr_false_neg_peek, na.rm=T=UE))
sensitivity_dr_inview <- sum(alldata_final$dr_true_pos_inview, =a.rm=TRUE)/(sum(alldata_final$dr_true_pos_inview, na.rm=TRUE)+sum(all=ata_final$dr_false_neg_inview, na.rm=TRUE))
sensitivity_dr_pictor <= sum(alldata_final$dr_true_pos_pictor, na.rm=TRUE)/(sum(alldata_final=dr_true_pos_pictor, na.rm=TRUE)+sum(alldata_final$dr_false_neg_pictor, =a.rm=TRUE))

sensitivity_dr_peek
sensitivity_dr_inview
sensitivit=_dr_pictor


#sensitivity for DME
alldata <- alldata %>%
=20 mutate(dme_true_pos_peek = ifelse(((dme_right==1 | dme_l=ft==1) & (dme==2|dme==3)) & camera=="peek", 1, =)) %>%
  mutate(dme_false_neg_peek = ifelse(((dme_right=== | dme_left==1) & (dme==0|dme==1)) & camera=="p=ek", 1, 0)) %>%  
  mutate(dme_true_pos_inview = ifel=e(((dme_right==1 | dme_left==1) & (dme==2|dme==3)) = camera=="inview", 1, 0)) %>%
  mutate(dme_false_neg_invi=w = ifelse(((dme_right==1 | dme_left==1) & (dme===|dme==1)) & camera=="inview", 1, 0)) %>%
  mutate(d=e_true_pos_pictor = ifelse(((dme_right==1 | dme_left==1) = (dme==2|dme==3)) & camera=="pictor", 1, 0)) %>%
=20 mutate(dme_false_neg_pictor = ifelse(((dme_right==1 | dm=_left==1) & (dme==0|dme==1)) & camera=="pictor", =, 0))

sensitivity_dme_peek <- sum(alldata$dme_true_pos_peek, n=.rm=TRUE)/(sum(alldata$dme_true_pos_peek, na.rm=TRUE)+sum(alldata$dme=false_neg_peek, na.rm=TRUE))
sensitivity_dme_inview <- sum(alldat=$dme_true_pos_inview, na.rm=TRUE)/(sum(alldata$dme_true_pos_inview, n=.rm=TRUE)+sum(alldata$dme_false_neg_inview, na.rm=TRUE))
sensitivit=_dme_pictor <- sum(alldata$dme_true_pos_pictor, na.rm=TRUE)/(sum(al=data$dme_true_pos_pictor, na.rm=TRUE)+sum(alldata$dme_false_neg_pictor,=20na.rm=TRUE))

sensitivity_dme_peek
sensitivity_dme_inview
sensi=ivity_dme_pictor

#sensitivity for amd
alldata <- alldata %=%
  mutate(amd_true_pos_peek = ifelse(((amd_right==1 | =md_left==1) & (amd==2|amd==3)) & camera=="peek", =, 0)) %>%
  mutate(amd_false_neg_peek = ifelse(((amd_right==3D1 | amd_left==1) & (amd==0|amd==1)) & camera===peek", 1, 0)) %>%  
  mutate(amd_true_pos_inview = if=lse(((amd_right==1 | amd_left==1) & (amd==2|amd==3)= & camera=="inview", 1, 0)) %>%
  mutate(amd_false_neg_=nview = ifelse(((amd_right==1 | amd_left==1) & (amd==3D0|amd==1)) & camera=="inview", 1, 0)) %>%
  mutat=(amd_true_pos_pictor = ifelse(((amd_right==1 | amd_left===) & (amd==2|amd==3)) & camera=="pictor", 1, 0)) %=%
  mutate(amd_false_neg_pictor = ifelse(((amd_right==1 |=20amd_left==1) & (amd==0|amd==1)) & camera=="pictor=, 1, 0))

sensitivity_amd_peek <- sum(alldata$amd_true_pos_peek= na.rm=TRUE)/(sum(alldata$amd_true_pos_peek, na.rm=TRUE)+sum(alldat=$amd_false_neg_peek, na.rm=TRUE))
sensitivity_amd_inview <- sum(a=ldata$amd_true_pos_inview, na.rm=TRUE)/(sum(alldata$amd_true_pos_inview= na.rm=TRUE)+sum(alldata$amd_false_neg_inview, na.rm=TRUE))
sensi=ivity_amd_pictor <- sum(alldata$amd_true_pos_pictor, na.rm=TRUE)/(s=m(alldata$amd_true_pos_pictor, na.rm=TRUE)+sum(alldata$amd_false_neg_pi=tor, na.rm=TRUE))

sensitivity_amd_peek
sensitivity_amd_inview
=ensitivity_amd_pictor

#sensitivity for glaucoma
alldata <- a=ldata %>%
  mutate(glaucoma_true_pos_peek = ifelse((glaucoma_=ight==1 | glaucoma_left==1) & cdr>=0.7 & camera===peek", 1, 0)) %>%
  mutate(glaucoma_false_neg_peek = ifel=e((glaucoma_right==1 | glaucoma_left==1) & cdr<0.7 & ca=era=="peek", 1, 0)) %>%  
  mutate(glaucoma_true_pos_in=iew = ifelse((glaucoma_right==1 | glaucoma_left==1) & =dr>=0.7 & camera=="inview", 1, 0)) %>%
  mutate(glauc=ma_false_neg_inview = ifelse((glaucoma_right==1 | glaucoma_le=t==1) & cdr<0.7 & camera=="inview", 1, 0)) %>%
  =utate(glaucoma_true_pos_pictor = ifelse((glaucoma_right==1 | =laucoma_left==1) & cdr>=0.7 & camera=="pictor", 1, 0)= %>%
  mutate(glaucoma_false_neg_pictor = ifelse((glaucoma_ri=ht==1 | glaucoma_left==1) & cdr<0.7 & camera=="pict=r", 1, 0))

sensitivity_glaucoma_peek <- sum(alldata$glaucoma_t=ue_pos_peek, na.rm=TRUE)/(sum(alldata$glaucoma_true_pos_peek, na.rm==RUE)+sum(alldata$glaucoma_false_neg_peek, na.rm=TRUE))
sensitivity_gl=ucoma_inview <- sum(alldata$glaucoma_true_pos_inview, na.rm=TRUE)/(=um(alldata$glaucoma_true_pos_inview, na.rm=TRUE)+sum(alldata$glaucoma_f=lse_neg_inview, na.rm=TRUE))
sensitivity_glaucoma_pictor <- sum(a=ldata$glaucoma_true_pos_pictor, na.rm=TRUE)/(sum(alldata$glaucoma_true_=os_pictor, na.rm=TRUE)+sum(alldata$glaucoma_false_neg_pictor, na.rm==RUE))

sensitivity_glaucoma_peek
sensitivity_glaucoma_inview
sensit=vity_glaucoma_pictor

#sensitivity for other dx
alldata <- =lldata %>%
  mutate(otherdx_true_pos_peek = ifelse(((otherdx_=ight==1 | otherdx_left==1) & (other_dx==2|other_dx===)) & camera=="peek", 1, 0)) %>%
  mutate(otherdx_false_=eg_peek = ifelse(((otherdx_right==1 | otherdx_left==1) = (other_dx==0|other_dx==1)) & camera=="peek", 1, 0)) =>%  
  mutate(otherdx_true_pos_inview = ifelse(((otherdx_ri=ht==1 | otherdx_left==1) & (other_dx==2|other_dx==3=) & camera=="inview", 1, 0)) %>%
  mutate(otherdx_false=neg_inview = ifelse(((otherdx_right==1 | otherdx_left==1)=20& (other_dx==0|other_dx==1)) & camera=="inview", 1, =)) %>%
  mutate(otherdx_true_pos_pictor = ifelse(((otherdx_ri=ht==1 | otherdx_left==1) & (other_dx==2|other_dx==3=) & camera=="pictor", 1, 0)) %>%
  mutate(otherdx_false=neg_pictor = ifelse(((otherdx_right==1 | otherdx_left==1)=20& (other_dx==0|other_dx==1)) & camera=="pictor", 1, =))

sensitivity_otherdx_peek <- sum(alldata$otherdx_true_pos_peek, =a.rm=TRUE)/(sum(alldata$otherdx_true_pos_peek, na.rm=TRUE)+sum(alldat=$otherdx_false_neg_peek, na.rm=TRUE))
sensitivity_otherdx_inview <-=20sum(alldata$otherdx_true_pos_inview, na.rm=TRUE)/(sum(alldata$otherdx=true_pos_inview, na.rm=TRUE)+sum(alldata$otherdx_false_neg_inview, na=rm=TRUE))
sensitivity_otherdx_pictor <- sum(alldata$otherdx_true_po=_pictor, na.rm=TRUE)/(sum(alldata$otherdx_true_pos_pictor, na.rm=TR=E)+sum(alldata$otherdx_false_neg_pictor, na.rm=TRUE))

sensitivity_=therdx_peek
sensitivity_otherdx_inview
sensitivity_otherdx_pictor

=0A#specificity for DR
alldata <- alldata %>%
  mutate(dr_=rue_neg_peek = ifelse(((dr_right==0 | dr_left==0) & (=r==0|dr==1)) & camera=="peek", 1, 0)) %>%
  mut=te(dr_false_pos_peek = ifelse(((dr_right==0 | dr_left==0)=20& (dr==2|dr==3)) & camera=="peek", 1, 0)) %>%  =0A  mutate(dr_true_neg_inview = ifelse(((dr_right==0 | dr=left==0) & (dr==0|dr==1)) & camera=="inview", 1, =)) %>%
  mutate(dr_false_pos_inview = ifelse(((dr_right=== | dr_left==0) & (dr==2|dr==3)) & camera=="invi=w", 1, 0)) %>%
  mutate(dr_true_neg_pictor = ifelse(((dr_=ight==0 | dr_left==0) & (dr==0|dr==1)) & camera=3D="pictor", 1, 0)) %>%
  mutate(dr_false_pos_pictor = =felse(((dr_right==0 | dr_left==0) & (dr==2|dr==3)) = camera=="pictor", 1, 0))

specificity_dr_peek <- sum(all=ata$dr_true_neg_peek, na.rm=TRUE)/(sum(alldata$dr_true_neg_peek, na.r==TRUE)+sum(alldata$dr_false_pos_peek, na.rm=TRUE))
specificity_dr_i=view <- sum(alldata$dr_true_neg_inview, na.rm=TRUE)/(sum(alldata$dr=true_neg_inview, na.rm=TRUE)+sum(alldata$dr_false_pos_inview, na.rm==RUE))
specificity_dr_pictor <- sum(alldata$dr_true_neg_pictor, na.r==TRUE)/(sum(alldata$dr_true_neg_pictor, na.rm=TRUE)+sum(alldata$dr_fa=se_pos_pictor, na.rm=TRUE))

specificity_dr_peek
specificity_dr_i=view
specificity_dr_pictor


#specificity for DME
alldata <= alldata %>%
  mutate(dme_true_neg_peek = ifelse(((dme_righ===0 | dme_left==0) & (dme==0|dme==1)) & camera==3D"peek", 1, 0)) %>%
  mutate(dme_false_pos_peek = ifelse=((dme_right==0 | dme_left==0) & (dme==2|dme==3)) = camera=="peek", 1, 0)) %>%  
  mutate(dme_true_neg_i=view = ifelse(((dme_right==0 | dme_left==0) & (dme==3D0|dme==1)) & camera=="inview", 1, 0)) %>%
  mutat=(dme_false_pos_inview = ifelse(((dme_right==0 | dme_left===) & (dme==2|dme==3)) & camera=="inview", 1, 0)) %=%
  mutate(dme_true_neg_pictor = ifelse(((dme_right==0 | =me_left==0) & (dme==0|dme==1)) & camera=="pictor", =, 0)) %>%
  mutate(dme_false_pos_pictor = ifelse(((dme_righ===0 | dme_left==0) & (dme==2|dme==3)) & camera==3D"pictor", 1, 0))

specificity_dme_peek <- sum(alldata$dme_tru=_neg_peek, na.rm=TRUE)/(sum(alldata$dme_true_neg_peek, na.rm=TRUE)+=um(alldata$dme_false_pos_peek, na.rm=TRUE))
specificity_dme_inview =- sum(alldata$dme_true_neg_inview, na.rm=TRUE)/(sum(alldata$dme_true_=eg_inview, na.rm=TRUE)+sum(alldata$dme_false_pos_inview, na.rm=TRUE=)
specificity_dme_pictor <- sum(alldata$dme_true_neg_pictor, na.rm==RUE)/(sum(alldata$dme_true_neg_pictor, na.rm=TRUE)+sum(alldata$dme_fals=_pos_pictor, na.rm=TRUE))

specificity_dme_peek
specificity_dme_i=view
specificity_dme_pictor


#specificity for AMD
alldata =- alldata %>%
  mutate(amd_true_neg_peek = ifelse(((amd_rig=t==0 | amd_left==0) & (amd==0|amd==1)) & camera=3D="peek", 1, 0)) %>%
  mutate(amd_false_pos_peek = ife=se(((amd_right==0 | amd_left==0) & (amd==2|amd==3))=20& camera=="peek", 1, 0)) %>%  
  mutate(amd_true_ne=_inview = ifelse(((amd_right==0 | amd_left==0) & (amd=3D=0|amd==1)) & camera=="inview", 1, 0)) %>%
  mu=ate(amd_false_pos_inview = ifelse(((amd_right==0 | amd_left==3D0) & (amd==2|amd==3)) & camera=="inview", 1, 0)) =>%
  mutate(amd_true_neg_pictor = ifelse(((amd_right==0 |=20amd_left==0) & (amd==0|amd==1)) & camera=="pictor=, 1, 0)) %>%
  mutate(amd_false_pos_pictor = ifelse(((amd=right==0 | amd_left==0) & (amd==2|amd==3)) & ca=era=="pictor", 1, 0))

specificity_amd_peek <- sum(alldata$=md_true_neg_peek, na.rm=TRUE)/(sum(alldata$amd_true_neg_peek, na.rm==RUE)+sum(alldata$amd_false_pos_peek, na.rm=TRUE))
specificity_amd_inv=ew <- sum(alldata$amd_true_neg_inview, na.rm=TRUE)/(sum(alldata$amd=true_neg_inview, na.rm=TRUE)+sum(alldata$amd_false_pos_inview, na.rm==RUE))
specificity_amd_pictor <- sum(alldata$amd_true_neg_pictor, na=rm=TRUE)/(sum(alldata$amd_true_neg_pictor, na.rm=TRUE)+sum(alldata$am=_false_pos_pictor, na.rm=TRUE))

specificity_amd_peek
specificity=amd_inview
specificity_amd_pictor


#specificity for glaucoma
=lldata <- alldata %>%
  mutate(glaucoma_true_neg_peek = i=else((glaucoma_right==0 | glaucoma_left==0) & cdr<0.7 & =amera=="peek", 1, 0)) %>%
  mutate(glaucoma_false_pos_peek =3D ifelse((glaucoma_right==0 | glaucoma_left==0) & cdr>==.7 & camera=="peek", 1, 0)) %>%  
  mutate(glaucoma=true_neg_inview = ifelse((glaucoma_right==0 | glaucoma_left==3D0) & cdr<0.7 & camera=="inview", 1, 0)) %>%
  mut=te(glaucoma_false_pos_inview = ifelse((glaucoma_right==0 | gl=ucoma_left==0) & cdr>=0.7 & camera=="inview", 1, 0)) =>%
  mutate(glaucoma_true_neg_pictor = ifelse((glaucoma_right==3D0 | glaucoma_left==0) & cdr<0.7 & camera=="pictor", =, 0)) %>%
  mutate(glaucoma_false_pos_pictor = ifelse((glau=oma_right==0 | glaucoma_left==0) & cdr>=0.7 & camera==3D"pictor", 1, 0))

specificity_glaucoma_peek <- sum(alldata$gl=ucoma_true_neg_peek, na.rm=TRUE)/(sum(alldata$glaucoma_true_neg_peek, =a.rm=TRUE)+sum(alldata$glaucoma_false_pos_peek, na.rm=TRUE))
specif=city_glaucoma_inview <- sum(alldata$glaucoma_true_neg_inview, na.rm==RUE)/(sum(alldata$glaucoma_true_neg_inview, na.rm=TRUE)+sum(alldata$gla=coma_false_pos_inview, na.rm=TRUE))
specificity_glaucoma_pictor <- =um(alldata$glaucoma_true_neg_pictor, na.rm=TRUE)/(sum(alldata$glaucoma_=rue_neg_pictor, na.rm=TRUE)+sum(alldata$glaucoma_false_pos_pictor, na=rm=TRUE))

specificity_glaucoma_peek
specificity_glaucoma_inview
=pecificity_glaucoma_pictor

#specificity for otherdx
alldata <-=20alldata %>%
  mutate(otherdx_true_neg_peek = ifelse(((other=x_right==0 | otherdx_left==0) & (other_dx==0|other_dx==3D1)) & camera=="peek", 1, 0)) %>%
  mutate(otherdx_fal=e_pos_peek = ifelse(((otherdx_right==0 | otherdx_left==0)=20& (other_dx==2|other_dx==3)) & camera=="peek", 1, 0=) %>%  
  mutate(otherdx_true_neg_inview = ifelse(((other=x_right==0 | otherdx_left==0) & (other_dx==0|other_dx==3D1)) & camera=="inview", 1, 0)) %>%
  mutate(otherdx_f=lse_pos_inview = ifelse(((otherdx_right==0 | otherdx_left===) & (other_dx==2|other_dx==3)) & camera=="inview", 1,=200)) %>%
  mutate(otherdx_true_neg_pictor = ifelse(((otherdx=right==0 | otherdx_left==0) & (other_dx==0|other_dx===)) & camera=="pictor", 1, 0)) %>%
  mutate(otherdx_fals=_pos_pictor = ifelse(((otherdx_right==0 | otherdx_left==0= & (other_dx==2|other_dx==3)) & camera=="pictor", 1, =))

specificity_otherdx_peek <- sum(alldata$otherdx_true_neg_peek, =a.rm=TRUE)/(sum(alldata$otherdx_true_neg_peek, na.rm=TRUE)+sum(alldat=$otherdx_false_pos_peek, na.rm=TRUE))
specificity_otherdx_inview <-=20sum(alldata$otherdx_true_neg_inview, na.rm=TRUE)/(sum(alldata$otherdx=true_neg_inview, na.rm=TRUE)+sum(alldata$otherdx_false_pos_inview, na=rm=TRUE))
specificity_otherdx_pictor <- sum(alldata$otherdx_true_ne=_pictor, na.rm=TRUE)/(sum(alldata$otherdx_true_neg_pictor, na.rm=TR=E)+sum(alldata$otherdx_false_pos_pictor, na.rm=TRUE))

specificity_=therdx_peek
specificity_otherdx_inview
specificity_otherdx_pictor

=0A#########EPI TABLES#####

## epi dr peek
table_dr_peek <-=20as.table(matrix(c(sum(alldata$dr_true_pos_peek, na.rm=TRUE),sum(allda=a$dr_false_pos_peek, na.rm=TRUE),sum(alldata$dr_false_neg_peek, na.rm=3DTRUE),sum(alldata$dr_true_neg_peek, na.rm=TRUE)), nrow = 2, =yrow = TRUE))
colnames(table_dr_peek) <- c("Dis+","Dis-")
row=ames(table_dr_peek) <- c("Test+","Test-")
table_dr_peek

epi_dr_p=ek <- epi.tests(table_dr_peek, conf.level = 0.95)
epi_dr_peek=0A

## epi dr inview
table_dr_inview <- as.table(matrix(c(s=m(alldata$dr_true_pos_inview, na.rm=TRUE),sum(alldata$dr_false_pos_invi=w, na.rm=TRUE),sum(alldata$dr_false_neg_inview, na.rm=TRUE),sum(all=ata$dr_true_neg_inview, na.rm=TRUE)), nrow = 2, byrow = =RUE))
colnames(table_dr_inview) <- c("Dis+","Dis-")
rownames(table_=r_inview) <- c("Test+","Test-")
table_dr_inview

epi_dr_inview =- epi.tests(table_dr_inview, conf.level = 0.95)
epi_dr_inview
=0A
## epi dr pictor
table_dr_pictor <- as.table(matrix(c(sum(=lldata$dr_true_pos_pictor, na.rm=TRUE),sum(alldata$dr_false_pos_pictor,=20na.rm=TRUE),sum(alldata$dr_false_neg_pictor, na.rm=TRUE),sum(alldat=$dr_true_neg_pictor, na.rm=TRUE)), nrow = 2, byrow = TR=E))
colnames(table_dr_pictor) <- c("Dis+","Dis-")
rownames(table_dr=pictor) <- c("Test+","Test-")
table_dr_pictor

epi_dr_pictor <-=20epi.tests(table_dr_pictor, conf.level = 0.95)
epi_dr_pictor

=0A## epi dme peek
table_dme_peek <- as.table(matrix(c(sum(allda=a$dme_true_pos_peek, na.rm=TRUE),sum(alldata$dme_false_pos_peek, na.r==TRUE),sum(alldata$dme_false_neg_peek, na.rm=TRUE),sum(alldata$dme_tr=e_neg_peek, na.rm=TRUE)), nrow = 2, byrow = TRUE))
co=names(table_dme_peek) <- c("Dis+","Dis-")
rownames(table_dme_peek) =- c("Test+","Test-")
table_dme_peek

epi_dme_peek <- epi.tests(=able_dme_peek, conf.level = 0.95)
epi_dme_peek


## epi =me inview
table_dme_inview <- as.table(matrix(c(sum(alldata$dme_tru=_pos_inview, na.rm=TRUE),sum(alldata$dme_false_pos_inview, na.rm=TR=E),sum(alldata$dme_false_neg_inview, na.rm=TRUE),sum(alldata$dme_true_n=g_inview, na.rm=TRUE)), nrow = 2, byrow = TRUE))
coln=mes(table_dme_inview) <- c("Dis+","Dis-")
rownames(table_dme_inview) =- c("Test+","Test-")
table_dme_inview

epi_dme_inview <- epi.te=ts(table_dme_inview, conf.level = 0.95)
epi_dme_inview


##=20epi dme pictor
table_dme_pictor <- as.table(matrix(c(sum(alldat=$dme_true_pos_pictor, na.rm=TRUE),sum(alldata$dme_false_pos_pictor, n=.rm=TRUE),sum(alldata$dme_false_neg_pictor, na.rm=TRUE),sum(alldata$d=e_true_neg_pictor, na.rm=TRUE)), nrow = 2, byrow = TRUE=)
colnames(table_dme_pictor) <- c("Dis+","Dis-")
rownames(table_dme=pictor) <- c("Test+","Test-")
table_dme_pictor

epi_dme_pictor =- epi.tests(table_dme_pictor, conf.level = 0.95)
epi_dme_pictor=0A

## epi amd peek
table_amd_peek <- as.table(matrix(c(sum=alldata$amd_true_pos_peek, na.rm=TRUE),sum(alldata$amd_false_pos_peek, =a.rm=TRUE),sum(alldata$amd_false_neg_peek, na.rm=TRUE),sum(alldata$am=_true_neg_peek, na.rm=TRUE)), nrow = 2, byrow = TRUE))
=olnames(table_amd_peek) <- c("Dis+","Dis-")
rownames(table_amd_peek) =- c("Test+","Test-")
table_amd_peek

epi_amd_peek <- epi.tests(=able_amd_peek, conf.level = 0.95)
epi_amd_peek


## epi =md inview
table_amd_inview <- as.table(matrix(c(sum(alldata$amd_tru=_pos_inview, na.rm=TRUE),sum(alldata$amd_false_pos_inview, na.rm=TR=E),sum(alldata$amd_false_neg_inview, na.rm=TRUE),sum(alldata$amd_true_n=g_inview, na.rm=TRUE)), nrow = 2, byrow = TRUE))
coln=mes(table_amd_inview) <- c("Dis+","Dis-")
rownames(table_amd_inview) =- c("Test+","Test-")
table_amd_inview

epi_amd_inview <- epi.te=ts(table_amd_inview, conf.level = 0.95)
epi_amd_inview


##=20epi amd pictor
table_amd_pictor <- as.table(matrix(c(sum(alldat=$amd_true_pos_pictor, na.rm=TRUE),sum(alldata$amd_false_pos_pictor, n=.rm=TRUE),sum(alldata$amd_false_neg_pictor, na.rm=TRUE),sum(alldata$a=d_true_neg_pictor, na.rm=TRUE)), nrow = 2, byrow = TRUE=)
colnames(table_amd_pictor) <- c("Dis+","Dis-")
rownames(table_amd=pictor) <- c("Test+","Test-")
table_amd_pictor

epi_amd_pictor =- epi.tests(table_amd_pictor, conf.level = 0.95)
epi_amd_pictor=0A


## epi glaucoma peek
table_glaucoma_peek <- as.table=matrix(c(sum(alldata$glaucoma_true_pos_peek, na.rm=TRUE),sum(alldata$gl=ucoma_false_pos_peek, na.rm=TRUE),sum(alldata$glaucoma_false_neg_peek, =a.rm=TRUE),sum(alldata$glaucoma_true_neg_peek, na.rm=TRUE)), nrow =3D 2, byrow = TRUE))
colnames(table_glaucoma_peek) <- c("Di=+","Dis-")
rownames(table_glaucoma_peek) <- c("Test+","Test-")
tabl=_glaucoma_peek

epi_glaucoma_peek <- epi.tests(table_glaucoma_peek,=20conf.level = 0.95)
epi_glaucoma_peek


## epi glaucoma =nview
table_glaucoma_inview <- as.table(matrix(c(sum(alldata$glaucoma=true_pos_inview, na.rm=TRUE),sum(alldata$glaucoma_false_pos_inview, n=.rm=TRUE),sum(alldata$glaucoma_false_neg_inview, na.rm=TRUE),sum(alld=ta$glaucoma_true_neg_inview, na.rm=TRUE)), nrow = 2, byrow =3D TRUE))
colnames(table_glaucoma_inview) <- c("Dis+","Dis-")
row=ames(table_glaucoma_inview) <- c("Test+","Test-")
table_glaucoma_invi=w

epi_glaucoma_inview <- epi.tests(table_glaucoma_inview, conf.l=vel = 0.95)
epi_glaucoma_inview


## epi glaucoma picto=
table_glaucoma_pictor <- as.table(matrix(c(sum(alldata$glaucoma_true=pos_pictor, na.rm=TRUE),sum(alldata$glaucoma_false_pos_pictor, na.rm==RUE),sum(alldata$glaucoma_false_neg_pictor, na.rm=TRUE),sum(alldata$gla=coma_true_neg_pictor, na.rm=TRUE)), nrow = 2, byrow = T=UE))
colnames(table_glaucoma_pictor) <- c("Dis+","Dis-")
rownames(t=ble_glaucoma_pictor) <- c("Test+","Test-")
table_glaucoma_pictor

=pi_glaucoma_pictor <- epi.tests(table_glaucoma_pictor, conf.level ==200.95)
epi_glaucoma_pictor



## epi otherdx peek
table_=therdx_peek <- as.table(matrix(c(sum(alldata$otherdx_true_pos_peek, n=.rm=TRUE),sum(alldata$otherdx_false_pos_peek, na.rm=TRUE),sum(alldata=otherdx_false_neg_peek, na.rm=TRUE),sum(alldata$otherdx_true_neg_peek, =a.rm=TRUE)), nrow = 2, byrow = TRUE))
colnames(table_ot=erdx_peek) <- c("Dis+","Dis-")
rownames(table_otherdx_peek) <- c(=Test+","Test-")
table_otherdx_peek

epi_otherdx_peek <- epi.tests=table_otherdx_peek, conf.level = 0.95)
epi_otherdx_peek


#= epi otherdx inview
table_otherdx_inview <- as.table(matrix(c(s=m(alldata$otherdx_true_pos_inview, na.rm=TRUE),sum(alldata$otherdx_fals=_pos_inview, na.rm=TRUE),sum(alldata$otherdx_false_neg_inview, na.rm==RUE),sum(alldata$otherdx_true_neg_inview, na.rm=TRUE)), nrow = =, byrow = TRUE))
colnames(table_otherdx_inview) <- c("Dis+","=is-")
rownames(table_otherdx_inview) <- c("Test+","Test-")
table_ot=erdx_inview

epi_otherdx_inview <- epi.tests(table_otherdx_inview, =onf.level = 0.95)
epi_otherdx_inview


## epi otherdx p=ctor
table_otherdx_pictor <- as.table(matrix(c(sum(alldata$otherdx_tr=e_pos_pictor, na.rm=TRUE),sum(alldata$otherdx_false_pos_pictor, na.rm=3DTRUE),sum(alldata$otherdx_false_neg_pictor, na.rm=TRUE),sum(alldata$o=herdx_true_neg_pictor, na.rm=TRUE)), nrow = 2, byrow = =RUE))
colnames(table_otherdx_pictor) <- c("Dis+","Dis-")
rownames(t=ble_otherdx_pictor) <- c("Test+","Test-")
table_otherdx_pictor

e=i_otherdx_pictor <- epi.tests(table_otherdx_pictor, conf.level = =.95)
epi_otherdx_pictor
=
