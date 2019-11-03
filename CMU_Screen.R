library(readr)
library(tidyverse)
library(rlang)
library(skimr)
library(dplyr)
library(rsample)

costscreened_redcapexport <- read_csv("CostSCREENED_DATA_2019-11-02_2305.csv")

glimpse(costscreened_redcapexport)
## I use skim frequently, but rarely use glimpse. For my learning, what do like to use glimpse for specifically?
xtabs(data=costscreened_redcapexport , ~redcap_event_name+clinic_attended_v2, addNA=TRUE) 
xtabs(data=costscreened_redcapexport, ~clinic_attended_v2+consent, addNA=TRUE)
# I guess we only want screening visit
# Who to include? Consented? At clinic?
## We only want patients consented in one of the three clinics, no missing data here

costscreened <- costscreened_redcapexport %>%
  mutate(dmclinic=case_when(clinic_attended_v2==0 ~ 1,
                            clinic_attended_v2 %in% c(1,2) ~ 0,
                            TRUE ~ NA_real_),
         thyroidclinic=case_when(clinic_attended_v2 == 1 ~ 1,
                                 clinic_attended_v2 %in% c(0,2) ~ 0,
                                 TRUE ~ NA_real_),
         generalclinic=case_when(clinic_attended_v2 == 2 ~ 1,
                                 clinic_attended_v2 %in% c(0,1) ~ 0,
                                 TRUE ~ NA_real_),
         # I guess the above is fine but I think eventually we're going to just use the 3-level variable.
         # If you want to just be able to remember what each is, try this:
         clinic_attended_v2=recode_factor(clinic_attended_v2, '0'="Diabetes", '1'="Thyroid",'2'="General"))

yingdata <- costscreened %>%
  filter(redcap_event_name=="ying_data_entry_arm_1") %>%
  select_if(~sum(!is.na(.)) > 0) %>%
  select(-redcap_event_name)

jeremyreview <- costscreened %>%
  filter(redcap_event_name=="ophthalmologist_re_arm_1") %>%
  select_if(~sum(!is.na(.)) > 0) %>%
  select(-redcap_event_name)

screeningdata <- costscreened %>%
  filter(redcap_event_name=="screening_visit_arm_1") %>%
  select_if(~sum(!is.na(.)) > 0) %>%
  select(-redcap_event_name)

# randomization_model <- read_csv("randomization_model_results.csv")
# JK: I am not understanding why there is missing data. I wouldn't think there would be missing data.

## BMS: Jeremy, I was able to get the above csv directly from Redcap on the labeled data export. 
## I think more straightforward than the way you did below, however, as you'll see it produces the same output, 
## so either way works. I just included the below explanation as an alternative
## This download is from the labeled export from redcap
## There is no missing data. Not all paients were randomized. If you were screening positive you were not given an answer

# JK: Where did this randomization_model_results.csv file come from? Is it from Redcap? 
# You would have had to have merged it correct? I wonder if safer to just do it in the R code?
# This is the original file, I believe. But we should export from Redcap

library(readxl)

Random_10_percent_list_for_negatives_copy <- read_excel("Random 10 percent list for negatives copy.xlsx") %>%
   rename(refer_negative=number,
          referresultnegatives=refer) %>%
   select(refer_negative, referresultnegatives)
screeningdata2test <- left_join(screeningdata, Random_10_percent_list_for_negatives_copy, by=("refer_negative"))

## BMS: This should be the same as mine below. Let me check.

randomization_model <- read_csv("randomization_model_results.csv")

screeningdata2 <- full_join(randomization_model, screeningdata, by="study_id")

sd3 <- screeningdata2 %>%
  select(study_id, randomization_model) %>%
  arrange(study_id)
sd3test <- screeningdata2test %>%
 select(study_id, referresultnegatives) %>%
  arrange(study_id)

randomcompare <- full_join(sd3, sd3test, by="study_id")

View(randomcompare)

## BMS: Ok, this is the same. I proceeded with the repcap labeled export let me know if you feel differently.
## You just export directly from redcap as a labeled csv and join by column title. 

#Import randomization model 

View(screeningdata2)
skim(screeningdata)
skim(screeningdata2)

## BMS: iop (iop_left_eye_os, iop_right_eye_od) and travel_cost is wrongly labeled as a character 
## and not numeric here, need to correct 

screeningdata2$travel_cost <- as.numeric(screeningdata2$travel_cost)
screeningdata2$iop_left_eye_os <- as.numeric(screeningdata2$iop_left_eye_os)
##BMS - Is it ok that NAs were introduced by coercion?
screeningdata2$iop_right_eye_od <- as.numeric(screeningdata2$iop_right_eye_od)

## BMS: Reviewing the skim file, there are a couple areas of "missing" data, in particularm VCDR (several) and iop (1), 
## I double checked and these were all correctly identified and VCDR was unobtainable 
## At what stage do you remove these eyes from analysis? 

xtabs(data=screeningdata2, ~refer_positive+randomization_model, addNA=TRUE)
# JK: What is refer_positive? Check Redcap
## BMS: refer_positive is a box that Ying would check. I think better to 
## Proceed with definitions for referred to avoid data entry error (later in script)
## I found that her box-checking was not always exactly in line with defintions (as with the above)
addmargins(xtabs(data=screeningdata2, ~refer_negative+randomization_model, addNA=TRUE))
# JK: looks like this is right but should confirm...
## BMS: Yes, this is the randomization model

# We essentially want only 2 main data frames: a wide one and long one.
# I am only including the screeningdata and yingdata dataframes for now but we can add more before this point if needed
csdatawide <- full_join(screeningdata2, yingdata, by="study_id") %>%
  # In order to reshape to long, get all variables in common format, with ".re" or ".le" at the end...
  rename(visual_symptoms.re=visual_symptoms_re,
         sx_blurry.re=symptoms_right___0,
         sx_floater.re=symptoms_right___1,
         sx_flash.re=symptoms_right___2,
         sx_scotoma.re=symptoms_right___3,
         sx_other.re=symptoms_right___4,
         sx_specother.re=other_symptoms_right,
         visual_symptoms.le=visual_symptoms_le,
         sx_blurry.le=symptoms_left_v2___0,
         sx_floater.le=symptoms_left_v2___1,
         sx_flash.le=symptoms_left_v2___2,
         sx_scotoma.le=symptoms_left_v2___3,
         sx_other.le=symptoms_left_v2___4,
         sx_specother.le=other_symptoms_left_v2,
         va.re=va_re,
         vaph.re=va_re_ph,
         va.le=va_le,
         vaph.le=va_le_ph,
         visualacuity_referral.re=visual_acuity_referral_re,
         visualacuity_referral.le=visual_acuity_referral_le,
         vcdr_cnd.re=vcdr_cnd_re,
         vcdr.re=vcdr_re,
         vcdr_grader.re=vcdr_re_grader,
         vcdr_program.re=vcdr_re_program,
         vcdr_referral.re=vcdr_referral_re,
         vcdr_referral.re=vcdr_referral_re,
         vcdr_referralpro.re=vcdr_referral_re_pro,
         photo_abdisc.re=photo_ab_disc_re,
         referral_disc_abnormal.re=referral_disc_abnormal_re,
         disc_heme.re=disc_hemorage_re,
         disc_notch.re=notch_re,
         disc_other.re=disc_other_finding_re,
         dr.re=dr_re,
         dr_referral.re=dr_referral_re,
         amd.re=amd_re,
         amd_referral.re=amd_referral_re,
         other_referable.re=other_referable_finding_re,
         other_referral.re=other_referral_re,
         vcdr_cnd.le=vcdr_cnd_le,
         vcdr.le=vcdr_le,
         vcdr_grader.le=vcdr_le_grader,
         vcdr_program.le=vcdr_le_program,
         vcdr_referral.le=vcdr_referral_le,
         vcdr_referralpro.le=vcdr_referral_le_pro,
         photo_abdisc.le=photo_ab_disc_le,
         referral_disc_abnormal.le=referral_disc_abnormal_le,
         disc_heme.le=disc_hemorrage_le,
         disc_notch.le=disc_notch_le,
         disc_other.le=disc_other_finding_le,
         dr.le=dr_le,
         dr_referral.le=dr_ref_le,
         amd.le=amd_le,
         amd_referral.le=amd_ref_le,
         other_referable.le=other_referable_finding_le,
         other_referral.le=other_ref_le,
         iop.re=iop_right_eye_od,
         iop.le=iop_left_eye_os,
         iop_ref.re=iop_ref_re,
         iop_ref.le=iop_ref_le,
         eyeart_rded.re=eyeart_od_rded,
         eyeart_vtded.re=eyeart_od_vtded,
         eyeart_rded.le=eyeart_os_rded,
         eyeart_vtded.le=eyeart_os_vtded,
         eyeart_flag=flag_eyenuk,
         oe_vacc.re=oe_va_re_cc,
         oe_plusminus.re=plus_minus_va_re,
         oe_phva.re=oe_phva_re,
         oe_plusminus_ph.re=plus_minus_vaph_re,
         oe_iop.re=app_ot_mmhg_re,
         oe_lens_clear.re=lens_re___0,
         oe_lens_cataract.re=lens_re___1,
         oe_lens_pciol.re=lens_re___2,
         oe_lens_aphakia.re=lens_re___3,
         oe_lens_cd.re=lens_re___4,
         oe_vcdr.re=oe_vcdr_re,
         oe_discnotch.re=optic_disc_notch_re,
         oe_discheme.re=optic_disc_heme_re,
         oe_rnfldefect.re=optic_disc_rnfl_re,
         oe_dr.re=oe_dr_re,
         oe_npdr_mild.re=npdr_type_re___1,
         oe_npdr_mod.re=npdr_type_re___2,
         oe_npdr_sev.re=npdr_type_re___3,
         oe_amd_none.re=oe_amd_re___0,
         oe_amd_drusen.re=oe_amd_re___1,
         oe_amd_ga.re=oe_amd_re___2,
         oe_amd_wet.re=oe_amd_re___3,
         oe_amd_cd.re=oe_amd_re___4,
         oe_majorcause_nl.re=major_cause_re___0,
         oe_majorcause_cat.re=major_cause_re___1,
         oe_majorcause_ref.re=major_cause_re___2,
         oe_majorcause_glc.re=major_cause_re___3,
         oe_majorcause_amd.re=major_cause_re___4,
         oe_majorcause_dr.re=major_cause_re___5,
         oe_majorcause_other.re=major_cause_re___6,
         oe_majorcause_cd.re=major_cause_re___7,
         oe_majorcause_specother.re=other_diagnosis_re,
         oe_vacc.le=oe_va_le_cc,
         oe_plusminus.le=plus_minus_va_le,
         oe_phva.le=oe_phva_le,
         oe_plusminus_ph.le=plus_minus_vaph_le,
         oe_iop.le=app_ot_mmhg_le,
         oe_lens_clear.le=lens_le___0,
         oe_lens_cataract.le=lens_le___1,
         oe_lens_pciol.le=lens_le___2,
         oe_lens_aphakia.le=lens_le___3,
         oe_lens_cd.le=lens_le___4,
         oe_vcdr.le=oe_vcdr_le,
         oe_discnotch.le=optic_disc_notch_le,
         oe_discheme.le=optic_disc_heme_le,
         oe_rnfldefect.le=optic_disc_rnfl_le,
         oe_dr.le=oe_dr_le,
         oe_npdr_mild.le=npdr_type_le___1,
         oe_npdr_mod.le=npdr_type_le___2,
         oe_npdr_sev.le=npdr_type_le___3,
         oe_amd_none.le=oe_amd_le___0,
         oe_amd_drusen.le=oe_amd_le___1,
         oe_amd_ga.le=oe_amd_le___2,
         oe_amd_wet.le=oe_amd_le___3,
         oe_amd_cd.le=oe_amd_le___4,
         oe_majorcause_nl.le=major_cause_le___0,
         oe_majorcause_cat.le=major_cause_le___1,
         oe_majorcause_ref.le=major_cause_le___2,
         oe_majorcause_glc.le=major_cause_le___3,
         oe_majorcause_amd.le=major_cause_le___4,
         oe_majorcause_dr.le=major_cause_le___5,
         oe_majorcause_other.le=major_cause_le___6,
         oe_majorcause_cd.le=major_cause_le___7,
         oe_majorcause_specother.le=other_diagnosis_le,
         oe_tx_none=treatment___0,
         oe_tx_vftest=treatment___1,
         oe_tx_fu=treatment___2,
         oe_tx_catsurg=treatment___4,
         oe_tx_rectx=treatment___5,
         oe_tx_refer=treatment___6,
         oe_tx_other=treatment___3,
         oe_tx_otherspec=other_treatment,
         vf_fixation_loss.re=vf_re_fixation_loss,
         vf_false_pos.re=vf_re_false_pos,
         vf_false_neg.re=vf_re_false_neg,
         vf_vfi.re=vf_re_vfi,
         vf_md.re=vf_re_md,
         vf_md_pvalue.re=vf_re_md_pvalue,
         vf_psd.re=vf_re_psd,
         vf_psd_pvalue.re=vf_re_psd_pvalue,
         vf_progression.re=vf_re_progression,
         vf_fixation_loss.le=vf_le_fixation_loss,
         vf_false_pos.le=vf_le_false_pos,
         vf_false_neg.le=vf_le_false_neg,
         vf_vfi.le=vf_le_vfi,
         vf_md.le=vf_le_md,
         vf_md_pvalue.le=vf_le_md_pvalue,
         vf_psd.le=vf_le_psd,
         vf_psd_pvalue.le=vf_le_psd_pvalue,
         vf_progression.le=vf_le_progression,
         oe_glaucomadx.re=glaucomadx_re_oe,
         oe_glaucomadx.le=glaucomadx_le_oe,
         eyeart_oephoto_rded.re=eyeart_od_rded_oephoto,
         eyeart_oephoto_vtded.re=eyeart_od_vtded_oephoto,
         eyeart_oephoto_rded.le=eyeart_os_rded_oephoto,
         eyeart_oephoto_vtded.le=eyeart_os_vtded_oephoto,
         pecenscr_photo_quality.re=photo_quality_re_or_pecen,
         pecenscr_photo_field.re=photo_field_re_or_pecen,
         pecenscr_vcdr.re=vcdr_re_or_pecen,
         pecenscr_vcdr_referral.re=vcdr_referral_re_or_pecen,
         pecenscr_photo_ab_disc.re=photo_ab_disc_re_or_pecen,
         pecenscr_referral_disc_ab.re=referral_disc_ab_re_or_pecen,
         pecenscr_dr.re=dr_re_or_pecen,
         pecenscr_referral_dr.re=referral_dr_re_or_pecen,
         pecenscr_amd.re=amd_re_or_pecen,
         pecenscr_referral_amd.re=referral_amd_re_or_pecen,
         pecenscr_other_ref_find.re=other_ref_find_re_or_pecen,
         pecenscr_referral_other.re=referral_other_re_or_pecen,
         pecenscr_photo_quality.le=photo_quality_le_or_pecen,
         pecenscr_photo_field.le=photo_field_le_or_pecen,
         pecenscr_vcdr.le=vcdr_le_or_pecen,
         pecenscr_vcdr_referral.le=vcdr_referral_le_or_pecen,
         pecenscr_photo_ab_disc.le=photo_ab_disc_le_or_pecen,
         pecenscr_referral_disc_ab.le=referral_disc_ab_le_or_pecen,
         pecenscr_dr.le=dr_le_or_pecen,
         pecenscr_referral_dr.le=referral_dr_le_or_pecen,
         pecenscr_amd.le=amd_le_or_pecen,
         pecenscr_referral_amd.le=referral_amd_le_or_pecen,
         pecenscr_other_ref_find.le=other_ref_find_le_or_pecen,
         pecenscr_referral_other.le=other_ref_le_or_pecen,
         pecendil_vcdr_referral.re=vcdr_referral_re_or_pecen_v2,
         pecendil_referral_disc_ab.re=referral_disc_ab_re_or_pecen_v2,
         pecendil_referral_dr.re=referral_dr_re_or_pecen_v2,
         pecendil_referral_amd.re=referral_amd_re_or_pecen_v2,
         pecendil_referral_other.re=referral_other_re_or_pecen_v2,
         pecendil_vcdr_referral.le=vcdr_referral_le_or_pecen_v2,
         pecendil_referral_disc_ab.le=referral_disc_ab_le_or_pecen_v2,
         pecendil_referral_dr.le=referral_dr_le_or_pecen_v2,
         pecendil_referral_amd.le=referral_amd_le_or_pecen_v2,
         pecendil_referral_other.le=other_ref_le_or_pecen_v2) %>%
  mutate(iop.re=as.numeric(iop.re),
         iop.le=if_else(iop.le=="na", "", iop.le),
         iop.le=as.numeric(iop.le),
         travel_cost=as.numeric(travel_cost)) %>%
  # Now put all the re/le variables at the end so can reshape to long
  select(study_id:examiner_v2, end_time_ying_screen:examiner, dilated:eyeart_screen_outcome, eyeart_flag:oe_examiner, oe_tx_none:vf_testing_complete, vf_further_tx:eyeart_screen_outcome_oephoto, eyeart_result_dilate_opexam_complete,time_start_exam_or_pecen, this_form_refer_calc_pecen:pecen_review_screening_complete, this_form_refer_calc_pecen_v2:pecen_review_dilated_complete, everything()) # to find duplicate column names: select(csdata)



csdatalong <- csdatawide %>%
  gather(variable, value, visual_symptoms.re:pecendil_referral_other.le) %>%
  separate(variable, into=c("field", "eye"), sep="\\.") %>%  
  # Note you need the \\ before the period when separating at a period for some reason...
  spread(field, value, convert=TRUE) %>%
  mutate(oe_amd_missing=if_else(oe_amd_none==0 & oe_amd_none==0 & oe_amd_drusen==0 & oe_amd_ga==0 & oe_amd_wet==0 & oe_amd_cd==0, 1,0),
         oe_tx_missing=if_else(oe_tx_none==0 & oe_tx_vftest==0 & oe_tx_fu==0 & oe_tx_catsurg==0 & oe_tx_rectx==0 & oe_tx_refer==0 & oe_tx_other==0, 1,0),
         screenfail_va=if_else(va>=4, 0, 
                               if_else(is.na(vaph) & va<4, NA_real_, 
                                       if_else(vaph<4,1,0))), # xtabs(data=csdatalong, ~va+screenfail_va, addNA=TRUE)  # xtabs(data=csdatalong, ~va+screenfail_va, addNA=TRUE)
         screenfail_iop=if_else(is.na(iop), 0, 
                                if_else(iop>22, 1, 0)), # xtabs(data=csdatalong, ~iop+screenfail_iop, addNA=TRUE)
         screenfail_abnldisc=if_else(is.na(photo_abdisc), NA_real_, 
                                     if_else(photo_abdisc==1,1,0)), # xtabs(data=csdatalong, ~photo_abdisc+screenfail_abnldisc, addNA=TRUE)
         screenfail_abnldisccd=if_else(is.na(photo_abdisc), NA_real_, 
                                       if_else(photo_abdisc>=1,1,0)), # xtabs(data=csdatalong, ~photo_abdisc+screenfail_abnldisccd, addNA=TRUE)
         screenfail_vcdr=if_else(is.na(vcdr), 0, 
                                 if_else(vcdr>0.6,1,0)),
         screenfail_vcdrcd=if_else(is.na(vcdr), 1, 
                                   if_else((vcdr>0.6),1,0)),# xtabs(data=csdatalong, ~vcdr+screenfail_vcdr, addNA=TRUE)
         screenfail_programvcdr=if_else(is.na(vcdr_program), 0, 
                                        if_else(vcdr_program>0.6,1,0)), # xtabs(data=csdatalong, ~vcdr_program+screenfail_programvcdr, addNA=TRUE)
         screenfail_programvcdrcd=if_else(is.na(vcdr_program), 1, 
                                          if_else(vcdr_program>0.6,1,0)), # xtabs(data=csdatalong, ~vcdr_program+screenfail_programvcdr, addNA=TRUE)
         screenfail_dr=if_else(is.na(dr), NA_real_, 
                               if_else(dr==1,1,0)), # xtabs(data=csdatalong, ~dr+screenfail_dr, addNA=TRUE)
         screenfail_drcd=if_else(is.na(dr), NA_real_, 
                                 if_else(dr %in% c(1,2),1,0)), # xtabs(data=csdatalong, ~dr+screenfail_drcd, addNA=TRUE)
         screenfail_amd=if_else(is.na(amd), NA_real_, 
                                if_else(amd==1,1,0)), # xtabs(data=csdatalong, ~amd+screenfail_amd, addNA=TRUE)
         screenfail_amdcd=if_else(is.na(amd), NA_real_, 
                                  if_else(amd %in% c(1,2),1,0)), # xtabs(data=csdatalong, ~amd+screenfail_amdcd, addNA=TRUE)
         screenfail_other=if_else(is.na(other_referral), NA_real_, 
                                  if_else(other_referral==1,1,0)),
         screenfail_photopos=if_else(is.na(amd) & is.na(dr) & is.na(vcdr) & is.na(photo_abdisc) & is.na(other_referral), NA_real_, 
                                     if_else( (screenfail_amd==1 | screenfail_vcdr==1 | screenfail_dr==1 | other_referral==1 | photo_abdisc==1),1,0)),
         screenfail_photoposcd=if_else(is.na(amd) & is.na(dr) & is.na(vcdr) & is.na(photo_abdisc) & is.na(other_referral), NA_real_, 
                                       if_else( (screenfail_amdcd==1 | screenfail_vcdrcd==1 | screenfail_drcd==1 | screenfail_abnldisccd==1),1,0)),
         screenfail_any=if_else(screenfail_va==1 | screenfail_iop==1 | screenfail_abnldisc==1 | screenfail_vcdr==1 | screenfail_dr==1 | screenfail_amd==1 | screenfail_photopos==1 | screenfail_other==1 , 1, 0),
         screenfail_anycd=if_else(screenfail_va==1 | screenfail_iop==1 | screenfail_abnldisccd==1 | screenfail_vcdr==1 |  screenfail_dr==1 | screenfail_amd==1 | screenfail_other==1 | screenfail_amdcd==1 | screenfail_drcd==1 | screenfail_photopos==1 | screenfail_vcdrcd==1 |screenfail_abnldisccd ==1 | screenfail_photoposcd==1, 1, 0),
         # xtabs(data=csdatalong, ~amd+screenfail_any, addNA=TRUE)
         oe_anyamd=if_else(oe_amd_missing==1, NA_real_, 
                           if_else(oe_amd_drusen==1 | oe_amd_ga==1 | oe_amd_wet==1, 1, 0)), # xtabs(data=csdatalong, ~oe_anyamd+oe_amd_cd, addNA=TRUE)  # xtabs(data=csdatalong, ~oe_anyamd+oe_amd_drusen, addNA=TRUE)  # xtabs(data=csdatalong, ~oe_anyamd+oe_amd_ga, addNA=TRUE)  # xtabs(data=csdatalong, ~oe_anyamd+oe_amd_wet, addNA=TRUE)
         oe_anydr=if_else(is.na(oe_dr), NA_real_, 
                          if_else(oe_dr %in% c(1,2),1,0)),  # xtabs(data=csdatalong, ~oe_anydr, addNA=TRUE)
         oe_anyglcfalse=if_else(is.na(oe_iop), NA_real_, 
                                if_else(oe_glaucomadx %in% c(1),1,0)),
         oe_anyglc=if_else(oe_tx_missing==0,oe_anyglcfalse,0),
         oe_anyglcsuspect=if_else(is.na(oe_vcdr) & is.na(oe_iop), NA_real_, 
                                  if_else(oe_vcdr>0.6 | oe_iop>22,1,0)), 
         oe_anyamddrglc=if_else(is.na(oe_anyamd) & is.na(oe_anydr) & is.na(oe_anydr),NA_real_,
                                if_else(oe_anyamd==1 | oe_anydr==1 | oe_anyglc==1,1,0)),
         oe_tx_catsurgnotmiss =if_else(oe_tx_missing==1, NA_real_, 
                                       if_else(oe_tx_catsurg==1, 1, 0)),
         oe_tx_vftestnotmiss =if_else(oe_tx_missing==1, NA_real_, 
                                      if_else(oe_tx_vftest==1, 1, 0)),
         oe_tx_rectxnotmiss =if_else(oe_tx_missing==1, NA_real_, 
                                     if_else(oe_tx_rectx==1, 1, 0)),
         sumoedx=oe_anyamd+oe_anydr+oe_anyglc+oe_anyglcsuspect+oe_anyamddrglc+oe_tx_catsurgnotmiss+oe_tx_vftestnotmiss+oe_tx_rectxnotmiss,
         clinic_attended_v2=recode_factor(clinic_attended_v2, '0'="Diabetes", '1'="Thyroid",'2'="General"))    

addmargins(xtabs(data=filter(csdatalong, consent==1), ~ screenfail_anycd + refer_patient_randomizatio, addNA=TRUE))

#####################
# Data Cleaning     #
#####################

##Find missing and incorrect outlier data
# 
# # 
# csts1 <- csdatalong %>%
#     filter(vcdr > 1 | vcdr < 0.1)
# 
# csts2 <- csdatalong %>%
#     filter((is.na(vcdr) & vcdr_cnd == "1"))
# # 
# csts3 <- csdatalong %>%
#      filter((is.na(amd) | is.na(dr)))
# 
# csts4 <- csscreeningexamcomplete %>%
#   filter(iop < 3 | iop > 40)

# View(csts#)

# csscreenmissingany <- csscreeningexamcomplete %>%
#    filter((is.na(amd) | is.na(dr)))

# csscreeningexamcomplete <- csdatalong %>%
#   filter(screening_exam_ying_complete == 2)
#  csgsexamcomplete <- csdatalong %>%
#      filter(ophthalmologist_exam_complete == 2)
# #  
# csgsts1 <- csgsexamcomplete %>%
#     filter(oe_vcdr > 1 | oe_vcdr < 0.1)
#  
# csgst2 <- csgsexamcomplete %>%
#     filter(oe_iop < 3 | oe_iop > 40)
# 
# csgsts3 <- csgsexamcomplete %>%
#       filter((is.na(oe_anyamd) | is.na(oe_anydr)))
# 
# csgsts4 <- csgsexamcomplete %>%
#       filter(!is.na(oe_anyamd) | !is.na(oe_anydr) | !is.na(oe_anyglc) | !is.na(oe_anyglcsuspect) | !is.na(oe_anyamddrglc)) %>%
#       select(study_id, oe_examiner:ophthalmologist_exam_complete, oe_amd_missing:sumoedx)
# 
# csgsts5 <- csgsexamcomplete %>%
#     filter(is.na(oe_vcdr))
# 

###### BMS Question?

# # These cases are true missing. (ie. cannot determine, for example no eye)
# #? How should we define these for analysis?

#	3371230	NA - White cataract - No VCDR on screening or GS exam
#	3654437	NA - LxT Old Chorio - Retinal - CND VCDR on screening or GS exam
#	3744448	NA - Optic atrophy  s/p brain surgery - CND VCDR OS
#	3867000	NA - White cataract - No VCDR on screening or GS exam
#	3867000	NA - White cataract - No VCDR on screening or GS exam
#	3867078	NA - Eye Prosthesis OS

# csgsts6 <- csgsexamcomplete %>%
#     filter(is.na(oe_iop))

#	3867078	NA - Eye Prosthesis OS

# This 1 case are true missing. 3867078 (ie. cannot determine, for example no eye)
# Need to remove patient le from analysis: 3867078, because prosthetic eye

#   missing.screening.data <- csdatalong %>%
#     filter(is.na(screenfail_any) & consent==1) %>%
#      select(study_id, eye, screenfail_va, screenfail_iop, screenfail_abnldisc, screenfail_vcdr, screenfail_dr, screenfail_amd)
# View(missing.screening.data)
#   
# 
# missingglcdx.data <- csdatalong %>%
# filter(is.na(oe_anyglc) & !is.na(oe_anyglcsuspect))
# 
#   #Double check 
# xtabs(data=csdatalong, ~vcdr+screenfail_vcdr, addNA=TRUE)
# xtabs(data=csdatalong, ~oe_vcdr+oe_anyglc, addNA=TRUE)
# xtabs(data=csdatalong, ~iop, addNA=TRUE)
# xtabs(data=csdatalong, ~oe_iop, addNA=TRUE)
# 
# #Correct AMD, not mutually exclusive
# csdatalong %>% skim(c("oe_amd_none", "oe_amd_cd", "oe_amd_drusen", "oe_amd_ga", "oe_amd_wet"))
# amdexploredata <- csdatalong %>%
  # mutate(oe_amdsum=oe_amd_cd+oe_amd_drusen+oe_amd_ga+oe_amd_wet)
# xtabs(data=amdexploredata, ~oe_amdsum+oe_amd_none, addNA=TRUE)
# xtabs(data=filter(amdexploredata, oe_amdsum>1 | (oe_amdsum>=1 & oe_amd_none==1)), ~study_id+eye, addNA=TRUE)

## Both 2254364 and 3059079 have both Drusen and GA checked. This is correct.
-----------
  ###########################
#Data Cleaning Seems to be relatively complete besides redefining/removing cannot determines
##resolved all above missing, remaing are true ie. missing eye etc. 

#? Any other recs/advise for data cleaning that I may have overlooked?

## Check referral to referred to definitions 

# #By eye:
# addmargins(xtabs(data=filter(csdatalong, consent==1), ~ screenfail_anycd + refer_patient_randomizatio, addNA=TRUE))
# #this is by eye not person, not eye, so incorrect
# #By Person:

csdatalongreferralcheck <- csdatalong %>%
  group_by(study_id) %>% # So this is seeing whether the entire patient was referred
  mutate(maxanyfail=max(screenfail_anycd)) %>%
  filter(maxanyfail!=randomization_model) %>%
  select(consent, maxanyfail, randomization_model, iop, starts_with("screenfail"))

## BMS: I keep getting the same error message: Adding missing grouping variables: 
##`study_id` Error in FUN(left) : invalid argument to unary operator. Unsure how to correct this. 
## Did some trouble shooting to no avail

shouldcompletegs <- csdatalongreferralcheck %>%
  filter(consent==1)
filter(maxanyfail== 1 |  randomization_model == "Refer")  

accountedfor <- csdatalongreferralcheck %>%
  filter(ophthalmologist_exam_complete ==2 | phone_call_complete==2)

#Cleaner

csdatalongreferralcheck2 <- csdatalong %>%
  group_by(study_id) %>%
  mutate(maxanyfail=max(screenfail_anycd, na.rm=TRUE)) %>%
  mutate(shouldcompletegs=ifelse((maxanyfail== 1 | randomization_model == "Refer"), 1, 0),
         accountedfor=ifelse((ophthalmologist_exam_complete ==2 | phone_call_complete==2), 1, 0))

xtabs(data=csdatalongreferralcheck2, ~accountedfor+shouldcompletegs, addNA=TRUE)

## No patients should be accounted for, but are not, that is good, but... 
## Need to investigste NAs, and those with: accountedfor == 1 & shouldcompletegs ==0

tsgsextra <- csdatalongreferralcheck2 %>%
  filter(accountedfor == 1 & shouldcompletegs ==0)

## Jeremy, let me know your thoughts on the below:

# 6 Had Ophthalmologist Exam, no referral or positive screen:
# 0610859
# 2205338
# 2519102
# 2686698
# 2856213
# 3691739

## Should we just not include them since they should have never techinally got the exam in the first place?

#Others had phone call listed, but no referal or exam. No problems here. 

tsgsna <- csdatalongreferralcheck2 %>%
  filter(is.na(shouldcompletegs)) 
# 5 Were Screen Negative, never seen. Never randomized. Should we just randomize now and if any end up being Refer calling them lost to follow up? 
# 0309840	
# 1897808
# 2162734
# 2361064
# 2812307

View(tsgsextra)
View(tsgsna)

## Sorry, this is messy. I didn't fully get that this error with Ying had occured until cleaning. 

xtabs(data=csdatalongreferralcheck2, ~accountedfor+shouldcompletegs, addNA=TRUE)

# I saw that 0916205 had screenfail_iop==1 even though the IOP was 8. That meant there was something wrong with the screenfail_iop variable
# The first thing to do then was to check the class, and I saw it was a character variable
# I want you to be able to troubleshoot like this. 
# I know it might not be super interesting but you have to make sure the data is clean before starting the analysis
# The data management like this is 90% of the work. The analysis is the easy part.
# You may be thinking that you'll just hire someone to clean the data for you. 
# But if you don't think about things like this you could have major data issues and have no idea.
# I'm not sure how else to help you do it besides walk you through my thought process
# But for example, you should check every single new variable to make sure it is doing what you want.
# Had we done that, we might have done the following, which would have made it also obvious:
  
## BMS: Ok, this makes sense. Thank you for walking me through step by step. I caught the IOP issue in skim earlier,
## However, I undersatand how this is 100% essential to the process and I should have caught it sooner
## Thank you for your help in continuing to assist me in understanding this
  
xtabs(data=csdatalong, ~iop+screenfail_iop,addNA=TRUE) # I changed code, assuming that we didn't refer if missing IOP
# Finishing for the rest of them...
# The reason this is important is because we eventually want to see whether referring based on a specific test was sens/spec for diagnosing a specific disease
# For example we wouldn't necessarily expect IOP to be a great test for DR/AMD.
addmargins(xtabs(data=csdatalong, ~va+screenfail_va,addNA=TRUE))
# This shows that no one with presenting VA of 4 or 5 was referred (correct)
# THe addmargins is nice because easier to add up 201+15+55+80=351
# If vision <4, then got pinhole...
addmargins(xtabs(data=filter(csdatalong, va<4), ~vaph+screenfail_va,addNA=TRUE))
# Note 351 total, so this accounting for everyone it should.
# Here it's cleaner (the 0/1/2/3 are referred, and the 4/5 are not)
xtabs(data=csdatalong, ~photo_abdisc+screenfail_abnldisc,addNA=TRUE)
xtabs(data=csdatalong, ~photo_abdisc+screenfail_abnldisccd,addNA=TRUE)
xtabs(data=csdatalong, ~vcdr+screenfail_vcdr,addNA=TRUE)
# For this one, I don't think we want missing values. So I will change the above code to fix this.
xtabs(data=csdatalong, ~vcdr+screenfail_vcdrcd,addNA=TRUE)
xtabs(data=csdatalong, ~vcdr_program+screenfail_programvcdr,addNA=TRUE)
# Hard to see because so many values, try this instead:
csdatalong %>% group_by(screenfail_programvcdr) %>% skim(vcdr_program)
# Again note that there are a bunch of negatives, but I am not sure we want that. I think we didn't refer if CD, right?
csdatalong %>% group_by(screenfail_programvcdrcd) %>% skim(vcdr_program)
xtabs(data=csdatalong, ~dr+screenfail_dr,addNA=TRUE)
xtabs(data=csdatalong, ~dr+screenfail_drcd,addNA=TRUE)
xtabs(data=csdatalong, ~amd+screenfail_amd,addNA=TRUE)
xtabs(data=csdatalong, ~amd+screenfail_amdcd,addNA=TRUE)
xtabs(data=csdatalong, ~other_referral+screenfail_other,addNA=TRUE)
# Noticed that there were no "2"'s for other_referral so deleted screenfail_othercd variable above
# Do you want to check the rest of the variables we made to make sure everything worked?

addmargins(xtabs(data=filter(csdatalongreferralcheck, consent==1), ~ maxanyfail + refer_patient_randomizatio, addNA=TRUE))
#why is the above different then the below??? # because the above is based on person and the below is based on eye ie. some people had one positive eye and one negative eye
addmargins(xtabs(data=filter(csdatalongreferralcheck, consent==1), ~ screenfail_anycd + refer_patient_randomizatio, addNA=TRUE))

##########
# Kappas for agreement to OpReview 
###
library(irr)
install.packages(lpSolve)
library(lpSolve)
library(lubridate)

#View(jeremyreview)

# ?separate
# ?ymd_hms
# ?kappa2

# to my understanding we should do this on the person level, not eye level.
# What is the agreement of a positive referral, correct? 

photoagreement <- full_join(csdatawide, jeremyreview, by="study_id") %>%
   mutate(photoscreenpos=case_when((vcdr_referral.re== 1 | dr_referral.re==1 | amd_referral.re==1 | other_referral.re==1 | vcdr_referral.le==1 | referral_disc_abnormal.le==1 | dr_referral.le==1 | amd_referral.le==1 | other_referral.le==1) ~ 1,
                                   (vcdr_referral.re== 0 & dr_referral.re==0 & amd_referral.re==0 & other_referral.re==0 & vcdr_referral.le==0 & referral_disc_abnormal.le==0 & dr_referral.le==0 & amd_referral.le==0 & other_referral.le==0) ~ 0,
                                   TRUE ~ NA_real_)) %>%
  select(start_time_inclusion, photoscreenpos, this_form_referral) %>%
   ymd_hms(start_time_inclusion)
# 
# #? unsure why this is lubridate function is not working. 
# #I am trying to separate out the date to give month by month, but will leave out for now
# 
 photoagreement <- full_join(csdatawide, jeremyreview, by="study_id") %>%
   mutate(photoscreenpos=case_when((vcdr_referral.re== 1 | dr_referral.re==1 | amd_referral.re==1 | other_referral.re==1 | vcdr_referral.le==1 | referral_disc_abnormal.le==1 | dr_referral.le==1 | amd_referral.le==1 | other_referral.le==1) ~ 1,
                                   (vcdr_referral.re== 0 & dr_referral.re==0 & amd_referral.re==0 & other_referral.re==0 & vcdr_referral.le==0 & referral_disc_abnormal.le==0 & dr_referral.le==0 & amd_referral.le==0 & other_referral.le==0) ~ 0,
                                   TRUE ~ NA_real_)) %>%
   select(photoscreenpos, this_form_referral) %>%
   filter(!is.na(this_form_referral))
#     
 View(photoagreement)
# 
photoagreement2 <- full_join(csdatawide, jeremyreview, by="study_id") %>%
   select(refer_patient_screen_cal, this_form_referral) %>%
   filter(!is.na(this_form_referral))
# 
 kappa2(photoagreement, weight = "unweighted", sort.levels = FALSE)
 kappa2(photoagreement2, weight = "unweighted", sort.levels = FALSE)
# 
# # Overall poor agreement : photo referral
# #   Subjects = 95 
# # Raters = 2 
# # Kappa = 0.432 
# # 
# # z = 4.22 
# # p-value = 2.46e-05 
# 
# # For any referral:
# # Subjects = 95 
# # Raters = 2 
# # Kappa = 0.432 
# # 
# # z = 4.22 
# # p-value = 2.46e-05 
# 
# #maybe sensitivity will be better 
# 
# #install.packages("caret")
# library(caret)
# library(lattice)
# 
# gold <- (photoagreement$photoscreenpos)
# screen <- (photoagreement$this_form_referral)
# 
# addmargins(xtabs(data=photoagreement, ~photoscreenpos+this_form_referral, addNA=TRUE))
# # 28/45 = .62 

####################
# Tables & Figures # 
#################### 
 
## Figure 1: Flow diagram

          # Patients Screened 
# Eyes Screened # Missing Eyes for what reason
# 
# Screen positive Eyes - from how many people - refer to table for reason referred
# Screen Negative Eyes - how many referred for screening (random)
# 
# Total for FU exam 
# - How many eyes positive
# - How manyeyes negative 
# - How many lost to FU

install.packages("DiagrammeR")
library(DiagrammeR)

test1 <- grViz("digraph flowchart {
      # node definitions with substituted label text
      node [fontname = Helvetica, shape = rectangle]        
      tab1 [label = '@@1']
      tab2 [label = '@@2']
      tab3 [label = '@@3']
      tab4 [label = '@@4']
      tab5 [label = '@@5']
      tab6 [label = '@@6']
      tab7 [label = '@@7']
      tab8 [label = '@@8']
      tab9 [label = '@@9']
      tab10 [label = '@@10']
      tab11 [label = '@@11']
      tab12 [label = '@@12']
      tab13 [label = '@@13']
      tab14 [label = '@@14']

      # edge definitions with the node IDs
      tab1 -> tab2;
      tab1 -> tab3;
      tab2 -> tab4 -> tab5
      tab2 -> tab6;
      tab6 -> tab7 -> tab5;
      tab6 -> tab8;
      tab5 -> tab9;
      tab5 -> tab10;
      tab9 -> tab11 -> tab12;
      tab9 -> tab13 -> tab14;
      }

      [1]: 'Participants consented and screened n=XXX'
      [2]: 'Participant Eyes consented and screened n=XXX'
      [3]: 'Eyes excluded and not screened due to innability to screen (ie Prosthetic Eye) n=XXX'
      [4]: 'Participant Eyes with Positive Screen n=XXX eyes, n= XXX participants (table 3 for positive screen distribution)'
      [5]: 'Participant Eyes Sent for Ophthalmologist exam ie. with Positive Screen n=XXX eyes, n= XXX participants OR Random Negative Referral n=XXX eyes, n= XXX participants'
      [6]: 'Participant Eyes with All Negative Screen n=XXX, n= XXX participants'
      [7]: 'Participant Eyes with All Negative Screen, AND Random Referral n=XXX eyes, n= XXX participants)'
      [8]: 'Participant Eyes with All Negative Screen, NO Random Referral n=XXX eyes, n= XXX participants'
      [9]: 'Participants & Eyes with Ophthalmologist Exam, n=XXX eyes, n= XXX participants'
      [10]: 'Participants & Eyes with Loss of Follow Up, n=XXX eyes, n= XXX participants'
      [11]: 'Screen Positive Participants with Exam, n=XXX eyes, n= XXX participants'
      [12]: 'Screen Positive Participants with Positive Exam, n=XXX eyes, n= XXX participants'
      [13]: 'Random Referral Participants with Exam, n=XXX eyes, n= XXX participants'
      [14]: 'Screen Positive Participants with Positive Exam, n=XXX eyes, n= XXX participants'
      ")
test1

## BMS: I want this to look cleaner. Any Recs? Do you use this or another package. 
## Are these the does/ connections that you would make?

#Table 2: demographics by clinic
# JK: I think better that we use the original data to do this, and then in the code filter things as needed
# Because when you create lots of different objects, it can get confusing to figure out when numbers don't match up
# And we're more confident in numbers if they always come from the same basic data. (in this case, csdatawide/csdatalong)

demographicstable <- csdatawide %>%
  filter(consent == 1) %>%
  group_by(clinic_attended_v2) %>%
  summarize(consentyes_num=sum(consent==1),
            consentyes_total=sum(!is.na(consent)),
            consentyes_per=(consentyes_num/consentyes_total),
            age_p50=quantile(age, 2/4),
            age_p25=quantile(age, 1/4),
            age_p75=quantile(age, 3/4),
            age_total=sum(!is.na(age)),
            traveltime_p50=quantile(travel_time, 2/4, na.rm=TRUE),
            traveltime_p25=quantile(travel_time, 1/4),
            traveltime_p75=quantile(travel_time, 3/4),
            traveltime_total=sum(!is.na(travel_time)), 
            yearswdm_p50=quantile(years_with_diabetes, 2/4, na.rm=TRUE),
            yearswdm_p25=quantile(years_with_diabetes, 1/4, na.rm=TRUE),
            yearswdm_p75=quantile(years_with_diabetes, 3/4, na.rm=TRUE),
            yearswdm_total=sum(!is.na(years_with_diabetes)),
            travelcost_p50=quantile(travel_cost, 2/4),
            travelcost_p25=quantile(travel_cost, 1/4),
            travelcost_p75=quantile(travel_cost, 3/4),
            travelcost_total=sum(!is.na(travel_cost)),
            female_num=sum(sex1==1),
            female_total=sum(!is.na(sex1)),
            female_per=female_num/female_total,
            dm_num=sum(diagnosis_of_diabetes==1),
            dm_total=sum(!is.na(diagnosis_of_diabetes)),
            dm_per=dm_num/dm_total)

----
  #Other way, just for documentation should be a less smooth version of the above script
demographics <- costscreened %>%
  filter(consent == 1 & clinic_attended_v2 %in% c(0,1,2)) %>%
  select(study_id, dmclinic, thyroidclinic, generalclinic, age, sex1, clinic_attended_v2, diagnosis_of_diabetes, years_with_diabetes, travel_time, travel_cost)

dmdem <- demographics %>%
  filter(clinic_attended_v2==0)
thyroiddem <- demographics %>%
  filter(clinic_attended_v2==1)
gendem <- demographics %>%
  filter(clinic_attended_v2==2)

summary(dmdem)
summary(thyroiddem)
summary(gendem)
summary(demographics)

demographics$travel_cost <- as.numeric(demographics$travel_cost)

skim(dmdem)
skim(gmclinic)

# JK: The following makes it easier to read, and could do write_csv and copy/paste in if you wanted.
demographicstablelong <- demographicstable %>%
  gather(field, value, consentyes_num:dm_per) %>%
  separate(field, into=c("variable","stat"), sep="_") %>%
  mutate(clinicstat=paste(clinic_attended_v2, stat, sep="_")) %>%
  select(-clinic_attended_v2, -stat) %>%
  spread(clinicstat, value)
# Note: missing duration of DM for one person in the General clinic:
csdatawide %>% filter(diagnosis_of_diabetes==1 & is.na(years_with_diabetes)) %>% select(study_id)
## Spoke with Ying, 6 years, updated in Redcaop


# Table 3
# NEED TO INVESTIGATE THE MISSING VALUES!!
table3 <- csdatalong %>%
  filter(consent==1) %>%
  group_by(clinic_attended_v2) %>%
  summarize(vafail_num=sum(screenfail_va==1, na.rm=TRUE),
            vafail_total=sum(!is.na(screenfail_va)),
            vafail_per=vafail_num/vafail_total,
            iopfail_num=sum(screenfail_iop==1, na.rm=TRUE),
            iopfail_total=sum(!is.na(screenfail_iop)),
            iopfail_per=iopfail_num/iopfail_total,
            abnldiscfail_num=sum(screenfail_abnldisc==1, na.rm=TRUE),
            abnldiscfail_total=sum(!is.na(screenfail_abnldisc)),
            abnldiscfail_per=abnldiscfail_num/abnldiscfail_total,
            vcdrfail_num=sum(screenfail_vcdr==1, na.rm=TRUE),
            vcdrfail_total=sum(!is.na(screenfail_vcdr)),
            vcdrfail_per=vcdrfail_num/vcdrfail_total,
            amdfail_num=sum(screenfail_amd==1, na.rm=TRUE),
            amdfail_total=sum(!is.na(screenfail_amd)),
            amdfail_per=amdfail_num/amdfail_total,
            drfail_num=sum(screenfail_dr==1, na.rm=TRUE),
            drfail_total=sum(!is.na(screenfail_dr)),
            drfail_per=drfail_num/drfail_total,
            photofail_num=sum(screenfail_photopos==1, na.rm=TRUE),
            photofail_total=sum(!is.na(screenfail_photopos)),
            photofail_per=photofail_num/photofail_total,
            anyfail_num=sum(screenfail_any==1, na.rm=TRUE),
            anyfail_total=sum(!is.na(screenfail_any)),
            anyfail_per=anyfail_num/anyfail_total) %>%
  gather(field, value, vafail_num:anyfail_per) %>%
  separate(field, into=c("variable","stat"), sep="_") %>%
  mutate(clinicstat=paste(clinic_attended_v2, stat, sep="_")) %>%
  select(-clinic_attended_v2, -stat) %>%
  spread(clinicstat, value)

## JK STOPPED HERE...

table4 <- csdatalong %>%
  filter(consent==1) %>%
  group_by(clinic_attended_v2) %>%
  

#Table 4 line by line (no longer using this)
# addmargins(xtabs(data=filter(csdatalong, consent==1), ~ oe_anyamd + dmclinic, addNA = TRUE))
# addmargins(xtabs(data=filter(csdatalong, consent==1), ~ oe_anydr + dmclinic, addNA = TRUE))
# addmargins(xtabs(data=filter(csdatalong, consent==1), ~ oe_anyglc + dmclinic, addNA = TRUE))
# addmargins(xtabs(data=filter(csdatalong, consent==1), ~ oe_anyglcsuspect + dmclinic, addNA = TRUE))
# addmargins(xtabs(data=filter(csdatalong, consent==1), ~ oe_anyamddrglc + dmclinic, addNA = TRUE))
# addmargins(xtabs(data=filter(csdatalong, consent==1), ~ oe_tx_vftestnotmiss  + dmclinic, addNA = TRUE))
# addmargins(xtabs(data=filter(csdatalong, consent==1), ~ oe_tx_catsurgnotmiss  + dmclinic, addNA = TRUE))
# addmargins(xtabs(data=filter(csdatalong, consent==1), ~ oe_tx_rectxnotmiss + dmclinic, addNA = TRUE))

##table 4 wide
table4wide <- csdatalong %>%
  filter(consent==1) %>%
  group_by(clinic_attended_v2, screenfail_anycd) %>%
  summarize(consent_n=sum(!is.na(consent)),
            oe_anyamd.mean=mean(oe_anyamd, na.rm=TRUE), 
            oe_anyamd.num=sum(oe_anyamd == 1, na.rm=TRUE),
            oe_anyamd.denom=sum(!is.na(oe_anyamd), na.rm=TRUE),
            oe_anydr.mean=mean(oe_anydr, na.rm=TRUE), 
            oe_anydr.num=sum(oe_anydr == 1, na.rm=TRUE),
            oe_anydr.denom=sum(!is.na(oe_anydr), na.rm=TRUE),
            oe_anyglc.mean=mean(oe_anyglc, na.rm=TRUE), 
            oe_anyglc.num=sum(oe_anyglc == 1, na.rm=TRUE),
            oe_anyglc.denom=sum(!is.na(oe_anyglc), na.rm=TRUE),
            oe_anyglcsuspect.mean=mean(oe_anyglcsuspect, na.rm=TRUE), 
            oe_anyglcsuspect.num=sum(oe_anyglcsuspect == 1, na.rm=TRUE),
            oe_anyglcsuspect.denom=sum(!is.na(oe_anyglcsuspect), na.rm=TRUE),
            oe_anyamddrglc.mean=mean(oe_anyamddrglc, na.rm=TRUE), 
            oe_anyamddrglc.num=sum(oe_anyamddrglc == 1, na.rm=TRUE),
            oe_anyamddrglc.denom=sum(!is.na(oe_anyamddrglc), na.rm=TRUE),
            oe_tx_vftestnotmiss.mean=mean(oe_tx_vftestnotmiss, na.rm=TRUE), 
            oe_tx_vftestnotmiss.num=sum(oe_tx_vftestnotmiss == 1, na.rm=TRUE),
            oe_tx_vftestnotmiss.denom=sum(!is.na(oe_tx_vftestnotmiss), na.rm=TRUE),
            oe_tx_catsurgnotmiss.mean=mean(oe_tx_catsurgnotmiss, na.rm=TRUE), 
            oe_tx_catsurgnotmiss.num=sum(oe_tx_catsurgnotmiss == 1, na.rm=TRUE),
            oe_tx_catsurgnotmiss.denom=sum(!is.na(oe_tx_catsurgnotmiss), na.rm=TRUE),
            oe_tx_rectxnotmiss.mean=mean(oe_tx_rectxnotmiss, na.rm=TRUE), 
            oe_tx_rectxnotmiss.num=sum(oe_tx_rectxnotmiss == 1, na.rm=TRUE),
            oe_tx_rectxnotmiss.denom=sum(!is.na(oe_tx_rectxnotmiss), na.rm=TRUE))

## I troubleshooted the denom diffferences for glc suspect and anyglc. 
## While I understand glcsuspect, True missing values for VCDR like white cataract messing with our R definition
## I do not understand the issue with anyglc being so large, can you assist? Thank you

tsscreenfail <- csdatalong %>%
  filter(is.na(screenfail_anycd))

addmargins(xtabs(data=filter(csdatalong, consent==1), ~ screenfail_anycd, addNA=TRUE))

skim(csdatalong)

tsscreenfail <- csdatalong %>%
  filter(is.na(screenfail_anycd))

View(tsscreenfail)

table4long <- table4wide %>%
  select(-(consent_n)) %>%
  gather(field, value, oe_anyamd.mean:oe_tx_rectxnotmiss.denom) %>%
  separate(field, into = c("diagnosis", "stat"), sep= "\\.") %>%
  mutate(dmclinic_stat = paste(dmclinic , stat, sep = "." ))%>%
  select(-dmclinic, -stat) %>%
  spread(dmclinic_stat, value) %>%
  mutate(diagnosis=case_when(diagnosis=="oe_anyamd" ~ "1oe_anyamd", 
                             diagnosis=="oe_anydr" ~ "2oe_anydr",
                             diagnosis=="oe_tx_rectxnotmiss" ~ "3oe_tx_rectx",
                             diagnosis=="oe_anyglcsuspect" ~ "4oe_anyglcsuspect",
                             diagnosis=="oe_tx_vftestnotmiss" ~ "5oe_tx_vftest",
                             diagnosis=="oe_anyglc" ~ "6oe_anyglc",
                             diagnosis=="oe_anyamddrglc" ~ "7oe_anyamddrglc",
                             diagnosis=="oe_tx_catsurgnotmiss" ~ "8oe_tx_catsurg",
                             TRUE ~ NA_character_)) %>%
  arrange(diagnosis)%>%
  select("diagnosis", "0.num", "0.denom", "0.mean", "1.num", "1.denom", "1.mean")

View(table4long)

## Table 5 & 6:
# https://stats.stackexchange.com/questions/7513/how-to-use-weights-in-function-lm-in-r

# x <-c(rnorm(10),NA)
# df <- data.frame(y= 10x+rnorm, x=x, wght1=1:10)


## BMS: Or would we use the below?
# ## Fancy weights as numeric vector
# summary(lm(y~x,data=df,weights=(df$wght1)^(3/4))) 
# 
# # Fancy weights as formula on column of the data set
# summary(lm(y~x,data=df,weights=I(wght1^(3/4))))
# 
# # Mundane weights as the column of the data set
# summary(lm(y~x,data=df,weights=wght1))


## DATA CLEANING FOR BLAKE...
# Investigate the missing data--try to find missing data. For photos, can at least change to cannot determine...
missing.screening.data <- csdatalong %>%
  filter(is.na(screenfail_any) & consent==1) %>%
  select(study_id, eye, screenfail_va, screenfail_iop, screenfail_abnldisc, screenfail_vcdr, screenfail_dr, screenfail_amd)
missingglcdx.data <- csdatalong %>%
  filter(is.na(oe_anyglc) & !is.na(oe_anyglcsuspect))

View(missing.screening.data)

tsreferrandom <- csdatalong %>%
  filter(randomization_model == "Refer") %>%
  filter(ophthalmologist_exam_complete == "0") %>%
  filter(phone_call_complete == "0")


##Missing oe data
missingoedata <- csdatalong %>%
  filter(is.na(sumoedx) & (!is.na(oe_anyamd) | !is.na(oe_anydr) | !is.na(oe_anyglc) | !is.na(oe_anyglcsuspect) | !is.na(oe_anyamddrglc) | !is.na(oe_tx_catsurgnotmiss) | !is.na(oe_tx_vftestnotmiss) | !is.na(oe_tx_rectxnotmiss)))

#View(missingoedata)
#skim(missingoedata)
tsdelete <- missingoedata %>%
  filter(is.na(sex1))


# Note that there are 2 outliers for screening VCDR -- these need to be changed in Redcap:
xtabs(data=csdatalong, ~vcdr+screenfail_vcdr, addNA=TRUE)
xtabs(data=filter(csdatalong, vcdr>1), ~study_id+eye, addNA=TRUE)
# Note that there are 4 outliers for screening VCDR -- these need to be changed in Redcap if possible;
# these look like data entry errors (VA instead of IOP)
xtabs(data=csdatalong, ~oe_vcdr+oe_anyglc, addNA=TRUE)
xtabs(data=filter(csdatalong, oe_vcdr>1), ~study_id+eye, addNA=TRUE)
# Look for outliers in IOP at screening...
xtabs(data=csdatalong, ~iop, addNA=TRUE)
xtabs(data=csdatalong, ~oe_iop, addNA=TRUE)
# There are 7 that look like VCDRs
xtabs(data=filter(csdatalong, oe_iop<1), ~study_id+eye, addNA=TRUE)
wrongoeiop <- csdatalong %>% filter(oe_iop<1) %>% select(study_id, oe_iop, oe_vcdr, eye)


# Note that we seem to have coded oph exam AMD incorrectly in Redcap (not mutually exclusive):
# Identify study id's to clean the data for...
# This shows only 0s and 1s for the oe_amd fields, so can make a sum where 1 is meaningful
csdatalong %>% skim(c("oe_amd_none", "oe_amd_cd", "oe_amd_drusen", "oe_amd_ga", "oe_amd_wet"))
amdexploredata <- csdatalong %>%
  mutate(oe_amdsum=oe_amd_cd+oe_amd_drusen+oe_amd_ga+oe_amd_wet)
xtabs(data=amdexploredata, ~oe_amdsum+oe_amd_none, addNA=TRUE)
xtabs(data=filter(amdexploredata, oe_amdsum>1 | (oe_amdsum>=1 & oe_amd_none==1)), ~study_id+eye, addNA=TRUE)


#############################################
## BOOTSTRAPPED 95% CI ACCOUNTING FOR CLUSTERING OF EYES ##
###########################################################
library(rsample)
library(purrr)

# This creates a nested data frame, where all data with same study id get put on the same line
# So if we resample, we will automatically resample all data from the same person
D <- alldata %>% filter(!is.na(camera)) %>% nest(-study_id)
head(D)

set.seed(154234)
# The bs object is the boostrap object; we are creating separate populations with resampling
# You could alter the "times" option; usually use small number of replications as testing code because faster
# But then change to a larger number (9999?) for the final analysis
bs <- bootstraps(D, times = 9)

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
bs_sensspec <- map(bs$splits, ~as_tibble(.) %>% unnest %>% 
                     filter(!is.na(camera)) %>%
                     group_by(camera) %>% 
                     class_metrics(., truth =dr_right, estimate = dr_adj)) %>% 
  bind_rows(.id = 'boots') %>%
  select(-.estimator) %>%
  spread(.metric, .estimate, convert=TRUE) %>%
  group_by(camera) %>%
  summarize_at(vars(sens, spec, ppv, npv), lst(!!!p_funs))


# Now merge together the estimates and CIs and reshape into more useful format
sensspectable <- full_join(sensspec_estimates, bs_sensspec, by="camera")
sensspectablelong <- sensspectable %>%
  gather(field, value, npv_est:npv_upper95) %>%
  separate(field, into=c("metric", "stat"), sep="_") %>%
  spread(stat, value, convert=TRUE)

------------------
  
  demo_stats <- csdatawide %>%
  filter(consent==1) %>%
  group_by(dmclinic) %>%
  summarize(consent_n=sum(!is.na(consent)),
            consentyes_n=sum(consent==1),
            consentno_n=sum(consent==0),
            age_p50=quantile(age, 2/4),
            age_p25=quantile(age, 1/4),
            age_p75=quantile(age, 3/4),
            age_n=sum(!is.na(age)),
            female_per=mean(sex1),
            female_n=sum(!is.na(sex1)),
            dm_per=mean(diagnosis_of_diabetes, na.rm=TRUE),
            dm_n=sum(!is.na(diagnosis_of_diabetes)))

demostats_wide <- demo_stats %>%
  select(-(consent_n:age_n)) %>%
  gather(field, value, female_per:dm_n) %>%
  separate(field, into=c("variable","stat"), sep="_") %>%
  mutate(dmclinicstat=paste(dmclinic, stat, sep="_")) %>%
  select(-dmclinic, -stat) %>%
  spread(dmclinicstat, value)

ts1 <- csdatalong %>%
  filter(is.na(oe_anyglcsuspect),
         ophthalmologist_exam_complete == 2)
View(ts1)
