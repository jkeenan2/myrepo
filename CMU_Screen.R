library(readr)
library(readxl)
library(tidyverse)
# library(rlang)
library(skimr)
# library(irr)
# install.packages(lpSolve)
# library(lpSolve)
library(lubridate)

csaddress <- read_csv("csdatawide.addresses_english.csv")
costscreened_redcapexport <- read_csv("CostSCREENED_DATA_2020-04-29_0845.csv")
glimpse(costscreened_redcapexport)
# Check out different types of records
xtabs(data=costscreened_redcapexport , ~redcap_event_name+clinic_attended_v2, addNA=TRUE) # I guess we only want screening visit
# Who to include? Consented? At clinic?
xtabs(data=costscreened_redcapexport, ~clinic_attended_v2+consent, addNA=TRUE)

# Addresses
# Blake did this but his file does not have a leading zero for the ones that start with zero.
# So we can add a leading zero to anything with less than 7 characters -- see str_pad b elow.
csaddress2 <- csaddress %>%
  mutate(chiangmai.province=if_else(province_english == "Chiang Mai", 1, 0),
         chiangmai.city=if_else((province_english == "Chiang Mai" & district_english == "city"), 1, 0),
         study_id=as.character(study_id),
         study_id=str_pad(as.character(study_id), 7, pad = "0")) %>%
  select(study_id, chiangmai.province, chiangmai.city)
# csdatawide_blake <- full_join(csdatawide, csaddress2, by="study_id") %>%
#   select(study_id, clinic_attended_v2, chiangmai.province, chiangmai.city)
# addmargins(xtabs(data=filter(csdatawide_blake, is.na(chiangmai.province) | is.na(chiangmai.city)), ~study_id+chiangmai.city, addNA=TRUE))


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

randomization_model <- read_csv("randomization_model_results.csv")
# JK: I am not understanding why there is missing data. I wouldn't think there would be missing data. 
# Where did this randomization_model_results.csv file come from? Is it from Redcap? 
# You would have had to have merged it correct? I wonder if safer to just do it in the R code?
# This is the original file, I believe. But we should export from Redcap

randomization_model_jk <- read_csv("CostscreenedRandomizationCopiedFromRedcap.csv") %>%
  separate(number_result, into=c("refer_negative", "randomization_model"), sep=", ") %>%
  mutate(refer_negative=as.numeric(refer_negative))

screeningdata3 <- right_join(randomization_model_jk, screeningdata, by="refer_negative")


# BLAKE's ALTERNATIVE...
randomization_model <- read_csv("randomization_model_results.csv")
screeningdata2 <- full_join(randomization_model, screeningdata, by="study_id")
glimpse(screeningdata2)

# Comparing Jeremy's and Blake's;they are the same. Will use Jeremy's.
# sd2 <- screeningdata2 %>%
#   select(study_id, randomization_model) %>%
#   arrange(study_id)
# sd3 <- screeningdata3 %>%
#   select(study_id, randomization_model) %>%
#   arrange(study_id)
# randomcompare <- full_join(sd2, sd3, by="study_id") %>%
#   mutate(identical=if_else(randomization_model.x==randomization_model.y,T,F))
# View(randomcompare)



## BMS: iop (iop_left_eye_os, iop_right_eye_od) and travel_cost is wrongly labeled as a character 
## and not numeric here, need to correct 

# JK: I like skim in this situation to easily see if there are any numeric variables currently listed as character
# This reveals travel_cost, iop_right_eye_od, iop_left_eye_os
# I would then just tabulate to see what the issue is
skim(screeningdata2)
# xtabs(data=screeningdata2, ~travel_cost, addNA=TRUE) # The 00 --> JK will fix in Redcap: xtabs(data=filter(screeningdata2, travel_cost=="00"), ~study_id)
# xtabs(data=screeningdata2, ~iop_right_eye_od, addNA=TRUE) # Just leading zeros --> JK will fix in Redcap: xtabs(data=filter(screeningdata2, iop_right_eye_od=="08"), ~study_id)
# xtabs(data=screeningdata2, ~iop_left_eye_os, addNA=TRUE) # The "na" --> JK will fix in Redcap: xtabs(data=filter(screeningdata2, iop_left_eye_os=="na" | iop_left_eye_os=="09"), ~study_id)
# screeningdata2$travel_cost <- as.numeric(screeningdata2$travel_cost)
# screeningdata2$iop_left_eye_os <- as.numeric(screeningdata2$iop_left_eye_os)
##BMS - Is it ok that NAs were introduced by coercion? # JK: see above; sometimes easier just to change in Redcap

## BMS: Reviewing the skim file, there are a couple areas of "missing" data, in particular VCDR (several) and iop (1), 
## I double checked and these were all correctly identified and VCDR was unobtainable 
## At what stage do you remove these eyes from analysis? 
#JK: do not remove. I don't think. 

xtabs(data=screeningdata3, ~refer_positive+randomization_model, addNA=TRUE)
# 1, Refer Positive Screen ดูหน้าจอบวก
# 2, Refer Randomization อ้างถึงการสุ่มตัวอย่าง
# 3, Refer Can Not Determine อ้างอิงไม่สามารถกำหนด
# 0, No referral ไม่มีการอ้างอิง
# Blake says: If I am not mistaken, this variable (refer_positive) is just a radio button that Ying would select an answer 
# I think better to use a calculation to avoid potential error.
# JK: confirmed, we never use this again, I think I was just doing some data checks with it

# We essentially want only 2 main data frames: a wide one and long one.
# I am only including the screeningdata and yingdata dataframes for now but we can add more before this point if needed
csdatawide <- full_join(screeningdata3, yingdata, by="study_id") %>%
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
  # Now put all the re/le variables at the end so can reshape to long
  select(study_id:examiner_v2, end_time_ying_screen:examiner, dilated:eyeart_screen_outcome, eyeart_flag:oe_examiner, oe_tx_none:vf_testing_complete, vf_further_tx:eyeart_screen_outcome_oephoto, eyeart_result_dilate_opexam_complete,time_start_exam_or_pecen, this_form_refer_calc_pecen:pecen_review_screening_complete, this_form_refer_calc_pecen_v2:pecen_review_dilated_complete, everything()) %>% # to find duplicate column names: select(csdata)
  left_join(., csaddress2, by="study_id")
# TO GET CSV SO WE CAN TRANSLATE THE ADDRESSES
# csdatawide.addresses <- csdatawide %>%
#   select(study_id, address, current_address_provinc, current_address_amphoe,current_address_tambon)
# write_csv(csdatawide.addresses, "csdatawide.addresses.csv")

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
         screenfail_amdcdonly=if_else(is.na(amd), NA_real_, if_else(amd %in% c(2),1,0)),
         screenfail_drcdonly=if_else(is.na(dr), NA_real_, if_else(dr %in% c(2),1,0)),
         screenfail_vcdrcdonly=if_else(is.na(vcdr), 1, 0),
         screenfail_photo_abdisccdonly=if_else(is.na(photo_abdisc), NA_real_, if_else(photo_abdisc %in% c(2),1,0)),
         screenfail_photocdonly=if_else(screenfail_amdcdonly==1 & screenfail_drcdonly==1 & screenfail_vcdrcdonly==1 & screenfail_photo_abdisccdonly==1,1,0),
         screenfail_photocdany=if_else(screenfail_amdcdonly==1 | screenfail_drcdonly==1 | screenfail_vcdrcdonly==1 | screenfail_photo_abdisccdonly==1,1,0),
         screenfail_other=if_else(is.na(other_referral), NA_real_, 
                          if_else(other_referral==1 & screenfail_amdcd==0 & screenfail_drcd==0 & screenfail_abnldisccd==0 & screenfail_vcdrcd==0,1,0)),
         screenfail_photopos=if_else(is.na(amd) & is.na(dr) & is.na(vcdr) & is.na(photo_abdisc & is.na(screenfail_other)), NA_real_, 
                             if_else( (screenfail_amd==1 | screenfail_dr==1 | screenfail_vcdr==1 | screenfail_abnldisc==1 | screenfail_other==1),1,0)),
         screenfail_photoposcd=if_else(is.na(amd) & is.na(dr) & is.na(vcdr) & is.na(photo_abdisc) & is.na(screenfail_other), NA_real_, 
                               if_else( (screenfail_amdcd==1 | screenfail_drcd==1 | screenfail_vcdrcd==1 | screenfail_abnldisccd==1 | screenfail_other==1),1,0)),
         # JK: These screenfail_any are old; leaving them here but I changed it a little. I think screenfail_other might have been checked when poor quality photos?
         # screenfail_any=if_else(screenfail_va==1 | screenfail_iop==1 | screenfail_abnldisc==1 | screenfail_vcdr==1 | screenfail_dr==1 | screenfail_amd==1 | screenfail_photopos==1 | screenfail_other==1 , 1, 0),
         # screenfail_anycd=if_else(screenfail_va==1 | screenfail_iop==1 | screenfail_abnldisccd==1 | screenfail_vcdr==1 |  screenfail_dr==1 | screenfail_amd==1 | screenfail_other==1 | screenfail_amdcd==1 | screenfail_drcd==1 | screenfail_photopos==1 | screenfail_vcdrcd==1 |screenfail_abnldisccd ==1 | screenfail_photoposcd==1, 1, 0),
         screenfail_any=  if_else(screenfail_va==1 | screenfail_iop==1 | screenfail_abnldisc==1   | screenfail_vcdr==1   | screenfail_dr==1   | screenfail_amd==1, 1, 0),
         screenfail_anycd=if_else(screenfail_va==1 | screenfail_iop==1 | screenfail_abnldisccd==1 | screenfail_vcdrcd==1 | screenfail_drcd==1 | screenfail_amdcd==1 | screenfail_other==1, 1, 0),
         # xtabs(data=csdatalong, ~amd+screenfail_any, addNA=TRUE)
         oe_anyamd=if_else(oe_amd_missing==1, NA_real_, 
                   if_else(oe_amd_drusen==1 | oe_amd_ga==1 | oe_amd_wet==1, 1, 0)), # xtabs(data=csdatalong, ~oe_anyamd+oe_amd_cd, addNA=TRUE)  # xtabs(data=csdatalong, ~oe_anyamd+oe_amd_drusen, addNA=TRUE)  # xtabs(data=csdatalong, ~oe_anyamd+oe_amd_ga, addNA=TRUE)  # xtabs(data=csdatalong, ~oe_anyamd+oe_amd_wet, addNA=TRUE)
         oe_anydr=if_else(is.na(oe_dr), NA_real_, 
                  if_else(oe_dr %in% c(1,2),1,0)),  # xtabs(data=csdatalong, ~oe_anydr, addNA=TRUE)
         oe_anyglcfalse=if_else(is.na(oe_iop), NA_real_, 
                   if_else(oe_glaucomadx %in% c(1),1,0)),
         oe_anyglc=if_else(oe_tx_missing %in% 0,oe_anyglcfalse,0),
         oe_anyglcsuspect=if_else(is.na(oe_vcdr) & is.na(oe_iop) & is.na(oe_discheme) & is.na(oe_discnotch) & is.na(oe_rnfldefect), NA_real_, 
                                  if_else((!is.na(oe_vcdr) & oe_vcdr>0.6) | oe_iop>21 | oe_discheme %in% 1 | oe_discnotch %in% 1 | oe_rnfldefect %in% 1 ,1,0)), # xtabs(data=filter(csdatalong, is.na(oe_anyglcsuspect) & oe_anyglcfalse==0), ~study_id)
         oe_anyglcsuspectglc=if_else(is.na(oe_anyglc) & is.na(oe_anyglcsuspect),NA_real_,
                                     if_else(oe_anyglc==1 | oe_anyglcsuspect==1,1,0)),
         oe_anyamddrglc=if_else(is.na(oe_anyamd) & is.na(oe_anydr) & is.na(oe_anyglc),NA_real_,
                        if_else(oe_anyamd==1 | oe_anydr==1 | oe_anyglc==1,1,0)),
         oe_anyamddrglcsuspglc=if_else(is.na(oe_anyamd) & is.na(oe_anydr) & is.na(oe_anyglc) & is.na(oe_anyglcsuspect),NA_real_,
                                if_else(oe_anyamd==1 | oe_anydr==1 | oe_anyglc==1 | oe_anyglcsuspect==1,1,0)),
         oe_tx_catsurgnotmiss =if_else(oe_tx_missing==1, NA_real_, 
                               if_else(oe_tx_catsurg==1, 1, 0)),
         oe_tx_vftestnotmiss =if_else(oe_tx_missing==1, NA_real_, 
                               if_else(oe_tx_vftest==1, 1, 0)),
         oe_tx_rectxnotmiss =if_else(oe_tx_missing==1, NA_real_, 
                               if_else(oe_tx_rectx==1, 1, 0)),
         sumoedx=oe_anyamd+oe_anydr+oe_anyglc+oe_anyglcsuspect+oe_anyamddrglc+oe_tx_catsurgnotmiss+oe_tx_vftestnotmiss+oe_tx_rectxnotmiss,
         clinic_attended_v2=recode_factor(clinic_attended_v2, '0'="Diabetes", '1'="Thyroid",'2'="General"),
         eyeexamined=if_else(!is.na(oe_iop), 1,0)) %>%
  group_by(study_id) %>% 
  mutate(maxanyfailcd=max(screenfail_anycd), # So this is seeing whether the entire patient was referred, dichotomous 1/0
         maxvafail=max(screenfail_va),
         maxiopfail=max(screenfail_iop),
         maxphotocdfail=max(screenfail_photoposcd),
         maxphotofail=max(screenfail_photopos),
         maxphotocdany=max(screenfail_photocdany),
         maxabnldisccdfail=max(screenfail_abnldisccd),
         maxamdcdfail=max(screenfail_amdcd),
         maxdrcdfail=max(screenfail_drcd),
         maxvcdrcdfail=max(screenfail_vcdrcd),
         
         maxabnldiscfail=max(screenfail_abnldisc),
         maxamdfail=max(screenfail_amd),
         maxdrfail=max(screenfail_dr),
         maxvcdrfail=max(screenfail_vcdr),
         
         maxsxblurry=max(sx_blurry),
         maxsxfloater=max(sx_floater),
         maxsxflash=max(sx_flash),
         maxsxscotoma=max(sx_scotoma),
         maxsxother=max(sx_other),
         maxsxany=if_else(maxsxblurry==1 | maxsxfloater==1 | maxsxflash==1 | maxsxscotoma==1 | maxsxother,1,0),
         maxsxvisualsymptoms=max(visual_symptoms),
         maxoeamd=max(oe_anyamd),
         maxoedr=max(oe_anydr),
         maxoeglc=max(oe_anyglcfalse, na.rm = TRUE),
         maxoeglcsuspect=max(oe_anyglcsuspect, na.rm = TRUE),
         maxoeglcsuspectorglc=case_when(maxoeglc==1 | maxoeglcsuspect==1 ~ 1,
                                        maxoeglc==0 & maxoeglcsuspect==0 ~ 0,
                                        TRUE ~ NA_real_),
         maxoecataract=max(oe_tx_catsurgnotmiss),
         maxoecataractlens=max(oe_lens_cataract),
         maxoeamddrglc=case_when(maxoeglc==1 | maxoeglcsuspect==1 | maxoedr==1 | maxoeamd==1 ~ 1,
                                 maxoeglc==0 & maxoeglcsuspect==0 & maxoedr==0 & maxoeamd==0 ~ 0,
                                 TRUE ~ NA_real_),
         maxoeamddrglccat=case_when(maxoeglc==1 | maxoeglcsuspect==1 | maxoedr==1 | maxoeamd==1  | maxoecataract==1 ~ 1,
                                    maxoeglc==0 & maxoeglcsuspect==0 & maxoedr==0 & maxoeamd==0  & maxoecataract==0 ~ 0,
                                    TRUE ~ NA_real_),
         maxoeretina=max(oe_tx_rectxnotmiss),
         maxoevftest=max(oe_tx_vftestnotmiss),
         maxoeiop=max(oe_iop, na.rm=TRUE),
         maxoeiop=if_else(maxoeiop==-Inf, NA_real_, as.numeric(maxoeiop)),
         shouldcompletegs=ifelse((maxanyfailcd== 1 | randomization_model == "Refer"), 1, 0),
         personexamined=max(eyeexamined),
         accountedfor=ifelse((ophthalmologist_exam_complete ==2 | phone_call_complete==2), 1, 0))

addmargins(xtabs(data=filter(csdatalong, consent==1), ~ screenfail_anycd + refer_patient_randomizatio, addNA=TRUE))

# JK: I figured out why this wasn't working before.
# IOP was a character variable, not numeric.
# So when I tried to do the screenfail_iop variable, 
# it was treating anything whose character was greater than 22 (eg, 3, 8, etc.) as fulfilling the criteria
# So I added the as.numeric() and now it's working.
# The way I troubleshot this was to make some dataframes with the ones that didn't make sense, and then just spot checked


  

ts48.nonefail <- csdatalong %>% filter(!is.na(oe_anyamd) & maxanyfailcd==0 & randomization_model=="Refer") %>% select(starts_with("oe"))
ts229.anyfail <- csdatalong %>% filter(!is.na(oe_anyamd) & maxanyfailcd==1) %>% select(starts_with("oe"))

xtabs(data=csdatalong, ~accountedfor+shouldcompletegs, addNA=TRUE)
xtabs(data=filter(csdatalong, shouldcompletegs==0 & accountedfor==1), ~study_id)
# JK: Why are there missing values for shouldcompletegs? (It's because randomization_model is missing; Blake to randomize these)
xtabs(data=filter(csdatalong, is.na(shouldcompletegs)), ~study_id+randomization_model, addNA=TRUE)
# JK: 8 patients who didn't need a gold standard exam but got one. Agree with BMS to just ignore (ie not include in analysis)
# gsts <- csdatalongreferralcheck %>% filter(is.na(shouldcompletegs)) %>% select(consent, maxanyfail, randomization_model, iop, oe_iop, starts_with("screenfail"))

###   PREVIOUS CLEANING   ###
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
# xtabs(data=csdatalong, ~iop+screenfail_iop,addNA=TRUE) # I changed code, assuming that we didn't refer if missing IOP
# Finishing for the rest of them...
# The reason this is important is because we eventually want to see whether referring based on a specific test was sens/spec for diagnosing a specific disease
# For example we wouldn't necessarily expect IOP to be a great test for DR/AMD.
# addmargins(xtabs(data=csdatalong, ~va+screenfail_va,addNA=TRUE))
# This shows that no one with presenting VA of 4 or 5 was referred (correct)
# THe addmargins is nice because easier to add up 201+15+55+80=351
# If vision <4, then got pinhole...
# addmargins(xtabs(data=filter(csdatalong, va<4), ~vaph+screenfail_va,addNA=TRUE))
# Note 351 total, so this accounting for everyone it should.
# Here it's cleaner (the 0/1/2/3 are referred, and the 4/5 are not)
# xtabs(data=csdatalong, ~photo_abdisc+screenfail_abnldisc,addNA=TRUE)
# xtabs(data=csdatalong, ~photo_abdisc+screenfail_abnldisccd,addNA=TRUE)
# xtabs(data=csdatalong, ~vcdr+screenfail_vcdr,addNA=TRUE)
# For this one, I don't think we want missing values. So I will change the above code to fix this.
# xtabs(data=csdatalong, ~vcdr+screenfail_vcdrcd,addNA=TRUE)
# xtabs(data=csdatalong, ~vcdr_program+screenfail_programvcdr,addNA=TRUE)
# Hard to see because so many values, try this instead:
# csdatalong %>% group_by(screenfail_programvcdr) %>% skim(vcdr_program)
# Again note that there are a bunch of negatives, but I am not sure we want that. I think we didn't refer if CD, right?
# csdatalong %>% group_by(screenfail_programvcdrcd) %>% skim(vcdr_program)
# xtabs(data=csdatalong, ~dr+screenfail_dr,addNA=TRUE)
# xtabs(data=csdatalong, ~dr+screenfail_drcd,addNA=TRUE)
# xtabs(data=csdatalong, ~amd+screenfail_amd,addNA=TRUE)
# xtabs(data=csdatalong, ~amd+screenfail_amdcd,addNA=TRUE)
# xtabs(data=csdatalong, ~other_referral+screenfail_other,addNA=TRUE)
# Noticed that there were no "2"'s for other_referral so deleted screenfail_othercd variable above
# Do you want to check the rest of the variables we made to make sure everything worked?


# addmargins(xtabs(data=filter(csdatalongreferralcheck, consent==1), ~ maxanyfail + refer_patient_randomizatio, addNA=TRUE))
#why is the above different then the below??? # because the above is based on person and the below is based on eye ie. some people had one positive eye and one negative eye
# addmargins(xtabs(data=filter(csdatalongreferralcheck, consent==1), ~ screenfail_anycd + refer_patient_randomizatio, addNA=TRUE))


##########
# Kappas for agreement to OpReview 
###
photoagreement <- full_join(csdatawide, jeremyreview, by="study_id") %>%
  mutate(photoscreenpos=case_when((vcdr_referral.re== 1 | dr_referral.re==1 | amd_referral.re==1 | other_referral.re==1 | vcdr_referral.le==1 | referral_disc_abnormal.le==1 | dr_referral.le==1 | amd_referral.le==1 | other_referral.le==1) ~ 1,
                                  (vcdr_referral.re== 0 & dr_referral.re==0 & amd_referral.re==0 & other_referral.re==0 & vcdr_referral.le==0 & referral_disc_abnormal.le==0 & dr_referral.le==0 & amd_referral.le==0 & other_referral.le==0) ~ 0,
                                  TRUE ~ NA_real_),
         year=year(start_time_inclusion),
         month=month(start_time_inclusion,label=T),
         yr_mo=(paste(year(start_time_inclusion),month(start_time_inclusion,label=T),sep="-"))) %>%
  # select(photoscreenpos, this_form_referral) %>%
  arrange(year,month) %>%
  filter(!is.na(this_form_referral))

photoagreement2 <- full_join(csdatawide, jeremyreview, by="study_id") %>%
  select(refer_patient_screen_cal, this_form_referral) %>%
  filter(!is.na(this_form_referral))

# JK: I think irr package masks other things so prefer not to call it, just use it before the kappa2 command...
(irr::kappa2(photoagreement %>% select(photoscreenpos,this_form_referral), weight = "unweighted", sort.levels = FALSE))
(irr::kappa2(photoagreement2 %>% select(refer_patient_screen_cal,this_form_referral), weight = "unweighted", sort.levels = FALSE))
# JK: kappa is pretty low.

# JM - this is a for loop that will run through calculating kappas at each month
allyrmo <- photoagreement %>% select(yr_mo) %>% distinct() %>% pull()
kappalist=data.frame()
for (value in allyrmo){
  kappa <-(irr::kappa2(photoagreement %>% filter(yr_mo==value) %>% select(photoscreenpos,this_form_referral), weight = "unweighted", sort.levels = FALSE))[5]
  n <-(irr::kappa2(photoagreement %>% filter(yr_mo==value) %>% select(photoscreenpos,this_form_referral), weight = "unweighted", sort.levels = FALSE))[2]
  df <- data.frame(month=c(value),n,kappa) %>% rename(kappa=value) 
  kappalist <- bind_rows(kappalist,df)
}
kappalist # JM - this output shows the month, subjects included in the calculation, and the resulting kappa (for photo referrals)
# try doing the above steps I did in lines 654-676 for photoagreement2 (any referral)

########################################
###     PAPER ANALYSES / RESULTS     ###
########################################
# Flow diagram
# ANY POSITIVE
addmargins(xtabs(data=filter(csdatalong, eye=="re"), ~maxanyfailcd+personexamined, addNA=TRUE ))
addmargins(xtabs(data=filter(csdatalong, eye=="re" & maxanyfailcd==0), ~randomization_model+personexamined, addNA=TRUE ))

# ANY POSITIVE TESTS -- TARGET CONDITION PRESENT
addmargins(xtabs(data=filter(csdatalong, eye=="re" & maxoeamddrglc==1 & maxanyfailcd==1), ~maxoeamd +maxoedr +maxoeglcsuspectorglc, addNA=TRUE ))
addmargins(xtabs(data=filter(csdatalong, eye=="re" & maxoeamddrglc==1 & maxanyfailcd==1 & maxoeglcsuspectorglc==1), ~maxoeglc+maxoeglcsuspect, addNA=TRUE ))
addmargins(xtabs(data=filter(csdatalong, eye=="re" & maxoeamddrglc==1 & maxanyfailcd==1), ~maxoecataract+maxoeamddrglc, addNA=TRUE ))
# ANY POSITIVE TESTS -- TARGET CONDITION ABSENT
addmargins(xtabs(data=filter(csdatalong, eye=="re" & maxoeamddrglc==0 & maxanyfailcd==1), ~maxoeamd +maxoedr +maxoeglcsuspectorglc, addNA=TRUE ))
addmargins(xtabs(data=filter(csdatalong, eye=="re" & maxoeamddrglc==0 & maxanyfailcd==1), ~maxoecataract+maxoeamddrglc, addNA=TRUE ))
# NO POSITIVE TESTS -- TARGET CONDITION PRESENT
addmargins(xtabs(data=filter(csdatalong, eye=="re" & maxoeamddrglc==1 & maxanyfailcd==0 & randomization_model=="Refer"), ~maxoeamd +maxoedr +maxoeglcsuspectorglc, addNA=TRUE ))
addmargins(xtabs(data=filter(csdatalong, eye=="re" & maxoeamddrglc==1 & maxanyfailcd==0 & maxoeglcsuspectorglc==1), ~maxoeglc+maxoeglcsuspect, addNA=TRUE ))
addmargins(xtabs(data=filter(csdatalong, eye=="re" & maxoeamddrglc==1 & maxanyfailcd==0 & randomization_model=="Refer"), ~maxoecataract, addNA=TRUE ))
# NO POSITIVE TESTS -- TARGET CONDITION ABSENT
addmargins(xtabs(data=filter(csdatalong, eye=="re" & maxoeamddrglc==0 & maxanyfailcd==0 & randomization_model=="Refer"), ~maxoeamd +maxoedr +maxoeglcsuspectorglc, addNA=TRUE ))
addmargins(xtabs(data=filter(csdatalong, eye=="re" & maxoeamddrglc==0 & maxanyfailcd==0 & randomization_model=="Refer"), ~maxoecataract, addNA=TRUE ))


# Text first paragraph: age/sex of overall population:
csdatawide %>% 
  filter(consent == 1) %>%
  summarize(consentyes_num=sum(consent==1),
            consentyes_total=sum(!is.na(consent)),
            consentyes_per=(consentyes_num/consentyes_total),
            female_num=sum(sex1==1),
            female_total=sum(!is.na(sex1)),
            female_per=female_num/female_total,
            age_p50=quantile(age, 2/4),
            age_p25=quantile(age, 1/4),
            age_p75=quantile(age, 3/4),
            age_total=sum(!is.na(age))) %>%
  gather(field, value, consentyes_num:age_total) %>%
  separate(field, into=c("variable","stat"), sep="_") %>%
  spread(stat, value)
#Table 2: demographics by clinic
# JK: I think better that we use the original data to do this, and then in the code filter things as needed
# Because when you create lots of different objects, it can get confusing to figure out when numbers don't match up
# And we're more confident in numbers if they always come from the same basic data. (in this case, csdatawide/csdatalong)

table2.demographics <- csdatawide %>%
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
            cm.province_num=sum(chiangmai.province == 1, na.rm=TRUE),
            cm.province_total=sum(!is.na(chiangmai.province)),
            cm.province_per=(cm.province_num/cm.province_total),
            cm.city_num=sum(chiangmai.city == 1, na.rm=TRUE),
            cm.city_total=sum(!is.na(chiangmai.city)),
            cm.city_per=(cm.city_num/cm.city_total),
            dm_num=sum(diagnosis_of_diabetes==1),
            dm_total=sum(!is.na(diagnosis_of_diabetes)),
            dm_per=dm_num/dm_total) %>%
  gather(field, value, consentyes_num:dm_per) %>%
  separate(field, into=c("variable","stat"), sep="_") %>%
  mutate(clinicstat=paste(clinic_attended_v2, stat, sep="_")) %>%
  select(-clinic_attended_v2, -stat) %>%
  spread(clinicstat, value)
# Note: missing duration of DM for one person in the General clinic--later must have been found...
# csdatawide %>% filter(diagnosis_of_diabetes==1 & is.na(years_with_diabetes)) %>% select(study_id)

# Symptoms for table 2:
table2.symptoms <- csdatalong %>% 
  filter(eye=="re") %>% 
  group_by(clinic_attended_v2) %>%
  summarize(blurry=sum(maxsxblurry==1),
            floater=sum(maxsxfloater==1),
            flash=sum(maxsxflash==1),
            scotoma=sum(maxsxscotoma==1),
            othersx=sum(maxsxother==1),
            anysx=sum(maxsxany==1),
            vis_sx=sum(maxsxvisualsymptoms==1),
            anysx_total=sum(!is.na(maxsxany)))

# TEXT
patientscreenfails <- csdatalong %>% 
  filter(eye=="re") %>% 
  group_by(clinic_attended_v2) %>%
  summarize(anyfailcd=sum(maxanyfailcd==1),
            total=sum(!is.na(maxanyfailcd)),
            nonefailrandom=sum(maxanyfailcd==0 & randomization_model=="Refer", na.rm=TRUE),
            vafail=sum(maxvafail==1), # xtabs(data=filter(csdatalongreferralcheck,is.na(maxvafail)), ~study_id)
            # JK cleaned on redcap. Assumed zero letters for pinhole for 3093274 OS.
            iopfail=sum(maxiopfail==1),
            abnldisccdfail=sum(maxabnldisccdfail==1),
            amdcdfail=sum(maxamdcdfail==1),
            drcdfail=sum(maxdrcdfail==1),
            vcdrcdfail=sum(maxvcdrcdfail==1),
            x_onlyphotocdfail=sum(maxphotocdfail==1 & maxvafail==0 & maxiopfail==0, na.rm = TRUE),
            x_onlyvafail=sum(maxphotocdfail==0 & maxvafail==1 & maxiopfail==0),
            x_onlyiopfail=sum(maxphotocdfail==0 & maxvafail==0 & maxiopfail==1),
            x_vaiopfail=sum(maxphotocdfail==0 & maxvafail==1 & maxiopfail==1),
            x_vaphotocdfail=sum(maxphotocdfail==1 & maxvafail==1 & maxiopfail==0),
            x_iopphotocdfail=sum(maxphotocdfail==1 & maxvafail==0 & maxiopfail==1),
            x_vaiopphotocdfail=sum(maxphotocdfail==1 & maxvafail==1 & maxiopfail==1),
            anyfail_completedophthoexam=sum(!is.na(oe_anyamd) & maxanyfailcd==1),
            nonefail_completedophthoexam=sum(!is.na(oe_anyamd) & maxanyfailcd==0 & randomization_model=="Refer", na.rm=TRUE))
# TOTALS for text
patientscreenfails %>%   
  pivot_longer(cols=anyfailcd:nonefail_completedophthoexam,
               names_to = "field",
               values_to = "value") %>%
  group_by(field) %>%
  summarize(sum=sum(value),
            percent=sum/889)
# FIGURE 1: Venn diagram
library(eulerr)
# A=VA; B=IOP; C=Photo; D=Normal (this is 889-229=660)
# x_iopphotocdfail                 5 --> B&C
# x_onlyiopfail                   22 --> B 
# x_onlyphotocdfail               81 --> C 
# x_onlyvafail                    69 --> A 
# x_vaiopfail                      3 --> A&B
# x_vaiopphotocdfail               2 --> A&B&C
# x_vaphotocdfail                 47 --> A&C 
venndiagram <- euler(c("D&A"=69, "D&B"=22, "D&C"=81, D=660, "D&A&B"=3, "D&A&C"=47, "D&B&C"=5, "D&A&B&C"=2), shape="ellipse")
vennplot <- plot(venndiagram, quantities=TRUE)
vennplot
# ggsave("vennplot.svg", plot=vennplot, device="svg", width=3.3, height=3.3, units="in")

prostheses <- csdatalong %>%
  filter(grepl("prosthe",notes_for_flag) | grepl("prosthe",oe_majorcause_specother)) %>%
  select(study_id, eye,  va,iop, vcdr, dr, amd, notes_for_flag, oe_majorcause_specother, screenfail_anycd, screenfail_va, randomization_model)
# JK: Not sure but the notes_for_flag seems to be an error since there is screening data for everything
# So will only exclude 3867078 OS
# Table 3: Screening test results
table3 <- csdatalong %>%
  filter(consent==1) %>%
  filter(!(study_id==3867078 & eye=="le")) %>%
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
            abnldiscfailcd_num=sum(screenfail_abnldisccd==1 & screenfail_abnldisc==0, na.rm=TRUE),
            abnldiscfailcd_total=sum(!is.na(screenfail_abnldisc) & !is.na(screenfail_abnldisccd)),
            abnldiscfailcd_per=abnldiscfailcd_num/abnldiscfailcd_total,
            vcdrfail_num=sum(screenfail_vcdr==1, na.rm=TRUE),
            vcdrfail_total=sum(!is.na(screenfail_vcdr)),
            vcdrfail_per=vcdrfail_num/vcdrfail_total,
            vcdrfailcd_num=sum(screenfail_vcdrcd==1 & screenfail_vcdr==0, na.rm=TRUE),
            vcdrfailcd_total=sum(!is.na(screenfail_vcdr) & !is.na(screenfail_vcdrcd)),
            vcdrfailcd_per=vcdrfailcd_num/vcdrfailcd_total,
            amdfail_num=sum(screenfail_amd==1, na.rm=TRUE),
            amdfail_total=sum(!is.na(screenfail_amd)),
            amdfail_per=amdfail_num/amdfail_total,
            amdfailcd_num=sum(screenfail_amdcd==1 & screenfail_amd==0, na.rm=TRUE),
            amdfailcd_total=sum(!is.na(screenfail_amd) & !is.na(screenfail_amdcd)),
            amdfailcd_per=amdfailcd_num/amdfailcd_total,
            drfail_num=sum(screenfail_dr==1, na.rm=TRUE),
            drfail_total=sum(!is.na(screenfail_dr)),
            drfail_per=drfail_num/drfail_total,
            drfailcd_num=sum(screenfail_drcd==1 & screenfail_dr==0, na.rm=TRUE),
            drfailcd_total=sum(!is.na(screenfail_dr) & !is.na(screenfail_drcd)),
            drfailcd_per=drfailcd_num/drfailcd_total,
            otherfail_num=sum(screenfail_other==1, na.rm=TRUE),
            otherfail_total=sum(!is.na(screenfail_other)),
            otherfail_per=otherfail_num/otherfail_total,
            photofail_num=sum(screenfail_photopos==1, na.rm=TRUE),
            photofail_total=sum(!is.na(screenfail_photopos)),
            photofail_per=photofail_num/photofail_total,
            photofailcd_num=sum(screenfail_photoposcd==1 & screenfail_photopos==0, na.rm=TRUE),
            photofailcd_total=sum(!is.na(screenfail_photoposcd) & !is.na(screenfail_photopos)),
            photofailcd_per=photofailcd_num/photofailcd_total,
            oldphotofail_num=sum(screenfail_abnldisc==1 | screenfail_vcdr==1 | screenfail_amd==1 | screenfail_dr==1),
            oldphotofail_total=sum(!is.na(screenfail_abnldisc) & !is.na(screenfail_vcdr) & !is.na(screenfail_amd) & !is.na(screenfail_dr)),
            oldphotofail_per=oldphotofail_num/oldphotofail_total,
            anyfailcd_num=sum(screenfail_anycd==1, na.rm=TRUE),
            anyfailcd_total=sum(!is.na(screenfail_anycd)),
            anyfailcd_per=anyfailcd_num/anyfailcd_total, 
            anyfail_num=sum(screenfail_any==1, na.rm=TRUE),
            anyfail_total=sum(!is.na(screenfail_any)),
            anyfail_per=anyfail_num/anyfail_total) %>%
  gather(field, value, vafail_num:anyfail_per) %>%
  separate(field, into=c("variable","stat"), sep="_") %>%
  mutate(clinicstat=paste(clinic_attended_v2, stat, sep="_")) %>%
  select(-clinic_attended_v2, -stat) %>%
  spread(clinicstat, value)

# TABLE 4: OPHTHALMOLOGIST EXAM results
# Data of just the people with eye exams:
csdatalong.eyeexam <- csdatalong %>%
  filter(consent==1 & !is.na(oe_iop)) %>% # ie, consent means they were consented for screening and oe_iop is complete for everyone getting a gold standard exam
  filter(!(study_id==3867078 & eye=="le")) %>% # This is the prosthetic eye
  filter(maxanyfailcd==1 | (maxanyfailcd==0 & randomization_model %in% "Refer"))
# Text: These give the number of patients and eyes that actually got an ophthalmology exam
addmargins(xtabs(data=filter(csdatalong.eyeexam, eye=="re"), ~clinic_attended_v2+maxanyfailcd,addNA=TRUE))
addmargins(xtabs(data=csdatalong.eyeexam, ~clinic_attended_v2+screenfail_anycd,addNA=TRUE))

## Supplemental Table 1 -- eye level gold standard results: MISSING DATA FOR 2725659 (ophth exam, referral questions)
# Note we shouldn't report referral questions here because those were asked at the patient level not eye level
table4wide <- csdatalong.eyeexam %>%
  group_by(clinic_attended_v2, screenfail_anycd) %>%
  summarize(consent.denom=sum(consent==1),
            # oe_anyamd.mean=mean(oe_anyamd, na.rm=TRUE), 
            oe_anyamd.num=sum(oe_anyamd == 1, na.rm=TRUE),
            oe_anyamd.denom=sum(!is.na(oe_anyamd), na.rm=TRUE),
            oe_anyamd.prop=oe_anyamd.num/oe_anyamd.denom,
            # oe_anydr.mean=mean(oe_anydr, na.rm=TRUE), 
            oe_anydr.num=sum(oe_anydr == 1, na.rm=TRUE),
            oe_anydr.denom=sum(!is.na(oe_anydr), na.rm=TRUE),
            oe_anydr.prop=oe_anydr.num/oe_anydr.denom,
            # oe_anyglc.mean=mean(oe_anyglcfalse, na.rm=TRUE), 
            oe_anyglc.num=sum(oe_anyglcfalse == 1, na.rm=TRUE),
            oe_anyglc.denom=sum(!is.na(oe_anyglcfalse), na.rm=TRUE),
            oe_anyglc.prop=oe_anyglc.num/oe_anyglc.denom,
            # oe_anyglcsuspect.mean=mean(oe_anyglcsuspect, na.rm=TRUE), 
            oe_anyglcsuspect.num=sum(oe_anyglcsuspect == 1, na.rm=TRUE),
            oe_anyglcsuspect.denom=sum(!is.na(oe_anyglcsuspect), na.rm=TRUE),
            oe_anyglcsuspect.prop=oe_anyglcsuspect.num/oe_anyglcsuspect.denom,
            # oe_anyamddrglc.mean=mean(oe_anyamddrglc, na.rm=TRUE), 
            oe_anyamddrglc.num=sum(oe_anyamddrglc == 1, na.rm=TRUE),
            oe_anyamddrglc.denom=sum(!is.na(oe_anyamddrglc), na.rm=TRUE),
            oe_anyamddrglc.prop=oe_anyamddrglc.num/oe_anyamddrglc.denom,
            # oe_tx_vftestnotmiss.mean=mean(oe_tx_vftestnotmiss, na.rm=TRUE), 
            oe_tx_vftestnotmiss.num=sum(oe_tx_vftestnotmiss == 1, na.rm=TRUE),
            oe_tx_vftestnotmiss.denom=sum(!is.na(oe_tx_vftestnotmiss), na.rm=TRUE),
            oe_tx_vftestnotmiss.prop=oe_tx_vftestnotmiss.num/oe_tx_vftestnotmiss.denom,
            # oe_tx_catsurgnotmiss.mean=mean(oe_tx_catsurgnotmiss, na.rm=TRUE), 
            oe_tx_catsurgnotmiss.num=sum(oe_tx_catsurgnotmiss == 1, na.rm=TRUE),
            oe_tx_catsurgnotmiss.denom=sum(!is.na(oe_tx_catsurgnotmiss), na.rm=TRUE),
            oe_tx_catsurgnotmiss.prop=oe_tx_catsurgnotmiss.num/oe_tx_catsurgnotmiss.denom,
            oe_lens_cataract.num=sum(oe_lens_cataract == 1, na.rm=TRUE),
            oe_lens_cataract.denom=sum(!is.na(oe_lens_cataract), na.rm=TRUE),
            oe_lens_cataract.prop=oe_lens_cataract.num/oe_lens_cataract.denom,
            # oe_tx_rectxnotmiss.mean=mean(oe_tx_rectxnotmiss, na.rm=TRUE), 
            oe_tx_rectxnotmiss.num=sum(oe_tx_rectxnotmiss == 1, na.rm=TRUE),
            oe_tx_rectxnotmiss.denom=sum(!is.na(oe_tx_rectxnotmiss), na.rm=TRUE),
            oe_tx_rectxnotmiss.prop=oe_tx_rectxnotmiss.num/oe_tx_rectxnotmiss.denom) %>%
  pivot_longer(cols=consent.denom:oe_tx_rectxnotmiss.prop,
               names_to=c("var", "stat"),
               names_sep="\\.",
               values_to="value") %>%
  mutate(clinicstat=paste(clinic_attended_v2, stat, sep="_")) %>%
  ungroup() %>%
  select(-clinic_attended_v2, -stat) %>%
  pivot_wider(names_from = clinicstat,
              values_from="value")

##table 4 -- patient level gold standard results MISSING DATA FOR 2725659 (ophth exam, referral questions)
table4.patient <- csdatalong.eyeexam %>%
  filter(eye=="re") %>%
  group_by(clinic_attended_v2, maxanyfailcd) %>%
  summarize(consent.denom=sum(consent==1),
            oe_anyamd.num=sum(maxoeamd == 1),
            oe_anyamd.denom=sum(!is.na(maxoeamd)),
            oe_anyamd.prop=oe_anyamd.num/oe_anyamd.denom,
            oe_anydr.num=sum(maxoedr == 1),
            oe_anydr.denom=sum(!is.na(maxoedr)),
            oe_anydr.prop=oe_anydr.num/oe_anydr.denom,
            oe_anyglcsus.num=sum(maxoeglcsuspect == 1, na.rm=TRUE),
            oe_anyglcsus.denom=sum(!is.na(maxoeglcsuspect)),
            oe_anyglcsus.prop=oe_anyglcsus.num/oe_anyglcsus.denom,
            oe_anyglc.num=sum(maxoeglc == 1, na.rm=TRUE),
            oe_anyglc.denom=sum(!is.na(maxoeglc)),
            oe_anyglc.prop=oe_anyglc.num/oe_anyglc.denom,
            oe_anyamddrglc.num=sum(maxoeamddrglc == 1, na.rm=TRUE),
            oe_anyamddrglc.denom=sum(!is.na(maxoeamddrglc)),
            oe_anyamddrglc.prop=oe_anyamddrglc.num/oe_anyamddrglc.denom,
            oe_anycatamddrglc.num=sum(maxoeamddrglc == 1 | maxoecataract==1, na.rm=TRUE),
            oe_anycatamddrglc.denom=sum(!is.na(maxoeamddrglc) & !is.na(maxoecataract)),
            oe_anycatamddrglc.prop=oe_anycatamddrglc.num/oe_anycatamddrglc.denom,
            # oe_retref.num=sum(maxoeretina == 1, na.rm=TRUE),
            # oe_retref.denom=sum(!is.na(maxoeretina)),
            # oe_retref.prop=oe_retref.num/oe_retref.denom,
            # oe_vfref.num=sum(maxoevftest == 1, na.rm=TRUE),
            # oe_vfref.denom=sum(!is.na(maxoevftest)),
            # oe_vfref.prop=oe_vfref.num/oe_vfref.denom,
            oe_anycatlens.num=sum(maxoecataractlens== 1, na.rm=TRUE),
            oe_anycatlens.denom=sum(!is.na(maxoecataractlens)),
            oe_anycatlens.prop=oe_anycatlens.num/oe_anycatlens.denom,
            # Note this anycatlens is from the eye-level checkbox "cataract"
            # Based on the numbers, this was checked for even mild cataract
            # The anycataract is person-level checkboxd for advise cataract surgery
            # So for Suppl Table 1 do not report cataract for now. 
            # If they ask in revision you could use the eye-level and person-level variables 
            # to make a mild vs moderate cataract designation (mild if eye-level yes and person-level no, mod if eye-level yes and person-level yes)
            # But requires assumptions so better to try to get away with not reporting the eye-level data
            oe_anycataract.num=sum(maxoecataract == 1, na.rm=TRUE),
            oe_anycataract.denom=sum(!is.na(maxoecataract)),
            oe_anycataract.prop=oe_anycataract.num/oe_anycataract.denom) %>%
  pivot_longer(cols=consent.denom:oe_anycataract.prop,
             names_to=c("var", "stat"),
             names_sep="\\.",
             values_to="value") %>%
  mutate(clinicstat=paste(clinic_attended_v2, stat, sep="_")) %>%
  ungroup() %>%
  select(-clinic_attended_v2, -stat) %>%
  pivot_wider(names_from = clinicstat,
              values_from="value") %>%
  mutate(All_denom=Diabetes_denom+Thyroid_denom+General_denom,
         All_num=Diabetes_num+Thyroid_num+General_num,
         All_prop=All_num/All_denom)

# TABLE 5: PREDICTIVE VALUES
# Shouldn't these weights be different for the different clinics?
xtabs(data=filter(csdatalong, eye=="re"), ~maxanyfailcd + randomization_model, addNA=TRUE) # This for screened population
xtabs(data=filter(csdatalong.eyeexam, eye=="re"), ~maxanyfailcd + randomization_model, addNA=TRUE) # This for ophth-examined population
library(survey)
library(broom)
# EXAMPLE START
# data(api)
# xtabs(data=apistrat, ~pw+fpc)
# EXAMPLE END (I explored that data)
# So based on that example, fpc needs to be 229 and 660
# and weights need to be 1 [1/(229/229)] and 8.91 [1/(76/660)] ;  ie the numbers failing (229) and not failing (660) test, 
# and then the proportion of each that were referred (all of failing; 76/660 of not failing; see tables just above)
# Though this does not account for non-response weights
# https://bookdown.org/jespasareig/Book_How_to_weight_a_survey/nonresponse-weights.html
# Note I am basing this on the numbers failing/not failing and not the numbers presenting for exam. I am not sure which is correct.

# csdatalong.eyeexam.weights <- csdatalong.eyeexam %>%
#   filter(eye=="re") %>%
#   mutate(pw=case_when(maxanyfailcd==1 ~ 1,
#                       maxanyfailcd==0 ~ 1/(76/660),
#                       TRUE ~ NA_real_),
#          fpc=case_when(maxanyfailcd==1 ~ 229,
#                        maxanyfailcd==0 ~ 660,
#                        TRUE ~ NA_real_))
# # Just confirming weights look OK; these should add up to total sample size, which they do:
# csdatalong.screened <- csdatalong %>%
#   filter(consent==1) %>% # ie, consent means they were consented for screening
#   mutate(pw=case_when(maxanyfailcd==1 ~ 1,
#                       maxanyfailcd==0 ~ 1/(76/660),
#                       TRUE ~ NA_real_),
#          fpc=case_when(maxanyfailcd==1 ~ 229,
#                        maxanyfailcd==0 ~ 660,
#                        TRUE ~ NA_real_))
# csdatalong.screened %>% ungroup() %>% dplyr::summarize(sumwt=sum(pw))
# csdatalong.screened %>% ungroup() %>% filter(eye=="re") %>% dplyr::summarize(sumwt=sum(pw))

# Trying lasso regression for nonresponse
library(glmnet)
# First, need population that should have gotten an exam
csdatalongreferred <- csdatalong %>%
  filter(consent==1 & eye=="re") %>% # ie, consent means they were consented for screening and oe_iop is complete for everyone getting a gold standard exam
  filter(maxanyfailcd==1 | (maxanyfailcd==0 & randomization_model %in% "Refer"))
csdatalongreferred.eye <- csdatalong %>%
  filter(consent==1) %>% # ie, consent means they were consented for screening and oe_iop is complete for everyone getting a gold standard exam
  filter(!(study_id==3867078 & eye=="le")) %>% # This is the prosthetic eye
  filter(maxanyfailcd==1 | (maxanyfailcd==0 & randomization_model %in% "Refer")) # 609 eyes referred

# Once not missing could add these in: chiangmai.province + chiangmai.city
formula.resp.lasso <- as.formula("personexamined ~ age + sex1 + travel_time + diagnosis_of_diabetes + maxsxany")
options(na.action = 'na.pass')
x.matrix <- model.matrix(formula.resp.lasso, data = csdatalongreferred)
glm.cv.model <- cv.glmnet(x.matrix, csdatalongreferred$personexamined, alpha = 1, family="binomial")
predicted.lasso <- predict(glm.cv.model, x.matrix, s = "lambda.min", type = "response")[,1]
csdatalongreferred$predicted.lasso <- predicted.lasso
head(predicted.lasso)
# Logistic regression
log.reg.m <- glm(formula.resp.lasso, data = csdatalongreferred, family = "binomial")
predicted.log <- log.reg.m$fitted.values
csdatalongreferred$predicted.log <- predicted.log
csdatalongreferred.eye %<>%
  left_join(., select(csdatalongreferred, predicted.log))
# Comparing lasso vs logistic regression



library(magrittr)
csdatalongreferred %<>%
  mutate(predicted.category.log.reg = (predicted.log > 0.5) %>% as.numeric,
         predicted.category.lasso = (predicted.lasso > 0.5) %>% as.numeric)
train.correct.logreg <- table(csdatalongreferred$personexamined, csdatalongreferred$predicted.category.log.reg) %>% diag() %>% sum()/nrow(csdatalongreferred)
train.correct.lasso <- table(csdatalongreferred$personexamined, csdatalongreferred$predicted.category.lasso) %>% diag() %>% sum()/nrow(csdatalongreferred)
c(train.correct.logreg = train.correct.logreg, train.correct.lasso = train.correct.lasso)
# Seems very close. Identical?
csdatapersonexamined <- csdatalongreferred %>%
  ungroup() %>%
  mutate(pw=case_when(maxanyfailcd==1 ~ 1,
                      maxanyfailcd==0 ~ 1/(76/660),
                      TRUE ~ NA_real_),
         pw.scaled=pw/mean(pw),
         fpc=case_when(maxanyfailcd==1 ~ 229,
                       maxanyfailcd==0 ~ 660,
                       TRUE ~ NA_real_),
         predicted.log.wt=1/predicted.log) %>%
  filter(personexamined==1) %>%
  mutate(predicted.log.wt.scaled=predicted.log.wt/mean(predicted.log.wt),
         finalweight=pw*predicted.log,
         finalweight.scaled=pw.scaled*predicted.log.wt.scaled)
csdatapersonexamined %>% ungroup() %>% summarize(sumpws=sum(pw.scaled), sumlws=sum(predicted.log.wt.scaled), sumfws=sum(finalweight.scaled))
ggplot(data=csdatapersonexamined, aes(x=finalweight.scaled, y=predicted.log.wt.scaled)) +
  geom_point()

addmargins(xtabs(data=csdatalong, ~maxanyfailcd + randomization_model, addNA=TRUE)) # This for screened population
addmargins(xtabs(data=csdatalongreferred.eye, ~maxanyfailcd + randomization_model, addNA=TRUE)) # This for ophth-examined population

csdatapersonexamined.eye <- csdatalongreferred.eye %>%
  ungroup() %>%
  mutate(pw=case_when(maxanyfailcd==1 ~ 1,
                      maxanyfailcd==0 ~ 1/(76/660),
                      TRUE ~ NA_real_),
         pw.scaled=pw/mean(pw),
         fpc=case_when(maxanyfailcd==1 ~ 458,
                       maxanyfailcd==0 ~ 1320,
                       TRUE ~ NA_real_),
         predicted.log.wt=1/predicted.log) %>%
  filter(personexamined==1) %>%
  mutate(predicted.log.wt.scaled=predicted.log.wt/mean(predicted.log.wt),
         finalweight.scaled=pw.scaled*predicted.log.wt.scaled)
csdatapersonexamined.eye %>% ungroup() %>% summarize(sumpws=sum(pw.scaled), sumlws=sum(predicted.log.wt.scaled), sumfws=sum(finalweight.scaled))

surv2 <- svydesign(id=~1,strata=~maxanyfailcd, weights=~pw, data=csdatalong.eyeexam.weights, fpc=~fpc)
newsurv2 <- svydesign(id=~1,strata=~maxanyfailcd, weights=~finalweight.scaled, data=csdatapersonexamined, fpc=~fpc)
  

# So then the PPV of any failed test for cataract:
# (A) Demonstration of difference between weighted and unweighted; looks like this weighting code working
# (B) Demonstration that the answers of suvey analyses are similar for just sample weights (surv2) and sample plus nonresponse weights (newsurv2)
xtabs(data=csdatalong.eyeexam.weights , ~maxanyfailcd+maxoedr, addNA=TRUE)
xtabs(data=csdatapersonexamined , ~maxanyfailcd+maxoedr, addNA=TRUE)
# PPV: 44/(145+44)=0.233
# NPV: 48/(48+2)=0.96
# Sens: wrong way: 44/(2+44)=0.956. If we want to give the screen negatives 10 times the weight, then more like 44/(20+44)=0.688
# Spec: wrong way: 48/(48+145)=0.249. If we want to give the screen negatives 10 times the weight, then more like 480/(480+145)=0.768
# Unweighted ppv
tidy(lm(maxoedr ~ 1, data = filter(csdatalong.eyeexam.weights, maxanyfailcd == 1)), conf.int=TRUE) %>% mutate(stat="ppv")
tidy(lm(maxoedr ~ 1, data = filter(csdatapersonexamined, maxanyfailcd == 1)), conf.int=TRUE) %>% mutate(stat="ppv")
# Weighted ppv
svyciprop(~I(maxoedr==1), subset(surv2, maxanyfailcd==1), method="xlogit")
svyciprop(~I(maxoedr==1), subset(newsurv2, maxanyfailcd==1), method="xlogit")
# Unweighted npv (also by the way note the problem that CI above 1 in this case)
tidy(lm(abs(maxoedr-1) ~ 1, data = filter(csdatalong.eyeexam.weights, maxanyfailcd == 0)), conf.int=TRUE) %>% mutate(stat="npv")
# Weighted npv
svyciprop(~I(abs(maxoedr-1)==1), subset(surv2, maxanyfailcd==0), method="xlogit")
svyciprop(~I(abs(maxoedr-1)==1), subset(newsurv2, maxanyfailcd==0), method="xlogit")
# Unweighted sens
tidy(lm(maxanyfailcd ~ 1, data = filter(csdatalong.eyeexam.weights, maxoedr == 1)), conf.int=TRUE) %>% mutate(stat="sens")
# Weighted sens
svyciprop(~I(maxanyfailcd==1), subset(surv2, maxoedr==1), method="xlogit")
svyciprop(~I(maxanyfailcd==1), subset(newsurv2, maxoedr==1), method="xlogit")

# Unweighted spec
tidy(lm(abs(maxanyfailcd-1) ~ 1, data = filter(csdatalong.eyeexam.weights, maxoedr == 0)), conf.int=TRUE) %>% mutate(stat="spec")
# Weighted spec
svyciprop(~I(abs(maxanyfailcd-1)==1), subset(surv2, maxoedr==0), method="xlogit")
svyciprop(~I(abs(maxanyfailcd-1)==1), subset(newsurv2, maxoedr==0), method="xlogit")

# Function to get sens/spec/ppv/npv
sspvfx <- function(screenresult, examresult) {
  screenresult_mod <- enquo(screenresult)
  examresult_mod <- enquo(examresult)
  # Need to first use this quo_name to put the enquoted variables into an actual formula; then pass the formula to the function
  # When I tried to just use the quo_name in the function, I couldn't get it to work 
  ssf1 <- formula(paste0("~", quo_name(screenresult_mod)))
  ssf2 <- formula(paste0("~I(", quo_name(examresult_mod), "==1)"))
  ssnos <- svyby(ssf1,ssf2,design=newsurv2, unwtd.count) %>% 
    rename(exampos=1)
  ss <- svyby(ssf1,ssf2,design=newsurv2, svyciprop, vartype="ci") %>% 
    rename(exampos=1) %>%
    mutate(est=if_else(exampos==FALSE, 1-(!!screenresult_mod), !!screenresult_mod),
           low95=if_else(exampos==FALSE, 1-ci_u, ci_l),
           up95=if_else(exampos==FALSE, 1-ci_l, ci_u),
           stat=if_else(exampos==FALSE, "spec", "sens")) %>%
    left_join(., ssnos, by="exampos") %>%
    select(stat, est, low95, up95, counts)
  pvf1 <- formula(paste0("~", quo_name(examresult_mod)))
  pvf2 <- formula(paste0("~I(", quo_name(screenresult_mod), "==1)"))
  pvnos <- svyby(pvf1,pvf2,design=newsurv2, unwtd.count) %>% 
    rename(screenpos=1)
  pv <- svyby(pvf1,pvf2,design=newsurv2, svyciprop, vartype="ci") %>% 
    rename(screenpos=1) %>%
    mutate(est=if_else(screenpos==FALSE, 1-(!!examresult_mod), !!examresult_mod),
           low95=if_else(screenpos==FALSE, 1-ci_u, ci_l),
           up95=if_else(screenpos==FALSE, 1-ci_l, ci_u),
           stat=if_else(screenpos==FALSE, "npv", "ppv")) %>%
    left_join(., pvnos, by="screenpos") %>%
    select(stat, est, low95, up95, counts) 
  sspv <- bind_rows(ss, pv) %>%
    mutate(exam=quo_name(examresult_mod),
           screen=quo_name(screenresult_mod))
  newname <- paste0(quo_name(screenresult_mod), ".", quo_name(examresult_mod))
  assign(newname, sspv, envir=.GlobalEnv)
}

# To run all possible combinations, need to use the cross functionality
# examvars <- vars(maxoeamd, maxoedr, maxoeglc, maxoeglcsuspect, maxoeglcsuspectorglc, maxoecataract, maxoeamddrglc, maxoeamddrglccat)
# screenvars <- vars(maxanyfailcd, maxvafail, maxiopfail, maxphotocdfail, maxabnldisccdfail, maxamdcdfail, maxdrcdfail, maxvcdrcdfail, maxphotofail, maxabnldiscfail, maxamdfail, maxdrfail, maxvcdrfail)
examvars <- vars(maxoeamd, maxoedr, maxoeglcsuspectorglc, maxoecataract, maxoeamddrglccat)
screenvars <- vars(maxanyfailcd, maxvafail, maxiopfail, maxphotocdfail)
screenexamcdf <- cross_df(list(screenvars=screenvars, examvars=examvars))
# Just for record-keeping/possible future use, this works for a single variable
# a <- map(examvars, ~sspvfx(maxanyfailcd, !!.x))
# aa <- bind_rows(a, .id="label")
sspvdata <- map2_dfr(screenexamcdf$screenvars, screenexamcdf$examvars, sspvfx)


sspvplotdata <- sspvdata %>%
  mutate(exam=factor(exam, levels=c("maxoeamddrglccat", "maxoecataract", "maxoedr", "maxoeglcsuspectorglc", "maxoeamd")),
         screen=factor(screen, levels=c("maxanyfailcd", "maxphotocdfail", "maxvafail", "maxiopfail")),
         stat=case_when(stat=="sens" ~ "Sens",
                        stat=="spec" ~ "Spec",
                        stat=="ppv" ~ "PPV",
                        stat=="npv" ~ "NPV",
                        TRUE ~ NA_character_))
# To get numbers to put in graph:
sspvplotnums <- sspvplotdata %>%
  filter(stat=="Sens" | stat=="PPV") %>%
  ungroup() %>%
  mutate(senscountwork=if_else(stat=="Sens", counts, NA_real_),
            ppvcountwork=if_else(stat=="PPV", counts, NA_real_)) %>%
  group_by(screen, exam) %>%
  summarize(examnum=max(senscountwork, na.rm=TRUE),
            screennum=max(ppvcountwork, na.rm=TRUE))
library(viridis)
library(RColorBrewer)
brewer.pal(n = 4, name = "Paired")
examlabs <- c(maxoeamddrglccat="Any disease\n134 diagnosed", maxoecataract="Cataract\n57 diagnosed", maxoedr="DR\n46 diagnosed", maxoeglcsuspectorglc="Glaucoma\n34 diagnosed", maxoeamd="AMD\n19 diagnosed")
screenlabs <- c(maxanyfailcd="Any test\n189 failed", maxvafail="VA\n98 failed", maxiopfail="IOP\n25 failed", maxphotocdfail="Photo\n114 failed")
sspvplot <- ggplot(data = sspvplotdata, aes(x=stat, y=est, group=stat, fill=stat, color=stat)) +
  geom_bar(stat = "identity", position = "dodge", color=NA, alpha = 0.7) +
  geom_errorbar(aes(ymin=low95, ymax=up95), width=0.2, size=(0.5/2.141959), position = position_dodge(0.9)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), expand = c(0, 0), name ="Percent", limits=c(0,1)) +
  xlab("Statistic") +
  # scale_color_viridis(discrete=TRUE, option="inferno") + # options: magma, inferno, plasma, viridis
  # scale_fill_viridis(discrete=TRUE, option="inferno") +
  # scale_color_brewer(palette = "Paired") + # the color-blind friendly are: Dark2, Paired, Set2
  # scale_fill_brewer(palette = "Paired") +
  scale_fill_manual(values=c("#A6CEE3", "#1F78B4", "#33A02C", "#B2DF8A")) +
  scale_color_manual(values=c("#A6CEE3", "#1F78B4", "#33A02C", "#B2DF8A")) +
  facet_grid(exam ~ screen, labeller=labeller(exam=examlabs, screen=screenlabs)) +
  theme(axis.line.y = element_line(colour = "black", size = (0.5/2.141959)),
        axis.line.x = element_line(colour = "black", size = (0.5/2.141959)),
        axis.text.y = element_text(color = "black", size = 8),
        axis.text.x= element_text(color="black", size=8, angle=45, hjust = 1),
        axis.title = element_text(color="black", size=9),
        axis.ticks = element_line(colour = "black", size = (0.5/2.141959)),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        # legend.title=element_text(size=8), 
        # legend.text=element_text(size=8),
        legend.position="none",
        strip.text = element_text(size=8), # face="bold"
        panel.spacing.y = unit(1, "lines"), # Otherwise labels overlap
        strip.background = element_rect(colour=NA, fill=NA))
sspvplot
ggsave("sspvplot.svg", sspvplot, width = 3.4, height = 6, units = c("in"), device="svg")

# Try supplemental figure --eye level
addmargins(xtabs(data=csdatalong, ~maxanyfailcd + randomization_model, addNA=TRUE)) # This for screened population
# So just double the previous
# csdatalong.eyeexam.weights.eyelevel <- csdatalong.eyeexam %>%
#   mutate(pw=case_when(maxanyfailcd==1 ~ 1,
#                       maxanyfailcd==0 ~ 1/(152/1320),
#                       TRUE ~ NA_real_),
#          fpc=case_when(maxanyfailcd==1 ~ 458,
#                        maxanyfailcd==0 ~ 1320,
#                        TRUE ~ NA_real_))


surv.eyelevel <- svydesign(id=~1,strata=~maxanyfailcd, weights=~pw, data=csdatapersonexamined.eye, fpc=~fpc)
# Function to get sens/spec/ppv/npv at eye level
sspvfx.eye <- function(screenresult, examresult) {
  screenresult_mod <- enquo(screenresult)
  examresult_mod <- enquo(examresult)
  # Need to first use this quo_name to put the enquoted variables into an actual formula; then pass the formula to the function
  # When I tried to just use the quo_name in the function, I couldn't get it to work 
  ssf1 <- formula(paste0("~", quo_name(screenresult_mod)))
  ssf2 <- formula(paste0("~I(", quo_name(examresult_mod), "==1)"))
  ssnos <- svyby(ssf1,ssf2,design=surv.eyelevel, unwtd.count) %>% 
    rename(exampos=1)
  ss <- svyby(ssf1,ssf2,design=surv.eyelevel, svyciprop, vartype="ci") %>% 
    rename(exampos=1) %>%
    mutate(est=if_else(exampos==FALSE, 1-(!!screenresult_mod), !!screenresult_mod),
           low95=if_else(exampos==FALSE, 1-ci_u, ci_l),
           up95=if_else(exampos==FALSE, 1-ci_l, ci_u),
           stat=if_else(exampos==FALSE, "spec", "sens")) %>%
    left_join(., ssnos, by="exampos") %>%
    select(stat, est, low95, up95, counts)
  pvf1 <- formula(paste0("~", quo_name(examresult_mod)))
  pvf2 <- formula(paste0("~I(", quo_name(screenresult_mod), "==1)"))
  pvnos <- svyby(pvf1,pvf2,design=surv.eyelevel, unwtd.count) %>% 
    rename(screenpos=1)
  pv <- svyby(pvf1,pvf2,design=surv.eyelevel, svyciprop, vartype="ci") %>% 
    rename(screenpos=1) %>%
    mutate(est=if_else(screenpos==FALSE, 1-(!!examresult_mod), !!examresult_mod),
           low95=if_else(screenpos==FALSE, 1-ci_u, ci_l),
           up95=if_else(screenpos==FALSE, 1-ci_l, ci_u),
           stat=if_else(screenpos==FALSE, "npv", "ppv")) %>%
    left_join(., pvnos, by="screenpos") %>%
    select(stat, est, low95, up95, counts) 
  sspv <- bind_rows(ss, pv) %>%
    mutate(exam=quo_name(examresult_mod),
           screen=quo_name(screenresult_mod))
  newname <- paste0(quo_name(screenresult_mod), ".", quo_name(examresult_mod))
  assign(newname, sspv, envir=.GlobalEnv)
}

examvars.eye <- vars(oe_anyamd, oe_anydr, oe_anyglcsuspectglc, oe_anyamddrglcsuspglc)
screenvars.eye <- vars(screenfail_photoposcd, screenfail_photopos, 
                       screenfail_abnldisccd, screenfail_abnldisc, 
                       screenfail_vcdrcd, screenfail_vcdr, 
                       screenfail_drcd, screenfail_dr, 
                       screenfail_amdcd, screenfail_amd,
                       screenfail_other)
screenvars.eye2 <- vars(screenfail_photocdonly, screenfail_photopos, 
                        screenfail_photo_abdisccdonly, screenfail_abnldisc, 
                        screenfail_vcdrcdonly, screenfail_vcdr, 
                        screenfail_drcdonly, screenfail_dr, 
                        screenfail_amdcdonly, screenfail_amd,
                        screenfail_other)
screenexamcdf.eye <- cross_df(list(screenvars.eye=screenvars.eye, examvars.eye=examvars.eye))
screenexamcdf.eye2 <- cross_df(list(screenvars.eye2=screenvars.eye2, examvars.eye=examvars.eye))
sspvdata.eye <- map2_dfr(screenexamcdf.eye$screenvars.eye, screenexamcdf.eye$examvars.eye, sspvfx.eye)
sspvdata.eye2 <- map2_dfr(screenexamcdf.eye2$screenvars.eye2, screenexamcdf.eye2$examvars.eye, sspvfx.eye)
sspvplotdata.eye <- sspvdata.eye %>%
  mutate(exam=factor(exam, levels=c("oe_anyamddrglcsuspglc", "oe_anydr", "oe_anyglcsuspectglc","oe_anyamd")),
         screen=factor(screen, levels=c("screenfail_photoposcd", "screenfail_photopos", 
                                        "screenfail_drcd", "screenfail_dr", 
                                        "screenfail_abnldisccd", "screenfail_abnldisc", 
                                        "screenfail_vcdrcd", "screenfail_vcdr", 
                                        "screenfail_amdcd", "screenfail_amd",
                                        "screenfail_other")),
         stat=case_when(stat=="sens" ~ "Sens",
                        stat=="spec" ~ "Spec",
                        stat=="ppv" ~ "PPV",
                        stat=="npv" ~ "NPV",
                        TRUE ~ NA_character_),
         screendisease=factor(case_when(screen %in% c("screenfail_photopos", "screenfail_photoposcd") ~ "Any abnormality",
                                        screen %in% c("screenfail_dr", "screenfail_drcd") ~ "DR",
                                        screen %in% c("screenfail_abnldisc", "screenfail_abnldisccd") ~ "Disk abnormality",
                                        screen %in% c("screenfail_vcdr", "screenfail_vcdrcd") ~ "VCDR",
                                        screen %in% c("screenfail_amd", "screenfail_amdcd") ~ "AMD",
                                        screen %in% c("screenfail_other") ~ "Other",
                                        TRUE ~ NA_character_), levels=c("Any abnormality","DR", "Disk abnormality","VCDR","AMD", "Other")))
screenlabs.eye <- c(screenfail_photopos="Ungradables\nnot referred\n133 test +", screenfail_photoposcd="Ungradables\nreferred\n176 test +", 
                    screenfail_dr="Ungradables\nnot referred\n65 test +", screenfail_drcd="Ungradables\nreferred\n100 test +",
                    screenfail_abnldisc="Ungradables\nnot referred\n17 test +", screenfail_abnldisccd="Ungradables\nreferred\n41 test +",
                    screenfail_vcdr="Ungradables\nnot referred\n30 test +", screenfail_vcdrcd="Ungradables\nreferred\n61 test +",
                    screenfail_amd="Ungradables\nnot referred\n15 test +", screenfail_amdcd="Ungradables\nreferred\n53 test +",
                    screenfail_other="Ungradables\nnot referred\n23 test +")

sspvplotdata.eye2 <- sspvdata.eye2 %>%
  mutate(exam=factor(exam, levels=c("oe_anyamddrglcsuspglc",  "oe_anydr", "oe_anyglcsuspectglc", "oe_anyamd")),
         screen=factor(screen, levels=c("screenfail_photopos", "screenfail_photocdonly", 
                                        "screenfail_dr", "screenfail_drcdonly", 
                                        "screenfail_abnldisc", "screenfail_photo_abdisccdonly", 
                                        "screenfail_vcdr", "screenfail_vcdrcdonly",  
                                        "screenfail_amd", "screenfail_amdcdonly", 
                                        "screenfail_other")),
         stat=case_when(stat=="sens" ~ "Sens",
                        stat=="spec" ~ "Spec",
                        stat=="ppv" ~ "PPV",
                        stat=="npv" ~ "NPV",
                        TRUE ~ NA_character_),
         screendisease=factor(case_when(screen %in% c("screenfail_photopos", "screenfail_photocdonly") ~ "Any abnormality",
                                 screen %in% c("screenfail_dr", "screenfail_drcdonly") ~ "DR",
                                 screen %in% c("screenfail_abnldisc", "screenfail_photo_abdisccdonly") ~ "Disk abnormality",
                                 screen %in% c("screenfail_vcdr", "screenfail_vcdrcdonly") ~ "VCDR",
                                 screen %in% c("screenfail_amd", "screenfail_amdcdonly") ~ "AMD",
                                 screen %in% c("screenfail_other") ~ "Other",
                                 TRUE ~ NA_character_), levels=c("Any abnormality","DR", "Disk abnormality","VCDR","AMD", "Other")),
         cdonly=if_else(grepl("cdonly", screen),"Ungradable","Abnormal")) %>%
  filter(stat=="PPV") %>%
  filter(screendisease != "Other")
sspvplotnums.eye <- sspvplotdata.eye %>%
  ungroup() %>%
  mutate(senscountwork=if_else(stat=="Sens", counts, NA_real_),
         ppvcountwork=if_else(stat=="PPV", counts, NA_real_),
         speccountwork=if_else(stat=="Spec", counts, NA_real_),
         npvcountwork=if_else(stat=="PPV", counts, NA_real_)) %>%
  group_by(screendisease, screen, exam) %>%
  summarize(examnum=max(senscountwork, na.rm=TRUE),
            screennum=max(ppvcountwork, na.rm=TRUE),
            examtotal=max(senscountwork, na.rm=TRUE)+max(speccountwork, na.rm=TRUE))
examlabs.eye <- c(oe_anyamddrglcsuspglc="DR/Glaucoma/AMD\n152 diagnosed", oe_anydr="DR\n82 diagnosed", oe_anyglcsuspectglc="Glaucoma\n59 diagnosed", oe_anyamd="AMD\n32 diagnosed")
examlabs.eye2 <- c(oe_anyamddrglcsuspglc="DR/Glaucoma/AMD", oe_anydr="DR", oe_anyglcsuspectglc="Glaucoma", oe_anyamd="AMD")

screenlabs.eye2 <- c(screenfail_photopos="Gradable\n133 failed", screenfail_photocdonly="Ungradable\n17 failed", 
                    screenfail_dr="Gradable\n65 failed", screenfail_drcdonly="Ungradable\n35 failed",
                    screenfail_abnldisc="Gradable\n17 failed", screenfail_photo_abdisccdonly="Ungradable\n24 failed",
                    screenfail_vcdr="Gradable\n30 failed", screenfail_vcdrcdonly="Ungradable\n31 failed",
                    screenfail_amd="Gradable\n15 failed", screenfail_amdcdonly="Ungradable\n38 failed")
screendiseaselabs.eye2 <- c("Any abnormality"="Any\nN=133/17", 
                            "DR"="DR\nN=65/35", 
                            "Disk abnormality"="Disk\nN=17/24", 
                            "VCDR"="VCDR\nN=30/31", 
                            "AMD"="AMD\nN=15/38")

sspvplot.eye <- ggplot(data = sspvplotdata.eye, aes(x=stat, y=est, group=stat, fill=stat, color=stat)) +
  geom_bar(stat = "identity", position = "dodge", color=NA, alpha = 0.7) +
  geom_errorbar(aes(ymin=low95, ymax=up95), width=0.2, size=(0.5/2.141959), position = position_dodge(0.9)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), expand = c(0, 0), name ="Percent", limits=c(0,1)) +
  xlab("Statistic") +
  scale_fill_manual(values=c("#A6CEE3", "#1F78B4", "#33A02C", "#B2DF8A")) +
  scale_color_manual(values=c("#A6CEE3", "#1F78B4", "#33A02C", "#B2DF8A")) +
  facet_grid(exam ~ screendisease + screen, labeller=labeller(exam=examlabs.eye, screen=screenlabs.eye)) +
  theme(axis.line.y = element_line(colour = "black", size = (0.5/2.141959)),
        axis.line.x = element_line(colour = "black", size = (0.5/2.141959)),
        axis.text.y = element_text(color = "black", size = 8),
        axis.text.x= element_text(color="black", size=8, angle=45, hjust = 1),
        axis.title = element_text(color="black", size=9),
        axis.ticks = element_line(colour = "black", size = (0.5/2.141959)),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        # panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.position="none",
        strip.text = element_text(size=8, face="italic"), # face="bold"
        panel.spacing.y = unit(1, "lines"), # Otherwise labels overlap
        strip.background = element_rect(colour=NA, fill=NA))
sspvplot.eye
ggsave("sspvplot.eye.svg", sspvplot.eye, width = 10, height = 6, units = c("in"), device="svg")


sspvplot.eye2 <- ggplot(data = sspvplotdata.eye2, aes(x=cdonly, y=est, group=cdonly)) +
  geom_bar(stat = "identity", position = "dodge", fill="#1F78B4", color=NA, alpha = 0.7) +
  geom_errorbar(aes(ymin=low95, ymax=up95), width=0.2, size=(0.5/2.141959), position = position_dodge(0.9), color="#1F78B4",) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), expand = c(0, 0), name ="Positive Predictive Value", limits=c(0,1)) +
  xlab("Test-positive group") +
  facet_grid(exam ~ screendisease, labeller=labeller(exam=examlabs.eye2, screendisease=screendiseaselabs.eye2)) +
  theme(axis.line.y = element_line(colour = "black", size = (0.5/2.141959)),
        axis.line.x = element_line(colour = "black", size = (0.5/2.141959)),
        axis.text.y = element_text(color = "black", size = 8),
        axis.text.x= element_text(color="black", size=8, angle=45, hjust = 1),
        axis.title = element_text(color="black", size=9),
        axis.ticks = element_line(colour = "black", size = (0.5/2.141959)),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.position="none",
        strip.text = element_text(size=8), # face="bold"
        panel.spacing.y = unit(1, "lines"), # Otherwise labels overlap
        strip.background = element_rect(colour=NA, fill=NA))
sspvplot.eye2
ggsave("sspvplot.eye2.svg", sspvplot.eye2, width = 5, height = 6, units = c("in"), device="svg")

# TEXT: How many patients had ungradable images in at least 1 versus gradable images for all 4.
library(janitor)
# csdatalong %>% 
#   filter(eye=="re") %>% filter(maxanyfailcd==1 | (maxanyfailcd==0 & randomization_model=="Refer")) %>%
#   tabyl(maxphotocdany,maxphotocdfail) # Numbers are correct
csdatalong %>%
  filter(eye=="re") %>% 
  filter(maxanyfailcd==1 | (maxanyfailcd==0 & randomization_model=="Refer")) %>%
  filter(maxphotocdany==1 & maxphotocdfail==1) %>% # Just move around these 1's to get the numbers
  tabyl(maxvafail) 






screenexamcdf <- cross_df(list(screenvars=screenvars, examvars=examvars))
# Just for record-keeping/possible future use, this works for a single variable
# a <- map(examvars, ~sspvfx(maxanyfailcd, !!.x))
# aa <- bind_rows(a, .id="label")
sspvdata <- map2_dfr(screenexamcdf$screenvars, screenexamcdf$examvars, sspvfx)


sspvplotdata <- sspvdata %>%
  mutate(exam=factor(exam, levels=c("maxoeamddrglccat", "maxoecataract", "maxoedr", "maxoeglcsuspectorglc", "maxoeamd")),
         screen=factor(screen, levels=c("maxanyfailcd", "maxvafail", "maxiopfail", "maxphotocdfail")),
         stat=case_when(stat=="sens" ~ "Sens",
                        stat=="spec" ~ "Spec",
                        stat=="ppv" ~ "PPV",
                        stat=="npv" ~ "NPV",
                        TRUE ~ NA_character_))







xtabs(data=csdatalong.eyeexam.weights, ~maxanyfailcd+maxoedr)
# For PPV
a <- svyby(~maxoedr,~I(maxanyfailcd==1),design=surv2, svyciprop, vartype="ci")
# Note this svyby gives PPV (under TRUE) and NPV (if take 1 minus FALSE)


surv3 <- svydesign(id=~1,strata=~maxanyfailcd, weights=~pw, subset=~I(in.subcohort | rel), data=csdatalong.eyeexam.weights, fpc=~fpc)

surv3 <- as.svydesign2(subset(surv2, maxoedr==0))
senssvydata <- svydesign(paste0(  id=~1,strata=~examresult_mod, weights=~pw, data=csdatalong.eyeexam.weights, fpc=~fpc
                                  ))

drexam <- function(screenresult, examresult) {
  screenresult_mod <- enquo(screenresult)
  # npv <- svyciprop(~I(maxoedr==0), subset(surv2, maxanyfailcd==0), method="xlogit")
  # ppv <- svyciprop(~I(maxoedr==1), subset(surv2, maxanyfailcd==1), method="xlogit")
  sensformula <- formula(paste0("~I(", quo_name(screenresult_mod), ")==1"))
  sens <- svyciprop(sensformula, subset(surv2, eval(examresult)>=1), method="xlogit")
  # specformula <- formula(paste0("~I(", quo_name(screenresult_mod), ")==0"))
  # spec <- svyciprop(specformula, subset(surv2, maxoedr==0), method="xlogit")
  # as.vector(sens)
  # attr(spec, "ci")
  data.frame(spec=as.vector(spec), as.list(attr(spec, "ci")), sens=as.vector(sens), as.list(attr(sens, "ci")), stat="dr")
}
drexam(maxanyfailcd, maxoedr)

specdata <- paste0("subset(surv2, (", "examresult", ")==0)")

subset = rlang::eval_tidy( expr( !!subpop_quo == i), data =  data) )

subset(surv2, examresult ==0)

# So for the predictive values only need a single survey object for PPVs and another for NPVs. 
# But for 
npv.anyfail.dr.svy <- svyglm(abs(maxoedr-1) ~ 1, nonefailcd.surv, family=binomial())
npv.anyfail.dr.svy.predict <- predict(npv.anyfail.dr.svy, data.frame(examresult=1), type="response")
confint(npv.anyfail.dr.svy.predict)

confint(npv.anyfail.dr.svy)

sens.anyfail.dr.svy <- svyglm(maxanyfailcd ~ 1, dr.yes.surv.anyfail))


xtabs(data=csdatalong.eyeexam.weights, ~maxanyfailcd+maxoedr)
npvmod <- lm(abs(maxoedr-1) ~ 1, data = filter(csdatalong.eyeexam.weights, maxanyfailcd == 0))
summary(npvmod)
npvmod2 <- glm(abs(maxoedr-1) ~ 1, data = filter(csdatalong.eyeexam.weights, maxanyfailcd == 0))

ppvfx(csdatalong.eyeexam.weights, maxanyfailcd, maxoedr)
ppvfx2(csdatalong.eyeexam.weights, maxanyfailcd, maxoedr)
# First without weights:
xtabs(data=filter(csdatalong2, maxoedr==1), ~maxanyfail)
sens.anyfail.dr = lm(maxanyfail ~ 1, data=filter(csdatalong2, maxoedr==1))
summary(sens.anyfail.dr)
# With weights
dr.yes.surv.anyfail <- svydesign(id=~1,strata=~maxanyfail, weights=~pw, data=filter(csdatalong2, maxoedr==1), fpc=~fpc)
(sens.anyfail.dr.svy <- svyglm(maxanyfail ~ 1, dr.yes.surv.anyfail))
# It's a lower estimate with the sampling weights. Only 71% compared with 96% if no weights. That seems about right. 


# PPV of any failed test
ppvfx(csdatalong.eyeexam.weights, maxanyfailcd, maxoedr)
ppvfx(csdatalong.eyeexam.weights, maxanyfailcd, maxoeamd)
ppvfx(csdatalong.eyeexam.weights, maxanyfailcd, maxoeglc)
ppvfx(csdatalong.eyeexam.weights, maxanyfailcd, maxoeglcsuspect)
# PPV of failed photo (CD)
ppvfx(csdatalong.eyeexam.weights, maxphotocdfail, maxoedr)
ppvfx(csdatalong.eyeexam.weights, maxphotocdfail, maxoeamd)
ppvfx(csdatalong.eyeexam.weights, maxphotocdfail, maxoeglc)
ppvfx(csdatalong.eyeexam.weights, maxphotocdfail, maxoeglcsuspect)
# PPV of failed photo (not CD)
ppvfx(csdatalong.eyeexam.weights, maxphotofail, maxoedr)
ppvfx(csdatalong.eyeexam.weights, maxphotofail, maxoeamd)
ppvfx(csdatalong.eyeexam.weights, maxphotofail, maxoeglc)
ppvfx(csdatalong.eyeexam.weights, maxphotofail, maxoeglcsuspect)
# Should probably calculate separately just for CD ones maybe?

%>% select(!!screenresult_mod, !!examresult_mod)

ppv.anyfail.cataract = lm(maxoecataract ~ 1, data=filter(csdatalong.eyeexam.weights, maxanyfailcd==1))
summary(ppv.anyfail.cataract)



# Now with weights (it's the same, as it should be):
anyfail.surv <- svydesign(id=~1,strata=~maxanyfail, weights=~pw, data=filter(csdatalong2, maxanyfail==1), fpc=~fpc)
(ppv.anyfail.cataract.svy <- svyglm(maxoecataract ~ 1, anyfail.surv))
# And now the PPV of any failed test for DR:
# First without weights:
xtabs(data=filter(csdatalong2, maxanyfail==1), ~maxoedr)
ppv.anyfail.dr = lm(maxoedr ~ 1, data=filter(csdatalong2, maxanyfail==1))
summary(ppv.anyfail.dr)
# Now with weights (it's the same, as it should be, since this is a patient level analysis.):
(ppv.anyfail.dr.svy <- svyglm(maxoedr ~ 1, anyfail.surv))
# What about sensitivity?

# First without weights:
xtabs(data=filter(csdatalong2, maxoedr==1), ~maxanyfail)
sens.anyfail.dr = lm(maxanyfail ~ 1, data=filter(csdatalong2, maxoedr==1))
summary(sens.anyfail.dr)
# With weights
dr.yes.surv.anyfail <- svydesign(id=~1,strata=~maxanyfail, weights=~pw, data=filter(csdatalong2, maxoedr==1), fpc=~fpc)
(sens.anyfail.dr.svy <- svyglm(maxanyfail ~ 1, dr.yes.surv.anyfail))
# It's a lower estimate with the sampling weights. Only 71% compared with 96% if no weights. That seems about right. 







t5.va.ppv.patient <- csdatalong.eyeexam.weights %>%
  group_by(clinic_attended_v2, maxanyfailcd) %>%

addmargins(xtabs(data=csdatalong, ~screenfail_va+clinic_attended_v2, addNA=TRUE))
t5.va.ppv.eye <- csdatalong %>%
  filter(consent==1 & screenfail_va==1 & !is.na(oe_iop)) %>%
  filter(maxanyfail==1 | (maxanyfail==0 & randomization_model %in% "Refer")) %>%
  filter(!(study_id==3867078 & eye=="le")) %>% # This is the prosthetic eye
  ungroup() %>%
  # group_by(clinic_attended_v2) %>%
  summarize(TOTALFAIL=sum(!is.na(screenfail_va)),
            TOTALEXAMINED=sum(!is.na(oe_iop)),
            ppv_cataract_num=sum(oe_tx_catsurgnotmiss==1, na.rm=TRUE),
            ppv_cataract_total=sum(!is.na(oe_tx_catsurgnotmiss==1)),
            ppv_cataract_prop=ppv_cataract_num/ppv_cataract_total,
            ppv_dr_num=sum(oe_anydr==1, na.rm=TRUE),
            ppv_dr_total=sum(!is.na(oe_anydr==1)),
            ppv_dr_prop=ppv_dr_num/ppv_dr_total,
            ppv_amd_num=sum(oe_anyamd==1, na.rm=TRUE),
            ppv_amd_total=sum(!is.na(oe_anyamd==1)),
            ppv_amd_prop=ppv_amd_num/ppv_amd_total)
t5.va.npv.eye <- csdatalong %>%
  filter(consent==1 & screenfail_va==0 & !is.na(oe_iop)) %>%
  filter(maxanyfail==1 | (maxanyfail==0 & randomization_model %in% "Refer")) %>%
  filter(!(study_id==3867078 & eye=="le")) %>% # This is the prosthetic eye
  ungroup() %>%
  # group_by(clinic_attended_v2) %>%
  summarize(TOTALNOTFAIL=sum(!is.na(screenfail_va)),
            TOTALEXAMINED=sum(!is.na(oe_iop)),
            npv_cataract_num=sum(oe_tx_catsurgnotmiss==0, na.rm=TRUE),
            npv_cataract_total=sum(!is.na(oe_tx_catsurgnotmiss==0)),
            npv_cataract_prop=npv_cataract_num/npv_cataract_total,
            npv_dr_num=sum(oe_anydr==0, na.rm=TRUE),
            npv_dr_total=sum(!is.na(oe_anydr==0)),
            npv_dr_prop=npv_dr_num/npv_dr_total,
            npv_amd_num=sum(oe_anyamd==0, na.rm=TRUE),
            npv_amd_total=sum(!is.na(oe_anyamd==0)),
            npv_amd_prop=npv_amd_num/npv_amd_total)

# PATIENT LEVEL
# OK I think that the survey design weights would be 1/229 for the anyfail group and 1/660 for the nonefailed group.
library(survey)
data(api)
xtabs(data=apistrat, ~pw+fpc)
# So fpc needs to be 229 and 660
# and weights need to be 1 [1/(229/229)] and 8.91 [1/(74/660)]
# Though this does not account for non-response weights
# https://bookdown.org/jespasareig/Book_How_to_weight_a_survey/nonresponse-weights.html
# Note I am basing this on the numbers failing/not failing and not the numbers presenting for exam. I am not sure which is correct.
csdatalong2 <- csdatalong %>%
  filter(maxanyfail==1 | (maxanyfail==0 & randomization_model %in% "Refer")) %>%
  filter(consent==1 & eye=="re" & !is.na(maxoeiop)) %>%
  filter(!(study_id==3867078 & eye=="le")) %>% # This is the prosthetic eye
  mutate(pw=case_when(maxanyfail==1 ~ 1,
                      maxanyfail==0 ~ 1/(74/660),
                      TRUE ~ NA_real_),
         fpc=case_when(maxanyfail==1 ~ 229,
                      maxanyfail==0 ~ 660,
                      TRUE ~ NA_real_))
# So then the PPV of any failed test for cataract:
# First without weights:
xtabs(data=filter(csdatalong2, maxanyfail==1), ~maxoecataract)
ppv.anyfail.cataract = lm(maxoecataract ~ 1, data=filter(csdatalong2, maxanyfail==1))
summary(ppv.anyfail.cataract)
# Now with weights (it's the same, as it should be):
anyfail.surv <- svydesign(id=~1,strata=~maxanyfail, weights=~pw, data=filter(csdatalong2, maxanyfail==1), fpc=~fpc)
(ppv.anyfail.cataract.svy <- svyglm(maxoecataract ~ 1, anyfail.surv))
# And now the PPV of any failed test for DR:
# First without weights:
xtabs(data=filter(csdatalong2, maxanyfail==1), ~maxoedr)
ppv.anyfail.dr = lm(maxoedr ~ 1, data=filter(csdatalong2, maxanyfail==1))
summary(ppv.anyfail.dr)
# Now with weights (it's the same, as it should be, since this is a patient level analysis.):
(ppv.anyfail.dr.svy <- svyglm(maxoedr ~ 1, anyfail.surv))
# What about sensitivity?

# First without weights:
xtabs(data=filter(csdatalong2, maxoedr==1), ~maxanyfail)
sens.anyfail.dr = lm(maxanyfail ~ 1, data=filter(csdatalong2, maxoedr==1))
summary(sens.anyfail.dr)
# With weights
dr.yes.surv.anyfail <- svydesign(id=~1,strata=~maxanyfail, weights=~pw, data=filter(csdatalong2, maxoedr==1), fpc=~fpc)
(sens.anyfail.dr.svy <- svyglm(maxanyfail ~ 1, dr.yes.surv.anyfail))
# It's a lower estimate with the sampling weights. Only 71% compared with 96% if no weights. That seems about right. 

t5.va.ppv.pt <- csdatalong %>%
  filter(maxvafail==1) %>%
  ungroup() %>%
  # group_by(clinic_attended_v2) %>%
  summarize(ppv_cataract_num=sum(maxoecataract==1, na.rm=TRUE),
            ppv_cataract_total=sum(!is.na(maxoecataract==1)),
            ppv_cataract_prop=ppv_cataract_num/ppv_cataract_total,
            ppv_dr_num=sum(maxoedr==1, na.rm=TRUE),
            ppv_dr_total=sum(!is.na(maxoedr==1)),
            ppv_dr_prop=ppv_dr_num/ppv_dr_total,
            ppv_amd_num=sum(maxoeamd==1, na.rm=TRUE),
            ppv_amd_total=sum(!is.na(maxoeamd==1)),
            ppv_amd_prop=ppv_amd_num/ppv_amd_total)
t5.va.npv.pt <- csdatalong %>%
  filter(maxanyfail==1 | (maxanyfail==0 & randomization_model %in% "Refer")) %>%
  filter(consent==1 & eye=="re" & !is.na(maxoeiop)) %>%
  filter(!(study_id==3867078 & eye=="le")) %>% # This is the prosthetic eye
  filter(maxvafail==0) %>%
  group_by(maxanyfail) %>%
  # group_by(clinic_attended_v2) %>%
  summarize(ppv_cataract_num=sum(maxoecataract==0, na.rm=TRUE),
            ppv_cataract_total=sum(!is.na(maxoecataract)),
            ppv_cataract_prop=ppv_cataract_num/ppv_cataract_total,
            ppv_dr_num=sum(maxoedr==0, na.rm=TRUE),
            ppv_dr_total=sum(!is.na(maxoedr)),
            ppv_dr_prop=ppv_dr_num/ppv_dr_total,
            ppv_amd_num=sum(maxoeamd==0, na.rm=TRUE),
            ppv_amd_total=sum(!is.na(maxoeamd)),
            ppv_amd_prop=ppv_amd_num/ppv_amd_total)

ts <- csdatalong %>%
  filter(consent==1  & !is.na(maxoeiop)) %>%
  filter(!(study_id==3867078 & eye=="le")) %>% # This is the prosthetic eye
  filter(maxvafail==0)

tsscreenfail <- csdatalong %>%
  filter(is.na(screenfail_anycd))

addmargins(xtabs(data=filter(csdatalong, consent==1), ~ screenfail_anycd, addNA=TRUE))

#72 eyes with missing data 

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

## DATA CLEANING FOR BLAKE...
# Investigate the missing data--try to find missing data. For photos, can at least change to cannot determine...
missing.screening.data <- csdatalong %>%
  filter(is.na(screenfail_any) & consent==1) %>%
  select(study_id, eye, screenfail_va, screenfail_iop, screenfail_abnldisc, screenfail_vcdr, screenfail_dr, screenfail_amd)
missingglcdx.data <- csdatalong %>%
  filter(is.na(oe_anyglc) & !is.na(oe_anyglcsuspect))

View(missing.screening.data)

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



