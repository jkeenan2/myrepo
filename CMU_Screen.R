library(readr)
library(readxl)
library(tidyverse)
# library(rlang)
library(skimr)
# library(irr)
# install.packages(lpSolve)
# library(lpSolve)
library(lubridate)


costscreened_redcapexport <- read_csv("CostSCREENED_DATA_2020-04-29_0845.csv")
glimpse(costscreened_redcapexport)
# Check out different types of records
xtabs(data=costscreened_redcapexport , ~redcap_event_name+clinic_attended_v2, addNA=TRUE) # I guess we only want screening visit
# Who to include? Consented? At clinic?
xtabs(data=costscreened_redcapexport, ~clinic_attended_v2+consent, addNA=TRUE)


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
  select(study_id:examiner_v2, end_time_ying_screen:examiner, dilated:eyeart_screen_outcome, eyeart_flag:oe_examiner, oe_tx_none:vf_testing_complete, vf_further_tx:eyeart_screen_outcome_oephoto, eyeart_result_dilate_opexam_complete,time_start_exam_or_pecen, this_form_refer_calc_pecen:pecen_review_screening_complete, this_form_refer_calc_pecen_v2:pecen_review_dilated_complete, everything()) # to find duplicate column names: select(csdata)
csdatawide.addresses <- csdatawide %>%
  select(study_id, address, current_address_provinc, current_address_amphoe,current_address_tambon)
write_csv(csdatawide.addresses, "csdatawide.addresses.csv")

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
         oe_anyglc=if_else(oe_tx_missing %in% 0,oe_anyglcfalse,0),
         oe_anyglcsuspect=if_else(is.na(oe_vcdr) & is.na(oe_iop), NA_real_, 
                          if_else((!is.na(oe_vcdr) & oe_vcdr>0.6) | oe_iop>22,1,0)), # xtabs(data=filter(csdatalong, is.na(oe_anyglcsuspect) & oe_anyglcfalse==0), ~study_id)
         oe_anyamddrglc=if_else(is.na(oe_anyamd) & is.na(oe_anydr) & is.na(oe_anydr),NA_real_,
                        if_else(oe_anyamd==1 | oe_anydr==1 | oe_anyglc==1,1,0)),
         oe_tx_catsurgnotmiss =if_else(oe_tx_missing==1, NA_real_, 
                               if_else(oe_tx_catsurg==1, 1, 0)),
         oe_tx_vftestnotmiss =if_else(oe_tx_missing==1, NA_real_, 
                               if_else(oe_tx_vftest==1, 1, 0)),
         oe_tx_rectxnotmiss =if_else(oe_tx_missing==1, NA_real_, 
                               if_else(oe_tx_rectx==1, 1, 0)),
         sumoedx=oe_anyamd+oe_anydr+oe_anyglc+oe_anyglcsuspect+oe_anyamddrglc+oe_tx_catsurgnotmiss+oe_tx_vftestnotmiss+oe_tx_rectxnotmiss,
         clinic_attended_v2=recode_factor(clinic_attended_v2, '0'="Diabetes", '1'="Thyroid",'2'="General")) %>%
  group_by(study_id) %>% 
  mutate(maxanyfail=max(screenfail_anycd), # So this is seeing whether the entire patient was referred, dichotomous 1/0
         maxvafail=max(screenfail_va),
         maxiopfail=max(screenfail_iop),
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
         maxoeglc=max(oe_anyglcfalse),
         maxoeglcsuspect=max(oe_anyglcsuspect),
         maxoecataract=max(oe_tx_catsurgnotmiss),
         maxoeiop=max(oe_iop, na.rm=TRUE),
         maxoeiop=if_else(maxoeiop==-Inf, NA_real_, maxoeiop),
         shouldcompletegs=ifelse((maxanyfail== 1 | randomization_model == "Refer"), 1, 0),
         accountedfor=ifelse((ophthalmologist_exam_complete ==2 | phone_call_complete==2), 1, 0))

addmargins(xtabs(data=filter(csdatalong, consent==1), ~ screenfail_anycd + refer_patient_randomizatio, addNA=TRUE))

# JK: I figured out why this wasn't working before.
# IOP was a character variable, not numeric.
# So when I tried to do the screenfail_iop variable, 
# it was treating anything whose character was greater than 22 (eg, 3, 8, etc.) as fulfilling the criteria
# So I added the as.numeric() and now it's working.
# The way I troubleshot this was to make some dataframes with the ones that didn't make sense, and then just spot checked


# This for table 2 and text:
patientscreenfails <- csdatalong %>% 
  filter(eye=="re") %>% 
  group_by(clinic_attended_v2) %>%
  summarize(anyfail=sum(maxanyfail==1),
            nonefailrandom=sum(maxanyfail==0 & randomization_model=="Refer", na.rm=TRUE),
            vafail=sum(maxvafail==1), # xtabs(data=filter(csdatalongreferralcheck,is.na(maxvafail)), ~study_id)
                                      # JK cleaned on redcap. Assumed zero letters for pinhole for 3093274 OS.
            iopfail=sum(maxiopfail==1),
            abnldiscfail=sum(maxabnldiscfail==1),
            amdfail=sum(maxamdfail==1),
            drfail=sum(maxdrfail==1),
            vcdrfail=sum(maxvcdrfail==1),
            blurry=sum(maxsxblurry==1),
            floater=sum(maxsxfloater==1),
            flash=sum(maxsxflash==1),
            scotoma=sum(maxsxscotoma==1),
            othersx=sum(maxsxother==1),
            anysx=sum(maxsxany==1),
            vis_sx=sum(maxsxvisualsymptoms==1),
            anysx_total=sum(!is.na(maxsxany)),
            anyfail_completedophthoexam=sum(!is.na(oe_anyamd) & maxanyfail==1),
            nonefail_completedophthoexam=sum(!is.na(oe_anyamd) & maxanyfail==0 & randomization_model=="Refer", na.rm=TRUE))

ts48.nonefail <- csdatalong %>% filter(!is.na(oe_anyamd) & maxanyfail==0 & randomization_model=="Refer") %>% select(starts_with("oe"))
ts229.anyfail <- csdatalong %>% filter(!is.na(oe_anyamd) & maxanyfail==1) %>% select(starts_with("oe"))

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


addmargins(xtabs(data=filter(csdatalongreferralcheck, consent==1), ~ maxanyfail + refer_patient_randomizatio, addNA=TRUE))
#why is the above different then the below??? # because the above is based on person and the below is based on eye ie. some people had one positive eye and one negative eye
addmargins(xtabs(data=filter(csdatalongreferralcheck, consent==1), ~ screenfail_anycd + refer_patient_randomizatio, addNA=TRUE))


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

# Text only: age sex of overall population:
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
# JK: The following makes it easier to read, and could do write_csv and copy/paste in if you wanted.
demographicstablelong <- demographicstable %>%
  gather(field, value, consentyes_num:dm_per) %>%
  separate(field, into=c("variable","stat"), sep="_") %>%
  mutate(clinicstat=paste(clinic_attended_v2, stat, sep="_")) %>%
  select(-clinic_attended_v2, -stat) %>%
  spread(clinicstat, value)
# Note: missing duration of DM for one person in the General clinic:
csdatawide %>% filter(diagnosis_of_diabetes==1 & is.na(years_with_diabetes)) %>% select(study_id)


prostheses <- csdatalong %>%
  filter(grepl("prosthe",notes_for_flag) | grepl("prosthe",oe_majorcause_specother)) %>%
  select(study_id, eye,  va,iop, vcdr, dr, amd, notes_for_flag, oe_majorcause_specother, screenfail_anycd, screenfail_va, randomization_model)
# JK: Not sure but the notes_for_flag seems to be an error since there is screening data for everything
# So will only exclude 3867078 OS
# Table 3
# NEED TO INVESTIGATE THE MISSING VALUES!!
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
            vcdrfail_num=sum(screenfail_vcdr==1, na.rm=TRUE),
            vcdrfail_total=sum(!is.na(screenfail_vcdr)),
            vcdrfail_per=vcdrfail_num/vcdrfail_total,
            amdfail_num=sum(screenfail_amd==1, na.rm=TRUE),
            amdfail_total=sum(!is.na(screenfail_amd)),
            amdfail_per=amdfail_num/amdfail_total,
            drfail_num=sum(screenfail_dr==1, na.rm=TRUE),
            drfail_total=sum(!is.na(screenfail_dr)),
            drfail_per=drfail_num/drfail_total,
            photofail_num=sum(screenfail_abnldisc==1 | screenfail_vcdr==1 | screenfail_amd==1 | screenfail_dr==1),
            photofail_total=sum(!is.na(screenfail_abnldisc) & !is.na(screenfail_vcdr) & !is.na(screenfail_amd) & !is.na(screenfail_dr)),
            photofail_per=photofail_num/photofail_total,
            anyfail_num=sum(screenfail_any==1, na.rm=TRUE),
            anyfail_total=sum(!is.na(screenfail_any)),
            anyfail_per=anyfail_num/anyfail_total) %>%
  gather(field, value, vafail_num:anyfail_per) %>%
  separate(field, into=c("variable","stat"), sep="_") %>%
  mutate(clinicstat=paste(clinic_attended_v2, stat, sep="_")) %>%
  select(-clinic_attended_v2, -stat) %>%
  spread(clinicstat, value)

#Table 4 line by line
addmargins(xtabs(data=filter(csdatalong, consent==1), ~ oe_anyamd + dmclinic, addNA = TRUE))
addmargins(xtabs(data=filter(csdatalong, consent==1), ~ oe_anydr + dmclinic, addNA = TRUE))
addmargins(xtabs(data=filter(csdatalong, consent==1), ~ oe_anyglc + dmclinic, addNA = TRUE))
addmargins(xtabs(data=filter(csdatalong, consent==1), ~ oe_anyglcsuspect + dmclinic, addNA = TRUE))
addmargins(xtabs(data=filter(csdatalong, consent==1), ~ oe_anyamddrglc + dmclinic, addNA = TRUE))
addmargins(xtabs(data=filter(csdatalong, consent==1), ~ oe_tx_vftestnotmiss  + dmclinic, addNA = TRUE))
addmargins(xtabs(data=filter(csdatalong, consent==1), ~ oe_tx_catsurgnotmiss  + dmclinic, addNA = TRUE))
addmargins(xtabs(data=filter(csdatalong, consent==1), ~ oe_tx_rectxnotmiss + dmclinic, addNA = TRUE))

# Text: These give the number of patients and eyes that actually got an ophthalmology exam
xtabs(data=filter(csdatalong, consent==1 & !is.na(oe_anyamd) & eye=="re"), ~clinic_attended_v2,addNA=TRUE)
xtabs(data=filter(csdatalong, consent==1 & !is.na(oe_anyamd)), ~clinic_attended_v2,addNA=TRUE)
# Noticed a missing cataract, looked up on Redcap and patient did not have cataract so OK to just take the numerator:
ts <- csdatalong %>%
  filter(consent==1) %>%
  filter(!(study_id==3867078 & eye=="le")) %>% # This is the prosthetic eye
  filter(maxanyfail==1 | (maxanyfail==0 & randomization_model %in% "Refer")) %>%
  filter(is.na(oe_tx_catsurgnotmiss) & !is.na(oe_iop)) %>%
  select(study_id, eye)

##table 4 wide -- eye level
table4wide <- csdatalong %>%
  filter(consent==1) %>%
  filter(!(study_id==3867078 & eye=="le")) %>% # This is the prosthetic eye
  filter(maxanyfail==1 | (maxanyfail==0 & randomization_model %in% "Refer")) %>%
  group_by(clinic_attended_v2, screenfail_anycd) %>%
  summarize(consent_n=sum(!is.na(consent)),
            oe_anyamd.mean=mean(oe_anyamd, na.rm=TRUE), 
            oe_anyamd.num=sum(oe_anyamd == 1, na.rm=TRUE),
            oe_anyamd.denom=sum(!is.na(oe_anyamd), na.rm=TRUE),
            oe_anydr.mean=mean(oe_anydr, na.rm=TRUE), 
            oe_anydr.num=sum(oe_anydr == 1, na.rm=TRUE),
            oe_anydr.denom=sum(!is.na(oe_anydr), na.rm=TRUE),
            oe_anyglc.mean=mean(oe_anyglcfalse, na.rm=TRUE), 
            oe_anyglc.num=sum(oe_anyglcfalse == 1, na.rm=TRUE),
            oe_anyglc.denom=sum(!is.na(oe_anyglcfalse), na.rm=TRUE),
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

##table 4 wide -- patient level
table4.patient <- csdatalong %>%
  filter(consent==1 & eye=="re" & !is.na(maxoeiop)) %>%
  filter(!(study_id==3867078 & eye=="le")) %>% # This is the prosthetic eye
  filter(maxanyfail==1 | (maxanyfail==0 & randomization_model %in% "Refer")) %>%
  group_by(clinic_attended_v2, maxanyfail) %>%
  summarize(consent_n=sum(consent==1),
            oe_anyamd.num=sum(maxoeamd == 1),
            oe_anyamd.denom=sum(!is.na(maxoeamd)),
            oe_anyamd.prop=oe_anyamd.num/oe_anyamd.denom,
            oe_anydr.num=sum(maxoedr == 1),
            oe_anydr.denom=sum(!is.na(maxoedr)),
            oe_anydr.prop=oe_anydr.num/oe_anydr.denom,
            oe_anyglcsus.num=sum(maxoeglcsuspect == 1),
            oe_anyglcsus.denom=sum(!is.na(maxoeglcsuspect)),
            oe_anyglcsus.prop=oe_anyglcsus.num/oe_anyglcsus.denom,
            oe_anycataract.num=sum(maxoecataract == 1),
            oe_anycataract.denom=sum(!is.na(maxoecataract)),
            oe_anycataract.prop=oe_anycataract.num/oe_anycataract.denom)


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



