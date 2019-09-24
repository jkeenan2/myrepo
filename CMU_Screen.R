library(readr)
library(tidyverse)
library(rlang)
library(skimr)
library(dplyr)
library(rsample)

costscreened_redcapexport <- read_csv("CostSCREENED_DATA_2019-09-24_0706.csv")
glimpse(costscreened_redcapexport)
# Check out different types of records
xtabs(data=costscreened_redcapexport , ~redcap_event_name+clinic_attended_v2, addNA=TRUE) # I guess we only want screening visit
# Who to include? Consented? At clinic?
xtabs(data=costscreened_redcapexport, ~clinic_attended_v2+consent, addNA=TRUE)

costscreened <- costscreened_redcapexport %>%
  mutate(dmclinic=case_when(clinic_attended_v2 == 0 ~ 1,
                            clinic_attended_v2 %in% c(1,2) ~ 0,
                            TRUE ~ NA_real_)) %>%
  mutate(thyroidclinic=case_when(clinic_attended_v2 == 1 ~ 1,
                                 clinic_attended_v2 %in% c(0,2) ~ 0,
                                 TRUE ~ NA_real_)) %>%
  mutate(generalclinic=case_when(clinic_attended_v2 == 2 ~ 1,
                                 clinic_attended_v2 %in% c(0,1) ~ 0,
                                 TRUE ~ NA_real_))

yingdata <- costscreened %>%
  filter(redcap_event_name=="ying_data_entry_arm_1") %>%
  select_if(~sum(!is.na(.)) > 0) %>%
  select(-redcap_event_name)

jeremyreview <- costscreened %>%
  filter(redcap_event_name=="ophthalmologist_re_arm_1") %>%
  select_if(~sum(!is.na(.)) > 0) %>%
  select(-redcap_event_name)

## Added Op review for kappas, 
## What do we want kappas for? Just positive screen?

screeningdata <- costscreened %>%
  filter(redcap_event_name=="screening_visit_arm_1") %>%
  select_if(~sum(!is.na(.)) > 0) %>%
  select(-redcap_event_name)

# We essentially want only 2 main data frames: a wide one and long one.
# I am only including the screeningdata and yingdata dataframes for now

#Import randomization model 

randomization_model <- read_csv("randomization_model_results.csv")

screeningdata2 <- full_join(randomization_model, screeningdata, by="study_id")

#View(screeningdata2)
skim(screeningdata)
skim(screeningdata2)

#Ok, 889 in both so merge worked properly 

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
         screenfail_iop=if_else(is.na(iop), NA_real_, 
                                if_else(iop>22, 1, 0)), # xtabs(data=csdatalong, ~iop+screenfail_iop, addNA=TRUE)
         screenfail_abnldisc=if_else(is.na(photo_abdisc), NA_real_, 
                                     if_else(photo_abdisc==1,1,0)), # xtabs(data=csdatalong, ~photo_abdisc+screenfail_abnldisc, addNA=TRUE)
         screenfail_abnldisccd=if_else(is.na(photo_abdisc), NA_real_, 
                                       if_else(photo_abdisc>=1,1,0)), # xtabs(data=csdatalong, ~photo_abdisc+screenfail_abnldisccd, addNA=TRUE)
         screenfail_vcdr=if_else(is.na(vcdr), NA_real_, 
                                 if_else(vcdr>0.6,1,0)),
         screenfail_vcdrcd=if_else(is.na(photo_abdisc), NA_real_, 
                                       if_else((vcdr>0.6 | vcdr_cnd >1),1,0)),# xtabs(data=csdatalong, ~vcdr+screenfail_vcdr, addNA=TRUE)
         screenfail_programvcdr=if_else(is.na(vcdr_program), NA_real_, 
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
         screenfail_othercd=if_else(is.na(other_referral), NA_real_, 
                                  if_else(other_referral %in% c(1,2),1,0)),
         screenfail_photopos=if_else(is.na(amd) & is.na(dr) & is.na(vcdr) & is.na(photo_abdisc) & is.na(other_referral), NA_real_, 
                                     if_else( (screenfail_amd==1 | screenfail_vcdr==1 | screenfail_dr==1 | other_referral==1 | photo_abdisc==1),1,0)),
         screenfail_photoposcd=if_else(is.na(amd) & is.na(dr) & is.na(vcdr) & is.na(photo_abdisc) & is.na(other_referral), NA_real_, 
                                     if_else( (screenfail_amdcd==1 | screenfail_vcdrcd==1 | screenfail_drcd==1 | screenfail_othercd==1 | screenfail_abnldisccd==1),1,0)),
         screenfail_any=if_else(screenfail_va==1 | screenfail_iop==1 | screenfail_abnldisc==1 | screenfail_vcdr==1 | screenfail_dr==1 | screenfail_amd==1 | screenfail_photopos==1 | screenfail_other==1 , 1, 0),
         screenfail_anycd=if_else(screenfail_va==1 | screenfail_iop==1 | screenfail_abnldisccd==1 | screenfail_vcdr==1 |  screenfail_dr==1 | screenfail_amd==1 | screenfail_other==1 | screenfail_amdcd==1 | screenfail_drcd==1 | screenfail_othercd==1 | screenfail_photopos==1 | screenfail_vcdrcd==1 |screenfail_abnldisccd ==1 | screenfail_photoposcd==1, 1, 0),
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
         sumoedx=oe_anyamd+oe_anydr+oe_anyglc+oe_anyglcsuspect+oe_anyamddrglc+oe_tx_catsurgnotmiss+oe_tx_vftestnotmiss+oe_tx_rectxnotmiss)    

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

#csscreeningexamcomplete <- csdatalong %>%
#  filter(screening_exam_ying_complete == 2)
# csgsexamcomplete <- csdatalong %>%
#     filter(ophthalmologist_exam_complete == 2)
#  
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
# #? Returning too many records. Not sure what is wrong with definition
# 
# csgsts5 <- csgsexamcomplete %>%
#    filter(is.na(oe_vcdr))
# 
# # These cases are true missing. (ie. cannot determine, for example no eye)
# #? How should we define these for analysis?
# 3867078 = prosthetic LE no IOP; 3371230, 3867000 = CND RE VCDR; 3654437, 3744448, 3867000, 3867078 CND VCDR LE
# 
# csgsts6 <- csgsexamcomplete %>%
#    filter(is.na(oe_iop))

# This 1 case are true missing. 3867078 (ie. cannot determine, for example no eye)
# Need to remove patient le from analysis: 3867078, because prosthetic eye
# csgsexamcomplete <- csgsexamcomplete %>%
# filter(study_id != 3867078) - this is worng - removes both re and le

# View(csgsts#)

#   missing.screening.data <- csdatalong %>%
#    filter(is.na(screenfail_any) & consent==1) %>%
#     select(study_id, eye, screenfail_va, screenfail_iop, screenfail_abnldisc, screenfail_vcdr, screenfail_dr, screenfail_amd)
# 
# # This 1 case are true missing. 3355193 (ie. cannot determine, for example no eye)
#  
#   missingglcdx.data <- csdatalong %>%
#     filter(is.na(oe_anyglc) & !is.na(oe_anyglcsuspect))
# 
#   #Double check 
# xtabs(data=csdatalong, ~vcdr+screenfail_vcdr, addNA=TRUE)
# xtabs(data=csdatalong, ~oe_vcdr+oe_anyglc, addNA=TRUE)
# xtabs(data=csdatalong, ~iop, addNA=TRUE)
# xtabs(data=csdatalong, ~oe_iop, addNA=TRUE)
# 
# #Correct AMD, not mutually exclusive
# csdatalong %>% skim(c("oe_amd_none", "oe_amd_cd", "oe_amd_drusen", "oe_amd_ga", "oe_amd_wet"))
 amdexploredata <- csdatalong %>%
   mutate(oe_amdsum=oe_amd_cd+oe_amd_drusen+oe_amd_ga+oe_amd_wet)
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
       group_by(study_id) %>%
       mutate(maxanyfail=max(screenfail_anycd))

# addmargins(xtabs(data=filter(csdatalongreferralcheck, consent==1), ~ maxanyfail + refer_patient_randomizatio, addNA=TRUE))
## Correct definition of maxanyfail, and confirmation that all calcs for refer match those referred 

csdatalongreferralcheck2 <- csdatalong %>%
  group_by(study_id) %>%
  mutate(maxanyfail=max(screenfail_anycd, na.rm=TRUE)) %>%
  mutate(shouldcompletegs=ifelse((maxanyfail== 1 | randomization_model == "Refer"), 1, 0),
         accountedfor=ifelse((ophthalmologist_exam_complete ==2 | phone_call_complete==2), 1, 0))

xtabs(data=csdatalongreferralcheck2, ~accountedfor+shouldcompletegs, addNA=TRUE)

tsgsneeded <- csdatalongreferralcheck2 %>%
  filter(accountedfor == 0 & shouldcompletegs ==1)

# One patient that we did not call for referral, 2210451, 
# I had Ying call and they have follow up in outside clinic, updated in Redcap


shouldcompletegs <- csdatalongreferralcheck %>%
  filter(consent==1)
  filter(maxanyfail== 1 |  randomization_model == "Refer")  
  
accountedfor <- csdatalongreferralcheck %>%
  filter(ophthalmologist_exam_complete ==2 | phone_call_complete==2)

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

# photoagreement <- full_join(csdatawide, jeremyreview, by="study_id") %>%
#   mutate(photoscreenpos=case_when((vcdr_referral.re== 1 | dr_referral.re==1 | amd_referral.re==1 | other_referral.re==1 | vcdr_referral.le==1 | referral_disc_abnormal.le==1 | dr_referral.le==1 | amd_referral.le==1 | other_referral.le==1) ~ 1,
#                                   (vcdr_referral.re== 0 & dr_referral.re==0 & amd_referral.re==0 & other_referral.re==0 & vcdr_referral.le==0 & referral_disc_abnormal.le==0 & dr_referral.le==0 & amd_referral.le==0 & other_referral.le==0) ~ 0,
#                                   TRUE ~ NA_real_)) %>%
#   select(start_time_inclusion, photoscreenpos, this_form_referral) %>%
#   ymd_hms(start_time_inclusion)
# 
# #? unsure why this is lubridate function is not working. 
# #I am trying to separate out the date to give month by month, but will leave out for now
# 
# photoagreement <- full_join(csdatawide, jeremyreview, by="study_id") %>%
#   mutate(photoscreenpos=case_when((vcdr_referral.re== 1 | dr_referral.re==1 | amd_referral.re==1 | other_referral.re==1 | vcdr_referral.le==1 | referral_disc_abnormal.le==1 | dr_referral.le==1 | amd_referral.le==1 | other_referral.le==1) ~ 1,
#                                   (vcdr_referral.re== 0 & dr_referral.re==0 & amd_referral.re==0 & other_referral.re==0 & vcdr_referral.le==0 & referral_disc_abnormal.le==0 & dr_referral.le==0 & amd_referral.le==0 & other_referral.le==0) ~ 0,
#                                   TRUE ~ NA_real_)) %>%
#   select(photoscreenpos, this_form_referral) %>%
#   filter(!is.na(this_form_referral))
#     
# #View(photoagreement)
# 
# photoagreement2 <- full_join(csdatawide, jeremyreview, by="study_id") %>%
#   select(refer_patient_screen_cal, this_form_referral) %>%
#   filter(!is.na(this_form_referral))
# 
# kappa2(photoagreement, weight = "unweighted", sort.levels = FALSE)
# kappa2(photoagreement2, weight = "unweighted", sort.levels = FALSE)
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

#Table 2: demographics by clinic

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

# Table 3
addmargins(xtabs(data=filter(csdatalong, consent==1), ~ screenfail_va + clinic_attended_v2))
addmargins(xtabs(data=filter(csdatalong, consent==1), ~ screenfail_iop + clinic_attended_v2, addNA=TRUE))
addmargins(xtabs(data=filter(csdatalong, consent==1), ~ screenfail_abnldisc + clinic_attended_v2))
addmargins(xtabs(data=filter(csdatalong, consent==1), ~ screenfail_vcdr + clinic_attended_v2))
addmargins(xtabs(data=filter(csdatalong, consent==1), ~ screenfail_amd + clinic_attended_v2))
addmargins(xtabs(data=filter(csdatalong, consent==1), ~ screenfail_dr + clinic_attended_v2))

addmargins(xtabs(data=filter(csdatalong, consent==1), ~ screenfail_any + oe_anyamddrglc, addNA=TRUE))
addmargins(xtabs(data=filter(csdatalong, consent==1 & refer_negative==1), ~ screenfail_any + oe_anyamddrglc, addNA=TRUE))


#Table 4 line by line

addmargins(xtabs(data=filter(csdatalong, consent==1), ~ oe_anyamd + clinic_attended_v2, addNA = TRUE))
addmargins(xtabs(data=filter(csdatalong, consent==1), ~ oe_anydr + clinic_attended_v2, addNA = TRUE))
addmargins(xtabs(data=filter(csdatalong, consent==1), ~ oe_anyglc + clinic_attended_v2, addNA = TRUE))
addmargins(xtabs(data=filter(csdatalong, consent==1), ~ oe_anyglcsuspect + clinic_attended_v2, addNA = TRUE))
addmargins(xtabs(data=filter(csdatalong, consent==1), ~ oe_anyamddrglc + clinic_attended_v2, addNA = TRUE))
addmargins(xtabs(data=filter(csdatalong, consent==1), ~ oe_tx_vftestnotmiss  + clinic_attended_v2, addNA = TRUE))
addmargins(xtabs(data=filter(csdatalong, consent==1), ~ oe_tx_catsurgnotmiss  + clinic_attended_v2, addNA = TRUE))
addmargins(xtabs(data=filter(csdatalong, consent==1), ~ oe_tx_rectxnotmiss + clinic_attended_v2, addNA = TRUE))

##table 4 wide
table4wide <- csdatalong%>%
  filter(phone_call_complete==2 | ophthalmologist_exam_complete==2) %>%
  filter(consent==1) %>%
  group_by(clinic_attended_v2, screenfail_anycd)%>%
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

table4long <- table4wide %>%
  select(-(consent_n)) %>%
  gather(field, value, oe_anyamd.mean:oe_tx_rectxnotmiss.denom) %>%
  separate(field, into = c("diagnosis", "stat"), sep= "\\.") %>%
  mutate(clinic_stat = paste(clinic_attended_v2, stat, sep = "." ))%>%
  select(-clinic_attended_v2, -stat) %>%
  spread(clinic_stat, value) %>%
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

#? Am I unsure why this isn't working, 
# I believe it is because we need to exclude people who were referred, but did not show.
# However, I believe that I have already done so in line 553 

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

 #table 5 (snes and spec)
             
             # lm(formula, data, subset, weights, na.action,
   method = "qr", model = TRUE, x = FALSE, y = FALSE, qr = TRUE,
   singular.ok = TRUE, contrasts = NULL, offset, ...)
             
             
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

