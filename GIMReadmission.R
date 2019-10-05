####GIM Readmission####

en_test <- read.csv("data/raw/test/outcsv_test_encounters.csv", head=TRUE)
en_training <- read.csv("data/raw/training/outcsv_train_train_encounters.csv", head=TRUE)
en_val <- read.csv("data/raw/validation/outcsv_valid_valid_encounters.csv")
encounters <- rbind(en_test, en_training, en_val)
rm(en_test, en_training, en_val)

demo_test <- read.csv("data/raw/test/outcsv_test_demographic.csv", head=TRUE)
demo_training <- read.csv("data/raw/training/outcsv_train_train_demographics.csv", head=TRUE)
demo_val <- read.csv("data/raw/validation/outcsv_valid_valid_demographics.csv")
demographics <- rbind(demo_test, demo_training, demo_val)
rm(demo_test, demo_training, demo_val)

baseline_test <- read.csv("data/raw/test/outcsv_test_baseline_values.csv", head=TRUE)
baseline_training <- read.csv("data/raw/training/outcsv_train_train_baseline_values.csv", head=TRUE)
baseline_val <- read.csv("data/raw/validation/outcsv_valid_valid_baseline_values.csv")
baseline_values <- rbind(baseline_test, baseline_training, baseline_val)
rm(baseline_test, baseline_training, baseline_val)

orders_test <- read.csv("data/raw/test/outcsv_test_clinical_orders.csv", head=TRUE)
orders_training <- read.csv("data/raw/training/outcsv_train_train_clinical_orders.csv", head=TRUE)
orders_val <- read.csv("data/raw/validation/outcsv_valid_valid_clinical_orders.csv")
orders <- rbind(orders_test, orders_training, orders_val)
rm(orders_test, orders_training, orders_val)

#encounters, demographics, baseline_values, orders
all1 <- merge(encounters, demographics, by=c('ENCOUNTER_NUM'))
all <- merge(all1, baseline_values, by=c('ENCOUNTER_NUM'))
write.csv(all, file.path(getwd(), 'raw_pooled.csv'))


library(dplyr)

a <-
  all %>%
  select(PATIENT_DK, ENCOUNTER_NUM, age.x, pre_gim_icu, post_gim_icu, OUTCOME_TYPE, ICD10) %>%
  arrange(PATIENT_DK, age.x)
  
b <-
  all %>%
  select(PATIENT_DK, ENCOUNTER_NUM, age.x, pre_gim_icu, post_gim_icu, OUTCOME_TYPE, gim_to_outcome, ICD10) %>%
  filter(OUTCOME_TYPE==5) %>% 
  group_by(PATIENT_DK) %>% 
  arrange(age.x) %>%
  filter(row_number() == 1)

b$ind <- 1
b$dis_age <- b$age.x + (b$gim_to_outcome/365.25)
b2 <- 
  b %>%
  select(ENCOUNTER_NUM, ind)
all2 <- merge(all, b2, by=c('ENCOUNTER_NUM'), all.x=TRUE, all.y=TRUE)
all2$index <- ifelse(is.na(all2$ind), 0, all2$ind)

b3 <- 
  b %>%
  select(PATIENT_DK, dis_age)
all3 <- merge(all2, b3, by.x=c('PATIENT_DK.x'), by.y=c("PATIENT_DK"), all.x=TRUE, all.y=TRUE)

all3$days_since_discharge <- ifelse(all3$index==1, NA, ((all3$age.x - all3$dis_age)*365.25))

all4 <-
  all3 %>%
  filter(days_since_discharge <=30 & days_since_discharge>=0)

all4$readmit_30d <- 1

outcome.flags <-
  all4 %>%
  select(PATIENT_DK.x, readmit_30d)

outcome.flags2 <- 
  outcome.flags %>%
  group_by(PATIENT_DK.x) %>%
  filter(row_number() == 1)
  

all5 <- left_join(all3, outcome.flags2, by = 'PATIENT_DK.x')
all5$readmit_30d <- ifelse(is.na(all5$readmit_30d), 0, all5$readmit_30d)
all6 <- 
  all5 %>%
  filter(index==1)

all6$PATIENT_DK <- all6$PATIENT_DK.x
all6$age <- all6$age.x
all6$gender <- all6$gender.x
all7 <-
  all6 %>%
  select(-PATIENT_DK.y, -PATIENT_DK.x, -age.y, -age.x, -gender.x, -gender.y)

semi_final <-
    all7 %>%
    select(PATIENT_DK, ENCOUNTER_NUM, FROM_SERVICE, ADT_DISCHARGE, MRP_DIAGNOSIS, age, gim_to_outcome, pre_gim_icu, post_gim_icu, ICD10, OUTCOME_TYPE, marital, language, religion, gender, readmit_30d)

write.csv(semi_final, file.path(getwd(), 'semi_final.csv'))


##Medication Flags##
med_test <- read.csv("data/raw/test/outcsv_test_medication_admin.csv", head=TRUE)
med_training <- read.csv("data/raw/training/outcsv_train_train_medication_admin.csv", head=TRUE)
med_val <- read.csv("data/raw/validation/outcsv_valid_valid_medication_admin.csv")
med <- rbind(med_test, med_training, med_val)
rm(med_test, med_training, med_val)

med$class <- substr(med$AHFS_CODE, 1,2)
med_short <- distinct(med, class, ENCOUNTER_NUM)

library(reshape2)
med2 <- melt(med_short, id="ENCOUNTER_NUM")
med3 <- dcast(med2, ENCOUNTER_NUM ~ value)
med4 <- 
  med3 %>%
  select(-"NA", -"96")


med4$med_08 <- med4$"08"
med4$med_10 <- med4$"10"
med4$med_12 <- med4$"12"
med4$med_20 <- med4$"20"
med4$med_24 <- med4$"24"
med4$med_28 <- med4$"28"
med4$med_40 <- med4$"40"
med4$med_48 <- med4$"48"
med4$med_56 <- med4$"56"
med4$med_68 <- med4$"68"
med4$med_72 <- med4$"72"
med4$med_80 <- med4$"80"
med4$med_86 <- med4$"86"
med4$med_88 <- med4$"88"
med4$med_92 <- med4$"92"
med4$med_94 <- med4$"94"

med5 <- 
  med4 %>%
  select(-"04", -"08", -"10", -"12", -"20", -"24", -"28", -"36", -"40", -"44", -"48", -"52", -"56", -"68",
         -"72", -"80", -"84", -"86", -"88", -"92", -"94")

for(i in names(med5[,2:17])) {
  med5[[i]] <-  ifelse(is.na(med5[[i]]),0,1)
}

semi_final2 <- merge(semi_final, med5, by="ENCOUNTER_NUM", all.x=TRUE)
attach(semi_final2)
semi_final2$med_sum <- med_08 + med_10 + med_12 + med_20 + med_24 + med_28 + med_40 +
  med_48 + med_56 + med_68 + med_72 + med_80 + med_86 + med_88 + med_92 + med_94
detach(semi_final2)

write.csv(semi_final2, file.path(getwd(), 'semi_final2.csv'))


##Sepsis + Resp##
alt_test <- read.csv("data/preprocessed/test/outcsv_outcomes_test_alternate_outcome_timeseries_8hr.csv", head=TRUE)
alt_training <- read.csv("data/preprocessed/training/outcsv_outcomes_train_alternate_outcome_timeseries_8hr.csv", head=TRUE)
alt_val <- read.csv("data/preprocessed/validation/outcsv_outcomes_valid_alternate_outcome_timeseries_8hr.csv", head=TRUE)
alt <- rbind(alt_test, alt_training, alt_val)
rm(alt_test, alt_training, alt_val)

alt2 <- 
  alt %>%
  select(-window, -sepsis_24, -sepsis_48, -resp_24, -resp_48)

sepsis1 <-
  alt2 %>%
  filter(sepsis_72==1)
alt_sepsis <- distinct(sepsis1, ENCOUNTER_NUM)
alt_sepsis$sepsis <- 1

resp1 <-
  alt2 %>%
  filter(alt2$resp_72==1)
alt_resp <- distinct(resp1, ENCOUNTER_NUM)    
alt_resp$resp <- 1

semi_final3 <- read.csv("semi_final3.txt", head=TRUE)

merge1 <- merge(semi_final3, alt_sepsis, by='ENCOUNTER_NUM', all.x=TRUE)
semi_final4 <- merge(merge1, alt_resp, by='ENCOUNTER_NUM', all.x=TRUE)
semi_final4$sepsis <- ifelse(is.na(semi_final4$sepsis), 0, 1)
semi_final4$resp <- ifelse(is.na(semi_final4$resp), 0, 1)

write.csv(semi_final4, file.path(getwd(), 'semi_final4.csv'))

semi_final5 <- read.csv("semi_final4.txt", head=TRUE)

outcomes.final <-
  semi_final %>%
  select(ENCOUNTER_NUM,readmit_30d)

semi_final5b <-
  semi_final5 %>%
  select(-readmit_30d)

semi_final6 <- merge(semi_final5b, outcomes.final, by="ENCOUNTER_NUM")

write.csv(semi_final6, file.path(getwd(), 'final.csv'))


final <- read.csv("final_fewercols.txt", head=TRUE)


##Analyses##

#Continuous age
semi_final6 %>%
  group_by(readmit_30d) %>%
  summarise(meanage= mean(age))
semi_final6 %>%
  group_by(readmit_30d) %>%
  summarise(quant25= quantile(age, probs=c(0.25)), quant75= quantile(age, probs=c(0.75)), 
            mean(FROM_SERVICE_ICU))

semi_final6 %>%
  group_by(readmit_30d) %>%
  summarise(outcome= n(), gender= mean(gender), medsum= mean(med_sum), neopla= mean(med_10),
            med25= quantile(med_num, prob=c(0.25)), med75= quantile(med_num, probs=c(0.75)))



