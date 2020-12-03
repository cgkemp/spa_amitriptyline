library(haven)
library(tidyverse)
senegal2012 <- read_sas("~/Data/Datasets/SN_2012-13_SPA_07272020_336_150437/SNFC6ISDSR/SNFC6IFLSR.SAS7BDAT")
senegal2012 = senegal2012 %>% mutate("Facility" = case_when(
  V007== 1 ~ 1, 
  V007== 2 ~ 2,
  V007== 3 ~ 2,
  V007== 4 ~ 3,))
senegal2012 = senegal2012 %>% mutate("Primary" = case_when(
  V007== 1 ~ 1, 
  V007== 2 ~ 2,
  V007== 3 ~ 2,
  V007== 4 ~ 1,))
senegal2012 = senegal2012  %>% mutate(Country = "Senegal", WorldBank_Classification = "Lower_Middle_Income")
senegal2012 = senegal2012  %>% mutate("Staff" = senegal2012$V102DT)
senegal2012 = senegal2012 %>% mutate("Amitriptyline" = case_when(
  V903_16== 0 ~ 1,
  V903_16== 3 ~ 1,
  V903_16== 4 ~ 1,
  V903_16== 5 ~ 1,
  V903_16== 2 ~ 2))
senegal2012 = senegal2012 %>% mutate("Power_Source" = case_when(
  V120A== 0 ~ 0, #not connected
  V120A== 1 ~ 1, #connected
  V120A== 2 ~ 1, 
  V120A== 3 ~ 1,
  V120A== 8 ~ 0))
senegal2012 = senegal2012 %>% mutate("Water_Source" = case_when(
  V123== 0 ~ 2,
  V123== 13 ~ 1,
  V123== 14 ~ 1,
  V123== 15 ~ 1,
  V123== 20 ~ 1,
  V123== 21 ~ 2,
  V123== 22 ~ 1,
  V123== 23 ~ 1,
  V123== 24 ~ 2,
  V123== 30 ~ 2, 
  V123== 31 ~ 1,
  V123== 32 ~ 1,
  V123== 41 ~ 2,
  V123== 42 ~ 2,
  V123== 96 ~ 2,
  V123== 98 ~ 2,
  V123== 3 ~ 2))
senegal2012 = senegal2012 %>% mutate("Email_or_Internet_Access" = case_when(
  V129== 0 ~ 0, #no
  V129== 1 ~ 1, #yes
  V129== 2 ~ 1))
senegal2012 = senegal2012 %>% mutate("Computer" = case_when(
  V129== 0 ~ 0, #no 
  V129== 1 ~ 1, #yes
  V129== 2 ~ 1))
senegal2012 = senegal2012 %>% mutate("Noncommunicable_Diseases_Room_Privacy" = case_when(
  V1609== 0 ~ 0, #no
  V1609== 1 ~ 1, #yes
  V1609== 2 ~ 1,
  V1609== 4 ~ 0))
senegal2012 = senegal2012 %>% mutate("OPD_Room_Privacy" = case_when(
  V167== 0 ~ 0, #no
  V167== 1 ~ 1, #yes
  V167== 2 ~ 1, 
  V167== 4 ~ 0))
senegal2012 = senegal2012 %>% mutate("Sanitation" = case_when(
  V153A== 0 ~ 0,
  V153A== 11 ~ 1,
  V153A== 12 ~ 1,
  V153A== 13 ~ 1,
  V153A== 14 ~ 0,
  V153A== 15 ~ 0,
  V153A== 21 ~ 1,
  V153A== 22 ~ 1,
  V153A== 23 ~ 0,
  V153A== 31 ~ 1, 
  V153A== 41 ~ 0,
  V153A== 51 ~ 0))
senegal2012 <- dplyr::select(senegal2012, 'Facility', 
                               Primary,
                               Staff,
                               Amitriptyline,
                               V004,
                               V035,
                               V008,
                               V081,
                               V082,
                               Country,
                               WorldBank_Classification,
                             Noncommunicable_Diseases_Room_Privacy,
                             Computer,
                               Water_Source,
                               Power_Source,
                               Email_or_Internet_Access,
                               OPD_Room_Privacy,
                             Sanitation)
names(senegal2012)[names(senegal2012) == 'V008'] <- 'Ownership'
names(senegal2012)[names(senegal2012) == 'V035'] <- 'Medication.Storage'
names(senegal2012)[names(senegal2012) == 'V004'] <- 'Facility_Number'
names(senegal2012)[names(senegal2012) == 'V081'] <- 'Month'
names(senegal2012)[names(senegal2012) == 'V082'] <- 'Year'
senegal2012