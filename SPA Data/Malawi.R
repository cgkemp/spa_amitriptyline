rm(list = ls())

library("tidyverse")
library("haven")
library("dplyr")

setwd("~/GitHub/spa_amitriptyline/SPA Data")

df <- read_sas("Facility Inventory/malawi 13-14/MWFC6JSDSR/MWFC6JFLSR.SAS7BDAT")

malawi <- df %>%  
  mutate(rural = case_when(
    V003== 1 ~ 0,
    V003== 2 ~ 1)) %>%
  mutate(ownership = case_when(
    V008==1 ~ 1,
    V008==2 ~ 4,
    V008==3 ~ 3,
    V008==4 ~ 4,
    V008==5 ~ 2,
    V008==6 ~ 2)) %>%
  mutate(facility_type = case_when(
  V007== 2 ~ "hospital", 
  V007== 3 ~ "hospital",
  V007== 4 ~ "hospital",
  V007== 5 ~ "primary",
  V007== 6 ~ "other",
  V007== 7 ~ "other",
  V007== 8 ~ "primary",
  V007== 9 ~ "other",
  V007== TRUE ~ "hospital")) %>%
  mutate(primary = case_when(
  V007== 2 ~ 0, 
  V007== 3 ~ 0,
  V007== 4 ~ 0,
  V007== 5 ~ 1,
  V007== 6 ~ 0,
  V007== 7 ~ 0,
  V007== 8 ~ 1,
  V007== 9 ~ 0,
  TRUE ~ 0)) %>%
  mutate(store_meds = case_when(
    V035==0 ~ 0,
    V035==1 ~ 1)) %>%
  mutate(ncd_services = case_when(
    V048==0 ~ 0,
    V048==1 ~ 1)) %>%
  mutate(amitriptyline = case_when(
    V903_16== 0 ~ 0,
    V903_16== 3 ~ 0,
    V903_16== 4 ~ 0,
    V903_16== 5 ~ 0,
    V903_16== 2 ~ 1,
    V903_16== 1 ~ 0)) %>%
  mutate(total_staff = V102DT) %>%
  mutate(power = case_when(
    V120A== 0 ~ 0, #not connected
    V120A== 1 ~ 1, #connected
    V120A== 2 ~ 0,
    V120A== 3 ~ 0, 
    V120A== 8 ~ 0)) %>%
  mutate(improved_water = case_when(
    V123== 0 ~ 0,
    V123== 13 ~ 1,
    V123== 14 ~ 1,
    V123== 15 ~ 1,
    V123== 20 ~ 1,
    V123== 21 ~ 0,
    V123== 22 ~ 1,
    V123== 23 ~ 1,
    V123== 24 ~ 0,
    V123== 30 ~ 0, 
    V123== 31 ~ 1,
    V123== 32 ~ 1,
    V123== 41 ~ 0,
    V123== 42 ~ 0,
    V123== 96 ~ 0,
    V123== 98 ~ 0)) %>%
  mutate(improved_sanitation = case_when(
    V153A== 0 ~ 0,
    V153A== 11 ~ 1,
    V153A== 12 ~ 1,
    V153A== 13 ~ 1,
    V153A== 14 ~ 1,
    V153A== 15 ~ 1,
    V153A== 21 ~ 1,
    V153A== 22 ~ 0,
    V153A== 23 ~ 0,
    V153A== 31 ~ 0, 
    V153A== 41 ~ 0,
    V153A== 51 ~ 0)) %>%
  mutate(email = case_when(
    V129== 0 ~ 0, #no
    V129== 1 ~ 1, #yes
    V129== 2 ~ 1)) %>%
  mutate(computer = case_when(
    V128A== 0 ~ 0, #no
    V128A== 1 ~ 1, #yes
    V128A== 2 ~ 1 )) %>%
  mutate(general_opd_private_room = case_when(
    V167== 0 ~ 0, #no
    V167== 1 ~ 1, #yes
    V167== 2 ~ 1,
    V167== 4 ~ 0)) %>%
  mutate(ncd_private_room = case_when(
    V1609== 0 ~ 0, #no
    V1609== 1 ~ 1, #yes
    V1609== 2 ~ 1, 
    V1609== 4 ~ 0)) %>%
  mutate(country = "Malawi", 
         worldbank = "Low Income") %>%
  dplyr::rename(province = V001,
                district = V002,
                facility_number = V004,
                month = V081,
                year = V082) %>%
  select(province, district, rural, facility_number, month, year, ownership, facility_type, primary, store_meds, ncd_services, amitriptyline, 
         total_staff, power, improved_water, improved_sanitation, email, computer, general_opd_private_room, ncd_private_room, country, worldbank)

#Import SPA lat/long
df_spatial <- read.dbf("Facility Inventory/malawi 13-14/geo/MWGE6IFLSR.dbf") %>%
  select(facility_number = SPAFACID, province_name = ADM1NAME, district_name = SPAREGNA, facility_type_name = SPATYPEN, ownership_name = SPAMANGN, latitude = LATNUM, longitude = LONGNUM) %>%
  mutate(latitude = na_if(latitude,0),
         longitude = na_if(longitude,0))

malawi <- malawi %>%
  left_join(df_spatial, by="facility_number")

saveRDS(malawi, "malawi.rds")