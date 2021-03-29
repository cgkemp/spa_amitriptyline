rm(list = ls())

library("tidyverse")
library("haven")
library(foreign)
library(ggplot2)
library(SpatialEpi)
library(readstata13)
library(xlsx)
library(doBy)
library(gdistance)
library(abind)
library(rje)
library(malariaAtlas)
library(raster)

setwd("~/GitHub/spa_amitriptyline/SPA Data")

df2012 <- read_sas("Facility Inventory/SN_2012-13_SPA_07272020_336_150437/SNFC6ISDSR/SNFC6IFLSR.SAS7BDAT")

senegal2012 <- df2012 %>%  
  mutate(rural = case_when(
    V003== 1 ~ 0,
    V003== 2 ~ 1)) %>%
  mutate(facility_type = case_when(    
    V007== 1 ~ "hospital", 
    V007== 2 ~ "primary",
    V007== 3 ~ "primary",
    V007== 4 ~ "other",)) %>%
  mutate(primary = case_when(
    V007== 1 ~ 0, 
    V007== 2 ~ 1,
    V007== 3 ~ 1,
    V007== 4 ~ 0,)) %>%
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
  mutate(country = "Senegal", 
         worldbank = "Lower Middle Income") %>%
  dplyr::rename(province = V001,
                district = V002,
                facility_number = V004,
                ownership = V008,
                month = V081,
                year = V082) %>%
  dplyr::select(province, district, rural, facility_number, month, year, ownership, facility_type, primary, store_meds, ncd_services, amitriptyline, 
         total_staff, power, improved_water, improved_sanitation, email, computer, general_opd_private_room, ncd_private_room, country, worldbank)

library(foreign)
library(SpatialEpi)

#Import SPA lat/long
df_spatial <- read.dbf("Facility Inventory/SN_2012-13_SPA_07272020_336_150437/geo/SNGE6IFLSR.dbf") %>%
  dplyr::select(facility_number = SPAFACID, province_name = ADM1NAME, district_name = SPAREGNA, facility_type_name = SPATYPEN, ownership_name = SPAMANGN, latitude = LATNUM, longitude = LONGNUM) %>%
  mutate(latitude = na_if(latitude,0),
         longitude = na_if(longitude,0))

senegal2012 <- senegal2012 %>%
  left_join(df_spatial, by="facility_number")

saveRDS(senegal2012, "senegal2012.rds")


df2014 <- read_sas("Facility Inventory/SN_2014_SPA_07272020_336_150437/SNFC72SDSP/SNFC72FLSP.SAS7BDAT")

senegal2014 <- df2014 %>%  
  mutate(rural = case_when(
    FTYPE== 1 ~ 0,
    FTYPE== 2 ~ 1)) %>%  
  mutate(facility_type = case_when(
  FACTYPE== 1 ~ "hospital", 
FACTYPE== 2 ~ "primary",
FACTYPE== 3 ~ "primary",
FACTYPE== 4 ~ "other")) %>%
  mutate(primary = case_when(
    FACTYPE== 1 ~ 0, 
    FACTYPE== 2 ~ 1,
    FACTYPE== 3 ~ 1,
    FACTYPE== 4 ~ 0)) %>%
  mutate(store_meds = case_when(
    Q210==0 ~ 0,
    Q210==1 ~ 1)) %>%
  mutate(ncd_services = case_when(
    Q102_14==0 ~ 0,
    Q102_14==1 ~ 1)) %>%
  mutate(amitriptyline = case_when(
    Q903_16== 2 ~ 0,
    Q903_16== 3 ~ 0,
    Q903_16== 4 ~ 0,
    Q903_16== 5 ~ 0,
    Q903_16== 1 ~ 1)) %>%
  mutate(total_staff = Q400AT) %>%
  mutate(power = case_when(
    Q340== 1 ~ 1,
    Q340== 2 ~ 0)) %>%
  mutate(improved_water = case_when(
    Q330== 1 ~ 1,
    Q330== 2 ~ 1,
    Q330== 3 ~ 1,
    Q330== 4 ~ 1,
    Q330== 5 ~ 1,
    Q330== 6 ~ 0,
    Q330== 7 ~ 1,
    Q330== 8 ~ 0,
    Q330== 9 ~ 1,
    Q330== 10 ~ 1,
    Q330== 11 ~ 0,
    Q330== 12 ~ 0,
    Q330== 13 ~ 0,
    Q330== 96 ~ 0,
    Q330== 98 ~ 0,
    Q330== 0 ~ 0)) %>%
  mutate(improved_sanitation = case_when(
    Q620== 11 ~ 1,
    Q620== 12 ~ 1,
    Q620== 13 ~ 1,
    Q620== 14 ~ 1,
    Q620== 15 ~ 1,
    Q620== 21 ~ 1,
    Q620== 22 ~ 0,
    Q620== 23 ~ 0,
    Q620== 31 ~ 0,
    Q620== 41 ~ 0,
    Q620== 51 ~ 0,
    Q620== 61 ~ 0)) %>%
  mutate(email = case_when(
    Q322== 1 ~ 1, #yes
    Q322== 2 ~ 0)) %>%
  mutate(computer = case_when(
    Q319== 1 ~ 1, #yes
    Q319== 2 ~ 0)) %>%
  mutate(general_opd_private_room = case_when(
    Q1952== 1 ~ 1, #yes
    Q1952== 2 ~ 1, #other room, but yes
    Q1952== 3 ~ 0,
    Q1952== 4 ~ 0)) %>%
  mutate(ncd_private_room = case_when(
    Q2352== 1 ~ 1, #yes
    Q2352== 2 ~ 1, #other room, but yes
    Q2352== 3 ~ 0,
    Q2352== 4 ~ 0)) %>%
  mutate(country = "Senegal", 
         worldbank = "Lower Middle Income") %>%
  dplyr::rename(province = REGION,
                facility_number = FACIL,
                ownership = MGA,
                month = MONTH,
                year = YEAR) %>%
  dplyr::select(province, rural, facility_number, month, year, ownership, facility_type, primary, store_meds, ncd_services, amitriptyline, 
         total_staff, power, improved_water, improved_sanitation, email, computer, general_opd_private_room, ncd_private_room, country, worldbank)

#Import SPA lat/long
df_spatial <- read.dbf("Facility Inventory/SN_2014_SPA_07272020_336_150437/geo/SNGE71FLSR.dbf") %>%
  dplyr::select(facility_number = SPAFACID, province_name = ADM1NAME, district_name = SPAREGNA, facility_type_name = SPATYPEN, ownership_name = SPAMANGN, latitude = LATNUM, longitude = LONGNUM) %>%
  mutate(latitude = na_if(latitude,0),
         longitude = na_if(longitude,0))

senegal2014 <- senegal2014 %>%
  left_join(df_spatial, by="facility_number")

saveRDS(senegal2014, "senegal2014.rds")


df2015 <- read_sas("Facility Inventory/SN_2015_SPA_07272020_336_150437/SNFC7ASDSP/SNFC7AFLSP.SAS7BDAT")

senegal2015 <- df2015 %>%  
  mutate(rural = case_when(
    FTYPE== 1 ~ 0,
    FTYPE== 2 ~ 1)) %>%  
  mutate(facility_type = case_when(
    FACTYPE== 1 ~ "hospital", 
    FACTYPE== 2 ~ "primary",
    FACTYPE== 3 ~ "primary",
    FACTYPE== 4 ~ "other")) %>%
  mutate(primary = case_when(
    FACTYPE== 1 ~ 0, 
    FACTYPE== 2 ~ 1,
    FACTYPE== 3 ~ 1,
    FACTYPE== 4 ~ 0)) %>%
  mutate(store_meds = case_when(
    Q210==0 ~ 0,
    Q210==1 ~ 1)) %>%
  mutate(ncd_services = case_when(
    Q102_14==0 ~ 0,
    Q102_14==1 ~ 1)) %>%
  mutate(amitriptyline = case_when(
    Q903_16== 2 ~ 0,
    Q903_16== 3 ~ 0,
    Q903_16== 4 ~ 0,
    Q903_16== 5 ~ 0,
    Q903_16== 1 ~ 1)) %>%
  mutate(total_staff = Q400AT) %>%
  mutate(power = case_when(
    Q340== 1 ~ 1,
    Q340== 2 ~ 0)) %>%
  mutate(improved_water = case_when(
    Q330== 1 ~ 1,
    Q330== 2 ~ 1,
    Q330== 3 ~ 1,
    Q330== 4 ~ 1,
    Q330== 5 ~ 1,
    Q330== 6 ~ 0,
    Q330== 7 ~ 1,
    Q330== 8 ~ 0,
    Q330== 9 ~ 1,
    Q330== 10 ~ 1,
    Q330== 11 ~ 0,
    Q330== 12 ~ 0,
    Q330== 13 ~ 0,
    Q330== 96 ~ 0,
    Q330== 98 ~ 0,
    Q330== 0 ~ 0)) %>%
  mutate(improved_sanitation = case_when(
    Q620== 11 ~ 1,
    Q620== 12 ~ 1,
    Q620== 13 ~ 1,
    Q620== 14 ~ 1,
    Q620== 15 ~ 1,
    Q620== 21 ~ 1,
    Q620== 22 ~ 0,
    Q620== 23 ~ 0,
    Q620== 31 ~ 0,
    Q620== 41 ~ 0,
    Q620== 51 ~ 0,
    Q620== 61 ~ 0)) %>%
  mutate(email = case_when(
    Q322== 1 ~ 1, #yes
    Q322== 2 ~ 0)) %>%
  mutate(computer = case_when(
    Q319== 1 ~ 1, #yes
    Q319== 2 ~ 0)) %>%
  mutate(general_opd_private_room = case_when(
    Q1952== 1 ~ 1, #yes
    Q1952== 2 ~ 1, #other room, but yes
    Q1952== 3 ~ 0,
    Q1952== 4 ~ 0)) %>%
  mutate(ncd_private_room = case_when(
    Q2352== 1 ~ 1, #yes
    Q2352== 2 ~ 1, #other room, but yes
    Q2352== 3 ~ 0,
    Q2352== 4 ~ 0)) %>%
  mutate(country = "Senegal", 
         worldbank = "Lower Middle Income") %>%
  dplyr::rename(province = REGION,
                facility_number = FACIL,
                ownership = MGA,
                month = MONTH,
                year = YEAR) %>%
  dplyr::select(province, rural, facility_number, month, year, ownership, facility_type, primary, store_meds, ncd_services, amitriptyline, 
         total_staff, power, improved_water, improved_sanitation, email, computer, general_opd_private_room, ncd_private_room, country, worldbank)

#Import SPA lat/long
df_spatial <- read.dbf("Facility Inventory/SN_2015_SPA_07272020_336_150437/geo/SNGE7AFLSR.dbf") %>%
  dplyr::select(facility_number = SPAFACID, province_name = ADM1NAME, district_name = SPAREGNA, facility_type_name = SPATYPEN, ownership_name = SPAMANGN, latitude = LATNUM, longitude = LONGNUM) %>%
  mutate(latitude = na_if(latitude,0),
         longitude = na_if(longitude,0))

senegal2015 <- senegal2015 %>%
  left_join(df_spatial, by="facility_number")

saveRDS(senegal2015, "senegal2015.rds")

df2016 <- read_sas("Facility Inventory/SN_2016_SPA_07272020_337_150437/SNFC7QSDSP/SNFC7QFLSP.SAS7BDAT")

senegal2016 <- df2016 %>%  
  mutate(rural = case_when(
    FTYPE== 1 ~ 0,
    FTYPE== 2 ~ 1)) %>%  
  mutate(facility_type = case_when(
    FACTYPE== 1 ~ "hospital", 
    FACTYPE== 2 ~ "primary",
    FACTYPE== 3 ~ "primary",
    FACTYPE== 4 ~ "other")) %>%
  mutate(primary = case_when(
    FACTYPE== 1 ~ 0, 
    FACTYPE== 2 ~ 1,
    FACTYPE== 3 ~ 1,
    FACTYPE== 4 ~ 0)) %>%
  mutate(store_meds = case_when(
    Q210==0 ~ 0,
    Q210==1 ~ 1)) %>%
  mutate(ncd_services = case_when(
    Q102_14==0 ~ 0,
    Q102_14==1 ~ 1)) %>%
  mutate(amitriptyline = case_when(
    Q903_16== 2 ~ 0,
    Q903_16== 3 ~ 0,
    Q903_16== 4 ~ 0,
    Q903_16== 5 ~ 0,
    Q903_16== 1 ~ 1)) %>%
  mutate(total_staff = Q400AT) %>%
  mutate(power = case_when(
    Q340== 1 ~ 1,
    Q340== 2 ~ 0)) %>%
  mutate(improved_water = case_when(
    Q330== 1 ~ 1,
    Q330== 2 ~ 1,
    Q330== 3 ~ 1,
    Q330== 4 ~ 1,
    Q330== 5 ~ 1,
    Q330== 6 ~ 0,
    Q330== 7 ~ 1,
    Q330== 8 ~ 0,
    Q330== 9 ~ 1,
    Q330== 10 ~ 1,
    Q330== 11 ~ 0,
    Q330== 12 ~ 0,
    Q330== 13 ~ 0,
    Q330== 96 ~ 0,
    Q330== 98 ~ 0,
    Q330== 0 ~ 0)) %>%
  mutate(improved_sanitation = case_when(
    Q620== 11 ~ 1,
    Q620== 12 ~ 1,
    Q620== 13 ~ 1,
    Q620== 14 ~ 1,
    Q620== 15 ~ 1,
    Q620== 21 ~ 1,
    Q620== 22 ~ 0,
    Q620== 23 ~ 0,
    Q620== 31 ~ 0,
    Q620== 41 ~ 0,
    Q620== 51 ~ 0,
    Q620== 61 ~ 0)) %>%
  mutate(email = case_when(
    Q322== 1 ~ 1, #yes
    Q322== 2 ~ 0)) %>%
  mutate(computer = case_when(
    Q319== 1 ~ 1, #yes
    Q319== 2 ~ 0)) %>%
  mutate(general_opd_private_room = case_when(
    Q1952== 1 ~ 1, #yes
    Q1952== 2 ~ 1, #other room, but yes
    Q1952== 3 ~ 0,
    Q1952== 4 ~ 0)) %>%
  mutate(ncd_private_room = case_when(
    Q2352== 1 ~ 1, #yes
    Q2352== 2 ~ 1, #other room, but yes
    Q2352== 3 ~ 0,
    Q2352== 4 ~ 0)) %>%
  mutate(country = "Senegal", 
         worldbank = "Lower Middle Income") %>%
  dplyr::rename(province = REGION,
                facility_number = FACIL,
                ownership = MGA,
                month = MONTH,
                year = YEAR) %>%
  dplyr::select(province, rural, facility_number, month, year, ownership, facility_type, primary, store_meds, ncd_services, amitriptyline, 
         total_staff, power, improved_water, improved_sanitation, email, computer, general_opd_private_room, ncd_private_room, country, worldbank)

#Import SPA lat/long
df_spatial <- read.dbf("Facility Inventory/SN_2016_SPA_07272020_337_150437/geo/SNGE7IFLSR.dbf") %>%
  dplyr::select(facility_number = SPAFACID, province_name = ADM1NAME, district_name = SPAREGNA, facility_type_name = SPATYPEN, ownership_name = SPAMANGN, latitude = LATNUM, longitude = LONGNUM) %>%
  mutate(latitude = na_if(latitude,0),
         longitude = na_if(longitude,0))

senegal2016 <- senegal2016 %>%
  left_join(df_spatial, by="facility_number")

saveRDS(senegal2016, "senegal2016.rds")

df2017 <- read_sas("Facility Inventory/SN_2017_SPA_07272020_337_150437/SNFC7ZSDSP/SNFC7ZFLSP.SAS7BDAT")

senegal2017 <- df2017 %>%  
  mutate(rural = case_when(
    FTYPE== 1 ~ 0,
    FTYPE== 2 ~ 1)) %>%  
  mutate(facility_type = case_when(
    FACTYPE== 1 ~ "hospital", 
    FACTYPE== 2 ~ "primary",
    FACTYPE== 3 ~ "primary",
    FACTYPE== 4 ~ "other")) %>%
  mutate(primary = case_when(
    FACTYPE== 1 ~ 0, 
    FACTYPE== 2 ~ 1,
    FACTYPE== 3 ~ 1,
    FACTYPE== 4 ~ 0)) %>%
  mutate(store_meds = case_when(
    Q210==0 ~ 0,
    Q210==1 ~ 1)) %>%
  mutate(ncd_services = case_when(
    Q102_14==0 ~ 0,
    Q102_14==1 ~ 1)) %>%
  mutate(amitriptyline = case_when(
    Q903_16== 2 ~ 0,
    Q903_16== 3 ~ 0,
    Q903_16== 4 ~ 0,
    Q903_16== 5 ~ 0,
    Q903_16== 1 ~ 1)) %>%
  mutate(total_staff = Q400AT) %>%
  mutate(power = case_when(
    Q340== 1 ~ 1,
    Q340== 2 ~ 0)) %>%
  mutate(improved_water = case_when(
    Q330== 1 ~ 1,
    Q330== 2 ~ 1,
    Q330== 3 ~ 1,
    Q330== 4 ~ 1,
    Q330== 5 ~ 1,
    Q330== 6 ~ 0,
    Q330== 7 ~ 1,
    Q330== 8 ~ 0,
    Q330== 9 ~ 1,
    Q330== 10 ~ 1,
    Q330== 11 ~ 0,
    Q330== 12 ~ 0,
    Q330== 13 ~ 0,
    Q330== 96 ~ 0,
    Q330== 98 ~ 0,
    Q330== 0 ~ 0)) %>%
  mutate(improved_sanitation = case_when(
    Q620== 11 ~ 1,
    Q620== 12 ~ 1,
    Q620== 13 ~ 1,
    Q620== 14 ~ 1,
    Q620== 15 ~ 1,
    Q620== 21 ~ 1,
    Q620== 22 ~ 0,
    Q620== 23 ~ 0,
    Q620== 31 ~ 0,
    Q620== 41 ~ 0,
    Q620== 51 ~ 0,
    Q620== 61 ~ 0)) %>%
  mutate(email = case_when(
    Q322== 1 ~ 1, #yes
    Q322== 2 ~ 0)) %>%
  mutate(computer = case_when(
    Q319== 1 ~ 1, #yes
    Q319== 2 ~ 0)) %>%
  mutate(general_opd_private_room = case_when(
    Q1952== 1 ~ 1, #yes
    Q1952== 2 ~ 1, #other room, but yes
    Q1952== 3 ~ 0,
    Q1952== 4 ~ 0)) %>%
  mutate(ncd_private_room = case_when(
    Q2352== 1 ~ 1, #yes
    Q2352== 2 ~ 1, #other room, but yes
    Q2352== 3 ~ 0,
    Q2352== 4 ~ 0)) %>%
  mutate(country = "Senegal", 
         worldbank = "Lower Middle Income") %>%
  dplyr::rename(province = REGION,
                facility_number = FACIL,
                ownership = MGA,
                month = MONTH,
                year = YEAR) %>%
  dplyr::select(province, rural, facility_number, month, year, ownership, facility_type, primary, store_meds, ncd_services, amitriptyline, 
         total_staff, power, improved_water, improved_sanitation, email, computer, general_opd_private_room, ncd_private_room, country, worldbank)

#NO GEO DATA

saveRDS(senegal2017, "senegal2017.rds")

df2018 <- read_sas("Facility Inventory/senegal 2018/SNFC8HSDSP/SNFC8HFLSP.SAS7BDAT")

senegal2018 <- df2018 %>%  
  mutate(rural = case_when(
    FTYPE== 1 ~ 0,
    FTYPE== 2 ~ 1)) %>%  
  mutate(facility_type = case_when(
    FACTYPE== 1 ~ "hospital", 
    FACTYPE== 2 ~ "primary",
    FACTYPE== 3 ~ "primary",
    FACTYPE== 4 ~ "other")) %>%
  mutate(primary = case_when(
    FACTYPE== 1 ~ 0, 
    FACTYPE== 2 ~ 1,
    FACTYPE== 3 ~ 1,
    FACTYPE== 4 ~ 0)) %>%
  mutate(store_meds = case_when(
    Q210==0 ~ 0,
    Q210==1 ~ 1)) %>%
  mutate(ncd_services = case_when(
    Q102_14==0 ~ 0,
    Q102_14==1 ~ 1)) %>%
  mutate(amitriptyline = case_when(
    Q903_16== 2 ~ 0,
    Q903_16== 3 ~ 0,
    Q903_16== 4 ~ 0,
    Q903_16== 5 ~ 0,
    Q903_16== 1 ~ 1)) %>%
  mutate(total_staff = Q400AT) %>%
  mutate(power = case_when(
    Q340== 1 ~ 1,
    Q340== 2 ~ 0)) %>%
  mutate(improved_water = case_when(
    Q330== 1 ~ 1,
    Q330== 2 ~ 1,
    Q330== 3 ~ 1,
    Q330== 4 ~ 1,
    Q330== 5 ~ 1,
    Q330== 6 ~ 0,
    Q330== 7 ~ 1,
    Q330== 8 ~ 0,
    Q330== 9 ~ 1,
    Q330== 10 ~ 1,
    Q330== 11 ~ 0,
    Q330== 12 ~ 0,
    Q330== 13 ~ 0,
    Q330== 96 ~ 0,
    Q330== 98 ~ 0,
    Q330== 0 ~ 0)) %>%
  mutate(improved_sanitation = case_when(
    Q620== 11 ~ 1,
    Q620== 12 ~ 1,
    Q620== 13 ~ 1,
    Q620== 14 ~ 1,
    Q620== 15 ~ 1,
    Q620== 21 ~ 1,
    Q620== 22 ~ 0,
    Q620== 23 ~ 0,
    Q620== 31 ~ 0,
    Q620== 41 ~ 0,
    Q620== 51 ~ 0,
    Q620== 61 ~ 0)) %>%
  mutate(email = case_when(
    Q322== 1 ~ 1, #yes
    Q322== 2 ~ 0)) %>%
  mutate(computer = case_when(
    Q319== 1 ~ 1, #yes
    Q319== 2 ~ 0)) %>%
  mutate(general_opd_private_room = case_when(
    Q1952== 1 ~ 1, #yes
    Q1952== 2 ~ 1, #other room, but yes
    Q1952== 3 ~ 0,
    Q1952== 4 ~ 0)) %>%
  mutate(ncd_private_room = case_when(
    Q2352== 1 ~ 1, #yes
    Q2352== 2 ~ 1, #other room, but yes
    Q2352== 3 ~ 0,
    Q2352== 4 ~ 0)) %>%
  mutate(country = "Senegal", 
         worldbank = "Lower Middle Income") %>%
  dplyr::rename(province = REGION,
                facility_number = FACIL,
                ownership = MGA,
                month = MONTH,
                year = YEAR) %>%
  dplyr::select(province, rural, facility_number, month, year, ownership, facility_type, primary, store_meds, ncd_services, amitriptyline, 
         total_staff, power, improved_water, improved_sanitation, email, computer, general_opd_private_room, ncd_private_room, country, worldbank)

#NO GEO DATA

saveRDS(senegal2018, "senegal2018.rds")

df2019 <- read_sas("Facility Inventory/SN_2019_SPA_01132021_048_150437/SNFC8ASDSP/SNFC8AFLSP.SAS7BDAT")

senegal2019 <- df2019 %>%  
  mutate(rural = case_when(
    FTYPE== 1 ~ 0,
    FTYPE== 2 ~ 1)) %>%  
  mutate(facility_type = case_when(
    FACTYPE== 1 ~ "hospital", 
    FACTYPE== 2 ~ "primary",
    FACTYPE== 3 ~ "primary",
    FACTYPE== 4 ~ "other")) %>%
  mutate(primary = case_when(
    FACTYPE== 1 ~ 0, 
    FACTYPE== 2 ~ 1,
    FACTYPE== 3 ~ 1,
    FACTYPE== 4 ~ 0)) %>%
  mutate(store_meds = case_when(
    Q210==0 ~ 0,
    Q210==1 ~ 1)) %>%
  mutate(ncd_services = case_when(
    Q102_14==0 ~ 0,
    Q102_14==1 ~ 1)) %>%
  mutate(amitriptyline = case_when(
    Q903_16== 2 ~ 0,
    Q903_16== 3 ~ 0,
    Q903_16== 4 ~ 0,
    Q903_16== 5 ~ 0,
    Q903_16== 1 ~ 1)) %>%
  mutate(total_staff = Q400AT) %>%
  mutate(power = case_when(
    Q340== 1 ~ 1,
    Q340== 2 ~ 0)) %>%
  mutate(improved_water = case_when(
    Q330== 1 ~ 1,
    Q330== 2 ~ 1,
    Q330== 3 ~ 1,
    Q330== 4 ~ 1,
    Q330== 5 ~ 1,
    Q330== 6 ~ 0,
    Q330== 7 ~ 1,
    Q330== 8 ~ 0,
    Q330== 9 ~ 1,
    Q330== 10 ~ 1,
    Q330== 11 ~ 0,
    Q330== 12 ~ 0,
    Q330== 13 ~ 0,
    Q330== 96 ~ 0,
    Q330== 98 ~ 0,
    Q330== 0 ~ 0)) %>%
  mutate(improved_sanitation = case_when(
    Q620== 11 ~ 1,
    Q620== 12 ~ 1,
    Q620== 13 ~ 1,
    Q620== 14 ~ 1,
    Q620== 15 ~ 1,
    Q620== 21 ~ 1,
    Q620== 22 ~ 0,
    Q620== 23 ~ 0,
    Q620== 31 ~ 0,
    Q620== 41 ~ 0,
    Q620== 51 ~ 0,
    Q620== 61 ~ 0)) %>%
  mutate(email = case_when(
    Q322== 1 ~ 1, #yes
    Q322== 2 ~ 0)) %>%
  mutate(computer = case_when(
    Q319== 1 ~ 1, #yes
    Q319== 2 ~ 0)) %>%
  mutate(general_opd_private_room = case_when(
    Q1952== 1 ~ 1, #yes
    Q1952== 2 ~ 1, #other room, but yes
    Q1952== 3 ~ 0,
    Q1952== 4 ~ 0)) %>%
  mutate(ncd_private_room = case_when(
    Q2352== 1 ~ 1, #yes
    Q2352== 2 ~ 1, #other room, but yes
    Q2352== 3 ~ 0,
    Q2352== 4 ~ 0)) %>%
  mutate(country = "Senegal", 
         worldbank = "Lower Middle Income") %>%
  dplyr::rename(province = REGION,
                facility_number = FACIL,
                ownership = MGA,
                month = MONTH,
                year = YEAR) %>%
  dplyr::select(province, rural, facility_number, month, year, ownership, facility_type, primary, store_meds, ncd_services, amitriptyline, 
         total_staff, power, improved_water, improved_sanitation, email, computer, general_opd_private_room, ncd_private_room, country, worldbank)

saveRDS(senegal2019, "senegal2019.rds")

#Not including Senegal 2017-2019 as no geographical data, no way to ID duplicate facilities)

senegal <- bind_rows(senegal2012, senegal2014, senegal2015, senegal2016)

#Remove duplicates from Senegal 2012-2016
senegal <- senegal %>%
  group_by(latitude, longitude) %>% 
  mutate(dupe = n()>1) %>%
  arrange(latitude, longitude) %>%
  filter(row_number()==n() | is.na(latitude)) %>%
  dplyr::select(-dupe)

#Fix lat/long in 1 facility
senegal <- senegal %>%
  mutate(latitude= if_else(latitude== -17.376675, 14.780725, latitude),
         longitude=if_else(longitude== 14.780725, -17.376675, longitude))

#Add travel time to central MOH
shape <- shapefile("Facility Inventory/SN_2012-13_SPA_07272020_336_150437/geo/sen_admbnda_adm0_1m_gov_ocha_20190426.shp")
plot(shape, main="Shape for Clipping")
friction <- malariaAtlas::getRaster(
  surface = "A global friction surface enumerating land-based travel speed for a nominal year 2015",
  #shp = shape,
  extent = matrix(c("-18", "12","-11", "17"), nrow = 2, ncol = 2, dimnames = list(c("x", "y"), c("min", "max"))))
malariaAtlas::autoplot_MAPraster(friction)
T <- gdistance::transition(friction, function(x) 1/mean(x), 8) 
T.GC <- gdistance::geoCorrection(T)    
point.locations <- read.csv(file = "Facility Inventory/SN_2012-13_SPA_07272020_336_150437/geo/moh_location.csv")
names(point.locations) <- c("X_COORD", "Y_COORD", "name")
coordinates(point.locations) <- ~ X_COORD + Y_COORD
proj4string(point.locations) <- proj4string(shape)
points <- as.matrix(point.locations@coords)
access.raster <- gdistance::accCost(T.GC, points)
p <- malariaAtlas::autoplot_MAPraster(access.raster, 
                                      shp_df=shape, printed=F)
full_plot <- p[[1]] + geom_point(data=data.frame(point.locations@coords), 
                                 aes(x=X_COORD, y=Y_COORD)) +
  scale_fill_gradientn(colors = rev(rje::cubeHelix(gamma=1.0, 
                                                   start=1.5, 
                                                   r=-1.0, 
                                                   hue=1.5, 
                                                   n=16)), 
                       name="Minutes \n of Travel") + 
  ggtitle("Travel Time to MOH") +
  theme(axis.text=element_blank(),
        panel.border=element_rect(fill=NA, color="white"))
print(full_plot)
loc <- senegal[,c("longitude", "latitude")]
travel_time <- raster::extract(access.raster, loc)
senegal <- cbind(senegal, travel_time=travel_time)
senegal <- senegal %>%
  mutate(travel_time = na_if(travel_time, Inf))

#2012
#Import cluster lat/long
df_clusterlatlong <- read.dbf("DHS/Senegal 2012/SNGE6AFL/SNGE6AFL.dbf") %>%
  dplyr::select(cluster_id = DHSCLUST, latitude = LATNUM, longitude = LONGNUM) %>%
  filter(latitude != 0 & longitude != 0)
#Import DHS data
df_hh <- read_sas("DHS/Senegal 2012/SNHR6DSD/SNHR6DFL.SAS7BDAT")
#Collapse DHS SES data to cluster level
ggplot(df_hh, aes(x=HV271)) + geom_histogram()
df_clusterses <- summaryBy(HV271 ~ HV001, FUN=c(mean,median,sd), data=df_hh)
ggplot(df_clusterses, aes(x=HV271.mean)) + geom_histogram()
ggplot(df_clusterses, aes(x=HV271.mean, y=HV271.median)) + geom_point()
names(df_clusterses) <- c("cluster_id", "hh.wealthindex.mean", "hh.wealthindex.median", "hh.wealthindex.sd")
#Merge cluster lat/long and SES info
df_cluster2012 <- merge(df_clusterlatlong, df_clusterses, by="cluster_id")

#2014
#Import cluster lat/long
df_clusterlatlong <- read.dbf("DHS/Senegal 2014/SNGE71FL/SNGE71FL.dbf") %>%
  dplyr::select(cluster_id = DHSCLUST, latitude = LATNUM, longitude = LONGNUM) %>%
  filter(latitude != 0 & longitude != 0)
#Import DHS data
df_hh <- read_sas("DHS/Senegal 2014/SNHR70SD/SNHR70FL.SAS7BDAT")
#Collapse DHS SES data to cluster level
ggplot(df_hh, aes(x=HV271)) + geom_histogram()
df_clusterses <- summaryBy(HV271 ~ HV001, FUN=c(mean,median,sd), data=df_hh)
ggplot(df_clusterses, aes(x=HV271.mean)) + geom_histogram()
ggplot(df_clusterses, aes(x=HV271.mean, y=HV271.median)) + geom_point()
names(df_clusterses) <- c("cluster_id", "hh.wealthindex.mean", "hh.wealthindex.median", "hh.wealthindex.sd")
#Merge cluster lat/long and SES info
df_cluster2014 <- merge(df_clusterlatlong, df_clusterses, by="cluster_id")

#2015
#Import cluster lat/long
df_clusterlatlong <- read.dbf("DHS/Senegal 2015/SNGE7AFL/SNGE7AFL.dbf") %>%
  dplyr::select(cluster_id = DHSCLUST, latitude = LATNUM, longitude = LONGNUM) %>%
  filter(latitude != 0 & longitude != 0)
#Import DHS data
df_hh <- read_sas("DHS/Senegal 2015/SNHR7HSD/SNHR7HFL.SAS7BDAT")
#Collapse DHS SES data to cluster level
ggplot(df_hh, aes(x=HV271)) + geom_histogram()
df_clusterses <- summaryBy(HV271 ~ HV001, FUN=c(mean,median,sd), data=df_hh)
ggplot(df_clusterses, aes(x=HV271.mean)) + geom_histogram()
ggplot(df_clusterses, aes(x=HV271.mean, y=HV271.median)) + geom_point()
names(df_clusterses) <- c("cluster_id", "hh.wealthindex.mean", "hh.wealthindex.median", "hh.wealthindex.sd")
#Merge cluster lat/long and SES info
df_cluster2015 <- merge(df_clusterlatlong, df_clusterses, by="cluster_id")

#2016
#Import cluster lat/long
df_clusterlatlong <- read.dbf("DHS/Senegal 2016/SNGE7IFL/SNGE7IFL.dbf") %>%
  dplyr::select(cluster_id = DHSCLUST, latitude = LATNUM, longitude = LONGNUM) %>%
  filter(latitude != 0 & longitude != 0)
#Import DHS data
df_hh <- read_sas("DHS/Senegal 2016/SNHR7ISD/SNHR7IFL.SAS7BDAT")
#Collapse DHS SES data to cluster level
ggplot(df_hh, aes(x=HV271)) + geom_histogram()
df_clusterses <- summaryBy(HV271 ~ HV001, FUN=c(mean,median,sd), data=df_hh)
ggplot(df_clusterses, aes(x=HV271.mean)) + geom_histogram()
ggplot(df_clusterses, aes(x=HV271.mean, y=HV271.median)) + geom_point()
names(df_clusterses) <- c("cluster_id", "hh.wealthindex.mean", "hh.wealthindex.median", "hh.wealthindex.sd")
#Merge cluster lat/long and SES info
df_cluster2016 <- merge(df_clusterlatlong, df_clusterses, by="cluster_id")

df_cluster <- bind_rows(df_cluster2012, df_cluster2014, df_cluster2015, df_cluster2016)
df_cluster$cluster_id <- seq(1:825)
library(ggmap)
senegalmap <- get_stamenmap(bbox = c(left = -18, bottom = 12, 
                                right = -11, top = 17),  
                       zoom = 8,
                       maptype = "toner-lite") 
ggmap(senegalmap, extent='device') +
  geom_point(data=df_cluster2012, aes(x=longitude, y=latitude, color="2012")) +
  geom_point(data=df_cluster2014, aes(x=longitude, y=latitude, color="2014")) +
  geom_point(data=df_cluster2015, aes(x=longitude, y=latitude, color="2015")) +
  geom_point(data=df_cluster2016, aes(x=longitude, y=latitude, color="2016"))
  

#Convert lat/long to coords in km

df_cluster[, c("xvar", "yvar")] <- latlong2grid( 
  df_cluster[, c("longitude", "latitude")]
)

senegal[, c("xvar", "yvar")] <- latlong2grid( 
  senegal[, c("longitude", "latitude")]
)

#Minimum Euclidean distance function

dist <- function(x1, y1, x2, y2) {
  ((x1-x2)^2 + (y1-y2)^2)^0.5
}

dist.merge <- function(x, y, xeast, xnorth, yeast, ynorth){
  tmp <- t(apply(x[,c(xeast, xnorth)], 1, function(x, y){
    dists <- apply(y, 1, function(x, y) dist(x[2],
                                             x[1], y[2], y[1]), x)
    cbind(1:nrow(y), dists)[dists == min(dists),,drop=F][1,]
  }
  , y[,c(yeast, ynorth)]))
  tmp <- cbind(x, min.dist=tmp[,2], cluster_id=tmp[,1])
  y <<- y
  row.names(tmp) <- NULL
  tmp
}

#Join data based on minimum Euclidean distance
df_cluster <- df_cluster %>%
  dplyr::select(-latitude, -longitude)
senegal <- dist.merge(senegal, df_cluster, 'xvar', 'yvar', 'xvar', 'yvar') 
senegal <- senegal %>%
  left_join(dplyr::select(y, c(-xvar, -yvar)), by="cluster_id")
ggplot(senegal, aes(x=min.dist)) + geom_histogram()
ggplot(senegal, aes(x=travel_time, y=hh.wealthindex.mean)) + geom_point() + geom_smooth()


saveRDS(senegal, "senegal.rds")
