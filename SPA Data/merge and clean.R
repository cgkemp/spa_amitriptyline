rm(list = ls())

library("tidyverse")
library("haven")
library("dplyr")

setwd("~/GitHub/spa_amitriptyline/SPA Data")

afghanistan <- readRDS("afghanistan.Rds")
bangladesh <- readRDS("bangladesh.Rds")
congo <- readRDS("congo.Rds")
haiti <- readRDS("haiti.Rds")
malawi <- readRDS("malawi.Rds")
nepal <- readRDS("nepal.Rds")
senegal2012 <- readRDS("senegal2012.Rds")
senegal2014 <- readRDS("senegal2014.Rds")
senegal2015 <- readRDS("senegal2015.Rds")
senegal2016 <- readRDS("senegal2016.Rds")
senegal2017 <- readRDS("senegal2017.Rds")
senegal2018 <- readRDS("senegal2018.Rds")
tanzania <- readRDS("tanzania.Rds")


df <- bind_rows(afghanistan, bangladesh, congo, haiti, malawi, nepal, senegal2012, senegal2014, senegal2015, senegal2016, senegal2017, senegal2017, senegal2018, tanzania)

df <- df %>%
  mutate("psychiatristsper100k" = case_when(
  country== 'Afghanistan' ~ 0.23,
  country== 'Bangladesh' ~ 0.13,
  country== 'Democratic Republic of the Congo' ~ 0.08,
  country== 'Haiti' ~ 0.07,
  country== 'Malawi' ~ 0.01,
  country== 'Nepal' ~ 0.36,
  country== 'Senegal' ~ 0.2,
  country== 'Tanzania' ~ 0.06)) %>%
  mutate("mhnursesper100k" = case_when(
    country== 'Afghanistan' ~ 0.1,
    country== 'Bangladesh' ~ 0.87,
    country== 'Democratic Republic of the Congo' ~ 0.5,
    country== 'Haiti' ~ 0.2,
    country== 'Malawi' ~ 0.22,
    country== 'Nepal' ~ 0.56,
    country== 'Senegal' ~ 0.27,
    country== 'Tanzania' ~ 0.36)) %>%
  mutate("psychologistsper100k" = case_when(
    country== 'Afghanistan' ~ 0.3,
  country== 'Bangladesh' ~ 0.12,
  country== 'Democratic Republic of the Congo' ~ 0.02,
  country== 'Haiti' ~ 0.56,
  country== 'Malawi' ~ 0.02,
  country== 'Nepal' ~ 0.52,
  country== 'Senegal' ~ 0.02,
  country== 'Tanzania' ~ 0.01)) %>%
mutate("mddpercentprevalence" = case_when(
  country== 'Afghanistan' ~ 2.387806,
  country== 'Bangladesh' ~ 2.7939406,
  country== 'Democratic Republic of the Congo' ~ 2.25458,
  country== 'Haiti' ~ 1.8508355,
  country== 'Malawi' ~ 1.7241404,
  country== 'Nepal' ~ 2.1452084,
  country== 'Senegal' ~ 1.7020095,
  country== 'Tanzania' ~ 1.9252937)) %>%
 mutate(quarter = case_when(
    month== 1 ~ 1,
    month== 2 ~ 1,
    month== 3 ~ 1,
    month== 4 ~ 2,
    month== 5 ~ 2,
    month== 6 ~ 2,
    month== 7 ~ 3,
    month== 8 ~ 3,
    month== 9 ~ 3,
    month== 10 ~ 4,
    month== 11 ~ 4,
    month== 12 ~ 4)) %>%
  mutate("country_code" = case_when(
    country== 'Afghanistan' ~ 1,
    country== 'Nepal' ~ 2,
    country== 'Bangladesh' ~ 3,
    country== 'Tanzania' ~ 4,
    country== 'Senegal' ~ 5,
    country== 'Haiti' ~ 6,
    country== 'Democratic Republic of the Congo' ~ 7,
    country== 'Malawi' ~ 8)) %>%
 mutate(month=factor(month),
        year=factor(year),
        ownership=factor(ownership, levels = c(1,2,3,4), labels = c("Government/Public", "NGO/Private Not-For-Profit", "Private-For-Profit", "Mission/Faith-Based")),
        facility_type=factor(facility_type),
        country=factor(country),
        worldbank=factor(worldbank)) %>%
  mutate(staffcat = cut(total_staff, breaks=c(-Inf, 5, 10, 25, Inf)))

df$ownership[df$ownership == 9] <- NA ##Fix 1 facility in DRC

library(foreign)
write.dta(df, "clean_stata.dta")

saveRDS(df, "clean_R.Rds")
