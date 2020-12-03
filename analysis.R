rm(list = ls())

library(tidyverse)
library(haven)
library(dplyr)
library(tableone)

setwd("~/GitHub/spa_amitriptyline/SPA Data")

df <- readRDS("clean_R.Rds")


#df$PowerSource <-as.factor(df$Power_Source)
#df$EmailInternet <-as.factor(df$Email_or_Internet_Access)
#df$Staff <-as.factor(df$Staff)
#df$Sanitation <-as.factor(df$Sanitation)
#df$Amitriptyline <-as.factor(df$Amitriptyline)
#df$MonthGrouped <-as.factor(df$Month_Grouped)
#df$country <- as.factor(df$country)
#df$Computer <- as.factor(df$Computer)
#df$WaterSource <-as.factor(df$Water_Source)

df$mddpercentprevalence_std <- (df$mddpercentprevalence - mean(na.omit(df$mddpercentprevalence))) / sd(na.omit(df$mddpercentprevalence))
df$psychiatristsper100k_std <- (df$psychiatristsper100k - mean(na.omit(df$psychiatristsper100k))) / sd(na.omit(df$psychiatristsper100k))

##Table 1

df_tableone <- subset(df, !is.na(amitriptyline))

vars <- c("staffcat", "ownership", "facility_type", "power", "improved_water", "improved_sanitation", "computer", "email", "country", "general_opd_private_room")
cat_vars <- c("staffcat", "ownership", "facility_type", "power", "improved_water", "improved_sanitation", "computer", "email", "country", "general_opd_private_room")

table_one <- CreateTableOne(data = df_tableone, vars=vars,  factorVars = cat_vars, strata=c("amitriptyline"), addOverall=T)
table_one.tableMat <- print(table_one, quote = FALSE, noSpaces = TRUE, printToggle = FALSE)
write.csv(table_one.tableMat, file = "table_one.csv")

library('lme4')

formula1 <- (amitriptyline) ~ staffcat + ownership + facility_type +
  (1 | country)

formula2 <- (amitriptyline) ~ staffcat + ownership + facility_type +
  power + improved_water + improved_sanitation + computer + email +
  (1 | country)

formula3 <- (amitriptyline) ~ staffcat + ownership + facility_type +
  power + improved_water + improved_sanitation + computer + email +
  worldbank + mddpercentprevalence_std + psychiatristsper100k_std   +
  (1 | country)

formulas <- list(formula1, formula2, formula3)

models <- lapply(formulas, function(f) glmer(f, family= binomial(link = "logit"), data = df))

library(sjPlot)
tab_model(models[[1]], models[[2]], models[[3]], file="regressiontable.html")
