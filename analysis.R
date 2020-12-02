df$PowerSource <-as.factor(df$Power_Source)
df$EmailInternet <-as.factor(df$Email_or_Internet_Access)
df$Staff <-as.factor(df$Staff)
df$Sanitation <-as.factor(df$Sanitation)
df$Amitriptyline <-as.factor(df$Amitriptyline)
df$MonthGrouped <-as.factor(df$Month_Grouped)
df$Nation <- as.factor(df$Nation)
df$Computer <- as.factor(df$Computer)
df$WaterSource <-as.factor(df$Water_Source)

df$mdd_std <- (df$Major_depressive_disorder - mean(df$Major_depressive_disorder)) / sd(df$Major_depressive_disorder)
df$psychiatrists_std <- (df$Psychiatrists - mean(df$Psychiatrists)) / sd(df$Psychiatrists)

##Table 1
library(tableone)

df_tableone <- subset(df, !is.na(Amitriptyline))

vars <- c("Staff", "Ownership", "Facility", "PowerSource", "WaterSource", "Sanitation", "Computer", "EmailInternet", "Nation", "OPD_Room_Privacy")
cat_vars <- c("Staff", "Ownership", "Facility", "PowerSource", "WaterSource", "Sanitation", "Computer", "EmailInternet",  "Nation", "OPD_Room_Privacy")

table_one <- CreateTableOne(data = df_tableone, vars=vars,  factorVars = cat_vars, strata=c("Amitriptyline"), addOverall=T)
table_one.tableMat <- print(table_one, quote = FALSE, noSpaces = TRUE, printToggle = FALSE)
write.csv(table_one.tableMat, file = "table_one.csv")

library('lme4')

formula1 <- (Amitriptyline) ~ Staff + Ownership + Facility +
  (1 | Nation)

formula2 <- (Amitriptyline) ~ Staff + Ownership + Facility + 
  PowerSource + WaterSource + Sanitation + Computer + EmailInternet +
  (1 | Nation)

formula3 <- (Amitriptyline) ~ Staff + Ownership + Facility + 
  PowerSource + WaterSource + Sanitation + Computer + EmailInternet +
  WorldBank_Classification + mdd_std + psychiatrists_std   +
  (1 | Nation)

formulas <- list(formula1, formula2, formula3)
               
models <- lapply(formulas, function(f) glmer(f, family= binomial(link = "logit"), data = df))

library(sjPlot)
tab_model(models[[1]], models[[2]], models[[3]], file="regressiontable.html")
