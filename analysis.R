rm(list = ls())

library(tidyverse)
library(haven)
library(dplyr)
library(tableone)
library(ggplot2)
library(ggmap)
library(ggthemes)
library(ggpubr)
library(scales)
library(gridExtra)

setwd("~/GitHub/spa_amitriptyline/SPA Data")

df <- readRDS("clean_R.Rds")
levels(df$country)[levels(df$country)=="Democratic Republic of the Congo"] <- "DRC"
    
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

##Exploratory Figures
#Proportion by ownership and country
ownership_country <- subset(df, !is.na(amitriptyline)) %>% 
  group_by(country, ownership) %>% 
  summarize(mean = mean(amitriptyline), se = sd(amitriptyline) / sqrt(n()))
plot_ownership_country <- ggplot(ownership_country, 
                                 aes(y=mean, x=ownership, ymin = mean - 1.96*se, ymax = mean + 1.96*se)) +
  geom_point() + 
  geom_errorbar(width=0.2) +
  scale_y_continuous(labels = percent, limits=c(0,1)) +
  facet_grid(country ~ .) +
  theme_bw()

#Proportion by type and country
type_country <- subset(df, !is.na(amitriptyline)) %>% 
  group_by(country, facility_type) %>% 
  summarize(mean = mean(amitriptyline), se = sd(amitriptyline) / sqrt(n()))
plot_type_country <- ggplot(type_country, 
                                 aes(y=mean, x=facility_type, ymin = mean - 1.96*se, ymax = mean + 1.96*se)) +
  geom_point() + 
  geom_errorbar(width=0.2) +
  scale_y_continuous(labels = percent, limits=c(0,1)) +
  facet_grid(country ~ .) +
  theme_bw()

plot_travel_ownership <- ggplot(data=subset(df, !is.na(amitriptyline)), aes(x=ownership, fill=as.factor(amitriptyline))) +
  geom_bar(position="fill") +
  facet_grid(country ~ .) +
  theme_bw()

#Proportion by wealth and country
wealth_country <- subset(df, !is.na(amitriptyline)) %>% 
  group_by(country, hhwealth_quartile) %>% 
  summarize(mean = mean(amitriptyline), se = sd(amitriptyline) / sqrt(n()))
plot_wealth_country <- ggplot(wealth_country, 
                                 aes(y=mean, x=hhwealth_quartile, ymin = mean - 1.96*se, ymax = mean + 1.96*se)) +
  geom_point() + 
  geom_errorbar(width=0.2) +
  scale_y_continuous(labels = percent, limits=c(0,1)) +
  facet_grid(country ~ .) +
  theme_bw()

#Proportion by travel time and country
travel_country <- subset(df, !is.na(amitriptyline)) %>% 
  group_by(country, travel_quartile) %>% 
  summarize(mean = mean(amitriptyline), se = sd(amitriptyline) / sqrt(n()))
plot_travel_country <- ggplot(travel_country, 
                              aes(y=mean, x=travel_quartile, ymin = mean - 1.96*se, ymax = mean + 1.96*se)) +
  geom_point() + 
  geom_errorbar(width=0.2) +
  scale_y_continuous(labels = percent, limits=c(0,1)) +
  facet_grid(country ~ .) +
  theme_bw()

#Proportion by travel time and ownership and country
travel_ownership_country <- subset(df, !is.na(amitriptyline)) %>% 
  group_by(country, travel_quartile, ownership) %>% 
  summarize(mean = mean(amitriptyline), se = sd(amitriptyline) / sqrt(n()))
plot_travel_ownership_country <- ggplot(travel_ownership_country, 
                              aes(y=mean, x=travel_quartile, ymin = mean - 1.96*se, ymax = mean + 1.96*se)) +
  geom_point() + 
  geom_errorbar(width=0.2) +
 # scale_y_continuous(labels = percent, limits=c(0,1)) +
  facet_grid(country ~ ownership) +
  theme_bw()

#Proportion by facility type and ownership and country
type_ownership_country <- subset(df, !is.na(amitriptyline)) %>% 
  group_by(country, facility_type, ownership) %>% 
  summarize(mean = mean(amitriptyline), se = sd(amitriptyline) / sqrt(n()))
plot_type_ownership_country <- ggplot(type_ownership_country, 
                              aes(y=mean, x=facility_type, ymin = pmax(mean - 1.96*se), ymax = mean + 1.96*se)) +
  geom_point() + 
  geom_errorbar(width=0.2) +
  # scale_y_continuous(labels = percent, limits=c(0,1)) +
  facet_grid(country ~ ownership) +
  xlab("Facility Type") +
  ylab("Proportion of Facilities with Amitriptyline") +
  scale_y_continuous(labels=percent) +
  scale_x_discrete(labels = c('Hospital','Primary Care','Other')) +
  coord_cartesian(ylim=c(0,1)) +
  theme_bw()

##Travel Time by Amitryptiline
plot_travel_ownership <- ggplot(data=subset(df, !is.na(amitriptyline)), aes(x=travel_time, y=amitriptyline)) +
  geom_point() +
  geom_smooth(na.rm=TRUE) +
  # ylim(0,1) +
  facet_grid(. ~ ownership) +
  theme_bw()

plot_travel_type <- ggplot(data=subset(df, !is.na(amitriptyline)), aes(x=travel_time, y=amitriptyline)) +
  geom_point() +
  geom_smooth(na.rm=TRUE) +
  # ylim(0,1) +
  facet_grid(. ~ facility_type) +
  theme_bw()

plot_travel_ownership_country <- ggplot(data=subset(df, !is.na(amitriptyline)), aes(x=travel_time, y=amitriptyline)) +
  geom_point() +
  geom_smooth(na.rm=TRUE) +
 # ylim(0,1) +
  facet_grid(country ~ ownership) +
  theme_bw()

plot_travel_type_country <- ggplot(data=subset(df, !is.na(amitriptyline)), aes(x=travel_decile, y=amitriptyline)) +
  geom_point() +
  geom_smooth(na.rm=TRUE) +
  # ylim(0,1) +
  facet_grid(country ~ facility_type) +
  theme_bw()

plot_wealth_type_country <- ggplot(data=subset(df, !is.na(amitriptyline)), aes(x=hhwealth_decile, y=amitriptyline)) +
  geom_point() +
  geom_smooth(na.rm=TRUE) +
  # ylim(0,1) +
  facet_grid(country ~ facility_type) +
  theme_bw()

##Travel Time by HH Wealth
plot_travel_wealth <- ggplot(data=df, aes(x=travel_decile, y=hhwealth_decile)) +
  geom_point() +
  geom_smooth(na.rm=TRUE) +
  facet_grid(country ~ .) +
  theme_bw()


plot_type_ownership_country <- ggplot(subset(df, !is.na(amitriptyline)), aes(y=amitriptyline, x=facility_type)) +
  stat_summary(fun=mean, geom="point") +
  stat_summary(fun.data=mean_cl_boot, geom="errorbar", width=0.2) +
  facet_grid(country ~ ownership) +
  xlab("Facility Type") +
  ylab("Proportion of Facilities with Amitriptyline") +
  scale_y_continuous(labels=percent) +
  scale_x_discrete(labels = c('Hospital','Primary','Other')) +
  theme_bw()
ggsave("plot_type_ownership_country.png", plot_type_ownership_country, height=9, width=8)


##Maps
afghanistan <- get_stamenmap(bbox = c(left = 60, bottom = 29, 
                                right = 76, top = 39),  
                       zoom = 6,
                       maptype = "toner-lite") 
afghanistan_map <- ggmap(afghanistan) +
  geom_point(data=subset(df, !is.na(amitriptyline) & country=="Afghanistan"), aes(x=longitude, y=latitude, color=as.factor(amitriptyline), shape=facility_type), size=2.5, alpha=0.7) +
  scale_colour_brewer(palette = "Set1") +
  ggtitle("Afghanistan") +
  theme(legend.position = "none",
        axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks.x=element_blank(),
        axis.ticks.y=element_blank(),
        panel.border=element_rect(fill=NA, color="black"))

bangladesh <- get_stamenmap(bbox = c(left = 88, bottom = 20, 
                                      right = 93, top = 27),  
                             zoom = 7,
                             maptype = "toner-lite") 
bangladesh_map <- ggmap(bangladesh) +
  geom_point(data=subset(df, !is.na(amitriptyline) & country=="Bangladesh"), aes(x=longitude, y=latitude, color=as.factor(amitriptyline), shape=facility_type), size=2.5, alpha=0.7) +
  scale_colour_brewer(palette = "Set1") +
  ggtitle("Bangladesh") +
  theme(legend.position = "none",
        axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks.x=element_blank(),
        axis.ticks.y=element_blank(),
        panel.border=element_rect(fill=NA, color="black"))

congo <- get_stamenmap(bbox = c(left = 12, bottom = -15, 
                                right = 33, top = 6),  
                       zoom = 6,
                       maptype = "toner-lite") 
congo_map <- ggmap(congo) +
  geom_point(data=subset(df, !is.na(amitriptyline) & country=="DRC"), aes(x=longitude, y=latitude, color=as.factor(amitriptyline), shape=facility_type), size=2.5, alpha=0.7) +
  scale_colour_brewer(palette = "Set1") +
  ggtitle("DRC") +
  theme(legend.position = "none",
        axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks.x=element_blank(),
        axis.ticks.y=element_blank(),
        panel.border=element_rect(fill=NA, color="black"))

haiti <- get_stamenmap(bbox = c(left = -74.5, bottom = 18, 
                                right = -71.5, top = 20.1),  
                       zoom = 9,
                       maptype = "toner-lite") 
haiti_map <- ggmap(haiti) +
  geom_point(data=subset(df, !is.na(amitriptyline)), aes(x=longitude, y=latitude, color=as.factor(amitriptyline), shape=facility_type), size=2.5, alpha=0.7) +
  scale_colour_brewer(palette = "Set1") +
  ggtitle("Haiti") +
    theme(legend.position = "none",
        axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks.x=element_blank(),
        axis.ticks.y=element_blank(),
        panel.border=element_rect(fill=NA, color="black"))

malawi <- get_stamenmap(bbox = c(left = 32.5, bottom = -17.5, 
                                     right = 36, top = -9),  
                            zoom = 7,
                            maptype = "toner-lite") 
malawi_map <- ggmap(malawi) +
  geom_point(data=subset(df, !is.na(amitriptyline) & country=="Malawi"), aes(x=longitude, y=latitude, color=as.factor(amitriptyline), shape=facility_type), size=2.5, alpha=0.7) +
  scale_colour_brewer(palette = "Set1") +
  ggtitle("Malawi") +
  theme(legend.position = "none",
        axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks.x=element_blank(),
        axis.ticks.y=element_blank(),
        panel.border=element_rect(fill=NA, color="black"))

senegal <- get_stamenmap(bbox = c(left = -18, bottom = 12, 
                                   right = -11, top = 17),  
                          zoom = 7,
                          maptype = "toner-lite") 
senegal_map <- ggmap(senegal) +
  geom_point(data=subset(df, !is.na(amitriptyline) & country=="Senegal"), aes(x=longitude, y=latitude, color=as.factor(amitriptyline), shape=facility_type), size=2.5, alpha=0.7) +
  scale_colour_brewer(palette = "Set1") +
  ggtitle("Senegal") +
    theme(legend.position = "none",
        axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks.x=element_blank(),
        axis.ticks.y=element_blank(),
        panel.border=element_rect(fill=NA, color="black"))


tanzania <- get_stamenmap(bbox = c(left = 29.5, bottom = -12, 
                                 right = 40.5, top = -0.5),  
                        zoom = 7,
                        maptype = "toner-lite") 
tanzania_map <- ggmap(tanzania) +
  geom_point(data=subset(df, !is.na(amitriptyline) & country=="Tanzania"), aes(x=longitude, y=latitude, color=as.factor(amitriptyline), shape=facility_type), size=2.5, alpha=0.7) +
  scale_colour_brewer(palette = "Set1") +
  ggtitle("Tanzania") +
    theme(legend.position = "none",
        axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks.x=element_blank(),
        axis.ticks.y=element_blank(),
        panel.border=element_rect(fill=NA, color="black"))

legend_map <- ggmap(tanzania) +
  geom_point(data=subset(df, !is.na(amitriptyline) & country=="Tanzania"), aes(x=longitude, y=latitude, color=as.factor(amitriptyline), shape=facility_type), size=2.5, alpha=0.7) +
  scale_colour_brewer(palette = "Set1", name = "Amitriptyline Availability", labels = c("Not Available", "Available")) +
  scale_shape_discrete(name = "Facility Type", labels = c("Hospital", "Primary", "Other")) +
  ggtitle("Tanzania")

legend <- get_legend(legend_map)

all_maps <- ggarrange(congo_map, 
                      tanzania_map,
                      bangladesh_map,
                      malawi_map,
                      afghanistan_map,
                      haiti_map,
                      senegal_map,
                      legend,
          ncol=4, nrow=2)
ggsave("all_maps.png", all_maps, height=7, width=12)

maps <- arrangeGrob(congo_map, 
                    afghanistan_map,
                    bangladesh_map,
                    malawi_map,
                    tanzania_map,
                    haiti_map,
                    senegal_map,
                    nrow = 3)
ggsave("maps.png", maps, height=12, width=12)

##Table 1

df_tableone <- subset(df, !is.na(amitriptyline))

vars <- c("staffcat", "ownership", "facility_type", "power", "improved_water", "improved_sanitation", "computer", "email", "country", "general_opd_private_room", "travel_quartile", "hhwealth_quartile")
cat_vars <- c("staffcat", "ownership", "facility_type", "power", "improved_water", "improved_sanitation", "computer", "email", "country", "general_opd_private_room", "travel_quartile", "hhwealth_quartile")

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

formula4 <- (amitriptyline) ~ staffcat + ownership + facility_type +
  power + improved_water + improved_sanitation + computer + email +
  worldbank + mddpercentprevalence_std + psychiatristsper100k_std +
  travel_quartile +
  (1 | country)

formula5 <- (amitriptyline) ~ staffcat + ownership + facility_type +
  power + improved_water + improved_sanitation + computer + email +
  worldbank + mddpercentprevalence_std + psychiatristsper100k_std   +
  travel_quartile + hhwealth_quartile +
  (1 | country)


formulas <- list(formula1, formula2, formula3, formula4, formula5)

models <- lapply(formulas, function(f) glmer(f, family= binomial(link = "logit"), data = df))

library(sjPlot)
tab_model(models[[1]], models[[2]], models[[3]],  models[[4]], models[[5]], file="regressiontable.html")

countries <- list("Afghanistan", "Bangladesh", "DRC", "Haiti", "Malawi", "Nepal", "Senegal", "Tanzania")
country_formula <- (amitriptyline) ~ staffcat + ownership + facility_type +
  power + improved_water + improved_sanitation + computer + email
country_models <- lapply(countries, function(f) glm(country_formula, family= binomial(link = "logit"), data=subset(df, country==f)))
tab_model(country_models[[1]], country_models[[2]], country_models[[3]],  country_models[[4]], country_models[[5]], 
          country_models[[6]], country_models[[7]], country_models[[8]],file="country_regressions.html")
country_coefs <- as.data.frame(do.call(rbind, lapply(country_models, function(x) coef(x)/coef(summary(x))[,2])))
rownames(country_coefs) <- countries
country_coefs <- country_coefs %>%
  rownames_to_column() %>%
  dplyr::rename(country = rowname) %>%
  gather(var, logodds, `(Intercept)`:email)

heatmap <- ggplot(country_coefs, aes(x=country, y=var, fill=logodds)) +
  geom_tile() +
  scale_colour_brewer(palette = "Set1") +
  scale_fill_distiller(palette = "RdPu") +
  theme_bw()