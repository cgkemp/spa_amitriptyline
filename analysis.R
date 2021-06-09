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
library(survey)
library(srvyr)
library(lme4)
library(svylme)
library(sjPlot)
library(parameters)

setwd("~/GitHub/spa_amitriptyline/SPA Data")

df <- readRDS("clean_R.Rds")
levels(df$country)[levels(df$country)=="Democratic Republic of the Congo"] <- "DRC"

#Drop Afghanistan and facilities missing weights
df <- df %>%
  filter(country!="Afghanistan",
         !is.na(amitriptyline) )
  


df$mddpercentprevalence_std <- (df$mddpercentprevalence - mean(na.omit(df$mddpercentprevalence))) / sd(na.omit(df$mddpercentprevalence))
df$psychiatristsper100k_std <- (df$psychiatristsper100k - mean(na.omit(df$psychiatristsper100k))) / sd(na.omit(df$psychiatristsper100k))

df_survey <- df %>%
  mutate(facility_weight = facility_weight/1000000) %>%
  as_survey_design(weights = facility_weight)

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
plot_travel_ownership <- ggplot(data=subset(df, !is.na(amitriptyline)), aes(x=travel_hours, y=amitriptyline)) +
  geom_point() +
  geom_smooth(na.rm=TRUE) +
  # ylim(0,1) +
  #facet_grid(. ~ ownership) +
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
  stat_summary(fun.data=mean_cl_boot, geom="text", size=3, hjust=-.4,
               aes(label=paste0(round(..y.. * 100, 0),"%"))) +
  facet_grid(country ~ ownership, margins=TRUE) +
  xlab("Facility Type") +
  ylab("Proportion of Facilities with Amitriptyline") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1L)) +
  scale_x_discrete(labels = c('Hospital','Primary','Other')) +
  theme_bw()
ggsave("plot_type_ownership_country.png", plot_type_ownership_country, height=9, width=9)

plot_type_ownership <- ggplot(subset(df, !is.na(amitriptyline)), aes(y=amitriptyline, x=facility_type)) +
  stat_summary(fun=mean, geom="point") +
  stat_summary(fun.data=mean_cl_boot, geom="errorbar", width=0.2) +
  stat_summary(fun.data=mean_cl_boot, geom="text", size=3, hjust=-.4,
               aes(label=paste0(round(..y.. * 100, 0),"%"))) +
  facet_grid(. ~ ownership, margins=TRUE) +
  xlab("Facility Type") +
  ylab("Proportion of Facilities with Amitriptyline") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1L)) +
  scale_x_discrete(labels = c('Hospital','Primary','Other')) +
  theme_bw()
ggsave("plot_type_ownership.png", plot_type_ownership, height=4, width=10)



####Final figure: facility type by ownership by country, mean and 95% CI

df_figure_ami <- df_survey %>%
  filter(!is.na(amitriptyline)) %>%
  dplyr::select(country, facility_weight, facility_type, ownership, amitriptyline, diazepam) %>%
  group_by(facility_type) %>% 
  summarise(mean = survey_mean(amitriptyline, proportion=TRUE, vartype = c("ci"))) %>%
  mutate(var = "Amitriptyline",
         ownership = as.factor("(all)"),
         country = as.factor("(all)"))
df_figure_dia <- df_survey %>%
  filter(!is.na(amitriptyline)) %>%
  dplyr::select(country, facility_weight, facility_type, ownership, amitriptyline, diazepam) %>%
  group_by(facility_type) %>% 
  summarise(mean = survey_mean(diazepam, proportion=TRUE, vartype = c("ci"))) %>%
  mutate(var = "Diazepam",
         ownership = as.factor("(all)"),
         country = as.factor("(all)"))
df_figure_ami_country <- df_survey %>%
  filter(!is.na(amitriptyline)) %>%
  dplyr::select(country, facility_weight, facility_type, ownership, amitriptyline, diazepam) %>%
  group_by(facility_type, country) %>% 
  summarise(mean = survey_mean(amitriptyline, proportion=TRUE, vartype = c("ci"))) %>%
  mutate(var = "Amitriptyline",
         ownership = "(all)")
df_figure_dia_country <- df_survey %>%
  filter(!is.na(amitriptyline)) %>%
  dplyr::select(country, facility_weight, facility_type, ownership, amitriptyline, diazepam) %>%
  group_by(facility_type, country) %>% 
  summarise(mean = survey_mean(diazepam, proportion=TRUE, vartype = c("ci"))) %>%
  mutate(var = "Diazepam",
         ownership = "(all)")
df_figure_ami_ownership <- df_survey %>%
  filter(!is.na(amitriptyline)) %>%
  dplyr::select(country, facility_weight, facility_type, ownership, amitriptyline, diazepam) %>%
  group_by(facility_type, ownership) %>% 
  summarise(mean = survey_mean(amitriptyline, proportion=TRUE, vartype = c("ci"))) %>%
  mutate(var = "Amitriptyline",
         country = "(all)")
df_figure_dia_ownership <- df_survey %>%
  filter(!is.na(amitriptyline)) %>%
  dplyr::select(country, facility_weight, facility_type, ownership, amitriptyline, diazepam) %>%
  group_by(facility_type, ownership) %>% 
  summarise(mean = survey_mean(diazepam, proportion=TRUE, vartype = c("ci"))) %>%
  mutate(var = "Diazepam",
         country = "(all)")
df_figure_ami_ownership_country <- df_survey %>%
  filter(!is.na(amitriptyline)) %>%
  dplyr::select(country, facility_weight, facility_type, ownership, amitriptyline, diazepam) %>%
  group_by(country, facility_type, ownership) %>% 
  summarise(mean = survey_mean(amitriptyline, proportion=TRUE, vartype = c("ci"))) %>%
  mutate(var = "Amitriptyline")
df_figure_dia_ownership_country <- df_survey %>%
  filter(!is.na(amitriptyline)) %>%
  dplyr::select(country, facility_weight, facility_type, ownership, amitriptyline, diazepam) %>%
  group_by(country, facility_type, ownership) %>% 
  summarise(mean = survey_mean(diazepam, proportion=TRUE, vartype = c("ci"))) %>%
  mutate(var = "Diazepam")

df_figure <- bind_rows(df_figure_ami, df_figure_dia, df_figure_ami_country, df_figure_dia_country, df_figure_ami_ownership, df_figure_dia_ownership, df_figure_ami_ownership_country, df_figure_dia_ownership_country) %>%
  mutate(country_f = factor(country, levels=c('Bangladesh','DRC', 'Haiti', "Malawi", 'Nepal', 'Senegal', 'Tanzania', '(all)')),
         ownership_f = factor(ownership, levels=c('Government/Public','NGO/Private Not-For-Profit','Private-For-Profit','Mission/Faith-Based', '(all)')))

pd <- position_dodge(.8)

plot_type_ownership_country_ami <- ggplot(subset(df_figure, var=="Amitriptyline"), aes(y=mean, x=facility_type)) +
  geom_point() +
  geom_errorbar(aes(ymin=mean_low, ymax=mean_upp), width=0.2) +
  geom_text(size=3, hjust=-.4,
               aes(y=label=paste0(round(..y.. * 100, 0),"%"))) +
  facet_grid(country_f ~ ownership_f) +
  xlab("Facility Type") +
  ylab("Proportion of Facilities with Amitriptyline") + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 1L)) +
  scale_x_discrete(labels = c('Hospital','Primary','Other')) +
  theme_bw()
plot_type_ownership_country_dia <- ggplot(subset(df_figure, var=="Diazepam"), aes(y=mean, x=facility_type)) +
  geom_point() +
  geom_errorbar(aes(ymin=mean_low, ymax=mean_upp), width=0.2) +
  geom_text(size=3, hjust=-.4,
            aes(label=paste0(round(..y.. * 100, 0),"%"))) +
  facet_grid(country_f ~ ownership_f) +
  xlab("Facility Type") +
  ylab("Proportion of Facilities with Diazepam") + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 1L)) +
  scale_x_discrete(labels = c('Hospital','Primary','Other')) +
  theme_bw()

plot_type_ownership_country <- ggplot(df_figure, aes(y=mean, x=facility_type, color=var)) +
  geom_point(position=pd) +
  geom_errorbar(position=pd, aes(ymin=mean_low, ymax=mean_upp), width=0.2) +
  geom_text(position=pd, size=3, hjust=-.4, show.legend = FALSE,
            aes(label=paste0(round(..y.. * 100, 0),"%"))) +
  facet_grid(country_f ~ ownership_f) +
  xlab("Facility Type") +
  ylab("Proportion of Facilities with Medicine Available") + 
  scale_colour_manual(values=c("#ff0000", "#0000ff"), name = "Medicine") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1L)) +
  scale_x_discrete(labels = c('Hospital','Primary','Other')) +
  theme_bw()
 ggsave("plot_type_ownership_country_both.png", plot_type_ownership_country, height=10, width=16)


##Maps
df <- df %>%
  mutate(ami_dia = case_when(
    amitriptyline == 0 & diazepam == 0 ~ "Neither",
    amitriptyline == 1 & diazepam == 0 ~ "Only Amitriptyline",
    amitriptyline == 0 & diazepam == 1 ~ "Only Diazepam",
    amitriptyline == 1 & diazepam == 1 ~ "Both"),
    ami_dia_f = factor(ami_dia, levels=c('Neither','Only Amitriptyline','Only Diazepam','Both')))


afghanistan <- get_stamenmap(bbox = c(left = 60, bottom = 29, 
                                right = 76, top = 39),  
                       zoom = 6,
                       maptype = "toner-lite") 
afghanistan_map <- ggmap(afghanistan) +
  geom_point(data=subset(df, !is.na(amitriptyline) & country=="Afghanistan"), aes(x=longitude, y=latitude, color=ami_dia_f, shape=facility_type), size=1.5, alpha=0.7) +
  scale_colour_manual(values=c("#ff0000", "#87cefa", "#00ff00", "#0000ff")) +
  scale_shape_manual(values=c(2, 1, 5)) +
    ggtitle("Afghanistan") +
  theme(legend.position = "none",
        axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks.x=element_blank(),
        axis.ticks.y=element_blank(),
        panel.border=element_rect(fill=NA, color="black"))

bangladesh <- get_stamenmap(bbox = c(left = 87, bottom = 20, 
                                      right = 94, top = 27),  
                             zoom = 7,
                             maptype = "toner-lite") 
bangladesh_map <- ggmap(bangladesh) +
  geom_point(data=subset(df, !is.na(amitriptyline) & country=="Bangladesh"), aes(x=longitude, y=latitude, color=ami_dia_f, shape=facility_type), size=1.5, alpha=0.7) +
  scale_colour_manual(values=c("#ff0000", "#87cefa", "#00ff00", "#0000ff")) +
  scale_shape_manual(values=c(17, 16, 15)) +
    ggtitle("Bangladesh (2017)") +
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
  geom_point(data=subset(df, !is.na(amitriptyline) & country=="DRC"), aes(x=longitude, y=latitude, color=ami_dia_f, shape=facility_type), size=1.5, alpha=0.7) +
  scale_colour_manual(values=c("#ff0000", "#87cefa", "#00ff00", "#0000ff")) +
  scale_shape_manual(values=c(17, 16, 15)) +
    ggtitle("DRC (2017-2018)") +
  theme(legend.position = "none",
        axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks.x=element_blank(),
        axis.ticks.y=element_blank(),
        panel.border=element_rect(fill=NA, color="black"))

haiti <- get_stamenmap(bbox = c(left = -74.5, bottom = 17.55, 
                                right = -71.5, top = 20.55),  
                       zoom = 9,
                       maptype = "toner-lite") 
haiti_map <- ggmap(haiti) +
  geom_point(data=subset(df, !is.na(amitriptyline) & country=="Haiti"), aes(x=longitude, y=latitude, color=ami_dia_f, shape=facility_type), size=1.5, alpha=0.7) +
  scale_colour_manual(values=c("#ff0000", "#87cefa", "#00ff00", "#0000ff")) +
  scale_shape_manual(values=c(17, 16, 15)) +
    ggtitle("Haiti (2017)") +
    theme(legend.position = "none",
        axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks.x=element_blank(),
        axis.ticks.y=element_blank(),
        panel.border=element_rect(fill=NA, color="black"))

malawi <- get_stamenmap(bbox = c(left = 30, bottom = -17.5, 
                                     right = 38.5, top = -9),  
                            zoom = 7,
                            maptype = "toner-lite") 
malawi_map <- ggmap(malawi) +
  geom_point(data=subset(df, !is.na(amitriptyline) & country=="Malawi"), aes(x=longitude, y=latitude, color=ami_dia_f, shape=facility_type), size=1.5, alpha=0.7) +
  scale_colour_manual(values=c("#ff0000", "#87cefa", "#00ff00", "#0000ff")) +
  scale_shape_manual(values=c(17, 16, 15)) +
    ggtitle("Malawi (2013-2014)") +
  theme(legend.position = "none",
        axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks.x=element_blank(),
        axis.ticks.y=element_blank(),
        panel.border=element_rect(fill=NA, color="black"))

nepal <- get_stamenmap(bbox = c(left = 80, bottom = 24, 
                                 right = 88.5, top = 32.5),  
                        zoom = 7,
                        maptype = "toner-lite") 
nepal_map <- ggmap(nepal) +
  geom_point(data=subset(df, !is.na(amitriptyline) & country=="Nepal"), aes(x=longitude, y=latitude, color=ami_dia_f, shape=facility_type), size=1.5, alpha=0.7) +
  scale_colour_manual(values=c("#ff0000", "#87cefa", "#00ff00", "#0000ff")) +
  scale_shape_manual(values=c(17, 16, 15)) +
    ggtitle("Nepal (2015)") +
  theme(legend.position = "none",
        axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks.x=element_blank(),
        axis.ticks.y=element_blank(),
        panel.border=element_rect(fill=NA, color="black"))
nepal2 <- get_stamenmap(bbox = c(left = 80, bottom = 26, 
                                right = 88.5, top = 30.5),  
                       zoom = 8,
                       maptype = "toner-lite") 
nepal_map2 <- ggmap(nepal2) +
  geom_point(data=subset(df, !is.na(amitriptyline) & country=="Nepal"), aes(x=longitude, y=latitude, color=ami_dia_f, shape=facility_type), size=4, alpha=0.7) +
  scale_colour_manual(values=c("#ff0000", "#87cefa", "#00ff00", "#0000ff"), name = "Medicine Availability") +
  scale_shape_manual(name = "Facility Type", labels = c("Hospital", "Primary", "Other"), values=c(17, 16, 15)) +
  ggtitle("Nepal (2015)") +
  theme(axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks.x=element_blank(),
        axis.ticks.y=element_blank(),
        panel.border=element_rect(fill=NA, color="black"))
ggsave("nepal.png", nepal_map2, height=8, width=10)


senegal <- get_stamenmap(bbox = c(left = -18, bottom = 11, 
                                   right = -11, top = 18),  
                          zoom = 7,
                          maptype = "toner-lite") 
senegal_map <- ggmap(senegal) +
  geom_point(data=subset(df, !is.na(amitriptyline) & country=="Senegal"), aes(x=longitude, y=latitude, color=ami_dia_f, shape=facility_type), size=1.5, alpha=0.7) +
  scale_colour_manual(values=c("#ff0000", "#00ff00", "#0000ff")) +
  scale_shape_manual(values=c(17, 16, 15)) +
    ggtitle("Senegal (2012-2016)") +
    theme(legend.position = "none",
        axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks.x=element_blank(),
        axis.ticks.y=element_blank(),
        panel.border=element_rect(fill=NA, color="black"))


tanzania <- get_stamenmap(bbox = c(left = 29.25, bottom = -12, 
                                 right = 40.75, top = -0.5),  
                        zoom = 7,
                        maptype = "toner-lite") 
tanzania_map <- ggmap(tanzania) +
  geom_point(data=subset(df, !is.na(amitriptyline) & country=="Tanzania"), aes(x=longitude, y=latitude, color=ami_dia_f, shape=facility_type), size=1.5, alpha=0.7) +
  scale_colour_manual(values=c("#ff0000", "#87cefa", "#00ff00", "#0000ff")) +
  scale_shape_manual(values=c(17, 16, 15)) +
  ggtitle("Tanzania (2014-2015)") +
  theme(legend.position = "none",
        axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks.x=element_blank(),
        axis.ticks.y=element_blank(),
        panel.border=element_rect(fill=NA, color="black"))

legend_map <- ggmap(nepal) +
  geom_point(data=subset(df, !is.na(amitriptyline) & country=="Nepal"), aes(x=longitude, y=latitude, color=ami_dia_f, shape=facility_type), size=2.5, alpha=0.7) +
  #scale_colour_brewer(palette = "Set1", name = "Amitriptyline Availability", labels = c("Not Available", "Available")) +
  scale_shape_manual(name = "Facility Type", labels = c("Hospital", "Primary", "Other"), values=c(17, 16, 15)) +
  scale_colour_manual(values=c("#ff0000", "#87cefa", "#00ff00", "#0000ff"), name = "Medicine Availability") +
  ggtitle("Nepal")

legend <- get_legend(legend_map)

legend_exp <- ggarrange(legend,
                      ncol=1, nrow=1)
ggsave("legend.png", legend_exp)

all_maps <- ggarrange(bangladesh_map,
                      congo_map, 
                      haiti_map,
                      malawi_map,
                      nepal_map,
                      senegal_map,
                      tanzania_map,
                      #legend,
                     # widths=c(1, 1, 1, 1), heights=c(1, 1),
                      align="hv",
                      ncol=4, nrow=2)
ggsave("all_maps.png", all_maps, height=7, width=12)



##Table 1
df_tableone <- subset(df, !is.na(amitriptyline))
df_survey_tableone <- subset(df, !is.na(amitriptyline)) %>%
  mutate(facility_weight = facility_weight/1000000) %>%
  as_survey_design(weights = facility_weight)


vars <- c("staffcat", "ownership", "facility_type", "power", "improved_water", "improved_sanitation", "computer", "email", "country", "general_opd_private_room", "travel_hours",  "travel_hours_cat", "travel_quartile", "hhwealth_quartile", "amitriptyline", "diazepam")
cat_vars <- c("staffcat", "ownership", "facility_type", "power", "improved_water", "improved_sanitation", "computer", "email", "country", "general_opd_private_room",  "travel_hours_cat", "travel_quartile", "hhwealth_quartile", "amitriptyline", "diazepam")

table_one <- svyCreateTableOne(data = df_survey_tableone, vars=vars,  factorVars = cat_vars, strata=c("amitriptyline"), addOverall=T)
table_one.tableMat <- print(table_one, quote = FALSE, noSpaces = TRUE, printToggle = FALSE)
write.csv(table_one.tableMat, file = "table_one_ami.csv")

table_one <- svyCreateTableOne(data = df_survey_tableone, vars=vars,  factorVars = cat_vars, strata=c("diazepam"), addOverall=T)
table_one.tableMat <- print(table_one, quote = FALSE, noSpaces = TRUE, printToggle = FALSE)
write.csv(table_one.tableMat, file = "table_one_dia.csv")

table_one <- svyCreateTableOne(data = df_survey_tableone, vars=vars,  factorVars = cat_vars, strata=c("country"), addOverall=T)
table_one.tableMat <- print(table_one, quote = FALSE, noSpaces = TRUE, printToggle = FALSE)
write.csv(table_one.tableMat, file = "table_one_country.csv")

########
####Binomial Models
########

df <- df %>%
  mutate(amitriptyline = as.factor(amitriptyline),
         diazepam = as.factor(diazepam),
         travel_quartile = as.factor(travel_quartile),
         travel_decile = as.factor(travel_decile),
         hhwealth_quartile = as.factor(hhwealth_quartile),
       #  facility_weight = facility_weight*1000000,
         country_n = as.numeric(country)) %>%
  rescale_weights(group = "country_n", probability_weights = "facility_weight") 

formula1 <- (amitriptyline) ~ staffcat + ownership + facility_type +
  (1 | country)

formula2 <- (amitriptyline) ~ staffcat + ownership + facility_type +
  power + improved_water + improved_sanitation + computer + email + general_opd_private_room +
  (1 | country)

formula3 <- (amitriptyline) ~ staffcat + ownership + facility_type +
  power + improved_water + improved_sanitation + computer + email +general_opd_private_room +
  travel_hours_cat + hhwealth_quartile +
  (1 | country)

formula4 <- (diazepam) ~ staffcat + ownership + facility_type +
  (1 | country)

formula5 <- (diazepam) ~ staffcat + ownership + facility_type +
  power + improved_water + improved_sanitation + computer + email + general_opd_private_room +
  (1 | country)

formula6 <- (diazepam) ~ staffcat + ownership + facility_type +
  power + improved_water + improved_sanitation + computer + email +general_opd_private_room +
  travel_hours_cat + hhwealth_quartile +
  (1 | country)

formulas_ami <- list(formula1, formula2, formula3)
formulas_dia <- list(formula4, formula5, formula6)

models_ami_unweighted <- lapply(formulas_ami, function(f) glmer(f, family= binomial(link = "logit"), data = df))
models_dia_unweighted <-  lapply(formulas_dia, function(f) glmer(f, family= binomial(link = "logit"), data=df))

tab_model(models_ami_unweighted[[1]], models_ami_unweighted[[2]], models_ami_unweighted[[3]], file="regressiontable_ami_unweighted.html")
tab_model(models_dia_unweighted[[1]], models_dia_unweighted[[2]], models_dia_unweighted[[3]], file="regressiontable_dia_unweighted.html")
tab_model(models_ami_unweighted[[3]], models_dia_unweighted[[3]], file="regressiontable_ami_dia_model3_unweighted.html")



models_ami <- lapply(formulas_ami, function(f) glmer(f, family= binomial(link = "logit"), data = df, weights=facility_weight))
models_dia <-  lapply(formulas_dia, function(f) glmer(f, family= binomial(link = "logit"), data=df, weights=facility_weight))

tab_model(models_ami[[1]], models_ami[[2]], models_ami[[3]], file="regressiontable_ami_weighted.html")
tab_model(models_dia[[1]], models_dia[[2]], models_dia[[3]], file="regressiontable_dia_weighted.html")
tab_model(models_ami[[3]], models_dia[[3]], file="regressiontable_ami_dia_model3_weighted.html")


formula_ami_country <- (amitriptyline) ~ staffcat + ownership + facility_type +
  power + improved_water + improved_sanitation + computer + email +general_opd_private_room +
  travel_hours + hhwealth_quartile + country

formula_dia_country <- (diazepam) ~ staffcat + ownership + facility_type +
  power + improved_water + improved_sanitation + computer + email +general_opd_private_room +
  travel_hours + hhwealth_quartile + country

df_survey_model <- svydesign(data=df, ids=~1, weights=~facility_weight)
model_ami_country <- svyglm(formula_ami_country, family= binomial(link = "logit"), design = df_survey_model)
model_dia_country <-  svyglm(formula_dia_country, family= binomial(link = "logit"), design = df_survey_model)

tab_model(model_ami_country, model_dia_country, file="regressiontable_svyglm.html")


countries <- list("Afghanistan", "Bangladesh", "DRC", "Haiti", "Malawi", "Nepal", "Senegal", "Tanzania")
country_formula <- (amitriptyline) ~ staffcat + ownership + facility_type +
  power + improved_water + improved_sanitation + computer + email
country_models <- lapply(countries, function(f) glm(country_formula, family= binomial(link = "logit"), data=subset(df, country==f)))
tab_model(country_models[[1]], country_models[[2]], country_models[[3]],  country_models[[4]], country_models[[5]], 
          country_models[[6]], country_models[[7]], country_models[[8]],file="country_regressions_noafgh.html")
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

library(finalfit)

explanatory = c("staffcat", "ownership", "facility_type", "power", "improved_water", "improved_sanitation", "computer", "email", 
                  "travel_quartile", "hhwealth_quartile")
random_effect = "country"
dependent = 'amitriptyline'
df %>%
  summary_factorlist(dependent, explanatory,
                     p=TRUE, add_dependent_label=TRUE) -> t1
write.csv(t1, file = "finalfit_tableone.csv")

df %>%
  finalfit(dependent, explanatory, random_effect, metrics=TRUE) -> t5
write.csv(t5, file = "finalfit_tabletwo_univariateOR.csv")

df %>%
  or_plot(dependent, explanatory)

knitr::kable(t5)
, row.names=FALSE, align=c("l", "l", "r", "r", "r", "r"))
