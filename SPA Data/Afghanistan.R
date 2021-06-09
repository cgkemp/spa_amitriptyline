rm(list = ls())

library("tidyverse")
library("haven")
library("dplyr")
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

df <- read_dta("Facility Inventory/Afghanistan 18-19 spa/AFFC7ADTSR/AFFC7AFLSR.DTA")
  
afghanistan <- df %>%  
  mutate(rural = case_when(
    v003== 1 ~ 0,
    v003== 2 ~ 1)) %>%
  mutate(facility_type = case_when(
    v007== 1 ~ "hospital",
    v007== 2 ~ "hospital", 
    v007== 3 ~ "hospital", 
    v007== 4 ~ "hospital",
    v007== 5 ~ "primary")) %>%
  mutate(primary = case_when(
    v007== 2 ~ 0, 
    v007== 3 ~ 0,
    v007== 4 ~ 0,
    v007== 5 ~ 1,
    v007== 1 ~ 0)) %>%
  mutate(store_meds = case_when(
    v035==0 ~ 0,
    v035==1 ~ 1)) %>%
  mutate(ncd_services = case_when(
    v048==0 ~ 0,
    v048==1 ~ 1)) %>%
  mutate(amitriptyline = case_when(
    v903_16== 0 ~ 0,
    v903_16== 3 ~ 0,
    v903_16== 4 ~ 0,
    v903_16== 5 ~ 0,
    v903_16== 2 ~ 1,
    v903_16== 1 ~ 0)) %>%
  mutate(diazepam = case_when(
    v906_07== 0 ~ 0,
    v906_07== 3 ~ 0,
    v906_07== 4 ~ 0,
    v906_07== 5 ~ 0,
    v906_07== 2 ~ 1,
    v906_07== 1 ~ 0)) %>%
  mutate(total_staff = v102dt) %>%
  mutate(power = case_when(
    v120a== 0 ~ 0, #not connected
    v120a== 1 ~ 1, #connected
    v120a== 2 ~ 0,
    v120a== 3 ~ 0, 
    v120a== 8 ~ 0)) %>%
  mutate(improved_water = case_when(
    v123== 0 ~ 0,
    v123== 13 ~ 1,
    v123== 14 ~ 1,
    v123== 15 ~ 1,
    v123== 20 ~ 1,
    v123== 21 ~ 0,
    v123== 22 ~ 1,
    v123== 23 ~ 1,
    v123== 24 ~ 0,
    v123== 30 ~ 0, 
    v123== 31 ~ 1,
    v123== 32 ~ 1,
    v123== 41 ~ 0,
    v123== 42 ~ 0,
    v123== 96 ~ 0,
    v123== 98 ~ 0)) %>%
  mutate(improved_sanitation = case_when(
    v153a== 0 ~ 0,
    v153a== 11 ~ 1,
    v153a== 12 ~ 1,
    v153a== 13 ~ 1,
    v153a== 14 ~ 1,
    v153a== 15 ~ 1,
    v153a== 21 ~ 1,
    v153a== 22 ~ 0,
    v153a== 23 ~ 0,
    v153a== 31 ~ 0, 
    v153a== 41 ~ 0,
    v153a== 51 ~ 0)) %>%
  mutate(email = case_when(
    v129== 0 ~ 0, #no
    v129== 1 ~ 1, #yes
    v129== 2 ~ 1)) %>%
  mutate(computer = case_when(
   v128a== 0 ~ 0, #no
   v128a== 1 ~ 1, #yes
   v128a== 2 ~ 1 )) %>%
  mutate(general_opd_private_room = case_when(
    v167== 0 ~ 0, #no
    v167== 1 ~ 1, #yes
    v167== 2 ~ 1,
    v167== 4 ~ 0)) %>%
  mutate(ncd_private_room = case_when(
    v1609== 0 ~ 0, #no
    v1609== 1 ~ 1, #yes
    v1609== 2 ~ 1, 
    v1609== 4 ~ 0)) %>%
  mutate(country = "Afghanistan", 
         worldbank = "Low Income") %>%
  dplyr::rename(province = v001,
                district = v002,
                facility_number = v004,
                facility_weight = v005,
                ownership = v008,
                month = v081,
                year = v082) %>%
  dplyr::select(province, district, rural, facility_number, facility_weight, month, year, ownership, facility_type, primary, store_meds, ncd_services, amitriptyline, diazepam,
         total_staff, power, improved_water, improved_sanitation, email, computer, general_opd_private_room, ncd_private_room, country, worldbank)

#Import SPA lat/long
df_spatial <- read.dbf("Facility Inventory/Afghanistan 18-19 spa/geo/AFGE7ADTSR.dbf") %>%
  dplyr::select(facility_number = SPAFACID, province_name = ADM1NAME, district_name = SPAREGNA, facility_type_name = SPATYPEN, ownership_name = SPAMANGN, latitude = LATNUM, longitude = LONGNUM) %>%
  mutate(latitude = na_if(latitude,0),
         longitude = na_if(longitude,0))

afghanistan <- afghanistan %>%
  right_join(df_spatial, by="facility_number")

#Add travel time to central MOH
shape <- shapefile("Facility Inventory/Afghanistan 18-19 spa/geo/afg_admbnda_adm0_agcho_20180522.shp")
plot(shape, main="Shape for Clipping")
friction <- malariaAtlas::getRaster(
  surface = "A global friction surface enumerating land-based travel speed for a nominal year 2015",
  #shp = shape,
  extent = matrix(c("60", "29","76", "39"), nrow = 2, ncol = 2, dimnames = list(c("x", "y"), c("min", "max"))))
malariaAtlas::autoplot_MAPraster(friction)
T <- gdistance::transition(friction, function(x) 1/mean(x), 8) 
T.GC <- gdistance::geoCorrection(T)    
point.locations <- read.csv(file = "Facility Inventory/Afghanistan 18-19 spa/geo/moh_location.csv")
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
loc <- afghanistan[,c("longitude", "latitude")]
travel_time <- raster::extract(access.raster, loc)
afghanistan <- cbind(afghanistan, travel_time)
afghanistan <- afghanistan %>%
  mutate(travel_time = na_if(travel_time, Inf))

saveRDS(afghanistan, "afghanistan.rds")