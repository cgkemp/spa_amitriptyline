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

df <- read_sas("Facility Inventory/ht 2017/HTFC7ASDSR/HTFC7AFLSR.SAS7BDAT")

haiti <- df %>%  
  mutate(rural = case_when(
    V003== 1 ~ 0,
    V003== 2 ~ 1)) %>%
  mutate(facility_type = case_when(
    V007== 2 ~ "hospital", 
  V007== 3 ~ "hospital",
  V007== 4 ~ "hospital",
  V007== 5 ~ "primary",
  V007== 6 ~ "primary",
  V007== 7 ~ "primary",
  V007== 1 ~ "hospital")) %>%
  mutate(primary = case_when(
    V007== 2 ~ 0, 
  V007== 3 ~ 0,
  V007== 4 ~ 0,
  V007== 5 ~ 1,
  V007== 6 ~ 1,
  V007== 7 ~ 1,
  V007== 1 ~ 0)) %>%
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
  mutate(country = "Haiti", 
         worldbank = "Low Income") %>%
  dplyr::rename(province = V001,
                district = V002,
                facility_number = V004,
                ownership = V008,
                month = V081,
                year = V082) %>%
  select(province, district, rural, facility_number, month, year, ownership, facility_type, primary, store_meds, ncd_services, amitriptyline, 
         total_staff, power, improved_water, improved_sanitation, email, computer, general_opd_private_room, ncd_private_room, country, worldbank)

#Import SPA lat/long
df_spatial <- read.dbf("Facility Inventory/ht 2017/geo/HTGE7BFLSR.dbf") %>%
  select(facility_number = SPAFACID, province_name = ADM1NAME, district_name = SPAREGNA, facility_type_name = SPATYPEN, ownership_name = SPAMANGN, latitude = LATNUM, longitude = LONGNUM) %>%
  mutate(latitude = na_if(latitude,0),
         longitude = na_if(longitude,0))

haiti <- haiti %>%
  left_join(df_spatial, by="facility_number")

#Add travel time to central MOH
shape <- shapefile("Facility Inventory/ht 2017/geo/hti_admbnda_adm0_cnigs_20181129.shp")
plot(shape, main="Shape for Clipping")
friction <- malariaAtlas::getRaster(
  surface = "A global friction surface enumerating land-based travel speed for a nominal year 2015",
  shp = shape)
malariaAtlas::autoplot_MAPraster(friction)
T <- gdistance::transition(friction, function(x) 1/mean(x), 8) 
T.GC <- gdistance::geoCorrection(T)    
point.locations <- read.csv(file = "Facility Inventory/ht 2017/geo/moh_location.csv")
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
loc <- haiti[,c("longitude", "latitude")]
travel_time <- raster::extract(access.raster, loc)
haiti <- cbind(haiti, travel_time)
haiti <- haiti %>%
  mutate(travel_time = na_if(travel_time, Inf))


saveRDS(haiti, "haiti.rds")
  