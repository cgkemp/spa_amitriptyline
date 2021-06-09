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
  V007== 7 ~ "primary",
  V007== 8 ~ "primary",
  V007== 9 ~ "primary",
  V007== TRUE ~ "hospital")) %>%
  mutate(primary = case_when(
  V007== 2 ~ 0, 
  V007== 3 ~ 0,
  V007== 4 ~ 0,
  V007== 5 ~ 1,
  V007== 6 ~ 0,
  V007== 7 ~ 1,
  V007== 8 ~ 1,
  V007== 9 ~ 1,
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
  mutate(diazepam = case_when(
    V906_07== 0 ~ 0,
    V906_07== 3 ~ 0,
    V906_07== 4 ~ 0,
    V906_07== 5 ~ 0,
    V906_07== 2 ~ 1,
    V906_07== 1 ~ 0)) %>%
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
                facility_weight = V005,
                month = V081,
                year = V082) %>%
  dplyr::select(province, district, rural, facility_number, facility_weight, month, year, ownership, facility_type, primary, store_meds, ncd_services, amitriptyline, diazepam,
         total_staff, power, improved_water, improved_sanitation, email, computer, general_opd_private_room, ncd_private_room, country, worldbank)

#Import SPA lat/long
df_spatial <- read.dbf("Facility Inventory/malawi 13-14/geo/MWGE6IFLSR.dbf") %>%
  dplyr::select(facility_number = SPAFACID, province_name = ADM1NAME, district_name = SPAREGNA, facility_type_name = SPATYPEN, ownership_name = SPAMANGN, latitude = LATNUM, longitude = LONGNUM) %>%
  mutate(latitude = na_if(latitude,0),
         longitude = na_if(longitude,0))
malawi <- malawi %>%
  right_join(df_spatial, by="facility_number")

#Add travel time to central MOH
shape <- shapefile("Facility Inventory/malawi 13-14/geo/mwi_admbnda_adm0_nso_20181016.shp")
plot(shape, main="Shape for Clipping")
friction <- malariaAtlas::getRaster(
  surface = "A global friction surface enumerating land-based travel speed for a nominal year 2015",
  #shp = shape,
  extent = matrix(c("32.5", "-17.5","36", "-9"), nrow = 2, ncol = 2, dimnames = list(c("x", "y"), c("min", "max"))))
malariaAtlas::autoplot_MAPraster(friction)
T <- gdistance::transition(friction, function(x) 1/mean(x), 8) 
T.GC <- gdistance::geoCorrection(T)    
point.locations <- read.csv(file = "Facility Inventory/malawi 13-14/geo/moh_location.csv")
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
loc <- malawi[,c("longitude", "latitude")]
travel_time <- raster::extract(access.raster, loc)
malawi <- cbind(malawi, travel_time)
malawi <- malawi %>%
  mutate(travel_time = na_if(travel_time, Inf))

#Import cluster lat/long

df_clusterlatlong <- read.dbf("DHS/Malawi/MWGE7AFL/MWGE7AFL.dbf") %>%
  dplyr::select(cluster_id = DHSCLUST, latitude = LATNUM, longitude = LONGNUM) %>%
  filter(latitude != 0 & longitude != 0)

#Import DHS data
df_hh <- read_sas("DHS/Malawi/MWHR7ASD/MWHR7AFL.SAS7BDAT")

#Collapse DHS SES data to cluster level

ggplot(df_hh, aes(x=HV271)) + geom_histogram()
df_clusterses <- summaryBy(HV271 ~ HV001, FUN=c(mean,median,sd), data=df_hh)
ggplot(df_clusterses, aes(x=HV271.mean)) + geom_histogram()
ggplot(df_clusterses, aes(x=HV271.mean, y=HV271.median)) + geom_point()
names(df_clusterses) <- c("cluster_id", "hh.wealthindex.mean", "hh.wealthindex.median", "hh.wealthindex.sd")

#Merge cluster lat/long and SES info

df_cluster <- merge(df_clusterlatlong, df_clusterses, by="cluster_id")

#Convert lat/long to coords in km

df_cluster[, c("xvar", "yvar")] <- latlong2grid( 
  df_cluster[, c("longitude", "latitude")]
)

malawi[, c("xvar", "yvar")] <- latlong2grid( 
  malawi[, c("longitude", "latitude")]
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
  tmp <- cbind(x, min.dist=tmp[,2], y[tmp[,1],-match(c(yeast,
                                                       ynorth), names(y))])
  row.names(tmp) <- NULL
  tmp
}


#Join data based on minimum Euclidean distance
df_cluster <- df_cluster %>%
  dplyr::select(-latitude, -longitude)
malawi <- dist.merge(malawi, df_cluster, 'xvar', 'yvar', 'xvar', 'yvar') 

ggplot(malawi, aes(x=min.dist)) + geom_histogram()
ggplot(malawi, aes(x=travel_time, y=hh.wealthindex.mean)) + geom_point() + geom_smooth()

saveRDS(malawi, "malawi.rds")