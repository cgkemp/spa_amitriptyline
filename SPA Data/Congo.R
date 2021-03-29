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

df <- read_sas("Facility Inventory/congo 2017-18 sas spss/CDFC71SVSP/CDFC71FLSP.SAS7BDAT") #raw

congo <- df %>%  
  mutate(rural = case_when(
    FTYPE== 1 ~ 0,
    FTYPE== 2 ~ 1)) %>%  
  mutate(facility_type = case_when(
    FACTYPE== 1 ~ "hospital", 
  FACTYPE== 2 ~ "hospital",
  FACTYPE== 3 ~ "hospital",
  FACTYPE== 4 ~ "hospital",
  FACTYPE== 5 ~ "primary")) %>%
  mutate(primary = case_when(
    FACTYPE== 1 ~ 0, 
  FACTYPE== 2 ~ 0,
  FACTYPE== 3 ~ 0,
  FACTYPE== 4 ~ 0,
  FACTYPE== 5 ~ 1)) %>%
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
  mutate(country = "Democratic Republic of the Congo", 
         worldbank = "Lower Middle Income") %>%
  dplyr::rename(province = PROVINCE,
                district = ZONE,
                facility_number = FACIL,
                ownership = MGA,
                month = MONTH,
                year = YEAR) %>%
  dplyr::select(province, district, rural, facility_number, month, year, ownership, facility_type, primary, store_meds, ncd_services, amitriptyline, 
         total_staff, power, improved_water, improved_sanitation, email, computer, general_opd_private_room, ncd_private_room, country, worldbank)

#Import SPA lat/long
df_spatial <- read.dbf("Facility Inventory/congo 2017-18 sas spss/geo/CDGE71FLSR.dbf") %>%
  dplyr::select(facility_number = SPAFACID, province_name = ADM1NAME, district_name = SPAREGNA, facility_type_name = SPATYPEN, ownership_name = SPAMANGN, latitude = LATNUM, longitude = LONGNUM) %>%
  mutate(latitude = na_if(latitude,0),
         longitude = na_if(longitude,0))

congo <- congo %>%
  left_join(df_spatial, by="facility_number")

#Add travel time to central MOH

shape <- shapefile("Facility Inventory/congo 2017-18 sas spss/geo/cod_admbnda_adm0_rgc_itos_20190911.shp")
plot(shape, main="Shape for Clipping")
friction <- malariaAtlas::getRaster(
  surface = "A global friction surface enumerating land-based travel speed for a nominal year 2015",
  #shp = shape,
  extent = matrix(c("12", "-15","33", "6"), nrow = 2, ncol = 2, dimnames = list(c("x", "y"), c("min", "max"))))
malariaAtlas::autoplot_MAPraster(friction)
T <- gdistance::transition(friction, function(x) 1/mean(x), 8) 
T.GC <- gdistance::geoCorrection(T)    
point.locations <- read.csv(file = "Facility Inventory/congo 2017-18 sas spss/geo/moh_location.csv")
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
loc <- congo[,c("longitude", "latitude")]
travel_time <- raster::extract(access.raster, loc)
congo <- cbind(congo, travel_time)
congo <- congo %>%
  mutate(travel_time = na_if(travel_time, Inf))

#Import cluster lat/long

df_clusterlatlong <- read.dbf("DHS/DRC/CDGE61FL/CDGE61FL.dbf") %>%
  dplyr::select(cluster_id = DHSCLUST, latitude = LATNUM, longitude = LONGNUM) %>%
  filter(latitude != 0 & longitude != 0)

#Import DHS data
df_hh <- read_sas("DHS/DRC/CDHR61SD/CDHR61FL.SAS7BDAT")

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

congo[, c("xvar", "yvar")] <- latlong2grid( 
  congo[, c("longitude", "latitude")]
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
congo <- dist.merge(congo, df_cluster, 'xvar', 'yvar', 'xvar', 'yvar') 

ggplot(congo, aes(x=min.dist)) + geom_histogram()
ggplot(congo, aes(x=travel_time, y=hh.wealthindex.mean)) + geom_point() + geom_smooth()

saveRDS(congo, "congo.rds")