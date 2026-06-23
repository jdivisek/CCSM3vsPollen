###ESTIMATE 100-0 BP VALUES FOR POLLEN DATA USING MISSFOREST IMPUTATION--------------------------------
library(tidyverse)
library(terra)
library(missForest)

##Extract elevations from 90m digital elevation model
dem <- terra::rast(paste0(getwd(), "/GIS_database/DEM/DEM90_Europe.tif")) 
dem

###Mean annual temperature-----------------------------------------------------------------------------
wide <- pollen.Tannual.long %>% pivot_wider(id_cols = site, names_from = tw, values_from = Tannual)
dim(wide)

ext <- terra::extract(dem, sites.pollen.Tannual[, c("Lon", "Lat")], ID=F)
summary(ext)

sites.pollen.Tannual[is.na(ext$DEM90_Europe), ]
ext[is.na(ext$DEM90_Europe), ] <- 0

ext$site <- sites.pollen.Tannual$dataSetName
ext$Lon <- sites.pollen.Tannual$Lon
ext$Lat <- sites.pollen.Tannual$Lat

wide <- left_join(x = wide, y = ext, by="site")
head(wide)

set.seed(12)
imp <- missForest(as.data.frame(wide[,-1]), variablewise=T, verbose = F,
                  maxiter = 20, ntree = 500)
imp

Tann.50.RF <- data.frame(site = wide$site, RF50 = imp$ximp$`50`)

###Mean temperature of the coldest month-------------------------------------
wide <- pollen.Tcold.long %>% pivot_wider(id_cols = site, names_from = tw, values_from = Tcold)
dim(wide)

ext <- terra::extract(dem, sites.pollen.Tcold[, c("Lon", "Lat")], ID=F)
summary(ext)

sites.pollen.Tcold[is.na(ext$DEM90_Europe), ]
ext[is.na(ext$DEM90_Europe), ] <- 0

ext$site <- sites.pollen.Tcold$dataSetName
ext$Lon <- sites.pollen.Tcold$Lon
ext$Lat <- sites.pollen.Tcold$Lat

wide <- left_join(x = wide, y = ext, by="site")
head(wide)

set.seed(12)
imp <- missForest(as.data.frame(wide[,-1]), variablewise=T, verbose = F,
                  maxiter = 20, ntree = 500)
imp

Tcold.50.RF <- data.frame(site = wide$site, RF50 = imp$ximp$`50`)

###Mean temperature of the warmest month-------------------------------------
wide <- pollen.Twarm.long %>% pivot_wider(id_cols = site, names_from = tw, values_from = Twarm)
dim(wide)

ext <- terra::extract(dem, sites.pollen.Twarm[, c("Lon", "Lat")], ID=F)
summary(ext)

sites.pollen.Twarm[is.na(ext$DEM90_Europe), ]
ext[is.na(ext$DEM90_Europe), ] <- 0

ext$site <- sites.pollen.Twarm$dataSetName
ext$Lon <- sites.pollen.Twarm$Lon
ext$Lat <- sites.pollen.Twarm$Lat

wide <- left_join(x = wide, y = ext, by="site")
head(wide)

set.seed(12)
imp <- missForest(as.data.frame(wide[,-1]), variablewise=T, verbose = F,
                  maxiter = 20, ntree = 500)
imp

Twarm.50.RF <- data.frame(site = wide$site, RF50 = imp$ximp$`50`)

###Annual precipitation-------------------------------------
wide <- pollen.Pannual.long %>% pivot_wider(id_cols = site, names_from = tw, values_from = Pannual)
dim(wide)

ext <- terra::extract(dem, sites.pollen.Pannual[, c("Lon", "Lat")], ID=F)
summary(ext)

sites.pollen.Pannual[is.na(ext$DEM90_Europe), ]
ext[is.na(ext$DEM90_Europe), ] <- 0

ext$site <- sites.pollen.Pannual$dataSetName
ext$Lon <- sites.pollen.Pannual$Lon
ext$Lat <- sites.pollen.Pannual$Lat

wide <- left_join(x = wide, y = ext, by="site")
head(wide)

set.seed(12)
imp <- missForest(as.data.frame(wide[,-1]), variablewise=T, verbose = F,
                  maxiter = 20, ntree = 500)
imp

Pann.50.RF <- data.frame(site = wide$site, RF50 = imp$ximp$`50`)

