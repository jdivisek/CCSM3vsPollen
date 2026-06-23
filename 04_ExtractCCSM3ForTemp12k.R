##Extract CCSM3 and CHELSA TraCE21k data for Temp12k localities-----------------
library(terra)
library(rlist)
library(tidyverse)

##Load selected localities from Temp12k database
loc <- read.delim("temp12k.selection.txt", header=T, row.names = 1, as.is = T)
head(loc)

##Time windows of CHELSA TraCE21k
tw <- seq(0, 21000, 100)
trace.tw <- c(paste0("00", seq(20, 10, -1)),
              paste0("000", seq(9, 1, -1)),
              "0000", 
              paste0("-00", seq(1, 9, 1)),
              paste0("-0", seq(10, 99, 1)),
              paste0("-", seq(100, 190, 1)))

cbind(tw, trace.tw)

tw <- tw[1:130]
trace.tw <- trace.tw[1:130]

ext <- list()

###CHELSA TraCE21k data---------------------------------------------------------
#bio01
paths <- paste0(getwd(),"/GIS_database/CHELSA_TraCE21k/", 
                "bio01", "/CHELSA_TraCE21k_", "bio01", "_", trace.tw, "_V.1.0.tif")

r <- terra::rast(paths)
names(r) <- paste0("bio1_", tw)

ext$bio1 <- terra::extract(r, loc, method="bilinear", ID=F)

##Align with CCSM3 windows
ext$bio1 <- t(apply(ext$bio1, 1, FUN = function(y){ approx(x = tw, y = y, xout = seq(50, 12850, 100))$y }))
colnames(ext$bio1) <- paste0("bio1_", seq(50, 12850, 100))

ext$bio1 <- ext$bio1 - 273.15
rownames(ext$bio1) <- rownames(loc)
ext$bio1  %>% as.data.frame() %>% rownames_to_column(var = "site") %>% 
  pivot_longer(cols = bio1_50:bio1_12850, names_to = "tw", values_to = "bio1") %>% 
  mutate(tw = gsub("bio1_", "", tw)) -> ext$bio1

#bio12
paths <- paste0(getwd(), "/GIS_database/CHELSA_TraCE21k/", 
                "bio12", "/CHELSA_TraCE21k_", "bio12", "_", trace.tw, "_V.1.0.tif")

r <- terra::rast(paths)
names(r) <- paste0("bio12_", tw)

ext$bio12 <- terra::extract(r, loc, method="bilinear", ID=F)

ext$bio12 <- t(apply(ext$bio12, 1, FUN = function(y){ approx(x = tw, y = y, xout = seq(50, 12850, 100))$y }))
colnames(ext$bio12) <- paste0("bio12_", seq(50, 12850, 100))

rownames(ext$bio12) <- rownames(loc)
ext$bio12  %>% as.data.frame() %>% rownames_to_column(var = "site") %>% 
  pivot_longer(cols = bio12_50:bio12_12850, names_to = "tw", values_to = "bio12") %>% 
  mutate(tw = gsub("bio12_", "", tw)) -> ext$bio12

#January temperature
paths <- paste0(getwd(), "/GIS_database/CHELSA_TraCE21k/", 
                "tasmin", "/CHELSA_TraCE21k_", "tasmin", "_1_", seq(20, -109, -1), "_V1.0.tif")
r <- terra::rast(paths)
tasmin <- terra::extract(r, loc, method="bilinear", ID=F)

paths <- paste0(getwd(), "/GIS_database/CHELSA_TraCE21k/", 
                "tasmax", "/CHELSA_TraCE21k_", "tasmax", "_1_", seq(20, -109, -1), "_V1.0.tif")
r <- terra::rast(paths)
tasmax <- terra::extract(r, loc, method="bilinear", ID=F)

ext$tmean01 <- (tasmin + tasmax)/2
colnames(ext$tmean01) <- paste0("tmean01_", tw)

##Align with CCSM3 windows
ext$tmean01 <- t(apply(ext$tmean01, 1, FUN = function(y){ approx(x = tw, y = y, xout = seq(50, 12850, 100))$y }))
colnames(ext$tmean01) <- paste0("tmean01_", seq(50, 12850, 100))

ext$tmean01 <- (ext$tmean01/10) - 273.15
rownames(ext$tmean01) <- rownames(loc)
ext$tmean01  %>% as.data.frame() %>% rownames_to_column(var = "site") %>% 
  pivot_longer(cols = tmean01_50:tmean01_12850, names_to = "tw", values_to = "tmean01") %>% 
  mutate(tw = gsub("tmean01_", "", tw)) -> ext$tmean01

#July temperature
paths <- paste0(getwd(), "/GIS_database/CHELSA_TraCE21k/", 
                "tasmin", "/CHELSA_TraCE21k_", "tasmin", "_7_", seq(20, -109, -1), "_V1.0.tif")
r <- terra::rast(paths)
tasmin <- terra::extract(r, loc, method="bilinear", ID=F)

paths <- paste0(getwd(), "/GIS_database/CHELSA_TraCE21k/", 
                "tasmax", "/CHELSA_TraCE21k_", "tasmax", "_7_", seq(20, -109, -1), "_V1.0.tif")
r <- terra::rast(paths)
tasmax <- terra::extract(r, loc, method="bilinear", ID=F)

ext$tmean07 <- (tasmin + tasmax)/2
colnames(ext$tmean07) <- paste0("tmean07_", tw)

##Align with CCSM3 windows
ext$tmean07 <- t(apply(ext$tmean07, 1, FUN = function(y){ approx(x = tw, y = y, xout = seq(50, 12850, 100))$y }))
colnames(ext$tmean07) <- paste0("tmean07_", seq(50, 12850, 100))

ext$tmean07 <- (ext$tmean07/10) - 273.15
rownames(ext$tmean07) <- rownames(loc)
ext$tmean07  %>% as.data.frame() %>% rownames_to_column(var = "site") %>% 
  pivot_longer(cols = tmean07_50:tmean07_12850, names_to = "tw", values_to = "tmean07") %>% 
  mutate(tw = gsub("tmean07_", "", tw)) -> ext$tmean07

str(ext)

list.save(ext, file="temp12k.sites_CHELSATraCE21k.rdata")

###CCSM3 DATA-------------------------------------------------------------------

tw <- seq(50, 12850, 100)

ext <- list()

#bio01
paths <- paste0(getwd(), "/GIS_database/Paleoclimates_CCSM3/SAGA/bioclim/", 
                tw, "BP/", "bio1", "_", tw, "BP.sdat")

r <- terra::rast(paths)

ext$bio1 <- terra::extract(r, loc, method="bilinear", ID=F)

colnames(ext$bio1) <- gsub("BP", "", colnames(ext$bio1))
rownames(ext$bio1) <- rownames(loc)

ext$bio1  %>% as.data.frame() %>% rownames_to_column(var = "site") %>% 
  pivot_longer(cols = bio1_50:bio1_12850, names_to = "tw", values_to = "bio1") %>% 
  mutate(tw = gsub("bio1_", "", tw)) -> ext$bio1

#bio12
paths <- paste0(getwd(), "/GIS_database/Paleoclimates_CCSM3/SAGA/bioclim/", 
                tw, "BP/", "bio12", "_", tw, "BP.sdat")

r <- terra::rast(paths)

ext$bio12 <- terra::extract(r, loc, method="bilinear", ID=F)

colnames(ext$bio12) <- gsub("BP", "", colnames(ext$bio12))
rownames(ext$bio12) <- rownames(loc)

ext$bio12  %>% as.data.frame() %>% rownames_to_column(var = "site") %>% 
  pivot_longer(cols = bio12_50:bio12_12850, names_to = "tw", values_to = "bio12") %>% 
  mutate(tw = gsub("bio12_", "", tw)) -> ext$bio12


#January temperature
paths <- paste0(getwd(), "/GIS_database/Paleoclimates_CCSM3/SAGA/tmean_NoExtremes/01_Jan/", 
                "tmean01", "_", tw, "BP.tif")

r <- terra::rast(paths)

ext$tmean01 <- terra::extract(r, loc, method="bilinear", ID=F)

colnames(ext$tmean01) <- gsub("BP", "", colnames(ext$tmean01))
rownames(ext$tmean01) <- rownames(loc)

ext$tmean01  %>% as.data.frame() %>% rownames_to_column(var = "site") %>% 
  pivot_longer(cols = tmean01_50:tmean01_12850, names_to = "tw", values_to = "tmean01") %>% 
  mutate(tw = gsub("tmean01_", "", tw)) -> ext$tmean01

#July temperature
paths <- paste0(getwd(), "/GIS_database/Paleoclimates_CCSM3/SAGA/tmean_NoExtremes/07_Jul/", 
                "tmean07", "_", tw, "BP.tif")

r <- terra::rast(paths)

ext$tmean07 <- terra::extract(r, loc, method="bilinear", ID=F)

colnames(ext$tmean07) <- gsub("BP", "", colnames(ext$tmean07))
rownames(ext$tmean07) <- rownames(loc)

ext$tmean07  %>% as.data.frame() %>% rownames_to_column(var = "site") %>% 
  pivot_longer(cols = tmean07_50:tmean07_12850, names_to = "tw", values_to = "tmean07") %>% 
  mutate(tw = gsub("tmean07_", "", tw)) -> ext$tmean07

str(ext)

list.save(ext, file="temp12k.sites_CCSM3.rdata")

