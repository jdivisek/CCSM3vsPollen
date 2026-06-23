###MAP CORRELATIONS BETWEEN TEMPERATURE AND PRECIPITATION VALUES--------------------
library(rgdal)
library(RColorBrewer)
library(berryFunctions)
library(raster)
library(tidyverse)
library(rworldmap)

##Get countries
m <- getMap(resolution = "low", projection = NA)
m <- crop(m, extent(-50, 80, 25, 90))
mp <- spTransform(m, CRS("+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs"))

##coordinates of the ticks on axes
grat.p <- read.csv("Graticules_points_LAEA.txt", header=T)
##graticules
grat <- readOGR(getwd(), "/GIS_database/shapes/graticules.shp")

##import CCSM3 data
CCSM3 <- list.load("temp12k.sites_CCSM3.rdata")

###Plot------------------------------------------------------------------------------------------
tiff("Correlation_maps.tif", width = 10.1, height = 10, units="in", res=500, compression = "lzw")

par(mfrow = c(2,2), mar=c(3, 1, 3, 0), oma=c(0,3,0,7), xaxs = "i", yaxs = "i")

br <- seq(-1, 1, 0.2) ##correlation breaks

#Mean annual temperature-------------------------------------------------------
plot(NA, xlim=c(25e+05, 65e+05), ylim=c(15e+05, 55e+05), axes=F, asp=1, xlab="", ylab="",
     main = "Mean annual temperature", cex.main=1.4)
rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col = "gray", border = NA)
plot(grat, add=T, lwd=1, col="white")

plot(mp, add=T, col = "white")

axis(1, at=grat.p$X[4:7], labels = grat.p$Label[4:7])
axis(2, at=grat.p$Y[1:3], labels =grat.p$Label[1:3], las=1)
box()

sites.pollen.Tannual$length <- sites.pollen.Tannual$maxYear - sites.pollen.Tannual$minYear
summary(sites.pollen.Tannual$length)

plot.data <- pollen.Tannual.long[, c("site", "tw", "Tannual")] %>% 
  mutate(code = paste0(site, "_", tw)) %>% 
  left_join(y = mutate(CCSM3$bio1, code =  paste0(site, "_", tw)), by = "code") %>%
  dplyr::select(site.x, tw.x, Tannual, bio1) %>% 
  rename(site = site.x, tw = tw.x) %>%
  mutate(bio1 = round(bio1, 1)) %>% 
  group_by(site) %>%
  summarise(r = cor(Tannual, bio1)) %>% 
  left_join(x = mutate(sites.pollen.Tannual, site = dataSetName), y=., by = "site") %>% 
  dplyr::select(site, X, Y, length, r)

plot.data$r.class <- berryFunctions::classify(plot.data$r, breaks = br, method="custom")$index
plot.data$l.class <- berryFunctions::classify(plot.data$length, breaks = c(0, 4000, 8000, 12000, 20000), method="custom")$index

plot.data <- plot.data[order(plot.data$l.class, abs(plot.data$r)*-1, decreasing = T), ]

points(plot.data[, c("X", "Y")], pch=21, cex= c(0.5, 1.5, 2.5, 3.5)[plot.data$l.class],
       bg =rev(brewer.pal(10,"RdYlBu"))[plot.data$r.class])

#Temperature of the coldest month-------------------------------------------------------
plot(NA, xlim=c(25e+05, 65e+05), ylim=c(15e+05, 55e+05), axes=F, asp=1, xlab="", ylab="",
     main = "Temperature of the coldest month (January)", cex.main=1.4)
rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col = "gray", border = NA)
plot(grat, add=T, lwd=1, col="white")

plot(mp, add=T, col="white")

axis(1, at=grat.p$X[4:7], labels = grat.p$Label[4:7])
box()

sites.pollen.Tcold$length <- sites.pollen.Tcold$maxYear - sites.pollen.Tcold$minYear
summary(sites.pollen.Tcold$length)

plot.data <- pollen.Tcold.long[, c("site", "tw", "Tcold")] %>% 
  mutate(code = paste0(site, "_", tw)) %>% 
  left_join(y = mutate(CCSM3$tmean01, code =  paste0(site, "_", tw)), by = "code") %>%
  dplyr::select(site.x, tw.x, Tcold, tmean01) %>% 
  rename(site = site.x, tw = tw.x) %>%
  mutate(tmean01 = round(tmean01, 1)) %>% 
  group_by(site) %>%
  summarise(r = cor(Tcold, tmean01)) %>% 
  left_join(x = mutate(sites.pollen.Tcold, site = dataSetName), y=., by = "site") %>% 
  dplyr::select(site, X, Y, length, r)

plot.data$r.class <- berryFunctions::classify(plot.data$r, breaks = br, method="custom")$index
plot.data$l.class <- berryFunctions::classify(plot.data$length, breaks = c(0, 4000, 8000, 12000, 20000), method="custom")$index

plot.data <- plot.data[order(plot.data$l.class, abs(plot.data$r)*-1, decreasing = T), ]

points(plot.data[, c("X", "Y")], pch=21, cex= c(0.5, 1.5, 2.5, 3.5)[plot.data$l.class],
       bg =rev(brewer.pal(10,"RdYlBu"))[plot.data$r.class])

#Temperature of the warmest month-------------------------------------------------------
plot(NA, xlim=c(25e+05, 65e+05), ylim=c(15e+05, 55e+05), axes=F, asp=1, xlab="", ylab="",
     main = "Temperature of the warmest month (July)", cex.main=1.4)
rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col = "gray", border = NA)
plot(grat, add=T, lwd=1, col="white")

plot(mp, add=T, col="white")

axis(1, at=grat.p$X[4:7], labels = grat.p$Label[4:7])
axis(2, at=grat.p$Y[1:3], labels =grat.p$Label[1:3], las=1)
box()

sites.pollen.Twarm$length <- sites.pollen.Twarm$maxYear - sites.pollen.Twarm$minYear
summary(sites.pollen.Twarm$length)

plot.data <- pollen.Twarm.long[, c("site", "tw", "Twarm")] %>% 
  mutate(code = paste0(site, "_", tw)) %>% 
  left_join(y = mutate(CCSM3$tmean07, code =  paste0(site, "_", tw)), by = "code") %>%
  dplyr::select(site.x, tw.x, Twarm, tmean07) %>% 
  rename(site = site.x, tw = tw.x) %>%
  mutate(tmean07 = round(tmean07, 1)) %>% 
  group_by(site) %>%
  summarise(r = cor(Twarm, tmean07)) %>% 
  left_join(x = mutate(sites.pollen.Twarm, site = dataSetName), y=., by = "site") %>% 
  dplyr::select(site, X, Y, length, r)

plot.data$r.class <- berryFunctions::classify(plot.data$r, breaks = br, method="custom")$index
plot.data$l.class <- berryFunctions::classify(plot.data$length, breaks = c(0, 4000, 8000, 12000, 20000), method="custom")$index

plot.data <- plot.data[order(plot.data$l.class, abs(plot.data$r)*-1, decreasing = T), ]

points(plot.data[, c("X", "Y")], pch=21, cex= c(0.5, 1.5, 2.5, 3.5)[plot.data$l.class],
       bg =rev(brewer.pal(10,"RdYlBu"))[plot.data$r.class])

#Annual precipitation-------------------------------------------------------
plot(NA, xlim=c(25e+05, 65e+05), ylim=c(15e+05, 55e+05), axes=F, asp=1, xlab="", ylab="",
     main = "Annual precipitation", cex.main=1.4)
rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col = "gray", border = NA)
plot(grat, add=T, lwd=1, col="white")

plot(mp, add=T, col="white")

axis(1, at=grat.p$X[4:7], labels = grat.p$Label[4:7])
box()

sites.pollen.Pannual$length <- sites.pollen.Pannual$maxYear - sites.pollen.Pannual$minYear
summary(sites.pollen.Pannual$length)

plot.data <- pollen.Pannual.long[, c("site", "tw", "Pannual")] %>% 
  mutate(code = paste0(site, "_", tw)) %>% 
  left_join(y = mutate(CCSM3$bio12, code =  paste0(site, "_", tw)), by = "code") %>%
  dplyr::select(site.x, tw.x, Pannual, bio12) %>% 
  rename(site = site.x, tw = tw.x) %>%
  mutate(bio12 = round(bio12, 1)) %>% 
  group_by(site) %>%
  summarise(r = cor(Pannual, bio12)) %>% 
  left_join(x = mutate(sites.pollen.Pannual, site = dataSetName), y=., by = "site") %>% 
  dplyr::select(site, X, Y, length, r)

plot.data$r.class <- berryFunctions::classify(plot.data$r, breaks = br, method="custom")$index
plot.data$l.class <- berryFunctions::classify(plot.data$length, breaks = c(0, 4000, 8000, 12000, 20000), method="custom")$index

plot.data <- plot.data[order(plot.data$l.class, abs(plot.data$r)*-1, decreasing = T), ]

points(plot.data[, c("X", "Y")], pch=21, cex= c(0.5, 1.5, 2.5, 3.5)[plot.data$l.class],
       bg =rev(brewer.pal(10,"RdYlBu"))[plot.data$r.class])

#Legend
par(mfrow = c(1,1), mar = c(0,0,0,0), oma = c(0,0,0,0), new = TRUE)
plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")

raster::plot(raster(matrix(br)), 
             legend.only = TRUE, 
             col = rev(brewer.pal(10, "RdYlBu")), 
             breaks = br,   
             zlim = c(-1, 1),
             smallplot = c(0.91, 0.925, 0.30, 0.70), 
             axis.args = list(at = br, 
                              labels = format(br, nsmall = 1), 
                              cex.axis = 1,
                              lwd = 0,
                              lwd.ticks = 1,
                              tcl = -0.3,
                              mgp = c(3, 0.45, 0)),
             legend.args = list(text = 'Pearson correlation', 
                                side = 4, 
                                line = 2.5, 
                                cex = 1, 
                                font = 2))

dev.off()


