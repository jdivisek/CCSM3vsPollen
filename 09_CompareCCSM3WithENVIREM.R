###EXTRACT DATA FROM ENVIREM DATABASE-------------------------------------------
library(raster)
library(tidyverse)
library(berryFunctions)
library(hydroGOF)

##read coordinates of 10,000 random points
rp10 <- read.delim("rp10.txt", header=T, row.names = 1)
head(rp10)
plot(rp10)

envi.names <- c("annualPET", "aridityIndexThornthwaite", "climaticMoistureIndex",
                "continentality", "embergerQ", "growingDegDays0", "growingDegDays5",
                "maxTempColdest", "minTempWarmest", "monthCountByTemp10", "PETColdestQuarter",
                "PETDriestQuarter", "PETseasonality", "PETWarmestQuarter", "PETWettestQuarter",
                "thermicityIndex")

ENV.GCM.code <- c("CC", "MR", "ME") ##codes of GCMs in the ENVIREM database
ENV.GCM.name <- c("CCSM4", "MIROC-ESM", "MPI-ESM-P") ##names of GCMs in the ENVIREM database

### Middle Holocene and LGM
ENV.GCM.mid.data <- list()
ENV.GCM.LGM.data <- list()

for(i in seq(1, length(ENV.GCM.code)))
{
  print(ENV.GCM.name[i])
  
  ##mid-Holocene
  rs <- stack(paste0(getwd(), "/GIS_database/ENVIREM/Paleoclimate/Mid-Holocene/", 
                     ENV.GCM.code[i], "/holo_", gsub("-", "_", tolower(ENV.GCM.name[i])), "_2-5arcmin_", envi.names, ".tif"))
  
  ENV.GCM.mid.data[[ENV.GCM.code[i]]] <- as.data.frame(raster::extract(rs, rp10))
  colnames(ENV.GCM.mid.data[[ENV.GCM.code[i]]]) <- envi.names
  
  #transformations
  ENV.GCM.mid.data[[ENV.GCM.code[i]]]$growingDegDays0 <- ENV.GCM.mid.data[[ENV.GCM.code[i]]]$growingDegDays0 / 10
  ENV.GCM.mid.data[[ENV.GCM.code[i]]]$growingDegDays5 <- ENV.GCM.mid.data[[ENV.GCM.code[i]]]$growingDegDays5 / 10
  ENV.GCM.mid.data[[ENV.GCM.code[i]]]$maxTempColdest <- ENV.GCM.mid.data[[ENV.GCM.code[i]]]$maxTempColdest / 10
  ENV.GCM.mid.data[[ENV.GCM.code[i]]]$minTempWarmest <- ENV.GCM.mid.data[[ENV.GCM.code[i]]]$minTempWarmest / 10
  
  ##LGM
  rs <- stack(paste0(getwd(), "/GIS_database/ENVIREM/Paleoclimate/LGM/", 
                     ENV.GCM.code[i], "/lgm_", gsub("-", "_", tolower(ENV.GCM.name[i])), "_2-5arcmin_", envi.names, ".tif"))
  
  ENV.GCM.LGM.data[[ENV.GCM.code[i]]] <- as.data.frame(raster::extract(rs, rp10))
  colnames(ENV.GCM.LGM.data[[ENV.GCM.code[i]]]) <- envi.names
  
  #transformations
  ENV.GCM.LGM.data[[ENV.GCM.code[i]]]$growingDegDays0 <- ENV.GCM.LGM.data[[ENV.GCM.code[i]]]$growingDegDays0 / 10
  ENV.GCM.LGM.data[[ENV.GCM.code[i]]]$growingDegDays5 <- ENV.GCM.LGM.data[[ENV.GCM.code[i]]]$growingDegDays5 / 10
  ENV.GCM.LGM.data[[ENV.GCM.code[i]]]$maxTempColdest <- ENV.GCM.LGM.data[[ENV.GCM.code[i]]]$maxTempColdest / 10
  ENV.GCM.LGM.data[[ENV.GCM.code[i]]]$minTempWarmest <- ENV.GCM.LGM.data[[ENV.GCM.code[i]]]$minTempWarmest / 10
  
}

###EXTRACT CCSM3-BASED ENVIREM VARIABLES----------------------------------------

##Mid-Holocene
rs1 <- stack(paste0(getwd(), "/GIS_database/Paleoclimates_CCSM3/SAGA/FOR_SHARING/", envi.names, "/", envi.names, "_5950BP.tif"))
ext1 <- as.data.frame(raster::extract(rs1, rp10))

rs2 <- stack(paste0(getwd(), "/GIS_database/Paleoclimates_CCSM3/SAGA/FOR_SHARING/", envi.names, "/", envi.names, "_6050BP.tif"))
ext2 <- as.data.frame(raster::extract(rs2, rp10))

CCSM3.envi.mid.data <- as.data.frame((ext1 + ext2)/2)
colnames(CCSM3.envi.mid.data) <- envi.names

##apply scaling factor
CCSM3.envi.mid.data$aridityIndexThornthwaite <- CCSM3.envi.mid.data$aridityIndexThornthwaite / 10
CCSM3.envi.mid.data$climaticMoistureIndex <- CCSM3.envi.mid.data$climaticMoistureIndex / 1000
CCSM3.envi.mid.data$continentality <- CCSM3.envi.mid.data$continentality / 10
CCSM3.envi.mid.data$maxTempColdest <- CCSM3.envi.mid.data$maxTempColdest / 10
CCSM3.envi.mid.data$minTempWarmest <- CCSM3.envi.mid.data$minTempWarmest / 10

rm(ext1, ext2, rs1, rs2)

##LGM
rs <- stack(paste0(getwd(), "/GIS_database/Paleoclimates_CCSM3/SAGA/FOR_SHARING/", envi.names, "/", envi.names, "_20950BP.tif"))

CCSM3.envi.LGM.data <- as.data.frame(raster::extract(rs, rp10))
colnames(CCSM3.envi.LGM.data) <- envi.names

CCSM3.envi.LGM.data$aridityIndexThornthwaite <- CCSM3.envi.LGM.data$aridityIndexThornthwaite / 10
CCSM3.envi.LGM.data$climaticMoistureIndex <- CCSM3.envi.LGM.data$climaticMoistureIndex / 1000
CCSM3.envi.LGM.data$continentality <- CCSM3.envi.LGM.data$continentality / 10
CCSM3.envi.LGM.data$maxTempColdest <- CCSM3.envi.LGM.data$maxTempColdest / 10
CCSM3.envi.LGM.data$minTempWarmest <- CCSM3.envi.LGM.data$minTempWarmest / 10

##Calculate Willmott's dr and normalized mean error-----------------------------
##Mid-Holocene
env.dr.mid <- list()
for(i in ENV.GCM.mid.code)
{
  env.dr.mid[[i]] <- hydroGOF::dr(sim = as.matrix(CCSM3.envi.mid.data), obs = as.matrix(ENV.GCM.mid.data[[i]]), na.rm = TRUE)
}
env.dr.mid <- do.call(rbind, env.dr.mid) %>% t() %>% as.data.frame()

env.SME.mid <- list()
for(i in ENV.GCM.mid.code)
{
  env.SME.mid[[i]] <- hydroGOF::me(sim = as.matrix(CCSM3.envi.mid.data), obs = as.matrix(ENV.GCM.mid.data[[i]]), na.rm = TRUE) / apply(ENV.GCM.mid.data[[i]], 2, FUN= sd, na.rm=T)
}
env.SME.mid <- do.call(rbind, env.SME.mid) %>% t() %>% as.data.frame()

##LGM
env.dr.lgm <- list()
for(i in ENV.GCM.LGM.code)
{
  env.dr.lgm[[i]] <- hydroGOF::dr(sim = as.matrix(CCSM3.envi.LGM.data), obs = as.matrix(ENV.GCM.LGM.data[[i]]), na.rm = TRUE)
}
env.dr.lgm <- do.call(rbind, env.dr.lgm) %>% t() %>% as.data.frame()

env.SME.lgm <- list()
for(i in ENV.GCM.LGM.code)
{
  env.SME.lgm[[i]] <- hydroGOF::me(sim = as.matrix(CCSM3.envi.LGM.data), obs = as.matrix(ENV.GCM.LGM.data[[i]]), na.rm = TRUE) / apply(ENV.GCM.LGM.data[[i]], 2, FUN= sd, na.rm=T)
}
env.SME.lgm <- do.call(rbind, env.SME.lgm) %>% t() %>% as.data.frame()

#Assemble data for plotting
plot.data <- list()

dr <- cbind(env.dr.lgm, NA ,env.dr.mid)
rownames(dr) <- envi.names

r <- raster(as.matrix(dr), xmn=0, xmx=ncol(dr), ymn=0, ymx=nrow(dr))
plot.data$dr <- rasterToPoints(r)

sme <- cbind(env.SME.lgm, NA ,env.SME.mid)
rownames(sme) <- envi.names

r <- raster(as.matrix(sme), xmn=0, xmx=ncol(sme), ymn=0, ymx=nrow(sme))
plot.data$sme <- rasterToPoints(r)

##Plot heatmap------------------------------------------------------------------
tiff("ENVIREM_heatmap.tif", width = 13, height = 12, units = "cm", res = 400, compression = "lzw")

m <- matrix(data=0, ncol=(7*2), nrow=16+4)
m[1:16, 1:14] <- 1
m[17:20, 1:7] <- 2
m[17:20, 8:14] <- 3

par(mar=c(0,0,0,0), oma=c(0.3,8,7,2), xaxs="i", yaxs="i")
layout(m)

##Willmott's dr
plot(plot.data$dr[,1:2], type='n', axes=F, xlab="", ylab="", asp=1, ylim=c(0,16), xlim=c(0,14))

br <- seq(0.5,1,0.05)
cols <- inferno(length(br)-1)
colPoints(x=plot.data$dr[,1], y=plot.data$dr[,2], z=plot.data$dr[,3], col=cols,
          method = "custom", breaks=br, pch=15, cex=3.2, axes=F, add=T, legend = F, asp=1)
text(x=rep(0, 16), y=seq(0.5,15.5), labels=rev(c(envi.names)), cex=1, xpd=NA, pos=2)
text(x=seq(0.5, 6.5), y=rep(16.2,7), labels= c(ENV.GCM.name, NA, ENV.GCM.name), cex=1, xpd=NA, adj=c(0,0), srt=45)

#Mormalized mean error
br <- c(-0.6,-0.5,-0.4,-0.3,-0.2,-0.1,0,0.1,0.2,0.3,0.4,0.5,0.6)
my.ramp.cold <- colorRampPalette(c("dodgerblue", "white")); my.ramp.hot <- colorRampPalette(c("white", "red3"))
cols <- c(my.ramp.cold(sum(br<=0))[1:sum(br<0)], my.ramp.hot(sum(br>=0))[2:sum(br>=0)])
clas <- cut(plot.data$sme[,3], br, include.lowest = TRUE, labels=1:(length(br)-1))
colPoints(x=plot.data$sme[,1]+8, y=plot.data$sme[,2], z=plot.data$sme[,3], col=cols,
          method = "custom", breaks=br ,pch=15, cex=3.2, add=T, legend = F, asp=1)
text(x=seq(0.5, 6.5)+8, y=rep(16.2,7), labels= c(ENV.GCM.name, NA, ENV.GCM.name), cex=1, xpd=NA, adj=c(0,0), srt=45)
text(x = 1.5, y= 20, labels = "LGM", xpd=NA, cex = 1.3, font = 2)
text(x = 5.5, y= 20, labels = "mid-Holocene", xpd=NA, cex = 1.3, font = 2)
text(x = 9.5, y= 20, labels = "LGM", xpd=NA, cex = 1.3, font = 2)
text(x = 13.5, y= 20, labels = "mid-Holocene", xpd=NA, cex = 1.3, font = 2)

#plot legends
br <- seq(0.5,1,0.05)
cols <- inferno(length(br)-1)
clas <- cut(plot.data$dr[,3], br, include.lowest = TRUE, labels=1:(length(br)-1))
plot(1, type="n", axes=F, xlab="", ylab="")
colPointsLegend(z=plot.data$dr[,3], 
                col=cols, x1=0.1, x2=0.9, y1=0.2, y2=0.8, density = F, lines=T, 
                title="Willmott's refined index of agreement", cex=1, atminmax=F,
                bb = br, at=seq(0.6,0.9,0.1), labels = seq(0.6,0.9,0.1))

br <- c(-0.6,-0.5,-0.4,-0.3,-0.2,-0.1,0,0.1,0.2,0.3,0.4,0.5,0.6)
my.ramp.cold <- colorRampPalette(c("dodgerblue", "white")); my.ramp.hot <- colorRampPalette(c("white", "red3"))
cols <- c(my.ramp.cold(sum(br<=0))[1:sum(br<0)], my.ramp.hot(sum(br>=0))[2:sum(br>=0)])
clas <- cut(plot.data$sme[,3], br, include.lowest = TRUE, labels=1:(length(br)-1))
plot(1, type="n", axes=F, xlab="", ylab="")
colPointsLegend(z=plot.data$sme[,3], 
                col=cols, x1=0.1, x2=0.9, y1=0.2, y2=0.8, density = F, lines=T, 
                title="Normalized mean error", cex=1, atminmax=F,
                bb = br, at=seq(-0.4,0.4,0.2), labels = seq(-0.4,0.4,0.2))


dev.off()

