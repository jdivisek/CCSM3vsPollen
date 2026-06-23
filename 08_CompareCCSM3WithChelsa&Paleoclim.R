###COMPARE CCSM3-BASED BIOCLIM VARIABLES WITH CHELSA TRACE21K AND PALEOCLIM DATABASES------------------------------------------------------ 

library(terra)
library(rlist)
library(hydroGOF)
library(plyr)
library(viridis)
library(berryFunctions)

##read coordinates of 10,000 random points
rp10 <- read.delim("rp10.txt", header=T, row.names = 1)
head(rp10)
plot(rp10)

##time windows of CHELSA TraCE21K data
tw <- seq(0, 21000, 100)
trace.tw <- c(paste0("00", seq(20, 10, -1)),
              paste0("000", seq(9, 1, -1)),
              "0000", 
              paste0("-00", seq(1, 9, 1)),
              paste0("-0", seq(10, 99, 1)),
              paste0("-", seq(100, 190, 1)))

cbind(tw, trace.tw)

##BIOCLIM variables in CHELSA TraCE21K
bio.names <- c(paste0("bio0", 1:9), paste0("bio", 10:19))

TRaCE.dr <- list()
TRaCE.SME <- list()

bioclim.statistics <- list()

##Loop for BIOCLIM variables
for(q in bio.names)
{
  print(q)
  
  ##Extract CHELSA TraCE21k data
  ch.paths <- paste0(getwd(), "/GIS_database/CHELSA_TraCE21k/", q, "/CHELSA_TraCE21k_", q, "_", trace.tw, "_V.1.0.tif")
  
  r <- terra::rast(ch.paths)
  names(r) <- paste0(q, "_", tw)
  
  ext.chelsa <- terra::extract(r, rp10, ID=F)
  
  ##aling data with CCSM3 windows
  ext.chelsa <- t(apply(ext.chelsa, 1, FUN = function(y){ approx(x = tw, y = y, xout = seq(50, 20950, 100))$y }))
  colnames(ext.chelsa) <- paste0(q, "_", seq(50, 20950, 100))
  
  ##calculate temperatures in °C
  if(q %in% bio.names[c(1,5:11)]) { ext.chelsa <- ext.chelsa - 273.15}
  
  ##Extract CCSM3 data
  bio.paths <- paste0(getwd(), "/GIS_database/Paleoclimates_CCSM3/SAGA/FOR_SHARING/", 
                  gsub("0", "", q), "/", gsub("0", "", q), "_", seq(50, 20950, 100), "BP.tif")
  
  if(q %in% bio.names[2:3]){
    bio.paths <- paste0(getwd(), "/GIS_database/Paleoclimates_CCSM3/SAGA/FOR_SHARING/", 
                    gsub("0", "", q), "_corrected/", gsub("0", "", q), "_", seq(50, 20950, 100), "BP.tif")
  }
  
  if(q %in% bio.names[10:19]){
    bio.paths <- paste0(getwd(), "/GIS_database/Paleoclimates_CCSM3/SAGA/FOR_SHARING/", 
                    q, "/", q, "_", seq(50, 20950, 100), "BP.tif")
  }
  
  r <- terra::rast(paths)
  names(r) <- paste0(q, "_", seq(50, 20950, 100))
  
  ext.ccsm3 <- terra::extract(r, rp10, ID=F)
  
  if(q %in% bio.names[1:11]) { ext.ccsm3 <- ext.ccsm3/10} ##apply scaling factor
  
  ##Willmott's d
  TRaCE.dr[[q]] <- hydroGOF::dr(sim = ext.ccsm3, obs = ext.chelsa, na.rm = TRUE)
  #Mean error normalized by standard deviation
  TRaCE.SME[[q]] <- hydroGOF::me(sim = ext.ccsm3, obs = ext.chelsa) / apply(ext.chelsa, 2, FUN = sd)

  ##Statistics for anomaly curves-----------------------------------------------
  stats <- data.frame(matrix(data=NA, ncol=20, nrow = ncol(ext.chelsa)))
  colnames(stats) <- c(paste0("CHELSA_raw_", c("mean", "SD", "median", "Q25", "Q75")),
                       paste0("CHELSA_anomaly_", c("mean", "SD", "median", "Q25", "Q75")),
                       paste0("CCSM3_raw_", c("mean", "SD", "median", "Q25", "Q75")),
                       paste0("CCSM3_anomaly_", c("mean", "SD", "median", "Q25", "Q75")))
  rownames(stats) <- seq(50, 20950, 100)

  ##deviations from 50 BP
  #absolute deviations for temperature and realtive for precipitation
  if(q %in% bio.names[c(1:11,15)]) {
    dev <-  apply(ext.chelsa, 2, FUN = function(x){x - ext.chelsa[,1]})
    dev2 <-  apply(ext.ccsm3, 2, FUN = function(x){x - ext.ccsm3[,1]})
  } else {
    ext.chelsa <- ext.chelsa[ext.chelsa[,1] > 0, ]
    ext.ccsm3 <- ext.ccsm3[ext.ccsm3[,1] > 0, ]

    dev <-  apply(ext.chelsa, 2, FUN = function(x){((x - ext.chelsa[,1])/ext.chelsa[,1])*100})
    dev2 <-  apply(ext.ccsm3, 2, FUN = function(x){((x - ext.ccsm3[,1])/ext.ccsm3[,1])*100})
  }

  #Mean for Europe
  stats$CHELSA_raw_mean <- colMeans(ext.chelsa, na.rm=T)
  stats$CCSM3_raw_mean <- colMeans(ext.ccsm3, na.rm=T)
  stats$CHELSA_anomaly_mean <- colMeans(dev, na.rm=T)
  stats$CCSM3_anomaly_mean <- colMeans(dev2, na.rm=T)

  ##Standard deviation for Europe
  stats$CHELSA_raw_SD <- apply(ext.chelsa, 2, FUN = sd, na.rm=T)
  stats$CCSM3_raw_SD <- apply(ext.ccsm3, 2, FUN = sd, na.rm=T)
  stats$CHELSA_anomaly_SD <- apply(dev, 2, FUN = sd, na.rm=T)
  stats$CCSM3_anomaly_SD <- apply(dev2, 2, FUN = sd, na.rm=T)

  ##Median for Europe
  stats$CHELSA_raw_median <- apply(ext.chelsa, 2, FUN = median, na.rm=T)
  stats$CCSM3_raw_median <- apply(ext.ccsm3, 2, FUN = median, na.rm=T)
  stats$CHELSA_anomaly_median <- apply(dev, 2, FUN = median, na.rm=T)
  stats$CCSM3_anomaly_median <- apply(dev2, 2, FUN = median, na.rm=T)

  ##Q25
  stats$CHELSA_raw_Q25 <- apply(ext.chelsa, 2, FUN = quantile, probs = 0.25, na.rm=T)
  stats$CCSM3_raw_Q25 <- apply(ext.ccsm3, 2, FUN = quantile, probs = 0.25, na.rm=T)
  stats$CHELSA_anomaly_Q25 <- apply(dev, 2, FUN = quantile, probs = 0.25, na.rm=T)
  stats$CCSM3_anomaly_Q25 <- apply(dev2, 2, FUN = quantile, probs = 0.25, na.rm=T)

  ##Q75
  stats$CHELSA_raw_Q75 <- apply(ext.chelsa, 2, FUN = quantile, probs = 0.75, na.rm=T)
  stats$CCSM3_raw_Q75 <- apply(ext.ccsm3, 2, FUN = quantile, probs = 0.75, na.rm=T)
  stats$CHELSA_anomaly_Q75 <- apply(dev, 2, FUN = quantile, probs = 0.75, na.rm=T)
  stats$CCSM3_anomaly_Q75 <- apply(dev2, 2, FUN = quantile, probs = 0.75, na.rm=T)

  stats$Variable <- q

  bioclim.statistics[[q]] <- stats
}

list.save(TRaCE.dr, file = "TRaCE.dr.rdata")
list.save(TRaCE.SME, file = "TRaCE.SME.rdata")
list.save(bioclim.statistics, file="CCSM3_&_CHELSA_Bioclim_statistics.rdata")

##Assemble data
TRaCE.dr <- data.frame(mean = unlist(lapply(TRaCE.dr, FUN = mean)),
                       sd = unlist(lapply(TRaCE.dr, FUN = sd)),
                       err = unlist(lapply(TRaCE.dr, FUN = sd))/sqrt(210))
TRaCE.dr$bioclim <- paste0("bio", 1:19)
TRaCE.dr$comparison <- "Willmott's refined index of agreement"

TRaCE.SME <- data.frame(mean = unlist(lapply(TRaCE.SME, FUN = mean)),
                        sd = unlist(lapply(TRaCE.SME, FUN = sd)),
                        err = unlist(lapply(TRaCE.SME, FUN = sd))/sqrt(210))
TRaCE.SME$bioclim <- paste0("bio", 1:19)
TRaCE.SME$comparison <- "Normalized mean error"

##Plot Willmott's dr and normaized ME-------------------------------------------
plot.data1 <- rbind(TRaCE.dr, TRaCE.SME)
plot.data1$comparison <- factor(plot.data1$comparison, levels=c("Willmott's refined index of agreement", "Normalized mean error"))
plot.data1$bioclim <- factor(plot.data1$bioclim , levels=rev(paste0("bio", 1:19)))

ggplot(plot.data1, aes(x = mean, y = bioclim, 
                      xmin = mean-sd, 
                      xmax = mean+sd)) +
  geom_errorbar(linewidth = 0.5) +
  geom_point(size = 4, shape = 16) +
  geom_vline(xintercept = 0, 
             linetype = 'dashed', 
             linewidth = 1, 
             layout = 2) +
  xlab("") + 
  ylab("") + 
  theme_bw() +
  theme(strip.text = element_text(face="bold")) +
  facet_wrap(~ comparison, ncol=3, scales="free_x")


plot.data1$Dataset <- "CHELSA-TraCE21k-centennial-bioclim"

###PALEOCLIM DATABASE-----------------------------------------------------------

##Willmott's dr
Paleo.dr <- list()
for(i in paleoclim.code)
{
  Paleo.dr[[i]] <- wilmott_dr(sim = as.matrix(CCSM3.paleoclim.data[[i]]), obs = as.matrix(paleoclim.data[[i]]), na.rm = TRUE)
}
Paleo.dr <- as.data.frame(do.call(cbind, Paleo.dr))
head(Paleo.dr)
Paleo.dr <- Paleo.dr[, rev(colnames(Paleo.dr))]

##Normalized ME
Paleo.SME <- list()
for(i in paleoclim.code)
{
  Paleo.SME[[i]] <- hydroGOF::me(sim = as.matrix(CCSM3.paleoclim.data[[i]]), obs = as.matrix(paleoclim.data[[i]])) / apply(paleoclim.data[[i]], 2, FUN = sd, na.rm=T)
  
}
Paleo.SME <- as.data.frame(do.call(cbind, Paleo.SME))
head(Paleo.SME)
Paleo.SME <- Paleo.SME[, rev(colnames(Paleo.SME))]

##Plot Willmott's dr and normaized ME-------------------------------------------

plot.data2 <- rbind(data.frame(mean = rowMeans(Paleo.dr),
                               sd = apply(Paleo.dr, 1, sd),
                               err = apply(Paleo.dr, 1, sd)/sqrt(6),
                               comparison = "Willmott's refined index of agreement"),
                    data.frame(mean = rowMeans(Paleo.SME),
                               sd = apply(Paleo.SME, 1, sd),
                               err = apply(Paleo.SME, 1, sd)/sqrt(6),
                               comparison = "Normalized mean error"))
plot.data2$comparison <- factor(plot.data2$comparison, levels=c("Willmott's refined index of agreement", "Normalized mean error"))
plot.data2$bioclim <- c(paste0("bio", 1:19), paste0("bio", 1:19))
plot.data2$bioclim <- factor(plot.data2$bioclim, levels = rev(paste0("bio", 1:19)))

ggplot(plot.data2, aes(x = mean, y = bioclim, 
                       xmin = mean-sd, 
                       xmax = mean+sd)) +
  geom_errorbarh(linewidth = 0.5) +
  geom_point(size = 4, shape = 16) +
  geom_vline(xintercept = 0, 
             linetype = 'dashed', 
             linewidth = 1, 
             layout = 2) +
  xlab("") + 
  ylab("") + 
  theme_bw() +
  theme(strip.text = element_text(face="bold")) +
  facet_wrap(~ comparison, ncol=3, scales="free_x")

plot.data2$Dataset <- "PaleoClim"

###Plot comparison with both datasets-------------------------------------------

plot.data <- rbind(plot.data1, plot.data2)
plot.data$Dataset <- factor(plot.data$Dataset, levels = c("CHELSA-TraCE21k-centennial-bioclim", "PaleoClim"))

tiff("CCSM3_vs_CHELSATraCE+PaleoClim.tif", width = 6.8, height = 6.4, units = "in", res=500, compression = "lzw")
ggplot(plot.data, aes(x = mean, y = bioclim, 
                      xmin = mean-sd, 
                      xmax = mean+sd)) +
  geom_errorbar(linewidth = 0.3) +
  geom_point(size = 3.5, shape = 16) +
  geom_vline(xintercept = 0, 
             linetype = 'dashed', 
             linewidth = 1, 
             layout = c(2,4)) +
  xlab("") + 
  ylab("") + 
  theme_bw() +
  theme(strip.text = element_text(face="bold")) +
  facet_grid(vars(Dataset), vars(comparison), scales="free_x")

dev.off()

