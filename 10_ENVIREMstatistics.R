###EXTRACT ENVIREM VARIABLES FOR EUROPE----------------------------------------- 

library(terra)
library(rlist)

envi.names <- c("annualPET", "aridityIndexThornthwaite", "climaticMoistureIndex",
                "continentality", "embergerQ", "growingDegDays0", "growingDegDays5",
                "maxTempColdest", "minTempWarmest", "monthCountByTemp10", "PETColdestQuarter",
                "PETDriestQuarter", "PETseasonality", "PETWarmestQuarter", "PETWettestQuarter",
                "thermicityIndex")

envirem.statistics <- list()

for(q in envi.names)
{
  print(q)
  
  ##Extract CCSM3 data
  paths <- paste0(getwd(), "/GIS_database/Paleoclimates_CCSM3/SAGA/FOR_SHARING/", 
                  q, "/", q, "_", seq(50, 20950, 100), "BP.tif")
  
  r <- terra::rast(paths)
  names(r) <- paste0(q, "_", seq(50, 20950, 100))
  
  ext2 <- terra::extract(r, rp10, ID=F)
  
  if(q %in% envi.names[c(2,4,8,9)]) { ext2 <- ext2/10}
  if(q %in% envi.names[3]) { ext2 <- ext2/1000}
  
  #Statistics for anomly curves---------------------------------------------
  stats <- data.frame(matrix(data=NA, ncol=10, nrow = ncol(ext)))
  colnames(stats) <- c(paste0("CCSM3_raw_", c("mean", "SD", "median", "Q25", "Q75")),
                       paste0("CCSM3_anomaly_", c("mean", "SD", "median", "Q25", "Q75")))
  rownames(stats) <- seq(50, 20950, 100)
  
  ##deviations from 50 BP
  if(q %in% envi.names[c(2:10,16)]) {
    dev2 <-  apply(ext2, 2, FUN = function(x){x - ext2[,1]})
  } else {
    ext2 <- ext2[ext2[,1] > 0, ]
    
    dev2 <-  apply(ext2, 2, FUN = function(x){((x - ext2[,1])/ext2[,1])*100})
  }
  
  #Mean for Europe
  stats$CCSM3_raw_mean <- colMeans(ext2, na.rm=T)
  stats$CCSM3_anomaly_mean <- colMeans(dev2, na.rm=T)
  
  ##Standard deviation for Europe
  stats$CCSM3_raw_SD <- apply(ext2, 2, FUN = sd, na.rm=T)
  stats$CCSM3_anomaly_SD <- apply(dev2, 2, FUN = sd, na.rm=T)
  
  ##Median for Europe
  stats$CCSM3_raw_median <- apply(ext2, 2, FUN = median, na.rm=T)
  stats$CCSM3_anomaly_median <- apply(dev2, 2, FUN = median, na.rm=T)
  
  ##Q25
  stats$CCSM3_raw_Q25 <- apply(ext2, 2, FUN = quantile, probs = 0.25, na.rm=T)
  stats$CCSM3_anomaly_Q25 <- apply(dev2, 2, FUN = quantile, probs = 0.25, na.rm=T)
  
  ##Q75
  stats$CCSM3_raw_Q75 <- apply(ext2, 2, FUN = quantile, probs = 0.75, na.rm=T)
  stats$CCSM3_anomaly_Q75 <- apply(dev2, 2, FUN = quantile, probs = 0.75, na.rm=T)
  
  stats$Variable <- q
  
  envirem.statistics[[q]] <- stats
}

list.save(envirem.statistics, file="CCSM3_Envirem_statistics.rdata")
