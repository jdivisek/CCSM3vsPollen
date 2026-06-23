###PLOT ANOMALIES FOR ALL VARIABLES---------------------------------------------
library(rlist)
library(tidyverse)
library(ggh4x)

envi.names <- c("annualPET", "aridityIndexThornthwaite", "climaticMoistureIndex",
                "continentality", "embergerQ", "growingDegDays0", "growingDegDays5",
                "maxTempColdest", "minTempWarmest", "monthCountByTemp10", "PETColdestQuarter",
                "PETDriestQuarter", "PETseasonality", "PETWarmestQuarter", "PETWettestQuarter",
                "thermicityIndex")

bioclim.stats <- list.load("CCSM3_&_CHELSA_Bioclim_statistics.rdata")
bioclim.stats <- lapply(bioclim.stats, FUN = rownames_to_column, var = "tw")

bioclim.stats <- do.call(rbind.data.frame, bioclim.stats)
bioclim.stats$Variable <- factor(bioclim.stats$Variable, labels = paste0("bio", 1:19))
bioclim.stats$tw <- as.numeric(bioclim.stats$tw)

envirem.stats <- list.load("CCSM3_Envirem_statistics.rdata")
envirem.stats <- lapply(envirem.stats, FUN = rownames_to_column, var = "tw")

envirem.stats <- do.call(rbind.data.frame, envirem.stats)
envirem.stats$Variable <- factor(envirem.stats$Variable, labels = envi.names)
envirem.stats$tw <- as.numeric(envirem.stats$tw)

custom_labels <- c(
  "bio1" = "bio1 [°C]",
  "bio2" = "bio2 [°C]",
  "bio3" = "bio3 [°C × 100]",
  "bio4" = "bio4 [°C × 100]",
  "bio5" = "bio5 [°C]",
  "bio6" = "bio6 [°C]",
  "bio7" = "bio7 [°C]",
  "bio8" = "bio8 [°C]",
  "bio9" = "bio9 [°C]",
  "bio10" = "bio10 [°C]",
  "bio11" = "bio11 [°C]",
  "bio12" = "bio12 [%]",
  "bio13" = "bio13 [%]",
  "bio14" = "bio14 [%]",
  "bio15" = "bio15 [%]",
  "bio16" = "bio16 [%]",
  "bio17" = "bio17 [%]",
  "bio18" = "bio18 [%]",
  "bio19" = "bio19 [%]",
  "annualPET" = "annualPET [%]",
  "aridityIndexThornthwaite" = "aridityIndexThornthwaite [index]",
  "climaticMoistureIndex" = "climaticMoistureIndex [index]",
  "continentality" = "continentality [°C]",
  "embergerQ" = "embergerQ [index]",
  "growingDegDays0" = "growingDegDays0 [°C]",
  "growingDegDays5" = "growingDegDays5 [°C]",
  "maxTempColdest" = "maxTempColdest [°C]",
  "minTempWarmest" = "minTempWarmest [°C]",
  "monthCountByTemp10" = "monthCountByTemp10 [months]",
  "PETColdestQuarter" = "PETColdestQuarter [%]",
  "PETDriestQuarter" = "PETDriestQuarter [%]",
  "PETseasonality" = "PETseasonality [%]",
  "PETWarmestQuarter" = "PETWarmestQuarter [%]",
  "PETWettestQuarter" = "PETWettestQuarter [%]",
  "thermicityIndex" = "thermicityIndex [°C × 10]"
  
)

tiff("Anomalies_all_vars.tif", width = 10, height = 14, units = "in", res=400, compression = "lzw")
ggplot(rbind(bioclim.stats, envirem.stats), aes(x = tw*-1)) +
  geom_ribbon(aes(ymin = CHELSA_anomaly_mean - CHELSA_anomaly_SD, ymax = CHELSA_anomaly_mean + CHELSA_anomaly_SD), fill = "coral2", alpha = 0.3) +
  geom_ribbon(aes(ymin = CCSM3_anomaly_mean - CCSM3_anomaly_SD, ymax = CCSM3_anomaly_mean + CCSM3_anomaly_SD), fill = "dodgerblue", alpha = 0.3) +
  geom_line(aes(y = CHELSA_anomaly_mean), color = "coral2", linewidth = 0.5) +
  geom_line(aes(y = CCSM3_anomaly_mean), color = "dodgerblue", linewidth = 0.5) +
  geom_hline(yintercept = 0, 
             linetype = 'dashed', 
             linewidth = 0.5) +
  xlab("Years [ka BP]") +
  ylab(NULL) +
  scale_x_continuous(breaks = seq(0, -20000, -5000), labels = seq(0, 20, 5)) +
  theme_bw() +
  theme(strip.text = element_text(face="bold"),
        strip.background = element_blank()) +
  facet_wrap2(vars(Variable), ncol= 4, scales = "free_y", 
              strip.position = "top",
              labeller = as_labeller(custom_labels))
dev.off()


