###TEMPERATURE AND PRECIPITAION ANOMALIES IN EUROPE-----------------------------
library(tidyverse)
library(patchwork)
library(rlist)

##Load CCSM3 and CHELSA TraCE21K data for Temp12k sites 
CCSM3 <- list.load("temp12k.sites_CCSM3.rdata")
trace <- list.load("temp12k.sites_CHELSATraCE21k.rdata")

tiff("Anomalies.tif", width = 8.5, height = 6.8, res=500, units="in", compression = "lzw")
plots <- list()

##Mean annual temperature-------------------------------------------------------
##proxy
plot.data1 <- pollen.Tannual.long[, c("site", "tw", "Tannual")] %>% 
  left_join(y=Tann.50.RF, by = "site") %>% 
  mutate(dev = Tannual - RF50) %>% 
  group_by(tw) %>% summarise(SDminus = mean(dev) - sd(dev), 
                             mean = mean(dev),
                             SDplus = mean(dev) + sd(dev))
plot.data1 <- plot.data1[1:110,]
plot.data1$Source <- "pollen"

##ccsm3
plot.data2 <- pollen.Tannual.long[, c("site", "tw")] %>% 
  mutate(code = paste0(site, "_", tw)) %>% 
  left_join(y = mutate(CCSM3$bio1, code =  paste0(site, "_", tw)), by = "code") %>%
  rename(site = site.x, tw = tw.x) %>% 
  select(site, tw, bio1) %>% 
  left_join(y= filter(CCSM3$bio1, tw == "50"), by = "site") %>% 
  rename(tw = tw.x) %>% 
  mutate(bio1.x = round(bio1.x, 1), bio1.y = round(bio1.y, 1)) %>% 
  mutate(dev = bio1.x - bio1.y) %>% 
  group_by(tw) %>% 
  summarise(SDminus = mean(dev) - sd(dev), 
            mean = mean(dev),
            SDplus = mean(dev) + sd(dev))
plot.data2 <- plot.data2[1:110,] 
plot.data2$Source <- "ccsm3"

plot.data <- left_join(x = plot.data1, y = plot.data2, by="tw", suffix = c(".pollen", ".ccsm3"))

plots[[1]] <- ggplot(plot.data, aes(x = tw*-1)) +
  geom_ribbon(aes(ymin = SDminus.pollen, ymax = SDplus.pollen), fill = "gold", alpha = 0.3) +
  geom_ribbon(aes(ymin = SDminus.ccsm3, ymax = SDplus.ccsm3), fill = "dodgerblue", alpha = 0.3) +
  geom_line(aes(y = mean.pollen), color = "goldenrod", size = 0.7) +
  geom_line(aes(y = mean.ccsm3), color = "dodgerblue", size = 0.7) +
  geom_hline(yintercept = 0, linetype = 'dashed', size = 0.5) +
  scale_x_continuous(breaks = seq(0, -11000, -1000), labels = seq(0, 11, 1)) +
  labs(title = "Mean annual temperature", 
       y = "Temperature anomaly [°C]", x = NULL) +
  theme_bw() +
  theme(strip.text = element_text(face="bold", size = 11),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        plot.title = element_text(size = 12, face = "bold", hjust = 0.5))

##Temperature of the coldest month (January)-------------------------------------
##proxy
plot.data1 <- pollen.Tcold.long[, c("site", "tw", "Tcold")] %>% 
  left_join(y=Tcold.50.RF, by = "site") %>% 
  mutate(dev = Tcold - RF50) %>% 
  group_by(tw) %>% summarise(SDminus = mean(dev) - sd(dev), 
                             mean = mean(dev),
                             SDplus = mean(dev) + sd(dev))
plot.data1 <- plot.data1[1:110,]

##ccsm3
plot.data2 <- pollen.Tcold.long[, c("site", "tw")] %>% 
  mutate(code = paste0(site, "_", tw)) %>% 
  left_join(y = mutate(CCSM3$tmean01, code =  paste0(site, "_", tw)), by = "code") %>%
  rename(site = site.x, tw = tw.x) %>% 
  select(site, tw, tmean01) %>% 
  left_join(y= filter(CCSM3$tmean01, tw == "50"), by = "site") %>% 
  rename(tw = tw.x) %>% 
  mutate(tmean01.x = round(tmean01.x, 1), tmean01.y = round(tmean01.y, 1)) %>% 
  mutate(dev = tmean01.x - tmean01.y) %>% 
  group_by(tw) %>% 
  summarise(SDminus = mean(dev) - sd(dev), 
            mean = mean(dev),
            SDplus = mean(dev) + sd(dev))
plot.data2 <- plot.data2[1:110,]

plot.data <- left_join(x = plot.data1, y = plot.data2, by="tw", suffix = c(".pollen", ".ccsm3"))

plots[[2]] <- ggplot(plot.data, aes(x = tw*-1)) +
  geom_ribbon(aes(ymin = SDminus.pollen, ymax = SDplus.pollen), fill = "gold", alpha = 0.3) +
  geom_ribbon(aes(ymin = SDminus.ccsm3, ymax = SDplus.ccsm3), fill = "dodgerblue", alpha = 0.3) +
  geom_line(aes(y = mean.pollen), color = "goldenrod", size = 0.7) +
  geom_line(aes(y = mean.ccsm3), color = "dodgerblue", size = 0.7) +
  geom_hline(yintercept = 0, linetype = 'dashed', size = 0.5) +
  scale_x_continuous(breaks = seq(0, -11000, -1000), labels = seq(0, 11, 1)) +
  scale_y_continuous(breaks = seq(-6, 4, 2)) +
  labs(title = "Temperature of the coldest month (January)", 
       y = "Temperature anomaly [°C]", x = NULL) +
  theme_bw() +
  theme(strip.text = element_text(face="bold", size = 11),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        plot.title = element_text(size = 12, face = "bold", hjust = 0.5))


##Temperature of the warmest month (July)-------------------------------------
##proxy
plot.data1 <- pollen.Twarm.long[, c("site", "tw", "Twarm")] %>% 
  left_join(y=Twarm.50.RF, by = "site") %>% 
  mutate(dev = Twarm - RF50) %>% 
  group_by(tw) %>% summarise(SDminus = mean(dev) - sd(dev), 
                             mean = mean(dev),
                             SDplus = mean(dev) + sd(dev))
plot.data1 <- plot.data1[1:110,]

##ccsm3
plot.data2 <- pollen.Twarm.long[, c("site", "tw")] %>% 
  mutate(code = paste0(site, "_", tw)) %>% 
  left_join(y = mutate(CCSM3$tmean07, code =  paste0(site, "_", tw)), by = "code") %>%
  rename(site = site.x, tw = tw.x) %>% 
  select(site, tw, tmean07) %>% 
  left_join(y= filter(CCSM3$tmean07, tw == "50"), by = "site") %>% 
  rename(tw = tw.x) %>% 
  mutate(tmean07.x = round(tmean07.x, 1), tmean07.y = round(tmean07.y, 1)) %>% 
  mutate(dev = tmean07.x - tmean07.y) %>% 
  group_by(tw) %>% 
  summarise(SDminus = mean(dev) - sd(dev), 
            mean = mean(dev),
            SDplus = mean(dev) + sd(dev))
plot.data2 <- plot.data2[1:110,]

plot.data <- left_join(x = plot.data1, y = plot.data2, by="tw", suffix = c(".pollen", ".ccsm3"))

plots[[3]] <- ggplot(plot.data, aes(x = tw*-1)) +
  geom_ribbon(aes(ymin = SDminus.pollen, ymax = SDplus.pollen), fill = "gold", alpha = 0.3) +
  geom_ribbon(aes(ymin = SDminus.ccsm3, ymax = SDplus.ccsm3), fill = "dodgerblue", alpha = 0.3) +
  geom_line(aes(y = mean.pollen), color = "goldenrod", size = 0.7) +
  geom_line(aes(y = mean.ccsm3), color = "dodgerblue", size = 0.7) +
  geom_hline(yintercept = 0, linetype = 'dashed', size = 0.5) +
  scale_x_continuous(breaks = seq(0, -11000, -1000), labels = seq(0, 11, 1)) +
  labs(title = "Temperature of the warmest month (July)", 
       y = "Temperature anomaly [°C]", x = NULL) +
  theme_bw() +
  theme(strip.text = element_text(face="bold", size = 11),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        plot.title = element_text(size = 12, face = "bold", hjust = 0.5))


##Annual precipitation----------------------------------------------------------
##proxy
plot.data1 <- pollen.Pannual.long[, c("site", "tw", "Pannual")] %>% 
  left_join(y = Pann.50.RF, by = "site") %>% 
  mutate(dev = ((Pannual - RF50)/RF50)*100) %>% 
  group_by(tw) %>% summarise(SDminus = mean(dev) - sd(dev), 
                             mean = mean(dev),
                             SDplus = mean(dev) + sd(dev))
plot.data1 <- plot.data1[1:110,]

##ccsm3
plot.data2 <- pollen.Pannual.long[, c("site", "tw")] %>%
  mutate(code = paste0(site, "_", tw)) %>% 
  left_join(y = mutate(CCSM3$bio12, code =  paste0(site, "_", tw)), by = "code") %>%
  rename(site = site.x, tw = tw.x) %>% 
  select(site, tw, bio12) %>% 
  left_join(y= filter(CCSM3$bio12, tw == "50"), by = "site") %>% 
  rename(tw = tw.x) %>% 
  mutate(bio12.x = round(bio12.x, 0), bio12.y = round(bio12.y, 0)) %>%
  mutate(dev = ((bio12.x - bio12.y)/bio12.y)*100) %>% 
  group_by(tw) %>% 
  summarise(SDminus = mean(dev) - sd(dev), 
            mean = mean(dev),
            SDplus = mean(dev) + sd(dev))
plot.data2 <- plot.data2[1:110,] 

plot.data <- left_join(x = plot.data1, y = plot.data2, by="tw", suffix = c(".pollen", ".ccsm3"))

plots[[4]] <- ggplot(plot.data, aes(x = tw*-1)) +
  geom_ribbon(aes(ymin = SDminus.pollen, ymax = SDplus.pollen), fill = "gold", alpha = 0.3) +
  geom_ribbon(aes(ymin = SDminus.ccsm3, ymax = SDplus.ccsm3), fill = "dodgerblue", alpha = 0.3) +
  geom_line(aes(y = mean.pollen), color = "goldenrod", size = 0.7) +
  geom_line(aes(y = mean.ccsm3), color = "dodgerblue", size = 0.7) +
  geom_hline(yintercept = 0, linetype = 'dashed', size = 0.5) +
  scale_x_continuous(breaks = seq(0, -11000, -1000), labels = seq(0, 11, 1)) +
  scale_y_continuous(breaks = seq(-20, 40, 10)) +
  labs(title = "Annual precipitation", 
       y = "Precipitation change [%]", x = NULL) +
  theme_bw() +
  theme(strip.text = element_text(face="bold", size = 11),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        plot.title = element_text(size = 12, face = "bold", hjust = 0.5))

##draw all plots
wrap_plots(plots, nrow = 2, ncol = 2) +
  plot_annotation(
    caption = "Years [ka BP]",
    theme = theme(
      plot.margin = margin(2, 2, 5, 5, "pt"),
      plot.caption = element_text(hjust = 0.5, size = 11, vjust = 1),
      axis.text = element_text(size = 11)
    )
  )

dev.off()



