###PLOT CORRELATIONS IN TIME----------------------------------------------------
#Temporal changes in correlation between pollen-based and model-based
#temperatures and precipitation across Europe

library(rlist)
library(tidyverse)

CCSM3 <- list.load("temp12k.sites_CCSM3.rdata")
trace <- list.load("temp12k.sites_CHELSATraCE21k.rdata")

##Proxy vs CCSM3----------------------------------------------------------------
plot.data <- list()

plot.data$bio1 <- pollen.Tannual.long[, c("site", "tw", "Tannual")] %>% 
  mutate(code = paste0(site, "_", tw)) %>% 
  left_join(y = mutate(CCSM3$bio1, code =  paste0(site, "_", tw)), by = "code") %>%
  select(site.x, tw.x, Tannual, bio1, code) %>% 
  rename(site = site.x, tw = tw.x, bio1.ccsm3 = bio1) %>% 
  left_join(y = mutate(trace$bio1, code =  paste0(site, "_", tw)), by = "code") %>%
  select(site.x, tw.x, Tannual, bio1.ccsm3, bio1) %>%
  rename(site = site.x, tw = tw.x, bio1.chelsa = bio1) %>%
  mutate(bio1.ccsm3 = round(bio1.ccsm3, 1), bio1.chelsa = round(bio1.chelsa, 1)) %>% 
  filter(tw <= 10950) %>% 
  group_by(tw) %>% 
  summarise(ccsm3_r2.5 = cor.test(Tannual, bio1.ccsm3)$conf.int[1],
            ccsm3_r = cor(Tannual, bio1.ccsm3),
            ccsm3_r97.5 = cor.test(Tannual, bio1.ccsm3)$conf.int[2],
            chelsa_r2.5 = cor.test(Tannual, bio1.chelsa)$conf.int[1],
            chelsa_r = cor(Tannual, bio1.chelsa),
            chelsa_r97.5 = cor.test(Tannual, bio1.chelsa)$conf.int[2]) %>% 
  mutate(comparison =  "Mean annual temperature")

plot.data$tmean01 <- pollen.Tcold.long[, c("site", "tw", "Tcold")] %>% 
  mutate(code = paste0(site, "_", tw)) %>% 
  left_join(y = mutate(CCSM3$tmean01, code =  paste0(site, "_", tw)), by = "code") %>%
  select(site.x, tw.x, Tcold, tmean01, code) %>% 
  rename(site = site.x, tw = tw.x, tmean01.ccsm3 = tmean01) %>% 
  left_join(y = mutate(trace$tmean01, code =  paste0(site, "_", tw)), by = "code") %>%
  select(site.x, tw.x, Tcold, tmean01.ccsm3, tmean01) %>%
  rename(site = site.x, tw = tw.x, tmean01.chelsa = tmean01) %>%
  mutate(tmean01.ccsm3 = round(tmean01.ccsm3, 1), tmean01.chelsa = round(tmean01.chelsa, 1)) %>% 
  filter(tw <= 10950) %>% 
  group_by(tw) %>% 
  summarise(ccsm3_r2.5 = cor.test(Tcold, tmean01.ccsm3)$conf.int[1],
            ccsm3_r = cor(Tcold, tmean01.ccsm3),
            ccsm3_r97.5 = cor.test(Tcold, tmean01.ccsm3)$conf.int[2],
            chelsa_r2.5 = cor.test(Tcold, tmean01.chelsa)$conf.int[1],
            chelsa_r = cor(Tcold, tmean01.chelsa),
            chelsa_r97.5 = cor.test(Tcold, tmean01.chelsa)$conf.int[2]) %>% 
  mutate(comparison =  "Temperature of the coldest month (January)")

plot.data$tmean07 <- pollen.Twarm.long[, c("site", "tw", "Twarm")] %>% 
  mutate(code = paste0(site, "_", tw)) %>% 
  left_join(y = mutate(CCSM3$tmean07, code =  paste0(site, "_", tw)), by = "code") %>%
  select(site.x, tw.x, Twarm, tmean07, code) %>% 
  rename(site = site.x, tw = tw.x, tmean07.ccsm3 = tmean07) %>% 
  left_join(y = mutate(trace$tmean07, code =  paste0(site, "_", tw)), by = "code") %>%
  select(site.x, tw.x, Twarm, tmean07.ccsm3, tmean07) %>%
  rename(site = site.x, tw = tw.x, tmean07.chelsa = tmean07) %>%
  mutate(tmean07.ccsm3 = round(tmean07.ccsm3, 1), tmean07.chelsa = round(tmean07.chelsa, 1)) %>% 
  filter(tw <= 10950) %>% 
  group_by(tw) %>% 
  summarise(ccsm3_r2.5 = cor.test(Twarm, tmean07.ccsm3)$conf.int[1],
            ccsm3_r = cor(Twarm, tmean07.ccsm3),
            ccsm3_r97.5 = cor.test(Twarm, tmean07.ccsm3)$conf.int[2],
            chelsa_r2.5 = cor.test(Twarm, tmean07.chelsa)$conf.int[1],
            chelsa_r = cor(Twarm, tmean07.chelsa),
            chelsa_r97.5 = cor.test(Twarm, tmean07.chelsa)$conf.int[2]) %>% 
  mutate(comparison =  "Temperature of the warmest month (July)")


plot.data$bio12 <- pollen.Pannual.long[, c("site", "tw", "Pannual")] %>% 
  mutate(code = paste0(site, "_", tw)) %>% 
  left_join(y = mutate(CCSM3$bio12, code =  paste0(site, "_", tw)), by = "code") %>%
  select(site.x, tw.x, Pannual, bio12, code) %>% 
  rename(site = site.x, tw = tw.x, bio12.ccsm3 = bio12) %>% 
  left_join(y = mutate(trace$bio12, code =  paste0(site, "_", tw)), by = "code") %>%
  select(site.x, tw.x, Pannual, bio12.ccsm3, bio12) %>%
  rename(site = site.x, tw = tw.x, bio12.chelsa = bio12) %>%
  mutate(bio12.ccsm3 = round(bio12.ccsm3, 0), bio12.chelsa = round(bio12.chelsa, 0)) %>% 
  filter(tw <= 10950) %>% 
  group_by(tw) %>% 
  summarise(ccsm3_r2.5 = cor.test(Pannual, bio12.ccsm3)$conf.int[1],
            ccsm3_r = cor(Pannual, bio12.ccsm3),
            ccsm3_r97.5 = cor.test(Pannual, bio12.ccsm3)$conf.int[2],
            chelsa_r2.5 = cor.test(Pannual, bio12.chelsa)$conf.int[1],
            chelsa_r = cor(Pannual, bio12.chelsa),
            chelsa_r97.5 = cor.test(Pannual, bio12.chelsa)$conf.int[2]) %>% 
  mutate(comparison =  "Annual precipitation")

str(plot.data)
plot.data <- do.call(rbind.data.frame, plot.data)
plot.data$comparison <- factor(plot.data$comparison, 
                               levels = c("Mean annual temperature",
                                          "Temperature of the coldest month (January)",
                                          "Temperature of the warmest month (July)",
                                          "Annual precipitation"))

# ggplot(plot.data, aes(x = tw*-1)) +
#   geom_ribbon(aes(ymin = chelsa_r2.5, ymax = chelsa_r97.5), fill = "dodgerblue", alpha = 0.3) +
#   geom_ribbon(aes(ymin = ccsm3_r2.5, ymax = ccsm3_r97.5), fill = "gold", alpha = 0.3) +
#   geom_line(aes(y = chelsa_r), color = "dodgerblue", size = 1) +
#   geom_line(aes(y = ccsm3_r), color = "goldenrod", size = 1) +
#   coord_cartesian(ylim = c(0, 1)) +
#   scale_x_continuous(breaks = seq(0, -11000, -1000), labels = seq(0, 11, 1)) +
#   xlab("Years [ka BP]") +
#   ylab("Pearson correlation") +
#   theme_bw() +
#   theme(strip.text = element_text(face="bold"),
#         panel.grid.minor = element_blank()) +
#   facet_wrap(~ comparison, ncol=2)

ggplot(plot.data, aes(x = tw*-1)) +
  geom_ribbon(aes(ymin = ccsm3_r2.5, ymax = ccsm3_r97.5), fill = "gray", alpha = 0.5) +
  geom_line(aes(y = ccsm3_r), color = "black", size = 0.5) +
  coord_cartesian(ylim = c(0, 1)) +
  scale_x_continuous(breaks = seq(0, -11000, -1000), labels = seq(0, 11, 1)) +
  xlab("Years [ka BP]") +
  ylab("Pearson correlation") +
  theme_bw() +
  theme(strip.text = element_text(face="bold", size = 11),
        panel.grid.minor = element_blank(),
        strip.background = element_blank()) +
  facet_wrap(~ comparison, ncol=2)



