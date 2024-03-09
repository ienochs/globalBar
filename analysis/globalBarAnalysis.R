library(readxl)
library(dplyr)
library(magrittr)
library(ggplot2)
library(ggpubr)

#get data
rootFolder <-getwd() #get root folder
barData_raw <- read_excel(paste(rootFolder,"/data/bar/NewCalBARDataFinal.xlsx",sep=""))#import data
daysDeployed = 871

#clean data
barData_clean <- barData_raw %>% 
  filter(fate=='Successfully Recovered') %>% #remove bars that weren't recovered
  select(-c(fate,regionCode,subRegion,projectCode,siteName,instrumentType,species,blockDimensions)) #remove unnecessary columns

#standardize data to surface area and days.
barDataStd <- barData_clean  %>% 
  #general values
  mutate(deltaMass_mgcm2y = 1000*(postWeightClean-preWeightEpoxied)/preSA/daysDeployed) %>%  
  mutate(deltaVol_mm3cm2y = 1000*(postCTVolume-preCTVolume)/preSA/daysDeployed) %>%  
  mutate(deltaDensity_mgcm3cm2y = 1000*(postCTDensity-preCTDensity)/preSA/daysDeployed) %>%  
  #macroboring values
  mutate(macroboringAnnelid_mm3cm2y = -1*1000*erosionAnnelid/preSA/daysDeployed) %>%  
  mutate(macroboringBivalve_mm3cm2y = -1*1000*erosionBivalve/preSA/daysDeployed) %>%  
  mutate(macroboringSponge_mm3cm2y = -1*1000*erosionSponge/preSA/daysDeployed) %>%  
  mutate(macroboringTotal_mm3cm2y = macroboringAnnelid_mm3cm2y + macroboringBivalve_mm3cm2y + macroboringSponge_mm3cm2y) %>%  
  #accretion values
  mutate(accretionBivalve_mm3cm2y = 1000*accretionBivalve/preSA/daysDeployed) %>%  
  mutate(accretionCCA_mm3cm2y = 1000*accretionCCA/preSA/daysDeployed) %>%    
  mutate(accretionCoral_mm3cm2y = 1000*accretionCoral/preSA/daysDeployed) %>%    
  mutate(accretionVermetid_mm3cm2y = 1000*accretionVermetid/preSA/daysDeployed) %>%    
  mutate(accretionTotal_mm3cm2y = accretionBivalve_mm3cm2y + accretionCCA_mm3cm2y + accretionCoral_mm3cm2y + accretionVermetid_mm3cm2y) %>% 
  #grazing
  mutate(grazing_mm3cm2y = deltaVol_mm3cm2y-accretionTotal_mm3cm2y-macroboringTotal_mm3cm2y)

#General Carbonate Alteration
#Volume Change
plot_deltaVolume_site <-barDataStd %>%
  ggplot(aes(locationIan, deltaVol_mm3cm2y)) +
  geom_boxplot(fill = c("#ffbf00","#ffdc73","#398564","#419873","#52bf90"), outlier.shape = NA) +
  labs(x = "Site", y = expression(paste("Volume (", mm^{3}, " ", cm^{-2}, " ", yr^{-1}, ")")))+
  geom_jitter(width = 0.2, height = 0)+
  theme_light()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), aspect.ratio = .75)
plot_deltaVolume_zone <-barDataStd %>%
  ggplot(aes(zone, deltaVol_mm3cm2y)) +
  geom_boxplot(fill = c("#a67c00","#317256"), outlier.shape = NA) +
  geom_jitter(width = 0.2, height = 0)+
  theme_light()+
  rremove("ylab")+
  rremove("y.text")+
  labs(x = "Zone")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), aspect.ratio = 1.5)
plot_deltaVolume <- ggarrange(plot_deltaVolume_site, plot_deltaVolume_zone,  
                              ncol = 2, nrow = 1, widths = c(1.73,1), align = "v")
#Mass Change
plot_deltaMass_site <-barDataStd %>%
  ggplot(aes(locationIan, deltaMass_mgcm2y)) +
  geom_boxplot(fill = c("#ffbf00","#ffdc73","#398564","#419873","#52bf90"), outlier.shape = NA) +
  labs(x = "Site", y = expression(paste("Mass (mg ", cm^{-2}, " ", yr^{-1}, ")")))+
  geom_jitter(width = 0.2, height = 0)+
  theme_light()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), aspect.ratio = .75)
plot_deltaMass_zone <-barDataStd %>%
  ggplot(aes(zone, deltaMass_mgcm2y)) +
  geom_boxplot(fill = c("#a67c00","#317256"), outlier.shape = NA) +
  geom_jitter(width = 0.2, height = 0)+
  theme_light()+
  rremove("ylab")+
  rremove("y.text")+
  labs(x = "Zone")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), aspect.ratio = 1.5)
plot_deltaMass <- ggarrange(plot_deltaMass_site, plot_deltaMass_zone,  
  ncol = 2, nrow = 1, widths = c(1.73,1), align = "v")
#Denisty Change
plot_deltaDensity_site <-barDataStd %>%
  ggplot(aes(locationIan, deltaDensity_mgcm3cm2y)) +
  geom_boxplot(fill = c("#ffbf00","#ffdc73","#398564","#419873","#52bf90"), outlier.shape = NA) +
  labs(x = "Site", y = expression(paste("Density (mg ", cm^{-3}, " ", cm^{-2}, " ", yr^{-1}, ")")))+
  geom_jitter(width = 0.2, height = 0)+
  theme_light()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), aspect.ratio = .75)
plot_deltaDensity_zone <-barDataStd %>%
  ggplot(aes(zone, deltaDensity_mgcm3cm2y)) +
  geom_boxplot(fill = c("#a67c00","#317256"), outlier.shape = NA) +
  geom_jitter(width = 0.2, height = 0)+
  theme_light()+
  rremove("ylab")+
  rremove("y.text")+
  labs(x = "Zone")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), aspect.ratio = 1.5)
plot_deltaDensity <- ggarrange(plot_deltaDensity_site, plot_deltaDensity_zone,  
                            ncol = 2, nrow = 1, widths = c(1.73,1), align = "v")
#Mass vs volume point
plot_massVsVolume <- barDataStd %>%
  ggplot(aes(deltaVol_mm3cm2y,deltaMass_mgcm2y)) +
  geom_point(aes(colour = factor(locationIan)),show.legend = FALSE) +
  scale_color_manual(values = c("C1" = "#ffbf00","C2" = "#ffdc73","M1" = "#398564","M2" = "#419873","M3" = "#52bf90"))+
  labs(x = expression(paste("Volume (", mm^{3}, " ", cm^{-2}, " ", yr^{-1}, ")")), y = expression(paste("Mass (mg ", cm^{-2}, " ", yr^{-1}, ")")))+
  rremove("legend")+
  theme_light()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), aspect.ratio = 1)
#Density vs volume point
plot_densityVsVolume <- barDataStd %>%
  ggplot(aes(deltaVol_mm3cm2y,deltaDensity_mgcm3cm2y)) +
  geom_point(aes(colour = factor(locationIan)),show.legend = FALSE) +
  scale_color_manual(values = c("C1" = "#ffbf00","C2" = "#ffdc73","M1" = "#398564","M2" = "#419873","M3" = "#52bf90"))+
  labs(x = expression(paste("Volume (", mm^{3}, " ", cm^{-2}, " ", yr^{-1}, ")")), y = expression(paste("Density (mg ", cm^{-3}, " ", cm^{-2}, " ", yr^{-1}, ")")))+
  rremove("legend")+
  theme_light()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), aspect.ratio = 1)

#Macroboring
#Annelid
plot_macroboringAnnelid_site <-barDataStd %>%
  ggplot(aes(locationIan, macroboringAnnelid_mm3cm2y)) +
  geom_boxplot(fill = c("#ffbf00","#ffdc73","#398564","#419873","#52bf90"), outlier.shape = NA) +
  labs(x = "Site", y = expression(paste("Annelid (", mm^{3}, " ", cm^{-2}, " ", yr^{-1}, ")")))+
  geom_jitter(width = 0.2, height = 0)+
  theme_light()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), aspect.ratio = .75)
plot_macroboringAnnelid_zone <-barDataStd %>%
  ggplot(aes(zone, macroboringAnnelid_mm3cm2y)) +
  geom_boxplot(fill = c("#a67c00","#317256"), outlier.shape = NA) +
  geom_jitter(width = 0.2, height = 0)+
  theme_light()+
  rremove("ylab")+
  rremove("y.text")+
  labs(x = "Zone")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), aspect.ratio = 1.5)
plot_macroboringAnnelid <- ggarrange(plot_macroboringAnnelid_site, plot_macroboringAnnelid_zone,  
  ncol = 2, nrow = 1, widths = c(1.73,1), align = "v")
#Bivalve
plot_macroboringBivalve_site <-barDataStd %>%
  ggplot(aes(locationIan, macroboringBivalve_mm3cm2y)) +
  geom_boxplot(fill = c("#ffbf00","#ffdc73","#398564","#419873","#52bf90"), outlier.shape = NA) +
  labs(x = "Site", y = expression(paste("Bivalve (", mm^{3}, " ", cm^{-2}, " ", yr^{-1}, ")")))+
  geom_jitter(width = 0.2, height = 0)+
  theme_light()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), aspect.ratio = .75)
plot_macroboringBivalve_zone <-barDataStd %>%
  ggplot(aes(zone, macroboringBivalve_mm3cm2y)) +
  geom_boxplot(fill = c("#a67c00","#317256"), outlier.shape = NA) +
  geom_jitter(width = 0.2, height = 0)+
  theme_light()+
  rremove("ylab")+
  rremove("y.text")+
  labs(x = "Zone")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), aspect.ratio = 1.5)
plot_macroboringBivalve <- ggarrange(plot_macroboringBivalve_site, plot_macroboringBivalve_zone,  
  ncol = 2, nrow = 1, widths = c(1.73,1), align = "v")
#Sponge
plot_macroboringSponge_site <-barDataStd %>%
  ggplot(aes(locationIan, macroboringSponge_mm3cm2y)) +
  geom_boxplot(fill = c("#ffbf00","#ffdc73","#398564","#419873","#52bf90"), outlier.shape = NA) +
  labs(x = "Site", y = expression(paste("Sponge (", mm^{3}, " ", cm^{-2}, " ", yr^{-1}, ")")))+
  geom_jitter(width = 0.2, height = 0)+
  theme_light()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), aspect.ratio = .75)
plot_macroboringSponge_zone <-barDataStd %>%
  ggplot(aes(zone, macroboringSponge_mm3cm2y)) +
  geom_boxplot(fill = c("#a67c00","#317256"), outlier.shape = NA) +
  geom_jitter(width = 0.2, height = 0)+
  theme_light()+
  rremove("ylab")+
  rremove("y.text")+
  labs(x = "Zone")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), aspect.ratio = 1.5)
plot_macroboringSponge <- ggarrange(plot_macroboringSponge_site, plot_macroboringSponge_zone,  
  ncol = 2, nrow = 1, widths = c(1.73,1), align = "v")
#Macroboring Total
plot_macroboringTotal_site <-barDataStd %>%
  ggplot(aes(locationIan, macroboringTotal_mm3cm2y)) +
  geom_boxplot(fill = c("#ffbf00","#ffdc73","#398564","#419873","#52bf90"), outlier.shape = NA) +
  labs(x = "Site", y = expression(paste("Total (", mm^{3}, " ", cm^{-2}, " ", yr^{-1}, ")")))+
  geom_jitter(width = 0.2, height = 0)+
  theme_light()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), aspect.ratio = .75)
plot_macroboringTotal_zone <-barDataStd %>%
  ggplot(aes(zone, macroboringTotal_mm3cm2y)) +
  geom_boxplot(fill = c("#a67c00","#317256"), outlier.shape = NA) +
  geom_jitter(width = 0.2, height = 0)+
  theme_light()+
  rremove("ylab")+
  rremove("y.text")+
  labs(x = "Zone")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), aspect.ratio = 1.5)
plot_macroboringTotal <- ggarrange(plot_macroboringTotal_site, plot_macroboringTotal_zone,  
  ncol = 2, nrow = 1, widths = c(1.73,1), align = "v")

#Accretion
#Bivalve
plot_accretionBivalve_site <-barDataStd %>%
  ggplot(aes(locationIan, accretionBivalve_mm3cm2y)) +
  geom_boxplot(fill = c("#ffbf00","#ffdc73","#398564","#419873","#52bf90"), outlier.shape = NA) +
  labs(x = "Site", y = expression(paste("Bivalve (", mm^{3}, " ", cm^{-2}, " ", yr^{-1}, ")")))+
  geom_jitter(width = 0.2, height = 0)+
  theme_light()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), aspect.ratio = .75)
plot_accretionBivalve_zone <-barDataStd %>%
  ggplot(aes(zone, accretionBivalve_mm3cm2y)) +
  geom_boxplot(fill = c("#a67c00","#317256"), outlier.shape = NA) +
  geom_jitter(width = 0.2, height = 0)+
  theme_light()+
  rremove("ylab")+
  rremove("y.text")+
  labs(x = "Zone")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), aspect.ratio = 1.5)
plot_accretionBivalve <- ggarrange(plot_accretionBivalve_site, plot_accretionBivalve_zone,  
  ncol = 2, nrow = 1, widths = c(1.73,1), align = "v")
#CCA
plot_accretionCCA_site <-barDataStd %>%
  ggplot(aes(locationIan, accretionCCA_mm3cm2y)) +
  geom_boxplot(fill = c("#ffbf00","#ffdc73","#398564","#419873","#52bf90"), outlier.shape = NA) +
  labs(x = "Site", y = expression(paste("CCA (", mm^{3}, " ", cm^{-2}, " ", yr^{-1}, ")")))+
  geom_jitter(width = 0.2, height = 0)+
  theme_light()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), aspect.ratio = .75)
plot_accretionCCA_zone <-barDataStd %>%
  ggplot(aes(zone, accretionCCA_mm3cm2y)) +
  geom_boxplot(fill = c("#a67c00","#317256"), outlier.shape = NA) +
  geom_jitter(width = 0.2, height = 0)+
  theme_light()+
  rremove("ylab")+
  rremove("y.text")+
  labs(x = "Zone")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), aspect.ratio = 1.5)
plot_accretionCCA <- ggarrange(plot_accretionCCA_site, plot_accretionCCA_zone,  
  ncol = 2, nrow = 1, widths = c(1.73,1), align = "v")
#Coral
plot_accretionCoral_site <-barDataStd %>%
  ggplot(aes(locationIan, accretionCoral_mm3cm2y)) +
  geom_boxplot(fill = c("#ffbf00","#ffdc73","#398564","#419873","#52bf90"), outlier.shape = NA) +
  labs(x = "Site", y = expression(paste("Coral (", mm^{3}, " ", cm^{-2}, " ", yr^{-1}, ")")))+
  geom_jitter(width = 0.2, height = 0)+
  theme_light()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), aspect.ratio = .75)
plot_accretionCoral_zone <-barDataStd %>%
  ggplot(aes(zone, accretionCoral_mm3cm2y)) +
  geom_boxplot(fill = c("#a67c00","#317256"), outlier.shape = NA) +
  geom_jitter(width = 0.2, height = 0)+
  theme_light()+
  rremove("ylab")+
  rremove("y.text")+
  labs(x = "Zone")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), aspect.ratio = 1.5)
plot_accretionCoral <- ggarrange(plot_accretionCoral_site, plot_accretionCoral_zone,  
  ncol = 2, nrow = 1, widths = c(1.73,1), align = "v")
#Vermetid
plot_accretionVermetid_site <-barDataStd %>%
  ggplot(aes(locationIan, accretionVermetid_mm3cm2y)) +
  geom_boxplot(fill = c("#ffbf00","#ffdc73","#398564","#419873","#52bf90"), outlier.shape = NA) +
  labs(x = "Site", y = expression(paste("Vermetid (", mm^{3}, " ", cm^{-2}, " ", yr^{-1}, ")")))+
  geom_jitter(width = 0.2, height = 0)+
  theme_light()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), aspect.ratio = .75)
plot_accretionVermetid_zone <-barDataStd %>%
  ggplot(aes(zone, accretionVermetid_mm3cm2y)) +
  geom_boxplot(fill = c("#a67c00","#317256"), outlier.shape = NA) +
  geom_jitter(width = 0.2, height = 0)+
  theme_light()+
  rremove("ylab")+
  rremove("y.text")+
  labs(x = "Zone")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), aspect.ratio = 1.5)
plot_accretionVermetid <- ggarrange(plot_accretionVermetid_site, plot_accretionVermetid_zone,  
  ncol = 2, nrow = 1, widths = c(1.73,1), align = "v")
#Total
plot_accretionTotal_site <-barDataStd %>%
  ggplot(aes(locationIan, accretionTotal_mm3cm2y)) +
  geom_boxplot(fill = c("#ffbf00","#ffdc73","#398564","#419873","#52bf90"), outlier.shape = NA) +
  labs(x = "Site", y = expression(paste("Total (", mm^{3}, " ", cm^{-2}, " ", yr^{-1}, ")")))+
  geom_jitter(width = 0.2, height = 0)+
  theme_light()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), aspect.ratio = .75)
plot_accretionTotal_zone <-barDataStd %>%
  ggplot(aes(zone, accretionTotal_mm3cm2y)) +
  geom_boxplot(fill = c("#a67c00","#317256"), outlier.shape = NA) +
  geom_jitter(width = 0.2, height = 0)+
  theme_light()+
  rremove("ylab")+
  rremove("y.text")+
  labs(x = "Zone")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), aspect.ratio = 1.5)
plot_accretionTotal <- ggarrange(plot_accretionTotal_site, plot_accretionTotal_zone,  
  ncol = 2, nrow = 1, widths = c(1.73,1), align = "v")

#Grazing
plot_grazing_site <-barDataStd %>%
  ggplot(aes(locationIan, grazing_mm3cm2y)) +
  geom_boxplot(fill = c("#ffbf00","#ffdc73","#398564","#419873","#52bf90"), outlier.shape = NA) +
  labs(x = "Site", y = expression(paste("Grazing (", mm^{3}, " ", cm^{-2}, " ", yr^{-1}, ")")))+
  geom_jitter(width = 0.2, height = 0)+
  theme_light()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), aspect.ratio = .75)
plot_grazing_zone <-barDataStd %>%
  ggplot(aes(zone, grazing_mm3cm2y)) +
  geom_boxplot(fill = c("#a67c00","#317256"), outlier.shape = NA) +
  geom_jitter(width = 0.2, height = 0)+
  theme_light()+
  rremove("ylab")+
  rremove("y.text")+
  labs(x = "Zone")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), aspect.ratio = 1.5)
plot_grazing <- ggarrange(plot_grazing_site, plot_grazing_zone,  
  ncol = 2, nrow = 1, widths = c(1.73,1), align = "v")

#General Plots
plot_generalValues <- ggarrange(plot_deltaVolume,NULL,plot_deltaMass, plot_massVsVolume, plot_deltaDensity, plot_densityVsVolume,
                                ncol = 2, nrow = 3, widths = c(1.73,1))
plot_macroboring <- ggarrange(plot_macroboringAnnelid, plot_macroboringBivalve, plot_macroboringSponge, plot_macroboringTotal,
                                ncol = 1, nrow = 4, widths = c(1.73,1))
plot_accretion <- ggarrange(plot_accretionBivalve, plot_accretionCCA, plot_accretionCoral, plot_accretionVermetid, plot_accretionTotal,
                              ncol = 1, nrow = 5, widths = c(1.73,1))
plot_generalValues
plot_macroboring
plot_accretion
plot_grazing