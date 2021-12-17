library(tidyverse); library(lubridate); library(sf)

# Map for Decomposition Across German federal states

# Data with coordenates for the map
ger_map <- read_rds("Data/gadm36_DEU_1_sf.rds")

# Data from Decomposition Across States
load("Output/analysis.RData")

cfr <- total %>% filter(Date=="2021-02-09") %>% 
  mutate(Percent = CFR*100) %>% select(Region, Percent)
cfr %>% arrange(Percent)

cfr$Region <- recode(cfr$Region, "Bavaria"="Bayern", "Hesse"="Hessen",
                     "Lower Saxony"="Niedersachsen", "North Rhine-Westphalia"="Nordrhein-Westfalen",
                     "Rhineland-Palatinate"="Rheinland-Pfalz", "Saxony"="Sachsen", 
                     "Saxony-Anhalt"="Sachsen-Anhalt", "Thuringia"="Thüringen", 
                     "Meckl.-Western Pomerania"="Mecklenburg-Vorpommern")



# Map for CFRs ###############################################################
cfr <- cfr %>% rename(NAME_1=Region)

# Add to map
ger_map5 <- ger_map %>%
  left_join(cfr)

cfr %>% arrange(Percent)

# Plot
ger_map5 %>% 
  ggplot() + 
  geom_sf(aes(fill = Percent), col = "black", size = 0.2)+
  #scale_fill_gradient2(low = "#00441b", mid = "#d9f0d3", high = "#762a83", midpoint = 0, "CFR)+
  #scale_fill_viridis_c(option="plasma", "CFR", direction=-1)+ # or "magma" or "inferno" or _b 
  scale_fill_distiller(palette = "BuPu", "Case-fatality rate", direction=1)+
  theme_bw()+
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.background=element_blank(),
        panel.grid.minor=element_blank(),
        panel.grid.major = element_blank(),
        plot.background=element_blank())

ggsave("Figures/03_map_CFR_20210209.pdf", dpi = 600)
ggsave("Figures/03_map_CFR_20210209.png", dpi = 600)

