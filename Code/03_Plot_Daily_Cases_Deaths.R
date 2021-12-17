rm(list=ls())
library(tidyverse); library(lubridate);library(osfr); library(zoo); library(readxl); 
library(ggpubr); library(viridis); library(cowplot) 
Sys.setlocale("LC_TIME", "en_US.UTF-8")

# Loading data directly from OSF
#osf_retrieve_file("9dsfk") %>%
#  osf_download(conflicts = "overwrite")
# 
# idb <-  read_csv("Data/inputDB.zip",
#                 skip = 1,
#                 col_types = cols(.default = "c")) %>%
#  mutate(Date = dmy(Date),
#         Value = as.double(Value))
# 
# # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# # Estimate daily new cases and deaths for all ages and both sexes
# # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 
# # filter Germany and aggregate values for all ages and both sexes
# ger <- idb %>%
#  filter(Country == "Germany",
#         # exclude values from the ECDC
#         !str_detect(Code, "ECDC")) %>%
#  group_by(Country, Region, Date, Measure) %>%
#  summarise(Value = sum(Value)) %>%
#  ungroup() %>%
#  arrange(Country, Region, Measure, Date) %>%
#  group_by(Country, Region, Measure) %>%
#  mutate(New = Value - lag(Value)) %>%
#  ungroup()
# 
# pop <- read_excel("Data/Population2019.xlsx", range="A3:B20", col_names=TRUE)
# 
# ger <- merge(ger, pop, by = "Region", all=TRUE)
# 
# write_csv(ger, path="Output/Daily_Cases_Deaths.csv")

ger <- read_csv("Output/Daily_Cases_Deaths.csv")

# New cases and deaths have the problem of being affected by delays during 
# some week days, so it is better to smooth them using 7-day rolling average
# (splines are other option, better shape but kind of black-box)
ger_sm <- ger %>% 
  group_by(Country, Region, Measure) %>% 
  mutate(New_sm = rollapply(New, 7, mean, align = 'center', fill = NA)) %>% 
  ungroup() %>% 
  select(-Value) %>% 
  gather(New, New_sm, key = "Type", value = "New") %>% 
  mutate(Type = ifelse(Type == "New", "Observed", "Smoothed"))

ger_smoothed <- ger_sm %>% filter(Type=="Smoothed")

## Daily new cases and deaths per capita ####
ger_smoothed <- ger_smoothed %>% mutate(NewCapita = (New/Population)*1000000)

# Peaks
max_wave1cases <- ger_smoothed %>% filter(Measure =="Cases") %>%
  group_by(Region) %>%
  filter(Date <= dmy("01.05.2020")) %>%
  filter(NewCapita==max(NewCapita, na.rm=TRUE)) %>%
  select(Region, Date, NewCapita)

max_wave1deaths <- ger_smoothed %>% filter(Measure =="Deaths") %>%
  group_by(Region) %>%
  filter(Date <= dmy("01.05.2020")) %>%
  filter(NewCapita==max(NewCapita, na.rm=TRUE)) %>%
  select(Region, Date, NewCapita)

max_wave2cases <- ger_smoothed %>% filter(Measure =="Cases") %>%
  group_by(Region) %>%
  filter(Date >= dmy("01.11.2020"), Date <= dmy("15.02.2021")) %>%
  filter(NewCapita==max(NewCapita, na.rm=TRUE)) %>%
  select(Region, Date, NewCapita)

max_wave2deaths <- ger_smoothed %>% filter(Measure =="Deaths") %>%
  group_by(Region) %>%
  filter(Date >= dmy("01.11.2020")) %>%
  filter(NewCapita==max(NewCapita, na.rm=TRUE)) %>%
  select(Region, Date, NewCapita)



## Plot CFR ####
load("Output/analysis.RData")

total <- 
  total %>% 
  mutate(Region2=Region)

max_wave1CFR <- total %>%
  group_by(Region) %>%
  filter(Date <= dmy("01.01.2021"), Date >= dmy("15.03.2020")) %>%
  filter(CFR==max(CFR)) %>%
  select(Region, Date, CFR)

min_wave1CFR <- total %>%
  group_by(Region) %>%
  filter(Date >= dmy("01.10.2020"), Date <= dmy("20.12.2020")) %>%
  filter(CFR==min(CFR)) %>%
  select(Region, Date, CFR)

max_wave2CFR <- total %>%
  group_by(Region) %>%
  filter(Date >= dmy("01.01.2021")) %>%
  filter(CFR==max(CFR)) %>%
  select(Region, Date, CFR)

total <- 
  total %>% 
  mutate(Percent = CFR*100)


# Plots
tx <- 8

G_Cases_Capita <- 
  ger_smoothed %>% 
  filter(Measure == "Cases") %>% 
  ggplot(aes(x=Date, y=NewCapita))+
  geom_line(data=ger_smoothed %>% filter(Region != "All", Measure == "Cases"),
            aes(group=Region), color="gray70", size = 0.2, alpha = 0.9)+
  geom_line(data=ger_smoothed %>% filter(Region == "All", Measure == "Cases"),
            aes(color=Region), color="black", size = 0.5)+
  scale_x_date(date_breaks = "1 month", date_labels = "%b",
               limits = as.Date(c("2020-02-20", "2021-04-01")))+
  theme_bw()+
  theme(
    axis.title.y = element_text(size = tx),
    axis.text.y = element_text(size = tx - 1),
    axis.title.x = element_blank(),
    axis.text.x = element_text(size = tx - 1, angle = 60, hjust = 1))+
  ylab("New cases per million")

G_Deaths_Capita <- 
  ger_smoothed %>% 
  filter(Measure == "Deaths") %>% 
  ggplot(aes(x=Date, y=NewCapita))+
  geom_line(data=ger_smoothed %>% filter(Region!="All", Measure == "Deaths"),
            aes(group=Region), color="gray70", size = 0.2, alpha = 0.9)+
  geom_line(data=ger_smoothed %>% filter(Region=="All", Measure == "Deaths"),
            aes(color=Region), color="black", size = 0.5)+
  scale_x_date(date_breaks = "1 month", date_labels = "%b",
               limits = as.Date(c("2020-02-20", "2021-04-01")))+
  theme_bw()+
  theme(
    axis.title.y = element_text(size = tx),
    axis.text.y = element_text(size = tx - 1),
    axis.title.x = element_blank(),
    axis.text.x = element_text(size = tx - 1, angle = 60, hjust = 1))+
  ylab("New deaths per million")


G_CFR <- total %>%
  filter(Region2=="Germany") %>% 
  ggplot(aes(x=Date, y=Percent))+
  geom_line(data=total %>% dplyr::select(-Region), aes(group=Region2), 
            color="gray70", size = 0.2, alpha = 0.9)+
  geom_line(aes(color=Region), color="black", size = 0.5)+
  geom_vline(xintercept = as.Date("2020-04-28"), size = 0.3, color="gray10", lty=2)+
  geom_vline(xintercept=as.Date("2021-02-09"), size = 0.3, color="gray10", lty=2)+
  geom_vline(xintercept=as.Date("2020-11-13"), size = 0.3, color="gray10", lty=2)+
  scale_color_viridis(discrete=TRUE)+
  scale_y_continuous(limits=c(0,10), n.breaks=5)+
  scale_x_date(date_breaks = "1 month", date_labels = "%b",
               limits = as.Date(c("2020-02-20", "2021-04-01")))+
  theme_bw()+
  theme( 
    legend.position="none", 
    axis.title.y = element_text(size = tx),
    axis.text.y = element_text(size = tx - 1),
    axis.title.x = element_text(size = tx),
    axis.text.x = element_text(size = tx - 1))+
  ylab("Case-fatality rate in %")

top_row <- plot_grid(G_Cases_Capita, G_Deaths_Capita, labels = c('a', 'b'))
plot_grid(top_row, G_CFR, ncol = 1, labels = c('', 'c'))
ggsave("Figures/01_Plots_GER.pdf", width = 4.9, height = 4.9)
ggsave("Figures/01_Plots_GER.png", width = 4.9, height = 4.9, dpi = 600)

