library(tidyverse)
library(lubridate)
library(osfr)
library(cowplot)

# ~~~~~~~~~~~~~~~~~~~~~~~~~
# Age composition over time
# ~~~~~~~~~~~~~~~~~~~~~~~~~

# confirmed cases and deaths
# ~~~~~~~~~~~~~~~~~~~~~~~~~~
lvs <- rev(c("0", "5", "15", "35", "60", "80"))

to_eng_names <- 
  c("All" = "Germany", 
    "Bayern" = "Bavaria", 
    "Hessen" = "Hesse",
    "Niedersachsen" = "Lower Saxony", 
    "Nordrhein-Westfalen" = "North Rhine-Westphalia",
    "Rheinland-Pfalz" = "Rhineland-Palatinate", 
    "Sachsen" = "Saxony", 
    "Sachsen-Anhalt" = "Saxony-Anhalt", 
    "Thüringen" = "Thuringia", 
    "Mecklenburg-Vorpommern" = "Meckl.-Western Pomerania")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Loading COVerAGE-DB data input
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 
# First, load and prepare the data once for further analysis
#
# idb <-
#   read_csv("Data/inputDB.zip",
#            skip = 1,
#            col_types = cols(.default = "c"),
#            locale = locale(encoding = stringi::stri_enc_get())) %>%
#   mutate(Date = dmy(Date),
#          Value = as.double(Value))
#  
# ger_age <-
#   idb %>%
#   filter(Country == "Germany",
#          !str_detect(Code, "ECDC"),
#          Age != "UNK") %>%
#   mutate(Age = as.integer(Age)) %>%
#   # aggregate both sexes
#   group_by(Country, Region, Short, Date, Measure, Age) %>%
#   summarise(Value = sum(Value)) %>%
#   ungroup() %>%
#   group_by(Country, Region, Short, Date, Measure) %>%
#   mutate(All = sum(Value),
#          Prop = Value / All) %>%
#   ungroup()
#  
#  write_rds(ger_age, "Output/germany_cases_deaths_age.rds")
####

ger_age <- read_rds("Output/germany_cases_deaths_age.rds") %>% 
  mutate(Region = case_when(str_detect(Region, "Baden") ~ "Baden-Württemberg",
                            str_detect(Region, "ringen") ~ "Thüringen",
                            TRUE ~ Region),
         Age = factor(Age, levels = lvs)) %>% 
  select(Region, Date, Measure, Age, Prop)

unique(ger_age$Age)

# codes <- 
#   ger_age %>% 
#   select(Region, Short) %>% 
#   unique()

# population data by state
# ~~~~~~~~~~~~~~~~~~~~~~~~
pop <- 
  read_csv2("Data/germany_pop_states.csv",
            skip = 4,
            locale = locale(encoding = stringi::stri_enc_get()))

pop_reg <- 
  pop %>% 
  select(-1) %>% 
  rename(Age = 1) %>% 
  mutate(Age = str_sub(Age, 1, 2),
         Age = str_remove(Age, "-"),
         Age = ifelse(Age == "un", 0, as.integer(Age))) %>% 
  drop_na(Age) %>% 
  gather(-Age, key = "Region", value = "Pop") %>% 
  mutate(Age = case_when(Age >= 0 & Age < 5 ~ 0,
                         Age >= 5 & Age < 15 ~ 5,
                         Age >= 15 & Age < 35 ~ 15,
                         Age >= 35 & Age < 60 ~ 35,
                         Age >= 60 & Age < 80 ~ 60,
                         Age >= 80 ~ 80),
         Region = case_when(str_detect(Region, "Baden") ~ "Baden-Württemberg",
                            str_detect(Region, "ringen") ~ "Thüringen",
                            TRUE ~ Region)) %>% 
  group_by(Region, Age) %>% 
  summarise(Pop = sum(Pop)) %>% 
  ungroup()

pop_all <- 
  pop_reg %>% 
  group_by(Age) %>% 
  summarise(Pop = sum(Pop),
            Region = "All") %>% 
  ungroup()

pop_age <- 
  bind_rows(pop_reg, 
            pop_all) %>% 
  group_by(Region) %>% 
  mutate(Prop = Pop / sum(Pop)) %>% 
  ungroup() %>% 
  mutate(Measure = "Pop") %>% 
  select(Region, Measure, Age, Prop) %>% 
  # left_join(codes) %>% 
  mutate(Age = factor(Age, levels = lvs))

dates <- seq(ymd('2019-01-01'),ymd('2019-04-1'), by = '1 day')

ger_pop <- 
  expand_grid(Region = unique(pop_age$Region), 
              Age = unique(pop_age$Age), 
              Date = dates) %>% 
  left_join(pop_age)

ger_all <- 
  ger_age %>% 
  filter(Date >= "2020-03-01", 
         Date <= "2021-03-01") %>% 
  bind_rows(ger_pop) %>% 
  filter(Region == "All") %>% 
  mutate(Measure = factor(Measure, levels = c("Pop", "Cases", "Deaths")),
         Age_gr = recode(Age,
                         "0" = "0–4",
                         "5" = "5–14",
                         "15" = "15–34",
                         "35" = "35–59",
                         "60" = "60–79",
                         "80" = "80+"))
  

tx <- 8
# cols <- rev(c("#d9ed92", "#99d98c", "#52b69a", "#168aad", "#1e6091", "#184e77"))
cols <- c("#d9ed92", "#99d98c", "#52b69a", "#168aad", "#1e6091", "#184e77")
bks <- c(ymd("2020-03-01"), ymd("2020-08-01"), ymd("2021-01-01"))


ger_all %>% 
  ggplot()+
  geom_area(aes(Date, Prop, fill = Age_gr), stat = "identity")+
  scale_x_date(breaks = bks, date_labels = "%b")+
  scale_fill_manual(values = cols)+
  # scale_fill_viridis_d(option = "E")+
  facet_grid( ~ Measure, scales = "free_x", space = "free_x")+
  theme_bw()+
  labs(y = "Proportion",
       fill = "Age group")+
  coord_cartesian(expand = FALSE)+
  theme(strip.text = element_text(size = tx, face="bold", 
                                  margin = margin(.5, .5, .5, .5, unit = "mm")),
        strip.background = element_rect(fill = "transparent"),
        panel.spacing = unit(.9, "lines"),
        legend.text = element_text(size=tx),
        legend.title = element_text(size=tx),
        axis.title.y = element_text(size = tx),
        axis.text.y = element_text(size = tx - 1),
        axis.title.x = element_text(size = tx),
        axis.text.x = element_text(size = tx - 1))
ggsave("Figures/02_age_distribution_all_germany.png", dpi = 600, width = 4.9, height = 4.9)
ggsave("Figures/02_age_distribution_all_germany.pdf", width = 4.9, height = 4.9)
# ====


# regional age distributions
# ===========================

# redefine age population data frame, so it fits in the all states panel
dates <- seq(ymd('2019-01-01'),ymd('2019-06-1'), by = '1 day')

ger_pop <- 
  expand_grid(Region = unique(pop_age$Region), 
              Age = unique(pop_age$Age), 
              Date = dates) %>% 
  left_join(pop_age)

# merging pop and cases/deaths
lines <- 
  ger_age %>% 
  filter(Region != "All") %>% 
  select(Region) %>% 
  unique() %>% 
  rownames_to_column("ord") %>%
  mutate(ord = as.numeric(ord) - 1,
         gr = floor(ord / 4) + 1) %>% 
  select(-ord)

ger_reg <- 
  ger_age %>% 
  filter(Date >= "2020-03-01", 
         Date <= "2021-03-01") %>% 
  bind_rows(ger_pop) %>% 
  mutate(Measure = factor(Measure, levels = c("Pop", "Cases", "Deaths")),
         Age_gr = recode(Age,
                         "0" = "0–4",
                         "5" = "5–14",
                         "15" = "15–34",
                         "35" = "35–59",
                         "60" = "60–79",
                         "80" = "80+")) %>% 
  filter(Region != "All") %>% 
  left_join(lines) %>% 
  mutate(Region = recode(Region,
                         !!!to_eng_names))

tx <- 10
bks <- c(ymd("2020-04-01"), ymd("2020-08-01"), ymd("2020-12-01"))


gr1 <- 
  ger_reg %>% 
  filter(gr == 1) %>% 
  ggplot()+
  geom_area(aes(Date, Prop, fill = Age_gr), stat = "identity")+
  scale_x_date(breaks = bks, date_labels = "%b")+
  scale_fill_manual(values = cols)+
  facet_grid(Region ~ Measure, space = "free_x", scale = "free_x")+
  theme_bw()+
  labs(x = "", y = "Proportion")+
  coord_cartesian(expand = FALSE)+
  theme(strip.text = element_text(size = tx - 1, face="bold", 
                                  margin = margin(.5, .5, .5, .5, unit = "mm")),
        panel.spacing.y = unit(0.8, "lines"),
        strip.background = element_rect(fill = "transparent"),
        axis.text.y = element_text(size = tx - 1),
        axis.title.x = element_blank(),
        axis.text.x = element_text(size = tx - 2),
        legend.position = "none")

gr2 <- 
  ger_reg %>% 
  filter(gr == 2) %>% 
  ggplot()+
  geom_area(aes(Date, Prop, fill = Age_gr), stat = "identity")+
  scale_x_date(breaks = bks, date_labels = "%b")+
  scale_fill_manual(values = cols)+
  facet_grid(Region ~ Measure, space = "free_x", scale = "free_x")+
  labs(x = "")+
  theme_bw()+
  coord_cartesian(expand = FALSE)+
  theme(strip.text = element_text(size = tx - 1, face="bold", 
                                  margin = margin(.5, .5, .5, .5, unit = "mm")),
        strip.background = element_rect(fill = "transparent"),
        panel.spacing.y = unit(0.8, "lines"),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = tx - 2),
        axis.title.x = element_blank(),
        legend.position = "none")

gr3 <- 
  ger_reg %>% 
  filter(gr == 3) %>% 
  ggplot()+
  geom_area(aes(Date, Prop, fill = Age_gr), stat = "identity")+
  scale_x_date(breaks = bks, date_labels = "%b")+
  scale_fill_manual(values = cols)+
  facet_grid(Region ~ Measure, space = "free_x", scale = "free_x")+
  labs(x = "")+
  theme_bw()+
  coord_cartesian(expand = FALSE)+
  theme(strip.text = element_text(size = tx - 1, face="bold", 
                                  margin = margin(.5, .5, .5, .5, unit = "mm")),
        strip.background = element_rect(fill = "transparent"),
        panel.spacing.y = unit(0.8, "lines"),
        axis.text.y = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = tx - 2),
        legend.position = "none")

gr4 <- 
  ger_reg %>% 
  filter(gr == 4) %>% 
  ggplot()+
  geom_area(aes(Date, Prop, fill = Age_gr), stat = "identity")+
  scale_x_date(breaks = bks, date_labels = "%b")+
  scale_fill_manual(values = cols)+
  facet_grid(Region ~ Measure, space = "free_x", scale = "free_x")+
  labs(x = "")+
  theme_bw()+
  coord_cartesian(expand = FALSE)+
  theme(strip.text = element_text(size = tx - 1, face="bold", 
                                  margin = margin(.5, .5, .5, .5, unit = "mm")),
        strip.background = element_rect(fill = "transparent"),
        panel.spacing.y = unit(0.8, "lines"),
        axis.title.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = tx - 2),
        legend.position = "none")

lg <- 
  ger_reg %>% 
  filter(Date >= "2020-04-01") %>% 
  ggplot()+
  geom_area(aes(Date, Prop, fill = Age_gr), stat = "identity")+
  scale_fill_manual(values = cols)+
  facet_grid(Region ~ Measure, space = "free_x", scale = "free_x", shrink = T)+
  theme_bw()+
  labs(fill = "Age\ngroup")+
  coord_cartesian(expand = FALSE)+
  theme(strip.text = element_text(size = tx - 1, margin = margin()),
        strip.background = element_rect(fill = "transparent"),
        axis.text.y = element_text(size = tx - 2),
        axis.text.x = element_text(size = tx - 2))

legend <- get_legend(lg)

plot_grid(gr1, gr2, gr3, gr4, legend, 
          rel_widths = c(1.2, 1, 1, 1, .5), 
          rel_heights = c(1, 1, 1, 1, .2), 
          ncol = 5)

ggsave("Figures/s01_age_distribution_regions_pop.png", dpi = 600, width = 10, height = 11)
ggsave("Figures/s01_age_distribution_regions_pop.pdf", width = 10, height = 11)


