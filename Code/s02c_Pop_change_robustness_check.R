source("Code/00_Functions.R")

# loading population data
# ~~~~~~~~~~~~~~~~~~~~~~~
pop_age <- 
  read_rds("Output/pop_germany_state_age.rds")

pop_age_time <- 
  read_rds("Output/pop_germany_state_age_over_time.rds")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Loading COVerAGE-DB data input
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

ger_age <- read_rds("Output/germany_cases_deaths_age.rds") %>% 
  mutate(Region = case_when(str_detect(Region, "Baden") ~ "Baden-Württemberg",
                            str_detect(Region, "ringen") ~ "Thüringen",
                            TRUE ~ Region))

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


ger_age2 <- 
  ger_age %>% 
  select(Region, Date, Age, Measure, Value) %>% 
  spread(Measure, Value) %>% 
  drop_na() %>% 
  filter(Region != "All")


# total cfrs
# ~~~~~~~~~~

all_cfrs <- 
  ger_age2 %>% 
  group_by(Region, Date) %>% 
  summarise(Cases = sum(Cases),
            Deaths = sum(Deaths)) %>% 
  ungroup() %>% 
  mutate(CFR = Deaths / Cases)

# Date 1: Maximum CFR 
d1 <- 
  all_cfrs %>% 
  filter(Date >= "2020-03-15" & Date <= "2020-08-30") %>% 
  group_by(Region) %>% 
  filter(CFR == max(CFR)) %>% 
  filter(Date == min(Date)) %>% 
  mutate(cut = 1) %>% 
  select(Region, Date, cut)


# Date 2: Minimum CRF
d2 <- 
  all_cfrs %>% 
  filter(Date >= "2020-08-30" & Date <= "2021-02-15") %>% 
  group_by(Region) %>% 
  filter(CFR == min(CFR)) %>% 
  filter(Date == min(Date)) %>% 
  mutate(cut = 2) %>% 
  select(Region, Date, cut)

# Date 3: Maximum CFR 
d3 <- 
  all_cfrs %>% 
  filter(Date >= "2020-12-01") %>% 
  group_by(Region) %>% 
  filter(CFR == max(CFR)) %>% 
  filter(Date == min(Date)) %>% 
  mutate(cut = 3) %>% 
  select(Region, Date, cut)


cfrs_cuts <- 
  bind_rows(d1, d2, d3) 

ger_cfr_cuts <- 
  ger_age2 %>% 
  left_join(cfrs_cuts) %>% 
  filter(!is.na(cut)) %>% 
  left_join(pop_age) %>% 
  left_join(pop_age_time)

# Decomposing differences over time
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

give_me <- function(v = "Deaths", r = "Berlin", c = 1){
  chunk <- 
    ger_cfr_cuts %>% 
    filter(Region == r,
           cut == c) %>% 
    dplyr::pull(get(v))               
}

regs <- unique(ger_age2$Region)
db_decomps <- tibble()

r <- "Baden-Württemberg"

for(r in regs){
  
  print(r)

  # cut 1
  cas1 <- give_me("Cases", r, 1)
  dea1 <- give_me("Deaths", r, 1)
  pop1 <- give_me("Pop", r, 1)
  pop_ad1 <- give_me("Pop_int_adj", r, 1)
  
  # cut 2 
  cas2 <- give_me("Cases", r, 2)
  dea2 <- give_me("Deaths", r, 2)
  pop2 <- give_me("Pop", r, 2)
  pop_ad2 <- give_me("Pop_int_adj", r, 2)
  
  # cut 3 
  cas3 <- give_me("Cases", r, 3)
  dea3 <- give_me("Deaths", r, 3)
  pop3 <- give_me("Pop", r, 3)
  pop_ad3 <- give_me("Pop_int_adj", r, 3)
  
  cfr1 <- sum(dea1) / sum(cas1)
  cfr2 <- sum(dea2) / sum(cas2)
  cfr3 <- sum(dea3) / sum(cas3)

  h_1_2 <- 
    apply_horiuchi_comp(cas1, dea1, pop1, cas2, dea2, pop2) %>% 
    mutate(CFR_ini = cfr1,
           CFR_end = cfr2,
           cut = "1_2",
           Pop = "Constant",
           Date_ini = give_me("Date", r, 1) %>% unique(),
           Date_end = give_me("Date", r, 2) %>% unique())
  h_ad_1_2 <- 
    apply_horiuchi_comp(cas1, dea1, pop_ad1, cas2, dea2, pop_ad2) %>% 
    mutate(CFR_ini = cfr1,
           CFR_end = cfr2,
           cut = "1_2",
           Pop = "Adjusted",
           Date_ini = give_me("Date", r, 1) %>% unique(),
           Date_end = give_me("Date", r, 2) %>% unique())
  
  h_2_3 <-
    apply_horiuchi_comp(cas2, dea2, pop2, cas3, dea3, pop3) %>% 
    mutate(CFR_ini = cfr2,
           CFR_end = cfr3,
           cut = "2_3",
           Pop = "Constant",
           Date_ini = give_me("Date", r, 2) %>% unique(),
           Date_end = give_me("Date", r, 3) %>% unique())
  
  h_ad_2_3 <- 
    apply_horiuchi_comp(cas2, dea2, pop_ad2, cas3, dea3, pop_ad3) %>% 
    mutate(CFR_ini = cfr2,
           CFR_end = cfr3,
           cut = "2_3",
           Pop = "Adjusted",
           Date_ini = give_me("Date", r, 2) %>% unique(),
           Date_end = give_me("Date", r, 3) %>% unique())
  
  db_decomps <- 
    bind_rows(h_1_2,
              h_ad_1_2,
              h_2_3,
              h_ad_2_3) %>% 
    mutate(Region = r) %>% 
    bind_rows(db_decomps)
  
}

db_decomps2 <- 
  db_decomps %>% 
  mutate(prop_pop = h_pop_c / h_dCFR)

db_decomps_adj <- 
  db_decomps2 %>% 
  filter(Pop == "Adjusted")

min(abs(db_decomps_adj$prop_pop))
max(abs(db_decomps_adj$prop_pop))
mean(abs(db_decomps_adj$prop_pop))


decomp_to_plot <- 
  db_decomps %>% 
  select(Region, CFR_ini, CFR_end, dCFR = h_dCFR, h_pop_c, h_alpha, h_beta, Pop, cut) %>% 
  gather(h_pop_c, h_alpha, h_beta, key = "Components", value = Value) %>% 
  mutate(Components = factor(Components, 
                             levels = c("h_pop_c", "h_alpha", "h_beta"),
                             labels = c("pop_age", "incidence", "fatality")),
         t = "Total CFR\ndifference",
         Pop = ifelse(Pop == "Constant", "Constant population", "Changing population"),
         Pop = factor(Pop, levels = c("Constant population", "Changing population")),
         cut = ifelse(cut == "1_2", "CFR Decrease", "CFR Increase"),
         Region = recode(Region,
                         !!!to_eng_names))

tx <- 8

# color blind safe
cols <- c("pop_age" = "#7570b3", "incidence" = "#1b9e77", "fatality" = "#d95f02")


# Importing the CFR Horiuchi decomposition over time estimates
# with unique time span for all states  
dec_same_per_1 <- read_csv("Output/decomp_overtime1_sametime.csv")
dec_same_per_2 <- read_csv("Output/decomp_overtime2_sametime.csv")

dec_same_per_1_2 <- 
  dec_same_per_1 %>% 
  select(-Date, -Date2) %>% 
  mutate(cut = "CFR Decrease") %>% 
  rename(dCFR = Diff,
         incidence = AgeComp, 	
         fatality = RateComp,
         CFR_ini = CFR2,
         CFR_end = CFR1) %>% 
  gather(incidence, fatality, key = Components, value = Value)

dec_same_per_2_2 <- 
  dec_same_per_2 %>% 
  select(-Date, -Date3) %>% 
  mutate(cut = "CFR Increase") %>% 
  rename(dCFR = Diff,
         incidence = AgeComp, 	
         fatality = RateComp,
         CFR_ini = CFR2,
         CFR_end = CFR1) %>% 
  gather(incidence, fatality, key = Components, value = Value)

dec_same_per <- 
  bind_rows(dec_same_per_1_2,
            dec_same_per_2_2) %>% 
  mutate(Components = factor(Components, levels = c("incidence", "fatality")),
         t = "Total CFR\ndifference",
         Periods = "Unique time window")


decomp_to_plot2 <- 
  decomp_to_plot %>% 
  rename(type = Pop) %>% 
  mutate(type = ifelse(type == "Constant population", 
                      "Constant population,\nDifferent time windows", 
                      "Changing population")) %>% 
  bind_rows(dec_same_per %>% 
              rename(type = Periods)) %>% 
  mutate(type = factor(type, 
                       levels = c("Constant population,\nDifferent time windows",
                                  "Changing population",
                                  "Unique time window")))
  
decomp_to_plot2 %>% 
  ggplot()+
  facet_grid(type ~ cut, scales = "free_x", space = "free_x")+
  geom_vline(xintercept = 0, col = "black", size = 0.3, alpha = 0.5)+
  geom_bar(aes(Value, forcats::fct_rev(reorder(Region, Region)), fill = Components, col = Components), stat = "identity", alpha = 0.6)+
  geom_point(aes(dCFR, forcats::fct_rev(reorder(Region, Region)), shape = t), col = "black", size = 1.5)+
  # scale_y_continuous(limits = c(-l, l))+
  geom_vline(xintercept = 0, col = "black", size = 0.4, alpha = 0.7, linetype = "dashed")+
  scale_fill_manual(values = cols, labels = c("Population\nage structure",  "Cumulative\nincidence", "Fatality"))+
  scale_color_manual(values = cols, labels = c("Population\nage structure",  "Cumulative\nincidence", "Fatality"))+
  scale_x_continuous(breaks = seq(-0.04, 0.03, 0.01))+
  guides(shape = guide_legend(nrow = 1, title = "", title.position = "left", order = 1),
         fill = guide_legend(nrow = 1, title.position = "left"),
         color = guide_legend(nrow = 1, title.position = "left"))+
  labs(y = "CFR difference in percentage points")+
  theme_bw()+
  theme(
    plot.margin = margin(.4, .4, .5, .4, "cm"),
    legend.margin = margin(.1, .1, .1, .1, "cm"),
    legend.position = "bottom",
    legend.box = "horizontal",
    legend.title = element_text(size = tx),
    legend.text = element_text(size = tx - 1),
    legend.key.size = unit(0.6,"line"),
    strip.background = element_rect(fill = "transparent"),
    strip.text = element_text(size = tx - 1),
    axis.text.x = element_text(size = tx - 1),
    axis.text.y = element_text(size = tx - 1),
    axis.title.x = element_text(size = tx),
    axis.title.y = element_blank()
  )

ggsave("Figures/s02_Plot_Dec_BothOverTime.png", width = 5.5, height = 6, dpi = 600)
ggsave("Figures/s02_Plot_Dec_BothOverTime.pdf", width = 5.5, height = 6)


