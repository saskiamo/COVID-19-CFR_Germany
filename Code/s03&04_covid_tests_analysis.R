source("Code/00_Functions.R")

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


pop <- 
  read_rds("Output/pop_germany_state_age.rds") %>% 
  group_by(Region) %>% 
  summarise(Pop = sum(Pop)) %>% 
  ungroup()

covid <- read_rds("Output/germany_cases_deaths_age.rds") %>% 
  mutate(Region = case_when(str_detect(Region, "Baden") ~ "Baden-Württemberg",
                            str_detect(Region, "ringen") ~ "Thüringen",
                            TRUE ~ Region)) %>% 
  group_by(Region, Date, Measure) %>% 
  summarise(Value = sum(Value)) %>% 
  ungroup() %>% 
  mutate(week_day = date2ISOweek(Date),
         day = str_sub(week_day, 10),
         Year = year(Date),
         Week = str_sub(week_day, 7, 8) %>% as.numeric()) %>% 
  filter(day == "7") %>% 
  select(Region, Year, Week, Date, Measure, Value)

tests <- read_xlsx("Data/data_wochenbericht.xlsx",
                   sheet = "Abb. 3 Bundesland",
                   skip = 4) %>% 
  rename(Region =1,
         Year = 2, 
         Week = 3,
         new_test = 4,
         Pos = 5) %>% 
  group_by(Region) %>% 
  mutate(Tests = cumsum(new_test)) %>% 
  ungroup()

db_all <- 
  covid %>% 
  left_join(tests) %>% 
  left_join(pop) %>% 
  spread(Measure, Value) %>% 
  mutate(CFR = Deaths / Cases,
         CumPos = Cases / Tests,
         Incidence = Cases / Pop,
         TestPop = Tests / Pop,
         Region = recode(Region,
                         !!!to_eng_names)) %>% 
  drop_na() %>% 
  select(Region, Date, Year, Week, Cases, Deaths, CumPos, CFR, Pop, Incidence, TestPop)


# Date 1: Maximum CFR 
d1 <- 
  db_all %>% 
  filter(Date >= "2020-03-15" & Date <= "2020-08-30") %>% 
  group_by(Region) %>% 
  filter(CFR == max(CFR)) %>% 
  filter(Date == min(Date)) %>% 
  mutate(cut = "Period 1")

# Date 2: Minimum CRF
d2 <- 
  db_all %>% 
  filter(Date >= "2020-08-30" & Date <= "2021-02-15") %>% 
  group_by(Region) %>% 
  filter(CFR == min(CFR)) %>% 
  filter(Date == min(Date)) %>% 
  mutate(cut = "Period 2")

# Date 3: Maximum CFR 
d3 <- 
  db_all %>% 
  filter(Date >= "2020-12-01") %>% 
  group_by(Region) %>% 
  filter(CFR == max(CFR)) %>% 
  filter(Date == min(Date)) %>% 
  mutate(cut = "Period 3")

db_cuts <- 
  bind_rows(d1, d2, d3)
  
db_cuts %>% 
  ggplot()+
  geom_point(aes(Incidence, CumPos))+
  facet_wrap(~cut, scales = "free")

# Correlation between Testing coverage and CFR
corrs <- tibble(cut = paste0("Period ", 1:3), 
             corr = c(cor(d1$TestPop, d1$CFR, method = c("spearman")), 
                      cor(d2$TestPop, d2$CFR, method = c("spearman")), 
                      cor(d3$TestPop, d3$CFR, method = c("spearman"))))
# Testing coverage and CFR
db_cuts %>% 
  ggplot()+
  geom_point(aes(TestPop, CFR), alpha = 0.6)+
  geom_text(data = corrs, aes(Inf, Inf, label = round(corr, 3)),
            hjust = 1.1,
            vjust = 1.3,
            size = 3)+
  geom_text(data = corrs, aes(Inf, Inf, label = "rho = "), 
            hjust = 2.3,
            vjust = 1.3,
            size = 3)+
  facet_wrap(~cut, scales = "free")+
  labs(x = "Testing coverage")+
  theme_bw()+
  theme(    strip.background = element_rect(fill = "transparent"))

ggsave("Figures/s03_Plot_corr_testing_cfr.png", width = 6, height = 3, dpi = 600)
ggsave("Figures/s03_Plot_corr_testing_cfr.pdf", width = 6, height = 3)




db_all %>% 
  filter(Incidence >= 0.004) %>% 
  ggplot()+
  geom_line(aes(Incidence, CumPos, col = Region))

db_all %>% 
  filter(Incidence >= 0.004) %>% 
  ggplot()+
  geom_line(aes(Incidence, TestPop, col = Region))

db_all %>% 
  filter(Incidence >= 0.004) %>% 
  ggplot()+
  geom_line(aes(TestPop, CumPos, col = Region))

db_all %>% 
  filter(Incidence >= 0.004) %>% 
  ggplot()+
  geom_line(aes(TestPop, CFR, col = Region))

db_all2 <- 
  db_all %>% 
  filter(Date >= "2020-06-15")


cor(db_all2$TestPop, db_all2$CFR, method = c("spearman"))

# Testing coverage and CFR

tx <- 9
db_all %>% 
  filter(Date >= "2020-06-15") %>% 
  ggplot()+
  geom_point(aes(TestPop, CFR, col = Region), alpha = 0.4)+
  geom_point(data = db_all %>%
               filter(Date == max(Date)), aes(TestPop, CFR), 
             col = "black", 
             shape = 1,
             alpha = 0.8)+
  # geom_point(data = db_all %>% 
  #              filter(Date == max(Date)), aes(TestPop, CFR, col = Region), alpha = 0.9)+
  labs(x = "Testing coverage")+
  theme_bw()+
  theme(
    # plot.margin = margin(.4, 0.4, 1.5, .4, "cm"),
    # legend.position = c(0.3, -.15),
    # legend.box = "horizontal",
    legend.title = element_blank(),
    legend.text = element_text(size = tx - 1),
    legend.key.size = unit(0.5, "line"),
    axis.text.x = element_text(size = tx - 1),
    axis.text.y = element_text(size = tx - 1),
    axis.title.x = element_text(size = tx),
    axis.title.y = element_text(size = tx)
  )

ggsave("Figures/s04_Plot_corr_testing_cfr_color.png", dpi = 600,
       height = 4, width = 8)
ggsave("Figures/s04_Plot_corr_testing_cfr_color.pdf",
       height = 4, width = 8)
