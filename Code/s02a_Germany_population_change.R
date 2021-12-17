library(tidyverse)
library(lubridate)
library(osfr)
library(cowplot)
library(here)

source("Code/00_Functions.R")

# population data by state
# ~~~~~~~~~~~~~~~~~~~~~~~~
pop <- 
  read_csv2("Data/12411-0012.csv",
            skip = 5,
            locale = locale(encoding = stringi::stri_enc_get()))

pop_reg <- 
  pop %>% 
  rename(Date = 1,
         Age = 2) %>% 
  mutate(Age = str_sub(Age, 1, 2),
         Age = str_remove(Age, "-"),
         Age = case_when(Age == "un" ~ "0", 
                         Age == "To" ~ "TOT",
                         TRUE ~ Age)) %>% 
  filter(Age != "TOT") %>% 
  gather(-Date, -Age, key = "Region", value = "Pop") %>% 
  mutate(Age = Age %>% as.integer(),
         Age = case_when(Age >= 0 & Age < 5 ~ 0,
                         Age >= 5 & Age < 15 ~ 5,
                         Age >= 15 & Age < 35 ~ 15,
                         Age >= 35 & Age < 60 ~ 35,
                         Age >= 60 & Age < 80 ~ 60,
                         Age >= 80 ~ 80)) %>% 
  group_by(Date, Region, Age) %>% 
  summarise(Pop = sum(Pop)) %>% 
  ungroup()


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Daily population interpolation
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

ages <- unique(pop_reg$Age)
regs <- unique(pop_reg$Region)
dates <- seq(ymd('2012-12-31'), ymd('2021-12-31'), by='days')
n_dates <- length(dates)

db_daily <- 
  expand_grid(Date = dates, Age = ages, Region = regs) %>% 
  left_join(pop_reg) %>% 
  select(Date, Region, Age, Pop) %>% 
  arrange(Region, Age, Date) %>% 
  group_by(Region, Age) %>% 
  mutate(t = 1:n())


inters_pop <- NULL
r <- "Mecklenburg-Vorpommern"
a <- 60
for(r in regs){
  pop_temp1 <- db_daily %>% 
    filter(Region == r)
  for(a in ages){
    
    pop_temp2 <- pop_temp1 %>% 
      filter(Age == a)

    pop_temp3 <- pop_temp2 %>% 
      left_join(interpop(pop_temp2)) %>% 
      mutate(Region = r,
             Age = a)
    
    inters_pop <- inters_pop %>% 
      bind_rows(pop_temp3)
    
  }
}

# Visual test
r <- "Mecklenburg-Vorpommern"
a <- 0

inters_pop %>% 
  filter(Region == r,
         Age == a) %>% 
  ggplot()+
  geom_line(aes(t, Pop2), col = "black")+
  geom_point(aes(t, Pop), col = "red")

inters_pop2 <- 
  inters_pop %>% 
  select(-Pop) %>% 
  rename(Pop = Pop2) %>% 
  arrange(Region, Age, t)

write_rds(inters_pop2, here("Output", "pop_interpol_days__by_age.rds"))
unique(inters_pop2$Region) %>% sort()


# Excess and all deaths 
# ~~~~~~~~~~~~~~~~~~~~~

exc <- 
  read_csv("Data/cumulative_excess_age_2020_2021.csv") %>% 
  filter(Country == "Germany",
         Sex == "b") %>% 
  filter(Date == "2021-03-07") %>% 
  mutate(Age = case_when(Age < 5 ~ 0,
                         Age >= 5 & Age <= 14 ~ 5,
                         Age >= 15 & Age <= 34 ~ 15,
                         Age >= 35 & Age <= 59 ~ 35,
                         Age >= 60 & Age <= 79 ~ 60,
                         Age >= 80 ~ 80)) %>%
  group_by(Age) %>% 
  summarise(all_excess = sum(CumEpi)) %>% 
  ungroup()


ger <- read_rds("Output/germany_cases_deaths_age.rds") %>% 
  mutate(Region = case_when(str_detect(Region, "Baden") ~ "Baden-Württemberg",
                            str_detect(Region, "ringen") ~ "Thüringen",
                            TRUE ~ Region))
# confirmed covid deaths by state and age
ger_age <- 
  ger %>%
  filter(Measure == "Deaths") %>% 
  mutate(Age = as.integer(Age)) %>% 
  # aggregate both sexes
  group_by(Country, Region, Short, Date, Age) %>% 
  summarise(Deaths = sum(Value)) %>% 
  ungroup() %>% 
  select(Region, Short, Date, Age, Deaths)

# distributing excess according to the share of confirmed deaths in each state,
# by age
ger_exc_age_state <- 
  ger_age %>% 
  filter(Date == "2021-03-07",
         Region != "All") %>%
  group_by(Age) %>% 
  mutate(Prop_age = Deaths / sum(Deaths)) %>% 
  ungroup() %>% 
  left_join(exc) %>% 
  mutate(Excess = all_excess * Prop_age) %>% 
  select(Region, Age, Excess)

ger_exc_age_state_time <- 
  ger_age %>% 
  filter(Region != "All",
         Date <= "2021-03-07") %>% 
  group_by(Region, Age) %>% 
  mutate(new = Deaths - lag(Deaths),
         prop_new = new / sum(new, na.rm = T),
         prop_new = ifelse(is.na(prop_new), 0, prop_new)) %>% 
  ungroup() %>% 
  left_join(ger_exc_age_state) %>% 
  mutate(new_excess = Excess * prop_new,
         excess_deaths_adj = ifelse(Age < 35, 0, new_excess)) %>% 
  select(Region, Date, Age, excess_deaths_adj)


# adjusting population based on excess deaths
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
pop_adjusted <- 
  inters_pop2 %>% 
  left_join(ger_exc_age_state_time) %>% 
  replace_na(list(excess_deaths_adj = 0)) %>% 
  mutate(Pop_adj = Pop - excess_deaths_adj)


# Visual test
r <- "Thüringen"
a <- 80

pop_adjusted %>%
  filter(Date >= "2019-01-01" & Date <= "2021-05-31",
    Region == r,
         Age == a) %>% 
  ggplot()+
  geom_line(aes(Date, Pop), col = "black")+
  geom_line(aes(Date, Pop_adj), col = "blue")

min(ger$Date)
max(ger$Date)


pop_out <- 
  pop_adjusted %>% 
  filter(Date >= min(ger$Date) & Date <= max(ger$Date)) %>% 
  select(Region, Date, Age, Pop_int = Pop, Pop_int_adj = Pop_adj)

write_rds(pop_out, "Output/pop_germany_state_age_over_time.rds")
