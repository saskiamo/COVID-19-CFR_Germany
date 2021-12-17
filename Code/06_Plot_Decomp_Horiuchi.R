library(tidyverse)
source(("Code/00_Functions.R"))

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Decomposition by Horiuchi et al.
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

ger_age <- read_rds("Output/germany_cases_deaths_age.rds") %>% 
  mutate(Region = case_when(str_detect(Region, "Baden") ~ "Baden-Württemberg",
                            str_detect(Region, "ringen") ~ "Thüringen",
                            TRUE ~ Region))

max(ger_age$Date)


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
                         Age >= 80 ~ 80)) %>% 
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
  bind_rows(pop_reg, pop_all)

pop_age <- pop_age %>% 
  mutate(Region = case_when(str_detect(Region, "Baden") ~ "Baden-Württemberg",
                            str_detect(Region, "ringen") ~ "Thüringen",
                            TRUE ~ Region))


# Decomposing differences across regions

regs <- unique(ger_age$Region)
db_decomps <- tibble()

r <- "Baden-Württemberg"

for(r in regs){
  
  print(r)
  r1 <- "All"
  r2 <- r
  d1 <- "2021-02-09"
  d2 <- "2021-02-09"
  
  cas1 <- 
    ger_age %>% 
    filter(Region == r1,
           Date == d1,
           Measure == "Cases") %>% 
    dplyr::pull(Value)               
  
  dea1 <- 
    ger_age %>% 
    filter(Region == r1,
           Date == d1,
           Measure == "Deaths") %>% 
    dplyr::pull(Value)               
  
  pop1 <- 
    pop_age %>% 
    filter(Region == r1) %>% 
    dplyr::pull(Pop)
  
  cas2 <- 
    ger_age %>% 
    filter(Region == r2,
           Date == d2,
           Measure == "Cases") %>% 
    dplyr::pull(Value)               
  
  dea2 <- 
    ger_age %>% 
    filter(Region == r2,
           Date == d2,
           Measure == "Deaths") %>% 
    dplyr::pull(Value)               
  
  pop2 <- 
    pop_age %>% 
    filter(Region == r2) %>% 
    dplyr::pull(Pop)
  
  cfr1 <- sum(dea1) / sum(cas1)
  cfr2 <- sum(dea2) / sum(cas2)
  
  h <- apply_horiuchi_comp(cas1, dea1, pop1, cas2, dea2, pop2)
  k <- apply_kitagawa_comp(cas1, dea1, cas2, dea2)
  
  res <- 
    tibble(R1 = r1,
           R2 = r2,
           D1 = d1,
           D2 = d2,
           CFR1 = cfr1,
           CFR2 = cfr2,
           dCFR = cfr2 - cfr1) %>% 
    bind_cols(k, h)
  
  db_decomps <- 
    db_decomps %>% 
    bind_rows(res)
  
}

test <- 
  db_decomps %>%
  mutate(diff_kh = k_dCFR - h_dCFR)

# Plot ####
db_decomps$R2 <- recode(db_decomps$R2, "All"="Germany", "Bayern"="Bavaria", "Hessen"="Hesse",
                                "Niedersachsen"="Lower Saxony", "Nordrhein-Westfalen"="North Rhine-Westphalia",
                                "Rheinland-Pfalz"="Rhineland-Palatinate", "Sachsen"="Saxony", 
                                "Sachsen-Anhalt"="Saxony-Anhalt", "Thüringen"="Thuringia", 
                                "Mecklenburg-Vorpommern"="Meckl.-Western Pomerania")
db_decomps$R1 <- recode(db_decomps$R1, "All"="Germany")

# convert to percentages
db_decomps <- db_decomps %>% mutate(CFR1=CFR1*100, CFR2=CFR2*100, dCFR=dCFR*100, h_dCFR=h_dCFR*100, 
                                    h_pop_c=h_pop_c*100, h_alpha=h_alpha*100, h_beta=h_beta*100)

decomp_to_plot <- 
  db_decomps %>% 
  select(R1, R2, CFR1, CFR2, dCFR, h_dCFR, h_pop_c, h_alpha, h_beta) %>% 
  gather(h_pop_c, h_alpha, h_beta, key = "Components", value = Value) %>% 
  mutate(P2cfr = paste0(R2, "\n(", round(CFR2, 2), ")"),
         P1cfr = paste0("Germany (", round(CFR1, 2), ") as reference"),
         Components = factor(Components, 
                             levels = c("h_pop_c", "h_alpha", "h_beta"),
                             labels = c("pop_age", "cases_age", "fatality")),
         t = "Total CFR difference") %>% 
  filter(R2 != R1)

tx <- 8

cols <- c("pop_age" = "#2FB1A2", "cases_age" = "#457b9d", "fatality" = "#e76f51")
# color blind safe
cols <- c("pop_age" = "#7570b3", "cases_age" = "#1b9e77", "fatality" = "#d95f02")

decomp_to_plot %>% 
  ggplot()+
  geom_hline(yintercept = 0, col = "black", size = 0.3, alpha = 0.5)+
  geom_bar(aes(reorder(P2cfr, -dCFR), Value, fill = Components, col = Components), stat = "identity", alpha = 0.6)+
  geom_point(aes(reorder(P2cfr, -dCFR), dCFR, shape = t), col = "black", size = 2)+
  # scale_y_continuous(limits = c(-l, l))+
  geom_hline(yintercept = 0, col = "black", size = 0.4, alpha = 0.7, linetype = "dashed")+
  scale_fill_manual(values = cols, labels = c("Population\nage structure",  "Cases\nage structure", "Fatality"))+
  scale_color_manual(values = cols, labels = c("Population\nage structure",  "Cases\nage structure", "Fatality"))+
  guides(shape = guide_legend(nrow = 1, title = "", title.position = "left", order = 1),
         fill = guide_legend(nrow = 1, title.position = "left"),
         color = guide_legend(nrow = 1, title.position = "left"))+
  labs(y = "CFR difference in percentage points")+
  theme_bw()+
  coord_flip()+
  theme(
    plot.margin = margin(.4, 0.4, 1.5, .4, "cm"),
    legend.position = c(0.3, -.15),
    legend.box = "horizontal",
    legend.title = element_text(size = tx),
    legend.text = element_text(size = tx - 1),
    legend.key.size = unit(1,"line"),
    axis.text.x = element_text(size = tx - 1),
    axis.text.y = element_text(size = tx - 1),
    axis.title.x = element_text(size = tx),
    axis.title.y = element_blank()
  )

ggsave("Figures/Publication/04_horiuchi_decomp_n.png", width = 4.9, height = 4.9, dpi = 1000)
ggsave("Figures/04_horiuchi_decomp.pdf", width = 4.9, height = 4.9)


# importance of population age structure
db_proportions <- 
  db_decomps %>% 
  mutate(cont_abs = abs(h_pop_c) + abs(h_alpha) + abs(h_beta),
         prop_pop = abs(h_pop_c) / cont_abs,
         prop_cases = abs(h_alpha) / cont_abs,
         prop_fatal = abs(h_beta) / cont_abs) %>% 
  select(R2, prop_pop, prop_cases, prop_fatal) %>% 
  gather(-R2, key = Component, value = Value) %>%
  mutate(Component = factor(Component, 
                             levels = c("prop_pop", "prop_cases", "prop_fatal"),
                             labels = c("prop_pop", "prop_cases", "prop_fatal")))

# db_proportions %>%
#   ggplot()+
#   geom_bar(aes(R2, Value, fill = Component, col = Component), stat = "identity", alpha = 0.5)+
#   theme_bw()+
#   coord_flip()+
#   theme(
#     legend.position="bottom",
#     legend.title = element_text(size = tx),
#     legend.text = element_text(size = tx - 1),
#     legend.key.size = unit(0.5,"line"),
#     strip.background = element_rect(fill="transparent"),
#     strip.text.y = element_text(size = tx + 1),
#     axis.text.x = element_text(size = tx),
#     axis.text.y = element_text(size = tx),
#     axis.title.x = element_text(size = tx + 1),
#     axis.title.y = element_text(size = tx + 1)
#   )
# ggsave("Figures/proportions_components.png")

  


# =================================================================== #
# Test with in the same region in two different times
# same population age structure, so component 1 should be equal to 0
# =================================================================== #

db_res <- tibble()

r1 <- "All"
r2 <- "All"
d1 <- "2020-11-01"
d2 <- "2021-01-20"
print(r2)

cas1 <- 
  ger_age %>% 
  filter(Region == r1,
         Date == d1,
         Measure == "Cases") %>% 
  dplyr::pull(Value)

dea1 <- 
  ger_age %>% 
  filter(Region == r1,
         Date == d1,
         Measure == "Deaths") %>% 
  dplyr::pull(Value)

pop1 <- 
  pop_age %>% 
  filter(Region == r1) %>% 
  dplyr::pull(Pop)

cas2 <- 
  ger_age %>% 
  filter(Region == r2,
         Date == d2,
         Measure == "Cases") %>% 
  dplyr::pull(Value)

dea2 <- 
  ger_age %>% 
  filter(Region == r2,
         Date == d2,
         Measure == "Deaths") %>% 
  dplyr::pull(Value)

pop2 <- 
  pop_age %>% 
  filter(Region == r2) %>% 
  dplyr::pull(Pop)

cfr1 <- sum(dea1) / sum(cas1)
cfr2 <- sum(dea2) / sum(cas2)

h <- apply_horiuchi_comp(cas1, dea1, pop1, cas2, dea2, pop2)
k <- apply_kitagawa_comp(cas1, dea1, cas2, dea2)

res <- 
  tibble(R1 = r1,
         R2 = r2,
         D1 = d1,
         D2 = d2,
         CFR1 = cfr1,
         CFR2 = cfr2,
         dCFR = cfr2 - cfr1) %>% 
  bind_cols(k, h)

res

db_res <- 
  db_res %>% 
  bind_rows(res)
