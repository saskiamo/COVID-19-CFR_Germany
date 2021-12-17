library(tidyverse); library(ggpubr)

# Plot of decompositions ################################################

dec_ot1 <- read_csv("Output/decomp_overtime1.csv")
dec_ot2 <- read_csv("Output/decomp_overtime2.csv")

# adjusting names for CFRs and 
d1 <- 
  dec_ot1 %>% 
  mutate(Diff=Diff*100, 
         AgeComp=AgeComp*100, 
         RateComp=RateComp*100,
         CFR1=CFR1*100, 
         CFR2=CFR2*100,
         Per = 1) 

d2 <- 
  dec_ot2 %>% 
  mutate(Diff=Diff*100, 
         AgeComp=AgeComp*100, 
         RateComp=RateComp*100,
         CFR1=CFR1*100, 
         CFR2=CFR2*100,
         Per = 2)

# labels including region, dates, and CFRs
regs_labs <- 
  d1 %>% 
  select(Region, cfr1 = CFR2, cfr2 = CFR1, Date, Date2) %>% 
  left_join(d2 %>% 
              select(Region, cfr2 = CFR2, cfr3 = CFR1, Date2 = Date, Date3)) %>% 
  mutate(reg_lab = paste0(Region, "\n(", 
                          format(Date, "%d %b"), ", ", round(cfr1, 1), "; ",
                          format(Date2, "%d %b"), ", ", round(cfr2, 1), "; ", 
                          format(Date3, "%d %b"), ", ", round(cfr3, 1),")")) %>% 
  select(Region, reg_lab)

# both decompositions in one object
d <- 
  d1 %>% 
    select(Region, Diff, AgeComp, RateComp, Per) %>% 
    gather(AgeComp, RateComp, key = "Component", value = Value) %>% 
  bind_rows(d2 %>% 
              select(Region, Diff, AgeComp, RateComp, Per) %>% 
              gather(AgeComp, RateComp, key = "Component", value = Value)) %>% 
  left_join(regs_labs) %>% 
  mutate(t = "Total CFR difference")



cols <- c("#1b9e77", "#d95f02")
tx <- 8

d %>% 
  ggplot()+
  facet_grid(~ Per, scales = "free_x", space = "free_x")+
  geom_bar(aes(Value, forcats::fct_rev(reorder(reg_lab, reg_lab)), fill = Component, col = Component), 
           stat = "identity", alpha = 0.6)+
  geom_point(aes(Diff, reg_lab, shape = t), col = "black", size = 2)+
  geom_vline(xintercept = 0, col = "black", size = 0.4, alpha = 0.7, linetype = "dashed")+
  scale_color_manual(values = cols, labels = c("Conf. infection rate\nage structure", "Fatality"))+
  scale_fill_manual(values = cols, labels = c("Conf. infection rate\nage structure", "Fatality"))+
  guides(shape = guide_legend(nrow = 1, title = "", title.position = "left", order = 1),
         fill = guide_legend(nrow = 1, title.position = "left"),
         color = guide_legend(nrow = 1, title.position = "left"))+
  labs(x = "CFR difference in percentage points")+
  theme_bw()+
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
    axis.title.y = element_blank(),
    strip.text = element_blank()
  )

ggsave("Figures/05_Plot_Dec_BothOverTime.png", width = 4.9, height = 4.9, dpi = 600)
ggsave("Figures/05_Plot_Dec_BothOverTime.pdf", width = 4.9, height = 4.9)

