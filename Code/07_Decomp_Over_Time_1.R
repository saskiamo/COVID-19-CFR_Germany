library(tidyverse); library(dplyr); library(data.table); library(lubridate)
load("Output/analysis.RData")

#### Decomposition over time within German federal states:     ########################
# CFR at peak and CFR at minimum (Ref) 
# CFR-peaks since 15.03.2020 (first wave without outlier from the beginning)
max <- total %>%
  group_by(Region) %>%
  filter( Date >= dmy("15.03.2020"), Date <= dmy("01.06.2020")) %>%
  filter(CFR==max(CFR),
         Region!="Germany") %>%
  select(Region, Date, CFR)

range(max$Date)

# CFR-minimums since peak (set 11.05.2020) in autumn 
min <- total %>% 
  group_by(Region) %>%
  filter(Date >= dmy("11.05.2020"),
         Date < dmy("10.12.2020")) %>%
  filter(CFR==min(CFR),
         Region!="Germany") %>%
  select(Region, Date, CFR)

range(min$Date)

table(total$Region)

# Note: CFR1 = Lowest point; CFR2 = Peak
## Baden-Württemberg ####
D_BW <- as.data.table(db)[
  ,kitagawa_cfr( db$Cases[db$Region=="Baden-Württemberg" & db$Date==min$Date[min$Region=="Baden-Württemberg"]],
                 db$ascfr[db$Region=="Baden-Württemberg" & db$Date==min$Date[min$Region=="Baden-Württemberg"]],
                 Cases,ascfr),
  by=list(Date, Region)]

# Select date & state
D_BW <- D_BW %>% filter(Date==max$Date[max$Region=="Baden-Württemberg"] & Region=="Baden-Württemberg") 

# Calculate relative contributions
D_BW2 <- D_BW %>% mutate(relAgeDE = abs(AgeComp)/(abs(AgeComp)+abs(RateComp)))
D_BW2 <- D_BW2 %>% mutate(relRateDE = abs(RateComp)/(abs(AgeComp)+abs(RateComp)))

## Bavaria ####
D_BY <- as.data.table(db)[
  ,kitagawa_cfr(db$Cases[db$Region=="Bavaria" & db$Date==min$Date[min$Region=="Bavaria"]],
                db$ascfr[db$Region=="Bavaria" & db$Date==min$Date[min$Region=="Bavaria"]],
                Cases,ascfr),
  by=list(Date, Region)]

# Select date & state
D_BY <- D_BY %>% filter(Date==max$Date[max$Region=="Bavaria"] & Region=="Bavaria") 

# Calculate relative contributions
D_BY2 <- D_BY %>% mutate(relAgeDE = abs(AgeComp)/(abs(AgeComp)+abs(RateComp)))
D_BY2 <- D_BY2 %>% mutate(relRateDE = abs(RateComp)/(abs(AgeComp)+abs(RateComp)))

## Berlin ####
D_BER <- as.data.table(db)[
  ,kitagawa_cfr(db$Cases[db$Region=="Berlin" & db$Date==min$Date[min$Region=="Berlin"]],
                db$ascfr[db$Region=="Berlin" & db$Date==min$Date[min$Region=="Berlin"]],
                Cases,ascfr),
  by=list(Date, Region)]

# Select date & state
D_BER <- D_BER %>% filter(Date==max$Date[max$Region=="Berlin"] & Region=="Berlin") 

# Calculate relative contributions
D_BER2 <- D_BER %>% mutate(relAgeDE = abs(AgeComp)/(abs(AgeComp)+abs(RateComp)))
D_BER2 <- D_BER2 %>% mutate(relRateDE = abs(RateComp)/(abs(AgeComp)+abs(RateComp)))

## Brandenburg ####
D_BRA <- as.data.table(db)[
  ,kitagawa_cfr(db$Cases[db$Region=="Brandenburg" & db$Date==min$Date[min$Region=="Brandenburg"]],
                db$ascfr[db$Region=="Brandenburg" & db$Date==min$Date[min$Region=="Brandenburg"]],
                Cases,ascfr),
  by=list(Date, Region)]

# Select date & state
D_BRA <- D_BRA %>% filter(Date==max$Date[max$Region=="Brandenburg"] & Region=="Brandenburg") 

# Calculate relative contributions
D_BRA2 <- D_BRA %>% mutate(relAgeDE = abs(AgeComp)/(abs(AgeComp)+abs(RateComp)))
D_BRA2 <- D_BRA2 %>% mutate(relRateDE = abs(RateComp)/(abs(AgeComp)+abs(RateComp)))

## Bremen ####
D_HB <- as.data.table(db)[
  ,kitagawa_cfr(db$Cases[db$Region=="Bremen" & db$Date==min$Date[min$Region=="Bremen"]],
                db$ascfr[db$Region=="Bremen" & db$Date==min$Date[min$Region=="Bremen"]],
                Cases,ascfr),
  by=list(Date, Region)]

# Select date & state
D_HB <- D_HB %>% filter(Date==max$Date[max$Region=="Bremen"] & Region=="Bremen") 

# Calculate relative contributions
D_HB2 <- D_HB %>% mutate(relAgeDE = abs(AgeComp)/(abs(AgeComp)+abs(RateComp)))
D_HB2 <- D_HB2 %>% mutate(relRateDE = abs(RateComp)/(abs(AgeComp)+abs(RateComp)))

## Hamburg ####
D_HH <- as.data.table(db)[
  ,kitagawa_cfr(db$Cases[db$Region=="Hamburg" & db$Date==min$Date[min$Region=="Hamburg"]],
                db$ascfr[db$Region=="Hamburg" & db$Date==min$Date[min$Region=="Hamburg"]],
                Cases,ascfr),
  by=list(Date, Region)]

# Select date & state
D_HH <- D_HH %>% filter(Date==max$Date[max$Region=="Hamburg"] & Region=="Hamburg") 

# Calculate relative contributions
D_HH2 <- D_HH %>% mutate(relAgeDE = abs(AgeComp)/(abs(AgeComp)+abs(RateComp)))
D_HH2 <- D_HH2 %>% mutate(relRateDE = abs(RateComp)/(abs(AgeComp)+abs(RateComp)))

## Hesse ####
D_HES <- as.data.table(db)[
  ,kitagawa_cfr(db$Cases[db$Region=="Hesse" & db$Date==min$Date[min$Region=="Hesse"]],
                db$ascfr[db$Region=="Hesse" & db$Date==min$Date[min$Region=="Hesse"]],
                Cases,ascfr),
  by=list(Date, Region)]

# Select date & state
D_HES <- D_HES %>% filter(Date==max$Date[max$Region=="Hesse"] & Region=="Hesse") 

# Calculate relative contributions
D_HES2 <- D_HES %>% mutate(relAgeDE = abs(AgeComp)/(abs(AgeComp)+abs(RateComp)))
D_HES2 <- D_HES2 %>% mutate(relRateDE = abs(RateComp)/(abs(AgeComp)+abs(RateComp)))

## Lower Saxony ####
D_LS <- as.data.table(db)[
  ,kitagawa_cfr(db$Cases[db$Region=="Lower Saxony" & db$Date==min$Date[min$Region=="Lower Saxony"]],
                db$ascfr[db$Region=="Lower Saxony" & db$Date==min$Date[min$Region=="Lower Saxony"]],
                Cases,ascfr),
  by=list(Date, Region)]

# Select date & state
D_LS <- D_LS %>% filter(Date==max$Date[max$Region=="Lower Saxony"] & Region=="Lower Saxony") 

# Calculate relative contributions
D_LS2 <- D_LS %>% mutate(relAgeDE = abs(AgeComp)/(abs(AgeComp)+abs(RateComp)))
D_LS2 <- D_LS2 %>% mutate(relRateDE = abs(RateComp)/(abs(AgeComp)+abs(RateComp)))

## Mecklenburg-Western Pomerania ####
D_MW <- as.data.table(db)[
  ,kitagawa_cfr(db$Cases[db$Region=="Meckl.-Western Pomerania" & db$Date==min$Date[min$Region=="Meckl.-Western Pomerania"]],
                db$ascfr[db$Region=="Meckl.-Western Pomerania" & db$Date==min$Date[min$Region=="Meckl.-Western Pomerania"]],
                Cases,ascfr),
  by=list(Date, Region)]

# Select date & state
D_MW <- D_MW %>% filter(Date==max$Date[max$Region=="Meckl.-Western Pomerania"][1] & Region=="Meckl.-Western Pomerania") 
# MW has "two" peaks: 25 and 26 of April with same CFR

# Calculate relative contributions
D_MW2 <- D_MW %>% mutate(relAgeDE = abs(AgeComp)/(abs(AgeComp)+abs(RateComp)))
D_MW2 <- D_MW2 %>% mutate(relRateDE = abs(RateComp)/(abs(AgeComp)+abs(RateComp)))

## North Rhine-Westphalia ####
D_NR <- as.data.table(db)[
  ,kitagawa_cfr(db$Cases[db$Region=="North Rhine-Westphalia" & db$Date==min$Date[min$Region=="North Rhine-Westphalia"]],
                db$ascfr[db$Region=="North Rhine-Westphalia" & db$Date==min$Date[min$Region=="North Rhine-Westphalia"]],
                Cases,ascfr),
  by=list(Date, Region)]

# Select date & state
D_NR <- D_NR %>% filter(Date==max$Date[max$Region=="North Rhine-Westphalia"] & Region=="North Rhine-Westphalia") 

# Calculate relative contributions
D_NR2 <- D_NR %>% mutate(relAgeDE = abs(AgeComp)/(abs(AgeComp)+abs(RateComp)))
D_NR2 <- D_NR2 %>% mutate(relRateDE = abs(RateComp)/(abs(AgeComp)+abs(RateComp)))


## Rhineland-Palatinate ####
D_RP <- as.data.table(db)[
  ,kitagawa_cfr(db$Cases[db$Region=="Rhineland-Palatinate" & db$Date==min$Date[min$Region=="Rhineland-Palatinate"]],
                db$ascfr[db$Region=="Rhineland-Palatinate" & db$Date==min$Date[min$Region=="Rhineland-Palatinate"]],
                Cases,ascfr),
  by=list(Date, Region)]

# Select date & state
D_RP <- D_RP %>% filter(Date==max$Date[max$Region=="Rhineland-Palatinate"] & Region=="Rhineland-Palatinate") 

# Calculate relative contributions
D_RP2 <- D_RP %>% mutate(relAgeDE = abs(AgeComp)/(abs(AgeComp)+abs(RateComp)))
D_RP2 <- D_RP2 %>% mutate(relRateDE = abs(RateComp)/(abs(AgeComp)+abs(RateComp)))

## Saarland ####
D_SA <- as.data.table(db)[
  ,kitagawa_cfr(db$Cases[db$Region=="Saarland" & db$Date==min$Date[min$Region=="Saarland"]],
                db$ascfr[db$Region=="Saarland" & db$Date==min$Date[min$Region=="Saarland"]],
                Cases,ascfr),
  by=list(Date, Region)]

# Select date & state
D_SA <- D_SA %>% filter(Date==max$Date[max$Region=="Saarland"] & Region=="Saarland") 

# Calculate relative contributions
D_SA2 <- D_SA %>% mutate(relAgeDE = abs(AgeComp)/(abs(AgeComp)+abs(RateComp)))
D_SA2 <- D_SA2 %>% mutate(relRateDE = abs(RateComp)/(abs(AgeComp)+abs(RateComp)))

## Saxony ####
D_SX <- as.data.table(db)[
  ,kitagawa_cfr(db$Cases[db$Region=="Saxony" & db$Date==min$Date[min$Region=="Saxony"]],
                db$ascfr[db$Region=="Saxony" & db$Date==min$Date[min$Region=="Saxony"]],
                Cases,ascfr),
  by=list(Date, Region)]

# Select date & state
D_SX <- D_SX %>% filter(Date==max$Date[max$Region=="Saxony"] & Region=="Saxony") 

# Calculate relative contributions
D_SX2 <- D_SX %>% mutate(relAgeDE = abs(AgeComp)/(abs(AgeComp)+abs(RateComp)))
D_SX2 <- D_SX2 %>% mutate(relRateDE = abs(RateComp)/(abs(AgeComp)+abs(RateComp)))

## Saxony-Anhalt ####
D_SXA <- as.data.table(db)[
  ,kitagawa_cfr(db$Cases[db$Region=="Saxony-Anhalt" & db$Date==min$Date[min$Region=="Saxony-Anhalt"]],
                db$ascfr[db$Region=="Saxony-Anhalt" & db$Date==min$Date[min$Region=="Saxony-Anhalt"]],
                Cases,ascfr),
  by=list(Date, Region)]

# Select date & state
D_SXA <- D_SXA %>% filter(Date==max$Date[max$Region=="Saxony-Anhalt"] & Region=="Saxony-Anhalt") 

# Calculate relative contributions
D_SXA2 <- D_SXA %>% mutate(relAgeDE = abs(AgeComp)/(abs(AgeComp)+abs(RateComp)))
D_SXA2 <- D_SXA2 %>% mutate(relRateDE = abs(RateComp)/(abs(AgeComp)+abs(RateComp)))

## Schleswig-Holstein ####
D_SH <- as.data.table(db)[
  ,kitagawa_cfr(db$Cases[db$Region=="Schleswig-Holstein" & db$Date==min$Date[min$Region=="Schleswig-Holstein"]],
                db$ascfr[db$Region=="Schleswig-Holstein" & db$Date==min$Date[min$Region=="Schleswig-Holstein"]],
                Cases,ascfr),
  by=list(Date, Region)]

# Select date & state
D_SH <- D_SH %>% filter(Date==max$Date[max$Region=="Schleswig-Holstein"] & Region=="Schleswig-Holstein") 

# Calculate relative contributions
D_SH2 <- D_SH %>% mutate(relAgeDE = abs(AgeComp)/(abs(AgeComp)+abs(RateComp)))
D_SH2 <- D_SH2 %>% mutate(relRateDE = abs(RateComp)/(abs(AgeComp)+abs(RateComp)))

## Thuringia ####
D_TH <- as.data.table(db)[
  ,kitagawa_cfr(db$Cases[db$Region=="Thuringia" & db$Date==min$Date[min$Region=="Thuringia"]],
                db$ascfr[db$Region=="Thuringia" & db$Date==min$Date[min$Region=="Thuringia"]],
                Cases,ascfr),
  by=list(Date, Region)]

# Select date & state
D_TH <- D_TH %>% filter(Date==max$Date[max$Region=="Thuringia"] & Region=="Thuringia") 

# Calculate relative contributions
D_TH2 <- D_TH %>% mutate(relAgeDE = abs(AgeComp)/(abs(AgeComp)+abs(RateComp)))
D_TH2 <- D_TH2 %>% mutate(relRateDE = abs(RateComp)/(abs(AgeComp)+abs(RateComp)))

dec_ot1 <- rbind(D_BW, D_BY, D_BER, D_BRA, D_HB, D_HH, D_HES, D_LS, 
                 D_MW, D_NR, D_RP, D_SA, D_SX, D_SXA, D_SH, D_TH)
dec_ot1$Date2 <- min$Date

dec_ot1.1 <- rbind(D_BW2, D_BY2, D_BER2, D_BRA2, D_HB2, D_HH2, D_HES2, D_LS2, 
                   D_MW2, D_NR2, D_RP2, D_SA2, D_SX2, D_SXA2, D_SH2, D_TH2)


write_csv(dec_ot1, path="Output/decomp_overtime1.csv")
save.image("Output/decomp_overtime1.RData")

