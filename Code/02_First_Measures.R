library(tidyverse); library(dplyr); 
load("Output/data.RData")

head(db)
head(total)

#### Calculations ####
# Cases per 100.000 population
total <- total %>% mutate(CasesPop = (Cases/Population)*100000 )

# Deaths per 100.000 population
total <- total %>% mutate(DeathsPop = (Deaths/Population)*100000 )

# CFRs in Germany
total <- total %>% mutate(CFR = Deaths / Cases,
                          CFR = replace_na(CFR, 0))

total <- total %>% arrange(Region) 

# Calculate ASFRs
db <- db %>% mutate(ascfr = Deaths / Cases,
                    ascfr = replace_na(ascfr, 0))

save.image("Output/analysis.RData")

