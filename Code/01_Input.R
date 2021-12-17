### Get data ##################################################################

# Required packages
library(tidyverse)
library(readr)
library(lubridate)
library(ggplot2)
library(osfr)
library(doBy)
library(readxl)

source(("Code/00_Functions.R"))

# osf_retrieve_file("9dsfk") %>%
#   osf_download(conflicts = "overwrite", path="Data") 

dat <-  read_csv("Data/inputDB.zip",
                 skip = 1,
                 col_types = "cccccciccdc") 

### Edit data  ###############################################################
# Select country, remove ECDC-Data
dat <- dat %>% filter(Country %in% "Germany") 
dat <- dat %>% filter(Short!="DE_ECDC")

# Set date as date
dat$Date <- as.Date(dat$Date,"%d.%m.%Y")

# Drop if no cases/Deaths
dat <- na.omit(dat)

head(dat, n=20)
str(dat)
table(dat$Age) # no unknown ages
table(dat$Sex, useNA="ifany") # f m UNK
range(table(dat$Region))

### Data preparation ##########################################################
# separate df for cases and deaths
cases <- dat[dat$Measure=="Cases",]
deaths <- dat[dat$Measure=="Deaths",]

# aggregate counts for both sexes and unknown sex 
cases2 <- summaryBy(Value~Country+Region+Date+Age, data=cases, FUN=sum, keep.names=TRUE)
deaths2 <- summaryBy(Value~Country+Region+Date+Age, data=deaths, FUN=sum, keep.names=TRUE)

# total cases and deaths for all age groups
totalcases <- summaryBy(Value~Country+Region+Date, data=cases2, FUN=sum, keep.names=TRUE)
totaldeaths <- summaryBy(Value~Country+Region+Date, data=deaths2, FUN=sum, keep.names=TRUE)

totalcases$Cases<-totalcases$Value
totalcases$Value <- NULL
totaldeaths$Deaths <- totaldeaths$Value
totaldeaths$Value <- NULL
total <- merge(totalcases, totaldeaths, by=c("Country", "Region", "Date"), all=TRUE)

head(total)

# merging data frames of deaths and cases
deaths2$Deaths <- deaths2$Value
deaths2$Value <- NULL
cases2$Cases <- cases2$Value
cases2$Value <- NULL
db <- merge(cases2, deaths2, by=c("Country", "Region", "Date", "Age"), all=TRUE)

# sort by region and age
head(db, n=20)
str(db)

db$Age<-as.numeric(db$Age)
db <- db[order(db$Region, db$Age),]
head(db)
table(db$Age)
remove(cases); remove(deaths)
remove(cases2); remove(dat); remove(deaths2)
remove(totalcases); remove(totaldeaths)

# age-specific total numbers for Germany
db[db$Region=="All",]

#### Rename regions ####
db$Region <- recode(db$Region, "All"="Germany", "Bayern"="Bavaria", "Hessen"="Hesse",
                    "Niedersachsen"="Lower Saxony", "Nordrhein-Westfalen"="North Rhine-Westphalia",
                    "Rheinland-Pfalz"="Rhineland-Palatinate", "Sachsen"="Saxony", 
                    "Sachsen-Anhalt"="Saxony-Anhalt", "Thüringen"="Thuringia", 
                    "Mecklenburg-Vorpommern"="Meckl.-Western Pomerania")
table(db$Region)

total$Region <- recode(total$Region, "All"="Germany", "Bayern"="Bavaria", "Hessen"="Hesse",
                       "Niedersachsen"="Lower Saxony", "Nordrhein-Westfalen"="North Rhine-Westphalia",
                       "Rheinland-Pfalz"="Rhineland-Palatinate", "Sachsen"="Saxony", 
                       "Sachsen-Anhalt"="Saxony-Anhalt", "Thüringen"="Thuringia",
                       "Mecklenburg-Vorpommern"="Meckl.-Western Pomerania")
table(total$Region)


### German Population 2019 ####################################################
# Preparation of Data from "Statistisches Bundesamt"
pop <- read_excel("Data/Population2019.xlsx", range="A3:B20", col_names=TRUE)
pop$Region <- recode(pop$Region, "All"="Germany", "Bayern"="Bavaria", "Hessen"="Hesse",
                     "Niedersachsen"="Lower Saxony", "Nordrhein-Westfalen"="North Rhine-Westphalia",
                     "Rheinland-Pfalz"="Rhineland-Palatinate", "Sachsen"="Saxony", 
                     "Sachsen-Anhalt"="Saxony-Anhalt", "Thüringen"="Thuringia",
                     "Mecklenburg-Vorpommern"="Meckl.-Western Pomerania")
table(pop$Region)

# Add population data to total data
total <- merge(total, pop, by = "Region", all=TRUE)
remove(pop)

### Save ######################################################################
write_csv(db,path="Output/data.csv")
save.image("Output/data.RData")
