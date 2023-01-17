
library(dplyr)

######## Read needed data

ctry_profiles            <- read.csv("data/country_info/ctry_profiles.csv")
number_firms_stat_office <- read.csv("data/country_info/number_firms_stat_office.csv")
number_firms_orbis       <- read.csv("data/country_info/number_firms_orbis.csv")
worldbank_data           <- read.csv("data/country_info/worldbank_data.csv")


###############################################
# Investigate and clean data
###############################################


######## Focus on years 2010-2021

number_firms_orbis <- number_firms_orbis[number_firms_orbis$year %in% c(2010:2021),]
number_firms_stat_office <- number_firms_stat_office[number_firms_stat_office$year %in% c(2010:2021),]


######## Merge with worldbank data
number_firms_orbis <- merge(number_firms_orbis, worldbank_data, by=c("iso2", "year"), all.x = T)
number_firms_stat_office <- merge(number_firms_stat_office, worldbank_data, by=c("iso2", "year"), all.x = T)


######## Count number of country years for statistical offices and ORBIS

sum(!is.na(number_firms_orbis$number_firms))
sum(!is.na(number_firms_stat_office$number_firms))


######## Investigate cases with low number of observations

sum(number_firms_orbis$number_firms[!is.na(number_firms_orbis$number_firms)] < 100)
sum(number_firms_stat_office$number_firms[!is.na(number_firms_stat_office$number_firms)] < 100)

min(number_firms_orbis$number_firms, na.rm = T)
min(number_firms_stat_office$number_firms, na.rm = T)


######## Calculate ratio of firms to population

number_firms_orbis$pop       <- number_firms_orbis$number_firms / number_firms_orbis$population
number_firms_stat_office$pop <- number_firms_stat_office$number_firms / number_firms_stat_office$population

hist(number_firms_orbis$pop, breaks = 50)
hist(number_firms_stat_office$pop, breaks = 50)


######## Set number of firms to missing when share < 0.2% relative to population 

number_firms_orbis$number_firms[number_firms_orbis$pop < 0.002]             <- NA
number_firms_stat_office$number_firms[number_firms_stat_office$pop < 0.002] <- NA


######## Count number of country years for statistical offices and ORBIS after cleaning

sum(!is.na(number_firms_orbis$number_firms))
sum(!is.na(number_firms_stat_office$number_firms))


###############################################
# Describe data
###############################################


######## Share of covered sample years

sum(!is.na(number_firms_orbis$number_firms)) / (193*12)
sum(!is.na(number_firms_stat_office$number_firms)) / (193*12)


######## Merge both data sets 
colnames(number_firms_orbis) <- c("iso2","year","number_firms_orbis", "gdp", "population","pop") 
colnames(number_firms_stat_office) <- c("iso2","year","source_link", "number_firms_stat_office", "gdp", "population","pop") 

number_firms_orbis <- number_firms_orbis[, c("iso2","year","number_firms_orbis")]
df <- merge(number_firms_orbis , number_firms_stat_office)


year <- df %>%
  group_by(year) %>%
  summarize("obs" = length(iso2),
            "firms_office_yes" = sum(!is.na(number_firms_stat_office)),
            "firms_orbis_yes"  = sum(!is.na(number_firms_orbis)),
            "share_office"     = sum(!is.na(number_firms_stat_office)) / length(iso2),
            "share_orbis"      = sum(!is.na(number_firms_orbis)) / length(iso2))


######## Observations by continent

df <- merge(df, ctry_profiles , by = c("iso2"))

continent <- df %>%
  group_by(continent) %>%
  summarize("obs" = length(iso2),
            "firms_office_yes" = sum(!is.na(number_firms_stat_office)),
            "firms_orbis_yes"  = sum(!is.na(number_firms_orbis)),
            "share_office"     = sum(!is.na(number_firms_stat_office)) / length(iso2),
            "share_orbis"      = sum(!is.na(number_firms_orbis)) / length(iso2))


######## Observations by population

population            <- sum(df$population, na.rm =T)
population_office_yes <- sum(df$population[!is.na(df$number_firms_stat_office)], na.rm =T)
population_orbis_yes  <- sum(df$population[!is.na(df$number_firms_orbis)], na.rm =T)
share_office          <- population_office_yes / population
share_orbis           <- population_orbis_yes / population

population <- data.frame(population,
                         population_office_yes,
                         population_orbis_yes,
                         share_office,
                         share_orbis)


######## Observations by gdp

gdp                   <- sum(df$gdp, na.rm =T)
gdp_office_yes        <- sum(df$gdp[!is.na(df$number_firms_stat_office)], na.rm =T)
gdp_orbis_yes         <- sum(df$gdp[!is.na(df$number_firms_orbis)], na.rm =T)
share_office          <- gdp_office_yes / gdp
share_orbis           <- gdp_orbis_yes / gdp

gdp <- data.frame(gdp,
                         gdp_office_yes,
                         gdp_orbis_yes,
                         share_office,
                         share_orbis)


