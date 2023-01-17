###############################################
# Eurostat data for number of firms by legal form and year
###############################################

library(dplyr)

###### Link for downloading data from Eurostat
# https://ec.europa.eu/eurostat/databrowser/view/BD_9AC_L_FORM_R2__custom_3992779/default/table?lang=en


###### Load data 
number_firms_eurostat <- read.csv("data/raw/eurostat_data/number_firms_eurostat_raw.csv")


###### Adjust variable names
names(number_firms_eurostat)[names(number_firms_eurostat) == 'leg_form']    <- 'legal_form'
names(number_firms_eurostat)[names(number_firms_eurostat) == 'geo']         <- 'iso2'
names(number_firms_eurostat)[names(number_firms_eurostat) == 'TIME_PERIOD'] <- 'year'
names(number_firms_eurostat)[names(number_firms_eurostat) == 'OBS_VALUE']   <- 'number_firms_eurostat'


###### Rename legal forms
number_firms_eurostat$legal_form[number_firms_eurostat$legal_form %in% "ENT_LL"]   <- "Limited liability enterprise"
number_firms_eurostat$legal_form[number_firms_eurostat$legal_form %in% "ENT_PA"]   <- "Partnership, co-operatives, associations, etc."
number_firms_eurostat$legal_form[number_firms_eurostat$legal_form %in% "ENT_SOLE"] <- "Sole proprietorship"


###### We are interested in the active number of enterprises
number_firms_eurostat <- number_firms_eurostat %>% filter(indic_sb %in% "V11910")


###### Get all available industries
number_firms_eurostat <- number_firms_eurostat %>% filter(nace_r2 %in% "B-N_X_K642")


###### Remove totals
number_firms_eurostat <- number_firms_eurostat[!number_firms_eurostat$legal_form %in% "TOTAL",]


###### Get correct iso code for Greece and the united Kingdom
number_firms_eurostat$iso2[number_firms_eurostat$iso2 %in% "EL"] <- "GR"
number_firms_eurostat$iso2[number_firms_eurostat$iso2 %in% "UK"] <- "GB"

###### Keep needed variables only
number_firms_eurostat <- number_firms_eurostat[,c("year", "iso2", "legal_form", "number_firms_eurostat")]
number_firms_eurostat <- number_firms_eurostat[!number_firms_eurostat$iso2 %in% c("EU27_2020", "EU28", "EU_V", "EU27_2007"),]


###### Save
write.csv(number_firms_eurostat, "data/country_info/number_firms_eurostat.csv")
