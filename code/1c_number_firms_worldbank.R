
library(readxl)

###############################################
# Worldbank data for number of limited liability firms
###############################################


#Source link	https://www.worldbank.org/en/programs/entrepreneurship#total


worldbank_iso2 <- read_excel("data/raw/worldbank/number_firms_worldbank.xlsx", sheet = "worldbank_summary")

number_firms_worldbank <- read_excel("data/raw/worldbank/number_firms_worldbank.xlsx", sheet = "worldbank_data", skip = 1)





number_firms_worldbank <- merge(worldbank_iso2, number_firms_worldbank, by = c("ctry_name"), all.x = T)
number_firms_worldbank <- worldbank_data[!is.na(worldbank_data$no_llc),]


number_firms_worldbank <- number_firms_worldbank[c("iso2", "year", "no_llc")]
colnames(number_firms_worldbank) <- c("iso2", "year", "number_firms_worldbank")
number_firms_worldbank$legal_form <- "private_limited_liability"



# Save
###### Save
write.csv(number_firms_worldbank, "data/country_info/number_firms_worldbank.csv")
saveRDS(number_firms_worldbank, "data/raw/generated/number_firms_worldbank.rds")