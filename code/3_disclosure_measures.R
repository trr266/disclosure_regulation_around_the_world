###############################################
# Merge the generated files
###############################################


######## Load data
number_firms_orbis    <- readRDS("data/generated/number_firms_orbis.rds")
number_firms_eurostat <- readRDS("data/generated/number_firms_eurostat.rds")
number_bs_orbis       <- readRDS("data/generated/number_bs_orbis.rds")
number_is_orbis       <- readRDS("data/generated/number_is_orbis.rds")


######## Merge
disclosure_measures <- merge(number_firms_orbis, number_firms_eurostat, by = c("iso2", "year", "legal_form"), all.x = T)
disclosure_measures <- merge(disclosure_measures, number_bs_orbis, by = c("iso2", "year", "legal_form"), all.x = T)
disclosure_measures <- merge(disclosure_measures, number_is_orbis, by = c("iso2", "year", "legal_form"), all.x = T)
disclosure_measures <- disclosure_measures[disclosure_measures$year > 2009,]


######## Adjust NAs of disclosures
disclosure_measures$number_balance_sheets[is.na(disclosure_measures$number_balance_sheets)] <- 0
disclosure_measures$number_income_statements[is.na(disclosure_measures$number_income_statements)] <- 0


######## Calculate shares
disclosure_measures$bs_share <- disclosure_measures$number_balance_sheets/disclosure_measures$number_firms_orbis
disclosure_measures$is_share <- disclosure_measures$number_income_statements/disclosure_measures$number_firms_orbis



###### Save
saveRDS(disclosure_measures, "output/disclosure_measures.rds")
write.csv(disclosure_measures, "output/disclosure_measures.csv", row.names = F)
