
###############################################
# Estimate the number of active firms in ORBIS using history files 
###############################################

suppressMessages({
  library(tidyverse)
  library(DBI)
  library(jsonlite)
})

library(readxl)
library(writexl)
library(reshape2)
library(dplyr)

memory.limit(size=50000)

# Set TRUE if you want to use the remote data API and to FALSE
# if you have local data
USE_API <- FALSE

# For local use, you need to set the folder that stores the parquet files
PARQUET_FOLDER <- "C:/Users/maternaj/Documents/TRR/A01/Repositories/global_transparency/data/parquet_files"

# Load Pfile info
pfiles_info <- read.csv("data/parquet_files/bvd_parquet_files_info.csv")
pfiles_schemas <- read.csv("data/parquet_files/bvd_parquet_files_info.csv")




# Define some functions
pdata <- function(pfile) {
  file.path(PARQUET_FOLDER, pfile) 
} 

connect_duckdb <- function(dbase) {
  con <- dbConnect(duckdb::duckdb(), ":memory:")
  return(con)
}

disconnect_duckdb <- function(con){
  dbDisconnect(con, shutdown = TRUE)
}



# Load firms with data from eurostat
number_firms_eurostat <- readRDS("data/generated/number_firms_eurostat.rds")
ctries                <- unique(number_firms_eurostat$iso2)



pb <- txtProgressBar(min = 0, max = length(ctries), style = 3)
i <- 0
output <- data.frame()

for(i in 1:length(ctries)){
  
  ctry <- ctries[i]
  
  ########
  # Legal Info
  #######
  
  #Load the legal info
  legal_info <- pdata("legal_info.parquet")
  
  legal_info_cols <- c("BvD ID number",
                       "Standardised legal form",
                       "National legal form",
                       "Status",
                       "Status date", 
                       "Information provider",
                       "Date of incorporation",
                       "Historical record flag")
  
  query_str <- sprintf(
    paste0("SELECT \"%s\" FROM '%s' WHERE \"BvD ID number\" LIKE '",ctry, "%%'"),
    paste(legal_info_cols, collapse = '", "'),
    legal_info
  )
  
  
  con <- connect_duckdb(":memory:")
  legal_info_data <- dbGetQuery(con, query_str)
  disconnect_duckdb(con)
  
  
  legal_info_data$year_incorp <- substr(legal_info_data$`Date of incorporation`,1,4)
  
  
  
  ########
  # Load historical Info
  #######
  
  
  legal_hist_info <- pdata("statushistory.parquet")
  
  legal_hist_cols <- c("BvD ID number",
                       "Status",
                       "Status date",
                       "Status updated in Orbis")
  
  
  
  query_str <- sprintf(
    paste0("SELECT \"%s\" FROM '%s' WHERE \"BvD ID number\" LIKE '",ctry, "%%'"),
    paste(legal_hist_cols, collapse = '", "'),
    legal_hist_info 
  )
  
  
  
  con <- connect_duckdb(":memory:")
  legal_hist_data <- dbGetQuery(con, query_str)
  disconnect_duckdb(con)
  
  
  
  
  
  
  
  
  
  ########
  # Step 1: Get all companies that are currently active
  #######
  
  df_active                <- legal_info_data[legal_info_data$Status %in% "Active",]
  df_active$incorp_date    <- df_active $`Date of incorporation`
  df_active$year_inactive  <- NA
  
  df_active <- df_active[,c("BvD ID number", "year_incorp", "year_inactive", "National legal form")]
  
  
  ########
  # Step 2: Get all companies that have been active in the past and get the date they lost the status
  #######
  
  # Get history of companies that are currently non active
  non_active <- legal_info_data$`BvD ID number`[!legal_info_data$Status %in% "Active"]
  history    <- legal_hist_data[legal_hist_data$`BvD ID number` %in% non_active,]
  
  # Get history of companies that have been active at some point in time but are not active anymore
  former_active <- unique(history$`BvD ID number`[history$Status %in% "Active"]) 
  history       <- history[history$`BvD ID number` %in% former_active,]
  
  # Get the date the company lost its "Active" label
  history$date <- ifelse(is.na(history$`Status date`), history$`Status updated in Orbis`, history$`Status date`)
  
  df_inactive <- history %>%
    group_by(`BvD ID number`) %>% 
    filter(!Status %in% "Active") %>%
    summarize(inactive_date = min(date))
  
  df_inactive <- merge(df_inactive, legal_info_data , by = c("BvD ID number"), all.x = T)
  df_inactive$year_inactive <- substr(df_inactive$inactive_date,1,4)
  
  df_inactive <- df_inactive[,c("BvD ID number", "year_incorp", "year_inactive", "National legal form")]
  
  
  
  
  
  df <- rbind(df_active, df_inactive)
  df <- df[!is.na(df$year_incorp),]
  
  
  ########
  # Step 3: Count number of active firms
  #######
  
  
  years <- c(2010:2022)
  df$year_inactive[is.na(df$year_inactive)] <- 3000
  summary <- data_frame()
  
  for(y in years){
    
    # Get active firms
    d <- df[df$year_incorp <= y,]
    d <- d[!d$year_inactive < y,]
    
    d <- d %>% group_by(`National legal form`) %>%
      summarize("active_firms" = length(`BvD ID number`))
    
    d$year <- y
    d$iso2 <- ctry
    
    summary <- rbind(summary, d)
  }
  
  
  
  
  output <- rbind(output, summary)
  i <- i+1
  setTxtProgressBar(pb, i)
  
}

######## Save data

colnames(output) <- c("national_legal_form_orbis", "number_firms_orbis", "year", "iso2" )
number_firms_orbis_natLegForm <- output


########
# Process and save data
#######


####### Get standardized legal form from matching table
orbis_legal_forms <- read.csv("data/county_info/orbis_legal_forms.csv")
number_firms_orbis<- merge(output, orbis_legal_forms, by=c("iso2", "national_legal_form_orbis"), all.x = T)



###### Rename legal forms
number_firms_orbis$legal_form <- NA
number_firms_orbis$legal_form[number_firms_orbis$standardised_legal_form_orbis %in% "Private limited companies"]   <- "Limited liability enterprise"
number_firms_orbis$legal_form[number_firms_orbis$standardised_legal_form_orbis %in% "Public limited companies"]    <- "Limited liability enterprise"
number_firms_orbis$legal_form[number_firms_orbis$standardised_legal_form_orbis %in% "Partnerships"]                <- "Partnership, co-operatives, associations, etc."
number_firms_orbis$legal_form[number_firms_orbis$standardised_legal_form_orbis %in% "Sole traders/proprietorships"]<- "Sole proprietorship"


####### Sum by standardized legal form
number_firms_orbis <- number_firms_orbis %>%
  group_by(iso2, year, legal_form) %>%
  summarize("number_firms_orbis" = sum(number_firms_orbis))



####### Relabel and save needed firms
number_firms_orbis <- number_firms_orbis[!is.na(number_firms_orbis$legal_form),]


###### Save
saveRDS(number_firms_orbis, "data/generated/number_firms_orbis.rds")
saveRDS(output, "data/generated/number_firms_orbis_natLegForm.rds")

