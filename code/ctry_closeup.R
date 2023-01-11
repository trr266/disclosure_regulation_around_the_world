
###############################################
# Get the number of available balance sheets and income statements in ORBIS
###############################################

suppressMessages({
  library(tidyverse)
  library(DBI)
  library(jsonlite)
})

library(readxl)
library(writexl)
library(reshape2)

memory.limit(size=50000)

# Set TRUE if you want to use the remote data API and to FALSE
# if you have local data
USE_API <- FALSE

# For local use, you need to set the folder that stores the parquet files
PARQUET_FOLDER <- "C:/Users/maternaj/Documents/TRR/A01/Repositories/global_transparency/data/parquet_files"

# Load Pfile info
pfiles_info <- read.csv("data/parquet_files/bvd_parquet_files_info.csv")
pfiles_schemas <- read.csv("data/parquet_files/bvd_parquet_files_schemas.csv")




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



# Have a look at france
ctries                <- "FR"



#Initialize loop
number_bs_orbis <- data.frame()
number_is_orbis <- data.frame()

pb <- txtProgressBar(min = 0, max = length(ctries), style = 3)
i <- 0

for(i in 1:length(ctries)){
  
  ctry <- ctries[i]
  
  
  ##########
  # Financial Info
  ##########
  
  financial_info <- pdata("industry_global_financials_and_ratios_eur.parquet")
  
  financial_info_cols <- {pfiles_schemas %>% 
      filter(parquet_file == "industry_global_financials_and_ratios_eur.parquet") %>% 
      pull(name)}
  
  financial_info_cols <- c(# Basic company info
    "BvD ID number",
    "Number of employees",
    "Estimated employees",
    "Closing date",
    
    # Disclosure info
    "Number of months",
    "Consolidation code",
    "Filing type",
    "Accounting practice",
    "Original currency",
    
    # Balance sheet items 
    #Assets
    "Fixed assets",
    "Current assets",
    "Total assets",
    
    # Liabilities
    "Shareholders funds",
    "Current liabilities",
    "Non-current liabilities",
    "Total shareh. funds & liab.",
    
    # Income statement items 
    "Sales",
    "Operating P/L [=EBIT]",
    "P/L for period [=Net income]")
  
  
  
  query_str <- sprintf(
    paste0("SELECT \"%s\" FROM '%s' WHERE \"BvD ID number\" LIKE '",ctry, "%%'"),
    paste(financial_info_cols, collapse = '", "'),
    financial_info 
  )
  
  con <- connect_duckdb(":memory:")
  financial_info_data <- dbGetQuery(con, query_str)
  disconnect_duckdb(con)
  financial_info_data$`Total assets` <- as.numeric(financial_info_data$`Total assets`)
  financial_info_data$year <- substr(financial_info_data$`Closing date`,1,4)
  
  
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
                       "Historical record flag")
  
  query_str <- sprintf(
    paste0("SELECT \"%s\" FROM '%s' WHERE \"BvD ID number\" LIKE '",ctry, "%%'"),
    paste(legal_info_cols, collapse = '", "'),
    legal_info
  )
  
  
  
  con <- connect_duckdb(":memory:")
  legal_info_data <- dbGetQuery(con, query_str)
  disconnect_duckdb(con)
  
  
  
  
  
  ########
  # Merge data
  #######
  
  df <- merge(financial_info_data, legal_info_data, by =c("BvD ID number"), all.x = T)
  
  
  
  ########
  # Do analysis
  #######
  
  
  # Check if the companies provide a balance sheet. A balance sheet is assumed to be existing if total assets, debt and equity are known
  df$bs <- complete.cases(df$`Total assets`,
                          df$`Shareholders funds`,
                          df$`Current liabilities`,
                          df$`Non-current liabilities`)
  
  # Check if the companies provide an income statement. An income statement is assumed to be existing if turnover and profit/loss are known
  df$is <- complete.cases(df$Sales,
                          df$`P/L for period [=Net income]`)
  
  #Count number of available financial statements 
  bs <- df %>%
    filter(
      `Total assets` > 0,
      `Number of months` == "12",
      bs == TRUE
    ) %>%
    group_by(year, `National legal form`) %>%
    summarize('Number of Balance Sheets' = length(unique(`BvD ID number`))) %>%
    collect()
  
  #Get main information provider for balance sheets
  i_prov_bs <- df %>%
    filter(
      `Total assets` > 0,
      `Number of months` == "12",
      bs == TRUE
    ) %>%
    group_by(year, `National legal form`) %>%
    count(`Information provider`) %>%
    slice(which.max(n))
  
  bs <- merge(bs, i_prov_bs, by=c("year", "National legal form"), all.x =T)
  
  # Clean data
  bs <- bs[,c(1:4)]
  colnames(bs) <- c( "year", "national_legal_form_orbis","number_balance_sheets" ,"main_information_provider_orbis")
  
  
  
  
  
  
  
  
  #Count number of available income statements
  is <- df %>%
    filter(
      `Total assets` > 0,
      `Number of months` == "12",
      is == TRUE
    ) %>%
    group_by(year, `National legal form`) %>%
    summarize('Number of Balance Sheets' = length(unique(`BvD ID number`))) %>%
    collect()
  
  
  #Get main information provider for income statements
  i_prov_is <- df %>%
    filter(
      `Total assets` > 0,
      `Number of months` == "12",
      is == TRUE
    ) %>%
    group_by(year, `National legal form`) %>%
    count(`Information provider`) %>%
    slice(which.max(n))
  
  is <- merge(is, i_prov_is, by=c("year", "National legal form"), all.x =T)
  
  # Clean data
  is <- is[,c(1:4)]
  colnames(is) <- c( "year", "national_legal_form_orbis","number_income_statements", "main_information_provider_orbis")
  
  
  # Save data
  bs <- cbind(iso2 =ctry,bs)
  is <- cbind(iso2 =ctry,is)
  
  bs <- bs[bs$year > 2000,]
  is <- is[is$year > 2000,]
  
  number_bs_orbis <- rbind(number_bs_orbis, bs)
  number_is_orbis <- rbind(number_is_orbis, is)
  
  
  
  #Save it
  i <- i+1
  setTxtProgressBar(pb, i)
}



df$`Total assets`



toas_over_time <- df %>%
  filter(`Standardised legal form` %in% "Sole traders/proprietorships" ) %>%
  group_by(year) %>%
  summarise("n" = length(unique(`BvD ID number`)))



df_2010 <-  df %>%
  filter(`Standardised legal form` %in% "Partnerships") %>%
  filter(year == 2010)


df_2012 <-  df %>%
  filter(`Standardised legal form` %in% "Partnerships") %>%
  filter(year == 2012)


hist(log(df_2010$`Total assets`[df_2010$`Total assets` < 15000000]), breaks = 100)
hist(log(df_2017$`Total assets`[df_2017$`Total assets` < 15000000]), breaks = 500)
hist(log(df_2018$`Total assets`), breaks = 500)
abline(v=log(200000), col="blue")
