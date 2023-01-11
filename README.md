
## Disclosure Regulation Around the World 

This repository provides the code for estimating the share of companies disclosing a balance sheet and an income statement in europe by legal form.

The output can be found in the output file. The measures have been calculated using the code provided in the 'code' folder:

- `code/corp_trans_measures/1a_number_firms_eurostat.R` Processes the number of firms by legal form provided by Eurostat
- `code/corp_trans_measures/1b_number_firms_orbis.R` Estimates the number of 'active' firms in the ORBIS database
- `code/corp_trans_measures/2_number_bs_is.R` Calculates the number of firms with avaiable balance sheet and income statements in ORBIS
- `code/corp_trans_measures/3_disclosure_measures.R` Merges the generated datasets and calculates the share of disclosing firms

The measures have are based on ORBIS parquet files which ware currently available to TRR 266 members only. 


