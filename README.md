
## Disclosure Regulation Around the World 

This repository provides the code and data for our analysis of disclosure regulation around the world.

Our datasets can be found in the data folder: 
- `data/country_info` Datasets on basic country information for 193 UN constituents
- `data/disclosure_regulation` Data on disclosure regulation in the European Union
- `data/ifrs_adoption` Data on IFRS adoption around the globe

The results of analysis 1 as well as all plots from analysis 2 are in the output foler. All codes can be found in the 'code' folder:

- `code/corp_trans_measures/1a_number_firms_eurostat.R` Processes the number of firms by legal form provided by Eurostat
- `code/corp_trans_measures/1b_number_firms_orbis.R` Estimates the number of 'active' firms in the ORBIS database
- `code/corp_trans_measures/2_number_bs_is.R` Calculates the number of firms with available balance sheet and income statements in ORBIS
- `code/corp_trans_measures/3_disclosure_measures.R` Merges the generated datasets and calculates the share of disclosing firms
- `code/corp_trans_measures/4_analysis_1.R` Produces the statistics for analysis 1
- `code/corp_trans_measures/5_analysis_2.R` Produces the plots and statistics for analysis 2

The ORBIS data used in the analysis is currently available to TRR 266 members only. 


