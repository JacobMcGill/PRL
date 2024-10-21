## Loading packages used in analysis
import pandas as pd
import numpy as np
import seaborn as sns
import matplotlib.pyplot as plt
import matplotlib.ticker as mtick

##setting filepath, sheet names for raw data
raw_filepath = 'C:/Users/Ben/Documents/datasets/Meta_rep/Replication_Data.xlsx'
raw_sheet = 'raw2'


##########################################################################################

# Constructing various datasets to be used for analysis

##########################################################################################

## Pulling in and formatting raw data
raw_df = pd.read_excel(raw_filepath,sheet_name = raw_sheet)

raw_df['publish_date'] = raw_df['publish_date'].dt.to_period('M')
raw_char_to_replace = {'?':'',
                       '-':'',
                       ':':'',
                       ',':'',
                       "'":''}
for key,value in raw_char_to_replace.items():
    raw_df['paper_name'] = raw_df['paper_name'].str.replace(key,value)
raw_df['paper_name'] = raw_df['paper_name'].str.title()
raw_df['paper_name'] = raw_df['paper_name'].str.replace(' ','')
raw_df['code_types'] = raw_df['code_types'].str.replace(',','')
raw_df['code_types'] = raw_df['code_types'].str.replace(' ','')

## Putting theory papers into their own df
theory_df = raw_df.loc[raw_df['theory'] == 1,raw_df.columns[0:6]]

## putting papers with long computation times into their own df
comp_time_df = raw_df.loc[raw_df['comp_time_too_long'] == 1, raw_df.columns[0:7]]

## putting papers with oddball software requirements into own df
software_gating_df = raw_df.loc[(raw_df['code_types'] == 'StataArcScene'),raw_df.columns[0:9]]
software_gating_df = pd.concat([software_gating_df, 
                                raw_df.loc[(raw_df['code_types'] == 'StataMaple'),raw_df.columns[0:9]]])

## dataframe with dropped theory, long comp papers papers
main_df = raw_df.loc[(raw_df['theory'] == 0) & 
                     (raw_df['comp_time_too_long'] != 1) &
                     (raw_df['code_types'] != 'StataArcScene') &
                     (raw_df['code_types'] != 'StataMaple') &
                     (raw_df['journal'] == 'AER'),raw_df.columns[0:]]
main_df.drop('authors',axis = 1, inplace = True)

##########################################################################################

# Analysis section

##########################################################################################

## getting various counts
###from full sample
ct_full_sample = len(main_df)
ct_rep_folder = len(main_df[main_df['replication_folder'] == 1])
ct_fully_replicable = len(main_df[main_df['fully_replicable'] == 1])
ct_missing_code = len(main_df[main_df['missing_code'] == 1])
ct_missing_data = len(main_df[main_df['missing_data'] == 1])
ct_prop_data = len(main_df[main_df['proprietary_data'] == 1])
ct_data_instruct = len(main_df[main_df['instructions_to_data'] == 1])
ct_results_match = len(main_df[main_df['results_match'] == 1])
ct_readme = len(main_df[main_df['readme'] == 1])
ct_code_avail = len(main_df[main_df['code_available'] == 1])
ct_all_code_avail = len(main_df[(main_df['code_available'] == 1) & (main_df['missing_code'] == 0)])
ct_data_avail = len(main_df[main_df['data_available'] == 1])
ct_master_script = len(main_df[main_df['master_script'] == 1])
ct_all_data_avail = len(main_df[(main_df['data_available'] == 1) & (main_df['missing_data'] == 0)])
ct_missing_some_data = len(main_df[(main_df['data_available'] == 1) & (main_df['missing_data'] == 1)])
ct_missing_some_code = len(main_df[(main_df['code_available'] == 1) & (main_df['missing_code'] == 1)])
ct_error_alldc = len(main_df[(main_df['ran_with_error'] == 1) & (main_df['data_available'] == 1) 
                             & (main_df['missing_data'] == 0) & (main_df['code_available'] == 1) 
                             & (main_df['missing_code'] == 0)])
ct_all_code_data_avail = len(main_df[(main_df['data_available'] == 1) & (main_df['missing_data'] == 0)
                                    & (main_df['code_available'] == 1) & (main_df['missing_code'] == 0)])
ct_data_instruct_alldc = len(main_df[(main_df['data_available'] == 1) & (main_df['missing_data'] == 0)
                                    & (main_df['code_available'] == 1) & (main_df['missing_code'] == 0)
                                    & (main_df['instructions_to_data'] == 1)])
ct_master_script_alldc = len(main_df[(main_df['data_available'] == 1) & (main_df['missing_data'] == 0)
                                    & (main_df['code_available'] == 1) & (main_df['missing_code'] == 0)
                                    & (main_df['master_script'] == 1)])
ct_alldc_rep_match = len(main_df[(main_df['results_match'] == 1) & (main_df['fully_replicable'] == 1)
                                & (main_df['data_available'] == 1) & (main_df['missing_data'] == 0)
                                    & (main_df['code_available'] == 1) & (main_df['missing_code'] == 0)])

## getting percentages on the full sample
pct_replicable_full = (ct_fully_replicable/ct_full_sample)*100
pct_readme_full = (ct_readme/ct_full_sample)*100
pct_codemiss_full = (ct_missing_code/ct_full_sample)*100
pct_datamiss_full = (ct_missing_data/ct_full_sample)*100
pct_propdata_full = (ct_prop_data/ct_full_sample)*100
pct_data_instruct_full = (ct_data_instruct/ct_full_sample)*100
pct_alldc_full = (ct_all_code_data_avail/ct_full_sample)*100

## getting percentages conditional on having all code, data avail
pct_replicable_alldc = (ct_fully_replicable/ct_all_code_data_avail)*100
pct_res_match_alldc = (ct_alldc_rep_match/ct_all_code_data_avail)*100
pct_data_instruct_alldc = (ct_data_instruct_alldc/ct_all_code_data_avail)*100
pct_master_script_alldc = (ct_master_script_alldc/ct_all_code_data_avail)*100


## percentage on fully replicable
pct_results_match = (ct_results_match/ct_fully_replicable)*100

### Making Dataframes ###
df_full = pd.DataFrame(columns = ['Count','% Missing Code', '% Missing Data', '% Proprietary Data','% All Data/Code','% Fully Replicable'])
df_full['Count'] = [ct_full_sample]
df_full['Count'] = df_full['Count'].round()
df_full['% Missing Code'] = [pct_codemiss_full]
df_full['% Missing Code'] = df_full['% Missing Code'].round(2)
df_full['% Missing Data'] = [pct_datamiss_full]
df_full['% Missing Data'] = df_full['% Missing Data'].round(2)
df_full['% Proprietary Data'] = [pct_propdata_full]
df_full['% Proprietary Data'] = df_full['% Proprietary Data'].round(2)
df_full['% All Data/Code'] = [pct_alldc_full]
df_full['% All Data/Code'] = df_full['% All Data/Code'].round(2)
df_full['% Fully Replicable'] = [pct_replicable_full]
df_full['% Fully Replicable'] = df_full['% Fully Replicable'].round(2)

                             
df_alldc = pd.DataFrame(columns = ['Count','% Fully Replicable', '% Fully Replicable and Results Match'])
df_alldc['Count'] = [ct_all_code_data_avail]
df_alldc['Count'] = df_alldc['Count'].round()
df_alldc['% Fully Replicable'] = [pct_replicable_alldc]
df_alldc['% Fully Replicable'] = df_alldc['% Fully Replicable'].round(2)
df_alldc['% Fully Replicable and Results Match'] = [pct_res_match_alldc]
df_alldc['% Fully Replicable and Results Match'] = df_alldc['% Fully Replicable and Results Match'].round(2)

df_resmatch = pd.DataFrame(columns = ['Count','% Results Match'])
df_resmatch['Count'] = [ct_fully_replicable]
df_resmatch['Count'] = df_resmatch['Count'].round()
df_resmatch['% Results Match'] = [pct_res_match_alldc]
df_resmatch['% Results Match'] = df_resmatch['% Results Match'].round(2)

## making plots
f, ax = plt.subplots(figsize = (8,8))
colors = ['#6D9EEB','#FFD966','#B6D7A8','#F6B26B','#D5A6BD']
sns.set_palette(sns.color_palette(colors))
sns.set_style("darkgrid")
plt_full = sns.barplot(data = df_full.drop(['Count'],axis = 1))
plt.title(f'Fig. 1: Proportional characteristics of the full sample, n = {ct_full_sample}', y = 1.05)
plt.ylim(bottom = 0, top = 100)
plt.ylabel("Percent", fontsize = 12)
plt.xlabel("Characteristic", fontsize = 12)
plt.xticks([0,1,2,3,4],['Missing Code','Missing Data', 'Proprietary Data', 'All Data/Code', 'Fully Replicable'],
           fontsize = 10, rotation = 45)
ax.bar_label(ax.containers[0])

f1, ax1 = plt.subplots(figsize = (8,8))
colors2 = ['#D5A6BD','#E06666']
sns.set_palette(sns.color_palette(colors2))
sns.set_style("darkgrid")
plt_full = sns.barplot(data = df_alldc.drop(['Count'],axis = 1))
plt.title(f'Fig. 2: Proportional Characteristics of papers with requisite replication materials, n = {ct_all_code_data_avail}', y = 1.05)
plt.ylim(bottom = 0, top = 100)
plt.ylabel("Percent", fontsize = 12)
plt.xlabel("Characteristic", fontsize = 12)
plt.xticks([0,1],['Fully Replicable','Matched Results'],
           fontsize = 10, rotation = 45)
ax1.bar_label(ax1.containers[0])
