# -*- coding: utf-8 -*-
"""
Created on Tue Nov 29 08:17:55 2022

@author: MMILLE16

Scripts to generate an excel "report" of comparison between manual classifcation of ohts
and output of toxval_map_to_oht.py function
"""

#Random one-of script to check work with oht mapping script to Evelyn's manually created oht mappings
#Still need to finish fingerprint file, but lets check what we have so far
import pandas as pd


#Step 1: Load in files
manual_in_file = r"C:\Users\mmille16\OneDrive - Environmental Protection Agency (EPA)\Profile\Downloads\toxval_full_pull_2022_08_18.xlsx"
script_in_file = r"C:\Users\mmille16\OneDrive - Environmental Protection Agency (EPA)\Profile\Desktop\check2_12_30_22.xlsx"

#Create pandas dataframes based on the excel files.
manual = pd.read_excel(manual_in_file)
script = pd.read_excel(script_in_file)


#%%

#Extract the unique oht numbers from the manually classified document
unique_nums = manual['OHT'].unique()
unique_nums = unique_nums[~pd.isnull(unique_nums)]

#Script labesl as "OHT (number)", manual labels as "(number)"
#Lets tack on the prefix "OHT" to any manual classifications that are numbered
unique_nums_col = ['OHT '+ str(x) for x in unique_nums if str(x) not in 'exclude']

#Then keep the "exclude" classification all alone
unique_nums_col.append('exclude')

#Identifiers for records
id_cols=["toxval_id", "toxval_hash", "source_hash", "parent_toxval_hash",'OHT']
#Array that will hold dataframes containing identifiers for records that do not match up.
comps = []

#Creeate a "report" summary page that will contain total counts and percentage of agreement.
report = pd.DataFrame()
report['OHT'] = unique_nums_col
for i in range(len(report)):
#Filter script output and manual classification dataframes to one oht at a time
    script_class = script[script['OHT'] == report.loc[i,'OHT']][id_cols]
    manual_class = manual[manual['OHT'] == unique_nums[i]][id_cols]
    
#Use merge to see where records are (script only, manual only, or both)
    comp = pd.merge(script_class[['toxval_id','OHT']],manual_class[['toxval_id','OHT']],
                    on = 'toxval_id',
                    how = 'outer',
                    suffixes = ('_script','_manual'),
                    indicator = True)
#Filter the merged dataframe to only records that appear in script dataframe or manual dataframe 
    not_in_common = comp[comp['_merge'] != 'both']
#Then add that to the list of dataframes with identifiers for nonmatching oht classifications
    comps.append(not_in_common)
#Add in the counts of matches and percent agreement to the report dataframe
    report.loc[i,'num_script'] = len(script_class)
    report.loc[i,'num_manual'] = len(manual_class)
    report.loc[i,'percent_yield'] = (len(script_class)/len(manual_class))*100

#Add in the total number of matches to an oht from manual to script
report.loc[i+1,'OHT'] = 'total'
report.loc[i+1,'num_script'] = report['num_script'].sum()
report.loc[i+1,'num_manual'] = report['num_manual'].sum()

#Couldn't name a sheet with ? character, replaced it with the word "Question" instead
unique_nums_col = [c.replace('?','Question') for c in unique_nums_col]

#Save the report cover page and the unmatched identifier dataframes as an excel file
#Each oht gets a sheet named after it that contains the identifiers for the unmatched records 
out_path = r"C:\Users\mmille16\OneDrive - Environmental Protection Agency (EPA)\Profile\Desktop\oht_comparison_1_10_22_v2.xlsx"    
with pd.ExcelWriter(out_path) as writer:
    report.to_excel(writer, index = False,sheet_name = 'summary')
    for i in range(len(unique_nums_col)):
        comps[i].to_excel(writer,index = False, sheet_name = f'{unique_nums_col[i]}')


#%%
"""
Manual one-off scripts I use when looking at particular records for why there is disagreement
Mostly looking at what decisions are made and seeing if they were supposed to be made
As well as testing different ideas for generating important information in the report scripts above
"""
#Checking specific records for diagnostic purposes
id_cols=["toxval_id", "toxval_hash", "source_hash", "parent_toxval_hash"]

decisions = script[script['toxval_id'] == 4799597]
j1 = [col for col in script.columns if 'message' not in col]
#Then remove the id columns
j1 = [col for col in j1 if col not in id_cols]

decision_matrix = decisions[j1]
summary = (str(decisions[['message_summary']]))
#%%
"""
Manual one-off scripts I use when looking at particular records for why there is disagreement
Mostly looking at what decisions are made and seeing if they were supposed to be made
As well as testing different ideas for generating important information in the report scripts above

"""
#Step 2: Find all the unique ohts from manual creation
unique_nums = manual['OHT'].unique()
#Step 3: Filter script output ohts to a unique oht mapping from manual

#Lets just check just the first oht (oht 60) mapped in manual
id_cols=["toxval_id", "toxval_hash", "source_hash", "parent_toxval_hash"]

#Script and manual agree for oht 61, differ for other oht classifications
num = 9
oht = 'exclude'
check = script[script['oht_classification'] == oht][id_cols]
manual_check = manual[manual['OHT'] == oht]

greater_by = abs(len(manual_check) - len(check))
print(greater_by)

#Step 4: See if the filtered script output and filtered manual extraction have the same id columns
check_id = check['toxval_id']
manual_id = manual_check['toxval_id']


comp = pd.merge(check_id,manual_id, on = 'toxval_id', how = 'outer', indicator = True)
not_in_common = comp[comp['_merge'] != 'both']

idx1 = pd.merge(script,not_in_common, on = 'toxval_id')
idx2 = pd.merge(manual,not_in_common, on = 'toxval_id')

    