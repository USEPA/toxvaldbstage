# -*- coding: utf-8 -*-
"""
Created on Mon Oct 17 08:43:36 2022

@author: MMILLE16
"""
import pandas as pd

"""
For decisions on the first page of the OHT classification SOP
https://ccte-confluence.epa.gov/display/DEQ/Mapping+ToxVal+Table+Records+to+OHTs+SOP?preview=/142332770/142332771/OHT%20Mapping%20Flow%20Chart%20for%20toxval_full_pull.pptx

Function whose input argument is an input file (in_file)
"""
in_file = r"C:\Users\mmille16\OneDrive - Environmental Protection Agency (EPA)\Profile\Desktop\toxval_pfas150_pfas430.csv"
df = pd.read_csv(in_file)

#New dataframe to be returned at the end of the decision tree traversal
newdf = pd.DataFrame()
#Unique identifiers for data in the input file
id_cols = ['toxval_id','toxval_hash','source_hash','parent_toxval_hash']
newdf[id_cols] = df[id_cols]

#Testing for the first 10 items in the toxval_pfas_150_430 file attached to jira ticket
#orig_types = df.head(10)
#this = newdf.head(10)

#First decision is to exclude from OHT classification if the toxval type is any of these 6 items
types = ["RfD","F","L","L/F","Micro","Meso"]
type_columns = [f'toxval_type_original_{z}' for z in types]
for e in types:
#Use df.apply method with a lambda function. x is going to be an entry in the toxval_type column and e is an element in the exlusion critera    
    newdf[f'toxval_type_original_{e}'] = df['toxval_type_original'].apply(lambda x: 1 if x == e
                                                                          else 0)

#Next we need to do a df.apply on a row basis to create the message column
#Writes an exclude message if there are any 1s (Yeses) in the toxval_type_original columns and a pass message if there are not
newdf['toxval_type_message'] = newdf[type_columns].apply(lambda r: f'excluded due to {r[r == 1].index.tolist()}' if r.any()
                                                  else 'passed on to check study_type_original',
                                                  axis = 1)

"""
Second decision is based on study_type_orignial field
Slightly more complicated since an OHT is determined if study type is acute. Which OHT
depends on the exposure route. 

"""
#Second decision is based on study_type_original

#Can probably write this logic as a funciton later? Did something similar for decision 1 above
newdf['study_type_original_acute'] = df['study_type_original'].apply(lambda x: 1 if 'acute' in x.lower()
                                                                     else 0)
newdf['study_type_original_message'] =newdf['study_type_original_acute'].apply(lambda x: 'checking exposure route and classifying as OHT' if x==1 
                                                                             else 'examining study type further before classigying as OHT')
routes = ['oral','inhalation','dermal']
route_columns = [f'exposure_route_original_{a}' for a in routes]
for e in routes:
    newdf[f'exposure_route_original_{e}'] = df['exposure_route_original'].apply(lambda x: 1 if x.lower() == e
                                                                               else 0)
oht_columns = [e for e in route_columns]
oht_columns.append('study_type_original_acute')
