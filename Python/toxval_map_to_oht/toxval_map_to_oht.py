# -*- coding: utf-8 -*-
"""
Created on Mon Oct 17 08:43:36 2022

@author: MMILLE16
"""
import pandas as pd


def get_decision_matrix(
    df, id_cols=["toxval_id", "toxval_hash", "source_hash", "parent_toxval_hash"]
):
    """

    Parameters
    ----------
    df : dataframe
        Input dataframe to use to generate decision matrix.
    id_cols : list
        List of ID columns to signify a unique input dataframe record.   
    
    Returns
    -------
    dataframe
        Input dataframe with just the desired ToxVal ID columns.

    """
    return df[df.columns.intersection(id_cols)]


# Decision 1
def exclude_toxval_type(df, df_in):
    """
    First decision is to exclude from OHT classification if the toxval type is any of these 6 items
    
    Parameters
    ----------
    df : dataframe
        Decision matrix dataframe to populate with decisions.
    df_in : dataframe
        Dataframe of data to base decisions upon

    Returns
    -------
    dataframe
        Modified input dataframe with decisions made.
    """
    #Toxval types for exclusion of oht classification
    types = ["RfD", "F", "L", "L/F", "Micro", "Meso"]
    df['OHT'] = ["" for i in range(len(df))]

    # Use in_data.apply method with a lambda function. x is going to be an entry in the toxval_type column and e is an element in the exlusion critera
    df["study_type_original_exclusion"] = df_in["study_type_original"].apply(
        lambda x: 1 if x in types else 0
    )
    # Next we need to do a in_data.apply on a row basis to create the message column
    # Writes an exclude message if there are any 1s (Yeses) in the toxval_type_original columns and a pass message if there are not
    df["exclude_type_message"] = df['study_type_original_exclusion'].apply(
        lambda x: "excluded"
        if x == 1
        else "passed on to check study_type_original",
    )
    df['OHT'] = df['study_type_original_exclusion'].apply(
        lambda x: 'exclude'
        if x == 1
        else "")
    return df


# Decision 2
def class_study_type_original(df, df_in):
    """
    Second decision is based on study_type_orignial field
    Slightly more complicated since an OHT is determined if study type is acute. Which OHT
    depends on the exposure route. 

    Parameters
    ----------
    df : dataframe
        Decision matrix dataframe to populate with decisions.
    df_in : dataframe
        Dataframe of data to base decisions upon
    
    Returns
    -------
    dataframe
        Modified input dataframe with decisions made.
    """
    # Second decision is based on study_type_original
    # Can probably write this logic as a funciton later? Did something similar for decision 1 above
    
    #First determine what entries in the input data have a study_type_original of acute (1 if yes, 0 if no)
    not_done = df['OHT'] == ""
    df.loc[not_done, "study_type_original_acute"] = df_in.loc[not_done, "study_type_original"].apply(
        lambda x: 1 if str(x).lower().find('acute') != -1
        else 0
    )
    #Output next steps message. Either classify as oht now (acute study type) or need more decisions (not acute study type_)
    df.loc[not_done,"study_type_original_message"] = df_in.loc[not_done, "study_type_original"].apply(
        lambda x: f'checking exposure route and classifying as OHT. Study type is {x}' 
        if str(x).lower().find('acute') != -1
        else f'examining study type further before classifying as OHT. Study type is {x}'
    )
    
    #Classification of oht for this first stage depends on the exposure route
    routes = ["oral", "inhalation", "dermal",'not specified','not reported']
    df['exposure_route_original_message'] = ["" for x in range(len(df))]
    #Since each exposure route contribute to a different oht, we will make a column for each
    #Then populate the column with 1 (record has the exposure route) or 0 (doesn't have exposure route)
    route_columns = [f"exposure_route_original_{a}" for a in routes]
    for e in routes:
        df.loc[not_done,f"exposure_route_original_{e}"] = df_in.loc[not_done,"exposure_route_original"].apply(
            lambda x: 1 if str(x).lower().find(e) != -1 
            else 0
        )
        

        df.loc[not_done,'exposure_route_original_message'] += df_in.loc[not_done,'exposure_route_original'].apply(
            lambda x: f'exposure route is {x}')
    #Once we have exposure routes, we can classify as an oht. either 60, 61, 62, or 63
    ohts = ['OHT 60','OHT 61','OHT 62','does not match to oht','does not match to oht']
    oht_dict = dict(zip(route_columns, ohts))
    acute = df['study_type_original_acute'] == 1
    not_acute = df['study_type_original_acute'] == 0
    #Populate the oht classification fields by first filtering dataframe rows to ones where study type was acute
    df.loc[not_done & acute,'acute_oht_classification_message'] = df.loc[not_done & acute][route_columns].apply(
        lambda r: f'study type was acute. expected to classify to {oht_dict[r[r == 1].index[0]]}'
        if r.any()
        else 'expected to classify to OHT 63',
        axis = 1)
    df.loc[not_done & acute,'OHT'] = df.loc[not_done & acute][route_columns].apply(
        lambda r: f'{oht_dict[r[r == 1].index[0]]}'
        if r.any()
        else 'OHT 63',
        axis = 1)
    df.loc[not_acute & not_done, 'acute_oht_classification_message'] = 'study type is not acute'
    
    return df

def page_2_decisions (df, df_in):
    """
    Further decisions are necessary based on study_type_original and exposure_route_original fields

    Parameters
    ----------
    df : dataframe
        decision matrix to be populated with binary decisions.
    df_in : dataframe
        raw data from input file to base decisions off of

    Returns
    -------
    df with modified decisions
    """  
    #Move on to a different page for the non classified records
    #What page for classification depends requires us to check the study_type original field for any of these 3 words
    #If it has the words, then page 2. If not, page 3
    not_done = df['OHT'] == ""
    page_2 = ['repeated','chronic','short']
    for n in page_2:
        not_done = df['OHT'] == ""
        df.loc[not_done, f'study_type_original_{n}'] = df_in.loc[not_done,'study_type_original'].apply(lambda x:
                                                                            1 if x.lower().find(n) != -1
                                                                            else 0)
    #Possible oht classifications from page 2
    page_2_dict = {'oral': 'OHT 67',
                   'inhalation': 'OHT 68',
                   'dermal': 'OHT 69-1',
                   'other': 'OHT 69-2'}
    #Options for oht classification on page 2
    repeated = df['study_type_original_repeated'] == 1
    chronic = df['study_type_original_chronic'] == 1
    short = df['study_type_original_short'] == 1
    
    df.loc[repeated|chronic|short,'repeated_chronic_short_message'] = ["" for x in range(len(df.loc[repeated|chronic|short]))]
                                                             
    for k in page_2_dict:
        not_done = df['OHT'] == ""
        #First check the study type original column for oral, inhalation, dermal, or other
        df.loc[repeated|chronic|short&not_done,f'study_type_original_repeated_chronic_short_and_{k}'] = df_in.loc[repeated|chronic|short&not_done]['study_type_original'].apply(
            lambda x: 1 if str(x).lower().find(k) != -1 
            else 0)
        temp = df[f'study_type_original_repeated_chronic_short_and_{k}'] == 1
        df.loc[temp,'repeated_chronic_short_message'] = df_in.loc[temp,'study_type_original'].apply(
            lambda x: f'study type original is {x}. It contains {k} and is expected to classify to {page_2_dict[k]}')
        df.loc[temp,'OHT'] = page_2_dict[k]
        
        #Then check the exposure route original column, doing the same methods
        not_done = df['OHT'] == ""
        df.loc[repeated|chronic|short & not_done,f'study_type_repeated_chronic_short_exposure_route_{k}'] = df_in.loc[repeated|chronic|short&not_done]['exposure_route_original'].apply(
            lambda x: 1 if str(x).lower().find(k) != -1
            else 0)
        temp = df[f'study_type_repeated_chronic_short_exposure_route_{k}'] == 1
        df.loc[temp,'repeated_chronic_short_message'] = df_in.loc[temp,'exposure_route_original'].apply(
            lambda x: f'exposure route original is {x}. It contains {k} and is expected to classify to {page_2_dict[k]}')
        df.loc[temp,'OHT'] = page_2_dict[k]
        
    #Output messages for this stage. Either classified as oht, or needed to look at more fields which will be referenced in future helper functions
    #df['study_type_message'] = df_in.loc[df['study_type_original_acute'] == 0]['study_type_original'].apply(
        #lambda x: f'page 2 since study_type is {x}' if any(word in x for word in page_2)
        #else
        #'page 3')
    #df.loc[not_short & not_chronic & not_repeated, 'page_2_decisions_message'] = 'classification based on page 3'

    return df

def page_3_decisions (df, df_in):
    """
    Parameters
    ----------
    df : dataframe
        decision matrix to be populated with binary decisions.
    df_in : dataframe
        raw data from input file to base decisions off of

    Returns
    -------
    df with modified decisions.
    """
#Dictionary with key words from page 3 and oht classifications from page 3
    page_3_types = {'cancer':'OHT 72',
                    'carcinogenicity': 'OHT 72',
                    'reproductive': 'OHT 73',
                    'developmental': 'OHT 74',
                    'developmental/teratogenicity': 'OHT 74',
                    'fish': 'OHT 41 or OHT 42'}
    not_done = df['OHT'] == ""
    
    for k in page_3_types:
        not_done = df['OHT'] == ""
        df.loc[not_done,f'study_type_original_{k}'] = df_in.loc[not_done,'study_type_original'].apply(lambda x:
                                                                            1 if x.lower().find(k) != -1
                                                                            else 0) 
        temp = df[f'study_type_original_{k}'] == 1
        df.loc[temp,'page_3_message'] = df_in['study_type_original'].apply(lambda x:
                                                                  f'study type is {x}. expected result is {page_3_types[k]}')
        df.loc[temp,'OHT'] = page_3_types[k]
    # df.loc[df['page_3_message'].isna(), 'page_3_message'] = 'page 3 not required'
    fish_types = ['short-term toxicity to fish', 'fet']
    filter6 = df['study_type_original_fish'] == 1
    for x in fish_types:
        df.loc[filter6,f'fish_and_{x}'] = df_in.loc[filter6,'study_type_original'].apply(
            lambda y: 1 if y.lower().find(x) != -1
            else 0)
        df.loc[df[f'fish_and_{x}'] == 1,'OHT'] = 'OHT 41'
    df.loc[filter6,'page_3_message'] = df_in.loc[filter6,'study_type_original'].apply(
        lambda x: 'expected classification to OHT 41' if any(thing in fish_types for thing in x.lower()) 
        else 'expect classification to OHT 42')
    df.loc[df[f'fish_and_{x}'] == 0,'OHT'] = 'OHT 42'
    return df
def message_concatenation (df):
    """
    Parameters
    ----------
    df : dataframe
        decision matrix populated with binary decisions and messages from previous steps

    Returns
    -------
    df with messages concatenated into one column for easier review.
    """
    temp = df['OHT'] == ""
    df.loc[temp,'OHT'] = 'Not classified to OHT'
    j = [x for x in df.columns if 'message' in x]
    for i in j:
        df[i] = df[i].str.strip()
    df['message_summary'] = df.filter(like = 'message').apply(lambda r: ', '.join(r.astype(str)), axis = 1)
    return df

def oht_classification (df, key):
    """
    Parameters
    ----------
    df : fully populated decision matrix from previous steps.
    key : dataframe containing unique rows and oht classifications based on 
    binary decision tree rows

    Returns
    -------
    df with oht classification. Any records with unmatched ohts will be stated.
    """
    #Record id columns must also be ignored
    id_cols=["toxval_id", "toxval_hash", "source_hash", "parent_toxval_hash"]
    #First, extract only the binary decision columns (ignor message columns)
    j = [col for col in df.columns if 'message' not in col]
    #Then remove the id columns
    j = [col for col in j if col not in id_cols]
    
    #Populated ignored entries with 0
    df[j] = df[j].fillna(0)
    df = df.merge(key, on = j, how = 'left')
    df['OHT'] = df['OHT'].fillna('not mapped to an oht in the key')
    return df  
def run_OHT_classification(in_file, oht_key_file):
    """
    For decisions on the first page of the OHT classification SOP
    https://ccte-confluence.epa.gov/display/DEQ/Mapping+ToxVal+Table+Records+to+OHTs+SOP?preview=/142332770/142332771/OHT%20Mapping%20Flow%20Chart%20for%20toxval_full_pull.pptx

    Function whose input argument is an input file (in_file)
    Parameters
    ----------
    in_file : string, required
        File path to input data to classify into OHT. The default is r"toxval_pfas150_pfas430.csv".
    oht_key_file: string, required
        File path to oht key "fingerprint" file to assign oht classification based on 
        binary decision tree
    Returns
    -------
    dec_matrix : dataframe
        Decision matrix for which OHT a record is classified.
    """

    # Load input data file
    in_data = pd.read_csv(in_file)

    # New dataframe to be returned at the end of the decision tree traversal
    dec_matrix = get_decision_matrix(in_data)
    
    #Decisions to be made to ammend the decision matrix
    dec_matrix = exclude_toxval_type(df=dec_matrix.copy(), df_in=in_data)
    dec_matrix = class_study_type_original(df = dec_matrix.copy(), df_in=in_data)
    dec_matrix = page_2_decisions(df = dec_matrix.copy(), df_in = in_data)
    dec_matrix = page_3_decisions(df = dec_matrix.copy(), df_in = in_data)
    dec_matrix = message_concatenation(dec_matrix.copy())
    #Load in the oht key as a pandas dataframe
    #key = pd.read_excel(oht_key_file)
    
    #Assign oht classification based on rows from the key
    #dec_matrix = oht_classification(df = dec_matrix.copy(),key = key)
    
    return dec_matrix


if __name__ == "__main__":
    in_file = r"C:\Users\mmille16\Downloads\toxval_full_pull_2022_08_18.csv"
    key_file = r"C:\Users\mmille16\OneDrive - Environmental Protection Agency (EPA)\Profile\Desktop\footprint_key.xlsx"
    out = run_OHT_classification(in_file = in_file, oht_key_file = key_file)

out_path = r"C:\Users\mmille16\OneDrive - Environmental Protection Agency (EPA)\Profile\Desktop\check2_12_30_22.xlsx"    
with pd.ExcelWriter(out_path) as writer:
    out.to_excel(writer, index = False)

