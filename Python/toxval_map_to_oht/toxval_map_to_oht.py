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
    # Use in_data.apply method with a lambda function. x is going to be an entry in the toxval_type column and e is an element in the exlusion critera
    df["toxval_type_original"] = df_in["toxval_type_original"].apply(
        lambda x: 1 if x in types else 0
    )
    # Next we need to do a in_data.apply on a row basis to create the message column
    # Writes an exclude message if there are any 1s (Yeses) in the toxval_type_original columns and a pass message if there are not
    df["toxval_type_message"] = df['toxval_type_original'].apply(
        lambda x: "excluded"
        if x == 1
        else "passed on to check study_type_original",
    )
    df.loc[df['toxval_type_original'] == 1,'oht_classification'] = 'excluded'
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
    df["study_type_original_acute"] = df_in["study_type_original"].apply(
        lambda x: 1 if "acute" in x.lower() else 0
    )
    #Output next steps message. Either classify as oht now (acute study type) or need more decisions (not acute study type_)
    df["study_type_original_message"] = df["study_type_original_acute"].apply(
        lambda x: "checking exposure route and classifying as OHT"
        if x == 1
        else "examining study type further before classigying as OHT"
    )
    
    #Classification of oht for this first stage depends on the exposure route
    routes = ["oral", "inhalation", "dermal"]
    
    #Since each exposure route contribute to a different oht, we will make a column for each
    #Then populate the column with 1 (record has the exposure route) or 0 (doesn't have exposure route)
    route_columns = [f"exposure_route_original_{a}" for a in routes]
    for e in routes:
        df[f"exposure_route_original_{e}"] = df_in["exposure_route_original"].apply(
            lambda x: 1 if x.lower() == e else 0
        )
    #Once we have exposure routes, we can classify as an oht. either 60, 61, 62, or 63
    ohts = ['OHT 60','OHT 61','OHT 62']
    oht_dict = dict(zip(route_columns, ohts))
    
    #Populate the oht classification fields by first filtering dataframe rows to ones where study type was acute
    df['oht_classification'] = df.loc[(df['study_type_original_acute'] == 1) &
                                      (df['oht_classification'].isna())][route_columns].apply(
        lambda r: oht_dict[r[r == 1].index[0]] 
        if r.any() 
        else 'OHT 63', 
        axis = 1)
    df.loc[df['study_type_original_acute'] == 1, 'study_type_message'] = 'classified to oht'
    

    return df

def decision_3 (df, df_in):
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
    #What page depends requires us to check the study_type original field for any of these 3 words
    #If it has the words, then page 2. If not, page 3
    
    page_2 = ['repeated','chronic','short']
    df['study_type_original_repeated_chronic_short'] = df_in['study_type_original'].apply(lambda x: 1 if any(word in x for word in page_2)
                                                                                          else
                                                                                          0)
    #Output messages for this stage. Either classified as oht, or needed to look at more fields which will be referenced in future helper functions
    df['study_type_message'] = df_in.loc[df['study_type_original_acute'] == 0]['study_type_original'].apply(
        lambda x: f'page 2 since study_type is {x}' if any(word in x for word in page_2)
        else
        'page 3')
    
    return df

def run_OHT_classification(in_file=r"toxval_pfas150_pfas430.csv"):
    """
    For decisions on the first page of the OHT classification SOP
    https://ccte-confluence.epa.gov/display/DEQ/Mapping+ToxVal+Table+Records+to+OHTs+SOP?preview=/142332770/142332771/OHT%20Mapping%20Flow%20Chart%20for%20toxval_full_pull.pptx

    Function whose input argument is an input file (in_file)

    Parameters
    ----------
    in_file : string, required
        File path to input data to classify into OHT. The default is r"toxval_pfas150_pfas430.csv".

    Returns
    -------
    dec_matrix : dataframe
        Decision matrix for which OHT a record is classified.
    """

    # Load input data file
    in_data = pd.read_csv(in_file)

    # New dataframe to be returned at the end of the decision tree traversal
    dec_matrix = get_decision_matrix(in_data)

    # Testing for the first 10 items in the toxval_pfas_150_430 file attached to jira ticket
    # orig_types = in_data.head(10)
    # this = dec_matrix.head(10)

    dec_matrix = exclude_toxval_type(df=dec_matrix.copy(), df_in=in_data)
    dec_matrix = class_study_type_original(dec_matrix.copy(), df_in=in_data)

    return dec_matrix


if __name__ == "__main__":
    out = run_OHT_classification()
