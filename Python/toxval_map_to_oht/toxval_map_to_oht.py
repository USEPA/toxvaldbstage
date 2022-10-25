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
    types = ["RfD", "F", "L", "L/F", "Micro", "Meso"]
    type_columns = [f"toxval_type_original_{z}" for z in types]
    for e in types:
        # Use in_data.apply method with a lambda function. x is going to be an entry in the toxval_type column and e is an element in the exlusion critera
        df[f"toxval_type_original_{e}"] = df_in["toxval_type_original"].apply(
            lambda x: 1 if x == e else 0
        )
    # Next we need to do a in_data.apply on a row basis to create the message column
    # Writes an exclude message if there are any 1s (Yeses) in the toxval_type_original columns and a pass message if there are not
    df["toxval_type_message"] = df[type_columns].apply(
        lambda r: f"excluded due to {r[r == 1].index.tolist()}"
        if r.any()
        else "passed on to check study_type_original",
        axis=1,
    )
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
    df["study_type_original_acute"] = df_in["study_type_original"].apply(
        lambda x: 1 if "acute" in x.lower() else 0
    )
    df["study_type_original_message"] = df["study_type_original_acute"].apply(
        lambda x: "checking exposure route and classifying as OHT"
        if x == 1
        else "examining study type further before classigying as OHT"
    )
    routes = ["oral", "inhalation", "dermal"]
    route_columns = [f"exposure_route_original_{a}" for a in routes]
    for e in routes:
        df[f"exposure_route_original_{e}"] = df_in["exposure_route_original"].apply(
            lambda x: 1 if x.lower() == e else 0
        )
    oht_columns = [e for e in route_columns]
    oht_columns.append("study_type_original_acute")

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
