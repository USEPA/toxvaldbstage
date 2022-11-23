# -*- coding: utf-8 -*-
"""
Created on Wed Nov 23 14:24:27 2022

@author: JWALL01
"""

import pyclowderext.pyclowderext as pct
import pandas as pd
import json
import time

# Helper function to pull metadata and select the desired, most updated metadata
def get_document_metadata(fileID, baseurl, apiKey):
    time.sleep(0.25)
    # Pull metadata
    tmp = pct.get_file_metadata(fileID, baseurl, apiKey)
    # Filter to metadata of interest
    tmp = [i for i in tmp if "Study Name" in i["content"].keys()]
    # Select most recently submitted (most updated)
    # https://stackoverflow.com/questions/5320871/how-to-find-the-min-max-value-of-a-common-key-in-a-list-of-dicts
    tmp = max(tmp, key=lambda x: x["created_at"])
    # Convert to dataframe
    out = pd.json_normalize(tmp["content"])
    # Set clowder_id
    out["clowder_id"] = fileID
    # Set document_name
    out = out.rename(columns={"File Name": "document_name"})

    return out


# Set working directory to Python/document_mapping/TOXVAL-304
# Get list of hawc_pfas_150 Clowder documents
doc_list = pd.read_excel("hawc_pfas_150_clowder_docs_20221123.xlsx")
# Enter Clowder credentials
baseurl = ""
apiKey = ""
# fileID = '61a10e47e4b0993a393800b3'
# Loop through all to get dataframes of metadata
out = [get_document_metadata(f, baseurl, apiKey) for f in doc_list["clowder_id"]]
# Combine dataframes
export = pd.concat(out)
# Export
export.to_excel("hawc_pfas_150_document_map_20221123.xlsx", index=False)
