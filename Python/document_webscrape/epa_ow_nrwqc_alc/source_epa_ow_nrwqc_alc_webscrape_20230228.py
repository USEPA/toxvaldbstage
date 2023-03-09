# -*- coding: utf-8 -*-
"""
Created on Tue Feb 28 13:18:32 2023

@author: JWALL01
"""

from bs4 import BeautifulSoup
import requests
import pandas as pd

# Base URL
url = "https://www.epa.gov/wqc/national-recommended-water-quality-criteria-aquatic-life-criteria-table"
# Navigate to the table section of the page
page = requests.get(f"{url}#table")

# Convert to soup
soup = BeautifulSoup(page.content, "html.parser")
##############################################
# Find table
table = soup.find("table")
# Convert to pandas DataFrame
out = pd.concat(pd.read_html(str(table)))
# Add columns
out["table_title"] = "National Recommended Aquatic Life Criteria table"
out["url"] = url
# Export
out.to_excel(
    r"source_epa_ow_nrwqc_alc_20230228.xlsx", index=False,
)
