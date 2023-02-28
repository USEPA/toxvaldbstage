# -*- coding: utf-8 -*-
"""
Created on Tue Feb 28 13:18:32 2023

@author: JWALL01
"""

from bs4 import BeautifulSoup
import requests
import pandas as pd

# Base URL
url = "https://www.epa.gov/pesticide-science-and-assessing-pesticide-risks/aquatic-life-benchmarks-and-ecological-risk"
# Navigate to the table section of the page
page = requests.get(url)

# Convert to soup
soup = BeautifulSoup(page.content, "html.parser")
##############################################
# Find table
table = soup.find("table")

# Remove sup tags (otherwise, they're represented as regular numbers)
for s in soup.select("sup"):
    s.decompose()
# Convert to pandas DataFrame
out = pd.concat(pd.read_html(str(table)))
# Combine MultiIndex Columns
out.columns = out.columns.map("_".join).str.strip("_")
# Rename columns
out = out.rename(
    columns={
        "Pesticide_Pesticide": "Pesticide",
        "Year Updated_Year Updated": "Year Updated",
        "CAS number_CAS number": "CAS number",
        "Nonvascular Plants_Nonvascular Plants": "Nonvascular Plants",
        "Vascular Plants_Vascular Plants": "Vascular Plants",
    }
)
# Add columns
out["table_title"] = "OPP Aquatic Life Benchmarks"
out["url"] = url
# Export
out.to_excel(
    r"source_epa_ow_opp_alb_20230228.xlsx", index=False,
)
