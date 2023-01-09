The notebook in this folder contains scripts used to match entries in efsa_raw to ICF curated index files

Due to the size of efsa_raw I found it best to do matching of unique entries in efsa_raw first, then duplicate results of unique entries to save time.
The cells in the script output 4 files
	source_efsa_key_mmille16_09212022.xlsx - Part of the key of unique entries after url matching was completed. Contains additonal information from index files used for manual verification.

	efsa_final_key_mmille16_09212022.xlsx - The final key used to duplicate results in efsa_raw. All fields in this file will appear in efsa_matched. Contains fields from efsa_raw as well as the clowder_id and file_name fields, populated after matching to index files.

	source_efsa_matched_mmille16_09212022.xlsx - Filled in efsa_raw file with all clowder_id and file_name fields that could be found and blanks for records that could not be matched.

	source_efsa_missing_mmille16_09212022.xlsx - All (391) of the unique efsa_raw entries that could not be mapped to index files.

Unique documents were matched initially based on the record urls listed in efsa raw, then by long refs to ensure all matches were found. Record hashes of matched documents were searched in the clowder api and the resulting clowder id and file name were entered into the key.