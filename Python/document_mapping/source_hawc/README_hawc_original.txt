Matching was first performed programatically to create the file  big_hawc_program_match_07052022_mmille16. This matching was based off of
matching the full citations and long reference sections of the original hawc file and the icf document map (there were no records whose documents
were found in the ccte document map). The python script is the file named big_hawc_matching_script.ipynb.

Certain authors and special symbols were not read correctly into the document maps. Additional mapping was done manually and color coded with the key:

White/No Fill: Found Programamtically
Yellow: Strong match found but slighly off. Comments on areas of concerns are made at the end of the row of the first instance of these
Green: Match found manually. Clowder ID and Document Name were copied manually across the document map and original hawc
Orange: Typos or misreadings were likely. Cannot determine any matches because I am unsure what information is correct (ex: title in citation varies
greatly from study title section).
Red: No match could be found. 


The process for manual mapping was typically ty use one of or a combination of the following methods:
	Search the document map for correct chemical listed in hawc original casrn number or chemical name.
	Search the document map for key phrases in the study title listed in hawc original.
	Search the document map for the authors and year of the study.

Completed manual and programatic mapping is reflected in the file hawc_original_matched_07072022_mmille16 with about 70% of records having some sort of
match (green, white, or yellow) from the icf document map.