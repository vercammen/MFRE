This folder contains the files for the FRE501 Palm Oil case study. 

Open "Palm Oil.RProj" first before running any scripts in the "Code" folder. The codes assume you have {pacman} installed on your devices. If not, run install.packages("pacman") first. 

1. Code
- ComTrade_Function - function provided by UN ComTrade
- Optional_exercise - Krisha's code for the optional exercise
- Palm_Oil_Codes_Only - contains codes of the lecture notes. Sections names and some comments provided

2. Data
- palm_data.csv - contains monthly average crude palm oil prices and the US-RM exchange rate, and the monthly cost of corn transportation. 

- comtradedata - contains both objects A and B queried from the get.comTrade() function. It is saved as a .RData format (equivalent to Stata's .dta). Just in case we run into issues with the get.comTrade() function in class, we can load this data in and proceed with the analysis 

- palm_data_optional_exercise - contains the data ready to proceed with the analysis

3. Documents
- Palm_Oil.Docx - lecture notes
- Palm_Oil.html - same lecture notes in html format
- Palm_Oil.RMD - the RMarkdown file used to generate the document